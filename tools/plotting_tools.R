# libraries ----

  library(plyr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(ggrepel)
  library(sciplot)
  library(readr)
  library(stringi)
  library(cowplot)

# themes and globals ----

  ## a common theme used in all standard plots
  
    common_theme <- theme_classic() + 
      theme(legend.text = element_text(size = 8, face = 'plain', color = 'black'), 
            legend.title = element_text(size = 8, face = 'plain', color = 'black'), 
            axis.text = element_text(size = 8, face = 'plain', color = 'black'), 
            axis.title = element_text(size = 8, face = 'plain', color = 'black'), 
            strip.text = element_text(size = 8, face = 'plain', color = 'black'), 
            strip.background = element_rect(color = 'black', fill = 'gray95'), 
            plot.tag = element_text(size = 8, face = 'plain', color = 'black', 
                                    margin = ggplot2::margin(8, 5, 5, 5)), 
            plot.tag.position = 'bottom', 
            plot.title = element_text(size = 8, face = 'plain'), 
            plot.subtitle = element_text(size = 8, face = 'plain'), 
            plot.margin = ggplot2::margin(t = 5, l = 3, unit = 'mm'))
  
  ## default significant digits
    
    default_signif <- 2
    
  # globals
    ## G4: stage 1 models of interest
    ## G3: stage 1 best class models
    ## G2: stage 2 models of interest
    ## G1: stage 2 best class models
    
    fill_scale_quality <- c(G3 = 'firebrick4', 
                            G2 = 'firebrick2', 
                            G1 = 'darkorange', 
                            G4 = 'gray80', 
                            comparator = 'steelblue4', 
                            hand_picked = 'mediumpurple3')
    
    group_labels <- c(G3 = 'Stage 1 significant\nmodels', 
                      G2 = 'Stage 1 best\nmodels', 
                      G1 = 'Stage 2 best\nmodels', 
                      G4 = 'not significant', 
                      comparator = 'Comparators', 
                      hand_picked = 'Hand-picked')
    
    hi_lo_scale = c(lo = 'steelblue4', 
                    hi = 'firebrick4')
    
    def_jitter <- 0.15
    def_nudge_y <- 0.25
    
    roc_colors <- c(ERS_risk_Compera = 'steelblue3', 
                    ERS_risk_SPAHR = 'steelblue4', 
                    FRENCH_risk3p = 'plum2', 
                    FRENCH_risk4p = 'plum4', 
                    mRASP_risk = 'darkseagreen4', 
                    score = 'firebrick4', 
                    subj_risk = 'black')
    
    roc_labels <- c(ERS_risk_Compera = 'COMPERA', 
                    ERS_risk_SPAHR = 'SPAHR', 
                    FRENCH_risk3p = 'FPHR3p', 
                    FRENCH_risk4p = 'FPHR4p', 
                    mRASP_risk = 'mRASP', 
                    subj_risk = 'Subjective\nRisk Assessment')
    
# plot editing functions ----
    
    plot_editor <- function(inp_plot, x_lab, y_lab, jitter_w = def_jitter, add_labels = T) {
      
      # makes plots consistent
      
      edited_plot <- inp_plot + 
        geom_point(shape = 21, 
                   size = 2, 
                   position = position_jitter(jitter_w)) + 
        scale_fill_manual(values = fill_scale_quality, name = '', labels = group_labels) + 
        labs(x = x_lab, y = y_lab) +
        common_theme
      
      if(add_labels){
        
        edited_plot <- edited_plot + geom_label_repel(aes(label = plot_label), size = 2.75)
        
      }
      
      return(edited_plot)
      
    }
    
# forest plotter ----
    
    plot_forest <- function(inp_model, error_barh = def_jitter, 
                            annotation_name = NULL, fill_color = fill_scale_quality['G3'], 
                            x_lab = 'exp Regression \u03B2 estimate') {
      
      if(any(class(inp_model) == 'coxph')) {
        
        plotting_table <- inp_model %>% 
          get_cox_results(exponentiate = T)
        
        annotation_n <- c(total_n = inp_model$n, event_n = inp_model$nevent)
        
      } else {
        
        plotting_table <- inp_model %>% 
          get_glm_results(exponentiate = T)
        
        annotation_n <- NULL
        
      }
      
      if(!is.null(annotation_n)) {
        
        annotation_label <- paste(annotation_name, 
                                  ', ', 
                                  'total n = ', 
                                  annotation_n['total_n'], 
                                  ', ', 
                                  'event n = ', 
                                  annotation_n['event_n'],  
                                  sep = '')
        
      } else {
        
        annotation_label <- annotation_name
        
      }
      
      optimal_ann_x = max(plotting_table$upper_ci) - 0.2*(max(plotting_table$upper_ci) - min(plotting_table$lower_ci))
      
      
      forest_plot <-  plotting_table %>% 
        filter(parameter != '(Intercept)') %>% 
        ggplot(aes(x = estimate, y = parameter)) + 
        geom_vline(xintercept = 1, lty = 2) + 
        geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = def_jitter) + 
        geom_point(size = 2, shape = 21, fill = fill_color) + 
        geom_text(aes(label = plot_label), size = 2.75, hjust = 0, nudge_y = def_nudge_y) +
        labs(tag = annotation_label, x = x_lab) +
        common_theme + 
        theme(axis.title.y = element_blank())
      
      return(forest_plot)
      
    }
    
# venn plotting -----
    
    plot_venn <- function(gene_study_list, filename,  ...) {
      
      # plots a Venn diagram for the given named list of gene vectors
      # and saves it as a pdf file
      
      require(VennDiagram)
      require(venn)
      
      output <- gene_study_list %>% 
        venn.diagram(., filename = NULL, ...)
      
      pdf(filename, width = 3, height = 3)
      
      grid.draw(output)
      
      dev.off()
      
    }
    
# Figure object and saving functions -----
    
    as_figure_object <- function(figure_plot, figure_label, h, w = 180) {
      
      ## creates a ready-to-save list with the figure and metadata
      
      figure_obj <- list(plot = figure_plot, 
                         figure_label = figure_label, 
                         h = h, 
                         w = w)
      
      return(figure_obj)
      
    }
    
    save_figure_object <- function(figure_obj, format = 'pdf', target_folder = 'figures', ...) {
      
      ## saves a figure object
      
      save_figure <- function(figure_plot, figure_name, h, w = 180, target_folder = 'figures', ...) {
        
        ## an error-resistant figure-saving function
        
        insert_msg(paste('Saving:', figure_name))
        
        enter_directory(target_folder)
        
        tryCatch(ggsave(filename = figure_name, 
                        plot = figure_plot, 
                        width = w, 
                        height = h, 
                        units = 'mm', 
                        ...), 
                 error = simpleError('Saving failed'), 
                 finally = go_proj_directory())
        
      }
      
      save_figure(figure_plot = figure_obj$plot, 
                  figure_name = paste(figure_obj$figure_label, format, sep = '.'), 
                  h = figure_obj$h, 
                  w = figure_obj$w, 
                  target_folder = target_folder, ...)
      
    }
    
    