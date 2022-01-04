# This script contains a forest plotting function

# libraries ----

  library(ggplot2)

# functions -----

  plot_forest <- function(inp_table, 
                          est_variable = 'estimate', 
                          param_variable = 'variable', 
                          plot_tag = NULL, 
                          fill_color = 'firebrick4', 
                          x_lab = 'Hazard Ratio', 
                          cutline = 1, 
                          y_labels = NULL) {
    
    ## generates a forest plot with the data from the given glm summary table
    
    forest_plot <- inp_table %>% 
      ggplot(aes(x = .[[est_variable]], y = .[[param_variable]])) + 
      geom_vline(xintercept = cutline, size = 0.5, lty = 2) + 
      geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.25) +
      geom_point(size = 2, shape = 21, fill = fill_color) + 
      geom_text(aes(label = plot_label), size = 2.75, vjust = 1, hjust = 0, nudge_y = -0.2) + 
      xlab(x_lab) + 
      common_theme + 
      theme(axis.title.y = element_blank(), 
            plot.tag = element_text(size = 8, face = 'plain'), 
            plot.tag.position = 'top')
    
    if(!is.null(plot_tag)) {
      
      forest_plot <- forest_plot + 
        labs(tag = plot_tag)
      
    }
    
    if(!is.null(y_labels)) {
      
      forest_plot <- forest_plot + 
        scale_y_discrete(labels = y_labels)
      
    }
    
    return(forest_plot)
    
  }
  
  plot_bar <- function(inp_table, 
                       param_variable = 'variable', 
                       p_value_variable = 'p_value_adj', 
                       reg_variable = 'regulation', 
                       plot_tag = NULL, 
                       fill_scale = NULL, 
                       fill_labs = NULL, 
                       fill_name = NULL, 
                       x_lab = '-log10 p', 
                       y_labels = NULL) {
    
    ## generates a bar plot with the data from the given glm summary table
    
    plot_table <- inp_table %>% 
      mutate(params = .[[param_variable]], 
             p_values = .[[p_value_variable]], 
             regs = .[[reg_variable]])
    
    bar_plot <- plot_table %>% 
      ggplot(aes(x = reorder(params, - p_values), y = -log10(p_values), fill = regs)) + 
      geom_bar(stat = 'identity', color = 'black') + 
      common_theme + 
      theme(axis.title.y = element_blank()) + 
      coord_flip() + 
      ylab(x_lab)
    
   
    if(!is.null(plot_tag)) {
      
      bar_plot <- bar_plot + 
        labs(tag = plot_tag)
      
    }
    
    if(!is.null(y_labels)) {
      
      bar_plot <- bar_plot + 
        scale_x_discrete(labels = y_labels)
      
    }
    
    if(!is.null(fill_scale)) {
      
      bar_plot <- bar_plot + 
        scale_fill_manual(values = fill_scale, 
                          labels = fill_labs, 
                          name = fill_name)
      
    }
    
    return(bar_plot)
    
  }