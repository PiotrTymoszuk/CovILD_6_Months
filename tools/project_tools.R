# This script contains general project tools

# libraries ------

  library(plyr)
  library(tidyverse)
  library(lme4)
  library(lmerTest)

# data import and wrangling -----

  recode_var <- function(inp_tbl, variable, new_name, rec_string, ...) {
  
  ## recodes a variable with the given rec_string and with the car::recode function
  
  out_tbl <- inp_tbl
  
  out_tbl[[new_name]] <- car::recode(out_tbl[[variable]], recodes = rec_string, ...)
  
  return(out_tbl)
  
}

  calulate_delta <- function(inp_data, variable, reference = 0) {
  
  ## calculates a delta of the given variable between the reference timepoint and all other
  
  ref_value <- inp_data[inp_data[, 'time'] == reference, variable]
  
  out_data <- inp_data
  
  out_data <- out_data %>% 
    mutate(delta = .data[[variable]] - ref_value)
  
  return(out_data)
  
}

  add_var_value <- function(inp_data, variable, reference = 0, simplify = F) {
  
  ## adds the variable value predefined in the reference time point to all other time points
  
  ref_value <- inp_data[inp_data[, 'time'] == reference, variable]
  
  if(simplify) {
    
    ID_vec <- inp_data[inp_data[, 'time'] == reference, 'ID']
    
    return(cbind(ID_vec, ref_value) %>% 
             as.data.frame %>% 
             as_tibble %>% 
             set_names(c('ID', variable)))
    
  } else {
    
    out_data <- inp_data
    
    out_data[, variable] <- ref_value
    
    return(out_data)
    
  }
  
}

  tbl_sum <- function(inp_data, variable_vec, na.rm = T) {
    
    var_list <- variable_vec %>% 
      map(function(x) inp_data[[x]])
    
    if(!na.rm) {
      
      var_sum <- var_list %>% 
        reduce(function(x, y) x + y)
      
    } else {
      
      var_sum <- var_list %>% 
        reduce(function(x, y) ifelse(is.na(x), 0, x) + ifelse(is.na(y), 0, y))
      
    }
    
    
    return(var_sum)
    
  }

# a universal modeling function -----

  make_model <- function(inp_data, response, idep_variable = 'time', random_term = '(1|ID)', 
                                 mod_function = glmer, family = NULL, err_resistant = F, ...) {
    
    ## returns a time lapse mixed-effect model
    
    if(length(idep_variable) > 1) {
      
      idep_variable <- idep_variable %>% 
        paste(collapse = ' + ')
      
    }
    
    if(!is.null(random_term)) {
      
      mod_formula <- paste(response, 
                           '~', 
                           idep_variable, 
                           '+', 
                           random_term) %>% 
        as.formula
      
    } else {
      
      mod_formula <- paste(response, 
                           '~', 
                           idep_variable) %>% 
        as.formula
      
    }
    
    
    
    if(is.null(family)) {
      
      if(!err_resistant) {
        
        return(mod_function(formula = mod_formula, 
                            data = inp_data, ...))
        
      } else {
        
        return(try(mod_function(formula = mod_formula, 
                                data = inp_data, ...), 
                   silent = T))
        
      }
      
      
    } else {
      
      if(!err_resistant) {
        
        return(mod_function(formula = mod_formula, 
                            family = family, 
                            data = inp_data, ...))
        
      } else {
        
        return(try(mod_function(formula = mod_formula, 
                                family = family, 
                                data = inp_data, ...), 
                   silent = T))
        
      }
      
    }
    
  }
  
# Clustering: neighbor plot ------
  
  neighbour_plot <- function(plotting_tbl, n_NN = 10, feature = 'CT_findings_V3') {
    
    ## makes a radial plot with the distances to the n nearest neighbors of the CT response
    
    ## axis labs
    
    axis_labs <- c(globals$mod_var_labels, 
                   globals$resp_labels) %>% 
      stri_replace(fixed = '\n', replacement = ' ') %>% 
      set_names(c(names(globals$mod_var_labels), 
                  names(globals$resp_labels)))
    
    ## plotting table
    
    if(!any(feature == plotting_tbl$feature1)) {
      
      return(NULL)
      
    }
    
    plotting_tbl <-  plotting_tbl %>% 
      filter(feature1 == feature) %>% 
      top_n(n = n_NN + 1, 
            desc(matching_dist)) %>% 
      arrange(matching_dist) %>% 
      mutate(plot_order = 1:nrow(.), 
             plot_lab = axis_labs[feature2])
    
    ## plotting limits
    
    x_max <- nrow(plotting_tbl) + 1
    
    ## base plot
    
    base_plot <- plotting_tbl %>% 
      ggplot(aes(x = plot_order, 
                 y = matching_dist, 
                 fill = matching_dist)) + 
      geom_point(size = 2, 
                 shape = 21) + 
      geom_label_repel(aes(label = plot_lab), 
                       size = 2.75, 
                       force = 2, 
                       box.padding = 0.1, 
                       label.padding = 0.1) + 
      expand_limits(y = 0, 
                    x = x_max) + 
      expand_limits(x = -1) + 
      globals$common_theme + 
      theme(axis.ticks.y = element_blank(), 
            axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.line = element_blank(), 
            panel.grid.major.y = element_line(color = 'gray80')) + 
      scale_fill_gradient2(low = 'firebrick4', 
                           mid = 'white', 
                           high = 'steelblue4', 
                           name = 'Matching\nDistance', 
                           limits = c(0, 1),
                           midpoint =  0.5)
    
    ## plot y labs
    
    y_breaks <- ggplot_build(base_plot)$layout$panel_params[[1]]$y$breaks
    
    y_breaks <- y_breaks[!is.na(y_breaks)]
    
    ## adding the radial scale
    
    for(i in y_breaks) {
      
      base_plot <- base_plot + 
        annotate('text',
                 label = i, 
                 x = x_max, 
                 y = i, 
                 size = 2.75, 
                 hjust = 0.5)
      
      
    }
    
    ## radial plot
    
    radial_plot <- base_plot + 
      coord_polar(theta = 'x') + 
      labs(title = paste(axis_labs[feature], 
                         cov_multiclust$clust_labs[plotting_tbl$kmeans_id[1]], 
                         sep = ': '))
    
    return(radial_plot)
    
  }
  
# Text functions ----
  
  wrap_vector <- function(txt_vec, line_length = 5) {
    
    split_vec <- function(inp_vector, chunk_size) {
      
      return(split(inp_vector, ceiling(seq_along(inp_vector)/chunk_size)))
      
    }
    
    split_txt <- split_vec(txt_vec, 
                           line_length) %>% 
      map(paste, 
          collapse = ', ') %>%
      paste(collapse = ',\n')
    
    return(split_txt)
    
  }
  
# Machine learning helper functions -----
  
  get_stat <- function(sim_results, stat_name = 'Se') {
    
    ## obtains the expected value and CI for the given statistic
    ## from the simulation result list
    
    return(sim_results %>% 
             map(filter, 
                 stat == stat_name) %>% 
             map2_dfr(., names(.), 
                      function(x, y) mutate(x, simulation_name = y)))
    
    
  }
  
  get_sim_stats <- function(split_list, 
                                     outcome = 'CT_findings_V3', 
                                     pred_fun = 'naive_bayes', 
                                     k = 5, 
                                     method = 'jaccard', 
                                     kernel_fun = function(x) 1/x,  
                                     .parallel = F, 
                                     .out_parallel = F) {
    
    ## a helper wrapper to handle the n size series of random splits
    ## error resistant cause for some cohort sizes not valid results can be get
    
    start_time <- Sys.time()
    
    message(paste('Simulating:', 
                  length(split_list), 
                  'conditions'))
    
    if(.out_parallel) {
      
      plan('multisession')
      
      size_test <- split_list %>% 
        future_map(function(x) try(test_accuracy(x, 
                                                 outcome = outcome, 
                                                 pred_fun = pred_fun, 
                                                 k = k, 
                                                 method = method, 
                                                 kernel_fun = kernel_fun, 
                                                 generate_random = F, 
                                                 ci_method = 'bca', 
                                                 .parallel = F), 
                                   silent = T), 
                   .options = furrr_options(seed = T))
      
      plan('sequential')
      
    } else {
      
      size_test <- split_list %>% 
        map(function(x) try(test_accuracy(x, 
                                          outcome = outcome, 
                                          pred_fun = pred_fun, 
                                          k = k, 
                                          method = method, 
                                          kernel_fun = kernel_fun, 
                                          generate_random = F, 
                                          ci_method = 'bca', 
                                          .parallel = .parallel), 
                            silent = T))
      
    }
    
    size_test <- size_test %>% 
      map(function(x) if(any(class(x) == 'try-error')) NULL else x) %>% 
      compact
    
    out_lst <- size_test %>% 
      map(function(x) x$summary)

    out_tbl <- c('Se', 'Sp', 'error_rate', 'correct_rate', 'youden_j') %>% 
      map_dfr(get_stat, 
              sim_results = out_lst) %>% 
      mutate(n_train = stri_extract(simulation_name, regex = '\\d+') %>% 
               as.numeric, 
             n_test = 108 - n_train)
    
    message(paste('Elapsed:', Sys.time() - start_time))
    
    return(out_tbl)
    
  }
  
  plot_stat <- function(simulation_results, stat_name, cutoff_at = 80, color = 'coral3', 
                                 plot_title = NULL, plot_subtitle = NULL) {
    
    ## plots a prediction statistic as a function of the training cohort size
    
    y_labs <- c(Se = 'Sensitivity', 
                Sp = 'specificity', 
                youden_j = 'Youden J', 
                error_rate = 'Error rate', 
                correct_rate = 'Correct rate')
    
    plot_tag <- simulation_results %>% 
      filter(stat == stat_name, 
             n_train == cutoff_at)
    
    plot_tag <- paste('\n', 
                      y_labs[stat_name], 
                      ' = ', 
                      signif(plot_tag$expected, 2), 
                      ' [95%CI: ', 
                      signif(plot_tag$lower_ci, 2), 
                      ' - ', 
                      signif(plot_tag$upper_ci, 2), 
                      ']', sep = '')
    
    sim_plot <- simulation_results %>% 
      filter(stat == stat_name) %>% 
      ggplot(aes(x = n_train, 
                 y = expected)) + 
      geom_ribbon(aes(ymin = lower_ci, 
                      ymax = upper_ci), 
                  alpha = 0.15, 
                  fill = color) + 
      geom_line(color = color) + 
      geom_line(aes(y = lower_ci), 
                alpha = 0.5, 
                color = color) + 
      geom_line(aes(y = upper_ci), 
                alpha = 0.5, 
                color = color) + 
      geom_vline(xintercept = cutoff_at, 
                 linetype = 'dashed') + 
      geom_hline(yintercept = 0.5, 
                 linetype = 'dashed') + 
      scale_y_continuous(limits = c(0, 1)) + 
      scale_x_continuous(limits = c(50, 100)) + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           y = y_labs[stat_name], 
           x = 'Training set size, n')
    
    return(sim_plot)
    
  }
  
# END -----