# This script contains general project tools

# libraries ------

  require(plyr)
  require(tidyverse)
  require(lme4)
  require(lmerTest)
  require(ggvenn)
  require(gmodels)
  require(vcd)
  require(coxed)

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
  
  complete_cases <- function(inp_tbl, index_var = 'ID', long_no = NULL) {
    
    ## excludes individuals with the incomplete longitudinal record
    
    out_tbl <- inp_tbl %>% 
      dlply(index_var, function(x) if(all(complete.cases(x))) x else NULL) %>% 
      compact %>% 
      reduce(rbind) %>% 
      as_tibble
    
    if(!is.null(long_no)) {
      
      stopifnot(is.numeric(long_no))
      
      out_tbl <- inp_tbl %>% 
        dlply(index_var, function(x) if(nrow(x) == long_no) x else NULL) %>% 
        compact %>% 
        reduce(rbind) %>% 
        as_tibble
      
    }
    
    return(out_tbl)
    
  }
  
  set_rownames <- function(data, rowname_vec) {
    
    rownames(data) <- rowname_vec
    
    return(data)
    
  }

# Modeling functions -----

  make_model <- function(inp_data, 
                         response, 
                         idep_variable = 'time', 
                         random_term = '(1|ID)', 
                         mod_function = glmer, 
                         family = NULL, 
                         err_resistant = FALSE, ...) {
    
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
  
  plot_forest <- function(summary_tbl, 
                          top_n = 5, 
                          plot_title = NULL, 
                          plot_subtitle = NULL, 
                          plot_tag = NULL, 
                          signif_only = TRUE, 
                          x_lab = 'OR', 
                          cutpoint = 1, 
                          x_trans = 'log2') {
    
    ## plots a detailed Forest plot for the modeling results
    
    ## plotting table
    
    plotting_tbl <- list()
    
    plotting_tbl$up <- summary_tbl %>% 
      arrange(desc(estimate))
    
    plotting_tbl$down <- summary_tbl %>% 
      arrange(estimate)
    
    plotting_tbl <- plotting_tbl %>% 
      map_dfr(~.x[1:top_n, ])
    
    if(signif_only) {
      
      plotting_tbl <- plotting_tbl %>% 
        filter(regulation != 'ns')
      
    }
    
    ## adding detailed variable labeling (baseline, n numbers)
    
    plotting_tbl <- plotting_tbl %>% 
      mutate(variable = globals$mod_var_labels[variable], 
             variable = stri_replace(variable, fixed = '\n', replacement = ' '), 
             var_label = paste0(variable, ', n = ', n_level, 
                                '\nref: no ', stri_replace(decapitalize(variable), fixed = 'any ', replacement = ''), 
                                ', n = ', n_complete - n_level), 
             var_label = stri_replace(var_label, fixed = 'no iCU', replacement = 'no ICU'), 
             var_label = stri_replace(var_label, fixed = 'no >', replacement = '\u2264'), 
             var_label = stri_replace(var_label, fixed = 'no over', replacement = '\u2264'),
             var_label = stri_replace(var_label, fixed = 'no hospitalized >7', replacement = 'hospitalized \u22647'), 
             var_label = stri_replace(var_label, fixed = 'Over', replacement = '>'), 
             var_label = stri_replace(var_label, fixed = 'cKD', replacement = 'CKD'), 
             var_label = stri_replace(var_label, fixed = 'no anti-S1/S2 IgG Q1', replacement = 'anti-S1/S2 IgG Q2 - Q4'), 
             est_label = paste0(signif(estimate, 3), ' [', signif(lower_ci, 3), ' - ', signif(upper_ci, 3), ']'))
    
    ## forest plot
    
    plotting_tbl %>% 
      ggplot(aes(x = estimate, 
                 y = reorder(var_label, estimate), 
                 color = regulation)) + 
      geom_vline(xintercept = cutpoint, 
                 linetype = 'dashed') + 
      geom_errorbarh(aes(xmin = lower_ci, 
                         xmax = upper_ci), 
                     height = 0) + 
      geom_point(size = 2, 
                 shape = 16) + 
      geom_text(aes(label = est_label), 
                size = 2.75, 
                hjust = 0.2, 
                vjust = -0.8) + 
      scale_color_manual(values = c(positive = 'coral3', 
                                    negative = 'steelblue', 
                                    ns = 'gray60'), 
                         labels = c(positive = 'unfavorable', 
                                    negative = 'favorable', 
                                    ns = 'ns'), 
                         name = 'Risk factor') + 
      scale_x_continuous(trans = x_trans) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab)
    
  }
  
# Clustering ------
  
  quality_bar_plot <- function(data, 
                               x_var, 
                               y_var, 
                               fill_var = NULL, 
                               plot_title = NULL, 
                               plot_subtitle = NULL, 
                               plot_tag = NULL, 
                               x_lab = NULL, 
                               bar_fill = 'cornsilk', 
                               txt_color = 'black', 
                               hjust = 1.3) {
    
    ## makes a simple bar plot

    if(is.null(fill_var)) {
      
      base_plot <- data %>% 
        ggplot(aes(x = .data[[x_var]],
                   y = reorder(.data[[y_var]], .data[[x_var]]))) + 
        geom_bar(stat = 'identity', 
                 color = 'black', 
                 fill = bar_fill) + 
        geom_text(aes(label = signif(.data[[x_var]], 2)), 
                  size = 2.75, 
                  color = txt_color, 
                  hjust = hjust, 
                  vjust = 0.5)
      
    } else {
      
      base_plot <- data %>% 
        ggplot(aes(x = .data[[x_var]],
                   y = reorder(.data[[y_var]], .data[[x_var]]), 
                   fill = .data[[fill_var]])) + 
        geom_bar(stat = 'identity', 
                 color = 'black', 
                 position = position_dodge(0.9)) + 
        geom_text(aes(label = signif(.data[[x_var]], 2)), 
                  size = 2.75, 
                  color = txt_color, 
                  hjust = hjust, 
                  vjust = 0.5, 
                  position = position_dodge(0.9))
      
    }
    
    base_plot + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle,
           tag = plot_tag, 
           x = x_lab)
    
  }
  
  neighbour_plot <- function(dist_tbl, 
                             center_var, 
                             n_neighbors = 5, 
                             ft_labels = ft_clust$ft_labels, 
                             plot_title = NULL, 
                             plot_subtitle = NULL, 
                             plot_tag = NULL) {
    
    ## generates a radial plot
    
    dist_tbl <- as.data.frame(dist_tbl) %>% 
      rownames_to_column('feature') %>% 
      select(feature, .data[[center_var]]) %>% 
      set_names(c('feature', 'distance')) %>%
      filter(feature != center_var) %>% 
      top_n(n = n_neighbors, -distance) %>% 
      mutate(feature = ft_labels[feature], 
             x_rand = sample(seq(1, nrow(.), by = 1), nrow(.), replace = FALSE))
    
    
    dist_ranges <- range(dist_tbl$distance)
    
    ## plot
    
    dist_tbl %>% 
      ggplot(aes(x = x_rand, 
                 y = distance, 
                 color = distance, 
                 size = 1 - distance)) + 
      geom_segment(aes(x = x_rand, 
                       xend = x_rand, 
                       y = 0, 
                       yend = distance), 
                   size = 0.25, 
                   alpha = 0.5) + 
      geom_point(shape = 16) + 
      geom_text_repel(aes(label = feature), 
                      show.legend = FALSE, 
                      point.padding = 3) + 
      coord_polar(theta = 'x') + 
      scale_x_discrete(breaks = c(1:(nrow(dist_tbl) + 1))) + 
      scale_size_continuous(range = c(2, 3.5)) + 
      scale_color_gradient2(low = 'firebrick', 
                            mid = 'black', 
                            high = 'steelblue', 
                            midpoint = mean(dist_ranges)) + 
      guides(size = FALSE) + 
      globals$common_theme + 
      theme(axis.line = element_blank(), 
            axis.text = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag)

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
  
  decapitalize <- function(txt) {
    
    start_char <- stri_extract(txt, regex = '^\\w{1}') %>% 
      tolower
    
    stri_replace(txt, regex = '^\\w{1}', replacement = start_char)
    
  }
  
  fill_zero <- function(txt, total_length = 3, prefix = 'P') {

    paste0(prefix, 
           paste(rep('0', total_length - stri_length(txt)), collapse = ''), 
           txt)
    
    
  }

# Variable overlap ------
  
  plot_overlap_venn <- function(data, 
                                id_var = 'ID', 
                                overlap_vars = c('sympt_present', 'lung_function_impaired', 'CT_findings'), 
                                subset_names = overlap$var_labels[overlap_vars], 
                                fill_color = unname(overlap$var_colors[overlap_vars]), 
                                plot_title = NULL, 
                                plot_subtitle = NULL, 
                                plot_tag = NULL, ...) {
    
    ## displays the overlap between binary variables in a Venn plot
    
    ovr_list <- overlap_vars %>% 
      map(~filter(data, .data[[.x]] == 1)) %>% 
      map(~.x[[id_var]]) %>% 
      set_names(subset_names)
    
    ggvenn(ovr_list, 
           fill_color = fill_color, 
           set_name_size = 2.75, 
           text_size = 2.5, ...) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag) + 
      theme(plot.title = element_text(size = 8, face = 'bold', color = 'black'), 
            plot.subtitle = globals$common_text, 
            plot.tag = element_text(size = 8, 
                                    face = 'plain', 
                                    color = 'black', 
                                    hjust = 0), 
            plot.tag.position = 'bottom')
    
    
  }
  
  get_kappa <- function(data, variable1, variable2, kappa_only = TRUE, chisq = TRUE) {
    
    ## calculates proportions and unweighted Cohen's Kappa for two variables

    cross_tbl <- table(data[[variable1]], 
                       data[[variable2]])
    
    kappa_stat <- try(Kappa(cross_tbl), silent = TRUE)
    
    if(any(class(kappa_stat) == 'try-error')) {
      
      kappa_stat <- Kappa(rbind(cross_tbl, c(0, 0))) ## to rescue the errors when no correct predictions were made
      
    }
    
    kappa_ci <- confint(kappa_stat)
    
    kappa_tbl <- tibble(kappa = kappa_stat[['Unweighted']][1], 
                        se = kappa_stat[['Unweighted']][2]) %>% 
      mutate(z = kappa/se, 
             lower_ci = kappa_ci['Unweighted', 1], 
             upper_ci = kappa_ci['Unweighted', 2], 
             p_value = 2*pnorm(z, lower.tail = F), 
             variable1 = variable1, 
             variable2 = variable2, 
             n_number = sum(cross_tbl)) ## two-tailed z test
    
    if(kappa_only) {
      
      return(kappa_tbl)
      
    } else {
      
      return(list(cross_tbl = cross_tbl, 
                  kappa = kappa_tbl))
      
    }
    
  }
  
  plot_kappa <- function(kappa_table, 
                         plot_title = NULL, 
                         plot_subtitle = NULL, 
                         plot_tag = NULL) {
    
    ## plots kappas between variable pairs as a heat map
    
    kappa_hm <- kappa_table %>% 
      mutate(plot_lab = paste0(signif(kappa, 2), 
                               '\n[', 
                               signif(lower_ci, 2), 
                               ' - ', 
                               signif(upper_ci, 2), 
                               ']\np = ', 
                               signif(p_adj, 2), 
                               '\nn = ', n_number)) %>% 
      ggplot(aes(x = variable2, 
                 y = variable1,
                 fill = kappa)) + 
      geom_tile(color = 'gray40') + 
      geom_text(aes(label = plot_lab), 
                size = 2.6) + 
      scale_fill_gradient2(low = 'steelblue', 
                           mid = 'white', 
                           high = 'firebrick', 
                           name = 'Kappa', 
                           midpoint = 0.5, 
                           limits = c(0, 1)) + 
      globals$common_theme + 
      theme(axis.title = element_blank(), 
            panel.grid.major = element_line(color = 'gray90')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag)
    
    return(kappa_hm)
    
  }
  
# Machine learning -----
  
  get_accuracy <- function(data, variable1, variable2) {
    
    ## calculates mean accuracy
    
    mean(data[[variable1]] == data[[variable2]], na.rm = TRUE)
    
  }
  
  train_lst <- function(form, 
                        data, 
                        trControl, 
                        methodList, 
                        ensMethod, 
                        continue_on_fail = TRUE, 
                        ci_method = c('norm', 'percentile', 'bca'), ...) {
    
    ## a wrapper for a caret list generation and ensemble development
    
    start_time = Sys.time()
    
    message(paste('Training the models for ', paste(methodList, collapse = ', ')))
    
    on.exit(message(paste('Elapsed:', Sys.time() - start_time)))
    
    ## CI calculation method
    
    ci_method <- match.arg(ci_method[1], c('norm', 'percentile', 'bca'))

    calc_summary <- function(stat_vec, ci_method) {
      
      ci_fun <- switch(ci_method, 
                       norm = function(x) c(qnorm(0.025, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 
                                            qnorm(0.9755, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))), 
                       percentile = function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE), 
                       bca = bca(x, conf.level = 0.95))
      
      tibble(mean_stat = mean(stat_vec, na.rm = TRUE), 
             lower_ci = ci_fun(stat_vec)[1], 
             upper_ci = ci_fun(stat_vec)[2])
      
    }
    
    ## model list
    
    mod_list <- caretList(form = form, 
                          data = data, 
                          trControl = trControl, 
                          methodList = methodList, 
                          continue_on_fail = continue_on_fail, 
                          metric = 'Kappa')
    
    mod_resamp <- resamples(mod_list)
    
    ## ensemble construction
    
    mod_ens <- caretStack(mod_list,
                          method = ensMethod, 
                          trControl = trControl, ...)
    
    ## true outcome
    
    cmm_outcome <- mod_list[[1]]$trainingData %>% 
      rownames_to_column('ID') %>% 
      mutate(rowIndex = 1:nrow(.)) %>% 
      select(ID, rowIndex, .outcome)
    
    ## model predictions in the training data
    
    mod_probs <- mod_list %>% 
      map(predict, type = 'prob') %>% 
      map(~.x[[1]]) %>% 
      as_tibble
    
    mod_probs <- mod_probs %>% 
      mutate(ensemble = predict(mod_ens$ens_model, 
                                newdata = mod_probs, 
                                type = 'prob')[[1]], 
             .outcome = cmm_outcome$.outcome, 
             ID = cmm_outcome$ID)
    
    mod_predictions <- mod_list %>% 
      map(predict) %>% 
      as_tibble %>%
      mutate(ensemble = predict(mod_ens$ens_model, 
                                newdata = mod_probs), 
             .outcome = cmm_outcome$.outcome, 
             ID = cmm_outcome$ID)
    
    ## model predictions in the CV
    
    cv_probs <- mod_list %>% 
      map(~.x$pred) %>% 
      map(arrange, rowIndex) %>% 
      map(~.x[c('rowIndex', 'Resample', levels(.x[['pred']])[1])]) %>% 
      map2(., names(.), ~set_names(.x, c('rowIndex', 'Resample', .y))) %>% 
      reduce(left_join, by = c('rowIndex', 'Resample')) %>% 
      left_join(cmm_outcome, by = 'rowIndex') %>% 
      as_tibble
    
    cv_predictions <- mod_list %>% 
      map(~.x$pred) %>% 
      map(arrange, rowIndex) %>% 
      map(~.x[c('rowIndex', 'Resample', 'pred')]) %>% 
      map2(., names(.), ~set_names(.x, c('rowIndex', 'Resample', .y))) %>% 
      reduce(left_join, by = c('rowIndex', 'Resample')) %>% 
      left_join(cmm_outcome, by = 'rowIndex') %>% 
      as_tibble
    
    cmm_ens_outcome <- mod_ens$ens_model$trainingData %>% 
      mutate(rowIndex = 1:nrow(.)) %>% 
      select(rowIndex, .outcome) %>% 
      left_join(cv_probs %>% 
                  mutate(rowIndex = 1:nrow(.)) %>% 
                  select(rowIndex, ID), 
                by = 'rowIndex')
    
    ens_cv_probs <- mod_ens$ens_model$pred %>% 
      arrange(rowIndex) %>% 
      .[c('rowIndex', 'Resample', levels(mod_ens$ens_model$pred$pred)[1])] %>% 
      left_join(cmm_ens_outcome, by = 'rowIndex') %>% 
      as_tibble() %>% 
      set_names(c('rowIndex', 'Resample', 'ensemble', '.outcome', 'ID'))
    
    ens_cv_predictions <- mod_ens$ens_model$pred %>% 
      arrange(rowIndex) %>% 
      .[c('rowIndex', 'Resample', 'pred')] %>% 
      left_join(cmm_ens_outcome, by = 'rowIndex') %>% 
      as_tibble() %>% 
      set_names(c('rowIndex', 'Resample', 'ensemble', '.outcome', 'ID'))
    
    
    ## model prediction stats in the training data
    
    train_stats <- list()
    
    train_stats$accuracy <- mod_predictions %>% 
      select( - .outcome, -ID) %>% 
      map_dbl(~mean(.x == mod_predictions[['.outcome']], na.rm = TRUE))
    
    train_stats$accuracy <- tibble(mean_stat = train_stats$accuracy, 
                                   lower_ci = NA_real_,
                                   upper_ci = NA_real_, 
                                   method = c(names(mod_list), 'ensemble'), 
                                   stat = 'Accuracy')
    
    train_stats$kappa <- c(names(mod_list), 'ensemble') %>% 
      map_dfr(~get_kappa(data = mod_predictions, 
                         variable1 = .x, 
                         variable2 = '.outcome', 
                         kappa_only = TRUE)) %>% 
      select(kappa, lower_ci, upper_ci) %>% 
      set_names(c('mean_stat', 'lower_ci', 'upper_ci')) %>% 
      mutate(method = c(names(mod_list), 'ensemble'), 
             stat = 'Kappa')
    
    train_stats <- train_stats %>% 
      reduce(rbind) %>% 
      arrange(method, stat)
    
    ## model prediction stats in the CV
    
    cv_stats <- list()
    
    cv_stats$accuracy <- cv_predictions %>% 
      dlply(.(Resample)) %>% 
      map_dfr(~map(names(mod_list), 
                   get_accuracy, 
                   data = .x, 
                   variable2 = '.outcome') %>% 
                set_names(names(mod_list)) %>% 
                as_tibble) %>% 
      map(calc_summary, ci_method = ci_method) %>% 
      map2_dfr(., names(.), ~mutate(.x, method = .y, stat = 'Accuracy'))
    
    cv_stats$accuracy_ens <- ens_cv_predictions %>% 
      dlply(.(Resample)) %>% 
      map_dbl(get_accuracy, 
              variable1 = 'ensemble', 
              variable2 = '.outcome')
    
    cv_stats$accuracy_ens <- calc_summary(cv_stats$accuracy_ens, ci_method = ci_method) %>% 
      mutate(stat = 'Accuracy', 
             method = 'ensemble')

    cv_stats$kappa <- cv_predictions %>% 
      dlply(.(Resample)) %>% 
      map_dfr(~map(names(mod_list), 
                   get_kappa, 
                   data = .x, 
                   variable2 = '.outcome', 
                   kappa_only = TRUE) %>% 
                set_names(names(mod_list)) %>% 
                map(~.x[['kappa']]) %>% 
                as_tibble) %>% 
      map(calc_summary, ci_method = ci_method) %>% 
      map2_dfr(., names(.), ~mutate(.x, method = .y, stat = 'Kappa'))
 
    cv_stats$kappa_ens <- ens_cv_predictions %>% 
      dlply(.(Resample)) %>% 
      map_dfr(get_kappa, 
              variable1 = 'ensemble', 
              variable2 = '.outcome', 
              kappa_only = TRUE) %>% 
      .$kappa
    
    cv_stats$kappa_ens <- calc_summary(cv_stats$kappa_ens, ci_method = ci_method) %>% 
      mutate(stat = 'Kappa', 
             method = 'ensemble')
    
    cv_stats <- reduce(cv_stats, rbind)
    
    ## model correlations
    
    mod_corr <- modelCor(mod_resamp)
    
    ## output
    
    list(model_list = mod_list, 
         ensemble = mod_ens, 
         train_pred = mod_predictions, 
         train_probs = mod_probs, 
         cv_pred = cv_predictions,
         cv_probs = cv_probs, 
         ens_cv_pred = ens_cv_predictions, 
         ens_cv_probs = ens_cv_probs, 
         train_stats = train_stats, 
         cv_stats = cv_stats, 
         correlations = mod_corr)
    
  }
  
  extract_preds <- function(train_list, dataset = c('cv', 'train')) {
    
    ## extracts the predictions and class probabilities
    
    dataset <- match.arg(dataset[1], c('cv', 'train'))

    model_types <- names(train_list$model_list)
    
    if(dataset == 'cv') {
      
      output <- list()
      
      output <- model_types %>% 
        map(~tibble(ID = train_list$cv_pred$ID, 
                    obs = train_list$cv_pred$.outcome, 
                    pred = train_list$cv_pred[[.x]], 
                    prob = train_list$cv_probs[[.x]], 
                    prob_rec = 1 - train_list$cv_probs[[.x]])) %>% 
        set_names(model_types)
      
      output$ensemble <- tibble(ID = train_list$ens_cv_pred$ID, 
                                obs = train_list$ens_cv_pred$.outcome, 
                                pred = train_list$ens_cv_pred$ensemble, 
                                prob = train_list$ens_cv_probs$ensemble, 
                                prob_rec = 1 - train_list$ens_cv_probs$ensemble)
      
      output %>% 
        map(set_names, c('ID', 'obs', 'pred', levels(train_list$train_pred[[1]])))

    } else {
      
      c(model_types, 'ensemble') %>% 
        map(~tibble(ID = train_list$train_pred$ID, 
                    obs = train_list$train_pred$.outcome, 
                    pred = train_list$train_pred[[.x]], 
                    prob = train_list$train_probs[[.x]], 
                    prob_rec = 1 - train_list$train_probs[[.x]])) %>% 
        map(set_names, c('ID', 'obs', 'pred', levels(train_list$train_pred[[1]]))) %>% 
        set_names(c(model_types, 'ensemble'))

    }
    
  }
  
  get_roc <- function(train_list, lev = c('yes', 'no')) {
    
    ## obtains the ROC cutpoint and AUC stats
    
    roc <- list()
    
    model_types <- c(names(train_list$model_list), 'ensemble')
    
    ## training
    
    roc$train <- train_list %>% 
      extract_preds('train') %>%
      map(as.data.frame) %>% 
      map(~twoClassSummary(data = .x, 
                           lev = lev)) %>% 
      map(as.list) %>% 
      map_dfr(as_tibble) %>% 
      mutate(method = model_types)
    
    ## cv 
    
    roc$cv <- train_list %>% 
      extract_preds('cv') %>% 
      map(as.data.frame) %>% 
      map(~twoClassSummary(data = .x, 
                           lev = lev)) %>% 
      map(as.list) %>% 
      map_dfr(as_tibble) %>% 
      mutate(method = model_types)
    
    roc
    
  }
  
  plot_line <- function(data, 
                        y_var = 'mean_stat', 
                        show_ci = FALSE, 
                        show_value = TRUE, 
                        plot_title = NULL, 
                        plot_subtitle = NULL, 
                        plot_tag = NULL, 
                        dodge_width = 0.1, 
                        connect_line = TRUE) {
    
    base_plot <- data %>% 
      ggplot(aes(x = method, 
                 y = .data[[y_var]], 
                 color = stat))
    
    if(connect_line) {
      
      base_plot <- base_plot +
        geom_line(aes(group = stat), 
                  position = position_dodge(dodge_width))
      
      
    }
    
    if(show_ci) {
      
      base_plot <- base_plot + 
        geom_errorbar(aes(ymin = lower_ci, 
                          ymax = upper_ci), 
                      width = 0, 
                      position = position_dodge(dodge_width))
      
    }
    
    if(show_value) {
      
      base_plot <- base_plot + 
        geom_text(aes(label = signif(mean_stat, 2)), 
                  size = 2.75, 
                  position = position_dodge(dodge_width), 
                  hjust = -0.2, 
                  vjust = -0.2)
      
    }
    
    base_plot + 
      geom_point(size = 2, 
                 shape = 16, 
                 position = position_dodge(dodge_width)) + 
      globals$common_theme + 
      theme(axis.title.x = element_blank(), 
            panel.grid.major = element_line(color = 'gray90')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           y = 'Mean statistic value', 
           tag = plot_tag)
    
  }
  
  plot_list_stats <- function(train_list, 
                              dataset = c('train', 'cv'), 
                              y_var = 'mean_stat', 
                              show_ci = FALSE, 
                              show_value = TRUE, 
                              plot_title = NULL, 
                              plot_subtitle = NULL, 
                              dodge_width = 0.1, 
                              connect_line = TRUE) {
    
    ## plots accuracy and kappa statistics for the caret model/ensemble list
    
    dataset <- match.arg(dataset[1], c('train', 'cv'))
    
    data <- switch(dataset, 
                   train = train_list$train_stats, 
                   cv = train_list$cv_stats)
    
    ## n numbers
    
    n_tag <- paste0('\nTotal: n = ', nrow(train_list$train_pred), 
                    ', Events: n = ', count(train_list$train_pred, .outcome)[['n']][1])
    
    ## plotting
    
    plot_line(data = data, 
              y_var = 'mean_stat', 
              show_ci = show_ci, 
              show_value = show_value, 
              plot_title = plot_title, 
              plot_subtitle = plot_subtitle, 
              plot_tag = n_tag, 
              dodge_width = dodge_width, 
              connect_line = connect_line)
    
  }
  
  plot_list_roc_stats <- function(train_list, 
                                  dataset = c('train', 'cv'), 
                                  lev = c('yes', 'no'), 
                                  show_value = TRUE, 
                                  plot_title = NULL, 
                                  plot_subtitle = NULL, 
                                  dodge_width = 0.1, 
                                  connect_line = TRUE) {
    
    dataset <- match.arg(dataset[1], c('train', 'cv'))
    
    data <- get_roc(train_list = train_list)[[dataset]] %>% 
      gather(key = 'stat', 
             value = 'mean_stat', 
             ROC, Sens, Spec)
    
    ## n numbers
    
    n_tag <- paste0('\nTotal: n = ', nrow(train_list$train_pred), 
                    ', Events: n = ', count(train_list$train_pred, .outcome)[['n']][1])
    
    ## plotting
    
    plot_line(data = data, 
              y_var = 'mean_stat', 
              show_ci = FALSE, 
              show_value = show_value, 
              plot_title = plot_title, 
              plot_subtitle = plot_subtitle, 
              plot_tag = n_tag, 
              dodge_width = dodge_width, 
              connect_line = connect_line)
    
    
    
  }

  get_roc_strata <- function(preds, lev = c('yes', 'no')) {
    
    ## gets the ROC strata for the whole cohort and the severity groups
    
    pred_lst <- list(cohort = preds, 
                     mild_moderate = filter(preds, ml_sev == 'mild_moderate'), 
                     severe_critical = filter(preds, ml_sev == 'severe_critical'))
    
    pred_lst %>% 
      map(as.data.frame) %>% 
      map(twoClassSummary, lev = lev) %>% 
      map(as.list) %>% 
      map_dfr(as_tibble) %>% 
      mutate(ml_sev = names(pred_lst))
    
    
  }
  
  plot_roc <- function(preds, 
                       lev = c('yes', 'no'), 
                       plot_title = NULL, 
                       plot_subtitle = NULL, 
                       plot_tag = NULL) {
    
    ## plots ROC curves
    
    ## stats to be shown in the plot
    
    pred_stats <- get_roc_strata(preds = preds, 
                                 lev = lev) %>% 
      mutate(plot_lab = paste0('AUC = ', signif(ROC, 2), 
                               ', Se = ', signif(Sens, 2), 
                               ', Sp = ', signif(Spec, 2)), 
             x = 0.47, 
             obs = 'no',
             y = c(0.3, 0.15, 0), 
             yes = 0.4)
    
    ## plotting table
    
    preds <- rbind(preds, 
                   mutate(preds, ml_sev = 'cohort'))
    
    ## n numbers
    
    n_numbers <- preds %>% 
      dlply(.(ml_sev)) %>% 
      map(filter, !duplicated(ID)) %>% 
      map_chr(function(x) paste0('Total: n = ', nrow(x), 
                             ', Events: n = ', count(x, obs)[['n']][1]))

    pred_stats <- pred_stats %>% 
      mutate(n_numbers = n_numbers, 
             plot_lab = paste(plot_lab, n_numbers, sep = '\n'))
    
    ## plot
    
    preds %>% 
      mutate(obs = factor(obs, levels = rev(lev)),
             obs = as.numeric(obs) - 1) %>% 
      ggplot(aes(m = .data[[lev[1]]], 
                 d = as.numeric(obs), 
                 color = ml_sev)) + 
      geom_roc(cutoffs.at = 0.5, 
               labelsize = 0, 
               pointsize = 0.75, 
               size = 0.5) + 
      geom_text(data = pred_stats, 
                aes(label = plot_lab, 
                    x = x, 
                    y = y), 
                size = 2.6, 
                show.legend = FALSE, 
                hjust = 0, 
                vjust = 0) + 
      style_roc() + 
      geom_abline(slope = 1, 
                  intercept = 0, 
                  color = 'gray70') + 
      theme(axis.text = globals$common_text, 
            axis.title = globals$common_text, 
            plot.title = element_text(size = 8, face = 'bold'),
            plot.tag = globals$common_text, 
            plot.tag.position = 'bottom', 
            legend.text = globals$common_text, 
            legend.title = globals$common_text, 
            plot.subtitle = globals$common_text) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag)
    
  }
  
  get_best_args <- function(caret_model) {
    
    ## extracts the optimal parameter combination
    
    best_args <- caret_model$bestTune %>% 
      map_dfc(function(x) if(is.numeric(x)) signif(x, 3) else x)
    
    best_args <- map2_chr(names(best_args), 
                          best_args, 
                          paste, sep = ' = ') %>% 
      paste(collapse = ', ')
    
    mod_details <- getModelInfo(caret_model$method)[[caret_model$method]]
    
    tibble(`function` = caret_model$method, 
           full_name = mod_details$label, 
           package = mod_details$library, 
           best_args = best_args)
    
  }
  
  get_best_list <- function(train_list) {
    
    unclass(train_list$model_list) %>% 
      c(list(ensemble = train_list$ensemble$ens_model)) %>% 
      map(get_best_args) %>% 
      map_dfr(~.x[1, ]) %>% 
      mutate(type = c(rep('model', length(train_list$model_list)), 'ensemble'))
    
  }
  
  plot_mod_cor <- function(train_list, 
                           plot_title = NULL, 
                           plot_subtitle = NULL) {
    
    ## plots correlations of the model performance metrics as a heat map
    
    n_tag <- paste0('\nTotal: n = ', nrow(train_list$train_pred), 
                    ', Events: n = ', count(train_list$train_pred, .outcome)[['n']][1])
    
    methods <- colnames(train_list$correlations)
    
    plotting_tbl <- train_list$correlations %>% 
      as.data.frame %>% 
      rownames_to_column('method1') %>% 
      gather(key = 'method2', 
             value = 'rho', 
             all_of(methods))
    
    plotting_tbl %>% 
      ggplot(aes(x = method1, 
                 y = method2, 
                 fill = rho)) + 
      geom_tile() + 
      geom_text(aes(label = signif(rho, 2)), 
                size = 2.75) + 
      globals$common_theme + 
      theme(plot.background = element_blank(), 
            axis.line = element_blank(), 
            axis.title = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = n_tag)
    
  }
  
  plot_ensemble <- function(train_list,
                            plot_title = NULL, 
                            plot_subtitle = NULL, 
                            x_lab = expression(beta)) {
    
    ### plots the regression estimates for the ensemble model  constructed by glm or glmnet
    
    # plotting table
    
    plotting_tbl <- coef(train_list$ensemble$ens_model$finalModel, 
                         s = train_list$ensemble$ens_model$bestTune$lambda) %>% 
      as.matrix %>% 
      as.data.frame %>% 
      rownames_to_column('classifier') %>% 
      set_names(c('classifier', 'beta')) %>% 
      mutate(classifier = ifelse(classifier == '(Intercept)', 'Intercept', classifier))
    
    ## n numbers
    
    n_tag <- paste0('\nTotal: n = ', nrow(train_list$ensemble$models[[1]]$trainingData), 
                    ', Events: n = ', count(train_list$ensemble$models[[1]]$trainingData, .outcome)[['n']][1])
    
    ## plot
    
    plotting_tbl %>% 
      ggplot(aes(x = beta, 
                 y = reorder(classifier, beta), 
                 color  = beta, 
                 size = abs(beta))) + 
      geom_vline(xintercept = 0, 
                 linetype = 'dashed') + 
      geom_segment(aes(x = 0, 
                       y = reorder(classifier, beta), 
                       xend = beta, 
                       yend = reorder(classifier, beta)), 
                   size = 0.5, 
                   color = 'black') + 
      geom_point(shape = 16) + 
      geom_text(aes(label = signif(beta, 2)), 
                size = 2.75, 
                hjust = 0.5, 
                vjust = -1.6) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = n_tag, 
           x = x_lab)
    
    
  }
  
# END -----