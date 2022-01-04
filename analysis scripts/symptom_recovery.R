# This script models and displays recovery from the symptoms
# and the CT recovery score

  insert_head()
  
# Data container and globals ----
  
  insert_msg('Globals setup, analysis table with the complete longitudinal symptom record')
  
  cov_recovery <- list()
  
  cov_recovery$analysis_tbl <- cov_data$long_data %>% 
    select(ID, time_months, time, pat_group, sympt_present, all_of(globals$symptoms)) %>% 
    complete_cases(long_no = 4)
  
# Symptom frequencies over the time -----
  
  insert_msg('Recovery from the symptoms')
  
  ## models
  
  cov_recovery$symptom_analyses$models <- globals$symptoms %>% 
    map(make_model, 
        inp_data = cov_recovery$analysis_tbl, 
        idep_variable = 'time_months', 
        random_term = '(1|ID)', 
        family = 'binomial') %>% 
    set_names(globals$symptoms)
  
  cov_recovery$symptom_analyses$null_models <- globals$symptoms %>% 
    map(make_model, 
        inp_data = cov_recovery$analysis_tbl, 
        idep_variable = '1', 
        random_term = '(1|ID)', 
        family = 'binomial') %>% 
    set_names(globals$symptoms)
  
  ## LRT used as p values to be shown in the plots
  
  cov_recovery$symptom_analyses$lrt <- map2(cov_recovery$symptom_analyses$models, 
                                            cov_recovery$symptom_analyses$null_models, 
                                            anova)
  
  ## model summaries, p correction by Benjamini-Hochberg
  
  cov_recovery$symptom_analyses$summaries <- cov_recovery$symptom_analyses$lrt %>% 
    map2_dfr(., names(.), ~tibble(symptom = .y, 
                                  p_lrt = .x$`Pr(>Chisq)`[2])) %>% 
    mutate(p_adj = p.adjust(p_lrt, 'BH'), 
           symptom_recoded = globals$symptom_labs[symptom], 
           plot_lab = ifelse(p_adj < 0.05, 
                             paste0(symptom_recoded, 
                                    ', p = ', 
                                    signif(p_adj, 2)), 
                             paste0(symptom_recoded, 
                                    ', ns (p = ', 
                                    signif(p_adj, 2), ')')))
    
  
  ## plotting table with the percents of the individuals with the given symptom
  
  cov_recovery$symptom_analyses$plotting_tbl <- globals$symptoms %>% 
    map(analyze_feature, 
        inp_tbl = cov_recovery$analysis_tbl %>% 
          map_dfr(function(x) if(is.numeric(x)) factor(x) else x), 
        split_var = 'time_months') %>% 
    map_dfr(extract_counts) %>% 
    filter(strata == 1) %>% 
    mutate(time_months = as.numeric(as.character(split_var)))
  
  ## plot

  cov_recovery$symptom_analyses$plot <- cov_recovery$symptom_analyses$plotting_tbl %>% 
    ggplot(aes(x = time_months, 
               y = percent, 
               fill = variable, 
               color = variable, 
               group = variable)) + 
    geom_line(size = 0.75) + 
    geom_point(size = 2, 
               shape = 21, 
               color = 'black') + 
    scale_fill_manual(values = globals$symptom_colors, 
                      labels = cov_recovery$symptom_analyses$summaries$plot_lab %>% 
                        set_names(cov_recovery$symptom_analyses$summaries$symptom), 
                      name = '') + 
    scale_color_manual(values = globals$symptom_colors, 
                       labels = cov_recovery$symptom_analyses$summaries$plot_lab %>% 
                         set_names(cov_recovery$symptom_analyses$summaries$symptom), 
                      name = '') + 
    scale_x_continuous(breaks = c(0, 2, 3, 6), 
                       labels = c('0' = 0, 
                                  '2' = 60, 
                                  '3' = 100, 
                                  '6' = 180)) + 
    globals$common_theme + 
    labs(x = 'Time post COVID-19, days', 
         y = '% cohort', 
         title = 'Symptom recovery', 
         tag = paste('n =', nrow(cov_recovery$analysis_tbl)/4))

# Any symptoms (long-COVID over time) ------
  
  insert_msg('Persistent symptoms in patient groups')
  
  ## models, for each patient group, LRT. Adjusted p values are displayed in the plot
  
  cov_recovery$long_covid$models <- cov_recovery$analysis_tbl %>% 
    dlply(.(pat_group), 
          function(x) glmer(sympt_present ~ time + (1|ID), 
                            family = 'binomial', 
                            data = x))
  
  cov_recovery$long_covid$null_models <- cov_recovery$analysis_tbl %>% 
    dlply(.(pat_group), 
          function(x) glmer(sympt_present ~ 1 + (1|ID), 
                            family = 'binomial', 
                            data = x))
  
  cov_recovery$long_covid$lrt <- map2(cov_recovery$long_covid$models, 
                                      cov_recovery$long_covid$null_models, 
                                      anova, 
                                      test = 'Chisq')
  
  cov_recovery$long_covid$lrt_summary <- cov_recovery$long_covid$lrt %>% 
    map2_dfr(., names(.), ~tibble(pat_group = .y, p_lrt = .x$`Pr(>Chisq)`[2])) %>% 
    mutate(p_lrt_adj = p.adjust(p_lrt, 'BH'), 
           p_lab = paste('p =', signif(p_lrt_adj, 2)))
  
  
  ## percent table for plotting
  
  cov_recovery$long_covid$count_tbl <- cov_recovery$analysis_tbl %>% 
    map_dfr(function(x) if(is.numeric(x)) factor(x) else x) %>% 
    mutate(pat_group = factor(pat_group)) %>% 
    dlply(.(pat_group)) %>% 
    map(analyze_feature, 
        variable = 'sympt_present', 
        split_var = 'time_months') %>% 
    map(extract_counts) %>% 
    map2_dfr(., names(.), ~mutate(.x, pat_group = .y)) %>% 
    filter(strata == 1) %>% 
    mutate(time_months = as.numeric(as.character(split_var)))
  
  ## n numbers
  
  cov_recovery$long_covid$n_tag <- cov_recovery$long_covid$count_tbl %>% 
    dlply(.(pat_group)) %>% 
    map_dbl(~.x$total_n[1])
  
  cov_recovery$long_covid$lrt_summary <- cov_recovery$long_covid$lrt_summary %>% 
    mutate(n = cov_recovery$long_covid$n_tag, 
           p_lab = paste(p_lab, n, sep = '\nn = '))

  ## plot
  
  cov_recovery$long_covid$plot <- cov_recovery$long_covid$count_tbl %>% 
    mutate(perc_lab = paste(signif(percent, 2), '%', sep = '')) %>% 
    ggplot(aes(x = time_months, 
               y = percent)) + 
    facet_grid(. ~ pat_group, 
               labeller = as_labeller(globals$pat_group_labels)) + 
    geom_line(color = 'gray40') + 
    geom_point(shape = 21, 
               size = 2, 
               color = 'gray40', 
               fill = 'gray40') + 
    geom_text(aes(label = perc_lab), 
              size = 2.5, 
              hjust = 0, 
              vjust = 0, 
              nudge_y = 0.7, 
              nudge_x = 0.2) + 
    geom_text(data = cov_recovery$long_covid$lrt_summary %>% 
                mutate(time_months = 2, 
                       percent = 20), 
              aes(label = p_lab), 
              size = 2.75) + 
    expand_limits(y = 0, 
                  x = 6.9) + 
    expand_limits(y = 110) + 
    scale_y_continuous(breaks = seq(0, 100, by = 25)) + 
    scale_x_continuous(breaks = c(0, 2, 3, 6), 
                       labels = c('0' = 0, 
                                  '2' = 60, 
                                  '3' = 100, 
                                  '6' = 180)) + 
    globals$common_theme + 
    theme(panel.background = element_rect(fill = 'gray95'), 
          panel.grid.major = element_line(color = 'white')) + 
    labs(title = 'Persistent symptoms', 
         x = 'Time post-COVID-19, days', 
         y = '% patient subgroup')
  
# END ----
  
  insert_tail()