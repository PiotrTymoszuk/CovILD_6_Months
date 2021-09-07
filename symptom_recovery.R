# This script models and displays recovery from the symptoms
# and the CT recovery score

# Data and toolbox ----

  c('./tools/sys_tools.R',
    './tools/counting_tools.R') %>% 
    walk(source)
  
  insert_head()
  
# Data container and globals ----
  
  insert_msg('Globals setup')
  
  cov_recovery <- list()
  
# Symptom frequencies over the time -----
  
  insert_msg('Recovery from the symptoms')
  
  ## models
  
  cov_recovery$symptom_analyses$models <- globals$symptoms %>% 
    map(make_model, 
        inp_data = cov_data$long_data, 
        idep_variable = 'time_months', 
        random_term = '(1|ID)', 
        family = 'binomial') %>% 
    set_names(globals$symptoms)
  
  cov_recovery$symptom_analyses$null_models <- globals$symptoms %>% 
    map(make_model, 
        inp_data = cov_data$long_data, 
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
    map2_dfr(., names(.), function(x, y) tibble(symptom = y, 
                                                p_lrt = x$`Pr(>Chisq)`[2])) %>% 
    mutate(p_adj = p.adjust(p_lrt, 'BH'), 
           symptom_recoded = globals$symptom_labs[symptom], 
           plot_lab = paste(symptom_recoded, 
                            ', p = ', 
                            signif(p_adj, 2)))
    
  
  ## plotting
  
  cov_recovery$symptom_analyses$plotting_tbl <- cov_data$long_data %>% ## a bit hardcoded, needed to include 0%
    ddply(.(time_months), 
          count_feature_lst, 
          var_to_count_vec = globals$symptoms, 
          positive_only = F) %>% 
    dlply(.(feature, time_months), 
          function(x) if(all(x$percent == 100)) rbind(x, mutate(x, percent = 0, feature_strata = ifelse(feature_strata == 1, 0, 1))) else x) %>% 
    reduce(rbind) %>% 
    filter(feature_strata == 1) %>% 
    as_tibble
  
  cov_recovery$symptom_analyses$plot <- cov_recovery$symptom_analyses$plotting_tbl %>% 
    ggplot(aes(x = time_months, 
               y = percent, 
               fill = feature, 
               color = feature, 
               group = feature)) + 
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
    theme(panel.background = element_rect(fill = 'gray95'), 
          panel.grid.major = element_line(color = 'white')) + 
    labs(x = 'Time post COVID-19, days', 
         y = '% cohort', 
         title = 'Symptom recovery')

# Any symptoms (long-COVID over time) ------
  
  insert_msg('Persistens symptoms in patient groups')
  
  ## count table
  
  cov_recovery$long_covid$count_tbl <- cov_data$long_data %>% 
    ddply(.(time_months, pat_group), 
          count_feature, 
          var_to_count = 'sympt_present') %>% 
    dlply(.(pat_group, time_months), 
          function(x) if(all(x$percent == 100)) rbind(x, mutate(x, percent = 0, sympt_present = ifelse(sympt_present == 1, 0, 1))) else x) %>% 
    reduce(rbind) %>% 
    filter(sympt_present == 1) %>% 
    as_tibble

  ## models, for each patient group, LRT. Adjusted p values are displayed in the plot
  
  cov_recovery$long_covid$models <- cov_data$long_data %>% 
    dlply(.(pat_group), 
          function(x) glmer(sympt_present ~ time + (1|ID), 
                            family = 'binomial', 
                            data = x))
  
  cov_recovery$long_covid$null_models <- cov_data$long_data %>% 
    dlply(.(pat_group), 
          function(x) glmer(sympt_present ~ 1 + (1|ID), 
                            family = 'binomial', 
                            data = x))
  
  cov_recovery$long_covid$lrt <- map2(cov_recovery$long_covid$models, 
                                      cov_recovery$long_covid$null_models, 
                                      anova, 
                                      test = 'Chisq')
  
  cov_recovery$long_covid$lrt_summary <- cov_recovery$long_covid$lrt %>% 
    map2_dfr(., names(.), 
             function(x, y) tibble(pat_group = y, 
                                   p_lrt = x$`Pr(>Chisq)`[2])) %>% 
    mutate(p_lrt_adj = p.adjust(p_lrt, 'BH'), 
           p_lab = paste('p =', signif(p_lrt_adj, 2)))
  
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
              size = 2.75, 
              fontface = 'bold') + 
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