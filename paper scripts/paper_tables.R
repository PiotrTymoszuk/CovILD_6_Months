# This script prepares the tables and supplementary tables for the manuscript

  insert_head()

# data container and globals ----

  paper_tables <- list()
  suppl_tables <- list()
  
# Table 2 and 3: acute COVID-19 treatment and number of the participants reaching the study endpoints -----
  
  insert_msg('Table 2 and 3')
  
  paper_tables[c('acute', 'outcome')] <- list(acute$summary, 
                                              outcome$summary) %>% 
    map(~map_dfc(.x, stri_replace, fixed = 'ncomplete', replacement = 'complete: n'))

# Table S1: study variables -----
  
  insert_msg('Table S1, study variables')
  
  suppl_tables$study_vars <- list(globals$mod_vars[c('idep_variable', 
                                                     'reference', 
                                                     'label', 
                                                     'var_type', 
                                                     'cutoff')] %>% 
                                    filter(var_type == 'binary') %>% 
                                    mutate(var_type = 'explanatory'), 
                                  globals$mod_resp[c('response', 'reference', 'label')] %>% 
                                    mutate(var_type = 'outcome', 
                                           cutoff = NA)) %>%
    map_dfr(set_names, c('Variable', 
                         'Reference time point', 
                         'Label', 
                         'Variable type', 
                         'Stratification cutoff')) %>% 
    mutate(`Reference time point` = ifelse(`Reference time point` == 0, 'V0: acute COVID-19', 
                                           ifelse(`Reference time point` == 1, 'V1: 60-day follow-up', '180-day follow-up'))) %>% 
    map_dfr(stri_replace, fixed = '\n', replacement = ' ')
  
# Table S2: results of univariate risk modeling -----
  
  insert_msg('Table S2: univariate risk modeling')
  
  suppl_tables$uni_risk <- cov_univariate$summary_tbl[names(cov_univariate$summary_tbl) != 'CT_sev_low_V3'] %>% 
    map_dfr(mutate, 
            response = globals$resp_labels[response], 
            variable = globals$mod_var_labels[variable], 
            baseline = paste('no', decapitalize(variable)),
            variable = paste0(variable, ', n = ', n_level), 
            baseline = paste0(baseline, ', n = ', n_complete - n_level), 
            estimate = paste0(signif(estimate, 3), ' [', signif(lower_ci, 3), ' - ', signif(upper_ci, 3), ']'),
            significance = ifelse(regulation == 'ns', 
                                  paste0('ns (p = ', signif(p_adj, 2), ')'), 
                                  paste('p =', signif(p_adj, 2)))) %>% 
    select(response, variable, baseline, n_complete, estimate, significance) %>% 
    map_dfr(stri_replace, fixed = '\n', replacement = ' ') %>% 
    set_names(c('Outcome', 'Co-variate', 'Baseline', 'Complete cases', 'OR', 'pFDR'))
  
# Table S3: assignment of the clinical features to the clusters -----
  
  insert_msg('Table S3: Assignment of the clinical features to the clusters')
  
  suppl_tables$clust_assignment <- ft_clust$test_clust$clust_obj %>% 
    extract('assignment') %>% 
    mutate(observation = ft_clust$ft_labels[observation]) %>% 
    arrange(clust_id) %>% 
    set_names(c('Variable', 'Cluster'))
  
  suppl_tables$clust_assignment <- suppl_tables$clust_assignment %>% 
    group_by(Cluster) %>% 
    summarise(Variable = paste(Variable, collapse = ', '))
  
# Table S4: optimal parameters of the ML algorithms ----
  
  insert_msg('Table S4: optimal algorithms')
  
  suppl_tables$optimal_algos <- ml$models %>% 
    map(get_best_list) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = globals$resp_labels[.y])) %>%  
    map_dfc(stri_replace, fixed = '\n', replacement = ' ') %>% 
    select(response, type, `function`, full_name, package, best_args) %>% 
    set_names(c('Outcome', 'Classifier type', 'Caret method', 'Description', 'Package', 'Optimal arguments'))

# Table S5: Stats of the ML algorithms ----
  
  insert_msg('Table S5: Stats of the ML algorithms, complete cohort')
  
  ## kappa and accuracy
  
  suppl_tables$ml_stats_complete <- ml$models %>% 
    map(~rbind(mutate(.x$train_stats, dataset = 'training'), 
               mutate(.x$cv_stats, dataset = 'cv')) %>% 
          mutate(mean_stat = ifelse(dataset == 'cv', 
                                    paste0(signif(mean_stat, 2), ' [', signif(lower_ci, 2), ' - ', signif(upper_ci, 2), ']'), 
                                    signif(mean_stat, 2)))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = globals$resp_labels[.y])) %>%
    select(response, method, dataset, stat, mean_stat) %>% 
    spread(key = stat, value = mean_stat)
  
  ## ROC stats
    
  suppl_tables$ml_stats_complete <- ml$roc %>% 
    map(~rbind(mutate(.x$train, dataset = 'training'), 
               mutate(.x$cv, dataset = 'cv'))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = globals$resp_labels[.y])) %>%
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    left_join(suppl_tables$ml_stats_complete, ., by = c('response', 'method', 'dataset'))
  
  ## n numbers
  
  suppl_tables$ml_stats_complete <- ml$analysis_tbl %>% 
    map(~tibble(n_total = nrow(.x), 
                n_events = sum(.x[[1]] == 'yes'))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = globals$resp_labels[.y])) %>% 
    left_join(suppl_tables$ml_stats_complete, ., by = 'response') %>% 
    map_dfc(stri_replace, fixed = '\n', replacement = ' ') %>% 
    select(response, n_total, n_events, method, dataset, Accuracy, Kappa, ROC, Sens, Spec) %>% 
    set_names(c('Outcome', 'Total N', 'Events N', 'Method', 'Data set', 'Accuracy', 'Kappa', 'AUC', 'Sensitivity', 'Specificity'))
    
# Table S6: Specificity and sensitivity of the ML algorithms, CoV severity -----
  
  insert_msg('Table S6: Sensitivity and specificity of the ML algorithms, severity groups')
  
  suppl_tables$ml_roc_severity <- ml_detail[c('train_roc', 'cv_roc')] %>% 
    map(~map2_dfr(.x, names(.x), ~mutate(.x, response = globals$resp_labels[.y]))) %>%
    map2_dfr(., c('training', 'cv'), ~mutate(.x, dataset = .y)) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    select(response, method, ml_sev, dataset, ROC, Sens, Spec)
  
  ## n numbers of complete observations
  
  suppl_tables$ml_roc_severity  <- ml_detail$train_predictions %>% 
    map(~.x[[1]]) %>%
    map(~rbind(mutate(.x, ml_sev = 'cohort'), 
               .x)) %>% 
    map(~ddply(.x, 'ml_sev', function(x) nrow(x)) %>% 
          set_names(c('ml_sev', 'n_total'))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = globals$resp_labels[.y])) %>% 
    left_join(suppl_tables$ml_roc_severity , ., by = c('response', 'ml_sev'))
  
  
  ## n numbers of events
  
  suppl_tables$ml_roc_severity <- ml_detail$train_predictions %>% 
    map(~.x[[1]]) %>%
    map(~rbind(mutate(.x, ml_sev = 'cohort'), 
               .x)) %>% 
    map(~group_by(.x, .data[['ml_sev']]) %>% 
          count(obs) %>% 
          ungroup %>% 
          filter(obs == 'yes') %>% 
          select( - obs) %>% 
          set_names(c('ml_sev', 'n_events'))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = globals$resp_labels[.y])) %>% 
    left_join(suppl_tables$ml_roc_severity, ., by = c('response', 'ml_sev')) %>% 
    map_dfc(stri_replace, fixed = '\n', replacement = ' ') %>% 
    mutate(ml_sev = car::recode(ml_sev, 
                                "'cohort' = 'whole cohort'; 
                                'mild_moderate' = 'mild - moderate COVID-19'; 
                                'severe_critical' = 'severe - critical COVID-19'")) %>% 
    select(response, ml_sev, n_total, n_events, method, dataset, ROC, Sens, Spec) %>% 
    set_names(c('Outcome', 'Cohort subset', 'Total N', 'Events N', 'Method', 'Data set', 'AUC', 'Sensitivity', 'Specificity'))

# Saving the tables -----
  
  paper_tables %>% 
    set_names(paste('Table', 2:(length(paper_tables) + 1))) %>% 
    write_xlsx(path = './paper/tables.xlsx')
  
  suppl_tables %>% 
    set_names(paste('Table', 1:length(suppl_tables))) %>% 
    write_xlsx(path = './paper/Appendix 1.xlsx')
  
# END -----
  
  insert_tail()
  