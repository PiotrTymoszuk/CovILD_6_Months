# This script prepares the tables and supplementary tables for the manuscript

# data and toolbox ------

  source('./tools/sys_tools.R')
  source('./tools/modeling_tools.R')
  
  if(!'cov_data' %in% ls()) {
    
    source('data_import.R')
    
  }

  library(writexl)

  insert_head()

# data container and globals ----

  cov_tabglobs <- list()
  cov_tables <- list()
  
  cov_tabglobs$var_glossary <- c(globals$mod_var_labels, 
                                 globals$resp_labels) %>% 
    stri_replace(fixed = '\n', replacement = ' ') %>% 
    set_names(c(names(globals$mod_var_labels), 
                names(globals$resp_labels))) %>% 
    c('ID' = 'Participant ID', 
      'time' = 'Visit index', 
      'pat_group' = 'Severity group', 
      'sympt_present' = 'Symptom presence', 
      'dyspnoe' = 'Dyspnoe', 
      'cough' = 'Cough', 
      'fever' = 'Fever', 
      'night_sweat' = 'Night sweating',
      'anosmia' = 'Hyposmia/anosmia', 
      'ECOG_imp' = 'Impaired performance', 
      'sleep_disorder' = 'Sleeping problems', 
      'time_months' = 'Visit time point, months', 
      'long_covid' = 'Symptom presence', 
      'CTsevabove5' = 'CT Severity Score > 5', 
      'CT_findings' = 'CT pathologies', 
      'CT_pat_GGO' = 'GGO (Ground Glass Opacities) present in CT', 
      'lung_function_impaired' = 'Any impairment of lung function')
    
  
  
# Table S1: study variables -----
  
  insert_msg('Table S1, study variables')
  
  cov_tables$study_vars <- list(tibble(var_name = names(cov_recovery$analysis_tbl$symptoms), 
                                       time_point = 'longitudinal'), 
                                tibble(var_name = names(cov_lung$analysis_tbl), 
                                       time_point = 'longitudinal'), 
                                tibble(var_name = globals$clust_vars, 
                                       time_point = stri_extract(globals$clust_vars, 
                                                                 regex = 'V\\d{1}$'))) %>% 
    reduce(rbind) %>% 
    filter(!duplicated(var_name)) %>% 
    mutate(var_label = cov_tabglobs$var_glossary[var_name])
  
# Table S2: results of risk modeling -----
  
  insert_msg('Table S2: results of univariate risk modeling')
  
  cov_tables$univariate_modeling <- cov_univariate$summary_tbl %>% 
    mutate(Variable = cov_tabglobs$var_glossary[parameter], 
           Response = cov_tabglobs$var_glossary[response], 
           OR = signif(estimate, 3), 
           `2.5% CI` = signif(lower_ci, 3), 
           `97.5% CI` = signif(upper_ci, 3), 
           `raw p` = signif(p_value, 3), 
           pFDR = signif(p_adj, 3)) %>% 
    select(Variable, Response, OR, `2.5% CI`, `97.5% CI`, `raw p`, pFDR) %>% 
    arrange(Response, desc(OR))
  
# Table S3: cluster assignment of the clinical features -----
  
  insert_msg('Table S3: cluster assignment of the clinical features')
  
  cov_tables$clust_assign <- cov_multiclust$kmeans_ass %>% 
    mutate(Variable = cov_tabglobs$var_glossary[feature], 
           Cluster = cov_multiclust$clust_labs[kmeans_id]) %>% 
    dlply(.(Cluster), function(x) paste(x$Variable, collapse = ', ')) %>% 
    tibble(Cluster = names(.), 
           Features = unlist(.)) %>% 
    select(Cluster, Features)
  
# Table S4 and S5: QC of the kNN approach ------
  
  ## kNN
  
  cov_tables$kNN_qc <- cov_knn$qc_plot$stat_tbl[c('CT_findings_V3', 
                                                  'CT_pat_GGO_V3', 
                                                  'CTsevabove5_V3')] %>% 
    map(filter, 
        pred_method == 'knn') %>% 
    map(mutate, 
        tab_content = paste(signif(expected, 2), 
                            ' [95%CI: ', 
                            signif(lower_ci, 2), 
                            ' to ', 
                            signif(upper_ci, 2), 
                            '], p = ', 
                            signif(p_value, 2), sep = '')) %>% 
    map(select, 
        stat, 
        tab_content) %>% 
    map2(., 
         names(.), 
         function(x, y) set_names(x, 
                                  stri_replace(c('Parameter', globals$resp_labels[y]), 
                                               fixed = '\n', replacement = ' '))) %>% 
    reduce(left_join, by = 'Parameter')
  
  ## Bayes
  
  cov_tables$bayes_qc <- cov_knn$qc_plot$stat_tbl[c('CT_findings_V3', 
                                                    'CT_pat_GGO_V3', 
                                                    'CTsevabove5_V3')] %>% 
    map(filter, 
        pred_method == 'naive_bayes') %>% 
    map(mutate, 
        tab_content = paste(signif(expected, 2), 
                            ' [95%CI: ', 
                            signif(lower_ci, 2), 
                            ' to ', 
                            signif(upper_ci, 2), 
                            '], p = ', 
                            signif(p_value, 2), sep = '')) %>% 
    map(select, 
        stat, 
        tab_content) %>% 
    map2(., 
         names(.), 
         function(x, y) set_names(x, 
                                  stri_replace(c('Parameter', globals$resp_labels[y]), 
                                               fixed = '\n', replacement = ' '))) %>% 
    reduce(left_join, by = 'Parameter')
  
# Saving the tables -----
  
  cov_tables[c('study_vars', 
               'univariate_modeling', 
               'clust_assign', 
               'kNN_qc',
               'bayes_qc')] %>% 
    walk2(., 
          paste('./tables/Table_', 1:length(cov_tables), '_', names(.), '.xlsx', sep = ''), 
          write_xlsx)
  
# END -----
  
  insert_tail()
  