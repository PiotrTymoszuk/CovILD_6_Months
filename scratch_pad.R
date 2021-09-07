# This script gets some figures and stats for the manuscript

# libraries -----

  require(plyr)
  require(tidyverse)
  library(e1071)

  source('./tools/counting_tools.R')

# data container -----

  scratch_data <- list()

# Long COVID prevalence and symptom precentages in the entire cohort @V3-----
  
  insert_msg('long COVID prevalence and symptoms')

  scratch_data$long_covid <- cov_recovery$analysis_tbl$symptoms %>% 
    filter(time == 3) %>%  
    count_feature(var_to_count = 'sympt_present')
  
  scratch_data$long_symptoms <- cov_recovery$symptoms %>%
    map(count_feature, 
        inp_tbl = cov_recovery$analysis_tbl$symptoms %>% 
          filter(time == 3) %>% 
          mutate()) %>% 
    map(set_names, 
        c('strata', 'n', 'percent', 'total_n')) %>% 
    set_names(cov_recovery$symptoms) %>% 
    map2_dfr(., names(.), 
             function(x, y) mutate(x, symptom = y))  %>% 
    filter(strata == 1) %>% 
    arrange(percent)
  
# Prevalence of lung pathologies ----
  
  insert_msg('Prevalence of lung pathologies')
  
  scratch_data$long_lung <- cov_lung$lung_vars %>%
    map(count_feature, 
        inp_tbl = cov_lung$analysis_tbl %>% 
          filter(time == 3) %>% 
          mutate()) %>% 
    map(set_names, 
        c('strata', 'n', 'percent', 'total_n')) %>% 
    set_names(cov_lung$lung_vars) %>% 
    map2_dfr(., names(.), 
             function(x, y) mutate(x, patho = y))  %>% 
    filter(strata == 1) %>% 
    arrange(percent)
  
# Strongest predictive factors for persisting lung patho -----
  
  insert_msg('Strongest risk factors for lung CT patho')

  
  cov_univariate$summary_tbl %>% 
    filter(response == 'CT_findings_V3', 
           regulation != 'ns') %>% 
    arrange(desc(estimate)) %>% 
    select(parameter, estimate, lower_ci, upper_ci)

# n numbers of the severity group -----
  
  insert_msg('N numbers of the severity group')
  
  scratch_data$n_numbers_pat_group <- cov_data$mod_tbl %>% 
    count_feature('pat_group_V0')

# some features for the table 1 ----
  
  insert_msg('Data for Table 1')
  
  scratch_data$table1$age <- list(mean_age = mean, 
                                  sd_age = sd) %>% 
    map(function(x) x(cov_data$mod_tbl$age_V0))
    
  scratch_data$table1$percents <- c('sex_male_V0', 
                                    'obesity_rec_V0',
                                    'overweight_V0', 
                                    'smoking_ex_V0', 
                                    'current_smoker_V0', 
                                    'comorb_present_V0', 
                                    'CVDis_rec_V0', 
                                    'PDis_rec_V0', 
                                    'endocrine_metabolic_rec_V0', 
                                    'CKDis_rec_V0', 
                                    'GITDis_rec_V0', 
                                    'malignancy_rec_V0', 
                                    'pat_group_G2_V0') %>% 
    map(count_feature, 
        inp_tbl = cov_data$mod_tbl)
  
# Severity stratification of the inpatients ----
  
  insert_msg('Severity within the inpatient subset')
  
  scratch_data$severity_inpatient <- cov_data$mod_tbl %>% 
    filter(pat_group_G2_V0 == 'yes') %>% 
    count_feature('pat_group_V0')
  
  

  
# Sleep disorders at the onset and at the visit 3 -----
  
  insert_msg('Prevalence of sleep disorders')
  
  scratch_data$sleep_disorders <- cov_data$data_tbl %>% 
    dlply(.(time), count_feature, 'sleep_disorder')
  
# OR for IL-6 and CRP at the V1 -----
  
  insert_msg('IL6 and CRP OR values')
  
  scratch_data$il6_crp_or <- cov_univariate$summary_tbl %>% 
    filter(parameter %in% c('IL6_elv_rec_V1', 'CRP_elv_rec_V1')) %>% 
    arrange(parameter) %>% 
    select(parameter, response, plot_label)
  
# ORs for the lung pathology/CT findings in the participant clusters -----
  
  insert_msg('ORs for the lung abnormalities in the clusters')
  
  scratch_data$lungct_clusters <- cov_partclust$ct_prevalence$logis_summaries %>% 
    filter(feature %in% globals$mod_resp$response) %>% 
    select(feature, kmeans_id, plot_label)
  
# QC of the kNN predictions -----
  
  insert_msg('QC of the kNN predictions')
  
  scratch_data$knn_predictions <- cov_knn$qc_plot$stat_tbl
  
  
# rehabilitation in the severity groups ----
  
  insert_msg('Percent of the subjects undergoing rehabilitation in the severity groups')
  
  scratch_data$reha_pat_groups <- cov_data$data_tbl %>% 
    dlply(.(pat_group), count, postacute_rehabilitation_rec)
  
# development ------