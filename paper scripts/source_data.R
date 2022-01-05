# This script extracts and seves the source data for the main figures

  insert_head()
  
# container list ----
  
  src_data <- list()
  
# globals ----
  
  src_data$sev_recoding <- "'G1' = 'Outpatient'; 
                            'G2' = 'Hospitalized'; 
                            'G3' = 'Hospitalized oxygen therapy'; 
                            'G4' = 'Hospitalized ICU'"

# Figure 2 -----
  
  insert_msg('Source data for Figure 2')
  
  src_data$figure_2_source_data_1 <- cov_recovery$analysis_tbl %>% 
    mutate(time_months = factor(time_months), 
           time = factor(time), 
           pat_group = car::recode(pat_group, src_data$sev_recoding)) %>% 
    map_dfc(function(x) if(is.numeric(x)) factor(ifelse(x == 0, 'no', 'yes')) else x) %>% 
    set_names(c('ID', 
                'Follow-up, months', 
                'Visit number', 
                'Severity', 
                'Symptoms present', 
                'Dyspnea', 
                'Cough', 
                'Fever', 
                'Night sweating', 
                'Hyposmia', 
                'Imp. performance', 
                'Sleep disorder', 
                'Pain', 
                'GI symptoms'))
  
# Figure 3 ----
  
  insert_msg('Source data for Figure 3')
  
  src_data$figure_3_source_data_1 <- cov_lung$analysis_tbl %>% 
    mutate(time_months = factor(time_months), 
           pat_group = car::recode(pat_group, src_data$sev_recoding)) %>% 
    map_dfc(function(x) if(is.numeric(x)) factor(ifelse(x == 0, 'no', 'yes')) else x) %>% 
    set_names(c('ID', 
                'Follow-up. months', 
                'Severity', 
                'CT abnormalities', 
                'CT sev. score 1 - 5', 
                'CT sev. score over 5', 
                'LF impaired', 
                'CT abnormality grading'))
  
# Figure 4 and 5 -----
  
  insert_msg('Source data for Figure 4 and 5')

  src_data$figure_4_and_5_source_data_1 <- suppl_tables$uni_risk

# Figure 6 -----
  
  insert_msg('Source data for Figure 6')
  
  src_data$figure_6_source_data_1 <- suppl_tables$clust_assignment

# Figure 7 and 8 ----
  
  insert_msg('Source data for Figure 7 and 8')
  
  src_data$figure_7_and_8_source_data_1 <- clust_outcome$analysis_tbl %>% 
    mutate(pat_group = car::recode(pat_group, src_data$sev_recoding)) %>% 
    set_names(c('SOM node', 
                'Risk cluster', 
                'CT abnormality', 
                'CT sev. score 1 - 5', 
                'CT sev. score over 5', 
                'Symptoms present', 
                'LF impaired', 
                'Severity',
                'Dyspnea @V3', 
                'Cough @V3', 
                'Fever @V3', 
                'Night sweating @V3', 
                'Hyposmia @V3',
                'Imp. performance @V3', 
                'Sleep disorder @V3', 
                'Pain @V3', 
                'GI symptoms @V3', 
                'CT abnormailty grading', 
                'ID'))
  
# Figure 9 -----
  
  insert_msg('Source data for Figure 9')
  
  src_data$figure_9_source_data_1 <- suppl_tables$ml_stats_complete
  
# Figure 10 ----
  
  insert_msg('Source data for Figure 10')
  
  src_data$figure_10_source_data_1 <- suppl_tables$ml_roc_severity
  
# Saving teh tables on the disc -----
  
  insert_msg('Saving the tables on the disc')
  
  src_data[names(src_data) != 'sev_recoding'] %>% 
    set_names(stri_replace_all(names(src_data)[names(src_data) != 'sev_recoding'], 
                               fixed = '_', 
                               replacement = ' ')) %>% 
    write_xlsx('./paper/source_data.xlsx')
  
# END -----
  
  insert_tail()
    

  