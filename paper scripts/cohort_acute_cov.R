# This script calculates means, medians and frequency of features of acute COVID-19

  insert_head()
  
# globals and analysis table -----
  
  insert_msg('Globals setup')
  
  acute <- list()
  
  acute$variables <- c('hosp_7d_V0', 
                       'treat_antiinfec_rec_V0', 
                       'treat_antiplat_rec_V0', 
                       'treat_anticoag_rec_V0', 
                       'treat_immunosuppr_rec_V0')
  
  acute$analysis_tbl <- left_join(cov_data$mod_tbl, 
                                  cov_data$long_data %>% 
                                    filter(time == 0) %>% 
                                    select(ID, pat_group), 
                                  by = 'ID') %>% 
    select(pat_group, 
           all_of(acute$variables)) %>% 
    map_dfc(factor)

# serial analysis -----
  
  insert_msg('Serial analysis')
  
  ## analyses
  
  acute$analyses <- acute$variables %>% 
    map(analyze_feature, 
        inp_tbl = acute$analysis_tbl, 
        split_var = 'pat_group')

  ## summaries
  
  acute$summary <- list(analysis_obj = acute$analyses, 
                        label = stri_replace(globals$mod_var_labels[acute$variables], fixed = '\n', replacement = ' ')) %>%
    pmap_dfr(get_feature_summary) %>%
    mutate(p_chi = p.adjust(p_chi, 'BH'), 
           significance = ifelse(p_chi < 0.05, 
                                 paste('p =', signif(p_chi, 2)), 
                                 paste0('ns (p = ', signif(p_chi, 2), ')'))) %>% 
    map_dfc(stri_replace, regex = '^.*\nyes:\\s{1}', replacement = '') %>% 
    select(label, G1, G2, G3, G4, significance) %>% 
    set_names(c('Parameter', 
                stri_replace(globals$pat_group_labels, fixed = '\n', replacement = ' '), 
                'pFDR'))
  
# END -----
  
  insert_tail()

  
  

  