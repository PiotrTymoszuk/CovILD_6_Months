# This script calculates the number of participants reaching the study outcomes

  insert_head()
  
# globals and analysis table -----
  
  insert_msg('Globals setup')
  
  outcome <- list()
  
  outcome$analysis_tbl <- cov_data$long_data %>% 
    select(ID, time_months, all_of(globals$mod_resp$variable)) %>% 
    filter(time_months != 0) %>% 
    map_dfc(factor)
  
  outcome$analysis_tbl <- globals$mod_resp$variable %>% 
    map(~select(outcome$analysis_tbl, ID, time_months, .x)) %>% 
    map(complete_cases) %>% 
    set_names(globals$mod_resp$variable)

# serial analysis -----
  
  insert_msg('Serial analysis')
  
  ## analyses
  
  outcome$analyses <- list(variable = globals$mod_resp$variable, 
                           inp_tbl = outcome$analysis_tbl) %>% 
    pmap(analyze_feature, 
        split_var = 'time_months')
  
  ## summaries
  
  outcome$summary <- list(analysis_obj = outcome$analyses, 
                          label = globals$resp_labels) %>%
    pmap_dfr(get_feature_summary) %>% 
    map_dfc(stri_replace, regex = '^.*\n1:\\s{1}', replacement = '') %>% 
    select(label, `2`, `3`, `6`) %>% 
    set_names(c('Outcome', '2-month FUP', '3-month FUP', '6-month FUP'))
  
# END -----
  
  insert_tail()

  
  

  