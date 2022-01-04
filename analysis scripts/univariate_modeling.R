# This script performs univariate modeling for variables @V0 and CT pathology responses @V3
# The method of choice is logistic regression

  insert_head()
  
# data container and globals -----
  
  insert_msg('Globals setup')
  
  cov_univariate <- list()
  
  ## list of binary modeling variables
  
  cov_univariate$mod_vars <- globals$clust_vars[!globals$clust_vars %in% globals$mod_resp$response]
  
  ## n numbers for the modeling variables
  
  cov_univariate$n_tables <- globals$mod_resp$response %>% 
    map(~filter(cov_data$mod_tbl, !is.na(.data[[.x]]))) %>% 
    map(function(response) map(cov_univariate$mod_vars, 
                               ~count(response, .data[[.x]])) %>% 
          map(~filter(.x, complete.cases(.x))) %>% 
          set_names(cov_univariate$mod_vars) %>% 
          map(set_names, c('level', 'n_level')) %>% 
          map2_dfr(., names(.), ~mutate(.x, variable = .y)))

# Serial modeling: generating one-independent-variable models ----
  
  insert_msg('Serial univariate modeling')
  
  cov_univariate$mod_results <- globals$mod_resp$response %>% 
    map(make_lm_model, 
        data = cov_data$mod_tbl %>% 
          mutate(CT_findings_V3 = car::recode(CT_findings_V3, "'no' = 0; 'yes' = 1"), 
                 CT_sev_low_V3 = car::recode(CT_sev_low_V3, "'no' = 0; 'yes' = 1"), 
                 CTsevabove5_V3 = car::recode(CTsevabove5_V3, "'no' = 0; 'yes' = 1"), 
                 sympt_present_V3 = car::recode(sympt_present_V3, "'no' = 0; 'yes' = 1"), 
                 lung_function_impaired_V3 = car::recode(lung_function_impaired_V3, "'no' = 0; 'yes' = 1")), 
        indep_variable = cov_univariate$mod_vars,
        mod_fun = glm, 
        family = 'binomial', 
        est_transf = exp) %>% 
    set_names(globals$mod_resp$response)

# Generating a common summary table with the results of univariate modeling, adjusting for multiple comparisons ----
  
  insert_msg('A summary table holding the results of univariate modeling')
  
  cov_univariate$summary_tbl <-  cov_univariate$mod_results %>% 
    map(get_model_summary) %>% 
    map(filter, 
        level != 'baseline', 
        !is.na(estimate), ## model convergence errors
        !is.na(lower_ci), 
        !is.na(upper_ci)) %>% 
    map(mutate, 
        regulation = ifelse(p_adj >= 0.05, 
                            'ns', 
                            ifelse(estimate > 1, 'positive', 'negative')), 
        level = ifelse(level == 'baseline', 'no', level))
  
  ## joining with the n numbers for each variable level, as requested by a Reviewer
  
  cov_univariate$summary_tbl <- map2(cov_univariate$summary_tbl, 
                                     cov_univariate$n_tables, 
                                     left_join, 
                                     by = c('variable', 'level'))

# Identifying the significant variables for each response ----

  insert_msg('Identifying significant variables correlating with at least one response')
  
  cov_univariate$signif_vars <- cov_univariate$summary_tbl %>% 
    map(filter, regulation != 'ns') %>% 
    map(~.x$variable)

# Visualizing the univariate modeling results as a classical forest plot ----
  # presented are the variables significantly associated with at least one CT response
  
  insert_msg('Forest plot with significant variables')
  
  cov_univariate$forest_plots <- list(summary_tbl = cov_univariate$summary_tbl, 
                                      plot_title = stri_replace(globals$mod_resp$label, fixed = '\n', replacement = ' ')) %>% 
    pmap(plot_forest, 
         top_n = 10, 
         signif_only = TRUE)

# END ----
  
  insert_tail()