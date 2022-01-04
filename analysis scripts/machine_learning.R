# This script models the outcomes: any CT abnormality, moderate-to-severe CT abnormality, lung function impairment
# and persistent symptoms at the 180 day follow-up given the set of non-CT and non-functional parameters available
# till the 60-day visit.
# The tool of choice: caret and caretEnsemble. The models will be 20-fold CV validated

  insert_head()
  
# globals: variables, responses and analysis tables -----
  
  insert_msg('Globals setup')
  
  ml <- list()
  
  ## responses and independent variables
  
  ml$responses <- globals$mod_resp$response[globals$mod_resp$response != 'CT_sev_low_V3']
  
  ml$indep_vars <- globals$clust_vars[!globals$clust_vars %in% globals$mod_resp$response]
  
  ml$formulas <- ml$responses %>% 
    map(~paste(.x, paste(ml$indep_vars, collapse = '+'), sep = '~')) %>% 
    map(as.formula) %>% 
    set_names(ml$responses)
  
  ## analysis tables
  
  ml$analysis_tbl <- ml$responses %>% 
    map(~cov_data$mod_tbl[c('ID', .x, ml$indep_vars)]) %>% 
    map(~filter(.x, complete.cases(.x)))
  
  ml$id_vec <- ml$analysis_tbl %>% 
    map(~.x$ID)

  ml$analysis_tbl <- ml$analysis_tbl %>% 
    map(select, - ID) %>% 
    map(~map_dfc(.x, factor, levels = c('no', 'yes'))) %>% 
    map(as.data.frame) %>% 
    map2(., ml$id_vec, set_rownames) %>% 
    set_names(ml$responses)
  
  ## releveling the responses
  
  ml$analysis_tbl <- map2(ml$analysis_tbl, 
                          ml$responses, 
                          ~mutate(.x, !!ensym(.y) := factor(.data[[.y]], c('yes', 'no'))))
  
  ## train control object
  
  ml$train_control <- trainControl(method = 'repeatedcv', 
                                   number = 20, 
                                   repeats = 5, 
                                   classProbs = TRUE, 
                                   savePredictions = 'final', 
                                   returnResamp = 'final', 
                                   allowParallel = TRUE)

# Serial modeling ------
  
  insert_msg('Serial modeling')
  
  set.seed(1234)
  
  registerDoParallel(cores = 7)

  ml$models <- list(form = ml$formulas, 
                    data = ml$analysis_tbl) %>% 
    pmap(train_lst, 
         metric = 'Kappa', 
         trControl = ml$train_control, 
         methodList = c('C5.0', 'rf', 'svmRadial', 'nnet', 'glmnet'), 
         ensMethod = 'glmnet', 
         continue_on_fail = TRUE, 
         ci_method = 'percentile', 
         family = 'binomial')

  stopImplicitCluster()

# ROC statistics -----
  
  insert_msg('ROC stats')
  
  ml$roc <- ml$models %>% 
   map(get_roc)
  
# Importance measures for the Random Forest, C50 and glmnet classifiers -----
  
  insert_msg('RF and C5.0 importance measures')

  ml$importance$rf <- ml$models %>% 
    map(~.x$model_list$rf$finalModel$importance) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'variable') %>% 
    map(as_tibble)
  
  ml$importance$C5.0 <- ml$models %>% 
    map(~.x$model_list$C5.0$finalModel) %>% 
    map(C5imp) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'variable') %>% 
    map(as_tibble)
  
  ml$importance$glmnet <- ml$model %>% 
    map(~coef(.x$model_list$glmnet$finalModel, 
              s = .x$model_list$glmnet$finalModel$tuneValue$lambda)) %>% 
    map(as.matrix) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'variable') %>% 
    map(as_tibble)

# END -----
  
  insert_tail()