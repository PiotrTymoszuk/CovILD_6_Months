# This script performs ROC analysis of the outcome prediction 
# in the particular cohort parts (whole, G1 + G2 vs G3 + G4 severity groups)

  insert_head()

# Globals and analysis table ------

  insert_msg('Globals and analysis table')
  
  ml_detail <- list()
  
  ## severity group labels and colors
  
  ml_detail$sev_labels <- c('cohort' = 'cohort', 
                            'mild_moderate' = 'mild - moderate', 
                            'severe_critical' = 'severe - critical')
  
  ml_detail$sev_colors <- c('cohort' = 'black', 
                            'mild_moderate' = 'steelblue', 
                            'severe_critical' = 'coral3')
  
  ## severity group assignment
  
  ml_detail$sev_ass <- ml$analysis_tbl %>% 
    map(mutate, 
        ml_sev = ifelse(pat_group_G1_V0 == 'yes' | pat_group_G2_V0 == 'yes', 'mild_moderate', 'severe_critical')) %>% 
    map(rownames_to_column, 'ID') %>% 
    map(select, ID, ml_sev)
  
  ## extracting the train and cv predictions
  
  ml_detail$train_predictions <- ml$models %>% 
    map(extract_preds, dataset = 'train') %>% 
    map2(., ml_detail$sev_ass, 
         function(preds, sev) preds %>% 
           map(left_join, sev, by = 'ID'))
  
  ml_detail$cv_predictions <- ml$models %>% 
    map(extract_preds, dataset = 'cv') %>% 
    map2(., ml_detail$sev_ass, 
         function(preds, sev) preds %>% 
           map(left_join, sev, by = 'ID'))
  
# ROC analysis -----
  
  insert_msg('Serial ROC analysis')
  
  ml_detail$train_roc <- ml_detail$train_predictions %>% 
    map(~map(.x, get_roc_strata) %>% 
          map2_dfr(., names(.), ~mutate(.x, method = .y)))
  
  ml_detail$cv_roc <- ml_detail$cv_predictions %>% 
    map(~map(.x, get_roc_strata) %>% 
          map2_dfr(., names(.), ~mutate(.x, method = .y)))
  
# ROC curves -----
  
  insert_msg('ROC plotting')
  
  ml_detail$train_plots <- ml_detail$train_predictions %>% 
    map2(., names(.), 
         function(pred, response) list(preds = pred, 
                                       plot_title = ft_clust$ft_labels[response], 
                                       plot_subtitle = globals$model_labels[names(pred)]) %>% 
           pmap(plot_roc) %>% 
           map(~.x + 
                 scale_color_manual(values = ml_detail$sev_colors, 
                                    labels = ml_detail$sev_labels, 
                                    name = '')))
  
  ml_detail$cv_plots <- ml_detail$cv_predictions %>% 
    map2(., names(.), 
         function(pred, response) list(preds = pred, 
                                       plot_title = paste(ft_clust$ft_labels[response], 
                                                          globals$model_labels[names(pred)], 
                                                          sep = ': ')) %>% 
           pmap(plot_roc) %>% 
           map(~.x + 
                 scale_color_manual(values = ml_detail$sev_colors, 
                                    labels = ml_detail$sev_labels, 
                                    name = '')))
  
# END -----
  
  insert_tail()