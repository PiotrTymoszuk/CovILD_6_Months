# This script generates plots with the results of machine learning modeling

  insert_head()
  
# globals ----
  
  insert_msg('Globals setup')
  
  ml_plots <- list()

# Kappa/accuracy plots for the training and CV -----
  
  insert_msg('Kappa/accuracy plots for particular responses')
  
  ml_plots$kappa_accuracy <- c('train', 'cv') %>% 
    map(function(dataset) list(train_list = ml$models, 
                               plot_title = ft_clust$ft_labels[names(ml$models)]) %>% 
          pmap(plot_list_stats, 
               dataset = dataset, 
               show_value = FALSE)) %>% 
    map(~map(.x, ~.x + 
               scale_color_manual(values = c('Accuracy' = 'coral3', 
                                             'Kappa' = 'cornflowerblue'), 
                                  labels = list('Accuracy' = 'Accuracy', 
                                                'Kappa' = expression(kappa)), 
                                  name = '') + 
               scale_x_discrete(labels = globals$model_labels, 
                                limits = names((globals$model_labels))))) %>% 
    set_names(c('train', 'cv'))
  
# ROC stats for the training and CV ------
  
  insert_msg('ROC stats for the particular responses')
  
  ml_plots$roc_stats <- c('train', 'cv') %>% 
    map(function(dataset) list(train_list = ml$models, 
                               plot_title = ft_clust$ft_labels[names(ml$models)]) %>% 
          pmap(plot_list_roc_stats, 
               dataset = dataset, 
               show_value = FALSE)) %>% 
    map(~map(.x, ~.x + 
               scale_color_manual(values = c('ROC' = 'darkolivegreen4', 
                                             'Sens' = 'coral3', 
                                             'Spec' = 'cornflowerblue'), 
                                  labels = c('ROC' = 'AUC', 
                                             'Sens' = 'Sens', 
                                             'Spec' = 'Spec'), 
                                  name = '') + 
               scale_x_discrete(labels = globals$model_labels, 
                                limits = names((globals$model_labels))))) %>% 
    set_names(c('train', 'cv'))
  
# Correlation of the classifier results -----
  
  insert_msg('Correlation of the classifier accuracies')
  
  ml_plots$correlation <- list(train_list = ml$models, 
                               plot_title = ft_clust$ft_labels[names(ml$models)]) %>% 
    pmap(plot_mod_cor) %>% 
    map(~.x + 
          scale_fill_gradient2(low = 'steelblue', 
                               mid = 'white', 
                               high = 'indianred', 
                               midpoint = 0.5, 
                               name = expression(rho)) + 
          scale_x_discrete(labels = globals$model_labels) + 
          scale_y_discrete(labels = globals$model_labels))
  
# Weighting of the classifiers used for the ensemble construction -----
  
  insert_msg('Ensemble, classifier weighting')
  
  ml_plots$ensemble <- list(train_list = ml$models, 
                            plot_title = ft_clust$ft_labels[names(ml$models)]) %>% 
    pmap(plot_ensemble) %>% 
    map(~.x + 
          scale_color_gradient2(low = 'steelblue2', 
                                mid = 'black', 
                                high = 'indianred2', 
                                midpoint = 0) + 
          scale_y_discrete(labels = c(globals$model_labels, c('Intercept' = 'Intercept'))))
  
# Variable importance, C50, Random Forest and glmnet ----
  
  insert_msg('Vaiable importance')
  
  ## Random Forests
  
  ml_plots$rf_importance_plots <- list(data = ml$importance$rf %>% 
                                      map(top_n, 10, MeanDecreaseGini) %>% 
                                        map(mutate, variable = stri_replace(variable, fixed = 'yes', replacement = '')), 
                                    bar_fill = globals$resp_colors[names(ml$importance$rf)], 
                                    plot_title = paste('RF', ft_clust$ft_labels[names(ml$importance$rf)], sep = ': ')) %>% 
    pmap(quality_bar_plot, 
         x_var = 'MeanDecreaseGini', 
         y_var = 'variable',
         x_lab = expression(Delta*' Gini Index')) %>% 
    map(~.x + scale_y_discrete(labels = ft_clust$ft_labels))

  ## C50
  
  ml_plots$c50_importance_plots <- list(data = ml$importance[['C5.0']] %>%
                                          map(filter, Overall > 0) %>% 
                                          map(top_n, 10, Overall) %>% 
                                          map(mutate, variable = stri_replace(variable, fixed = 'yes', replacement = '')), 
                                        bar_fill = globals$resp_colors[names(ml$importance$C5.0)], 
                                        plot_title = paste('C5.0', ft_clust$ft_labels[names(ml$importance$C5.0)], sep = ': ')) %>% 
    pmap(quality_bar_plot, 
         x_var = 'Overall', 
         y_var = 'variable', 
         x_lab = '% attribute usage') %>% 
    map(~.x + scale_y_discrete(labels = ft_clust$ft_labels))
  
  ## glmNET
  
  ml_plots$glmnet_importance_plots <- list(data = ml$importance$glmnet %>% 
                                             map(filter, s1 != 0, variable != '(Intercept)') %>% 
                                             map(mutate, 
                                                 abs_beta = abs(s1), 
                                                 variable = stri_replace(variable, fixed = 'yes', replacement = '')) %>% 
                                             map(top_n, 10, abs_beta), 
                                           bar_fill = globals$resp_colors[names(ml$importance$glmnet)], 
                                           plot_title = paste('glmNet', ft_clust$ft_labels[names(ml$importance$glmnet)], sep = ': ')) %>% 
    pmap(quality_bar_plot, 
         x_var = 'abs_beta', 
         y_var = 'variable', 
         x_lab = expression('abs '*beta)) %>% 
    map(~.x + scale_y_discrete(labels = ft_clust$ft_labels))

# END ------
  
  insert_tail()