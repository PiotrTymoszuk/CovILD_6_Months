# This script clusters binary non-CT modeling variables (training set). The outcome variables are added to the cluster structure
# by label propagation/semi-supervided clustering. Distance measure of choice: SMC. The optimal clustering algorithm
# as defined by variance and cluster stability (20-fold CV) id HCl Ward.D

  insert_head()

# data container and globals -----

  ft_clust <- list()

  ## cluster colors 
  
  ft_clust$clust_labs <- paste0('Cluster #', 1:4)

  ft_clust$clust_colors <- c('1' = 'cornsilk4', 
                             '2' = 'coral3', 
                             '3' = 'cornflowerblue', 
                             '4' = 'darkgoldenrod3') %>% 
    set_names(ft_clust$clust_labs)
  
  ## feature labels
  
  ft_clust$ft_labels <- stri_replace(c(globals$mod_var_labels, globals$resp_labels), 
                                     fixed = '\n', 
                                     replacement = ' ') %>% 
    #stri_replace(fixed = 'impairment', replacement = 'imp.') %>% 
    set_names(names(c(globals$mod_var_labels, globals$resp_labels)))
  
# analysis and distance calculation table ----
  
  insert_msg('Preparing the analysis data set with binary variables')
  
  ft_clust$analysis_tbl <-  cov_data$mod_tbl %>% 
    filter(complete.cases(.))
  
  ft_clust$id_vec <- ft_clust$analysis_tbl$ID
  
  ft_clust$analysis_tbl <- ft_clust$analysis_tbl %>% 
    select( - ID) %>% 
    map_dfc(function(x) ifelse(x == 'yes', 1, 0)) %>% 
    as.data.frame
  
  rownames(ft_clust$analysis_tbl) <- ft_clust$id_vec
  
  ft_clust$train_set <- ft_clust$analysis_tbl %>% 
    t %>% 
    as.data.frame
  
  ft_clust$test_set <- ft_clust$train_set[globals$mod_resp$response[-1], ]
  
  ft_clust$train_set <- ft_clust$train_set[!rownames(ft_clust$train_set) %in% globals$mod_resp$response, ]
  
# Identifying the optimal clustering algorithm, k = 3 based on the results of feature PCA -------
  
  insert_msg('Identifying the optimal clustering algorithm')

  ## K means family algorithms
  
  ft_clust$algo_perf$algos[c('kmeans_smc',
                             'kmeans_tanimoto', 
                             'kmeans_dice', 
                             'kmeans_cosine')] <- c('smc', 
                                                    'tanimoto', 
                                                    'dice', 
                                                    'cosine') %>% 
    map(~kcluster(data = ft_clust$train_set, 
                  distance_method = .x, 
                  k = 3, 
                  clust_fun = 'kmeans', 
                  nstart = 4, 
                  seed = 1234))
  
  ft_clust$algo_perf$algos[c('pam_smc',
                             'pam_tanimoto', 
                             'pam_dice', 
                             'pam_cosine')] <- c('smc', 
                                                 'tanimoto', 
                                                 'dice', 
                                                 'cosine') %>% 
    map(~kcluster(data = ft_clust$train_set, 
                  distance_method = .x, 
                  k = 3, 
                  clust_fun = 'pam', 
                  nstart = 4, 
                  seed = 1234))
  
  ## Hierarchical clustering

  ft_clust$algo_perf$algos[c('hcl_wardD2_smc', 
                             'hcl_wardD2_tanimoto', 
                             'hcl_wardD2_dice', 
                             'hcl_wardD2_cosine')] <- c('smc', 
                                                        'tanimoto', 
                                                        'dice', 
                                                        'cosine') %>% 
    map(hcluster, 
        data = ft_clust$train_set, 
        hc_method = 'ward.D2', 
        k = 3, 
        seed = 1234)
  
  ## calculating the clustering variances
  
  ft_clust$algo_perf$variances <- ft_clust$algo_perf$algos %>% 
    map(var) %>% 
    map(~as_tibble(.x[c('total_wss', 'total_ss', 'between_ss', 'frac_var')])) %>% 
    map2_dfr(., names(.), ~mutate(.x, algorithm = .y))
  
  ## 10-fold cross-validation, cluster stability
  
  ft_clust$algo_perf$cv <- ft_clust$algo_perf$algos %>% 
    map(cv, 
        nfolds = 20, 
        nearest_n = 5, 
        simple_vote = TRUE, 
        seed = 1234)
  
  ft_clust$algo_perf$cv_summary <- ft_clust$algo_perf$cv %>% 
    map2_dfr(., names(.), ~mutate(.x$summary, algorithm = .y))
  
  ## displaying the variance and cv errors in the plots
  
  ft_clust$algo_perf[c('variance_plot', 'cv_plot')] <- list(data = ft_clust$algo_perf[c('variances', 'cv_summary')], 
                                                            x_var = c('frac_var', 'mean_error'), 
                                                            plot_title = c('Clustering variance', 'Cluster stability'), 
                                                            plot_subtitle = c('Between/total sum of squares', 
                                                                              '20-fold cross-validation'), 
                                                            x_lab = c('Frac. total variance', 'Mean classification error'), 
                                                            bar_fill = c('steelblue', 'coral3')) %>% 
    pmap(quality_bar_plot, 
         y_var = 'algorithm', 
         txt_color = 'white') %>% 
    map(~.x + 
          scale_y_discrete(labels = c(kmeans_smc = 'K-means SMD', 
                                      kmeans_tanimoto = 'K-means Jaccard', 
                                      kmeans_dice = 'K-means Dice', 
                                      kmeans_cosine = 'K-means Cosine', 
                                      pam_smc = 'PAM SMD', 
                                      pam_tanimoto = 'PAM Jaccard', 
                                      pam_dice = 'PAM Dice', 
                                      pam_cosine = 'PAM Cosine',
                                      hcl_wardD2_smc = 'HCl Ward.D2 SMD', 
                                      hcl_wardD2_tanimoto = 'HCl Ward.D2 Jaccard', 
                                      hcl_wardD2_dice = 'HCl ward.D2 Dice', 
                                      hcl_wardD2_cosine = 'HCl ward.D2 Cosine')))
  
# Characteristic of the optimal clustering algorithm -----
  
  insert_msg('Characteristic of the best performing algorithm')
  
  ft_clust$train_clust$clust_obj <- ft_clust$algo_perf$algos$pam_smc
  
  ## renaming the clusters
  
  ft_clust$train_clust$clust_obj$clust_assignment <- ft_clust$train_clust$clust_obj %>% 
    extract('assignment') %>% 
    mutate(clust_id = paste0('Cluster #', clust_id))
  
  ## diagnostic plots
  
  ft_clust$train_clust$diagnostic <- plot(ft_clust$train_clust$clust_obj, 
                                          type = 'diagnostic', 
                                          cust_theme = globals$common_theme)
  
  ## heat map and PCA plots
  
  ft_clust$train_clust$heat_map <- plot(ft_clust$train_clust$clust_obj, 
                                        type = 'heat_map', 
                                        cust_theme = globals$common_theme) + 
    scale_x_discrete(labels = ft_clust$ft_labels) + 
    scale_y_discrete(labels = ft_clust$ft_labels)
  
  ft_clust$train_clust[c('pca_distance', 
                         'pca_data')] <- c('distance', 'data') %>% 
    map(~plot(ft_clust$train_clust$clust_obj, 
              type = 'components', 
              red_fun = 'pca', 
              with = .x, 
              cust_theme = globals$common_theme)) %>% 
    map(~.x + scale_fill_manual(values = ft_clust$clust_colors, name = ''))
  
  ## cv error vs cluster number
  
  ft_clust$train_clust$stability <- 2:10 %>% 
    map(kcluster, 
        data = ft_clust$train_set, 
        distance_method = 'smc', 
        clust_fun = 'pam', 
        seed = 1234) %>% 
    map(cv, 
        nfolds = 20, 
        nearest_n = 5, 
        simple_vote = TRUE, 
        seed = 1234) %>% 
    map2_dfr(., 2:10, ~mutate(.x$summary, k = .y))
  
  ## CV error vs cluster number in a plot
  
  ft_clust$train_clust$stability_scree <- ft_clust$train_clust$stability %>% 
    ggplot(aes(x = k, 
               y = mean_error)) + 
    geom_line(color = 'coral3') + 
    scale_y_continuous(limits = c(0, 1)) + 
    globals$common_theme + 
    labs(title = 'Cluster stability vs cluster number', 
         subtitle = '20-fold cross-validation', 
         y = 'Mean classification error', 
         x = 'k, cluster number')

# Test set, label propagation ------
  
  insert_msg('Test set, label propagation')
  
  ft_clust$test_clust$clust_predictions <- predict(ft_clust$train_clust$clust_obj, 
                                                   newdata = ft_clust$test_set, 
                                                   type = 'propagation', 
                                                   simple_vote = TRUE,
                                                   k = 5, 
                                                   detailed = TRUE)
  
  ## generating a common clustering object with the test assignments and test predictions
  
  ft_clust$test_clust$clust_obj <- list(data = quo(ft_clust$test_clust$clust_predictions$mix_data), 
                                        dist_mtx = ft_clust$test_clust$clust_predictions$mix_diss, 
                                        dist_method = 'smc',
                                        clust_fun = 'prediction', 
                                        clust_obj = NULL, 
                                        clust_assignment = rbind(extract(ft_clust$train_clust$clust_obj, 'assignment'), 
                                                                 extract(ft_clust$test_clust$clust_predictions$clust_analysis_object, 'assignment'))) %>% 
    clust_analysis
  
  ## cluster n numbers
  
  ft_clust$test_clust$n_numbers <- ngroups(ft_clust$test_clust$clust_obj)

  ft_clust$test_clust$n_tag <- map2_chr(ft_clust$test_clust$n_numbers$clust_id, 
                                        ft_clust$test_clust$n_numbers$n, 
                                        paste, sep = ': n = ') %>% 
    paste(collapse = '\n')
    
# Test set PCA plots ------
  
  insert_msg('Test set PCA plots')
  
  ## scores for the cT and functional responses
  
  ft_clust$test_clust[c('pca_data', 'pca_distance')] <- c('data', 'distance') %>% 
    map(~components(ft_clust$test_clust$clust_obj, 
                    red_fun = 'pca', 
                    with = .x)) %>% 
    map(extract, 'scores') %>% 
    map(filter, observation %in% globals$mod_resp$response)
  
  ## base plots
  
  ft_clust$test_clust[c('pca_plot_data', 'pca_plot_distance')] <- c('data', 'distance') %>% 
    map(~plot(ft_clust$test_clust$clust_obj, 
              type = 'components', 
              red_fun = 'pca', 
              with = .x, 
              cust_theme = globals$common_theme, 
              point_alpha = 0.62))
  
  ## adding the predictions
  
  ft_clust$test_clust[c('pca_plot_data', 'pca_plot_distance')] <- 
    map2(ft_clust$test_clust[c('pca_plot_data', 'pca_plot_distance')], 
         ft_clust$test_clust[c('pca_data', 'pca_distance')], 
         ~.x + 
           geom_point(data = .y, 
                      shape = 23, 
                      size = 3, 
                      show.legend = FALSE) + 
           geom_text_repel(data = .y, 
                           aes(label = globals$resp_labels[observation]),
                           size = 2.75, 
                           show.legend = FALSE, 
                           box.padding = 0.1, 
                           point.padding = 5, 
                           force_pull = 0.2, 
                           fontface = 'bold')) %>% 
    map(~.x + 
          scale_fill_manual(values = ft_clust$clust_colors, name = '') + 
          scale_color_manual(values = ft_clust$clust_colors, name = ''))
  
# Cluster heat maps ----
  
  insert_msg('Heat maps for the particular clusters')
  
  ## common heat map
  
  ft_clust$test_clust$heat_map <- plot(ft_clust$test_clust$clust_obj, 
                                                 type = 'heat_map', 
                                       cust_theme = globals$common_theme) + 
    scale_x_discrete(labels = ft_clust$ft_labels) + 
    scale_y_discrete(labels = ft_clust$ft_labels)
  
  ## particular clusters
  
  ft_clust$test_clust[c('heat_map_cl1', 
                        'heat_map_cl2', 
                        'heat_map_cl3')] <- ft_clust$test_clust[rep('heat_map', 3)]
  
  ft_clust$test_clust$heat_map_cl1$data <- ft_clust$test_clust$heat_map_cl1$data %>% 
    filter(clust_id == 'Cluster #1', clust_id2 == 'Cluster #1')
  
  ft_clust$test_clust$heat_map_cl2$data <- ft_clust$test_clust$heat_map_cl2$data %>% 
    filter(clust_id == 'Cluster #2', clust_id2 == 'Cluster #2')
  
  ft_clust$test_clust$heat_map_cl3$data <- ft_clust$test_clust$heat_map_cl3$data %>% 
    filter(clust_id == 'Cluster #3', clust_id2 == 'Cluster #3')

# Cluster radial plots ------
  
  insert_msg('Neighbor features of the study outcomes')
  
  ## distance tables
  
  ft_clust$neighbors$dist_tbl <- ft_clust$test_clust$clust_obj %>% 
    extract('assignment') %>% 
    dlply(.(clust_id)) %>% 
    map(~.x$observation)
  
  ft_clust$neighbors$dist_tbl <- ft_clust$neighbors$dist_tbl %>% 
    map(~dist(ft_clust$test_clust$clust_obj)[.x, .x])
  
  ## assignment of the outcome features
  
  ft_clust$neighbors$outcome_ass <- extract(ft_clust$test_clust$clust_predictions$clust_analysis_object, 
                                            'assignment') %>% 
    mutate(clust_id = as.character(clust_id))
  
  ## neighbor plots
  
  ft_clust$neighbors$plots <- list(center_var = ft_clust$neighbors$outcome_ass$observation, 
                                   dist_tbl = ft_clust$neighbors$dist_tbl[ft_clust$neighbors$outcome_ass$clust_id], 
                                   plot_title = ft_clust$ft_labels[ft_clust$neighbors$outcome_ass$observation], 
                                   plot_subtitle = ft_clust$neighbors$outcome_ass$clust_id) %>% 
    pmap(neighbour_plot, 
         n_neighbors = 5) %>% 
    set_names(ft_clust$neighbors$outcome_ass$observation)
  
# END ------