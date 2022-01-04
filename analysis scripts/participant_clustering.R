# This script defines clusters of the participants based on the non-CT and non-functional features
# Clustering via SOM

  insert_head()
  
# Container list, globals and analysis table -----
  
  insert_msg('Globals setup')
  
  part_clust <- list()
  
  ## clustering variables and their labels
  
  part_clust$mod_vars <- globals$clust_vars[!globals$clust_vars %in% globals$mod_resp$response]
  
  part_clust$ft_labels <- ft_clust$ft_labels
  
  ## analysis table
  
  part_clust$analysis_tbl <- cov_data$mod_tbl[, c('ID', part_clust$mod_vars)] %>% 
    filter(complete.cases(.))
  
  part_clust$id_vec <- part_clust$analysis_tbl$ID
  
  part_clust$analysis_tbl <- part_clust$analysis_tbl %>% 
    select( - ID) %>% 
    map_dfc(function(x) ifelse(x == 'yes', 1, 0)) %>% 
    as.data.frame
  
  rownames(part_clust$analysis_tbl) <- part_clust$id_vec
  
# Testing several clustering algorithms -----
  
  insert_msg('Identifying the optimal clustering algorithm')
  
  ## kmeans clustering
  
  part_clust$algo_perf$algos[c('som_kmeans_smc', 
                               'som_kmeans_tanimoto', 
                               'som_kmeans_dice', 
                               'som_kmeans_cosine')] <- c('smc', 
                                                          'tanimoto', 
                                                          'dice', 
                                                          'cosine') %>% 
    map(~combi_cluster(data = part_clust$analysis_tbl, 
                       distance_som = .x, 
                       xdim = 4, 
                       ydim = 4, 
                       topo = 'hexagonal', 
                       node_clust_fun = kcluster, 
                       clust_fun = 'kmeans', 
                       distance_nodes = 'euclidean', 
                       k = 3, 
                       rlen = 2000))
  
  ## PAM clustering
  
  part_clust$algo_perf$algos[c('som_pam_smc', 
                               'som_pam_tanimoto', 
                               'som_pam_dice', 
                               'som_pam_cosine')] <- c('smc', 
                                                       'tanimoto', 
                                                       'dice', 
                                                       'cosine') %>% 
    map(~combi_cluster(data = part_clust$analysis_tbl, 
                       distance_som = .x, 
                       xdim = 4, 
                       ydim = 4, 
                       topo = 'hexagonal', 
                       node_clust_fun = kcluster, 
                       clust_fun = 'pam', 
                       distance_nodes = 'euclidean', 
                       k = 3, 
                       rlen = 2000))
 
  ## hierarchical clustering

  part_clust$algo_perf$algos[c('som_hcl_D2_smc', 
                               'som_hcl_D2_tanimoto', 
                               'som_hcl_D2_dice', 
                               'som_hcl_D2_cosine')] <- c('smc', 
                                                       'tanimoto', 
                                                       'dice', 
                                                       'cosine') %>% 
    map(~combi_cluster(data = part_clust$analysis_tbl, 
                       distance_som = .x, 
                       xdim = 4, 
                       ydim = 4, 
                       topo = 'hexagonal', 
                       node_clust_fun = hcluster, 
                       hc_method = 'ward.D2', 
                       distance_nodes = 'euclidean', 
                       k = 3, 
                       rlen = 2000))

  ## calculating the clustering variances for all algorithms
  
  part_clust$algo_perf$variances <- part_clust$algo_perf$algos %>% 
    map(var) %>% 
    map(~as_tibble(.x[c('total_wss', 'total_ss', 'between_ss', 'frac_var')])) %>% 
    map2_dfr(., names(.), ~mutate(.x, algorithm = .y))

  ## 20-fold cross-validation, cluster stability

  part_clust$algo_perf$cv <- part_clust$algo_perf$algos %>% 
    map(~cv.combi_analysis(.x, 
                           nfolds = 20, 
                           nearest_n = 10, 
                           simple_vote = TRUE, 
                           seed = 1234, 
                           .parallel = FALSE))
  
  part_clust$algo_perf$cv_summary <- part_clust$algo_perf$cv %>% 
    map2_dfr(., names(.), ~mutate(.x$summary, algorithm = .y))
  
  ## displaying the variance and cv errors in the plots
  
  part_clust$algo_perf[c('variance_plot', 'cv_plot')] <- list(data = part_clust$algo_perf[c('variances', 'cv_summary')], 
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
          scale_y_discrete(labels = c('som_hcl_D2_smc' = 'SOM HCl SMD', 
                                      'som_hcl_D2_tanimoto' = 'SOM HCl Jaccard', 
                                      'som_hcl_D2_dice' = 'SOM HCl Dice', 
                                      'som_hcl_D2_cosine' = 'SOM HCl Cosine', 
                                      'som_pam_smc' = 'SOM PAM SMD', 
                                      'som_pam_tanimoto' = 'SOM PAM Jaccard', 
                                      'som_pam_dice' = 'SOM PAM Dice', 
                                      'som_pam_cosine' = 'SOM PAM Cosine', 
                                      'som_kmeans_smc' = 'SOM K-means SMD', 
                                      'som_kmeans_tanimoto' = 'SOM K-means Jaccard', 
                                      'som_kmeans_dice' = 'SOM K-means Dice', 
                                      'som_kmeans_cosine' = 'SOM K-means Cosine')))
  
# Characteristic of the optimal clustering algorithm -----
  
  insert_msg('Characteristic of the best performing algorithm')
  
  part_clust$train_clust$clust_obj <- part_clust$algo_perf$algos$som_hcl_D2_smc
  
  ## renaming the clusters
  
  part_clust$train_clust$clust_obj$clust_assignment <- part_clust$train_clust$clust_obj %>% 
    extract('assignment') %>% 
    mutate(clust_id = car::recode(clust_id, "'1' = 'IR'; '2' = 'LR'; '3' = 'HR'"), 
           clust_id = factor(clust_id, c('LR', 'IR', 'HR')))
  
  ## cluster n numbers
  
  part_clust$train_clust$n_numbers <- ngroups(part_clust$train_clust$clust_obj)
  
  part_clust$train_clust$n_tag <- map2_chr(part_clust$train_clust$n_numbers$clust_id, 
                                           part_clust$train_clust$n_numbers$n, 
                                           paste, sep = ': n = ') %>% 
    paste(collapse = ', ') %>% 
    paste0('\n', .)
  
  ## diagnostic plots
  
  part_clust$train_clust$diagnostic <- plot(part_clust$train_clust$clust_obj, 
                                            type = 'diagnostic', 
                                            cust_theme = globals$common_theme)
  
  part_clust$train_clust$training <- plot(part_clust$train_clust$clust_obj, 
                                          type = 'training', 
                                          cust_theme = globals$common_theme)
  
  
  ## heat map and PCA plots
  
  part_clust$train_clust$heat_map <- plot(part_clust$train_clust$clust_obj, 
                                          type = 'heat_map', 
                                          cust_theme = globals$common_theme)
  
  part_clust$train_clust[c('pca_distance', 
                           'pca_data')] <- c('distance', 'data') %>% 
    map(~plot(part_clust$train_clust$clust_obj, 
              type = 'components', 
              red_fun = 'pca', 
              with = .x, 
              cust_theme = globals$common_theme))
  
  part_clust$train_clust$pca_distance$final <- part_clust$train_clust$pca_distance$final + 
    scale_fill_manual(values = globals$clust_colors)
  
  part_clust$train_clust$pca_data$final <- part_clust$train_clust$pca_data$final + 
    scale_fill_manual(values = globals$clust_colors)
  
  ## cv error vs cluster number

  part_clust$train_clust$stability <- 2:6 %>% 
    map(~combi_cluster(data = part_clust$analysis_tbl, 
                       distance_som = 'smc', 
                       xdim = 4, 
                       ydim = 4, 
                       topo = 'hexagonal', 
                       node_clust_fun = hcluster, 
                       hc_method = 'ward.D2', 
                       distance_nodes = 'euclidean', 
                       k = .x, 
                       rlen = 2000)) %>% 
    map(cv, 
        nfolds = 20, 
        nearest_n = 10, 
        simple_vote = TRUE, 
        seed = 1234) %>% 
    map2_dfr(., 2:6, ~mutate(.x$summary, k = .y))
  
  plan('sequential')
  
  ## CV error vs cluster number in a plot
  
  part_clust$train_clust$stability_scree <- part_clust$train_clust$stability %>% 
    ggplot(aes(x = k, 
               y = mean_error)) + 
    geom_line(color = 'coral3') + 
    scale_y_continuous(limits = c(0, 1)) + 
    globals$common_theme + 
    labs(title = 'Cluster stability vs cluster number', 
         subtitle = '20-fold cross-validation', 
         y = 'Mean classification error', 
         x = 'k, cluster number')

# Identification of the most influential non-CT factors by noising -----  
  
  insert_msg('Factor influence')
  
  part_clust$fct_impact <- impact(part_clust$train_clust$clust_obj, 
                                  seed = 1234, 
                                  .parallel = TRUE) %>% 
    map(mutate, 
        var_label = part_clust$ft_labels[variable])
  
  ## the most important clustering features
  
  part_clust$fct_impact$fct_top <- part_clust$fct_impact$summary %>% 
    filter(frac_diff > 0) %>% 
    .$variable
  
  ## plotting
  
  part_clust$fct_impact$plot <- quality_bar_plot(data = part_clust$fct_impact$summary %>% 
                                                   filter(frac_diff > 0), 
                                                 x_var = 'frac_diff', 
                                                 y_var = 'var_label', 
                                                 hjust = -0.2, 
                                                 plot_title = 'Importance of clustering variables', 
                                                 x_lab = expression(Delta*' between/total sum of squares')) + 
    expand_limits(x = 0.06)
  
# Displaying the most relevant clustering variables in a heat map plot -----
  
  insert_msg('Heat map with the most important factors')
  
  part_clust$ft_heat <- plot_clust_hm(sample_clust_object = part_clust$train_clust$clust_obj, 
                                      feature_clust_object = ft_clust$train_clust$clust_obj, 
                                      cust_theme = globals$common_theme, 
                                      plot_title = 'The most relevant clustering features', 
                                      x_lab = 'Participant', 
                                      discrete_fill = TRUE) + 
    scale_fill_manual(values = c('0' = 'steelblue', 
                                 '1' = 'coral3'), 
                      labels = c('0' = 'absent', 
                                 '1' = 'present'), 
                      name = 'Feature') + 
    scale_y_discrete(labels = part_clust$ft_labels)
 
  ## limiting to the most important features
  
  part_clust$ft_heat$data <- part_clust$ft_heat$data %>% 
    filter(feature %in% part_clust$fct_impact$fct_top)
  
# END ----
  
  insert_tail()
  