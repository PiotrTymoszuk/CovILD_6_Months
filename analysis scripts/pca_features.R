# This script performs PCA to identify the most influential non-CT variables in the data set

  insert_head()
  
# container list, analysis table and globals -----
  
  insert_msg('Globals and analysis table')
  
  ft_pca <- list()
  
  ## analysis table
  
  ft_pca$analysis_tbl <-  cov_data$mod_tbl %>% 
    filter(complete.cases(.)) %>% 
    select( - all_of(globals$mod_resp$response))
  
  ft_pca$id_vec <- ft_pca$analysis_tbl$ID
  
  ft_pca$analysis_tbl <- ft_pca$analysis_tbl %>% 
    select( - ID) %>% 
    map_dfc(function(x) ifelse(x == 'yes', 1, 0)) %>% 
    as.data.frame
  
  rownames(ft_pca$analysis_tbl) <- ft_pca$id_vec
  
  ft_pca$analysis_tbl <- t(ft_pca$analysis_tbl)
  
# PCA: object and plots ------
  
  insert_msg('PCA')
  
  ## object
  
  ft_pca$pca_obj <- reduce_data(data = ft_pca$analysis_tbl, 
                                distance_method = 'smc', 
                                kdim = 10, 
                                red_fun = 'pca')
  
  ## scree plot
  
  ft_pca$scree_plot <- plot(ft_pca$pca_obj, ## k = 4 seems the good choice
                            type = 'scree', 
                            cust_theme = globals$common_theme)
  
  ## score plot to assess the natural clustering tendency
  
  ft_pca$plots <- c('scores', 'loadings') %>% 
    map(~plot(red_analysis_object = ft_pca$pca_obj, 
              type = .x, 
              cust_theme = globals$common_theme)) %>% 
    set_names(c('scores', 'loadings'))
  
# general clustering tendency by Hopkins ------
  
  insert_msg('General clustering tendency')
  
  ft_pca$clust_tendency <- get_clust_tendency(data = calculate_dist(ft_pca$analysis_tbl, 'smc'), 
                                              n = 50)
  
# END -----
  
  insert_tail()