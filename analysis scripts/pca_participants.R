# This script performs PCA to identify the most influential non-CT variables in the data set

  insert_head()
  
# container list, analysis table and globals -----
  
  insert_msg('Globals and analysis table')
  
  part_pca <- list()
  
  ## analysis table
  
  part_pca$analysis_tbl <-  cov_data$mod_tbl %>% 
    filter(complete.cases(.)) %>% 
    select( - all_of(globals$mod_resp$response))
  
  part_pca$id_vec <- part_pca$analysis_tbl$ID
  
  part_pca$analysis_tbl <- part_pca$analysis_tbl %>% 
    select( - ID) %>% 
    map_dfc(function(x) ifelse(x == 'yes', 1, 0)) %>% 
    as.data.frame
  
  rownames(part_pca$analysis_tbl) <- part_pca$id_vec

# PCA: object and plots ------
  
  insert_msg('PCA')
  
  ## object
  
  part_pca$pca_obj <- reduce_data(data = part_pca$analysis_tbl, 
                                      distance_method = 'smc', 
                                      kdim = 10, 
                                      red_fun = 'pca')
  
  ## scree
  
  part_pca$scree_plot <- plot(part_pca$pca_obj, ## k = 3 seems the good choice
                              type = 'scree', 
                              cust_theme = globals$common_theme)
  
  ## score plot to assess the natural clustering tendency
  
  part_pca$plots <- c('scores', 'loadings') %>% 
    map(~plot(red_analysis_object = part_pca$pca_obj, 
              type = .x, 
              cust_theme = globals$common_theme)) %>% 
    set_names(c('scores', 'loadings'))
  
# general clustering tendency by Hopkins ------
  
  insert_msg('General clustering tendency')
  
  part_pca$clust_tendency <- get_clust_tendency(data = calculate_dist(part_pca$analysis_tbl, 'smc'), 
                                                n = 50)
  
# END -----
  
  insert_tail()