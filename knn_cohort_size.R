# This script checks the optimal training cohort size for machine learning predictions

# data and tools -----

  source('./tools/sys_tools.R')
  source('./tools/knn_tools.R')

  library(furrr)
  
  insert_head()
  
# data container and globals ----
  
  knn_size <- list()
  
  ## training cohort sizes
  
  knn_size$n_train <- seq(50, 100, by = 5) %>% 
    set_names(paste('n_train', seq(50, 100, by = 5), sep = '_'))

# analysis data set -----
  
  insert_msg('Analysis data')
  
  ## analysis data sets
  
  knn_size$analysis_tbl <- cov_multiclust$analysis_tbl %>% 
    as.data.frame
  
  rownames(knn_size$analysis_tbl) <- cov_multiclust$id_vec
  
  knn_size$analysis_tbls$CT_findings_V3 <- knn_size$analysis_tbl %>% 
    select(- CTsevabove5_V3, 
           - CT_pat_GGO_V3) %>% 
    mutate(CT_findings_V3 = ifelse(CT_findings_V3 == 2, 1, 0))
  
  knn_size$analysis_tbls$CTsevabove5_V3 <- knn_size$analysis_tbl %>% 
    select(- CT_findings_V3, 
           - CT_pat_GGO_V3) %>% 
    mutate(CTsevabove5_V3 = ifelse(CTsevabove5_V3 == 2, 1, 0))
  
  knn_size$analysis_tbls$CT_pat_GGO_V3 <- knn_size$analysis_tbl %>% 
    select(- CT_findings_V3, 
           - CTsevabove5_V3) %>% 
    mutate(CT_pat_GGO_V3 = ifelse(CT_pat_GGO_V3 == 2, 1, 0))
  
# generating the train/test splits. For each training set size there are 50 random replicates ----
  
  insert_msg('Random split generation')
  
  set.seed(123)
  
  knn_size$splits <- knn_size$analysis_tbls %>% 
    map(function(x) map(knn_size$n_train, 
                        make_splits,
                        inp_tbl = x, 
                        n_splits = 2)) ## 2 sets for testing, change to 50 for analysis
  
# analyzing the simulations - kNN and naive Bayes -----
  
  insert_msg('Serial simulation, kNN and Bayes')
  
  knn_size$simulations_knn <- knn_size$splits %>% 
    map2(., 
         names(.), 
         get_sim_stats, 
         pred_fun = 'knn', 
         k = 5, 
         method = 'jaccard', 
         kernel_fun = function(x) 1/x, 
         .parallel = F, 
         .out_parallel = T)
  
  knn_size$simulations_bayes <- knn_size$splits %>% 
    map2(., 
         names(.), 
         get_sim_stats, 
         pred_fun = 'naive_bayes', 
         .parallel = T, 
         .out_parallel = F)

# plotting the correct rate as a function of n training set size -----
  
  insert_msg('Plotting the results for each response: correct rate as a prime paramater')
  
  knn_size$plot_titles <-  globals$resp_labels[names(knn_size$simulations_bayes)] %>% 
    stri_replace(fixed = '\n', 
                 replacement = ' ')
  
  knn_size$plots_knn <- list(simulation_results = knn_size$simulations_knn, 
                             plot_title = knn_size$plot_titles) %>% 
    pmap(plot_stat, 
         stat_name = 'correct_rate', 
         cutoff_at = 80, 
         color = 'coral3', 
         plot_subtitle = '5-NN algorithm')
  
  knn_size$plots_bayes <- list(simulation_results = knn_size$simulations_bayes, 
                               plot_title = knn_size$plot_titles) %>% 
    pmap(plot_stat, 
         stat_name = 'correct_rate', 
         cutoff_at = 80, 
         color = 'cornflowerblue', 
         plot_subtitle = 'naive Bayes algorithm')
  
# END -----
  
  insert_tail()