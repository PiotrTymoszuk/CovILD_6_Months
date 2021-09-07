# This script tests if the k-NN and naive Bayes prediction procedures may be applied to predict
# CT pathologies and risk cluster assignment given the set of binary features used
# for the participant clustering

# data and toolbox ----

  source('./tools/sys_tools.R')
  source('./tools/modeling_tools.R')
  source('./tools/counting_tools.R')
  source('./tools/knn_tools.R')
  
  if(!'cov_partclust' %in% ls()) {
    
    source('participant_clustering.R')
    
  }

  library(furrr)

  insert_head()

# data container and globals -----

  cov_knn <- list()

# script-specific functions -----

  cov_knn$plot_qc <- function(bootstrap_results, stat_tbl, 
                              plot_title = NULL, plot_subtitle = NULL, plot_tag = NULL) {
    
    ## generates a QC plot with the bootstrap Se/Sp and accuracy bootstrap results 
    ## summary stats and p values
    
    plotting_tbl <- bootstrap_results %>% 
      select(- error_rate) %>% 
      gather(key = 'stat', 
             value = 'value', 
             Se, Sp, correct_rate)
    
    stat_tbl <- stat_tbl %>% 
      filter(stat != 'error_rate') %>% 
      mutate(value = expected, 
             p_lab = paste('p =', signif(p_value, 2)))
    
    qc_plot <- plotting_tbl %>% 
      ggplot(aes(x = value,
                 y = stat, 
                 fill = pred_method)) + 
      geom_vline(xintercept = 0.5, 
                 linetype = 'dashed') + 
      geom_point(shape = 21, 
                 size = 2, 
                 alpha = 0.25, 
                 position = position_jitterdodge(jitter.width = 0.1, 
                                                 jitter.height = 0.05, 
                                                 dodge.width = 0.9)) + 
      geom_errorbarh(data = stat_tbl, 
                     aes(xmin = lower_ci, 
                         xmax = upper_ci), 
                     height = 0.1, 
                     position = position_dodge(0.9)) + 
      geom_point(data = stat_tbl, 
                 size = 3, 
                 shape = 23, 
                 position = position_dodge(0.9)) + 
      geom_text(data = stat_tbl, 
                aes(label = p_lab), 
                size = 2.75, 
                vjust = -1.5, 
                position = position_dodge(0.9)) + 
      scale_y_discrete(limits = c('correct_rate', 'Sp', 'Se'), 
                       labels = c('Correct\nrate', 'Sp', 'Se')) +
      scale_fill_manual(values = c('knn' = 'coral3', 
                                   'naive_bayes' = 'cornflowerblue'), 
                        labels = c('knn' = 'Weighted 5-kNN', 
                                   'naive_bayes' = 'Naive Bayes'), 
                        name = 'Algorithm') + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(x = 'Statistic value', 
           title = plot_title,
           subtitle = plot_subtitle, 
           plot_tag = plot_tag)
    
    return(qc_plot)
    
  }
  
# analysis data set and the random train/test splits -----
  
  insert_msg('Analysis data sets and the random splits')
  
  
  ## 200 bootstrap splits, 80 records train, 28 records test
  
  set.seed(1234)

  cov_knn$bootstraps[c('CT_findings_V3', 
                       'CTsevabove5_V3', 
                       'CT_pat_GGO_V3')] <- knn_size$analysis_tbls %>% 
    map(make_splits, 
        nrow_train = 80, 
        n_splits = 20) ## set to 20 for testing, set to 200 for analysis

# serial determination of the sensitivity and specificity of the k-NN approach with the bootstrap splits -----
  
  insert_msg('Serial investigation of the kNN approach quality')

  cov_knn$knn_qc[c('CT_findings_V3', 
                   'CTsevabove5_V3', 
                   'CT_pat_GGO_V3')] <- cov_knn$bootstraps %>% 
    map2(., names(.), 
         test_accuracy, 
         pred_fun = 'knn', 
         k = 5, 
         method = 'jaccard', 
         kernel_fun = function(x) 1/x, 
         generate_random = T, 
         ci_method = 'bca', 
         .parallel = T)

# serial determination of the sensitivity and specificity of the Bayes approach with the bootstrap splits ----- 
  
  insert_msg('Serial investigation of the kNN approach quality')
  
  cov_knn$bayes_qc[c('CT_findings_V3', 
                     'CTsevabove5_V3', 
                     'CT_pat_GGO_V3')] <- cov_knn$bootstraps %>% 
    map2(., names(.), 
         test_accuracy, 
         pred_fun = 'naive_bayes', 
         generate_random = T, 
         ci_method = 'bca', 
         .parallel = T)

# visualizing the results: point plots with the prediction features (Se, Sp, accuracy) for kNN and Bayes -----
  
  insert_msg('Result plotting')
  
  ## common result tables for plotting
  
  cov_knn$qc_plot$bootstrap_results <- map2(cov_knn$knn_qc, 
                                            cov_knn$bayes_qc, 
                                            function(x, y) rbind(x$boot_results, 
                                                                 y$boot_results)) %>% 
    map(mutate, 
        pred_method = factor(pred_method, c('naive_bayes', 'knn')))
  
  ## common stat tables with p values for plotting and plot titles
  
  cov_knn$qc_plot$stat_tbl <- map2(cov_knn$knn_qc, 
                                       cov_knn$bayes_qc, 
                                       function(x, y) rbind(left_join(x$summary, 
                                                                      x$significance, 
                                                                      by = c('stat', 'pred_method')), 
                                                            left_join(y$summary, 
                                                                      y$significance, 
                                                                      by = c('stat', 'pred_method')))) %>% 
    map(mutate, 
        pred_method = factor(pred_method, c('naive_bayes', 'knn')))
  
  cov_knn$qc_plot$plot_title <- globals$resp_labels[names(cov_knn$qc_plot$bootstrap_results)] %>% 
    stri_replace(fixed = '\n', 
                 replacement = ' ')
  
  ## plots

  cov_knn$qc_plot$plots <- cov_knn$qc_plot %>% 
    pmap(cov_knn$plot_qc)

# END -----
  
  insert_tail()