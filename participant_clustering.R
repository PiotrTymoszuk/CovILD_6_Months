# This script performs the clustering of the participants by their non-CT binary features

# data and toolbox ----

  c('./tools/sys_tools.R', 
    './tools/clust_tools.R', 
    './tools/lm_qc_tools.R') %>% 
  walk(source)

  library(plotly)

  insert_head()

# data container and globals -----

  cov_partclust <- list()
  
  ## clustering variables: clinical features without CT responses
  
  cov_partclust$clust_vars <- globals$clust_vars[!globals$clust_vars %in% globals$mod_resp$response]

  ## cluster colors 
  
  cov_partclust$clust_colors <- c(k1 = 'cornflowerblue', 
                                  k2 = 'coral3', 
                                  k3 = 'cornsilk4')
  
  cov_partclust$clust_labs <- c(k1 = 'Low risk subset', 
                                k2 = 'High risk subset', 
                                k3 = 'Int risk subset')

  ## variable labels
  
  cov_partclust$var_glossary <- c(globals$mod_var_labels, 
                                  globals$resp_labels)
  
# script-specific functions -----
  
  cov_partclust$bar_plot <- function(count_tbl, feature_to_plot, mod_res_table = NULL) {
    
    ## plots the prevalence of the given feature in the participant subsets
    ## by option, it adds OR stats as well
    
    ## plotting table
    
    plotting_tbl <- count_tbl %>% 
      filter(feature == feature_to_plot, 
             feature_strata == 'yes') %>% 
      mutate(perc_lab = paste(signif(percent, 2), '%', sep = ''))
    
    ## n numbers in the cluster labels
    
    grid_labs <- map2(cov_partclust$clust_labs, 
                      cov_partclust$clust_n, 
                      function(x, y) paste(x, '\nn = ', y, sep = ''))
    
    ## plot title and stats in the subtitle
    
    plot_title <- cov_partclust$var_glossary[feature_to_plot] %>% 
      stri_replace(fixed = '\n', 
                   replacement = ' ')
    
    if(!is.null(mod_res_table)) {
      
      mod_stat_labs <- mod_res_table %>% 
        filter(response == feature_to_plot) %>% 
        mutate(plot_label = paste0(signif(estimate, 2), 
                                   ' [', 
                                   signif(lower_ci, 2), 
                                   ' - ', 
                                   signif(upper_ci, 2), 
                                   ']')) %>% 
        .$plot_label
      
      plot_subtitle <- paste('\nLow risk subset: baseline\nInt risk subset: OR = ', 
                             mod_stat_labs[1], 
                             '\nHigh risk subset: OR = ', 
                             mod_stat_labs[2], 
                             sep = '')
      
    } else {
      
      plot_subtitle <- NULL
      
    }
    
    ## the bar plot
    
    bar_plot <- plotting_tbl %>% 
      ggplot(aes(x = clust_id, 
                 y = percent, 
                 fill = clust_id)) + 
      geom_bar(stat = 'identity', 
               color = 'black') + 
      geom_text(aes(label = perc_lab), 
                size = 2.75, 
                hjust = 0.5, 
                vjust = 0, 
                nudge_y = 0.6) + 
      scale_fill_manual(values = cov_partclust$clust_colors, 
                        labels = cov_partclust$clust_labs, 
                        name = '') + 
      scale_x_discrete(labels = cov_partclust$clust_labs) + 
      globals$common_theme + 
      theme(axis.title.x = element_blank()) + 
      labs(title = plot_title, 
           tag = plot_subtitle, 
           y = '% within the subset')
    
    return(bar_plot)
    
  }
  
# analysis and distance calculation table ----
  
  insert_msg('Preparing the analysis data set with binary variables')
  
  cov_partclust$analysis_tbl <- cov_multiclust$analysis_tbl %>% 
    as.data.frame
  
  rownames(cov_partclust$analysis_tbl) <- cov_multiclust$id_vec

# clustering: kmeans, 3 centers, Jaccard distances ------
  
  insert_msg('K-means clustering')

  cov_partclust$clust_obj <- kcluster_data(inp_tbl = cov_partclust$analysis_tbl %>% 
                                             select( - all_of(globals$mod_resp$response)), 
                                           distance_method = 'jaccard', 
                                           clust_fun = kmeans, 
                                           k = 3, 
                                           nstart = 3, 
                                           seed = 1234)
  
  ## assigning the participants to the clusters
  
  cov_partclust$clust_obj$clust_assignment <- cov_partclust$clust_obj$clust_assignment %>% 
    mutate(clust_id = paste0('k', clust_id))
  
  ## clustering summary: appending the input table with cluster ids and CT responses
  
  cov_partclust$analysis_tbl <- cov_partclust$analysis_tbl %>% 
    rownames_to_column('ID') %>% 
    left_join(cov_partclust$clust_obj$clust_assignment %>% 
                set_names(c('ID', 'clust_id')), 
              by = 'ID') %>% 
    as.tibble
  
  ## n numbers
  
  cov_partclust$clust_n <- cov_partclust$analysis_tbl %>% 
    dlply(.(clust_id), nrow)
  
# MDS and 2d scatter plot with the cluster assignment ------
  
  insert_msg('MDS and cluster assignment plotting')
  
  cov_partclust$scatter_plot <- plot_clust_mds(cluster_analysis = cov_partclust$clust_obj, 
                                               k_dim = 3, 
                                               red_fun = 'mds', 
                                               cluster_labs = cov_partclust$clust_labs, 
                                               cluster_colors = cov_partclust$clust_colors, 
                                               cust_theme = globals$common_theme, 
                                               plot_title = 'Unsupervised k-means clstering', 
                                               point_alpha = 1)
  
# 3D Scatter plot with the k-means clustering assignment with plotly -----
  
  insert_msg('Scatter plot with the k means clustering summary')

  cov_partclust$scatter_plot3d <- cov_partclust$scatter_plot$plot$data %>% 
    mutate(clust_lab = car::recode(clust_id, 
                                   "'k1' = 'Low risk subset'; 
                                   'k3' = 'Int risk subset'; 
                                   'k2' = 'High risk subset'")) %>% 
    plot_ly(x = ~ dim_1, 
            y = ~ dim_2,
            z = ~ dim_3, 
            color = ~ clust_lab, 
            colors = cov_partclust$clust_colors %>% 
              set_names(c('Low risk subset', 
                          'High risk subset', 
                          'Int risk subset'))) %>% 
    add_markers() %>% 
    layout(scene = list(xaxis = list(title = 'Dimension 1', 
                                     tickfont = list(family = 'Helvetica', 
                                                     size = 16), 
                                     titlefont = list(family = 'Helvetica', 
                                                      size = 18)), 
                        yaxis = list(title = 'Dimension 2', 
                                     tickfont = list(family = 'Helvetica', 
                                                     size = 16), 
                                     titlefont = list(family = 'Helvetica', 
                                                      size = 18)), 
                        zaxis = list(title = 'Dimension 3', 
                                     tickfont = list(family = 'Helvetica', 
                                                     size = 16), 
                                     titlefont = list(family = 'Helvetica', 
                                                      size = 18))), 
           legend = list(font = list(family = 'Helvetica', 
                                     size = 18)), 
           title = list(text = 'Unsupervised k-means clustering of the participants', 
                        font = list(family = 'Helvetica', 
                                    size = 18)))
  
  cov_partclust$scatter_plot3d %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        width = 1800,
        height = 600
      )
    )

  
# Modeling prevalence of CT pathology and other clinical features in the clusters -----
  
  insert_msg('Modeling prevalence of CT changes in the clusters')

  ## logistic regression to compare the ORs, identifying the significant variables by LRT
  
  cov_partclust$ct_prevalence$analysis_tbl <- cov_partclust$analysis_tbl %>% 
    select( - ID, - clust_id) %>% 
    map_dfc(function(x) factor(ifelse(x == 2, 'yes', 'no'))) %>% 
    cbind(cov_partclust$analysis_tbl[c('clust_id', 'ID')] %>% 
            mutate(clust_id = factor(clust_id, c('k1', 'k3', 'k2')))) %>% 
    as_tibble
  
  cov_partclust$ct_prevalence$logis_models <- globals$clust_vars %>% 
    map(make_lm_model, 
        indep_variable = 'clust_id', 
        data = cov_partclust$ct_prevalence$analysis_tbl, 
        family = 'binomial', 
        est_transf = exp)
  
  ## logistic regression summary, adjusting the estimate p values
  
  cov_partclust$ct_prevalence$logis_summaries <- cov_partclust$ct_prevalence$logis_models %>% 
    map(~.$summary) %>% 
    map(filter, 
        level != 'baseline') %>% 
    map_dfr(function(x) if(any(is.na(x$upper_ci) | is.na(x$lower_ci))) NULL else x) %>% ## removing the models with convergence errors
    mutate(p_adj = p.adjust(p_value, 'BH'), 
           regulation = ifelse(p_adj >= 0.05, 'ns', 
                               ifelse(estimate > 1, 'positive', 'negative'))) 
  
  ## classifying the models with non-significant estimates for both k2 and k3 subsets as not significant
  
  cov_partclust$ct_prevalence$logis_summaries <-  cov_partclust$ct_prevalence$logis_summaries %>% 
    ddply(.(response), mutate, signif = ifelse(any(p_adj < 0.05), 'yes', 'no')) %>%  
    as_tibble
  
  ## updating the vector with the significant features
  
  cov_partclust$ct_prevalence$signif_features <- cov_partclust$ct_prevalence$logis_summaries %>% 
    filter(signif == 'yes') %>% 
    .$response %>% 
    unique
  
# Visualizing the modeling of prevalence as a classical forest plot ------
  
  insert_msg('Forest plot of the modeling results')
  
  cov_partclust$ct_prevalence$forest_plot <- cov_partclust$ct_prevalence$logis_summaries %>% 
    filter(signif == 'yes') %>% 
    mutate(level = factor(level, c('k3', 'k2'))) %>% 
    ggplot(aes(x = estimate, 
               y = reorder(response, estimate), 
               fill = regulation)) + 
    facet_grid(. ~ level, 
               labeller = as_labeller(cov_partclust$clust_labs)) + 
    geom_vline(xintercept = 1, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0.15) + 
    geom_point(size = 2, 
               shape = 21) + 
    scale_x_continuous(trans = 'log2') + 
    scale_y_discrete(labels = cov_partclust$var_glossary %>% 
                       stri_replace(fixed = '\n', 
                                    replacement = ' ') %>% 
                       set_names(names(cov_partclust$var_glossary))) + 
    scale_fill_manual(values = globals$corr_colors, 
                      name = 'Correlation\nwith prevalence') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          legend.position = 'bottom', 
          panel.background = element_rect(fill = 'gray95'), 
          panel.grid.major = element_line(color = 'white')) + 
    labs(title = 'Modeling of prevalence in the participant subsets', 
         subtitle = 'Logistic regression, baseline: low risk subset', 
         x = 'OR')
  
# determining feature prevalence in each cluster -----
  
  insert_msg('Feature prevalence in the clusters')
  
  cov_partclust$gen_feature_prev <- cov_partclust$ct_prevalence$analysis_tbl %>% 
    ddply(.(clust_id ), 
          count_feature_lst, 
          var_to_count_vec = globals$clust_vars, 
          positive_only = T) %>% 
    as_tibble
  
# visualization of prevalence of the clinical features in the clusters as a heat map ----
  
  insert_msg('Visualization of the feature prevalence as a heat map')

  cov_partclust$ct_prevalence$heat_map <- cov_partclust$ct_prevalence$analysis_tbl %>% 
    gather(key = 'feature', 
           value = 'present', 
           all_of(globals$clust_vars)) %>% 
    left_join(., 
              cov_partclust$gen_feature_prev %>% 
                ddply(.(feature), 
                      summarise, 
                      percent = sum(percent)), 
              by = 'feature') %>% 
    ggplot(aes(x = reorder(ID, present), 
               y = reorder(feature, percent), 
               fill = present)) + 
    geom_tile() + 
    facet_grid(. ~ clust_id, 
               scales = 'free', 
               space = 'free', 
               labeller = as_labeller(cov_partclust$clust_labs)) + 
    scale_fill_manual(values = c('no' = 'steelblue4', 
                                 'yes' = 'firebrick4'), 
                      labels = c('no' = 'absent', 
                                 'yes' = 'present'), 
                      name = '') + 
    scale_y_discrete(labels = cov_partclust$var_glossary %>% 
                       stri_replace(fixed = '\n', 
                                    replacement = ' ') %>% 
                       set_names(names(cov_partclust$var_glossary))) + 
    globals$common_theme + 
    theme(axis.title = element_blank(), 
          axis.text.x = element_blank(), 
          axis.line.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          legend.position = 'bottom')
  
  
# Prevalence visualization as a series of bar plots for the significant features -----
  
  insert_msg('Prevalence bar plots')

  cov_partclust$ct_prevalence$bar_plots <- globals$clust_vars %>% 
    map(cov_partclust$bar_plot, 
        count_tbl = cov_partclust$gen_feature_prev, 
        mod_res_table = cov_partclust$ct_prevalence$logis_summaries) %>% 
    set_names(globals$clust_vars)

# END -----
  
  insert_tail()