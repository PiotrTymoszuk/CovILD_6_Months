# The goal is to check whether the clustering approaches add to the bare
# COVID-19 severity grade in terms of predicting 6-month lung lesions.
# Classical multi-variate modeling approach is used

# data and toolbox ----

  c('./tools/sys_tools.R', 
    './tools/lm_qc_tools.R', 
    './tools/counting_tools.R') %>% 
  walk(source)

  insert_head()
  
# data container ------
  
  cov_verif <- list()

# analysis table ----
  
  insert_msg('Analysis table')
  
  cov_verif$analysis_tbl <- cov_data$long_data %>% 
    filter(time == 3) %>% 
    select(ID, 
           pat_group, 
           all_of(globals$mod_resp$variable)) %>% 
    left_join(cov_partclust$clust_obj$clust_assignment %>% 
                set_names(c('ID', 'clust_id')), 
              by = 'ID') %>% 
    mutate(clust_id = factor(clust_id, c('k1', 'k3', 'k2')))

# Risk clusters and severity groups: modeling ------
  
  insert_msg('Plotting and modeling the risk cluster distribution in the severity groups')

  cov_verif$risk_clusters$full_models <- globals$mod_resp$variable %>% 
    map(make_lm_model, 
        data = cov_verif$analysis_tbl,
        indep_variable = 'clust_id', 
        confounder = 'pat_group', 
        family = 'binomial')
  
  cov_verif$risk_clusters$uni_models <- globals$mod_resp$variable %>% 
    map(make_lm_model, 
        data = cov_verif$analysis_tbl, 
        indep_variable = 'clust_id', 
        family = 'binomial')
  
# Summaries ------
  
  insert_msg('Model summaries')

  cov_verif$risk_clusters[c('full_summaries', 
                            'uni_summaries')] <- cov_verif$risk_clusters[c('full_models', 
                                                                           'uni_models')] %>% 
    map(function(mod_type) mod_type %>% 
          map(~.x$summary)) %>% 
    map2(., 
         c('multi', 'uni'), 
         function(x, y) map(x, mutate, model_type = y))

  cov_verif$risk_clusters$summaries <-  map2(cov_verif$risk_clusters$full_summaries, 
                                             cov_verif$risk_clusters$uni_summaries, 
                                             rbind) %>% 
    map(as_tibble) %>% 
    map(mutate, 
        p_adj = p.adjust(p_value, 'BH')) %>% 
    set_names(globals$mod_resp$variable)
  
  cov_verif$risk_clusters[c('full_summaries', 
                            'uni_summaries')] <- NULL
  
# plotting: bar plot of the cluster distribution in severity groups ----
  
  insert_msg('Cluster distribution in the severity groups')
  
  cov_verif$risk_clusters$distr_plot <- cov_verif$analysis_tbl %>% 
    ddply(.(pat_group), 
          count_feature, 
          'clust_id') %>% 
    arrange(desc(clust_id)) %>%
    ddply(.(pat_group), 
          mutate, 
          lab_y = cumsum(percent) - 0.5*percent) %>% 
    mutate(perc_lab = paste(signif(percent, 2), '%', sep = '')) %>% 
    ggplot(aes(x = pat_group, 
               y = percent, 
               fill = clust_id)) + 
    geom_bar(stat = 'identity', 
             color = 'black') + 
    geom_label(aes(label = perc_lab, 
                   y = lab_y), 
               size = 2.75, 
               show.legend = F) + 
    scale_fill_manual(values = cov_partclust$clust_colors, 
                      labels = cov_partclust$clust_labs, 
                      name = '') + 
    scale_x_discrete(labels = globals$pat_group_labels) + 
    globals$common_theme + 
    theme(axis.title.x = element_blank()) + 
    labs(title = 'Risk subset distribution\nin acute COVID-19 severity groups', 
         y = '% acute severity group')
  
# Risk modeling presented in a forest plot ------
  
  insert_msg('Forest plot of the risk modeling results')
  
  cov_verif$risk_clusters$forest_plot <- cov_verif$risk_clusters$summaries %>% 
    map_dfr(filter, 
        level != 'baseline', 
        variable != 'pat_group') %>% 
    mutate(p_lab = paste('p =', signif(p_adj, 2))) %>% 
    ggplot(aes(x = estimate, 
               y = level, 
               fill = level)) + 
    facet_grid(model_type ~ response, 
               labeller = labeller(.rows = c(multi = 'Severity\nadjusted', 
                                             uni = 'Unadjusted'), 
                                   .cols = set_names(globals$mod_resp$label, 
                                                     globals$mod_resp$variable)), 
               scales = 'free') + 
    geom_vline(xintercept = 1, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0.10) + 
    geom_point(shape = 23, 
               size = 2) + 
    geom_text(aes(label = p_lab), 
              size = 2.75, 
              hjust = 0, 
              vjust = 0, 
              nudge_y = 0.15) + 
    scale_x_continuous(trans = 'log2') + 
    scale_y_discrete(label = cov_partclust$clust_labs) + 
    scale_fill_manual(values = cov_partclust$clust_colors) + 
    guides(fill = F) + 
    labs(title = 'Prediction of persistent lung lessions in the risk clusters', 
         subtitle = 'Unadjusted and acute COVID-19 severity-adjusted logistic modeling', 
         x = 'OR') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          panel.background = element_rect(fill = 'gray95'), 
          panel.grid.major = element_line(color = 'white'))
  
# END ------
  
  insert_tail()