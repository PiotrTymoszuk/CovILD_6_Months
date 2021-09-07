# This script stitches the ready-to-use figures for the paper

# data and tools ------

  source('./tools/plotting_tools.R')

  insert_head()

# data container and globals -----

  cov_figures <- list()
  cov_supplements <- list()
  
  cov_figures$cmm_margin <- ggplot2::margin(t = 5, l = 3, r = 2, unit = 'mm')
  
# Figure 1 symptom recovery -----
  
  insert_msg('Figure 1: symptom recovery')
  
  cov_figures$figure_sympt_recovery <- plot_grid(cov_recovery$long_covid$plot, 
                                           plot_grid(cov_recovery$symptom_analyses$plot, 
                                                     ggdraw(), 
                                                     ncol = 2, 
                                                     rel_widths = c(1, 0.2)),  
                                           ncol = 1, 
                                           rel_widths = c(1, 0.8), 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_1_symptom_recovery', 
                     h = 140, 
                     w = 180)
  
# Figure 2 lung recovery -----
  
  insert_msg('Figure 2: lung recovery')
  
  cov_figures$figure_lung_recovery <- cov_lung$kinetics$plot %>% 
    as_figure_object(figure_label = 'figure_2_lung_recovery', 
                     h = 140, 
                     w = 180)
  
# Figure 3 risk factors ------
  
  insert_msg('Figure 3: risk factors of lung pathology')
  
  cov_figures$figure_risk_factors <- cov_univariate$summ_forest_plot %>% 
    as_figure_object(figure_label = 'figure_3_risk_modeling', 
                     h = 220, 
                     w = 180)
  
# Figure 4 clustering of the clinical features -----
  
  insert_msg('Figure 4: feature phenotypes')
  
  cov_figures$figure_feature_pheno <- plot_grid(plot_grid(cov_multiclust$scatter_plot$plot, 
                                                          ggdraw(), 
                                                          ncol = 2, 
                                                          rel_widths = c(1, 0.3)), 
                                                plot_grid(cov_multiclust$radial_plots$CT_findings_V3 + 
                                                            theme(legend.position = 'none'), 
                                                          cov_multiclust$radial_plots$CTsevabove5_V3 + 
                                                            theme(legend.position = 'none'), 
                                                          get_legend(cov_multiclust$radial_plots$CT_findings_V3), 
                                                          ncol = 3, 
                                                          rel_widths = c(1, 1, 0.2)), 
                                                ncol = 1, 
                                                rel_heights = c(0.8, 1), 
                                                labels = LETTERS, 
                                                label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_4_feature_phenotype', 
                     h = 180, 
                     w = 180)
  
# Figure 5 patient subsets -----
  
  insert_msg('Figure 5: patient subsets')
  
  cov_figures$figure_pat_subsets <- plot_grid(ggdraw() + 
                                                draw_image('./aux files/k_means.png') + 
                                                theme(plot.margin = cov_figures$cmm_margin), 
                                              cov_partclust$ct_prevalence$heat_map, 
                                              nrow = 2, 
                                              rel_heights = c(0.8, 2), 
                                              labels = LETTERS, 
                                              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_5_patient_subsets', 
                     h = 230, 
                     w = 180)
  
# Figure 6 patient subsets and pathology risk -----
  
  insert_msg('Figure 6: patient subsets and pathology risk')
  
  cov_figures$figure_subsets_risk <- plot_grid(cov_partclust$ct_prevalence$forest_plot + 
                                                 theme(plot.subtitle = element_blank()) + 
                                                 labs(title = 'Baseline: Low risk subset'), 
                                               plot_grid(cov_partclust$ct_prevalence$bar_plots$CT_findings_V3 + 
                                                           theme(legend.position = 'none', 
                                                                 axis.text.x = element_blank()) + 
                                                           expand_limits(y = 100), 
                                                         cov_partclust$ct_prevalence$bar_plots$CT_pat_GGO_V3 + 
                                                           theme(legend.position = 'none', 
                                                                 axis.text.x = element_blank()) + 
                                                           expand_limits(y = 95), 
                                                         cov_partclust$ct_prevalence$bar_plots$CTsevabove5_V3 + 
                                                           theme(legend.position = 'none', 
                                                                 axis.text.x = element_blank()) + 
                                                           expand_limits(y = 55), 
                                                         get_legend(cov_partclust$ct_prevalence$bar_plots$CT_findings_V3), 
                                                         ncol = 2), 
                                               nrow = 2, 
                                               labels = LETTERS, 
                                               label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_6_subset_risk', 
                     h = 230, 
                     w = 180)
  
  
# Figure 7: k-NN and Bayes prediction quality ------
  
  insert_msg('Figure 7: kNN prediction quality')
  
  cov_figures$figure_knn <-  cov_knn$qc_plot$plots[c('CT_findings_V3', 
                                                     'CT_pat_GGO_V3', 
                                                     'CTsevabove5_V3')] %>% 
    map(function(x) x + theme(legend.position = 'none')) %>% 
    c(., list(get_legend(cov_knn$qc_plot$plots$CT_findings_V3))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              labels = c('A', 'B', 'C', ''), 
              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_7_knn', 
                     w = 180, 
                     h = 160) 
  
# Supplementary Figure S1: analysis scheme -----
  
  insert_msg('Figure S1: analysis scheme')
  
  cov_supplements$figure_analysis_scheme <- ggdraw() + 
    draw_image('./analysis scheme/Analysis_scheme.png') + 
    theme(plot.margin = cov_figures$cmm_margin)
  
  cov_supplements$figure_analysis_scheme <- cov_supplements$figure_analysis_scheme %>% 
    as_figure_object(figure_label = 'figure_s1_analysis_scheme', 
                     w = 180, 
                     h = 190)
  
# Supplementary Figure S2: lufo and CT pathologies, common risk factors ------
  
  insert_msg('Figure S2: LUFO and common risk factors')
  
  cov_supplements$figure_lufo <- plot_grid(cov_lung$coocurrence$plot, 
                                           cov_univariate$venn_plot + 
                                             theme(plot.margin = cov_figures$cmm_margin), 
                                           nrow = 2, 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s2_lufo_risk', 
                     w = 180, 
                     h = 180)
  
# Supplementary Figure S3: clustering QC plots -----
  
  insert_msg('Figure S3: clsutering QC plots')
  
  cov_supplements$figure_clust_qc <- plot_grid(cov_multiclust$hmeans_opt_plot, 
                                               cov_partclust$hmeans_opt_plot, 
                                               ncol = 2, 
                                               labels = LETTERS, 
                                               label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s3_clustering_qc', 
                     w = 180, 
                     h = 80)
  
# Supplementary Figure S4: clusters of the participants and severity-adjusted lung lesion risk -----
  
  insert_msg('Figure S4: unadjusted and severity-adjusted lung lesion risk in the risk clusters')

  cov_supplements$figure_risk_clust <- plot_grid(plot_grid(cov_verif$risk_clusters$distr_plot, 
                                                           ggdraw(), 
                                                           ncol = 2, 
                                                           rel_widths = c(1, 0.3)), 
                                                 cov_verif$risk_clusters$forest_plot, 
                                                 nrow = 2, 
                                                 rel_heights = c(0.9, 1), 
                                                 labels = LETTERS, 
                                                 label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s4_clustering_adj_risk', 
                     w = 180, 
                     h = 180)
  
# Supplementary Figure S5: Training set size and machine learning predictions -----
  
  insert_msg('Figure S6: training set size and correctness of prediction')
  
  cov_supplements$figure_train_size <- plot_grid(knn_size$plots_knn$CT_findings_V3 + 
                                                   theme(plot.subtitle = element_blank()), 
                                                 knn_size$plots_knn$CT_pat_GGO_V3 + 
                                                   theme(plot.subtitle = element_blank()), 
                                                 knn_size$plots_knn$CTsevabove5_V3 + 
                                                   theme(plot.subtitle = element_blank()), 
                                                 ncol = 2) %>% 
    plot_grid(., 
              plot_grid(knn_size$plots_bayes$CT_findings_V3 + 
                          theme(plot.subtitle = element_blank()), 
                        knn_size$plots_bayes$CT_pat_GGO_V3 + 
                          theme(plot.subtitle = element_blank()), 
                        knn_size$plots_bayes$CTsevabove5_V3 + 
                          theme(plot.subtitle = element_blank()), 
                        ncol = 2), 
              nrow = 2, 
              labels = c('A', 'B'), 
              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s5_knn_train_size', 
                     w = 180, 
                     h = 210)
  
# Supplementary Figure S6: kNN and Bayes prediction in the severity groups -----
  
  insert_msg('Figure S6: kNN prediction in the severity groups')
  
  cov_supplements$figure_knn_severity <- cov_knn_verif$knn_qc_stats[c('CT_findings_V3', 
                                                                      'CT_pat_GGO_V3', 
                                                                      'CTsevabove5_V3')] %>% 
    map2(., names(.), 
         function(x, y) x$plot + 
           theme(legend.position = 'none') + 
           labs(title = globals$resp_labels[y] %>% 
                  stri_replace(fixed = '\n', replacement = ' '))) %>% 
    c(., 
      list(legend = get_legend(cov_knn_verif$knn_qc_stats$CT_findings_V3$plot))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2) %>% 
    as_figure_object(figure_label = 'figure_s6_knn_severity', 
                     w = 180, 
                     h = 180)

# Supplementary Figure S7: Bayes prediction in the severity groups -----
  
  insert_msg('Figure S7: naive Bayes prediction in the severity groups')

  cov_supplements$figure_bayes_severity <- cov_knn_verif$naive_bayes_qc_stats[c('CT_findings_V3', 
                                                                                'CT_pat_GGO_V3', 
                                                                                'CTsevabove5_V3')] %>% 
    map2(., names(.), 
         function(x, y) x$plot + 
           theme(legend.position = 'none') + 
           labs(title = globals$resp_labels[y] %>% 
                  stri_replace(fixed = '\n', replacement = ' '))) %>% 
    c(., 
      list(legend = get_legend(cov_knn_verif$naive_bayes_qc_stats$CT_findings_V3$plot))) %>%
    plot_grid(plotlist = ., 
              ncol = 2) %>% 
    as_figure_object(figure_label = 'figure_s7_bayes_severity', 
                     w = 180, 
                     h = 180)

# saving the main figures -----
  
  cov_figures[c('figure_sympt_recovery', 
                'figure_lung_recovery', 
                'figure_risk_factors', 
                'figure_feature_pheno', 
                'figure_pat_subsets', 
                'figure_subsets_risk', 
                'figure_knn')] %>% 
    walk(save_figure_object, 
         device = cairo_pdf, 
         target_folder = 'figures')
  
# saving the main figures as tiff ----
  
  cov_figures[c('figure_sympt_recovery', 
                'figure_lung_recovery', 
                'figure_risk_factors', 
                'figure_feature_pheno', 
                'figure_pat_subsets', 
                'figure_subsets_risk', 
                'figure_knn')] %>% 
    walk(save_figure_object,
         format = 'eps', 
         device = cairo_ps, 
         target_folder = 'figures')
  
  
# saving the supplementary figures -----
  
  cov_supplements[c('figure_analysis_scheme', 
                    'figure_lufo', 
                    'figure_clust_qc', 
                    'figure_risk_clust', 
                    'figure_train_size', 
                    'figure_knn_severity', 
                    'figure_bayes_severity')] %>% 
    walk(save_figure_object, 
         device = cairo_pdf, 
         target_folder = 'supplementary figures')
  
# END ----
  
  insert_tail()