# This script stitches the ready-to-use figures for the paper

  insert_head()

# data container and globals -----

  paper_figures <- list()
  suppl_figures <- list()
  
# Figure 1: analysis scheme -----
  
  insert_msg('Figure 1: analysis and study inclusion scheme')
  
  paper_figures$analysis_scheme <- plot_grid(ggdraw() + 
                                               draw_image('./analysis scheme/Analysis_scheme.png')) %>% 
    as_figure(label = 'figure_1_analysis_scheme', 
              w = 180, 
              h = 160)
  
# Figure 2: symptom recovery -----
  
  insert_msg('Figure 2: symptom recovery')
  
  paper_figures$sympt_recovery <- plot_grid(cov_recovery$long_covid$plot, 
                                            plot_grid(cov_recovery$symptom_analyses$plot + 
                                                        expand_limits(y = 100) + 
                                                        theme(plot.tag = element_blank(), 
                                                              legend.position = 'none'), 
                                                      plot_grid(get_legend(cov_recovery$symptom_analyses$plot), 
                                                                ggdraw() + 
                                                                  draw_text(cov_recovery$symptom_analyses$plot$labels$tag, 
                                                                            x = 0.2, 
                                                                            y = 0.6, 
                                                                            size = 8, 
                                                                            hjust = 0, 
                                                                            vjust = 0), 
                                                                nrow = 2, 
                                                                rel_heights = c(0.7, 0.3)), 
                                                      rel_widths = c(0.6, 0.4)), 
                                            nrow = 2, 
                                            rel_heights = c(0.51, 0.49), 
                                            labels = LETTERS, 
                                            label_size = 10) %>% 
    as_figure(label = 'figure_2_symptom_recovery', 
              w = 180, 
              h = 145)

# Figure 3: pulmonary recovery ------
  
  insert_msg('Figure 3: pulmonary recovery')
  
  paper_figures$pulmo_recovery <- plot_grid(cov_lung$kinetics$plot) %>% 
    as_figure(label = 'figure_3_pulmo_recovery', 
              w = 180, 
              h = 140)
  
# Figure 4: univariate modeling, CT responses -----
  
  insert_msg('Figure 4: univariate modeling, CT recovery')
  
  paper_figures$uni_mod_ct <- cov_univariate$forest_plots[c('CT_findings_V3', 
                                                            'CTsevabove5_V3')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              align = 'hv', 
              rel_heights = c(1.1, 0.9), 
              labels = LETTERS,
              label_size = 10) %>%
    as_figure(label = 'figure_4_univariate_ct', 
              w = 180, 
              h = 210)
  
# Figure 5: univariate modeling, lung function recovery ------
  
  insert_msg('Figure 5: univariate modeling, LUFO recovery')
  
  paper_figures$uni_mod_lufo <- plot_grid(cov_univariate$forest_plots$lung_function_impaired_V3 + 
                                            theme(legend.position = 'none')) %>% 
    as_figure(label = 'figure_5_univariate_lufo', 
              w = 180, 
              h = 110)
  
# Figure 6: feature clustering -----
  
  insert_msg('Figure 6: feature clustering')
  
  paper_figures$ft_clustering$top_panel <- plot_grid(ft_clust$test_clust$pca_plot_data + 
                                                       labs(title = 'Feature clustering') + 
                                                       theme(plot.tag = element_blank(), 
                                                             legend.position = 'none', 
                                                             plot.subtitle = element_blank()), 
                                                     plot_grid(get_legend(ft_clust$test_clust$pca_plot_data), 
                                                               ggdraw() + 
                                                                 draw_text(paste0(ft_clust$test_clust$n_tag, 
                                                                                  '\n\nObervations: n = ', 
                                                                                  nobs(ft_clust$test_clust$clust_obj)$variables), 
                                                                           x = 0.4, 
                                                                           y = 0.6, 
                                                                           size = 8, 
                                                                           hjust = 0, 
                                                                           vjust = 0), 
                                                               nrow = 2), 
                                                     ncol = 2, 
                                                     rel_widths = c(0.75, 0.25))
  
  paper_figures$ft_clustering$bottom_panel <- ft_clust$neighbors$plots[c('CT_sev_low_V3', 
                                                                         'CTsevabove5_V3', 
                                                                         'lung_function_impaired_V3', 
                                                                         'sympt_present_V3')] %>% 
    map(~.x + labs(color = 'SMD')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv')

  paper_figures$ft_clustering <- plot_grid(paper_figures$ft_clustering$top_panel, 
                                           paper_figures$ft_clustering$bottom_panel, 
                                           nrow = 2, 
                                           rel_heights = c(0.4, 0.6), 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    as_figure(label = 'figure_6_feature_clustering', 
              w = 180, 
              h = 220)
  
# Figure 7: participant clustering -----
  
  insert_msg('Figure 7: Participant clustering')
  
  paper_figures$som_clustering$top_panel <- plot_grid(ggdraw(), 
                                                      part_clust$train_clust$pca_data$final + 
                                                        labs(title = 'Participant clustering') + 
                                                        theme(plot.tag = element_blank(), 
                                                              legend.position = 'none'), 
                                                      plot_grid(get_legend(part_clust$train_clust$pca_data$final), 
                                                                ggdraw() + 
                                                                  draw_text(stri_replace_all(part_clust$train_clust$n_tag, 
                                                                                             fixed = ', ', 
                                                                                             replacement = '\n'), 
                                                                            x = 0.45, 
                                                                            y = 0.6, 
                                                                            size = 8, 
                                                                            hjust = 0, 
                                                                            vjust = 0), 
                                                                nrow = 2), 
                                                      ncol = 3, 
                                                      rel_widths = c(0.2, 0.6, 0.2))
  
  paper_figures$som_clustering <- plot_grid(paper_figures$som_clustering$top_panel, 
                                            part_clust$ft_heat + 
                                              theme(plot.tag = element_blank()), 
                                            nrow = 2, 
                                            rel_heights = c(0.3, 0.7), 
                                            labels = LETTERS, 
                                            label_size = 10) %>% 
    as_figure(label = 'figure_7_som_clustering', 
              w = 180, 
              h = 200)
  
# Figure 8: outcome in the participant clusters -----
  
  insert_msg('Figure 8: outcomes in the patient subsets')
  
  paper_figures$som_outcomes$top_panel <- clust_outcome$main_plots[c('CT_findings_rec', 
                                                           'lung_function_impaired_V3', 
                                                           'sympt_present_V3')] %>% 
    map(~.x + theme(legend.position = 'bottom')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv')
  
  paper_figures$som_outcomes$bottom_panel <- plot_grid(clust_outcome$persist_symptoms$plot + 
                                                         expand_limits(x = 60) + 
                                                         theme(legend.position = 'none', 
                                                               plot.tag = element_blank()), 
                                                       plot_grid(get_legend(clust_outcome$persist_symptoms$plot), 
                                                                 ggdraw() + 
                                                                   draw_text(stri_replace_all(clust_outcome$persist_symptoms$plot$labels$tag, 
                                                                                              fixed = ', ', 
                                                                                              replacement = '\n'), 
                                                                             x = 0.3, 
                                                                             y = 0.6, 
                                                                             size = 8, 
                                                                             hjust = 0, 
                                                                             vjust = 0), 
                                                                 nrow = 2), 
                                                       ncol = 2, 
                                                       rel_widths = c(0.8, 0.2))
  
  paper_figures$som_outcomes <- plot_grid(paper_figures$som_outcomes$top_panel, 
                                          paper_figures$som_outcomes$bottom_panel, 
                                          nrow = 2, 
                                          rel_heights = c(0.4, 0.6), 
                                          labels = LETTERS, 
                                          label_size = 10) %>% 
    as_figure(label = 'figure_8_som_outcome', 
              w = 180, 
              h = 220)
   
# Figure 9: machine learning, comparison of the algorithms -----
  
  insert_msg('Figure 9: machine learning')
  
  paper_figures$ml <- ml_plots$roc_stats$cv[c('CT_findings_V3', 
                                              'CTsevabove5_V3', 
                                              'lung_function_impaired_V3', 
                                              'sympt_present_V3')] %>% 
    map(~.x + 
          theme(legend.position = 'none') + 
          scale_y_continuous(limits = c(0, 1))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(ml_plots$roc_stats$cv[[1]]), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_9_machine_learning', 
              w = 180, 
              h = 140)
  
# Figure 10: machine learning, ensemble in the severity groups ----
  
  insert_msg('Figure 10: machine learning in the severity groups')
  
  paper_figures$ml_severity <- ml_detail$cv_plots[c('CT_findings_V3', 
                                                    'CTsevabove5_V3', 
                                                    'lung_function_impaired_V3', 
                                                    'sympt_present_V3')] %>% 
    map(~.x$ensemble + 
          theme(legend.position = 'none')) %>%  
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(ml_detail$cv_plots$CT_findings_V3[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_10_ml_severity', 
              w = 180, 
              h = 180)
  
# Supplementary Figure 3 - supplement 1: overlap between the outcome variables -------
  
  insert_msg('Figure 3 - supplement 1: overlap between the variables, any CT abnormalities')
  
  suppl_figures$outcome_overlap_anyCT <- overlap$venn$CT_findings %>% 
    plot_grid(plotlist = ., 
              align = 'hv', 
              ncol = 2,  
              label_size = 10) %>% 
    as_figure(label = 'figure3_supplement1', 
              w = 180, 
              h = 190)
  
# Supplementary Figure 3 - supplement 2: overlap between the outcome variables -------
  
  insert_msg('Figure 3 - supplement 2: overlap between the variables, moderate - severe abnormalities')
  
  suppl_figures$outcome_overlap_sev <- overlap$venn$CTsevabove5 %>% 
    plot_grid(plotlist = ., 
              align = 'hv', 
              ncol = 2,  
              label_size = 10) %>% 
    as_figure(label = 'figure3_supplement2', 
              w = 180, 
              h = 190)
  
# Supplementary Figure 3 - supplement 3: outcome kappa -----
  
  insert_msg('Figure 3 - supplement 3: outcome kappa')
  
  suppl_figures$outcome_kappa <- cov_lung$distribution$plots %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    c(list(legend = get_legend(cov_lung$distribution$plots[[1]] + 
                                 labs(fill = 'CT abnormality\ngrading')))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(overlap$kappa$plot, 
              ., 
              nrow = 2, 
              rel_heights = c(1, 2), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure3_supplement3', 
              w = 180, 
              h = 220)
  
# Supplementary Figure 6 - supplement 1: feature clustering QC -----
  
  insert_msg('Figure 6 - supplement 1: feature clustering QC')
  
  suppl_figures$ft_clust_qc <- plot_grid(ft_clust$algo_perf$variance_plot, 
                                         ft_clust$algo_perf$cv_plot, 
                                         ft_clust$train_clust$diagnostic$wss + 
                                           labs(subtitle = 'PAM clustering, SMD distance') +  
                                           theme(plot.tag = element_blank()), 
                                         ft_clust$train_clust$stability_scree + 
                                           labs(x = 'Number of clusters k'), 
                                         ncol = 2, 
                                         align = 'h', 
                                         labels = LETTERS, 
                                         label_size = 10) %>% 
    as_figure(label = 'figure6_supplement1', 
              w = 180, 
              h = 160)
  
# Supplementary Figure 6 - supplement 2: heat map of the clusters  -----
  
  insert_msg('Figure 6 - supplement 2: heat map of the feature clusters')
  
  suppl_figures$ft_heat_map <- plot_grid(ft_clust$test_clust$heat_map + 
                                           labs(tag = paste0(stri_replace_all(ft_clust$test_clust$n_tag, fixed = '\n', replacement = ', '), 
                                                             '\n\nObervations: n = ', 
                                                             nobs(ft_clust$test_clust$clust_obj)$variables)) + 
                                           theme(plot.subtitle = element_blank(), 
                                                 legend.position = 'bottom')) %>% 
    as_figure(label = 'figure6_supplement2', 
              w = 180, 
              h = 240)
  
# Supplementary Figure 7 - supplement 1: participant clustering QC -----
  
  insert_msg('Figure 7 - supplement 1: participant clustering QC')
  
  suppl_figures$part_clust_qc <- plot_grid(part_clust$algo_perf$variance_plot, 
                                           part_clust$algo_perf$cv_plot, 
                                           ncol = 2,
                                           align = 'hv', 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    plot_grid(plot_grid(part_clust$train_clust$training$observation + 
                          theme(plot.tag = element_blank()) + 
                          labs(subtitle = 'SMD'), 
                        part_clust$train_clust$diagnostic$node$wss + 
                          theme(plot.tag = element_blank()) + 
                          labs(title = 'Opimal number of SOM node clusters', 
                               subtitle = 'HCl, Eunclidean distance, Ward.D2 method'), 
                        part_clust$train_clust$diagnostic$node$dendrogram + 
                          theme(plot.tag = element_blank(), 
                                axis.title.x = globals$common_text) + 
                          labs(title = 'Opimal number of SOM node clusters', 
                               x = 'SOM node', 
                               y = 'Euclidean distance') + 
                          scale_y_continuous(limits = c(-1, 5.5), breaks = c(0:5)), 
                        part_clust$train_clust$stability_scree + 
                          labs(x = 'Number of clusters k'), 
                        ncol = 2, 
                        align = 'hv', 
                        labels = c('C', 'D', 'E', 'F'), 
                        label_size = 10), 
              nrow = 2, 
              rel_heights = c(1, 2)) %>% 
    as_figure(label = 'figure7_supplement1', 
              w = 180, 
              h = 210)
  
# Supplementary Figure 7 - supplement 2: the most important clustering factors -----
  
  insert_msg('Figure 7 - supplement 2: the most important clustering factors')
  
  suppl_figures$part_clust_fct <- plot_grid(part_clust$fct_impact$plot + 
                                              expand_limits(x = 0.085)) %>% 
    as_figure(label = 'figure7_supplement2', 
              w = 180, 
              h = 180)
  
# Supplementary Figure 8 - supplement 1: prediction of CT abnormalities by the cluster assignment -----
  
  insert_msg('Figure 8 - supplement 1: prediction of CT abnormalities by the clusters')
  
  suppl_figures$part_clust_risk <- plot_grid(plot_grid(clust_outcome$main_plots$pat_group, 
                                                       ggdraw(), 
                                                       ncol = 2, 
                                                       rel_widths = c(0.6, 0.4)), 
                                             clust_outcome$ct_risk$plot + 
                                               theme(legend.position = 'bottom'), 
                                             nrow = 2, 
                                             rel_heights = c(0.45, 0.55), 
                                             labels = LETTERS, 
                                             label_size = 10) %>% 
    as_figure(label = 'figure8_supplement1', 
              w = 180, 
              h = 190)
  
# Supplementary Figure 9 - supplement 1: classifier correlations ------
  
  insert_msg('Figure 9 - supplement 1: classifier correlations')
  
  suppl_figures$mod_cor <- ml_plots$correlation %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    as_figure(label = 'figure9_supplement1', 
              w = 180, 
              h = 180)
  
# Supplementary Figure 9 - supplement 2: ensemble construction ------
  
  insert_msg('Figure 9 - supplement 2: ensemble development')
  
  suppl_figures$ensemble <- ml_plots$ensemble %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    as_figure(label = 'figure9_supplement2', 
              w = 180, 
              h = 180)
  
# Supplementary Figure 9 - supplement 3: Testing of ML algorithms, training ------
  
  insert_msg('Figure 9 - supplement 3: ML classifiers in the training cohort')

  suppl_figures$ml_train_cohort <- ml_plots$roc_stats$train[c('CT_findings_V3', 
                                                              'CTsevabove5_V3', 
                                                              'lung_function_impaired_V3', 
                                                              'sympt_present_V3')] %>% 
    map(~.x + 
          theme(legend.position = 'none') + 
          scale_y_continuous(limits = c(0, 1))) %>% 
    plot_grid(plotlist = .,
              align = 'hv', 
              ncol = 2) %>% 
    plot_grid(get_legend(ml_plots$roc_stats$train[[1]]), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1)) %>%
    as_figure(label = 'figure9_supplement3', 
              w = 180, 
              h = 140)
  
# Supplementary Figure 9 - supplements 4 to 7: Variable importance ------
  
  insert_msg('Figure 9 - supplements 4 to 7: Variable importance')
  
  suppl_figures$importance_any_ct <- ml_plots[c('c50_importance_plots', 
                                                'rf_importance_plots', 
                                                'glmnet_importance_plots')] %>% 
    map(~.x$CT_findings_V3) %>% 
    plot_grid(plotlist = ., 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10)
  
  suppl_figures$importance_ct_severe <- ml_plots[c('c50_importance_plots', 
                                                   'rf_importance_plots', 
                                                   'glmnet_importance_plots')] %>% 
    map(~.x$CTsevabove5_V3) %>% 
    plot_grid(plotlist = ., 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10)
  
  suppl_figures$importance_lufo <- ml_plots[c('c50_importance_plots', 
                                              'rf_importance_plots', 
                                              'glmnet_importance_plots')] %>% 
    map(~.x$lung_function_impaired_V3) %>% 
    plot_grid(plotlist = ., 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10)
  
  suppl_figures$importance_symptoms <- ml_plots[c('c50_importance_plots', 
                                                  'rf_importance_plots', 
                                                  'glmnet_importance_plots')] %>% 
    map(~.x$sympt_present_V3) %>% 
    plot_grid(plotlist = ., 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10)
  
  suppl_figures[c('importance_any_ct', 
                  'importance_ct_severe', 
                  'importance_lufo', 
                  'importance_symptoms')] <- suppl_figures[c('importance_any_ct', 
                                                             'importance_ct_severe', 
                                                             'importance_lufo', 
                                                             'importance_symptoms')] %>% 
    map2(., c('figure9_supplement4', 
              'figure9_supplement5', 
              'figure9_supplement6', 
              'figure9_supplement7'), 
         as_figure, 
         w = 180, 
         h = 180)
  
# Saving the figures -----
  
  insert_msg('Saving the figures')
  
  paper_figures <- paper_figures %>%
    map(convert, to = 'in')
  
  suppl_figures <- suppl_figures %>% 
    map(convert, to = 'in')
  
  paper_figures %>% 
    walk(save_figure, 
         path = './paper/figures')
  
  suppl_figures %>% 
    walk(save_figure, 
         path = './paper/supplementary figures')

# END ----
  
  insert_tail()