# This script analyzes the outcome variables in the participant clusters by Chi2 test

  insert_head()
  
# globals and analysis table -----
  
  insert_msg('Globals setup')
  
  clust_outcome <- list()
  
  ## variable labels
  
  part_clust$sympt_vars <- paste(globals$symptoms, 'V3', sep = '_')
  
  clust_outcome$ft_labels <- c(part_clust$ft_labels, 
                               set_names(globals$symptom_labs, 
                                         part_clust$sympt_vars))
  
  ## radiological and functional outcomes
  
  clust_outcome$analysis_tbl <- extract(part_clust$train_clust$clust_obj, 'assignment') %>% 
    set_names(c('ID', 'node', 'clust_id')) %>% 
    left_join(cov_data$mod_tbl[c('ID', globals$mod_resp$response)], by = 'ID')
  
  ## particular long term symptoms
  
  clust_outcome$analysis_tbl <- clust_outcome$analysis_tbl %>% 
    left_join(cov_data$long_data %>% 
                filter(time_months == 6) %>% 
                select(ID, pat_group, all_of(globals$symptoms)) %>% 
                set_names(c('ID', 'pat_group', part_clust$sympt_vars)), 
              by = 'ID')
  
  ## recoding
  
  clust_outcome$analysis_tbl <- clust_outcome$analysis_tbl %>% 
    mutate(CT_findings_rec = ifelse(CT_findings_V3 == 'no', 'absent', 
                                   ifelse(CT_sev_low_V3 == 'yes', '1-5 pt', '>5 pt')), 
           CT_findings_rec = factor(CT_findings_rec, c('absent', '1-5 pt', '>5 pt')))
  
  clust_outcome$id_vec <- clust_outcome$analysis_tbl$ID
  
  clust_outcome$analysis_tbl <- clust_outcome$analysis_tbl %>% 
    select( - ID) %>% 
    map_dfc(factor) %>% 
    mutate(fever_V3 = factor(fever_V3, c(0, 1)))
  
  ## a vector with the outcome variables to be analyzed
  
  clust_outcome$variables <- names(clust_outcome$analysis_tbl)[!names(clust_outcome$analysis_tbl) %in% c('clust_id', 
                                                                                                         'CT_findings_V3', 
                                                                                                         'CT_sev_low_V3', 
                                                                                                         'CTsevabove5_V3', 
                                                                                                         'node')]
  
  ## the final table
  
  clust_outcome$analysis_tbl <- clust_outcome$analysis_tbl %>% 
    mutate(ID = clust_outcome$id_vec)

# Serial analysis -----
  
  insert_msg('Serial analysis')
  
  ## analyses
  
  clust_outcome$analyses <- clust_outcome$variables %>% 
    map(analyze_feature, 
        inp_tbl = clust_outcome$analysis_tbl, 
        split_var = 'clust_id') %>% 
    set_names(clust_outcome$variables)
  
  ## testing summaries
  
  clust_outcome$test_summary <- clust_outcome$analyses %>% 
    map_dfr(extract_test_summary) %>% 
    mutate(p_adj = p.adjust(p_value, 'BH'), 
           p_lab = ifelse(p_adj < 0.05, 
                          paste('p =', signif(p_adj, 2)), 
                          paste0('ns (p = ', signif(p_adj, 2), ')')))
  
  ## p value vectors
  
  clust_outcome$p_val_vecs <- set_names(clust_outcome$test_summary$p_lab, 
                                        clust_outcome$test_summary$variable)
  
# Plotting the main study outcomes: CT findings, functions lung impairment and presence of the symptoms and the severity groups -----
  
  insert_msg('Main outcome plots')
  
  clust_outcome$main_plots <- list(analysis_object = clust_outcome$analyses[c('sympt_present_V3', 
                                                                              'lung_function_impaired_V3', 
                                                                              'CT_findings_rec', 
                                                                              'pat_group')], 
                                   label = c('Symptoms\nat 180-day visit',
                                             'Lung function imp.\nat 180-day visit', 
                                             'CT abnormalities\nat 180-day visit', 
                                             'Acute COVID-19 severity'), 
                                   fill_colors = list(c('steelblue', 'coral3'), 
                                                      c('steelblue', 'coral3'), 
                                                      globals$ct_find_colors, 
                                                      globals$pat_group_colors)) %>% 
    pmap(plot_analysis, 
         pie = FALSE, 
         cust_theme = globals$common_theme, 
         x_lab = 'Participant cluster',
         y_lab = '% cluster') %>% 
    map2(., clust_outcome$p_val_vecs[names(.)], ~.x + labs(subtitle = .y))
  
  clust_outcome$main_plots$pat_group <- clust_outcome$main_plots$pat_group + 
    scale_fill_manual(values = globals$pat_group_colors, 
                      labels = globals$pat_group_labels)
  
# Plotting the frequency of the survey symptoms at the 180-day follow-up -------
  
  insert_msg('Persistent symptoms')
  
  ## plotting table
  
  clust_outcome$persist_symptoms$plotting_tbl <- clust_outcome$analyses[part_clust$sympt_vars] %>% 
    map_dfr(extract_counts) %>% 
    filter(strata == 1)
  
  ## a table with p values
  
  clust_outcome$persist_symptoms$signif_tbl <- right_join(clust_outcome$test_summary[c('variable', 'p_lab')], 
                                                         clust_outcome$persist_symptoms$plotting_tbl[c('variable', 'percent', 'split_var')] %>% 
                                                           group_by(variable) %>% 
                                                           summarise(percent = max(percent)), 
                                                         by = 'variable') %>% 
    mutate(split_var = 'IR')
  
  ## summary plot
  
  clust_outcome$persist_symptoms$plot <- clust_outcome$persist_symptoms$plotting_tbl %>% 
    ggplot(aes(x = percent, 
               y = reorder(variable, percent), 
               fill = split_var)) + 
    geom_bar(stat = 'identity', 
             position = position_dodge(0.9), 
             color = 'black') + 
    geom_text(data = clust_outcome$persist_symptoms$signif_tbl, 
              aes(label = p_lab), 
              hjust = -0.3, 
              vjust = 0.5, 
              size = 2.75) + 
    scale_fill_manual(values = globals$clust_colors, 
                      name = 'Participant cluster') + 
    scale_y_discrete(labels = clust_outcome$ft_labels) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Symtoms present at 180-day visit', 
         tag = clust_outcome$main_plots$sympt_present_V3$labels$tag, 
         x = '% cluster')
  
# Modeling the risk of any CT abnormalities and moderate - severe abnormalities by the cluster assignment ----
  # checking the adjustment for severe COVID-19
  
  insert_msg('Cluster assignment and the risk of any and moderate-to-severe lung lesions')
  
  ## model construction
  
  clust_outcome$ct_risk$models <- list(raw_any = CT_findings_V3 ~ clust_id, 
                                       raw_severe = CTsevabove5_V3 ~ clust_id, 
                                       raw_lufo = lung_function_impaired_V3 ~ clust_id, 
                                       adj_any = CT_findings_V3 ~ clust_id + pat_group, 
                                       adj_severe = CTsevabove5_V3 ~ clust_id + pat_group, 
                                       adj_lufo = lung_function_impaired_V3 ~ clust_id + pat_group) %>% 
    map(~glm(formula = .x, 
             family = 'binomial', 
             data = clust_outcome$analysis_tbl))
  
  ## LRT
  
  clust_outcome$ct_risk$lrt <- map2(clust_outcome$ct_risk$models[c('raw_any', 'raw_severe', 'raw_lufo')], 
                                    clust_outcome$ct_risk$models[c('adj_any', 'adj_severe', 'adj_lufo')], 
                                    anova, test = 'Chisq')
  
  clust_outcome$ct_risk$lrt_summary <- clust_outcome$ct_risk$lrt %>% 
    map(unclass) %>% 
    map(~.x[c('Df', 'Deviance', 'Pr(>Chi)')]) %>% 
    map(as_tibble) %>% 
    map2_dfr(., c('CT_findings_V3', 'CTsevabove5_V3', 'lung_function_impaired_V3'), ~mutate(.x[2, ], response = .y))
  
  ## inference summary
  
  clust_outcome$ct_risk$test_summary <- clust_outcome$ct_risk$models %>% 
    map(lmqc::get_estimates, transf_fun = exp) %>% 
    map2_dfr(., names(.), ~mutate(.x, model_type = .y)) %>% 
    filter(parameter != '(Intercept)') %>% 
    mutate(p_adj = p.adjust(p_value, 'BH'), 
           est_lab = paste0(signif(estimate, 3), ' [', signif(lower_ci, 3), ' - ', signif(upper_ci, 3), ']'), 
           response = ifelse(stri_detect(model_type, fixed = 'any'), 
                             'CT_findings_V3', 
                             ifelse(stri_detect(model_type, fixed = 'lufo'), 'lung_function_impaired_V3', 'CTsevabove5_V3')), 
           model_type = ifelse(stri_detect(model_type, fixed = 'raw'), 'unadjusted', 'severity-adjusted'), 
           var_lab = stri_replace(parameter, fixed = 'clust_id', replacement = ''), 
           var_lab = paste(var_lab, 'ref: LR', sep = '\n'))
  
  ## n numbers for the outcomes
  
  clust_outcome$ct_risk$outcome_n <- map2(c(CT_findings_V3 = 'CT_findings_V3', 
                                            CTsevabove5_V3 = 'CTsevabove5_V3', 
                                            lung_function_impaired_V3 = 'lung_function_impaired_V3'), 
                                          map(clust_outcome$ct_risk$models[1:3], model.frame), 
                                          ~count(.y, .data[[.x]])) %>% 
    map2(., names(.), ~mutate(.x, response = .y)) %>% 
    map_dfr(~.x[2, c('response', 'n')])
  
  clust_outcome$ct_risk$outcome_tag <- paste0('CT abnormalities: n = ', clust_outcome$ct_risk$outcome_n$n[1], 
                                              ', CT Severity Score > 5: n = ', clust_outcome$ct_risk$outcome_n$n[2], 
                                              ', Lung function impaired: n = ', clust_outcome$ct_risk$outcome_n$n[2])
  
  clust_outcome$ct_risk$pat_group_n <- count(model.frame(clust_outcome$ct_risk$models$adj_severe), pat_group)
  
  clust_outcome$ct_risk$pat_group_tag <- paste0('Outpatient: n = ', clust_outcome$ct_risk$pat_group_n$n[1], 
                                                ', Hospitalized: n = ', clust_outcome$ct_risk$pat_group_n$n[2], 
                                                ', Hospitalized oxygen therapy: n = ', clust_outcome$ct_risk$pat_group_n$n[3], 
                                                ', Hospitalized ICU: n = ', clust_outcome$ct_risk$pat_group_n$n[4])
  
  
  clust_outcome$ct_risk$common_tag <- paste0('complete: CT: n = ',  clust_outcome$ct_risk$test_summary$n_complete[1], 
                                             ', lung function: n = ', clust_outcome$ct_risk$test_summary$n_complete[5], 
                                            '\n', clust_outcome$ct_risk$outcome_tag, 
                                            clust_outcome$main_plots[[1]]$labels$tag, 
                                            '\n', clust_outcome$ct_risk$pat_group_tag)
  
  ## Forest plot
  
  clust_outcome$ct_risk$plot <- clust_outcome$ct_risk$test_summary %>% 
    filter(stri_detect(parameter, fixed = 'clust')) %>% 
    ggplot(aes(x = estimate, 
               y = var_lab, 
               color = model_type)) + 
    facet_grid(.~response, labeller = as_labeller(globals$resp_labels)) + 
    geom_vline(xintercept = 1, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0, 
                   position = position_dodge(0.9)) + 
    geom_point(shape = 16, 
               size = 2, 
               position = position_dodge(0.9)) + 
    geom_text(aes(label = est_lab), 
              position = position_dodge(0.9), 
              hjust = 0.2, 
              vjust = -0.8, 
              size = 2.75) + 
    scale_x_continuous(trans = 'log2') + 
    scale_color_manual(values = c('unadjusted' = 'steelblue4', 
                                  'severity-adjusted' = 'coral4'), 
                       name = 'Model type') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Participant clustering and CT abnormality risk', 
         x = 'OR', 
         tag = clust_outcome$ct_risk$common_tag)
  
# END ----
  
  insert_tail()
