# This script performs univariate modeling for variables @V0 and CT pathology responses @V3
# The method of choice is logistic regression

# data and toolbox ----

  source('./tools/sys_tools.R')
  source('./tools/lm_qc_tools.R')

  library(ggvenn)
  library(cowplot)

  insert_head()
  
# data container and globals -----
  
  cov_univariate <- list()
  
  ## list of binary modeling variables
  
  cov_univariate$mod_vars <- globals$clust_vars[!globals$clust_vars %in% globals$mod_resp$response]
  
# Serial modeling: generating one-indep.variable models ----
  
  insert_msg('Serial univariate modeling')
  
  cov_univariate$mod_results <- globals$mod_resp$response %>% 
    map(make_lm_model, 
        data = cov_data$mod_tbl %>% 
          mutate(CT_findings_V3 = car::recode(CT_findings_V3, "'no' = 0; 'yes' = 1"), 
                 CTsevabove5_V3 = car::recode(CTsevabove5_V3, "'no' = 0; 'yes' = 1"), 
                 CT_pat_GGO_V3 = car::recode(CT_pat_GGO_V3, "'no' = 0; 'yes' = 1")), 
        indep_variable = cov_univariate$mod_vars,
        mod_fun = glm, 
        family = 'binomial', 
        est_transf = exp) %>% 
    set_names(globals$mod_resp$response)

# Generating a common summary table with the results of univariate modeling, adjusting for multiple comparisons ----
  
  insert_msg('A summary table holding the results of univariate modeling')

  cov_univariate$summary_tbl <- cov_univariate$mod_results %>% 
    map(function(resp) resp %>% 
          map_dfr(~.x$summary)) %>% 
    map(filter,
        level != 'baseline') %>% 
    map_dfr(mutate, 
            p_adj = p.adjust(p_value, 'BH'), 
            regulation = ifelse(p_adj >= 0.05, 
                                'ns', 
                                ifelse(estimate > 1, 
                                       'positive', 
                                       'negative')))

  cov_univariate$res_heat_map$signif_number <-cov_univariate$summary_tbl %>% 
    dlply(.(parameter), function(x) sum(x$regulation != 'ns')) %>% 
    tibble(parameter = names(.), 
           n_significant = unlist(.)) %>% 
    select(parameter, 
           n_significant) ## number of significant correlations for each variable
  
  cov_univariate$summary_tbl <- left_join(cov_univariate$summary_tbl, 
                                          cov_univariate$res_heat_map$signif_number, 
                                          by = 'parameter')

# Identifying the variables correlating with at least one of the investigated responses ----

  insert_msg('Identifying significant variables correlating with at least one response')
  
  cov_data$signif_vars <- cov_univariate$summary_tbl %>% 
    filter(n_significant > 0) %>% 
    .$variable %>% 
    stri_replace(regex = 'G\\d{1}$', replacement = '') %>% 
    unique

# A convenience vector containing the variables significantly linked to at least one CT finding type ----
  
  insert_msg('Variables correlating with at least one CT response')
  
  cov_univariate$signif_vars <- cov_univariate$summary_tbl %>% 
    dlply(.(response), filter, regulation != 'ns') %>% 
    map(function(x) x$variable)
  
  ## correlating with at least one of the responses
  
  cov_univariate$signif_vars$all <- cov_univariate$signif_vars[globals$mod_resp$response] %>% 
    reduce(union)
  
  ## correlating with all responses
  
  cov_univariate$signif_vars$common <- cov_univariate$signif_vars[globals$mod_resp$response] %>% 
    reduce(intersect)
  
# Visualizing the univariate modeling results as a classical forest plot ----
  # presented are the variables significantly associated with at least one CT response
  
  insert_msg('Forest plot with significant variables')
  
  cov_univariate$summ_forest_plot <- cov_univariate$summary_tbl %>% 
    filter(variable %in% cov_univariate$signif_vars$all) %>% 
    ggplot(aes(x = estimate, 
               y = reorder(variable, estimate), 
               fill = regulation)) + 
    facet_grid(. ~ response, 
               labeller = as_labeller(globals$resp_labels)) + 
    geom_vline(xintercept = 1, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0.15) + 
    geom_point(size = 2, 
               shape = 21) + 
    scale_fill_manual(values = globals$corr_colors, 
                      name = 'Correlation with risk') + 
    scale_y_discrete(labels = globals$mod_var_labels) + 
    scale_x_continuous(trans = 'log2') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          legend.position = 'bottom', 
          panel.background = element_rect(fill = 'gray95'), 
          panel.grid.major = element_line(color = 'white')) + 
    labs(title = 'Univariate risk modeling, logistic regression', 
         x = 'OR')
  
# Visualizing the significant modeling variables in the Venn plot -----
  
  insert_msg('Significant modeling variables in the Venn plot')
  
  ## base Venn plot
  
  cov_univariate$venn_plot <- ggvenn(cov_univariate$signif_vars[globals$mod_resp$response] %>% 
                                       set_names(globals$mod_resp$label), 
                                     show_percentage = F, 
                                     fill_color = unname(globals$resp_colors[globals$mod_resp$response]), 
                                     stroke_size = 0.5, 
                                     set_name_size = 3, 
                                     text_size = 2.75)
  
  ## text labels for the common significant factors
  
  cov_univariate$venn_plot <- plot_grid(cov_univariate$venn_plot, 
                                        ggdraw() + 
                                          draw_text(globals$mod_var_labels[cov_univariate$signif_vars$common] %>% 
                                                      stri_replace_all(fixed = '\n', replacement = ' ') %>% 
                                                      wrap_vector(line_length = 2), 
                                                    size = 8, hjust = 0, x = 0), 
                                        ncol = 2, 
                                        rel_widths = c(1, 0.8))
  
# END ----
  
  insert_tail()