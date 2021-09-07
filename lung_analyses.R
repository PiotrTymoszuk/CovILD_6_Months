# This script checks the kinetics of lung impairment readouts (CT and lung function)
# their overlap and investigates whether early lung function assessment correlates with
# non-recovering (180 days) CT pathologies

# data and tools -----

  c('./tools/sys_tools.R', 
    './tools/counting_tools.R', 
    './tools/modeling_tools.R') %>% 
  walk(source)

  library(philentropy)

  insert_head()
  
# data container and globals -----
  
  cov_lung<- list()
  
  cov_lung$lung_vars <- c('CTsevabove5', 
                          'CT_findings', 
                          'CT_pat_GGO', 
                          'lung_function_impaired')
  
  cov_lung$lung_labels <- c('CTsevabove5' = 'CT Severity\nScore > 5', 
                            'CT_findings' = 'CT lung\nabnotmalities', 
                            'CT_pat_GGO' = 'GGOs', 
                            'lung_function_impaired' = 'Lung function\nimpairment')
  
  cov_lung$lung_colors <- c('CTsevabove5' = 'cornsilk4', 
                            'CT_findings' = 'coral3', 
                            'CT_pat_GGO' = 'dodgerblue2', 
                            'lung_function_impaired' = 'plum4')
  
# Analysis data set with the longitudinal lung function and CT pathology variables ----
  # I'm removing the onset visit (time 0), as only the severe cases were subjected to CT scans
  # The cases with the complete longitudinal record included
  
  insert_msg('Generating the analysis dataset')
  
  cov_lung$analysis_tbl <- cov_data$long_data %>% 
    filter(time != 0) %>% 
    select(ID, 
           time_months, 
           pat_group, 
           all_of(cov_lung$lung_vars))
  
# Modeling and displaying the percentages of lung function impairment and lung CT pathologies till 180 day followup ----
  # inclusion of the patient hospitalization status
  
  insert_msg('Kinteics of lung function and CT pathologies in the patient groups')
  
  ## modeling with a series of kinetic GLMERs
  
  cov_lung$kinetics$models <- cov_lung$analysis_tbl %>% 
    gather(key = 'feature', 
           value = 'present', 
           cov_lung$lung_vars) %>% 
    dlply(.(pat_group, feature), 
          function(x) glmer(present ~ time_months + (1|ID), 
                            data = x, 
                            family = 'binomial'))
  
  cov_lung$kinetics$null_models <- cov_lung$analysis_tbl %>% 
    gather(key = 'feature', 
           value = 'present', 
           cov_lung$lung_vars) %>% 
    dlply(.(pat_group, feature), 
          function(x) glmer(present ~ (1|ID), 
                            data = x, 
                            family = 'binomial'))
  
  ## checking model significance by LRT, using the BH-corrected p values in the plots
  
  cov_lung$kinetics$lrt <- map2(cov_lung$kinetics$models, 
                                cov_lung$kinetics$null_models, 
                                anova)
  
  cov_lung$kinetics$summary <- cov_lung$kinetics$lrt %>% 
    map2_dfr(., names(.), 
             function(x, y) tibble(pat_group = stri_extract(y, regex = 'G\\d'), 
                                   feature = stri_extract(y, regex = paste(cov_lung$lung_vars, collapse = '|')), 
                                   p_lrt = x$`Pr(>Chisq)`[2])) %>% 
    mutate(p_adj = p.adjust(p_lrt), 
           plot_lab = ifelse(p_adj >= 0.05, 'ns', 
                             paste('p =', signif(p_adj, 2))))
  
  
  ## plotting
  
  cov_lung$kinetics$plotting_tbl <- cov_lung$analysis_tbl %>% 
    ddply(.(time_months, pat_group), 
          count_feature_lst, 
          var_to_count_vec = cov_lung$lung_vars, 
          positive_only = F) %>% 
    filter(feature_strata == 1) %>% 
    as_tibble %>% 
    mutate(perc_lab = paste(signif(percent, 2), '%', sep = ''))
  
  cov_lung$kinetics$plot <- cov_lung$kinetics$plotting_tbl %>% 
    ggplot(aes(x = time_months, 
               y = percent, 
               fill = feature, 
               color = feature, 
               group = feature)) + 
    facet_grid(feature ~ pat_group, 
               labeller = labeller(.cols = globals$pat_group_labels, 
                                   .rows = cov_lung$lung_labels)) + 
    geom_line() + 
    geom_point(size = 2, 
               shape = 21) + 
    geom_text(aes(label = perc_lab), 
              size = 2.75, 
              hjust = 0, 
              vjust = 0, 
              nudge_y = 4) + 
    expand_limits(y = 110, x = 6.5) + 
    scale_fill_manual(values = cov_lung$lung_colors) + 
    scale_color_manual(values = cov_lung$lung_colors) + 
    scale_x_continuous(breaks = c(2, 3, 6), 
                       labels = c('2' = 60, 
                                  '3' = 100, 
                                  '6' = 180)) + 
    scale_y_continuous(breaks = seq(0, 100, by = 20)) + 
    guides(fill = F, 
           color = F) + 
    globals$common_theme + 
    theme(panel.background = element_rect(fill = 'gray95'), 
          panel.grid.major = element_line(color = 'white')) + 
    labs(x = 'Time post-COVID-19, days', 
         y = '% patient subgroup', 
         title = 'Pulmonary recovery') + 
    geom_text(data = cov_lung$kinetics$summary %>% 
                mutate(time_months = 6, 
                       percent = 100, 
                       fontface = ifelse(plot_lab != 'ns', 'bold', 'plain')), 
              aes(label = plot_lab, 
                  fontface = fontface), 
              size = 2.75, 
              hjust = 1, 
              vjust = 0.5, 
              color = 'black')
  
# Checking the overlap between the CT and lung function responses ----
  
  insert_msg('checking the co-occurence of lung function and CT pathologies')
  
  ## modeling: calculating pairwise Jacard coefficients (1 - distance) between the responses
  ## in a time-wise manner
  
  cov_lung$coocurrence$coeff_tbls <- cov_lung$analysis_tbl %>% 
    filter(complete.cases(.)) %>% 
    dlply(.(time_months), select, all_of(c('ID', cov_lung$lung_vars))) %>% 
    map(column_to_rownames, 'ID') %>% 
    map(t) %>% 
    map(distance, method = 'jaccard', use.row.names = T) %>% 
    map(function(x) 1 - x) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'response')
  
  ## representation as a heat map
  
  cov_lung$coocurrence$plot <- cov_lung$coocurrence$coeff_tbls %>% 
    map(gather, 
        key = 'response2', 
        value = 'identity', 
        cov_lung$lung_vars) %>% 
    map2_dfr(., names(.), function(x, y) mutate(x, time = y)) %>% 
    mutate(plot_lab = signif(identity, 2)) %>% 
    ggplot(aes(x = response, 
               y = response2, 
               fill = identity)) + 
    facet_grid(. ~ time, 
               labeller = as_labeller(c('2' = '60 days', 
                                        '3' = '100 days', 
                                        '6' = '180 days'))) + 
    geom_tile(color = 'black') + 
    geom_text(aes(label = plot_lab), 
              size = 2.75) + 
    scale_x_discrete(labels = cov_lung$lung_labels, 
                     name = '') + 
    scale_y_discrete(labels = cov_lung$lung_labels, 
                     name = '') + 
    scale_fill_gradient2(low = 'steelblue3', 
                         mid = 'white', 
                         high = 'firebrick4', 
                         midpoint = 0.5, 
                         name = 'Jaccard\ncoefficient') + 
    globals$common_theme + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
# Testing if the lung function impairment predicts CT pathologies by logistic regression -----
  
  insert_msg('Testing if impaired lung function predicts lung pathologies')
  
  ## models and null models
  
  cov_lung$lufo_predict$models <- cov_lung$analysis_tbl %>% 
    filter(complete.cases(.)) %>% 
    gather(key = 'response', 
           value = 'present', 
           cov_lung$lung_vars[cov_lung$lung_vars != 'lung_function_impaired']) %>% 
    dlply(.(response, time_months), 
          function(x) glm(present ~ lung_function_impaired, 
                          data = x, 
                          family = 'binomial'))
  
  cov_lung$lufo_predict$null_models <- cov_lung$analysis_tbl %>% 
    filter(complete.cases(.)) %>%  
    gather(key = 'response', 
           value = 'present', 
           cov_lung$lung_vars[cov_lung$lung_vars != 'lung_function_impaired']) %>% 
    dlply(.(response, time_months), 
          function(x) glm(present ~ 1, 
                          data = x, 
                          family = 'binomial'))
  
  ## LRT
  
  cov_lung$lufo_predict$lrt <- map2(cov_lung$lufo_predict$models, 
                                    cov_lung$lufo_predict$null_models, 
                                    anova, test = 'Chi')
  
  ## model summaries, p values corrected by BH
  
  cov_lung$lufo_predict$summary <- cov_lung$lufo_predict$models %>% 
    map(get_glm_results, 
        exponentiate = T) %>% 
    map2_dfr(., names(.), 
             function(x, y) mutate(x, 
                                   response = stri_extract(y, regex = paste(cov_lung$lung_vars, collapse = '|')), 
                                   time_months = stri_extract(y, regex = '\\d$'))) %>% 
    filter(parameter != '(Intercept)') %>% 
    mutate(p_adj = p.adjust(p_value, 'BH'), 
           plot_label = stri_replace(plot_label, regex = 'p =.*$', replacement = paste('p =', signif(p_adj, 2))))
  
# END ----
  
  insert_tail()