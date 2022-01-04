# This script checks the kinetics of lung impairment readouts (CT and lung function)
# their overlap and investigates whether early lung function assessment correlates with
# non-recovering (180 days) CT pathologies

  insert_head()
  
# data container, globals and analysis tables with the complete CT and functional readouts from the 60-day FUP on -----
  
  cov_lung<- list()
  
  ## globals
  
  cov_lung$lung_vars <- c('CT_findings', 
                          'CT_sev_low', 
                          'CTsevabove5', 
                          'lung_function_impaired')

  cov_lung$lung_labels <- c('CTsevabove5' = 'CT Severity\nScore > 5', 
                            'CT_findings' = 'CT lung\nabnormalities', 
                            'CT_sev_low' = 'CT Severity\nScore 1 - 5', 
                            'lung_function_impaired' = 'Lung function\nimpairment')
  
  cov_lung$lung_colors <- c('CTsevabove5' = 'cornsilk4', 
                            'CT_findings' = 'coral3', 
                            'CT_sev_low' = 'dodgerblue2', 
                            'lung_function_impaired' = 'plum4')
  
  ## analysis tables
  
  cov_lung$analysis_tbl <- cov_data$long_data %>% 
    filter(time != 0) %>% 
    select(ID, 
           time_months, 
           pat_group, 
           all_of(cov_lung$lung_vars))
  
  cov_lung$analysis_tbl <- cov_lung$lung_vars %>% 
    map(~select(cov_lung$analysis_tbl, ID, time_months, pat_group, .data[[.x]])) %>% 
    map(complete_cases) %>% 
    set_names(cov_lung$lung_vars)
  
  cov_lung$readout_tbls <- cov_lung$analysis_tbl %>% 
    map(set_names, c('ID', 'time_months', 'pat_group', 'response')) %>% 
    map(~dlply(.x, 'pat_group', as_tibble)) %>% 
    unlist(recursive = FALSE)
  
  cov_lung$analysis_tbl <- cov_lung$analysis_tbl %>% 
    reduce(full_join, by = c('ID', 'time_months', 'pat_group')) %>% 
    mutate(ct_patho = ifelse(CT_findings == 0, 'absent', 
                             ifelse(CT_sev_low == 1, '1-5 pt', '>5 pt')), 
           ct_patho = factor(ct_patho, c('absent', '1-5 pt', '>5 pt')))
  
# Calculating the total n numbers per analysis -----
  
  insert_msg('N numbers per analysis')
  
  cov_lung$n_numbers <- cov_lung$readout_tbls %>% 
    map(~nrow(.x)/3) %>% 
    map2_dfr(., names(.), ~tibble(pat_group = .y, 
                                  n = .x)) %>% 
    mutate(feature = stri_split_fixed(pat_group, pattern = '.', simplify = TRUE)[, 1], 
           pat_group = stri_extract(pat_group, regex = 'G\\d{1}'))

# Modeling and displaying the percentages of lung function impairment and lung CT pathologies till 180 day follow-up ----
  # inclusion of the patient hospitalization status
  
  insert_msg('Kinteics of lung function and CT pathologies in the patient groups')
  
  ## modeling with a series of kinetic GLMERs
  
  cov_lung$kinetics$models <- cov_lung$readout_tbls %>% 
    map(~glmer(formula = response ~ time_months + (1|ID), 
               data = .x, 
               family = 'binomial'))
  
  cov_lung$kinetics$null_models <- cov_lung$readout_tbls %>% 
    map(~glmer(formula = response ~ (1|ID), 
               data = .x, 
               family = 'binomial'))

  ## checking model significance by LRT, using the BH-corrected p values in the plots
  
  cov_lung$kinetics$lrt <- map2(cov_lung$kinetics$models, 
                                cov_lung$kinetics$null_models, 
                                anova)
  
  cov_lung$kinetics$summary <- cov_lung$kinetics$lrt %>% 
    map2_dfr(., names(.), 
             ~tibble(pat_group = stri_extract(.y, regex = 'G\\d'), 
                     feature = stri_extract(.y, regex = paste(cov_lung$lung_vars, collapse = '|')), 
                     p_lrt = .x$`Pr(>Chisq)`[2])) %>% 
    left_join(cov_lung$n_numbers, by = c('pat_group', 'feature')) %>% 
    mutate(p_adj = p.adjust(p_lrt), 
           plot_lab = ifelse(p_adj >= 0.05, 
                             paste0('ns (p = ', signif(p_adj, 2), ')'), 
                             paste('p =', signif(p_adj, 2))), 
           plot_lab = paste0(plot_lab, '\nn = ', n))
  
  ## plotting table with the percents
  
  cov_lung$kinetics$plotting_tbl <- cov_lung$analysis_tbl %>%
    ddply(.(time_months, pat_group), 
          count_feature_lst, 
          var_to_count_vec = cov_lung$lung_vars, 
          positive_only = FALSE) %>% 
    filter(feature_strata == 1, 
           feature != 'CT_sev_low') %>% 
    as_tibble %>% 
    mutate(perc_lab = paste(signif(percent, 2), '%', sep = ''))
  
  ## plot
  
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
              hjust = 0.1, 
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
                filter(feature != 'CT_sev_low') %>% 
                mutate(time_months = 6, 
                       percent = 100, 
                       fontface = ifelse(plot_lab != 'ns', 'bold', 'plain')), 
              aes(label = plot_lab), 
              size = 2.75, 
              hjust = 1, 
              vjust = 0.5, 
              color = 'black')
  
# Displaying the distribution of mild and moderate-sevele CT alterations ina plot ----
  
  insert_msg('CT abnormality severity')

  ## serial analysis
  
  cov_lung$distribution$analyses <- cov_lung$analysis_tbl %>% 
    mutate(time_months = factor(time_months)) %>% 
    dlply(.(pat_group)) %>% 
    map(analyze_feature, 
        variable = 'ct_patho', 
        split_var = 'time_months')
  
  cov_lung$distribution$testing_summary <- cov_lung$distribution$analyses %>% 
    map(extract_test_summary) %>% 
    map2_dfr(., names(.), ~mutate(.x, pat_group = .y)) %>% 
    mutate(p_adj = p.adjust(p_value, 'BH'), 
           p_lab = ifelse(p_adj < 0.05, 
                          paste('p =', signif(p_adj, 2)), 
                          paste0('ns (p = ', signif(p_adj, 2), ')')))
  
  cov_lung$distribution$counts <- cov_lung$distribution$analyses %>% 
    map(extract_counts)
  
  cov_lung$distribution$plots <- list(analysis_object = cov_lung$distribution$analyses, 
                                      label = stri_replace(globals$pat_group_labels, fixed = '\n', replacement = ' ')) %>% 
    pmap(plot_analysis, 
         fill_colors = unname(globals$ct_find_colors), 
         pie = FALSE, 
         cust_theme = globals$common_theme, 
         y_lab = '% patient patient subgroup', 
         x_lab = 'Time post COVID-19, days') %>% 
    map2(., cov_lung$distribution$testing_summary$p_lab, 
         ~.x + 
           labs(subtitle = .y) + 
           scale_x_discrete(labels = c(60, 100, 180))) %>% 
    map2(., cov_lung$distribution$counts, 
         ~.x + labs(tag = paste('\nn =', .y$total_n[1])))
  
# END ----
  
  insert_tail()