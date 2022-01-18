# this script investigates the overlap between the symptoms, lung function impairment, any CT abnormality and the severe abnormality
# The tools of choice: Venn plots and Cohen's kappa for the prediction of the radiological lesions
# Investigations done for the particular time points

  insert_head()
  
# globals -----
  
  overlap <- list()
  
  ## globals
  
  overlap$clin_variables <- c('sympt_present', 
                              'lung_function_impaired')
  
  overlap$ct_variables <- c('CT_findings', 
                            'CTsevabove5')

  overlap$var_labels <- c('CTsevabove5' = 'CT Severity\nScore > 5', 
                          'CT_findings' = 'CT lung\nabnormalities',
                          'lung_function_impaired' = 'Lung function\nimpairment', 
                          'sympt_present' = 'Persistent\nsymptoms')
  
  overlap$var_colors <- c('CTsevabove5' = 'cornsilk4', 
                          'CT_findings' = 'coral3',
                          'lung_function_impaired' = 'plum4', 
                          'sympt_present' = 'steelblue')
  
# analysis tables and n numbers -----
  
  insert_msg('Analysis tables')
  
  overlap$analysis_tbl <- cov_data$long_data %>% 
    filter(time_months != 0) %>% 
    select(ID, time_months, all_of(overlap$clin_variables), all_of(overlap$ct_variables)) %>% 
    dlply(.(time_months), as_tibble) %>% 
    map(~filter(.x, complete.cases(.x)))
  
  overlap$n_numbers <- overlap$analysis_tbl %>% 
    map(nrow)
  
  overlap$n_tags <- overlap$n_numbers %>% 
    map(~paste('n =', .x))
  
# displaying the overlap in Venn plots -----
  
  insert_msg('Venn plots of the overlap')
  
  overlap$venn$CT_findings <- list(data = overlap$analysis_tbl,
                                   plot_title = c('60 days', '100 days', '180 days'), 
                                   plot_tag = overlap$n_tags) %>% 
    pmap(plot_n_venn, 
         overlap_vars = c('sympt_present', 'lung_function_impaired', 'CT_findings'), 
         fill_color = c('SteelBlue', 'Plum', 'Coral'))

  overlap$venn$CTsevabove5 <- list(data = overlap$analysis_tbl,
                                   plot_title = c('60 days', '100 days', '180 days'), 
                                   plot_tag = overlap$n_tags) %>% 
    pmap(plot_n_venn, 
         overlap_vars = c('sympt_present', 'lung_function_impaired', 'CTsevabove5'), 
         fill_color = c('SteelBlue', 'Plum', 'BurlyWood'))
  
# Cohen's Kappa -----
  
  insert_msg('Cohens Kappa')
  
  ## Calculation of the kappas
  
  overlap$kappa$stats$CT_findings <- overlap$analysis_tbl %>% 
    map(~map_dfr(overlap$clin_variables, 
                 get_kappa, 
                 data = .x, 
                 variable1 = 'CT_findings')) %>% 
    map2_dfr(., names(.), ~mutate(.x, time_months = .y))
    
  
  overlap$kappa$stats$CTsevabove5 <- overlap$analysis_tbl %>% 
    map(~map_dfr(overlap$clin_variables, 
                 get_kappa, 
                 data = .x, 
                 variable1 = 'CTsevabove5')) %>% 
    map2_dfr(., names(.), ~mutate(.x, time_months = .y))
  
  overlap$kappa$stats <- overlap$kappa$stats %>% 
    reduce(rbind) %>% 
    mutate(p_adj = p.adjust(p_value, 'BH'))
  
  ## plotting
  
  overlap$kappa$plot <- overlap$kappa$stats %>% 
    plot_kappa + 
    facet_grid(.~ time_months, 
               labeller = as_labeller(c('2' = '60 days', 
                                        '3' = '100 days', 
                                        '6' = '180 days'))) + 
    theme(panel.background = element_blank()) + 
    scale_y_discrete(labels = overlap$var_labels) + 
    scale_x_discrete(labels = overlap$var_labels)
  
# END ----
  
  insert_tail()