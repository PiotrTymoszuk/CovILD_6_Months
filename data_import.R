# This script imports the data of the CovILD study
# And sets the project globals

# toolbox ----

  source('./tools/sys_tools.R')
  source('./tools/project_tools.R')

  library(readxl)

  insert_head()
  
# Data container ----

  globals <- list()
  
# Globals ----
  
  globals$yes_no_recoding <- "1 = 'yes'; 0 = 'no'"
  
  ## modeling variables and responses
  
  globals$mod_vars <- read_excel('./input data/mod_variables.xlsx') %>% 
    mutate(idep_variable = paste(variable, '_V', reference, sep = ''), 
           label = stri_replace(label, fixed = '\\n', replacement = '\n'))

  globals$mod_resp <- read_excel('./input data/mod_responses.xlsx') %>% 
    mutate(label = stri_replace(label, fixed = '\\n', replacement = '\n'), 
           mod_function = fun_type %>% map(function(x) if(x == 'glm') glm else lm)) %>% 
    mutate(null_model_formula = response %>% map(function(x) paste(x, '~ 1') %>% as.formula))
  
  ## graphics
  
  globals$corr_colors <- c('negative' = 'steelblue4', 
                           'positive' = 'firebrick4', 
                           'ns' = 'gray60')
  
  globals$common_text <- element_text(size = 8, face = 'plain', color = 'black')
  
  globals$common_margin <- margin(t = 5, l = 5, r = 5, unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = globals$common_text, 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 8, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  plot.margin = globals$common_margin)
  
  ## patient groups: labels and colors
  
  globals$pat_group_labels <- c(G1 = 'Outpatient', 
                                G2 = 'Hospitalized', 
                                G3 = 'Hospitalized\noxygen therapy', 
                                G4 = 'Hospitalized\nICU')
  
  globals$pat_group_colors <- c(G1 = 'steelblue2', 
                                G2 = 'goldenrod2', 
                                G3 = 'coral3', 
                                G4 = 'coral4')
  
  ## IgG classes: labels and colors
  
  globals$ab_group_labels <- c(Ab1 = 'IgG \u2264 55', 
                               Ab2 = 'IgG (55, 113]', 
                               Ab3 = 'IgG (113, 171]', 
                               Ab4 = 'IgG > 171')
  
  globals$ab_group_colors <- c(G1 = 'steelblue3', 
                               G2 = 'cornsilk', 
                               G3 = 'coral2', 
                               G4 = 'coral4')
  
  ## variable labels and colors
  
  globals$resp_labels <- globals$mod_resp$label %>% 
    set_names(globals$mod_resp$response)
  
  globals$resp_colors <- c(CTsevabove5_V3 = 'cornsilk4', 
                           CT_findings_V3 = 'coral3', 
                           CT_pat_GGO_V3 = 'dodgerblue2')
  
  globals$mod_var_labels <- globals$mod_vars %>% 
    filter(!is.na(label))
  
  globals$mod_var_labels <- globals$mod_var_labels$label %>% 
    set_names(globals$mod_var_labels$idep_variable) %>% 
    c(., globals$pat_group_labels %>% 
        set_names(c('pat_group_V0G1', 
                    'pat_group_V0G2', 
                    'pat_group_V0G3', 
                    'pat_group_V0G4'))) %>% 
    c(., globals$ab_group_labels %>% 
        set_names(c('SarsCov2_IgG_class_V1Ab1', 
                    'SarsCov2_IgG_class_V1Ab2', 
                    'SarsCov2_IgG_class_V1Ab3', 
                    'SarsCov2_IgG_class_V1Ab4')))
  
  ## symptoms
  
  globals$symptoms <- c('dyspnoe', 
                        'cough', 
                        'fever', 
                        'night_sweat', 
                        'anosmia',
                        'ECOG_imp', 
                        'sleep_disorder')
  
  globals$symptom_labs <- c('dyspnoe' = 'dyspnea', 
                            'cough' = 'cough', 
                            'fever' = 'fever', 
                            'night_sweat' = 'night sweating', 
                            'anosmia' = 'hyposmia/anosmia',
                            'ECOG_imp' = 'imp. performance', 
                            'sleep_disorder' = 'sleeping disorders')
  
  globals$symptom_colors <- c('dyspnoe' = 'firebrick4', 
                              'cough' = 'cornsilk4', 
                              'fever' = 'coral3', 
                              'night_sweat' = 'plum3', 
                              'anosmia' = 'darkgoldenrod3',
                              'ECOG_imp' = 'steelblue3', 
                              'sleep_disorder' = 'darkseagreen3')
  
# A vactor with binary clustering variables -----
  
  insert_msg('A vector with clustering variables')
  
  globals$clust_vars <- globals$mod_vars %>% 
    filter(var_type == 'binary') %>% 
    .$idep_variable %>%
    c(globals$mod_resp$response)

# reading patient's data formatted elsewhere -----
  ## binarized fixed-time point and longitudinal patient's data used in the study analysis
  
  insert_msg('Reading patient data')
  
  load(file = './input data/project_tbls.RData')

# END ----
  
  insert_tail()