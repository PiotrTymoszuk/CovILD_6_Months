# This script renders the legends for tables and figures ----

# data and tools ----

  source('./tools/sys_tools.R')

  library(knitr)
  library(bookdown)

  insert_head()
  
# Specific functions -----
  
  tab_preview <- function(inp_tbl, head_lines = 'all', signif_digits = 3) {
    
    ## makes a nice table head for preview in the figure file
    
    out_tbl <- inp_tbl %>% 
      map_dfc(function(x) if(is.numeric(x)) signif(x, digits = signif_digits) else x)
    
    if(head_lines != 'all') {
      
      return(out_tbl[1:head_lines, ])
      
    } else {
      
      return(out_tbl)
      
    }
    
  }
  
  collapse_and <- function(vector, collapse = ', ', last_link = ' and ') {
    
    ## collapses a given vector with the last_link string before the last item
    
    coll_phrase <- paste(vector[1:length(vector) - 1], collapse = collapse) %>% 
      paste(., vector[length(vector)], sep = last_link)
    
    return(coll_phrase)
    
  }
  
  
# table captions -----
  
  tab_captions <- list()
  
  tab_captions$table_variables <- 'Initial set of varibles and lung pathology readouts used for univariate modeling'
  
  tab_captions$table_univariate <- 'Summary of the results of univariate logistic regression'
  
  tab_captions$table_multi_vars <- 'The set of variables found significant by univariate modeling used for signature construction and multivariate logistic regression'
  
  tab_captions$table_multivariate <- 'Summary of the results of multivariate logistic regression for the common significanr risk signatures'
  
  tab_captions$table_n_clusters <- 'Numbers of patients displaying lung lesions and/or functional impairment in the symptom clusters'
  
  tab_captions$table_n_severity <- 'Numbers of patients displaying lung lesions and/or functional impairment stratified by severity of the acute infection'
  
# figure captions
  
  figure_captions <- list()
  
  figure_captions$analysis_scheme <- 'Scheme of data analysis, selection of the optimal risk signature and testing its performance.'
  
  figure_captions$clustering <- 'Clustering of the study participants by self-reported symptoms present in the course of acute SARS-Cov2 infection.'
  
  figure_captions$univariate_modeling_hm  <- 'Results of univariate modeling of correlation between demographic, clinical, biochemical and self-reported symptom paramaters and the non-recovering lung pathology at the 180 day followup presented as a heat map.'
  
  figure_captions$univariate_modeling_bar  <- 'Results of univariate modeling of correlation between demographic, clinical, biochemical and self-reported symptom paramaters and the non-recovering lung pathology at the 180 day followup presented as estimate forest plots.'
  
  figure_captions$multivariate_modeling <- 'Results of multivariate modeling of correlation between the candidata 2 - 4 paramater risk signatures and the non-recovering lung pathology at the 180 day followup.'
  
  figure_captions$estimate_panel <- 'OR estimate values for the best performing common significant risk signature #789 determined by multivariate logistic regression.'
  
  figure_captions$roc_analysis <- 'Results of ROC analysis for the best performing common significant #789 risk signature score.'
  
  figure_captions$performance_clust <- 'Difference in the #789 risk signature score in the participants with lung pathologies or impaired function at the 180 day followup and the pathology-free convalescents classfied by symptom cluster.'
  
  figure_captions$performance_pat_group <- 'Difference in the #789 risk signature score in the participants with lung pathologies or impaired function at the 180 day followup and the pathology-free convalescents classfied by severity of the acute SARS-Cvo2 infection.'
  
  figure_captions$CTsevabove5_recovery <- 'Difference in recovery from CT-detectable lung lesions in patients with high and low values of the #789 risk signature score.'
 
  figure_captions$CT_findings_recovery <- 'Difference in recovery from CT-detectable lung lesions in patients with high and low values of the #789 risk signature score.'
  
  figure_captions$CT_pat_GGO_recovery <- 'Difference in resolution of GGOs in patients with high and low values of the #789 risk signature score.'
   
  figure_captions$lung_pathology_recovery <- 'Difference in recovery from lung lesions or functional impairment of the lungs in patients with high and low values of the #789 risk signature score.'
  
  figure_captions$lufo_ct_patho <- 'Prediction of CT-detectable lung pathologies by examination of lung function.'
  
# n numbers ----
  
  insert_msg('N numbers for the study')  
  
  n_numbers <- list()

  n_numbers$total_cohort <- cov_data$mod_tbl %>% 
    nrow ## total patient n
  
  n_numbers$signif_vars <- cov_data$signif_vars %>% 
    length ## number of significant paramaters correlating with pathologies in univariate analysis
  
  n_numbers$combinations <- multi_globals$combi_list %>% 
    length ## initial number of candidate risk signatures
  
  n_numbers$signif_signatures <- cov_multivariate$common_signif_ids %>% 
    length ## nunber of common significant risk signatures
  
  n_numbers$clusters <- cov_clustering$clust_ass %>% 
    count(hclust_id) %>% 
    as.list
  
  n_numbers$clusters <- set_names(n_numbers$clusters$n, n_numbers$clusters$hclust_id)
  
# Generating figure legend file -----
  
  insert_msg('Rendering figure and table legend file')
  
  rmarkdown::render('covild6_figure_legends.Rmd', 
                    output_format = html_document2(number_sections = F, theme = 'readable')) 
  
  insert_msg()
  
# END -----
  
  insert_tail()

