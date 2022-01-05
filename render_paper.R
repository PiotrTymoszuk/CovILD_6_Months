# This script renders the paper tables and figures

  insert_head()
  
# tools ----
  
  c('./tools/sys_tools.R', 
    './tools/project_tools.R') %>% 
    walk(source)
  
  library(writexl)
  library(figur) ## available at https://github.com/PiotrTymoszuk/figur
  
# paper scripts -----
  
  insert_msg('Sourcing the paper scripts')
  
  c('./paper scripts/cohort_acute_cov.R', 
    './paper scripts/cohort_outcomes.R', 
    './paper scripts/paper_tables.R', 
    './paper scripts/paper_figures.R ', 
    './paper scripts/deploy_paper.R') %>% 
    walk(source)
  
# END -----
  
  insert_tail()