# This script renders the legends for tables and figures ----

# data and tools ----

  source('./tools/sys_tools.R')

  library(knitr)
  library(bookdown)
  library(kableExtra)
  library(rmarkdown)

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
  
  mm_inch <- function(input_mm) {
    
    return(0.0393700787 * input_mm)
    
  }

# Generating figure legend file -----
  
  insert_msg('Rendering figure and table legend file')
  
  render('./paper/markdown/figures.Rmd', 
         output_format = word_document2(number_sections = F, theme = 'readable'), 
         output_dir = './paper/')
  
  render('./paper/markdown/supplementary_material.Rmd', 
         output_format = pdf_document2(number_sections = F, theme = 'readable'), 
         output_dir = './paper/') 
  

# END -----
  
  insert_tail()

