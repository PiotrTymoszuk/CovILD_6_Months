# This script renders the legends for tables and figures ----

# data and tools ----

  library(knitr)
  library(bookdown)
  library(kableExtra)
  library(rmarkdown)

  insert_head()

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

