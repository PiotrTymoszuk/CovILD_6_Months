# This program executes the project scripts

# libraries -----

  require(readr)

# listing scripts ----

  executable_scripts <- c('data_import.R', ## OK
                          'symptom_recovery.R', ## OK
                          'lung_analyses.R', ## OK
                          'univariate_modeling.R', ## OK
                          'feature_clustering.R', ## OK
                          'participant_clustering.R', ## OK
                          'clustering_verification.R', ## OK
                          'knn_cohort_size.R', ## OK
                          'k_NN_predictions.R', ## OK
                          'kNN_verification.R', ## OK
                          'paper_figures.R') ## OK
  
# error container ----
  
  exec_errors <- list()
  
# executing the script list ----
  
  for(file in executable_scripts) {
    
    diagn_output <- try(source(file, encoding = 'UTF-8'), silent = T) ## UTF-8 encoding cause of greek letters 
                                                                      ##in plots and tables
    
    if(class(diagn_output) == 'try-error') {
      
      exec_errors <- c(exec_errors, 'execution failed')
      
    } else {
      
      exec_errors <- c(exec_errors, 'execution successful')
      
    }

  }
  
  require(dplyr)
  
  exec_errors <- exec_errors %>% 
    reduce(c) %>% 
    tibble(execution_result = .)
  
  exec_errors <- exec_errors %>% 
    mutate(file = executable_scripts) %>% 
    select(file, execution_result)
  
  rm(diagn_output, executable_scripts)
  
  message(rep('-', 60))
  print(exec_errors)
  message(rep('-', 60))
  
# saving execution results on the disc ----

  exec_errors %>% 
    write_tsv('exec_log.log')
  
# saving the image -----
  
  save.image()
  
# END ----