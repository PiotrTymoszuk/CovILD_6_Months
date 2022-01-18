# This program executes the project scripts

# tools -----

  library(soucer) ## available at https://github.com/PiotrTymoszuk/soucer

# executing the main scripts scripts ----

  exec_result <- source_all(paths = c('data_import.R', 
                                      'data_analysis.R', 
                                      'render_paper.R'), 
                            message = TRUE, 
                            crash = FALSE)
  
  print(exec_result)

# saving execution results on the disc ----

    write_tsv(exec_result, 'exec_log.log')

# saving the image -----

  save.image()

# END ----