# This simple script contains some tools for counting variable levels#


# libraries

  require(plyr)
  require(tidyverse)
  
# functions

  complete_cases <- function(inp_tbl, index_var = 'ID') {
    
    ## excludes individuals with the incomplete longitudinal record
    
    out_tbl <- inp_tbl %>% 
      dlply(index_var, function(x) if(all(complete.cases(x))) x else NULL) %>% 
      compact %>% 
      reduce(rbind) %>% 
      as_tibble
    
    return(out_tbl)
    
  }

  count_feature <- function(inp_tbl, var_to_count, remove_na = T) {
    
    ## calculates the percentage and number of participants with/without the given feature
    
    if(remove_na) {
      
      count_tbl <- inp_tbl %>% 
        filter(!is.na(.data[[var_to_count]]), 
               .data[[var_to_count]] != 'no_answer')
      
    } else {
      
      count_tbl <- inp_tbl
      
    }
    
    feature_counts <- count_tbl %>% 
      count(.data[[var_to_count]]) %>% 
      mutate(percent = n/sum(n) * 100, 
             total_n = sum(n))
    
    return(feature_counts)
    
  }

  count_feature_lst <- function(inp_tbl, var_to_count_vec, remove_na = T, positive_only = T) {
    
    ## wrapper for variable vectors
    
    count_tbl <- var_to_count_vec %>% 
      map(count_feature, 
          inp_tbl = inp_tbl, 
          remove_na = remove_na) %>% 
      map(set_names, 
          c('feature_strata', 
            'n', 
            'percent', 
            'total_n')) %>% 
      set_names(var_to_count_vec) %>% 
      map2_dfr(., names(.), 
               function(x, y) mutate(x, feature = y))
    
    if(positive_only) {
      
      count_tbl <- count_tbl %>% 
        filter(feature_strata == 'yes')
      
    }
    
    return(count_tbl)
    
  }
  
# END ----