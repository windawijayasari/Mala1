# project_LC_change
project_LC_change = function(initial_lc, 
                             transition_matrix, 
                             number_of_years,
                             reference_final_lc){
  final_lc_probs = initial_lc %*% 
    (transition_matrix ^ number_of_years)
  
  # create a function to convert probabilities to binaries
  from_probs_to_binary = function(scoretable){
    binarytable = scoretable %>%
      as_tibble() %>%
      mutate(Class = names(.)[max.col(.)],
             val = 1) %>%
      dplyr::select(Class, val) %>%
      rowid_to_column() %>%
      spread(Class, val, fill = 0) %>%
      dplyr::select(Forest, Agriculture,Developed) %>%
      as.matrix()%>%
    print()
    
    # #count total occurrences of each class
    # class_counts = colSums(binarytable)
    # print(class_counts)
  }
  
  
  final_lc_binary = from_probs_to_binary(final_lc_probs)%>%
  print()
  
  #errors = sum(final_lc_binary != reference_final_lc)
  #print(paste0("Number of errors: ", errors))
  
  performance = (reference_final_lc * final_lc_binary) %>% 
    colSums() %>%
    sum()%>%
  print()
  
  performance = performance / dim(reference_final_lc)[1]
  
  
  return(list(probs = final_lc_probs, 
              binary = final_lc_binary, 
              performance = performance))
}

