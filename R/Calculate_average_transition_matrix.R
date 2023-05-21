#' Title Average Transition Matrix
#'
#' @param all_transition_matrices matrix that contains transition matrix
#'
#' @description
#'This function calculates the average transition matrix with
# all_transition_matrices as the input.
#'
#' @export
#'
#' @examples
#' # Calculate all transition matrices
#'all_transition_matrices =   mapply(function(a, b){Calculate_transition_matrix(reclassified_table, a, b)},years[1:(length(years)-1)], years[2:length(years)], SIMPLIFY = FALSE)
#' Calculate average transition matrix
#' average_transition_matrix = Calculate_average_transition_matrix(all_transition_matrices)
#'
#'
Calculate_average_transition_matrix = function(all_transition_matrices){
  all_transition_matrices_multiplied = Reduce("%*%", all_transition_matrices)

  L = diag((all_transition_matrices_multiplied %>% eigen)$value)
  U = (all_transition_matrices_multiplied %>% eigen)$vectors
  Uin = ginv(U)

  average_transition_matrix =
    (U %*% (L ^ (1/length(all_transition_matrices))) %*% Uin) %>%
    `colnames<-`(colnames(all_transition_matrices_multiplied)) %>%
    `rownames<-`(rownames(all_transition_matrices_multiplied)) %>%
    print()
}
