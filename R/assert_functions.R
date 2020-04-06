##########################################################################################################
### THIS FILE CONTAINS ALL FUNCTIONS FOR ASSERTS
##########################################################################################################


##########################################################################################################
### ASSERTIONS FOR get_random_pop
##########################################################################################################

#Test for get_random_pop
is_non_zero_integer <- function(input) {

  #numeric and single number
  assert_that(is.numeric(input), length(input) == 1)

  #whole number
  input%%1 == 0

  #non-negative
  input > 0

}

on_failure(is_non_zero_integer) <- function(call, env) {

  "Inputs must be a non-zero integer"

}


##########################################################################################################
### ASSERTIONS FOR get_prob
##########################################################################################################

#Test for get_prob function
is_rank_vector <- function(input) {

  #numeric and non-empty vector
  assert_that(is.numeric(input), length(input) >= 1)

  #all numbers should be positive
  !any(input < 0)

  #single vector
  !is.null(nrow(input))

  #integers
  !any(input%%1 != 0)

  setequal(input , seq(length(input)))

}

on_failure(is_rank_vector) <- function(call, env) {

  "Inputs must be a non-zero length vector of sequential ordered integers"

}

#test for methods in get_prob function
is_known_method <- function(input) {

  #character string
  assert_that(is.character(input))

  #known method
  input %in% c('fit', 'random')

}

on_failure(is_known_method) <- function(call, env) {

  "Current known methods are: 'fit' and 'random'"

}



##########################################################################################################
### ASSERTIONS FOR get_parents
##########################################################################################################

#Test for get_parents function
is_ranked_data_frame <- function(input) {

  #numeric and non-empty vector
  #>=3 1 chromosome with fitness and rank column
  assert_that( is.data.frame(input), ncol(input) >=3)

  #must have a rank column
  'rank' %in% colnames(input)

}

on_failure(is_ranked_data_frame) <- function(call, env) {

  "Input Must be a Data Frame with a Rank Column"

}



##########################################################################################################
### ASSERTIONS FOR get_crossover
##########################################################################################################

#check that a valid method was used
is_known_cross_method <- function(input) {
  #check method is character
  is.character(input)

  #known method
  input %in% c('single', 'multi')
}

on_failure(is_known_cross_method) <- function(call, env) {
  "Current known methods are: 'single' and 'multi'"
}


#check that a valid type was used
is_known_cross_type <- function(input) {
  #check method is character
  is.character(input)

  #known method
  input %in% c('nonrandom', 'random')
}


#check that input is a valid matrix
is_matrix <- function(input) {
  #matrix
  is.matrix(input)

  #only 0's and 1's entries
  all(input %in% c(0,1))
}

on_failure(is_matrix) <- function(call, env) {
  "Input can only be a matrix with 0's and 1's as the entries"
}



##########################################################################################################
### ASSERTIONS FOR get_mutations
##########################################################################################################

#check that a valid method was used
is_known_mut_method <- function(input) {
  #check method is character
  is.character(input)

  #known method
  input %in% c('single', 'multi')
}

on_failure(is_known_mut_method) <- function(call, env) {
  "Current known methods are: 'single' and 'multi'"
}


#check that mutation rate is valid
is_mut_rate_valid <- function(input) {
  #check if NA
  assert_that(noNA(input), msg = 'Mutation rate is a missing value')

  #check if numeric
  is.numeric(input)

  #check less than 1
  input <= 1

  #check that it is length 1
  length(input) == 1
}

on_failure(is_mut_rate_valid) <- function(call, env) {
  "Mutation rate must be numeric and less than or equal to 1"
}

##########################################################################################################
### ASSERTIONS FOR select
##########################################################################################################

#test for known stopping criterias
is_known_stop_criteria <- function(input) {
  
  #character string
  assert_that(is.character(input))
  
  #known method
  input %in% c('iteration', 'threshold')
  
}

on_failure(is_known_stop_criteria) <- function(call, env) {
  
  "Current known stopping criterias are: 'iteration' and 'threshold'"
  
}

#test for populated iterations input
is_iterations <- function(iteration_input) {
  
  assert_that(is.numeric(iteration_input))
  
  #check if whole number
  iteration_input%%1 == 0
  
}

on_failure(is_iterations) <- function(call, env) {
  
  "Iterations must be an integer >= 1"
  
}

#test for stop_point input
is_stop_point <- function(stop_point_input) {
  
  assert_that(is.numeric(stop_point_input))
  
  #check if whole number
  stop_point_input%%1 == 0
  
}

on_failure(is_stop_point) <- function(call, env) {
  
  "Stop_point must be an integer >= 1"
  
}

#test for covariates and response inputs
is_covariate_response_data <- function(covariates, response) {
  
  assert_that(is.numeric(covariates), length(covariates) >= 1)
  assert_that(is.numeric(response), length(response) >= 1)
  
  is.vector(response)
  
  #response and covariates must be equal length
  nrow(as.matrix(covariates)) == length(response)
  
}

on_failure(is_covariate_response_data) <- function(call, env) {
  
  "Inputs must be appropriate covariate and response data"
  
}