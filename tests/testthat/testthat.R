##########################################################################################################
### UNIT TESTS FOR FUNCTIONS
##########################################################################################################


##########################################################################################################
### TESTS FOR get_random_pop() FUNCTION
##########################################################################################################


test_that("get_random_pop works with proper input", {

  set.seed(1)

  base_case_n <- 3
  base_case_c <- 3

  base_case_error <- "hello"


  expected_output <- matrix(data = c(0,1,0,0,1,0,0,0,1), nrow = 3, ncol = 3)
  expect_equal(get_random_pop(base_case_n, base_case_c), expected_output)
  expect_error(get_random_pop(base_case_error, base_case_c))
})


##########################################################################################################
### TESTS FOR get_prob test FUNCTION
##########################################################################################################


test_that("get_prob works with proper input", {

  base_input <- c(1,2,3,4,5)
  base_method1 = 'fit'
  base_method2 = 'random'
  base_method3 = 'non-method'

  expected_output1 <- c(0.067, 0.133, 0.200, 0.267, 0.333)
  expected_output2 <- c(0.2, 0.2, 0.2, 0.2, 0.2)

  expect_equal(round(get_prob(base_input, base_method1), digits = 3), expected_output1)
  expect_equal(sum(get_prob(base_input, base_method1)), 1)
  expect_equal(get_prob(base_input, base_method2), expected_output2)
  expect_equal(sum(get_prob(base_input, base_method2)), 1)
  expect_error(get_prob(base_input, base_method3))
})


##########################################################################################################
### TESTS FOR get_parents() FUNCTION
##########################################################################################################


test_that("get_parents works with normal input", {

  set.seed(1)

  #creating an appropriate input
  base_input_df <- matrix(data = c(0,1,0,0,1,0,0,0,1, 600, 600, 600, 2, 3, 1), nrow = 3, ncol = 5) %>%
    as.data.frame()
  colnames(base_input_df)[4:5] <- c("fitness", "rank")

  #set up the expected output matrix
  expected_output <- matrix(data = c(0,0,1,0,0,1,1,1,0), nrow = 3, ncol = 3)
  colnames(expected_output) <- c('V1', 'V2', 'V3')
  rownames(expected_output) <- c(3, 3.1, 2)

  expect_equal(get_parents(base_input_df), expected_output)
})

##########################################################################################################
### TESTS FOR get_fitness_and_rank() FUNCTION
##########################################################################################################

# Test the input and output arguments for the get_fitness_and_rank() function

# Test to see if the input argument is a numeric matrix and the output argument is a numeric vector
test_that('get_fitness_and_rank() produces expected error if the model_function argument is not one of the supported methods', {

  set.seed(1)

  c <-5
  p <- 30
  response <- rnorm(p, 0 ,100)
  covariates <- matrix(rnorm(c*p, 0 , 100), nrow = p, ncol = c)
  generation <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  expect_error(get_fitness_and_rank(generation, "dog", covariates), "Please input a numeric matrix for the covariates and a numeric vector for the response.")
  expect_error(get_fitness_and_rank(generation, response, "cat"), "Please input a numeric matrix for the covariates and a numeric vector for the response.")
})

# Test to see if the input and output arguments are the same length
test_that('get_fitness_and_rank() produces expected error if the model_function argument is not one of the supported methods', {

  set.seed(1)

  c <-5
  p <- 30
  response <- rnorm(p, 0 ,100)
  covariates <- matrix(rnorm(c*p, 0 , 100), nrow = p, ncol = c)
  generation <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  # Create longer response and covariates arguments
  response_long <- c(response, 0)
  covariates_long <- rbind(covariates, c(1, 2, 3, 1, 1))

  expect_error(get_fitness_and_rank(generation, response, covariates_long), "Please use covariates and response arguments with the same length.")
  expect_error(get_fitness_and_rank(generation, response_long, covariates), "Please use covariates and response arguments with the same length.")
})

# ==========================================================================================

# Test the model_function argument for the get_fitness_and_rank() function

# Test to see if the model_function argument is not one of the supported methods
test_that('get_fitness_and_rank() produces expected error if the model_function argument is not one of the supported methods', {

  set.seed(1)

  c <-5
  p <- 30
  response <- rnorm(p, 0 ,100)
  covariates <- matrix(rnorm(c*p, 0 , 100), nrow = p, ncol = c)
  generation <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  expect_error(get_fitness_and_rank(generation, response, covariates, model_function = 'quadratic'), "Please use one of the supported model functions: \'glm\' or \'lm\'.")
  expect_error(get_fitness_and_rank(generation, response, covariates, model_function = 10), "Please use one of the supported model functions: \'glm\' or \'lm\'.")
})

# ==========================================================================================

# Test the fitness_method argument for the get_fitness_and_rank() function

# Test to see if the fitness_method argument is not one of the supported methods
test_that('get_fitness_and_rank() produces expected error if the fitness_method argument is not one of the supported methods', {

  set.seed(1)

  c <-5
  p <- 30
  response <- rnorm(p, 0 ,100)
  covariates <- matrix(rnorm(c*p, 0 , 100), nrow = p, ncol = c)
  generation <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  expect_error(get_fitness_and_rank(generation, response, covariates, fitness_method = 'dog'), "Please use one of the supported fitness methods: \'AIC\', \'r_squared\', or \'custom\'.")
  expect_error(get_fitness_and_rank(generation, response, covariates, fitness_method = 9), "Please use one of the supported fitness methods: \'AIC\', \'r_squared\', or \'custom\'.")
})

# ==========================================================================================

# Test the generation argument for the get_fitness_and_rank() function

# Test to see if the generation argument is a matrix
test_that('get_fitness_and_rank() produces expected error if the generation matrix is not a matix', {

  set.seed(1)

  c <-5
  p <- 30
  response <- rnorm(p, 0 ,100)
  covariates <- matrix(rnorm(c*p, 0 , 100), nrow = p, ncol = c)
  generation <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  expect_error(get_fitness_and_rank('squirrel', response, covariates), "Please input a matrix for the generation.")
})

# Test to see if the generation argument contains entries that are not 0 or 1
test_that('get_fitness_and_rank() produces expected error if the generation matrix does not only contain 0s and 1s', {

  set.seed(1)

  c <-5
  p <- 30
  response <- rnorm(p, 0 ,100)
  covariates <- matrix(rnorm(c*p, 0 , 100), nrow = p, ncol = c)
  generation <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  # Create a generation matrix with numbers not equal to 0 or 1
  generation_1 <- generation
  generation_1[5,] <- c(0,1,5,0,1)

  # Create a generation matrix with an NA value
  generation_2 <- generation
  generation_2[5,] <- c(0,1,NA,0,1)

  expect_error(get_fitness_and_rank(generation_1, response, covariates), "Please ensure that the generation matrix only contains 0s and 1s.")
  expect_error(get_fitness_and_rank(generation_2, response, covariates), "Please ensure that the generation matrix only contains 0s and 1s.")
})


# ==========================================================================================

# Test to make sure that get_fitness_and_rank() produces expected output

test_that('get_fitness_and_rank() produces expected output', {

  set.seed(2)

  # Generate covariates and response
  covariates <- cbind(runif(8, 0, 1), rnorm(8, 0, 1), runif(8, 3, 4))
  response <- covariates[,1] + covariates[,2]
  generation <- as.matrix(expand.grid(0:1, 0:1, 0:1))

  # We expect the best model to be the one that includes only variables 1 and 2 since they
  # should perfectly explain the response
  expected_output <- as.integer(AIC(lm(response~covariates[,1:2])))

  output <- as.integer(get_fitness_and_rank(generation, response, covariates)[8,4])

  #all.equal(get_fitness_and_rank(generation, response, covariates)[8,4], expected_output)
  expect_equivalent(output, expected_output)

})


##########################################################################################################
### TESTS FOR get_crossover FUNCTION
##########################################################################################################


test_that("dimensions of input and output match", {
  c <- 5
  p <- 10
  parent1 <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)
  parent2 <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)
  offspring <- get_crossover(parent1, parent2, c, p)

  expect_equal(dim(parent1), dim(parent2), dim(offspring))
})


test_that("single crossover", {
  set.seed(1)
  c <- 5
  p <- 4
  parent1 <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)
  parent2 <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  #nonrandom
  #this is the expected matrix with the given seed
  expected_non <- matrix(c(0,1,0,0, 1,0,0,0, 0,1,1,1, 0,1,0,0, 1,0,1,1), nrow = p, ncol = c)
  evaluated_non <- get_crossover(parent1, parent2, c, p, cross_method = 'single', cross_type = 'nonrandom')
  expect_equal(expected_non, evaluated_non)

  #random
  #this is the expected matrix with the given seed
  expected_ran <- matrix(c(0,1,0,0, 1,0,0,0, 0,1,0,0, 0,1,0,1, 1,0,1,1), nrow = p, ncol = c)
  evaluated_ran <- get_crossover(parent1, parent2, c, p, cross_method = 'single', cross_type = 'random')
  expect_equal(expected_ran, evaluated_ran)
})

test_that("multi crossover", {
  set.seed(1)
  c <- 7
  p <- 4
  parent1 <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)
  parent2 <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  #nonrandom
  #this is the expected matrix with the given seed
  expected_non <- matrix(c(0,1,0,0, 1,0,0,0, 1,0,1,1, 1,1,0,1, 1,1,1,1, 0,0,1,0, 0,0,1,0),
                           nrow = p, ncol = c)
  evaluated_non <- get_crossover(parent1, parent2, c, p, cross_method = 'multi', cross_type = 'nonrandom')
  expect_equal(expected_non, evaluated_non)

  #random
  #this is the expected matrix with the given seed
  expected_ran <- matrix(c(0,1,0,0, 1,0,0,0, 1,0,0,0, 1,1,0,1, 1,1,1,0, 0,0,0,0, 1,1,0,0), nrow = p, ncol = c)
  evaluated_ran <- get_crossover(parent1, parent2, c, p, cross_method = 'single', cross_type = 'random')
  expect_equal(expected_ran, evaluated_ran)
})

test_that("function errors when it should", {
  c <- 7
  p <- 4
  parent1 <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)
  parent2 <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  #invalid cross_method
  expect_error(get_crossover(parent1, parent2, c, p, cross_method = 'invalid'))

  #invalid cross_type
  expect_error(get_crossover(parent1, parent2, c, p, cross_type = 'invalid'))

  #invalid inputs
  expect_error(get_crossover(3, 3, c, p))
  expect_error(get_crossover('invalid1', parent2, c, p))
})


##########################################################################################################
### TESTS FOR get_mutation FUNCTION
##########################################################################################################


test_that("output is what is expected", {
  c <- 5
  p <- 3
  offspring <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  offspring_mutate <- get_mutation(offspring, c, p)

  expect_equal(dim(offspring), dim(offspring_mutate))
})

test_that('single mutation', {
  set.seed(1)
  c <- 5
  p <- 3
  offspring <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  #with the given seed, this is the matrix that should be created
  expected <- matrix(c(0,1,0, 0,1,1, 0,0,1, 1,0,0, 0,0,0), p, c)
  evaluated <- get_mutation(offspring, c, p)

  expect_equal(expected, evaluated)
})

test_that('multiple mutation', {
  set.seed(1)
  c <- 5
  p <- 3
  offspring <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  #with the given seed, this is the matrix that should be created
  expected <- matrix(c(0,1,1, 0,1,0, 0,0,1, 1,0,0, 0,0,0), p, c)
  evaluated <- get_mutation(offspring, c, p, mut_method = 'multi')

  expect_equal(expected, evaluated)
})

test_that('function errors when it should', {
  c <- 5
  p <- 3
  offspring <- matrix(sample(c(0,1), replace=TRUE, size=c*p), nrow = p, ncol = c)

  #invalid mut_method
  expect_error(get_mutation(offspring, c, p, mut_method = 'invalid'))

  #invalid mutation rate
  expect_error(get_mutation(offspring, c, p, mut_rate = 20))
  expect_error(get_mutation(offspring, c, p, mut_rate = 'invalid'))
  expect_error(get_mutation(offspring, c, p, mut_rate = c(1,2)))

  #invalid input
  expect_error(get_mutations('invalid', c, p))
  expect_error(get_mutation(2, c, p))
})


##########################################################################################################
### TESTS FOR select FUNCTION
##########################################################################################################

test_that("select works for all types of inputs including errors", {

  set.seed(1)

  #create base testing to test upon
  population <- 6
  x1 <- runif(population)
  x2 <- runif(population)
  x3 <- runif(population)
  x3 <- runif(population)
  x4 <- runif(population)
  x5 <- runif(population)

  base_output <- 3*x1 + 4*x3 + rnorm(population)
  base_input <- cbind(x1,x2,x3,x4,x5)

  expected_output1 <- c(0,1,1,1,1,-12,6,5)
  expected_output2 <- c(0,0,1,1,1,5,6,8)

  #working tests
  expect_equal(as.integer(select(base_input, base_output, stop_criteria = 'iteration', iterations = 5)), expected_output1)
  expect_equal(as.integer(select(base_input, base_output, stop_criteria = 'threshold', stop_point = 5)), expected_output2)

  #error tests
  expect_error(select(base_input, base_output, stop_criteria = 'test', stop_point = 5))
  expect_error(select(base_input, base_output, stop_point = 3.5))
  expect_error(select(base_input, base_output, stop_criteria = 'iteration', iterations = "a"))
})
