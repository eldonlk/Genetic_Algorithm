######################################################################################################################
### THIS FILE CONTAINS ALL AUXILIARY FUNCTIONS
######################################################################################################################


######################################################################################################################
### GENERATE A RANDOM POPULATION
######################################################################################################################

#Get_random_pop: random population generator

#Description: create a random population with varying chromosome(0,1)
#orders as a matrix

#Usage: get_random_pop(n, c)

#Arguments:
#n (number) : Number of population members to be created
#c (chromosomes) : Number of chromosomes per population member
get_random_pop <- function(n, c) {

  #make sure inputs are appropriate
  assert_that(is_non_zero_integer(n))
  assert_that(is_non_zero_integer(c))

  return(matrix(sample(c(0,1), replace = TRUE, size = n*c), nrow = n, ncol = c))
}


######################################################################################################################
### FIND THE PROBABILIATY OF SURVIVAL
######################################################################################################################

# Get_prob: generate probabilities for parent selection
#
# Description: create a vector of probabilities based on fitness rank
# for parent selection of a genetic algorithm
#
# Usage: get_prob(x)
#
# Arguments:
#   x : a vector of ranks
# method : method of converting rank into probabilities
#
# Details:
#   x must be a vector containing only positive integers.
#   The entire vector must be an ordered sequence starting from 1
#   without any missing values in between.
#
# Currently only 2 methods are defined (random and fit).
# Random gives each element equal chance of being chosen (all probabilities are the same).
# Fit uses rank as an input and calculates the probability of being chosen by:
# 2*rank/(population_count*population_count+1).
#
# Value:
#   Output will be a vector of equal length with probabilities based on the rank respective to each position.

get_prob <- function(x, method = 'fit') {

  #make sure inputs are appropriate
  assert_that(is_rank_vector(x)) #test for vector
  assert_that(is_known_method(method)) #test if known defined method

  if (method == 'fit'){

    return((2*x)/(length(x)*(length(x)+1)))

  } else if (method == 'random'){

    return(rep(1/length(x), length(x)))

  } else {
    print("Error: method parameter must be either fit or random")
  }
}


######################################################################################################################
### GENERATE THE PARENTS
######################################################################################################################

# Get_parents: generate parents for next generation
#
# Description: create a matrix of parents / chromosome sequences
# based on fitness rank probabilities for the next generation of a genetic algorithm
#
# Usage: get_parents(x, by = 'fit')
#
# Arguments:
#   x : a vector of ranks
# fit: method of selection, only options are by fit or random selection
#
# Details:
#   x must be a data frame that contains a sequence of chromosomes,
#   their relative fitness, and their ranks based on their fitness.
#
# by determines how selection is chosen. By 'fit' uses
# 2*rank/(population_count*population_count+1).
# By random is simple random selection, where each row has equal chance of being chosen. Please view get_prob() function for more details.
#
# Value:
#   Probabilities of selection are generated using a method
#   specified by the parameter and a matrix of equal number of rows as the matrix is generated according to the probabilities created.

get_parents <- function(x, by = 'fit'){

  #make sure inputs are appropriate
  assert_that(is_ranked_data_frame(x))#test input dataframe

  #set vector of probabilities when sampling
  chance <- get_prob(x$rank, method = by)

  #list of all parents chosen
  parent_list <- sample(x$rank, size = nrow(x), prob = chance, replace = TRUE)

  #-2 top remove last 2 columns(fitness and rank)
  parents <- x[parent_list, 1:(ncol(x)-2)]

  return(as.matrix(parents))
}


######################################################################################################################
### CROSS-OVER TO CREATE OFFSPRING
######################################################################################################################

# This function performs crossover. Crossover combines the alleles of both parents. There are mutliple ways crossover
# can be performed. First, every individual can have the same crossover point or different crossover points. Second,
# there can be one crossover or two crossover points per individual.

# parent 1 is matrix of first parent
# parent 2 is matrix of second parent
# method is how crossover is determined
### single - one crossover point and same for all
### multi - 2 crossover points and same for all
# type is random or not
### nonrandom means every row is crossed at same point
### random means every row is crossed at a random (possibly different) point

get_crossover <- function(parent1, parent2, c , p, cross_method = 'single', cross_type = 'nonrandom') {

  #error and warnings
  assert_that(is_known_cross_method(cross_method))
  assert_that(is_known_cross_type(cross_type))
  assert_that(is_matrix(parent1))
  assert_that(is_matrix(parent2))
  if((c <= 2) && cross_method == 'multi'){
    stop("Cannot use 'multi' if covariates is less than 3")
  }

  #single crossover that is the same for all rows
  if(cross_method == 'single' & cross_type == 'nonrandom') {

    cross_point <- sample(seq(1,c-1,1), 1)
    #take columns from parent matrix based off cross over point
    chrom_parent1 <- parent1[ , 1:cross_point]
    chrom_parent2 <- parent2[ , (cross_point+1):c]
    #bind the columns together to create offspring that has parts of both parents
    offspring <- cbind(as.matrix(chrom_parent1), as.matrix(chrom_parent2))

  #single crossover that is random for each row
  }else if(cross_method == 'single' & cross_type == 'random') {
    #use sapply to go through each row
    offspring <- sapply(1:p, function(x) {
      #choose crossover for this specific row
      cross_point <- sample(seq(1,c-1,1), 1)
      #combine columns from parent matrix based on cross over point
      c(parent1[x, 1:cross_point], parent2[x, (cross_point+1):c])
    })
    #need the tranpose of the outputed matrix - each column is what we want as rows
    offspring <- t(offspring)

  #two cross over points that are the same for each row
  }else if(cross_method == 'multi' & cross_type == 'nonrandom') {
    #choose the two points and sort so the smaller value is listed fist
    #sorting is necessary to aid in choosing the correct columns from the parent matrices
    cross_point <- sort(sample(seq(1,c-1,1), 2))
    #if the cross points are one apart or the same, need to resample
    while((cross_point[2] - cross_point[1]) < 1){
      cross_point <- sort(sample(seq(1,c-1,1), 2))
    }
    #take columns 1 to the first cross over point
    chrom_parent1_1 <- parent1[ , 1:cross_point[1]]
    #take columns from the second cross over point to the last column
    chrom_parent1_2 <- parent1[ , (cross_point[2]+1):c]
    #take columns between cross over points
    chrom_parent2 <- parent2[ , (cross_point[1]+1):cross_point[2]]
    #combine together so that columns from parent 2 are between the columns from parent 1
    offspring <- cbind(as.matrix(chrom_parent1_1), as.matrix(chrom_parent2), as.matrix(chrom_parent1_2))

  #two cross over points that are random for each row
  }else if(cross_method == 'multi' & cross_type == 'random') {
    #use sapply to go through each row
    offspring <- sapply(1:p, function(x) {
      #same idea as above, choose cross over and keep sampling until cross over point meets criteria
      cross_point <- sort(sample(seq(1,c-1,1), 2))
      while((cross_point[2] - cross_point[1]) < 1){
        cross_point <- sort(sample(seq(1,c-1,1), 2))
      }
      #combine the columns in the same manner as above
      c(parent1[x, 1:cross_point[1]], parent2[x, (cross_point[1]+1):cross_point[2]], parent1[x, (cross_point[2]+1):c])
    })
    #like before, we need the tranpose of the outputted matrix
    offspring <- t(offspring)
  }

  #return the offpring matrix
  return(offspring)
}


######################################################################################################################
### MUTATE OFFSPRING
######################################################################################################################

# This function performs mutation on the offspring. Mutation is randomly selecting an allele and flipping it to its
# opposite value. There are mutliple ways mutation can performed. The first option is to only allow one mutation on a
# random set of individuals. The section method is to allow more than one mutation per individual and randomly select
# the alleles to be mutated. The number of mutations is controlled by the mutation rate.

# offspring is the matrix from the crossover function
# method determines if more than one mutation is allowed per individual
### single means only one mutation per individual
### multi means multiple mutaitons are allowed per individual
# mut_rate is the percentage of mutations
### any number is allowed but a warning will be issued for large mutuation rate

get_mutation <- function(offspring, c, p, mut_method = 'single', mut_rate = min(1/c, 0.01)) {

  #warnings and error messages
  assert_that(is_known_mut_method(mut_method))
  assert_that(is_matrix(offspring))
  assert_that(is_mut_rate_valid(mut_rate))

  #mutations are choosen randomly across all alleles, one individual could have more than one mutation
  if(mut_method == 'multi') {
    mutate_mat <- matrix(sample(c(1,0), prob=c(mut_rate, 1-mut_rate), size = (p*c), replace = TRUE),
                         nrow = p, ncol = c)
    offspring[which(mutate_mat == 1)] <- abs(offspring[which(mutate_mat == 1)] - 1)

  #only one mutation allowed per individual, individual and place are choosen randomly
  }else if(mut_method == 'single'){
    mutations <- sample(c(1,0), prob=c(mut_rate, 1-mut_rate), size = p, replace = TRUE)
    mutations <- cbind(which(mutations == 1), sample(seq(c), size = sum(mutations), replace = TRUE))
    offspring[mutations] <- abs(offspring[mutations]-1)

  } else{
    stop('invalid method inputted')
  }

  return(offspring)
}


#############################################################################################
### RANK THE GENERATION IN DESCENDING ORDER BASED ON FITNESS
#############################################################################################

# get_rank_desc: ranking function

# Description: add the fitness vector to the generation matrix and rank
# the generation matrix by fitness in descending order

# Usage: rank_desc(population_size, generation, fitness)

# Arguments:
# population_size (number) : Number of individuals in the population
# generation (matrix) : Matrix of the current generation's chromosomes
# fitness (vector) : Vector containing the fitness score for each of the
#   individuals in the current generation

# Details:
#   This function is used within get_fitness_and_rank() and is not intended
#   to be used by the user. get_fitness_and_rank() contains assertions that
#   ensure this function receives proper input.

# Value:
#   Output will be a matrix containing the chromosomes of the current generation,
#   a column with their fitness scores, and a column with their rank.


get_rank_desc <- function(population_size, generation, fitness){

  ranked_desc <- cbind(generation, fitness) %>% as.data.frame() %>%
    arrange(desc(fitness)) %>%  mutate(rank = seq(population_size))
  return(ranked_desc)
}

#############################################################################################
### RANK THE GENERATION IN ASCENDING ORDER BASED ON FITNESS
#############################################################################################

# get_rank_ascend: ranking function

# Description: add the fitness vector to the generation matrix and ranks
# the generation matrix by fitness in ascending order

# Usage: rank_ascend(population_size, generation, fitness)

# Arguments:
# population_size (number) : Number of individuals in the population
# generation (matrix) : Matrix of the current generation's chromosomes
# fitness (vector) : Vector containing the fitness score for each of the
# individuals in the current generation


# Details:
#   This function is used within get_fitness_and_rank() and is not intended
#   to be used by the user. get_fitness_and_rank() contains assertions that
#   ensure this function receives proper input.

# Value:
#   Output will be a matrix containing the chromosomes of the current generation,
#   a column with their fitness scores, and a column with their rank.


get_rank_ascend <- function(population_size, generation, fitness){

  ranked_desc <- cbind(generation, fitness) %>% as.data.frame() %>%
    arrange(fitness) %>%  mutate(rank = seq(population_size))
  return(ranked_desc)
}

#############################################################################################
### GET THE AIC FROM A GLM MODEL
#############################################################################################

# get_glm_AIC: generates AIC

# Description: Gets the AIC of a glm model for the purpose of assessing the
#   fitness of the model

# Usage: get_glm_AIC(response, covariates, generation, c, i)

# Arguments:
# response (vector) : Vector of the response variable input by the user
# covariates (matrix) : Matrix of the covariates input by the user
# generation (matrix) : Matrix of the current generation's chromosomes
# c (number) : Number of columns in the generation matrix
# i (number) : Current iteration of the genetic algorithm

# Details:
#   This function is used within get_fitness_and_rank() and is not intended
#   to be used by the user. get_fitness_and_rank() contains assertions that
#   ensure this function receives proper input.

# Value:
#   Output is the AIC value of the model

get_glm_AIC <- function(response, covariates, generation, c, i){

  model <- glm(response~covariates[,(seq(c) * generation[i,])])
  return(AIC(model))
}


#############################################################################################
### GET THE AIC FROM A LM MODEL
#############################################################################################

# get_lm_AIC: generates AIC

# Description: Gets the AIC of a lm model for the purpose of assessing the
#   fitness of the model

# Usage: get_lm_AIC(response, covariates, generation, c, i)

# Arguments:
# response (vector) : Vector of the response variable input by the user
# covariates (matrix) : Matrix of the covariates input by the user
# generation (matrix) : Matrix of the current generation's chromosomes
# c (number) : Number of columns in the generation matrix
# i (number) : Current iteration of the genetic algorithm

# Details:
#   This function is used within get_fitness_and_rank() and is not intended
#   to be used by the user. get_fitness_and_rank() contains assertions that
#   ensure this function receives proper input.

# Value:
#   Output is the AIC value of the model

get_lm_AIC <- function(response, covariates, generation, c, i){

  model <- lm(response~covariates[,(seq(c) * generation[i,])])
  return(AIC(model))
}

#############################################################################################
### GET THE R-SQUARED FROM A GLM MODEL
#############################################################################################

# get_glm_r_squared: generates AIC

# Description: Gets the r-squared value of a glm model for the purpose
#   of assessing the fitness of the model

# Usage: get_glm_r_squared(response, covariates, generation, c, i)

# Arguments:
# response (vector) : Vector of the response variable input by the user
# covariates (matrix) : Matrix of the covariates input by the user
# generation (matrix) : Matrix of the current generation's chromosomes
# c (number) : Number of columns in the generation matrix
# i (number) : Current iteration of the genetic algorithm

# Details:
#   This function is used within get_fitness_and_rank() and is not intended
#   to be used by the user. get_fitness_and_rank() contains assertions that
#   ensure this function receives proper input.

# Value:
#   Output is the r-squared value of the model

get_glm_r_squared <- function(response, covariates, generation, c, i){

  model <- glm(response~covariates[,(seq(c) * generation[i,])])
  r_squared <- cor(response,predict(model))^2
  return(r_squared)
}


#############################################################################################
### GET THE R-SQUARED FROM A LM MODEL
#############################################################################################

# get_lm_r_squared: generates AIC

# Description: Gets the r-squared value of a lm model for the purpose
#   of assessing the fitness of the model

# Usage: get_lm_r_squared(response, covariates, generation, c, i)

# Arguments:
# response (vector) : Vector of the response variable input by the user
# covariates (matrix) : Matrix of the covariates input by the user
# generation (matrix) : Matrix of the current generation's chromosomes
# c (number) : Number of columns in the generation matrix
# i (number) : Current iteration of the genetic algorithm

# Details:
#   This function is used within get_fitness_and_rank() and is not intended
#   to be used by the user. get_fitness_and_rank() contains assertions that
#   ensure this function receives proper input.

# Value:
#   Output is the r-squared value of the model

get_lm_r_squared <- function(response, covariates, generation, c, i){

  model <- lm(response~covariates[,(seq(c) * generation[i,])])
  r_squared <- cor(response,predict(model))^2
  return(r_squared)
}

#############################################################################################
### ASSESS THE FITNESS OF A GENERATION AND RANK ITS INDIVIDUALS BY FITNESS
#############################################################################################

# get_fitness_and_rank: ranks a generation by fitness

# Description: Assess the fitness of individuals in a generation and ranks them
#   in ascending or descending order

# Usage: get_fitness_and_rank(generation, output, input)

# Arguments:
# generation (matrix) : Matrix of the current generation's chromosomes
# response (vector) : Vector of the response variable input by the user
# covariates (matrix) : Matrix of the covariates input by the user
# model_function (character) : specifies the model function to use, accepted
#   values are 'lm' and 'glm', default value is 'lm'
# fitness_method (character) : specifies the method to assess the fitness of the
#   models, accepted values are 'AIC', 'r_squared', and 'custom', default value is
#   'AIC'
# FUN (function) : allows the user to specify a function such as BIC() to assess
#   the fitness of lm() and glm() models, the function must be able to run on output
#   from lm() or glm(), default value is NULL, if used fitness_method must be set to 'custom'
# rank_method (character) : the user must specify a ranking method if fitness_method
#   is set to 'custom', accepted values are 'ascending' and 'descending', default is NULL

# Details:
#   There are a number of conditions for the response and covariates arguments that are
#   checked by assertions in this function. The length of response must be equal to
#   the number of rows in covariates. Covariates must be numeric and may not contain any NAs.
#   It can only include 0s and 1s. Response must also be numeric.

# Value:
#   Returns a sorted matrix with the i-th generation's chromosomes augmented
#   with columns containing an assessment of their fitness and their rank.
#   The matrix is sorted by rank.


get_fitness_and_rank <- function(generation, response, covariates, model_function = 'lm', fitness_method = 'AIC',
                                 FUN = NULL, rank_method = NULL, ...) {

  # Set up variables
  population_size <- nrow(generation)
  c <- ncol(generation)

  # Create a vector to store the output of the fitness function
  fitness <- rep(NA,population_size)

  # Calculate fitness for AIC method
  if(fitness_method == 'AIC'){

    for (i in seq(population_size)){

      if(all(generation[i,] %in% 0)){

        zero_vector <- rep(0, length(response))
        model <- glm(response~zero_vector)
        fitness[i] <- AIC(model)

      } else if(model_function == 'glm'){

        fitness[i] <- get_glm_AIC(response, covariates, generation, c, i)

      } else if(model_function == 'lm'){

        fitness[i] <- get_lm_AIC(response, covariates, generation, c, i)
      }
    }

    ranked_generation <- get_rank_desc(population_size, generation, fitness)
  }

  # Calculate fitness for R-squared method
  if(fitness_method == 'r_squared'){

    for (i in seq(population_size)){

      if(all(generation[i,] %in% 0)){

        fitness[i] <- 0

      } else if(model_function == 'glm'){

        fitness[i] <- get_glm_r_squared(response, covariates, generation, c, i)

      } else if(model_function == 'lm'){

        fitness[i] <- get_lm_r_squared(response, covariates, generation, c, i)

      }
    }
    ranked_generation <- get_rank_ascend(population_size, generation, fitness)
  }
  # Calculate fitness for custom method
  if(fitness_method == 'custom'){

    if(rank_method == 'ascending'){

      for (i in seq(population_size)){

        if(model_function == 'glm'){

          fitness[i] <- FUN(glm(response~covariates[,(seq(c) * generation[i,])]))

        } else if(model_function == 'lm'){

          fitness[i] <- FUN(lm(response~covariates[,(seq(c) * generation[i,])]))
        }
      }

      ranked_generation <- get_rank_ascend(population_size, generation, fitness)

    } else if(rank_method == 'descending'){

      for (i in seq(population_size)){

        if(model_function == 'glm'){

          fitness[i] <- FUN(glm(response~covariates[,(seq(c) * generation[i,])]))

        } else if(model_function == 'lm'){

          fitness[i] <- FUN(lm(response~covariates[,(seq(c) * generation[i,])]))
        }
      }

      ranked_generation <- get_rank_desc
    }
  }
  return(ranked_generation)
}
