#' select()
#' @description select() implements a genetic algorithm for variable selection in linear regression and GLMs, it returns the best variables to include in your model as well as an assessment of fitness of that model and the number of generations the genetic algorithm went through before reaching its stopping criteria.
#' @param covariates A matrix, contains the covariates input by the user
#' @param response A vector, contains the response variable input by the user
#' @param stop_criteria A character, accepted values are "threshold" and "iteration", default is "threshold"
#' @param stop_point A number, determines how many previous generations of best fits match as a threshold of when to stop running if stop_criteria = "threshold", default is 3
#' @param iterations A number, determines how many iterations the genetic algorithm will run for if stop_criteria = "iteration", default is null
#' @param cross_method A charater, determines how many crossovers are performed when creating offspring , 'single' - one crossover point and same for all, 'multi' - 2 crossover points and same for all, default is 'single'
#' @param cross_type A character, determines how the crossovers are performed for each offspring, 'nonrandom' means each set of parents is crossed at same point, 'random' means each set of parents is crossed at a random (possibly different) point, default is 'nonrandom'
#' @param mut_method A character, determines if more than one mutation is allowed per individual, single means only one mutation per individual, multi means multiple mutaitons are allowed per individual, default is 'single'
#' @param mut_rate A number, specifies the mutation rate (percentage of mutations) for the genetic algorithm, default is min(1/c, 0.01)
#' @param model_function A character, specifies the model function to use, accepted values are 'lm' and 'glm', default value is 'lm'
#' @param fitness_method A character, specifies the method to assess the fitness of the models, accepted values are 'AIC', 'r_squared', and 'custom', default value is 'AIC'
#' @param FUN A function, allows the user to specify a function such as BIC() to assess the fitness of lm() and glm() models, the function must be able to run on output from lm() or glm(), default value is NULL, if used fitness_method must be set to 'custom'
#' @return Returns a list, the first entries in the list are the best variables amongst the covariates to build a model, a 0 indicates that that variable should not be included in your model, while a 1 indicates that that variable should be included, the fitness of the selected model is also included, as well as the model's rank in the current generation, and the generation number, the generation number is the number of generations created in the genetic algorithm, this is equal to the number of iterations the genetic algorithm completed
#' @export

select <- function(covariates, response, stop_criteria = "threshold", stop_point = 3, iterations = NULL, cross_method = 'single', cross_type = 'nonrandom',
                     mut_method = 'single', mut_rate = min(1/c, 0.01), model_function = 'lm',
                     fitness_method = 'AIC', FUN = NULL, rank_method = NULL, ...) {

  assert_that(is_known_stop_criteria(stop_criteria))
  assert_that(is_covariate_response_data(covariates, response))

  #Check and remove NA's
  na_rows <- unique(which(is.na(cbind(covariates, response)), arr.ind=TRUE)[,1])
  if(length(na_rows) > 0){

    response <- response[-na_rows]
    covariates <- as.matrix(covariates)[-na_rows,]

    print("Warning: NA's removed from dataset")

  }

  p <- nrow(covariates)
  c <- ncol(covariates)

  # Warning for if the mutation rate is too high
  if(mut_rate > 0.01) {
    warning('Mutation rate is higher than recommeded')
  }

  #starting point
  gen_start <- get_random_pop(p, c)

  #holding matrix/df for all generations
  gen_total <- matrix(, nrow = 1, ncol = (c+3))
  colnames(gen_total) <- c(paste("V", seq(c), sep = ""), "fitness", "rank", "gen")

  if(stop_criteria == "threshold"){

    assert_that(is_stop_point(stop_point))

    gen_count <- 1

    repeat{

      # Assess fitness of individuals and rank them
      ranked_gen <- get_fitness_and_rank(gen_start, response, covariates, model_function, fitness_method, FUN, rank_method)

      #Add current generation to holding matrix/df
      gen_total <- rbind(gen_total, cbind(ranked_gen, gen = gen_count))

      #stopping criteria
      if(gen_total %>% filter(rank == p & (gen > (gen_count-stop_point))) %>%
         dplyr::select(seq(c)) %>%
         distinct() %>%
         nrow() == 1 && gen_count >= stop_point){

        break()

      }

      #Create set of parents
      parent1 <- get_parents(ranked_gen)
      parent2 <- get_parents(ranked_gen)

      #Offspring/Next Generation
      offspring <- get_crossover(parent1, parent2, c, p, cross_method, cross_type) # Create next generation
      next_gen <- get_mutation(offspring, c, p, mut_method, mut_rate) #applying mutations

      gen_start <- next_gen
      gen_count <- gen_count + 1
    }
  }

  if(stop_criteria == "iteration"){

    assert_that(is_iterations(iterations))

    for(i in seq(iterations)){

      # Assess fitness of individuals and rank them
      ranked_gen <- get_fitness_and_rank(gen_start, response, covariates, model_function, fitness_method, FUN, rank_method)

      #Add current generation to holding matrix/df
      gen_total <- rbind(gen_total, cbind(ranked_gen, gen = i))

      if(i < iterations){ #final generation does not need parents/kids/mutations

        #Create set of parents
        parent1 <- get_parents(ranked_gen)
        parent2 <- get_parents(ranked_gen)

        offspring <- get_crossover(parent1, parent2, c, p, cross_method, cross_type)
        next_gen <- get_mutation(offspring, c, p, mut_method, mut_rate) #applying mutations

        gen_start <- next_gen
      }
    }
  }

  best_covariates <- tail(gen_total,1)
  return(best_covariates)
}
