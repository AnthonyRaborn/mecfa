#' An adaptation of the simulated annealing algorithm for exploratory psychometric
#' models.
#'
#' @description Simulated annealing mimics the physical process of annealing metals together. [Kirkpatrick et al. (1983)](https://science.sciencemag.org/content/220/4598/671) introduces this analogy and demonstrates its use; the implementation here follows this demonstration closely, with some modifications to make it better suited for psychometric models. Meant for exploratory purposes, e.g., finding the best-suited factor
#' structure given a starting model. This is not meant to be accessed directly!
#'
#' @details \strong{Outline of the Pieces of the Simulated Annealing Algorithm}
#' * initialModel -- the initial, full form
#' * currentModel -- the model of the current step
#' * maxSteps -- the maximum number of steps (iterations)
#' * currentStep -- the current step
#' * currentTemp -- the current temperature. A function of the number of steps (such that temp = 0 at maxSteps), and values that control the shape of the overall temperature. A part of the function that determines the acceptance probability of newly -- generated models
#' * randomNeighbor -- a function that determines how the form is changed at each step. Should be able to change one or more parameters, and should have a way to control how many are changed.
#' * goal -- a function that determines the "goodness" of the currentModel. Typically in SA goodness is defined as minimization! Sometimes called an energy function
#' * selectionFunction -- a function that determines if a randomNeighbor change is accepted. Uses the goal function that determines the "goodness" of the currentModel and the "goodness" of the randomNeighbor, and the currentTemp to generate a probability of acceptance, then compares this probability to a Uniform(0,1) variable to determine if accepted or not. A standard version of this is:
#' ![](SA-goal.jpg)
#' (Kirkpatrick et al., 1983)
#' * bestModel -- the model with the best value of the goal function achieved so far
#' * bestGoal -- the best value of the goal function achieved so far
#' * restartCriteria -- if utilized, this would "restart" the SA process by changing currentModel to bestModel and continuing the process. Could be based on (1) the currentStep value, (2) the difference between goal(currentModel) and goal(bestModel), (3) randomness (i.e., could randomly restart, could randomly restart based on some values, etc), (4) other critera.
#'
#' @param initialModel The initial model as a `character` vector with lavaan model.syntax.
#' @param originalData The original `data.frame` with variable names.
#' @param maxSteps The number of iterations for which the algorithm will run.
#' @param fitStatistic Either a single model fit statistic produced by lavaan, or a user-defined fit statistic function.
#' @param temperature Either an acceptable `character` value, or a user-defined temperature function. The acceptable values are "linear", "quadratic", or "logistic".
#' @param maximize Logical indicating if the goal is to maximize (`TRUE`) the fitStatistic for model selection.
#' @param Kirkpatrick Either `TRUE` to use Kirkpatrick et al. (1983) acceptance probability, or a user-defined function for accepting proposed models.
#' @param randomNeighbor Either `TRUE` to use the included function for randomNeighbor selection, or a user-defined function for creating random models.
#' @param lavaan.model.specs A `list` which contains the specifications for the
#'  lavaan model. The default values are the defaults for lavaan to perform a
#'  CFA. See \link[lavaan]{lavaan} for more details.
#' @param maxChanges An `integer` value greater than 1 setting the maximum number of parameters to change within randomNeighbor. When creating a short form, should be no greater than the smallest reduction in items loading on one factor; e.g., when reducing a 2-factor scale from 10 items on each factor to 8 items on the first and 6 items on the second, maxChanges should be no greater than 2.
#' @param restartCriteria Either "consecutive" to restart after maxConsecutiveSelection times with the same model chosen in a row, or a user-defined function.
#' @param maximumConsecutive A positive `integer` value used with restartCriteria.
#' @param maxItems When creating a short form, a `vector` of the number of items per factor you want the short form to contain. Defaults to `NULL`.
#' @param items A `character` vector of item names. Defaults to `NULL`.
#' @param bifactor Logical. Indicates if the latent model is a bifactor model. If `TRUE`, assumes that the last latent variable in the provided model syntax is the bifactor (i.e., all of the retained items will be set to load on the last latent variable). Ignored if `maxItems==FALSE`.
#' @param ... Further arguments to be passed to other functions. Not implemented for any of the included functions.
#'
#' @return A named list: the 'bestModel' found, the 'bestFit', and 'allFit' values found by the algorithm.
#'
#' @import lavaan utils
#' @md
#' @keywords internal

ecfaSA <-
  function(initialModel,
           originalData,
           maxSteps,
           fitStatistic = "cfi",
           temperature = "linear",
           maximize = TRUE,
           Kirkpatrick = TRUE,
           randomNeighbor = TRUE,
           lavaan.model.specs = list(
             model.type = "cfa",
             auto.var = TRUE,
             estimator = "default",
             ordered = NULL,
             int.ov.free = TRUE,
             int.lv.free = FALSE,
             std.lv = TRUE,
             auto.fix.first = FALSE,
             auto.fix.single = TRUE,
             auto.cov.lv.x = TRUE,
             auto.th = TRUE,
             auto.delta = TRUE,
             auto.cov.y = TRUE
           ),
           maxChanges = 5,
           restartCriteria = "consecutive",
           maximumConsecutive = 25,
           items = NULL,
           bifactor = FALSE,
           ...) {
    #### initial values ####
    if(!exists('originalData')) {
      stop("Please check that you have included the original data frame!")
    }
    allFit <- c()

    # creates objects in the function environment that are fed into the lavaan function in order to fine-tune the model to user specifications
    checkModelSpecs(lavaan.model.specs)
    mapply(
      assign,
      names(lavaan.model.specs),
      lavaan.model.specs,
      MoreArgs = list(envir = environment())
    )

    currentModel <-
      bestModel <-
      modelWarningCheck(
        lavaan::lavaan(
          model = initialModel,
          data = originalData,
          model.type = model.type,
          int.ov.free = int.ov.free,
          int.lv.free = int.lv.free,
          auto.fix.first = auto.fix.first,
          std.lv = std.lv,
          auto.fix.single = auto.fix.single,
          auto.var = auto.var,
          auto.cov.lv.x = auto.cov.lv.x,
          auto.th = auto.th,
          auto.delta = auto.delta,
          auto.cov.y = auto.cov.y,
          ordered = ordered,
          estimator = estimator,
        ),
        modelSyntax = lavaan::partable(initialModel)
      )
    bestFit <- tryCatch(
      lavaan::fitmeasures(object = currentModel$model.output,
                          fit.measures = fitStatistic),
      error = function(e, checkMaximize = maximize) {
        if (length(e) > 0) {
          if (checkMaximize == TRUE) {
            return(0)
          } else {
            return(Inf)
          }
        }
      }
    )



    #### selecting functions for use in algorithm ####
    if (class(temperature) == "function") {
      temperatureFunction <- temperature
    } else if (temperature == "linear") {
      temperatureFunction <- linearTemperature
    } else if (temperature == "quadratic") {
      temperatureFunction <- quadraticTemperature
    } else if (temperature == "logistic") {
      temperatureFunction <- logisticTemperature
    } else {
      stop(
        "You need to specify an appropriate default temperature (one of \"linear\", \"quadratic\", or \"logistic\") or include a temperature in the form of a function to continue."
      )
    }

    if (class(restartCriteria) == "function") {
      restartCriteria <- restartCriteria
    } else if (restartCriteria == "consecutive") {
      restartCriteria <- consecutiveRestart
    } else {
      restartCriteria <- function(maxConsecutiveSelection, consecutive) {

      }
      warning(
        "The restart criteria should to be either \"consecutive\" (the default) or a custom function. It has been set to NULL so the algorithm will not restart at all."
      )
    }

    currentStep <- 1
    consecutive <- 0

    #### perform algorithm ####
    start.time <- Sys.time()

    cat("\n Current Progress: \n")

    allModel <- currentModel$model.syntax
    allFit <- bestFit

    while (currentStep < maxSteps) {

      # how many changes to make?
      numChanges <- sample(2:maxChanges, size = 1)
      # generate random model
      randomNeighborModel <- randomNeighborFull(
        currentModelObject = currentModel$model.output,
        numChanges = numChanges,
        data = originalData
      )
      # select between random model and current model
      currentModel <- selectionFunction(
        currentModelObject = currentModel,
        randomNeighborModel = randomNeighborModel,
        currentTemp = temperatureFunction(currentStep, maxSteps),
        maximize = maximize,
        fitStatistic = fitStatistic,
        consecutive = consecutive
      )
      # recored fit
      allFit[currentStep + 1] <- tryCatch(
        # allFit <- tryCatch(
        lavaan::fitmeasures(object = currentModel$model.output, fit.measures = fitStatistic),
        error = function(e) {
          if (length(e) > 0) {
            bestFit
          }
        }
      )
      # check for current best model
      bestModel <- checkModels(currentModel, fitStatistic, maximize, bestFit, bestModel)
      bestFit <- tryCatch(
        lavaan::fitmeasures(object = bestModel$model.output, fit.measures = fitStatistic),
        error = function(e) {
          if (length(e) > 0) {
            bestFit
          }
        }
      )

      allModel <- c(allModel, currentModel$model.syntax)

      # restart if the same model was chosen too many times
      restartCriteria(
        maxConsecutiveSelection = maximumConsecutive,
        consecutive = consecutive
      )
      currentStep <- currentStep + 1
    }

    best_fit <-
      as.numeric(bestFit)

    names(best_fit) <-
      names(bestFit)

    all_fit <-
      as.numeric(allFit)


    results <-
      new(
      'ecfaSA',
      algorithm = "Simulated Annealing",
      function_call = match.call(),
      all_fit = all_fit,
      best_fit = best_fit,
      best_model = bestModel$model.output,
      best_syntax = as.data.frame(bestModel$model.syntax),
      number_of_factors = length(lavaan::lavaanNames(bestModel$model.output,
                                                     type = "lv")),
      runtime = Sys.time() - start.time
      )

    results
  }

