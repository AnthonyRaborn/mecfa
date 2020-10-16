#' Exploratory CFA using Simulated Annealing
#'
#' @param initialModels The initial model as a `character` vector with lavaan model syntax. Each starting model should be its own element in the vector.
#' @param originalData The original `data.frame` with variable names.
#' @param maxSteps Numeric value for maximum number of steps for the simulated annealing algorithm.
#' @param fitStatistic String indicating the fit statistic to use. Must be an appropriate fit statistic reported by `lavaan::fitstatistic`. Default is `'cfi'`.
#' @param maximize Logical. Is the chosen `fitStatistic` to be maximized? Default is `TRUE`.
#' @param Kirkpatrick Default is `TRUE` to use the Kirkpatrick goal function. Also accepts custom-defined goal functions.
#' @param randomNeighbor Default is `TRUE` to use the default random neighbor function. Also accepts custom-defined functions to specify how random neighbor models are generated.
#' @param lavaan.model.specs A list of named values that specify the model specifications as found in `lavaan::lavaan`. Default values match the values found for `lavaan::cfa`.
#' @param maxChanges An `integer` value greater than 1 setting the maximum number of parameters to change within randomNeighbor. .
#' @param restartCriteria Either "consecutive" to restart after maxConsecutiveSelection times with the same model chosen in a row, or a user-defined function.
#' @param maximumConsecutive A positive `integer` value used with restartCriteria.
#' @param temperature Either an acceptable `character` value, or a user-defined temperature function. The acceptable values are "linear", "quadratic", or "logistic". Defaults to a custom function.
#' @param bifactor Logical. Indicates if the latent model is a bifactor model. If `TRUE`, assumes that the last latent variable in the provided model syntax is the bifactor (i.e., all of the retained items will be set to load on the last latent variable). Ignored if `maxItems==FALSE`.
#' @param ... Further arguments to be passed to other functions. Not implemented for any of the included functions.
#'
#' @export
#'
exploratorySA <-
  function(initialModels,
           originalData,
           maxSteps = 5000,
           fitStatistic = "cfi",
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
           temperature = function(currentStep, maxSteps) {
             currentTemp <- ((maxSteps - (currentStep)) / maxSteps)^7
           },
           bifactor = FALSE,
           ...) {
    setChains = length(initialModels)
    #### prepare parallel processing ####
    if (setChains > 1) {
      chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
      progressPar <- function(n) {
        cat(paste("Chain number ", n, " complete. \n", sep = ""))
      }

      if (nzchar(chk) && chk == "TRUE") {
        # use 2 cores in CRAN/Travis/AppVeyor
        num_workers <- 2L
      } else {
        if (setChains <= parallel::detectCores()) {
          num_workers <- min(setChains, parallel::detectCores()-1)
        } else {
          # use all cores in devtools::test()
          num_workers <- max(1, parallel::detectCores()-1)
        }
      }
      cl <- parallel::makeCluster(num_workers,type="PSOCK", outfile = "")
      doSNOW::registerDoSNOW(cl)

    } else {
      num_workers <- 1L
      progressSeq <- function(currentStep, maxSteps) {
        cat(paste0(
          "\r Current Step = ",
          currentStep,
          " of a maximum ",
          maxSteps,
          ".  "
        ), file = stdout())
      }


    }

    `%dopar%` <- foreach::`%dopar%`
    `%do%` <- foreach::`%do%`

    startTime <-
      Sys.time()

    chainResults <-
      foreach::foreach(chain = 1:setChains,
                       .inorder = T,
                       .verbose = T) %dopar% {
                         mapply(
                           assign,
                           names(lavaan.model.specs),
                           lavaan.model.specs,
                           MoreArgs = list(envir = environment())
                         )

                         results <-
                           ecfaSA(
                             initialModel = lavaan::cfa(
                                model = initialModels[chain],
                                data = originalData,
                                model.type = model.type,
                                estimator = estimator,
                                ordered = ordered,
                                int.ov.free = int.ov.free,
                                int.lv.free = int.lv.free,
                                auto.fix.first = auto.fix.first,
                                std.lv = std.lv,
                                auto.fix.single = auto.fix.single,
                                auto.var = auto.var,
                                auto.cov.lv.x = auto.cov.lv.x,
                                auto.efa = auto.efa,
                                auto.th = auto.th,
                                auto.delta = auto.delta,
                                auto.cov.y = auto.cov.y),
                             originalData = originalData,
                             maxSteps = maxSteps,
                             fitStatistic = fitStatistic,
                             temperature = temperature,
                             maximize = maximize,
                             Kirkpatrick = Kirkpatrick,
                             randomNeighbor = randomNeighbor,
                             lavaan.model.specs = lavaan.model.specs,
                             maxChanges = maxChanges,
                             restartCriteria = restartCriteria,
                             maximumConsecutive = maximumConsecutive,
                             bifactor = bifactor
                           )
                         results@initial_model <-
                           initialModels[chain]
                         results
                       }
    if (setChains > 1) {
      foreach::registerDoSEQ()
      parallel::stopCluster(cl)
    }

    chainResults$totalTime <-
      Sys.time()-startTime
    chainResults
  }
