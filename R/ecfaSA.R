#' An S4 class for ecfaSA
#'
#' @slot algorithm Which metaheuristic algorithm was used.
#' @slot function_call The original function call.
#' @slot all_fit A vector of the model fit values.
#' @slot best_fit The best observed model fit value.
#' @slot best_model A `lavaan` object of the final solution.
#' @slot best_syntax A `character` vector of the final solution model syntax.
#' @slot initial_model A `character` vector of the initial model syntax.
#' @slot number_of_factors A `numeric` of the number of factors.
#' @slot runtime A `difftime` object of the total run time of the function.
#'
#' @importFrom methods new show
#'
#' @return An S4 object of class `ecfaSA`.
#' @export
#'
setClass('ecfaSA',
         slots =
           list(
             algorithm = "character",
             function_call = 'call',
             all_fit = 'vector',
             best_fit = 'numeric',
             best_model = 'lavaan',
             best_syntax = 'data.frame',
             initial_model = "character",
             number_of_factors = "numeric",
             runtime = 'ANY'
           )
)

#' Print method for class `ecfaSA`
#'
#' @param object An S4 object of class `ecfaSA`.
#'
#' @export
setMethod('show',
          signature = 'ecfaSA',
          definition = function(object) {

            if (is.data.frame(object@best_syntax)) {
              model.syntax <-
                modelTableToString(object)
            } else (model.syntax = object@best_syntax)

            line0 = paste0("ECFA Algorithm: ", object@algorithm)
            line1 = paste0(
              "Total Run Time: ",
              round(object@runtime[[1]], 3),
              " ",
              attr(object@runtime, "units")
              )
            line2 = suppressWarnings(
              stringr::str_wrap(
                as.vector(c("Function call:\n", object@function_call, "\n")),
                exdent = 2
              )
            )
            line3 = paste0(
              stringr::str_wrap(
                c("Final Model Syntax:\n",
                  # lavExport(object@best_model, export = F)[1])
                  unlist(strsplit(model.syntax, '\n'))),
                exdent = 2),
              collapse = "\n"
            )
            line4 = paste0(
              "Best model fit using ",
              names(object@best_fit),
              ": ",
              round(object@best_fit, 3)
            )
            to_console = paste0(c(line0, line1, line2, line3, line4),
                                collapse = "\n")
            cat(to_console)
          }
)