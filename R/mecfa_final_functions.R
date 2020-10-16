#' Pick the best-fit ECFA model(s)
#'
#' @param object A fit MECFA list.
#'
#' @export
#'
bestECFA <-
  function(object) {
    all_final_fit <-
      c()
    for (i in 1:(length(object)-1)) {
      all_final_fit[i] <-
        object[[i]]@best_fit
    }
    object[[which(all_final_fit==max(all_final_fit))]]
  }

#' Print the model in string format
#'
#' @param object A fit MECFA list.
#'
#' @export
#'
modelTableToString <-
  function(object) {
    # convert to model.syntax for printing
    factors = unique(object@best_syntax$lhs[which(object@best_syntax$op=='=~')])
    model.syntax = c()
    for (i in factors) {
      factorRows = which(object@best_syntax$lhs==i&
                           object@best_syntax$op=="=~"&
                           object@best_syntax$free>0)
      model.syntax = c(model.syntax,
                       paste0(i, " =~ ",
                              paste0(object@best_syntax$rhs[factorRows],
                                     collapse = " + "
                              )
                       )
      )
    }
    for (i in factors) {
      factorRows = which(object@best_syntax$lhs==i&
                           object@best_syntax$op=="~~"&
                           object@best_syntax$free>0)
      if (length(factorRows) > 0) {
        model.syntax = c(model.syntax,
                         paste0(i, " ~~ ",
                                paste0(object@best_syntax$rhs[factorRows],
                                       collapse = " + "
                                )
                         )
        )
      }
    }
    model.syntax = paste0(model.syntax, collapse = "\n")
  }
