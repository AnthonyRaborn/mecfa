#' Pick the best-fit ECFA model(s)
#'
#' @param object A fit MECFA list.
#'
#' @export
#'
bestECFA <-
  function(object) {
    max <-
      ifelse(
        is.null(
          object@modelArgs$maximize
        ),
        TRUE, as.logical(
          as.character(
            object@modelArgs$maximize
          )
        )
      )

    all_final_fit <-
      c()
    results <-
      object@modelResults
    if (length(results) > 1) {
      for (i in 1:(length(results)-1)) {
        all_final_fit[i] <-
          results[[i]]@best_fit
      }
    } else {
      all_final_fit[1] <-
        results[[1]]@best_fit
    }
    if (max) {
      results[[which(all_final_fit==max(all_final_fit))[1]]]
    } else {
      results[[which(all_final_fit==min(all_final_fit))[1]]]
    }
  }

#' Print the model in string format
#'
#' @param model A fit MECFA list.
#'
#' @export
#'
modelTableToString <-
  function(model) {
    # convert to model.syntax for printing
    factors = unique(model@best_syntax$lhs[which(model@best_syntax$op=='=~')])
    model.syntax = c()
    for (i in factors) {
      factorRows = which(model@best_syntax$lhs==i&
                           model@best_syntax$op=="=~"&
                           model@best_syntax$free>0)
      model.syntax = c(model.syntax,
                       paste0(i, " =~ ",
                              paste0(model@best_syntax$rhs[factorRows],
                                     collapse = " + "
                              )
                       )
      )
    }
    for (i in factors) {
      factorRows = which(model@best_syntax$lhs==i&
                           model@best_syntax$op=="~~"&
                           model@best_syntax$free>0)
      if (length(factorRows) > 0) {
        model.syntax = c(model.syntax,
                         paste0(i, " ~~ ",
                                paste0(model@best_syntax$rhs[factorRows],
                                       collapse = " + "
                                )
                         )
        )
      }
    }
    model.syntax = paste0(model.syntax, collapse = "\n")
  }
