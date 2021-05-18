#' Create a series of initial models with relatively equal numbers of items per
#' factor.
#'
#' @param maxFactors Either a `character` fector of factor names or a `numeric` for the number of factors to consider.
#' @param items Either a `character` vector of item names or a `numeric` for number of items.
#'
#' @export
#'

createInitialModels <-
  function(maxFactors, items) {
    factorNames <-
      if (is.numeric(maxFactors)) {
        paste0("f", 1:maxFactors)
      } else {
        maxFactors
      }
    itemNames <-
      if (is.numeric(items)) {
        paste0("Item", 1:items)
      } else {
        items
      }
    models <-
      vector(mode = 'character',
             length = length(factorNames) - 1
      )

    for (j in 1:length(models)) {
      tempFactors <-
        factorNames[1:(j+1)]
      itemAssignment <-
        suppressWarnings(
          split(itemNames,
                tempFactors)
        )

      tempItems <-
        c()
      for (k in 1:length(itemAssignment)) {
        tempItems <-
          c(tempItems,
            paste0(itemAssignment[[k]], collapse = " + ")
          )
      }

      tempModel <-
        paste0(
          paste0(tempFactors, " =~ ", tempItems),
          collapse = " \n "
        )


      models[j] <-
        tempModel
    }

    models
  }


