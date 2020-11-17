# Conditions for simulation ####
## Sample Size (per item)
n <-
  c(20, 80)
## Number of Items per Factor
items <-
  c(5, 15, 30)
## Number of Factors
f <-
  c(2, 4)
## Factor Correlations
rho <-
  c(.3, .7)
## Median Standardized Factor Loadings
beta <-
  c(.4, .6, .8)
## simulation repetitions
rep <-
  100

# Create the model syntax based on each condition ####
allConditionSyntax <-
  vector('list',
         length = length(items)*length(f)*length(rho)*length(beta)*rep)
allConditionExplanation <-
  vector(length = length(items)*length(f)*length(rho)*length(beta)*rep)
listElement <- 1
for (i in f) { # for each number of factors
  for (k in items) { # for each number of items
    for (j in beta) { # for each median standardized factor loadings
      for (l in rho) { # for each factor correlation
        for (m in 1:rep) {
          itemLoadings <-
            round( # to keep the syntax more legible
              runif(n = k * i, # k items times i factors factor loadings
                    min = j - .1, # min and max values +/- the median
                    max = j + .1),
              2)

          itemsWithLoadings <-
            paste0(itemLoadings,
                   "*Item",
                   1:length(itemLoadings))

          itemsPerFactorLabel <-
            rep(1:i, each = k)

          factorSyntax <-
            c()
          for (factors in 1:i) {
            factorSyntax[factors] <-
              paste0("f", factors, " ~ ",
                     paste0(itemsWithLoadings[itemsPerFactorLabel==factors], collapse = " + ")
              )
          }

          factorCorrelations <-
            c()
          for (factors in 1:(i-1)) {
            factorCorrelations[factors] <-
              paste0("f", factors, " ~~ ",
                     paste0(l, "*f", (factors+1):i, collapse = " + ")
              )
          }

          conditionModelSyntax <-
            c(factorSyntax,
              factorCorrelations)

          allConditionSyntax[[listElement]] <-
            conditionModelSyntax

          allConditionExplanation[listElement] <-
            paste0("Factors = ", i, ", Median Loadings = ", j,
                   ", # Items Per Factor = ", k, ", Total Items = ", k*i,
                   ", Factor Correlation = ", l, ", Repetition = ", m)

          listElement <-
            listElement + 1
        }
      }
    }
  }
}

# Create the data sets for each model syntax and sample size ####
allData <-
  vector('list',
         length = 600)

simDatElement <-
  1

set.seed(2020)
for (sample in n) {
  for (condition in 1:length(allConditionSyntax)) {
    numItems <-
      rep(c(10, 30, 60, 20, 60, 120), each = 600)
    simDat <-
      lavaan::simulateData(model = allConditionSyntax[[condition]],
                           model.type = 'cfa',
                           int.ov.free = TRUE,
                           int.lv.free = FALSE,
                           auto.fix.first = FALSE,
                           std.lv = TRUE,
                           auto.fix.single = TRUE,
                           auto.var = TRUE,
                           auto.cov.lv.x = TRUE,
                           auto.efa = TRUE,
                           auto.th = TRUE,
                           auto.delta = TRUE,
                           auto.cov.y = TRUE,
                           sample.nobs = sample*numItems[condition])

    allData[[simDatElement]] <-
      simDat

    if (simDatElement < 600) {
      simDatElement <-
        simDatElement + 1
    } else {
      filename <-
        paste0(
          stringr::str_replace_all(
            string = stringr::str_replace_all(
              string = allConditionExplanation[condition],
              pattern = "0\\.",
              replacement = "0,"
              ),
            pattern = "Repetition = 100",
            replacement = paste0("Sample Size = ", sample)
            ),
          ".RDS")

      saveRDS(object = allData,
              file = filename)

      simDatElement <-
        1

      print(paste0("File ", filename, " has been saved."))
    }

  }
}

