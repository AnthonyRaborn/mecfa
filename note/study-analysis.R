### Setup error catch and simulation management
url = 'https://discord.com/api/webhooks/789922445129678860/ILxEJH3s_0xm80emsqjNrdtpjHKQpNECDThylJj5Z8d_RBqWn7GxqMiyH1bEePimI2gG'
options(
  error = function() {
    condition <-
      ifelse(exists('condition'), condition, "setup")
    what <-
      paste0(
        '{
                "embeds": [
                {
                "title": "',
        Sys.info()['nodename'],
        ' Condition ',
        condition,
        '",
                "color": 16711680
                },
                {
                "title": "',
        paste(stringr::str_remove(geterrmessage(), '\n'), '"'),
        ',
                "color": 0
                }]}'
      )
    where = url
    RCurl::postForm(
      where,
      'Content-type' = 'application/json',
      payload_json = what,
      style = "POST"
    )
  }
)

library(lavaan)
library(stringr)
ifelse(
  require(mecfa),
  "Package loaded successfully",
  devtools::install_github("AnthonyRaborn/mecfa")
)


### Begin simulation
lavaanDesc <-
  list(model.type = "sem",
       estimator = "ML",
       ordered = NULL,
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
       auto.cov.y = TRUE)

#setwd("~/Desktop/Personal Projects/mecfa/note")
dataFiles <-
  list.files(#path = "data/",
    pattern = ".RDS")

for (i in 1:length(dataFiles)) {
  currentDataList <-
    readRDS(file = dataFiles[i])
  currentCondition <-
    stringr::str_remove(string = dataFiles[i],
                        pattern = "\\.RDS")

  message <-
    paste0("Computer **", Sys.info()['nodename'], "**\n starting condition **", currentCondition, "**.")
  RCurl::postForm(url, 'Content-type'='application/json', content=message, style= "POST")

  # create base model
  factorNumber <-
    stringr::str_extract(
      string = currentCondition,
      pattern = "(?<=Factors \\= )[0-9]{1}"
    )

  itemsNumber <-
    stringr::str_extract(
      string = currentCondition,
      pattern = "(?<=Total Items \\= )[0-9]{1,3}"
    )

  factorNames <-
    paste0("f", 1:5)
  itemNames <-
    paste0("Item", 1:itemsNumber)

  models <-
    vector(mode = 'character',
           length = length(factorNames) - 1
    )

  cfiResults <-
    bicResults <-
    vector(mode = 'list',
           length = length(currentDataList)
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

  for (l in 1:length(currentDataList)) {
    cfiResults[[l]] <-
      exploratorySA(
        initialModels = models,
        originalData = currentDataList[[l]],
        maxSteps = 4000,
        fitStatistic = 'cfi',
        maximize = T,
        lavaan.model.specs = lavaanDesc
      )
    bicResults[[l]] <-
      mecfa::exploratorySA(
        initialModels = models,
        originalData = currentDataList[[l]],
        maxSteps = 4000,
        fitStatistic = 'bic',
        maximize = F,
        lavaan.model.specs = lavaanDesc
      )

    if (l %% 10 == 0) {
      message <-
        paste0("Computer **", Sys.info()['nodename'], "**\nrunning condition **", currentCondition, "**\nis at iteration ", l, ".")
      RCurl::postForm(url, 'Content-type'='application/json', content=message, style= "POST")
      cat(message)

      saveRDS(object = cfiResults,
              file = paste0("results/", currentCondition, " CFI results.RDS"))
      saveRDS(object = bicResults,
              file = paste0("results/", currentCondition, " BIC results.RDS"))
    }

  }
}

message <-
  paste0("Computer **", Sys.info()['nodename'], " has completed its assigned conditions.")
RCurl::postForm(url, 'Content-type'='application/json', content=message, style= "POST")
