# data to use ####
## linear  temperature ####
testObj <-
  ecfaSA(
    initialModel =
      lavaan::cfa(
        model =
    ' visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9',
    data = lavaan::HolzingerSwineford1939
      ),
    originalData = lavaan::HolzingerSwineford1939,
    maxSteps = 2,
    fitStatistic = 'cfi',
    temperature = "linear",
    maximize = T,
    Kirkpatrick = T,
    randomNeighbor = T,
    lavaan.model.specs =
      list(model.type = "cfa",
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
           auto.cov.y = TRUE,
           missing = "listwise"),
    maxChanges = 2,
    items = colnames(lavaan::HolzingerSwineford1939),
    bifactor = F
  )
# tests ####
## using linear temp object ####
test_that(
  "Fit testObj, when printed, produces an output of class `ecfaSA`", {
    expect_output(
      print(testObj)
    )

    expect_s4_class(
      testObj,
      "ecfaSA"
    )
  }
)
## using quadratic  temperature ####
test_that(
  "Quadratic temperature and bad restartCriteria produces a warning, but doesn't stop", {
    expect_warning(
      ecfaSA(
        initialModel =
          lavaan::cfa(
            model =
              ' visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9',
            data = lavaan::HolzingerSwineford1939
          ),
        originalData = lavaan::HolzingerSwineford1939,
        maxSteps = 2,
        fitStatistic = 'cfi',
        temperature = "quadratic",
        maximize = T,
        Kirkpatrick = T,
        randomNeighbor = T,
        lavaan.model.specs =
          list(model.type = "cfa",
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
               auto.cov.y = TRUE,
               missing = "listwise"),
        maxChanges = 2,
        items = colnames(lavaan::HolzingerSwineford1939),
        bifactor = F,
        restartCriteria = "what"
      )
    )
  }
)

## using logistic  temperature ####
test_that(
  "Logistic temperature and custom restartCriteria produces object of class `ecfaSA`", {
    expect_s4_class(
      ecfaSA(
        initialModel =
          lavaan::cfa(
            model =
              ' visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9',
            data = lavaan::HolzingerSwineford1939
          ),
        originalData = lavaan::HolzingerSwineford1939,
        maxSteps = 2,
        fitStatistic = 'cfi',
        temperature = "logistic",
        maximize = T,
        Kirkpatrick = T,
        randomNeighbor = T,
        lavaan.model.specs =
          list(model.type = "cfa",
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
               auto.cov.y = TRUE,
               missing = "listwise"),
        maxChanges = 2,
        items = colnames(lavaan::HolzingerSwineford1939),
        bifactor = F,
        restartCriteria =
          function(maxConsecutiveSelection,
                   consecutive) ifelse(
                     consecutive >= maxConsecutiveSelection,
                     function() {currentModel <- bestModel; consecutive = 1},
                     consecutive
                   )
      ),
      'ecfaSA'
    )
  }
)

## using custom temperatures ####
test_that(
  "custom temperatures pass (if function) or fail (if not)", {
    # valid temperature so should pass
    expect_s4_class(
      ecfaSA(
        initialModel =
          lavaan::cfa(
            model =
              ' visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9',
            data = lavaan::HolzingerSwineford1939
          ),
        originalData = lavaan::HolzingerSwineford1939,
        maxSteps = 2,
        fitStatistic = 'cfi',
        temperature =
          function(currentStep, maxSteps) sqrt(currentStep/maxSteps),
        maximize = T,
        Kirkpatrick = T,
        randomNeighbor = T,
        lavaan.model.specs =
          list(model.type = "cfa",
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
               auto.cov.y = TRUE,
               missing = "listwise"),
        maxChanges = 2,
        items = colnames(lavaan::HolzingerSwineford1939),
        bifactor = F
      ),
      'ecfaSA'
    )

    #invalid temperature so expect error
    expect_error(
      ecfaSA(
        initialModel =
          lavaan::cfa(
            model =
              ' visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9',
            data = lavaan::HolzingerSwineford1939
          ),
        originalData = lavaan::HolzingerSwineford1939,
        maxSteps = 2,
        fitStatistic = 'cfi',
        temperature =
          "not a function"
      )
    )
  }
)





test_that(
  "Not including data throws an error", {
    expect_error(
      ecfaSA(
      initialModel =
        lavaan::cfa(
          model =
            ' visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9',
          data = lavaan::HolzingerSwineford1939
        ),
      originalData = NULL,
      maxSteps = 10
    )
    )
  }
)