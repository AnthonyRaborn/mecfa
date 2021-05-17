# data to use ####
baseModels =
  c(
    ' f1 =~ x1 + x2 + x3
      f2 =~ x4 + x5 + x6
      f3 =~ x7 + x8 + x9',
    ' f1 =~ x1 + x2 + x3 + x4 + x5
      f2 =~ x6 + x7 + x8 + x9'
  )
testObj <-
  exploratorySA(
    initialModels = baseModels,
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
# mecfa_final_function ####
test_that(
  "bestECFA extracts an `ecfaSA` object", {
    expect_s4_class(
      bestECFA(testObj),
      "ecfaSA"
    )
  }
)

# exploratorySA function ####
test_that(
  "print on an `exploratorySA` object produces output", {
    expect_output(
      print(testObj)
    )
  }
)
test_that(
  "summary on an `exploratorySA` object produces a `list` object", {
    expect_type(
      summary(testObj),
      'list'
    )
  }
)
test_that(
  "plot on an `exploratorySA` object creates a list for plotting", {
    expect_type(
      plot(testObj),
      "list"
    )
  }
)
test_that(
  "serial option executes and creates an `exploratorySA` object", {
    expect_s4_class(
      exploratorySA(
        initialModels = baseModels[1],
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
        ),
      "exploratorySA"
    )
  }
)