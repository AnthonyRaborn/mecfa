test_that(
  "fitmeasuresCheck returns silent when working, error when not", {
    #when provided correct fitmeasure
    expect_silent(
      fitmeasuresCheck('cfi')
      )

    #when provided incorrect fitmeasure as string
    expect_error(
      fitmeasuresCheck('something')
    )

    #when provided incorrect fitmeasure as not-a-string
    expect_error(
      fitmeasuresCheck(2)
    )
    }
  )

test_that(
  "checkModelSpecs returns silent when working, error when not", {
    # full, correct model specs list
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

    # correct model specs list with missing values
    lavaanErr <-
      list(model.type = "sem",
           estimator = "ML",
           ordered = NULL,
           int.ov.free = TRUE,
           std.lv = TRUE,
           auto.fix.single = TRUE,
           auto.var = TRUE,
           auto.cov.lv.x = TRUE,
           auto.efa = TRUE,
           auto.th = TRUE,
           auto.delta = TRUE,
           auto.cov.y = TRUE)

    expect_silent(
      checkModelSpecs(lavaanDesc)
    )

    expect_error(
      checkModelSpecs(lavaanErr)
    )
  }
  )

test_that(
  "fitWarningCheck returns value when working and 0 or Inf or -Inf depending on maximize", {
    exampleCFI <-
      0.95
    exampleChisq <-
      120

    # normal input/output
    expect_equal(
      object =
        fitWarningCheck(expr = exampleCFI,
                      maximize = TRUE),
      expected = exampleCFI
    )

    expect_equal(
      object =
        fitWarningCheck(expr = exampleChisq,
                      maximize = FALSE),
      expected = exampleChisq
    )

    # error input, maximize
    expect_equal(
      object =
        fitWarningCheck(expr = 'exampleNull'/2,
                        maximize = TRUE),
      expected = 0
    )

    # error input, minimize
    expect_equal(
      object =
        fitWarningCheck(expr = 'exampleNull'/2,
                        maximize = FALSE),
      expected = Inf
    )
  }
)

test_that(
  "syntaxExtraction creates a valid list", {
    initialModelSyntaxFile <-"
f1 =~ Item1 + Item2 + Item3 + Item4 + Item5
f2 =~ Item6 + Item7 + Item8 + Item9 + Item10
f3 =~ Item11 + Item12 + Item13 + Item14 + Item15
f4 =~ Item16 + Item17 + Item18 + Item19 + Item20
"
    items <-
      paste0("Item", c(1:20))

    expectedOutput <-
      list(
        'factors' = paste0("f", c(1:4)),
        'itemsPerFactor' =
          list(
            paste0("Item", c(1:5)),
            paste0("Item", c(6:10)),
            paste0("Item", c(11:15)),
            paste0("Item", c(16:20))
          )
        )

    expect_equal(
      object =
        syntaxExtraction(
          initialModelSyntaxFile = initialModelSyntaxFile,
          items = items
          ),
      expected =
        expectedOutput
    )

    }
  )

test_that(
  "modelWarningCheck identifies any warning/error in a model while returning model object", {
    defaultModel <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
    wrongItemModel <- ' visual  =~ y1 + y2 + y3
              textual =~ y4 + y5 + y6
              speed   =~ y7 + y8 + y9 '
    poorModel <- ' visual  =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9'

    # model fits without error
    goodModel <-
      modelWarningCheck(
        expr = lavaan::lavaan(defaultModel,
                              data = lavaan::HolzingerSwineford1939,
                              auto.var=TRUE,
                              auto.fix.first=TRUE,
                              auto.cov.lv.x=TRUE),
        modelSyntax = defaultModel
      )

    expect_length(goodModel$warnings, 1)
    expect_length(goodModel$errors, 1)
    expect_s4_class(goodModel$model.output, 'lavaan')
    # model doesn't fit and throws an error
    noModel <-
      modelWarningCheck(
        expr = lavaan::lavaan(wrongItemModel,
                              data = lavaan::HolzingerSwineford1939,
                              auto.var=TRUE,
                              auto.fix.first=TRUE,
                              auto.cov.lv.x=F),
        modelSyntax = wrongItemModel
      )

    expect_length(noModel$warnings, 1)
    expect_length(noModel$errors, 2)
    expect_null(noModel$model.output)

    # model fits but throws a warning
    warningModel <-
      modelWarningCheck(
        expr = lavaan::lavaan(poorModel,
                              data = lavaan::HolzingerSwineford1939,
                              auto.var=TRUE,
                              auto.fix.first=TRUE,
                              auto.cov.lv.x=F,
                              ordered = T),
        modelSyntax = poorModel
      )

    expect_length(warningModel$warnings, 2)
    expect_length(warningModel$errors, 1)
    expect_s4_class(warningModel$model.output, 'lavaan')

  }
)

test_that(
  "checkModels returns the appropriate bestModel depending on the conditions", {
    # example models
    bestHolzinger <-
      list('model.output' =
             lavaan::lavaan(model =
             ' visual  =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9 ',
             data = lavaan::HolzingerSwineford1939,
             auto.var=TRUE,
             auto.fix.first=TRUE,
             auto.cov.lv.x=TRUE)
           )
    badHolzinger <-
      list('model.output' =
             lavaan::lavaan(model =
               ' visual  =~ x1 + x2 + x3 + x7
              textual =~ x4 + x5 + x6 + x8 + x9',
             data = lavaan::HolzingerSwineford1939,
             auto.var=TRUE,
             auto.fix.first=TRUE,
             auto.cov.lv.x=TRUE)
      )

    # models are same, return one of them
    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        fitStatistic = 'cfi',
        maximize = TRUE,
        bestFit = fitmeasures(bestHolzinger$model.output, 'cfi'),
        bestModel = bestHolzinger
        ),
      bestHolzinger
    )

    # comparing best model to (current) bad model, return best
    expect_equal(
      checkModels(
        currentModel = badHolzinger,
        fitStatistic = 'cfi',
        maximize = TRUE,
        bestFit = fitmeasures(bestHolzinger$model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )

    # comparing (current bad) best model to (new) best model, return best
    expect_equal(
      checkModels(
        currentModel = bestHolzinger,
        fitStatistic = 'cfi',
        maximize = TRUE,
        bestFit = fitmeasures(badHolzinger$model.output, 'cfi'),
        bestModel = badHolzinger
      ),
      bestHolzinger
    )

    # expect the bestModel if the currentModel is improperly specified
    expect_equal(
      checkModels(
        currentModel = "bestHolzinger",
        fitStatistic = 'cfi',
        maximize = TRUE,
        bestFit = fitmeasures(bestHolzinger$model.output, 'cfi'),
        bestModel = bestHolzinger
      ),
      bestHolzinger
    )
  }
)