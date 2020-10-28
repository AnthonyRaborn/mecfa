library(mecfa);library(lavaan)
maynesDat <-
  read.csv(file = "C:/Users/araborn/Desktop/Personal Projects/Metaheuristic ECFA/MaynesData1.csv")
colnames(maynesDat) <-
  paste0("Item", 1:ncol(maynesDat))
initialData <-
  maynesDat
initialData$Outcome <-
  rowSums(initialData)+rnorm(nrow(initialData))

realModel <-"
f1 =~ Item1 + Item2 + Item3 + Item4 + Item5
f2 =~ Item6 + Item7 + Item8 + Item9 + Item10
f3 =~ Item11 + Item12 + Item13 + Item14 + Item15
f4 =~ Item16 + Item17 + Item18 + Item19 + Item20
"
startModel4 <-"
f1 =~ Item1 + Item7 + Item3 + Item17 + Item19
f2 =~ Item5 + Item2 + Item13 + Item14 + Item10
f3 =~ Item11 + Item16 + Item8 + Item9 + Item18
f4 =~ Item15 + Item12 + Item4 + Item6 + Item20"
startModel4a <-"
f1 =~ Item1 + Item7 + Item3 + Item17 + Item19 + Item6 + Item20
f2 =~ Item5 + Item2 + Item13 + Item14 + Item10 + Item9 + Item18
f3 =~ Item11 + Item16 + Item8
f4 =~ Item15 + Item12 + Item4"
startModel4b <-"
f1 =~ Item1 + Item7 + Item3 + Item17 + Item19 + Item14 + Item10
f2 =~ Item5 + Item2 + Item13
f3 =~ Item11 + Item16 + Item8 + Item9
f4 =~ Item15 + Item12 + Item4 + Item6 + Item20 + Item18"
# Outcome ~ f1 + f2 + f3 + f4"
startModel4c <-"
f1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10 + Item11 + Item12 + Item13 + Item14 + Item15 + 0*Item16 + Item17 + Item18 + Item19 + Item20
f2 =~ 0*Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10 + Item11 + Item12 + Item13 + Item14 + Item15 + Item16 + Item17 + Item18 + Item19 + Item20
f3 =~ Item1 + Item2 + Item3 + Item4 + Item5 + 0*Item6 + Item7 + Item8 + Item9 + Item10 + Item11 + Item12 + Item13 + Item14 + Item15 + Item16 + Item17 + Item18 + Item19 + Item20
f4 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10 + 0*Item11 + Item12 + Item13 + Item14 + Item15 + Item16 + Item17 + Item18 + Item19 + Item20
"
startModel2 <-"
f1 =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 + Item9 + Item10
f2 =~ Item11 + Item12 + Item13 + Item14 + Item15 + Item16 + Item16 + Item17 + Item19 + Item20"
# Outcome ~ f1 + f2"
startModel3 <-"
f1 =~ Item1 + Item2 + Item3 + Item4 + Item6 + Item5 + Item7
f2 =~ Item8 + Item9 + Item10 + Item11 + Item12 + Item13 + Item14
f3 =~ Item15 + Item16 + Item17 + Item18 + Item19 + Item20"
# Outcome ~ f1 + f2 + f3"
startModel5 <-"
f1 =~ Item1 + Item2 + Item3 + Item4
f2 =~ Item6 + Item5 + Item7 + Item8
f3 =~ Item9 + Item10 + Item11 + Item12
f4 =~ Item13 + Item14 + Item15 + Item16
f5 =~ Item17 + Item18 + Item19 + Item20"
# Outcome ~ f1 + f2 + f3 + f4 + f5"
startModel5a <-"
f1 =~ Item1 + Item2 + Item3
f2 =~ Item4 + Item5 + Item6 + Item7 + Item8
f3 =~ Item9 + Item10 + Item11
f4 =~ Item12 + Item13 + Item14 + Item15
f5 =~ Item16 + Item17 + Item18 + Item19 + Item20"
# Outcome ~ f1 + f2 + f3 + f4 + f5"
startModel5b <-"
f1 =~ Item1 + Item2 + Item3
f2 =~ Item4 + Item5 + Item6 + Item7 + Item8
f3 =~ Item9 + Item10 + Item11
f4 =~ Item12 + Item13 + Item14 + Item15
f5 =~ Item16 + Item17 + Item18 + Item19 + Item20"

initialModels <-
  c(#realModel,
    # startModel2,
    startModel3,
    # startModel4,
    startModel4a,
    # startModel4c,
    # startModel5a,
    startModel5b)

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
testMax <-
  exploratorySA(initialModels = initialModels,
                originalData = initialData,
                maxSteps = 3000,
                fitStatistic = 'cfi',
                lavaan.model.specs = lavaanDesc)

testMin <-
  exploratorySA(initialModels = initialModels,
                originalData = initialData,
                maxSteps = 3000,
                fitStatistic = 'chisq',
                maximize = F,
                lavaan.model.specs = lavaanDesc)
testMax
testMin
print(modelTableToString(bestECFA(testMax)))
print(modelTableToString(bestECFA(testMin)))
summary(testMax)
summary(testMin)
