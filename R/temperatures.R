#' A series of internal temperature functions.
#' Directly accessing these functions is not advised unless you know what you are doing.
#'
#' @keywords internal
#'

linearTemperature <- function(currentStep, maxSteps) {
  currentTemp <- (maxSteps - (currentStep)) / maxSteps
}

quadraticTemperature <- function(currentStep, maxSteps) {
  currentTemp <- ((maxSteps - (currentStep)) / maxSteps)^2
}

logisticTemperature <- function(currentStep, maxSteps) {
  x <- 1:maxSteps
  x.new <- scale(x, center = T, scale = maxSteps / 12)
  currentTemp <- 1 / (1 + exp((x.new[(currentStep + 1)])))
}

temperature <- function(currentStep, maxSteps) {
  currentTemp <- ((maxSteps - (currentStep)) / maxSteps)^7
}