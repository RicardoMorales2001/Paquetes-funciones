#' Title: Función de regresión lineal múltiple
#'
#' @param data Data to be used to fit the multiple linear regression model.
#' @param dependent_var La variable dependiente que se desea predecir mediante el modelo.
#' @param independent_vars Vector with the independent variables that will be used to fit the multiple linear regression model.
#'
#' @return Summary of the fitted model and a plot of fitted values
#' @export
#' @import tidyverse
#' @import testthat
#' @import dplyr
#'
#' @examples
#' df <- data.frame(x1 = c(1,2,3,4,5),
#'                 x2 = c(3,4,5,6,7),
#'                 y = c(6,9,12,15,18))
#' multi_reg(df, "y", c("x1", "x2"))

multi_reg <- function(data, dependent_var, independent_vars) {

  if (!is.data.frame(data)) {
    stop("El argumento data debe ser un dataframe.")
  }

  if (!(dependent_var %in% colnames(data))) {
    stop(paste0("La variable dependiente '", dependent_var, "' no existe en el dataframe."))
  }

  if (!all(independent_vars %in% colnames(data))) {
    stop("Alguna de las variables independientes no existe en el dataframe.")
  }

  formula <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+")))
  model <- lm(formula, data = data)
  print(summary(model))
  plot(model$fitted.values, data[, dependent_var], main = "Valores ajustados vs. valores reales")
  return(model)
}
