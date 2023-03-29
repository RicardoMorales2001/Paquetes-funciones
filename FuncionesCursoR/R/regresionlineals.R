#' Title: Función de regresión lineal simple
#'
#' @param x Numeric vector
#' @param y Numeric vector
#'
#' @return Simple linear regression fit
#' @export
#' @import ggplot2
#' @import dplyr
#' @import testthat
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(3, 5, 7, 9, 11)
#' reg_lineal(x, y)
#'
reg_lineal <- function(x, y) {

  if(length(x) != length(y)) {
    stop("Los vectores deben tener el mismo tamaño.")
  }

  modelo <- lm(y ~ x)
    plot(x, y, main = "Regresión lineal", xlab = "x", ylab = "y")
  abline(modelo, col = "red")
    return(modelo)
}
