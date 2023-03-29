#' Title: Función de correlación
#'
#' @param data Data frame that has the variables x_var and y_var.
#' @param x_var The name of the data frame data column to be used as the independent variable in the scatter plot.
#' @param y_var the name of the data frame data column to use as the dependent variable in the scatter plot.
#'
#' @return The value of the calculated correlation
#' @export
#' @import dplyr
#' @import ggplot2
#' @import testthat
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 6, 8, 10)
#' df <- data.frame(x, y)
#' my_corr_plot(df, "x", "y")
my_corr_plot <- function(data, x_var, y_var) {

  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un dataframe.")
  }
  if (!(x_var %in% colnames(data))) {
    stop(paste0("La columna '", x_var, "' no existe en el dataframe."))
  }
  if (!(y_var %in% colnames(data))) {
    stop(paste0("La columna '", y_var, "' no existe en el dataframe."))
  }

  corr <- cor(data[[x_var]], data[[y_var]])

  my_plot <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_point() +
    labs(title = paste0("Correlacion: ", round(corr, 2)), encoding = "UTF-8")
  print(my_plot)
  return(corr)
}
