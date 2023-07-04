#' Modelo Probit
#'
#' Analisis de regresion con variables de respuesta binaria.
#'
#' @param formula (vector) un vector que contiene las variables prdictoras en relacion de la variable respuesta
#' @param data (data.frame) un data frame que contiene los datos con los que se trabajará.
#'
#' @returnuna una lista con los coeficientes de correlacion y las predicciones de las variables
#' @export
#'
#' @examples
#' \dontrun{
#' # Cargar los datos
#' datos <- read.csv("/Users/marif/Downloads/dataset1.csv")
#'
#' # Eliminar filas con valores faltantes
#' datos <- na.omit(datos)
#' # Variables
#' y <- datos$presencia
#' x <- datos[, c("rough", "julmint", "pptcv", "evi")]
#'
#' # Ajustar modelo Probit
#' model <- probit_model(y ~ rough + julmint + pptcv + evi, data = data.frame(y, x))
#' # Obtener predicciones
#' predictions <- model$predict(data.frame(x))
#' # Imprimir coeficientes estimados y predicciones
#' print(model$coefficients)
#' print(predictions)
#' }
probit_model <- function(formula, data) {
  # Ajustar modelo Probit con variables numéricas continuas
  model <- glm(formula, data = data, family = binomial(link = "probit"))

  # Crear objeto de modelo
  model_obj <- list()
  model_obj$coefficients <- coef(model)
  model_obj$call <- match.call()

  # Definir función de predicción
  model_obj$predict <- function(newdata) {
    predict(model, newdata = newdata, type = "response")
  }

  return(model_obj)
}
