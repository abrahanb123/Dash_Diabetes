# =============================================================================
#  modules/limitaciones.R  ·  Pestaña 9: Limitaciones
#  -----------------------------------------------------------------------------
#  Presenta, en tarjetas, las limitaciones de los datos, del diseño y del
#  modelo. Las cifras que aparecen en el texto (desbalance, duplicados,
#  valores extremos, recall) se calculan en el server.
# =============================================================================

limitaciones_ui <- function(id) {
  ns <- NS(id)

  tagList(
    encabezado_pagina(
      "Limitaciones",
      "Aspectos que se deben tener en cuenta al interpretar los resultados y las predicciones.",
      icono = "triangle-exclamation", etiqueta = "Sección 9"
    ),

    layout_column_wrap(
      width = "300px", fill = FALSE, heights_equal = "row",
      tarjeta_info(
        "Datos de encuesta", icono = "clipboard-list", color = "azul",
        p("Los datos provienen de una encuesta telefónica (BRFSS). Las respuestas son autorreportadas: la persona
          puede no recordar con precisión o responder lo que considera socialmente aceptable.")
      ),
      tarjeta_info(
        "Posibles sesgos", icono = "scale-unbalanced-flip", color = "rosa",
        p("Puede haber sesgo de no respuesta (quien no contesta puede ser distinto de quien sí lo hace), de
          cobertura (personas sin teléfono) y de memoria. Además, la versión de Kaggle excluyó respuestas incompletas.")
      ),
      tarjeta_info(
        "Representatividad", icono = "people-group", color = "lavanda",
        p("Corresponde a adultos de Estados Unidos en 2015 y no usa los pesos muestrales de la encuesta, por lo
          que no representa necesariamente a toda la población. Extrapolar a otros países, como Colombia,
          requiere cautela.")
      ),
      tarjeta_info(
        "Indicadores, no diagnósticos clínicos", icono = "stethoscope", color = "verde",
        p("HighBP, HighChol o Diabetes_012 indican lo que a la persona le dijo un profesional; no son mediciones
          clínicas de presión, colesterol o glucosa. La clase «No diabetes» incluye la diabetes que solo se
          presentó durante el embarazo.")
      ),
      tarjeta_info(
        "Correlación no implica causalidad", icono = "link-slash", color = "amarillo",
        p("El diseño es transversal: todo se mide en el mismo momento. Se pueden describir asociaciones, pero no
          afirmar que un factor cause la diabetes ni saber qué ocurrió primero.")
      ),
      tarjeta_info(
        "Dependencia de las variables disponibles", icono = "puzzle-piece", color = "azul",
        p("El modelo solo conoce ", length(VARIABLES_MODELO), " indicadores. Faltan factores relevantes como
          antecedentes familiares, dieta detallada, medicamentos, perímetro abdominal o glucosa en sangre.")
      ),
      tarjeta_info(
        "Supuestos de la regresión logística multinomial", icono = "square-root-variable", color = "rosa",
        p("Supone efectos lineales en el log-odds, observaciones independientes e independencia de alternativas
          irrelevantes (IIA). Tratar las variables ordinales como puntajes asume saltos iguales entre categorías,
          y el modelo no aprovecha que las clases tienen un orden natural.")
      ),
      tarjeta_info(
        "Desbalance de clases", icono = "scale-unbalanced", color = "lavanda",
        p(frase(
          "La prediabetes es solo el ", strong(textOutput(ns("pct_prediabetes"), inline = TRUE)),
          " de los registros. Aun con pesos de clase, el modelo detecta el ",
          strong(textOutput(ns("recall_prediabetes"), inline = TRUE)),
          " de esos casos en el conjunto de prueba, y los pesos hacen que las probabilidades no reflejen la
          prevalencia real."
        ))
      ),
      tarjeta_info(
        "Calidad de los datos", icono = "broom", color = "verde",
        p(frase(
          "Hay ", strong(textOutput(ns("n_duplicados"), inline = TRUE)), " filas duplicadas (",
          textOutput(ns("pct_duplicados"), inline = TRUE), "), que pueden quedar a la vez en entrenamiento y
          prueba. También hay ", strong(textOutput(ns("n_imc_extremo"), inline = TRUE)),
          " registros con IMC de 60 o más: valores posibles pero poco frecuentes, que podrían ser errores de
          digitación."
        ))
      ),
      tarjeta_info(
        "No es un diagnóstico médico", icono = "user-doctor", color = "amarillo",
        p("Las predicciones son estimaciones estadísticas con fines educativos. No sustituyen la valoración de un
          profesional de la salud ni una prueba de glucosa.")
      )
    )
  )
}

limitaciones_server <- function(id, datos, modelo_info) {
  moduleServer(id, function(input, output, session) {

    columnas_csv <- setdiff(names(datos), COLUMNA_ESTADO)
    n_duplicados <- sum(duplicated(datos[columnas_csv]))
    recall_prediabetes <- with(modelo_info$metricas$por_clase, recall[clase == "Prediabetes"])

    output$pct_prediabetes <- renderText({
      formatear_porcentaje(mean(datos[[COLUMNA_ESTADO]] == "Prediabetes"))
    })
    output$recall_prediabetes <- renderText(formatear_porcentaje(recall_prediabetes))
    output$n_duplicados <- renderText(formatear_numero(n_duplicados))
    output$pct_duplicados <- renderText(formatear_porcentaje(n_duplicados / nrow(datos)))
    output$n_imc_extremo <- renderText(formatear_numero(sum(datos$BMI >= 60)))
  })
}
