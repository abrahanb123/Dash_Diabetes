# =============================================================================
#  modules/objetivos.R  ·  Pestaña 4: Objetivos
#  -----------------------------------------------------------------------------
#  Muestra el objetivo general y los ocho objetivos específicos en tarjetas.
#  Cada tarjeta tiene un enlace que lleva a la pestaña donde se cumple el
#  objetivo (el server usa la función ir_a() que recibe desde app.R).
# =============================================================================

# Lista de objetivos específicos. "pestana" y "subpestana" indican a dónde
# lleva el enlace de cada tarjeta.
OBJETIVOS_ESPECIFICOS <- list(
  list(titulo = "Explorar el dataset", icono = "magnifying-glass", destino = "Contexto", pestana = "contexto",
       texto = "Revisar la estructura del CSV, los tipos de variables, los valores faltantes y la distribución de Diabetes_012."),
  list(titulo = "Identificar patrones", icono = "chart-column", destino = "Problema", pestana = "problema",
       texto = "Describir cómo se distribuye la diabetes y cómo cambia con la presión arterial, el colesterol y el IMC."),
  list(titulo = "Analizar factores de riesgo", icono = "heart-pulse", destino = "Resultados", pestana = "resultados",
       subpestana = "factores",
       texto = "Comparar el estado de diabetes según hábitos, salud general, edad y correlaciones entre variables."),
  list(titulo = "Preparar los datos", icono = "broom", destino = "Metodología", pestana = "metodologia",
       texto = "Codificar la variable objetivo, seleccionar las predictoras y dividir los datos en entrenamiento y prueba."),
  list(titulo = "Entrenar el modelo", icono = "gears", destino = "Metodología", pestana = "metodologia",
       texto = "Ajustar una regresión logística multinomial con pesos de clase para compensar el desbalance."),
  list(titulo = "Evaluar el modelo", icono = "gauge-high", destino = "Resultados", pestana = "resultados",
       subpestana = "desempeno",
       texto = "Medir accuracy, precision, recall, F1-score, matriz de confusión y ROC-AUC multiclase."),
  list(titulo = "Crear la herramienta de predicción", icono = "calculator", destino = "Predicción", pestana = "prediccion",
       texto = "Construir un formulario que estime la probabilidad de cada estado de diabetes para un perfil dado."),
  list(titulo = "Interpretar los resultados", icono = "lightbulb", destino = "Conclusiones", pestana = "conclusiones",
       texto = "Traducir los hallazgos en conclusiones útiles, distinguiendo entre asociación y causalidad.")
)

objetivos_ui <- function(id) {
  ns <- NS(id)
  colores <- c("azul", "verde", "lavanda", "rosa", "amarillo")

  tarjetas <- lapply(seq_along(OBJETIVOS_ESPECIFICOS), function(i) {
    objetivo <- OBJETIVOS_ESPECIFICOS[[i]]
    div(
      class = paste("objetivo-card", paste0("tono-", colores[(i - 1) %% length(colores) + 1])),
      div(
        class = "objetivo-encabezado",
        span(class = "objetivo-numero", i),
        div(class = "kpi-icono", icon(objetivo$icono))
      ),
      h4(class = "objetivo-titulo", objetivo$titulo),
      p(class = "objetivo-texto", objetivo$texto),
      actionLink(ns(paste0("ir_", i)), paste("Ver en", objetivo$destino), icon = icon("arrow-right"),
                 class = "objetivo-enlace")
    )
  })

  tagList(
    encabezado_pagina(
      "Objetivos",
      "Qué se propone lograr el proyecto y en qué parte del dashboard se cumple cada objetivo.",
      icono = "bullseye", etiqueta = "Sección 4"
    ),

    div(
      class = "objetivo-general",
      div(class = "objetivo-general-icono", icon("flag")),
      div(
        span(class = "objetivo-general-etiqueta", "Objetivo general"),
        p(
          class = "objetivo-general-texto",
          "Desarrollar un dashboard interactivo en R Shiny que permita explorar indicadores de salud y
          utilizar un modelo de regresión logística multinomial para clasificar el estado de diabetes
          de una persona."
        )
      )
    ),

    h2(class = "titulo-seccion", "Objetivos específicos"),
    # "!!!" despliega la lista: cada tarjeta pasa a ser una celda de la cuadrícula.
    layout_column_wrap(width = "250px", fill = FALSE, heights_equal = "row", !!!tarjetas)
  )
}

objetivos_server <- function(id, ir_a) {
  moduleServer(id, function(input, output, session) {
    # Un observador por tarjeta: al hacer clic en "Ver en ..." cambia de pestaña.
    lapply(seq_along(OBJETIVOS_ESPECIFICOS), function(i) {
      objetivo <- OBJETIVOS_ESPECIFICOS[[i]]
      observeEvent(input[[paste0("ir_", i)]], {
        ir_a(objetivo$pestana, objetivo$subpestana)
      })
    })
  })
}
