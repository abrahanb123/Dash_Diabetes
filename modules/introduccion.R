# =============================================================================
#  modules/introduccion.R  ·  Pestaña 1: Introducción
#  -----------------------------------------------------------------------------
#  Presenta el tema del proyecto: qué es la diabetes, por qué analizarla, qué
#  es un indicador de salud, cómo ayudan los datos y qué busca el dashboard.
#  El server muestra el tamaño real del dataset y maneja el botón "Comenzar".
# =============================================================================

introduccion_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # ---- Portada ---------------------------------------------------------------
    div(
      class = "hero",
      div(
        class = "hero-texto",
        span(class = "hero-etiqueta", icon("graduation-cap"), "Proyecto de Ciencia de Datos"),
        h1(class = "hero-titulo", "Diabetes e indicadores de salud"),
        p(
          class = "hero-subtitulo",
          "Exploramos los factores asociados con la diabetes y clasificamos el estado de cada ",
          "persona (no diabetes, prediabetes o diabetes) con una regresión logística multinomial ",
          "entrenada con la encuesta BRFSS 2015."
        ),
        uiOutput(ns("datos_rapidos")),
        actionButton(
          ns("comenzar"), "Comenzar recorrido",
          icon = icon("arrow-right"), class = "btn-primary btn-lg"
        )
      ),
      div(
        class = "hero-ilustracion", `aria-hidden` = "true",
        div(class = "burbuja burbuja-1", icon("droplet")),
        div(class = "burbuja burbuja-2", icon("heart-pulse")),
        div(class = "burbuja burbuja-3", icon("chart-column"))
      )
    ),

    # ---- Conceptos clave -------------------------------------------------------
    h2(class = "titulo-seccion", "Conceptos clave"),
    layout_column_wrap(
      width = "250px", fill = FALSE, heights_equal = "row",
      tarjeta_info(
        "¿Qué es la diabetes?", icono = "droplet", color = "rosa",
        p("Es una enfermedad crónica en la que el cuerpo no produce suficiente insulina o no la usa bien.
          Por eso la glucosa (azúcar) se acumula en la sangre.")
      ),
      tarjeta_info(
        "¿Por qué analizarla?", icono = "heart-pulse", color = "lavanda",
        p("Es frecuente, puede avanzar años sin síntomas y se asocia con enfermedad cardiovascular,
          daño renal y pérdida de visión. Conocer sus factores asociados orienta la prevención.")
      ),
      tarjeta_info(
        "¿Qué es un indicador de salud?", icono = "stethoscope", color = "azul",
        p("Es una medida que resume un aspecto de la salud: por ejemplo, el índice de masa corporal,
          tener la presión alta o el número de días con mala salud física.")
      ),
      tarjeta_info(
        "¿Cómo ayudan los datos?", icono = "chart-line", color = "verde",
        p("Con miles de registros se pueden comparar grupos, encontrar patrones y entrenar modelos
          que estiman el estado de diabetes a partir de indicadores sencillos de obtener.")
      ),
      tarjeta_info(
        "¿Qué busca este proyecto?", icono = "bullseye", color = "amarillo",
        p("Explorar los indicadores del dataset, identificar factores asociados con la diabetes,
          entrenar y evaluar un modelo multinomial y ofrecer una herramienta de predicción educativa.")
      )
    ),

    # ---- Organización del dashboard ------------------------------------------------
    h2(class = "titulo-seccion", "¿Cómo está organizado el dashboard?"),
    layout_column_wrap(
      width = "280px", fill = FALSE, heights_equal = "row",
      tarjeta_info(
        "1 · Comprender el problema", icono = "earth-americas", color = "lavanda",
        div(class = "lista-chips", span(class = "chip", "Contexto"), span(class = "chip", "Problema"),
            span(class = "chip", "Objetivos"))
      ),
      tarjeta_info(
        "2 · Fundamentos y método", icono = "book-open", color = "azul",
        div(class = "lista-chips", span(class = "chip", "Marco teórico"), span(class = "chip", "Metodología"))
      ),
      tarjeta_info(
        "3 · Resultados y uso", icono = "chart-column", color = "verde",
        div(class = "lista-chips", span(class = "chip", "Resultados"), span(class = "chip", "Predicción"),
            span(class = "chip", "Limitaciones"), span(class = "chip", "Conclusiones"))
      )
    )
  )
}

introduccion_server <- function(id, datos, ir_a) {
  moduleServer(id, function(input, output, session) {

    # Cifras del dataset calculadas directamente del CSV
    output$datos_rapidos <- renderUI({
      div(
        class = "hero-datos",
        span(class = "dato", icon("users"), strong(formatear_numero(nrow(datos))), "respuestas"),
        span(class = "dato", icon("table-columns"), strong(contar_columnas_csv(datos)), "variables"),
        span(class = "dato", icon("layer-group"), strong(length(CLASES_DIABETES)), "estados de diabetes")
      )
    })

    # Botón "Comenzar recorrido": lleva a la pestaña Contexto
    observeEvent(input$comenzar, ir_a("contexto"))
  })
}
