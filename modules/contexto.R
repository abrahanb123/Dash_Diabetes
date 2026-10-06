# =============================================================================
#  modules/contexto.R  ·  Pestaña 2: Contexto
#  -----------------------------------------------------------------------------
#  Explica por qué la diabetes es un problema de salud pública, describe la
#  fuente de los datos y muestra indicadores generales del dataset.
#  Todas las cifras se calculan en el server a partir del CSV.
# =============================================================================

contexto_ui <- function(id) {
  ns <- NS(id)

  tagList(
    encabezado_pagina(
      "Contexto",
      "La diabetes como problema de salud pública y la fuente de los datos analizados.",
      icono = "earth-americas", etiqueta = "Sección 2"
    ),

    # Indicadores generales del dataset (calculados)
    uiOutput(ns("kpis")),

    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(icon("hospital"), "La diabetes, un reto de salud pública"),
        card_body(
          tags$ul(
            class = "lista-icono",
            tags$li(icon("chart-line"), span(
              strong("Enfermedad crónica y frecuente. "),
              "La Organización Mundial de la Salud (OMS) reporta que el número de personas con diabetes ha aumentado de forma sostenida en las últimas décadas."
            )),
            tags$li(icon("weight-scale"), span(
              strong("Relacionada con factores modificables. "),
              "La diabetes tipo 2, la más común, se asocia con exceso de peso, inactividad física, presión arterial alta y colesterol alto."
            )),
            tags$li(icon("eye-slash"), span(
              strong("Silenciosa. "),
              "Muchas personas no saben que la tienen o están en prediabetes, etapa en la que todavía es posible prevenirla o retrasarla."
            )),
            tags$li(icon("coins"), span(
              strong("Costosa. "),
              "Sus complicaciones (cardiovasculares, renales, visuales y amputaciones) generan una carga alta para las personas y los sistemas de salud."
            ))
          )
        )
      ),
      card(
        card_header(icon("database"), "¿De dónde vienen los datos?"),
        card_body(
          p(
            "El ", strong("Behavioral Risk Factor Surveillance System (BRFSS)"), " es una encuesta telefónica
            que el Centro para el Control y la Prevención de Enfermedades (CDC) de Estados Unidos realiza cada
            año sobre conductas de riesgo, enfermedades crónicas y uso de servicios preventivos."
          ),
          p(frase(
            "En Kaggle, Alex Teboul publicó una versión limpia de las respuestas de 2015 con indicadores de
            salud y la variable ", code("Diabetes_012"), "."
          )),
          uiOutput(ns("resumen_fuente")),
          a(href = URL_KAGGLE, target = "_blank", rel = "noopener", class = "enlace-externo",
            icon("arrow-up-right-from-square"), "Ver el dataset en Kaggle")
        )
      )
    ),

    h2(class = "titulo-seccion", "Factores del dataset relacionados con la diabetes"),
    p(class = "texto-seccion", "Porcentajes calculados sobre todos los registros del archivo."),
    uiOutput(ns("factores"))
  )
}

contexto_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {

    distribucion <- distribucion_clases(datos)
    pct_clase <- stats::setNames(distribucion$pct, distribucion$clase)
    n_clase <- stats::setNames(distribucion$n, distribucion$clase)

    # ---- Tarjetas KPI ----------------------------------------------------------
    output$kpis <- renderUI({
      layout_column_wrap(
        width = "200px", fill = FALSE, class = "fila-kpi",
        tarjeta_kpi("Registros", formatear_numero(nrow(datos)), "users", "lavanda",
                    detalle = "respuestas de la encuesta"),
        tarjeta_kpi("Variables", contar_columnas_csv(datos), "table-columns", "verde",
                    detalle = paste(contar_columnas_csv(datos) - 1, "indicadores + Diabetes_012")),
        tarjeta_kpi("Sin diabetes", formatear_porcentaje(pct_clase[["No diabetes"]]), "circle-check", "azul",
                    detalle = paste(formatear_numero(n_clase[["No diabetes"]]), "personas")),
        tarjeta_kpi("Prediabetes", formatear_porcentaje(pct_clase[["Prediabetes"]]), "triangle-exclamation", "amarillo",
                    detalle = paste(formatear_numero(n_clase[["Prediabetes"]]), "personas")),
        tarjeta_kpi("Diabetes", formatear_porcentaje(pct_clase[["Diabetes"]]), "droplet", "rosa",
                    detalle = paste(formatear_numero(n_clase[["Diabetes"]]), "personas"))
      )
    })

    # ---- Resumen de la fuente -------------------------------------------------------
    output$resumen_fuente <- renderUI({
      div(
        class = "resumen-fuente",
        div(span(class = "resumen-numero", formatear_numero(nrow(datos))), span("registros")),
        div(span(class = "resumen-numero", contar_columnas_csv(datos)), span("columnas")),
        div(span(class = "resumen-numero", formatear_numero(sum(is.na(datos)))), span("valores faltantes"))
      )
    })

    # ---- Factores de riesgo presentes en el dataset -----------------------------------
    output$factores <- renderUI({
      tarjeta_factor <- function(variable, valor, texto, icono, color) {
        div(
          class = paste("tarjeta-factor", paste0("tono-", color)),
          div(class = "tarjeta-factor-encabezado",
              div(class = "kpi-icono", icon(icono)),
              div(div(class = "tarjeta-factor-nombre", etiqueta_variable(variable)),
                  code(variable))),
          div(class = "tarjeta-factor-valor", valor),
          p(class = "tarjeta-factor-texto", texto)
        )
      }

      layout_column_wrap(
        width = "240px", fill = FALSE, heights_equal = "row",
        tarjeta_factor("HighBP", formatear_porcentaje(mean(datos$HighBP == 1)),
                       "reporta presión arterial alta. La hipertensión y la diabetes suelen presentarse juntas.",
                       "heart-pulse", "rosa"),
        tarjeta_factor("HighChol", formatear_porcentaje(mean(datos$HighChol == 1)),
                       "reporta colesterol alto, otro componente del riesgo cardiometabólico.",
                       "vial", "amarillo"),
        tarjeta_factor("BMI", formatear_porcentaje(mean(datos$BMI >= 30)),
                       paste0("tiene IMC de 30 o más (obesidad). La mediana del IMC es ",
                              formatear_numero(stats::median(datos$BMI)), " kg/m²."),
                       "weight-scale", "lavanda"),
        tarjeta_factor("Smoker", formatear_porcentaje(mean(datos$Smoker == 1)),
                       "ha fumado al menos 100 cigarrillos en su vida.",
                       "smoking", "azul"),
        tarjeta_factor("PhysActivity", formatear_porcentaje(mean(datos$PhysActivity == 1)),
                       "hizo actividad física en los últimos 30 días; la inactividad es un factor de riesgo.",
                       "person-running", "verde"),
        tarjeta_factor("GenHlth", formatear_porcentaje(mean(datos$GenHlth >= 4)),
                       "califica su salud general como regular o mala.",
                       "stethoscope", "lavanda"),
        tarjeta_factor("Age", formatear_porcentaje(mean(datos$Age >= 10)),
                       "tiene 65 años o más; el riesgo de diabetes tipo 2 aumenta con la edad.",
                       "calendar-days", "azul"),
        tarjeta_factor("DiffWalk", formatear_porcentaje(mean(datos$DiffWalk == 1)),
                       "tiene dificultad seria para caminar o subir escaleras.",
                       "person-walking", "amarillo")
      )
    })
  })
}
