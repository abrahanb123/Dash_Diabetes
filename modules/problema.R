# =============================================================================
#  modules/problema.R  ·  Pestaña 3: Problema
#  -----------------------------------------------------------------------------
#  Análisis descriptivo del problema:
#    * Distribución de Diabetes_012 (cantidad y porcentaje).
#    * Diabetes y presión arterial alta (HighBP).
#    * Diabetes y colesterol alto (HighChol).
#    * Diabetes e índice de masa corporal (BMI).
# =============================================================================

problema_ui <- function(id) {
  ns <- NS(id)

  tagList(
    encabezado_pagina(
      "Problema",
      "¿Cómo se distribuye el estado de diabetes y cómo cambia con la presión arterial, el colesterol y el IMC?",
      icono = "circle-question", etiqueta = "Sección 3"
    ),

    # ---- Planteamiento -------------------------------------------------------
    card(
      class = "tarjeta-destacada",
      card_body(
        layout_columns(
          col_widths = c(7, 5),
          div(
            h3(class = "titulo-tarjeta", "Planteamiento del problema"),
            p("La diabetes suele desarrollarse de forma silenciosa y muchas personas pasan por una etapa de
              prediabetes sin saberlo. Las encuestas de salud recogen indicadores sencillos (presión arterial,
              colesterol, peso, hábitos, edad) que podrían ayudar a identificar a quienes tienen mayor riesgo."),
            p("El reto es doble: entender qué indicadores se asocian con cada estado de diabetes y construir
              un modelo capaz de clasificar a una persona, teniendo en cuenta que las clases están muy desbalanceadas.")
          ),
          div(
            class = "pregunta-investigacion",
            span(class = "pregunta-etiqueta", icon("magnifying-glass"), "Pregunta de investigación"),
            p("¿Qué indicadores de salud, hábitos y características sociodemográficas se asocian con el estado
              de diabetes (no diabetes, prediabetes o diabetes) y qué tan bien puede una regresión logística
              multinomial clasificar a una persona a partir de ellos?")
          )
        )
      )
    ),

    # ---- Distribución de la variable objetivo ------------------------------------
    layout_columns(
      col_widths = c(7, 5),
      tarjeta_grafico(
        "Distribución de Diabetes_012",
        "Cantidad y porcentaje de registros en cada estado",
        plotlyOutput(ns("distribucion"), height = "340px")
      ),
      card(
        card_header(icon("lightbulb"), "Lo que muestran los datos"),
        card_body(uiOutput(ns("hallazgos")))
      )
    ),

    # ---- Presión arterial y colesterol --------------------------------------------
    layout_columns(
      col_widths = c(6, 6),
      tarjeta_grafico(
        "Diabetes y presión arterial (HighBP)",
        "% de prediabetes y de diabetes según si la persona reporta presión arterial alta",
        plotlyOutput(ns("presion"), height = "320px"),
        nota = uiOutput(ns("nota_presion"))
      ),
      tarjeta_grafico(
        "Diabetes y colesterol (HighChol)",
        "% de prediabetes y de diabetes según si la persona reporta colesterol alto",
        plotlyOutput(ns("colesterol"), height = "320px"),
        nota = uiOutput(ns("nota_colesterol"))
      )
    ),

    # ---- Índice de masa corporal ----------------------------------------------------
    layout_columns(
      col_widths = c(6, 6),
      tarjeta_grafico(
        "Diabetes e IMC (BMI)",
        "Distribución del IMC en cada estado: la caja contiene el 50 % central y el rombo marca la media",
        plotOutput(ns("imc_caja"), height = "320px"),
        nota = uiOutput(ns("nota_imc"))
      ),
      tarjeta_grafico(
        "Diabetes según categoría de IMC (OMS)",
        "% de prediabetes y de diabetes en cada categoría de peso",
        plotlyOutput(ns("imc_categorias"), height = "320px"),
        nota = "Categorías de la OMS: bajo peso < 18,5 · normal 18,5–24,9 · sobrepeso 25–29,9 · obesidad I 30–34,9 · obesidad II 35–39,9 · obesidad III ≥ 40 kg/m²."
      )
    )
  )
}

problema_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {

    distribucion <- distribucion_clases(datos)
    pct_clase <- stats::setNames(distribucion$pct, distribucion$clase)
    n_clase <- stats::setNames(distribucion$n, distribucion$clase)
    tasas_binarias <- comparar_tasa_diabetes(datos, c("HighBP", "HighChol"))

    # Texto que compara la tasa de diabetes con y sin la condición
    nota_comparacion <- function(variable, condicion) {
      fila <- tasas_binarias[tasas_binarias$variable == variable, ]
      frase(
        punto_clase("Diabetes"),
        "Diabetes con ", condicion, ": ", strong(formatear_porcentaje(fila$tasa_con)),
        " · sin ", condicion, ": ", strong(formatear_porcentaje(fila$tasa_sin)),
        " → ", strong(formatear_numero(fila$razon, 1), " veces"), " más frecuente."
      )
    }

    # ---- Distribución de Diabetes_012 -------------------------------------------
    output$distribucion <- renderPlotly({
      datos_grafico <- distribucion |>
        mutate(
          etiqueta = paste0(formatear_numero(n), " (", formatear_porcentaje(pct), ")"),
          tooltip = paste0("<b>", clase, "</b><br>", formatear_numero(n), " registros<br>",
                           formatear_porcentaje(pct), " del total")
        )

      g <- ggplot(datos_grafico, aes(x = clase, y = n, fill = clase, text = tooltip)) +
        geom_col(width = 0.38) +
        geom_text(aes(y = n + max(n) * 0.06, label = etiqueta), size = 3.6, color = COLOR_TEXTO) +
        scale_fill_manual(values = COLORES_CLASES) +
        scale_y_continuous(labels = etiqueta_eje_numero, expand = expansion(mult = c(0, 0.15))) +
        labs(x = NULL, y = "Número de registros") +
        tema_dashboard() +
        theme(legend.position = "none", panel.grid.major.x = element_blank())

      a_plotly(g, leyenda = FALSE)
    })

    # ---- Hallazgos principales (calculados) ----------------------------------------
    output$hallazgos <- renderUI({
      presion <- tasas_binarias[tasas_binarias$variable == "HighBP", ]
      hallazgo <- function(icono, ...) {
        div(class = "hallazgo", div(class = "hallazgo-icono", icon(icono)), div(frase(...)))
      }

      tagList(
        hallazgo("droplet", strong("1 de cada ", round(1 / pct_clase[["Diabetes"]])),
                 " personas del dataset reporta diabetes (", formatear_porcentaje(pct_clase[["Diabetes"]]), ")."),
        hallazgo("triangle-exclamation", "La prediabetes es la clase minoritaria: solo ",
                 strong(formatear_porcentaje(pct_clase[["Prediabetes"]])), " de los registros (",
                 formatear_numero(n_clase[["Prediabetes"]]), " personas)."),
        hallazgo("scale-unbalanced", "Por cada registro con prediabetes hay ",
                 strong(formatear_numero(n_clase[["No diabetes"]] / n_clase[["Prediabetes"]])),
                 " sin diabetes: el conjunto está ", strong("muy desbalanceado"), "."),
        hallazgo("heart-pulse", "Con presión arterial alta, la diabetes es ",
                 strong(formatear_numero(presion$razon, 1), " veces"), " más frecuente (",
                 formatear_porcentaje(presion$tasa_con), " frente a ", formatear_porcentaje(presion$tasa_sin), ").")
      )
    })

    # ---- Presión arterial y colesterol ------------------------------------------------
    output$presion <- renderPlotly({
      grafico_tasas(resumen_por_factor(datos, "HighBP"), "Presión arterial alta")
    })
    output$nota_presion <- renderUI(nota_comparacion("HighBP", "presión alta"))

    output$colesterol <- renderPlotly({
      grafico_tasas(resumen_por_factor(datos, "HighChol"), "Colesterol alto")
    })
    output$nota_colesterol <- renderUI(nota_comparacion("HighChol", "colesterol alto"))

    # ---- IMC: diagrama de caja ----------------------------------------------------------
    # Se deja estático (ggplot2) porque convertirlo a plotly enviaría los
    # 253.680 valores al navegador. Los valores extremos se recortan
    # visualmente al percentil 99 para que las cajas se vean bien.
    resumen_imc <- datos |>
      group_by(clase = .data[[COLUMNA_ESTADO]]) |>
      summarise(media = mean(BMI), mediana = stats::median(BMI), .groups = "drop")

    output$imc_caja <- renderPlot({
      etiquetas_x <- paste0(resumen_imc$clase, "\nmediana ", formatear_numero(resumen_imc$mediana),
                            " · media ", formatear_numero(resumen_imc$media, 1))
      limite_superior <- stats::quantile(datos$BMI, 0.99)

      ggplot(datos, aes(x = .data[[COLUMNA_ESTADO]], y = BMI, fill = .data[[COLUMNA_ESTADO]])) +
        geom_boxplot(width = 0.42, outlier.shape = NA, color = COLOR_TEXTO_SUAVE, linewidth = 0.45) +
        geom_point(data = resumen_imc, aes(x = clase, y = media), inherit.aes = FALSE,
                   shape = 23, size = 3.2, fill = "white", color = COLOR_TEXTO, stroke = 0.8) +
        scale_fill_manual(values = COLORES_CLASES) +
        scale_x_discrete(labels = etiquetas_x) +
        coord_cartesian(ylim = c(min(datos$BMI), limite_superior)) +
        labs(x = NULL, y = "IMC (kg/m²)") +
        tema_dashboard(tamano_base = 13) +
        theme(legend.position = "none", panel.grid.major.x = element_blank())
    }, res = 96)

    output$nota_imc <- renderUI({
      mediana <- stats::setNames(resumen_imc$mediana, resumen_imc$clase)
      frase(
        "La mediana del IMC pasa de ", strong(formatear_numero(mediana[["No diabetes"]])), " (no diabetes) a ",
        strong(formatear_numero(mediana[["Diabetes"]])), " (diabetes). Se ocultan visualmente los valores por
        encima del percentil 99 (", formatear_numero(stats::quantile(datos$BMI, 0.99)), " kg/m²)."
      )
    })

    # ---- IMC por categorías de la OMS -----------------------------------------------------
    output$imc_categorias <- renderPlotly({
      resumen <- resumen_tasas(categoria_imc(datos$BMI), datos[[COLUMNA_ESTADO]])
      grafico_tasas(resumen, "Categoría de IMC")
    })
  })
}
