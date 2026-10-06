# =============================================================================
#  modules/resultados.R  ·  Pestaña 7: Resultados
#  -----------------------------------------------------------------------------
#  Subpestañas:
#    EDA     · Distribución (dona) · Factores de riesgo (barras) ·
#              Variables numéricas (histogramas) · Edad · Correlación
#    Modelo  · Desempeño (KPIs, matriz de confusión, ROC multiclase, tablas) ·
#              Interpretación (odds ratios)
#  Las métricas del modelo se leen de model/model.rds: se calcularon en el
#  conjunto de prueba al ejecutar model/train_model.R.
# =============================================================================

# Variables que se muestran fijas en "Factores de riesgo".
FACTORES_PRINCIPALES <- c("HighBP", "HighChol", "PhysActivity", "GenHlth")

# Variables numéricas disponibles para el histograma (y ancho de cada barra).
VARIABLES_HISTOGRAMA <- c(
  "BMI · Índice de masa corporal" = "BMI",
  "MentHlth · Días de mala salud mental" = "MentHlth",
  "PhysHlth · Días de mala salud física" = "PhysHlth"
)
ANCHO_INTERVALO <- c(BMI = 2, MentHlth = 1, PhysHlth = 1)

# Envuelve un radioButtons para mostrarlo como control segmentado (ver CSS).
control_segmentado <- function(...) div(class = "control-segmentado", radioButtons(..., inline = TRUE))

resultados_ui <- function(id) {
  ns <- NS(id)

  # Otras variables categóricas del dataset para el gráfico "Explorar otra variable"
  categoricas <- DICCIONARIO_VARIABLES[
    DICCIONARIO_VARIABLES$tipo %in% c("Binaria", "Ordinal") &
      !DICCIONARIO_VARIABLES$variable %in% FACTORES_PRINCIPALES, ]
  opciones_extra <- stats::setNames(categoricas$variable, paste(categoricas$variable, "·", categoricas$etiqueta))

  tagList(
    encabezado_pagina(
      "Resultados",
      "Análisis exploratorio de datos (EDA) y desempeño de la regresión logística multinomial en el conjunto de prueba.",
      icono = "chart-column", etiqueta = "Sección 7"
    ),

    navset_pill(
      id = ns("subpestanas"),

      # ---- EDA: distribución -----------------------------------------------------
      nav_panel(
        "Distribución", value = "distribucion", icon = icon("chart-pie"),
        layout_columns(
          col_widths = c(7, 5),
          tarjeta_grafico(
            "Distribución de Diabetes_012",
            "Proporción de registros en cada estado de diabetes",
            plotlyOutput(ns("dona"), height = "380px")
          ),
          card(
            card_header(icon("table"), "Tabla de frecuencias"),
            card_body(
              tableOutput(ns("tabla_distribucion")),
              uiOutput(ns("nota_distribucion"))
            )
          )
        )
      ),

      # ---- EDA: factores de riesgo --------------------------------------------------
      nav_panel(
        "Factores de riesgo", value = "factores", icon = icon("heart-pulse"),
        p(class = "texto-seccion",
          "Cada barra suma 100 % y muestra cómo se reparte el estado de diabetes dentro de un grupo.
          El segmento de Diabetes arranca en cero para comparar su tamaño entre grupos."),
        layout_column_wrap(
          width = "320px", fill = FALSE,
          tarjeta_grafico("Presión arterial alta vs estado de diabetes", "HighBP",
                          plotlyOutput(ns("barras_HighBP"), height = "250px")),
          tarjeta_grafico("Colesterol alto vs estado de diabetes", "HighChol",
                          plotlyOutput(ns("barras_HighChol"), height = "250px")),
          tarjeta_grafico("Actividad física vs estado de diabetes", "PhysActivity",
                          plotlyOutput(ns("barras_PhysActivity"), height = "250px"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          tarjeta_grafico("Salud general vs estado de diabetes", "GenHlth (1 = excelente … 5 = mala)",
                          plotlyOutput(ns("barras_GenHlth"), height = "380px")),
          tarjeta_grafico(
            "Explorar otra variable", "Cualquier otra variable binaria u ordinal del dataset",
            plotlyOutput(ns("barras_extra"), height = "380px"),
            controles = selectInput(ns("variable_extra"), NULL, choices = opciones_extra,
                                    selected = "DiffWalk", width = "340px")
          )
        )
      ),

      # ---- EDA: variables numéricas -------------------------------------------------
      nav_panel(
        "Variables numéricas", value = "numericas", icon = icon("chart-area"),
        layout_columns(
          col_widths = c(3, 9),
          card(
            card_header(icon("sliders"), "Opciones"),
            card_body(
              selectInput(ns("variable_hist"), "Seleccionar variable:", choices = VARIABLES_HISTOGRAMA),
              checkboxInput(ns("hist_por_clase"), "Comparar por estado de diabetes", value = FALSE),
              uiOutput(ns("ayuda_hist"))
            )
          ),
          tarjeta_grafico(
            textOutput(ns("titulo_hist"), inline = TRUE),
            uiOutput(ns("subtitulo_hist"), inline = TRUE),
            plotlyOutput(ns("histograma"), height = "460px")
          )
        ),
        card(
          card_header(icon("table"), "Estadísticas descriptivas de la variable seleccionada"),
          card_body(tableOutput(ns("tabla_hist")))
        )
      ),

      # ---- EDA: edad ------------------------------------------------------------------
      nav_panel(
        "Edad", value = "edad", icon = icon("calendar-days"),
        tarjeta_grafico(
          "Estado de diabetes por grupo de edad",
          "Age: 13 grupos de 5 años",
          plotlyOutput(ns("grafico_edad"), height = "430px"),
          nota = uiOutput(ns("nota_edad")),
          controles = control_segmentado(
            ns("vista_edad"), NULL,
            choices = c("Tasas por grupo" = "tasas", "Composición 100 %" = "composicion")
          )
        )
      ),

      # ---- EDA: correlación -----------------------------------------------------------
      nav_panel(
        "Correlación", value = "correlacion", icon = icon("table-cells"),
        div(
          class = "barra-controles",
          control_segmentado(
            ns("metodo_cor"), "Método de correlación:",
            choices = c("Pearson (lineal)" = "pearson", "Spearman (por rangos)" = "spearman")
          ),
          span(class = "texto-pequeno",
               "Spearman es más adecuado para variables binarias y ordinales; Pearson mide asociación lineal.")
        ),
        layout_columns(
          col_widths = c(7, 5),
          tarjeta_grafico(
            "Matriz de correlación",
            "Diabetes_012 y las predictoras del modelo (se muestra el triángulo inferior; se rotulan los valores con |r| ≥ 0,15)",
            plotlyOutput(ns("heatmap_cor"), height = "580px")
          ),
          tarjeta_grafico(
            "Correlación con Diabetes_012",
            "Ordenadas de mayor a menor asociación (valor absoluto)",
            plotlyOutput(ns("barras_cor"), height = "580px")
          )
        ),
        caja_aviso(
          strong("Correlación no implica causalidad. "),
          "Un coeficiente alto indica que dos variables tienden a variar juntas, no que una cause la otra.
          Además, la correlación entre predictoras (por ejemplo, salud general y días de mala salud física)
          advierte sobre posible multicolinealidad.",
          tipo = "advertencia", icono = "triangle-exclamation"
        )
      ),

      # ---- Modelo: desempeño -------------------------------------------------------------
      nav_panel(
        "Desempeño del modelo", value = "desempeno", icon = icon("gauge-high"),
        div(
          class = "barra-controles",
          control_segmentado(
            ns("tipo_promedio"), "Promedio de las métricas por clase:",
            choices = c("Macro" = "macro", "Weighted" = "weighted")
          ),
          uiOutput(ns("explicacion_promedio"), inline = TRUE)
        ),
        uiOutput(ns("kpis_modelo")),
        uiOutput(ns("nota_accuracy")),
        layout_columns(
          col_widths = c(6, 6),
          tarjeta_grafico(
            "Matriz de confusión",
            "Conjunto de prueba · filas = clase real, columnas = clase predicha",
            plotlyOutput(ns("matriz_confusion"), height = "420px"),
            nota = "La diagonal contiene los aciertos. Con «% por fila», cada celda de la diagonal es el recall de esa clase.",
            controles = control_segmentado(ns("escala_matriz"), NULL,
                                           choices = c("% por fila" = "fila", "Conteos" = "conteo"))
          ),
          tarjeta_grafico(
            "Curvas ROC uno contra el resto (OvR)",
            "Cada curva compara una clase frente a las otras dos",
            plotlyOutput(ns("curvas_roc"), height = "420px"),
            nota = "La diagonal gris representa un clasificador al azar (AUC = 0,5)."
          )
        ),
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header(icon("table"), "Métricas por clase y promedios"),
            card_body(fillable = FALSE, DTOutput(ns("tabla_metricas"), fill = FALSE))
          ),
          card(
            card_header(icon("scale-unbalanced"), "¿Por qué ponderar las clases?"),
            card_body(
              fillable = FALSE,
              DTOutput(ns("tabla_comparacion"), fill = FALSE),
              uiOutput(ns("nota_comparacion"))
            )
          )
        )
      ),

      # ---- Modelo: interpretación ------------------------------------------------------------
      nav_panel(
        "Interpretación del modelo", value = "interpretacion", icon = icon("scale-balanced"),
        layout_columns(
          col_widths = c(7, 5),
          tarjeta_grafico(
            "Odds ratios del modelo final",
            "exp(β) de cada predictora frente a «No diabetes» (escala logarítmica)",
            plotlyOutput(ns("grafico_or"), height = "600px")
          ),
          card(
            card_header(icon("circle-question"), "¿Cómo leer los odds ratios?"),
            card_body(
              tags$ul(
                class = "lista-icono",
                tags$li(icon("arrow-up"), span(strong("OR > 1: "), "la variable se asocia con mayores odds de esa clase frente a «No diabetes».")),
                tags$li(icon("arrow-down"), span(strong("OR < 1: "), "se asocia con menores odds.")),
                tags$li(icon("equals"), span(strong("OR ≈ 1: "), "no hay asociación apreciable.")),
                tags$li(icon("ruler"), span("En variables numéricas el OR es ", strong("por cada unidad"),
                                            " (un punto de IMC, un día, un nivel). El efecto de k unidades es ORᵏ."))
              ),
              uiOutput(ns("resumen_or")),
              caja_aviso(
                "Son asociaciones ajustadas por las demás variables del modelo, no efectos causales. El modelo
                se entrenó con pesos de clase: los OR son estimaciones puntuales y no se reportan intervalos de
                confianza.",
                tipo = "info", icono = "circle-info"
              )
            )
          )
        ),
        card(
          card_header(icon("table"), "Coeficientes y odds ratios"),
          card_body(fillable = FALSE, DTOutput(ns("tabla_or"), fill = FALSE))
        )
      )
    )
  )
}

resultados_server <- function(id, datos, modelo_info) {
  moduleServer(id, function(input, output, session) {

    estado <- datos[[COLUMNA_ESTADO]]
    distribucion <- distribucion_clases(datos)
    metricas <- modelo_info$metricas
    metricas_base <- modelo_info$metricas_base

    # Valor de una métrica para una clase o para un promedio
    valor_clase <- function(m, columna, clase) m$por_clase[[columna]][m$por_clase$clase == clase]
    valor_promedio <- function(m, columna, tipo) m$promedios[[columna]][m$promedios$promedio == tipo]

    # =========================================================================
    # EDA · Distribución
    # =========================================================================

    # La dona se construye directamente con plotly: ggplotly no convierte
    # coordenadas polares (coord_polar).
    output$dona <- renderPlotly({
      tooltip <- paste0("<b>", distribucion$clase, "</b><br>", formatear_numero(distribucion$n),
                        " registros<br>", formatear_porcentaje(distribucion$pct))

      plot_ly(
        distribucion, labels = ~clase, values = ~n, type = "pie",
        hole = 0.62, sort = FALSE, direction = "clockwise", rotation = 60,
        marker = list(colors = unname(COLORES_CLASES[as.character(distribucion$clase)]),
                      line = list(color = "#FFFFFF", width = 2)),
        texttemplate = "<b>%{label}</b><br>%{percent:.1%}", textposition = "outside",
        hovertext = tooltip, hoverinfo = "text"
      ) |>
        layout(annotations = list(list(
          text = paste0("<b>", formatear_numero(sum(distribucion$n)), "</b><br>registros"),
          x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 17, color = COLOR_TEXTO)
        ))) |>
        estilo_plotly() |>
        layout(
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05, yanchor = "top"),
          margin = list(l = 30, r = 30, t = 40, b = 40)
        )
    })

    output$tabla_distribucion <- renderTable({
      data.frame(
        `Código` = c(as.character(0:2), ""),
        Estado = c(as.character(distribucion$clase), "Total"),
        Registros = formatear_numero(c(distribucion$n, sum(distribucion$n))),
        Porcentaje = formatear_porcentaje(c(distribucion$pct, 1), 2),
        check.names = FALSE
      )
    }, align = "llrr", class = "table tabla-simple", spacing = "s")

    output$nota_distribucion <- renderUI({
      n_clase <- stats::setNames(distribucion$n, distribucion$clase)
      tagList(
        caja_aviso(
          frase(
            "Por cada registro con prediabetes hay ",
            strong(formatear_numero(n_clase[["No diabetes"]] / n_clase[["Prediabetes"]])),
            " sin diabetes. Un clasificador que siempre respondiera «No diabetes» acertaría el ",
            strong(formatear_porcentaje(max(distribucion$pct))), " de las veces sin detectar a nadie con la enfermedad."
          ),
          tipo = "info", icono = "scale-unbalanced"
        )
      )
    })

    # =========================================================================
    # EDA · Factores de riesgo (barras 100 % apiladas)
    # =========================================================================
    lapply(FACTORES_PRINCIPALES, function(variable) {
      output[[paste0("barras_", variable)]] <- renderPlotly({
        alto <- if (variable == "GenHlth") 380 else 250
        grafico_composicion(resumen_por_factor(datos, variable), etiqueta_variable(variable), alto_px = alto)
      })
    })

    output$barras_extra <- renderPlotly({
      req(input$variable_extra)
      grafico_composicion(
        resumen_por_factor(datos, input$variable_extra),
        etiqueta_variable(input$variable_extra),
        alto_px = 380
      )
    })

    # =========================================================================
    # EDA · Histogramas
    # =========================================================================
    output$titulo_hist <- renderText({
      paste0("Histograma de ", input$variable_hist, " (", etiqueta_variable(input$variable_hist), ")")
    })

    output$subtitulo_hist <- renderUI({
      if (isTRUE(input$hist_por_clase)) {
        "Porcentaje de personas de cada estado en cada intervalo (los paneles son comparables entre sí)"
      } else {
        "Número de personas en cada intervalo"
      }
    })

    output$ayuda_hist <- renderUI({
      ancho <- ANCHO_INTERVALO[[input$variable_hist]]
      p(class = "texto-pequeno",
        "Cada barra agrupa ", if (ancho == 1) "un valor (1 día)." else paste(ancho, "unidades de IMC."),
        " Al comparar por estado se usan porcentajes porque los grupos tienen tamaños muy distintos.")
    })

    output$histograma <- renderPlotly({
      variable <- input$variable_hist
      ancho <- ANCHO_INTERVALO[[variable]]
      unidad <- if (variable == "BMI") "kg/m²" else "días"

      # Se agrupan los valores en intervalos [inicio, inicio + ancho)
      intervalos <- data.frame(inicio = floor(datos[[variable]] / ancho) * ancho, clase = estado)
      rotulo <- function(inicio) {
        if (ancho == 1) paste(inicio, unidad) else paste0(inicio, " a ", inicio + ancho - 1, " ", unidad)
      }

      if (isTRUE(input$hist_por_clase)) {
        resumen <- intervalos |>
          count(clase, inicio) |>
          group_by(clase) |>
          mutate(pct = n / sum(n)) |>
          ungroup() |>
          mutate(tooltip = paste0("<b>", clase, "</b><br>", rotulo(inicio), "<br>",
                                  formatear_porcentaje(pct), " del grupo (", formatear_numero(n), " personas)"))

        g <- ggplot(resumen, aes(x = inicio + ancho / 2, y = pct, fill = clase, text = tooltip)) +
          geom_col(width = ancho * 0.9) +
          facet_wrap(~clase, ncol = 1) +
          scale_fill_manual(values = COLORES_CLASES) +
          scale_y_continuous(labels = etiqueta_eje_porcentaje) +
          labs(x = paste0(variable, " (", unidad, ")"), y = "% de personas del grupo") +
          tema_dashboard() +
          theme(legend.position = "none")
      } else {
        resumen <- intervalos |>
          count(inicio) |>
          mutate(pct = n / sum(n),
                 tooltip = paste0("<b>", rotulo(inicio), "</b><br>", formatear_numero(n), " personas (",
                                  formatear_porcentaje(pct), ")"))

        g <- ggplot(resumen, aes(x = inicio + ancho / 2, y = n, text = tooltip)) +
          geom_col(width = ancho * 0.9, fill = COLOR_SERIE_UNICA) +
          scale_y_continuous(labels = etiqueta_eje_numero) +
          labs(x = paste0(variable, " (", unidad, ")"), y = "Número de personas") +
          tema_dashboard()
      }

      a_plotly(g, leyenda = FALSE)
    })

    output$tabla_hist <- renderTable({
      variable <- input$variable_hist
      resumir <- function(x) {
        c(Personas = formatear_numero(length(x)),
          Media = formatear_numero(mean(x), 2),
          Mediana = formatear_numero(stats::median(x), 1),
          `Desv. estándar` = formatear_numero(stats::sd(x), 2),
          `Mínimo` = formatear_numero(min(x)),
          `Máximo` = formatear_numero(max(x)))
      }
      filas <- lapply(CLASES_DIABETES, function(clase) resumir(datos[[variable]][estado == clase]))
      tabla <- as.data.frame(do.call(rbind, c(filas, list(resumir(datos[[variable]])))), check.names = FALSE)
      cbind(Estado = c(CLASES_DIABETES, "Total"), tabla)
    }, align = "lrrrrrr", class = "table tabla-simple", spacing = "s")

    # =========================================================================
    # EDA · Edad
    # =========================================================================
    resumen_edad <- resumen_por_factor(datos, "Age")

    output$grafico_edad <- renderPlotly({
      if (identical(input$vista_edad, "composicion")) {
        return(grafico_composicion(resumen_edad, "Edad", alto_px = 430))
      }

      tasas <- resumen_edad |>
        filter(clase != "No diabetes") |>
        mutate(clase = factor(clase, levels = c("Prediabetes", "Diabetes")))
      tasas$tooltip <- tooltip_tasas(tasas, "Edad")
      maximos <- tasas |> group_by(clase) |> slice_max(pct, n = 1, with_ties = FALSE) |> ungroup()

      g <- ggplot(tasas, aes(x = grupo, y = pct, color = clase, group = clase, text = tooltip)) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 2.8) +
        geom_text(
          data = maximos, inherit.aes = FALSE,
          aes(x = grupo, y = pct + max(tasas$pct) * 0.07, label = paste("máx.", formatear_porcentaje(pct))),
          size = 3.3, color = COLOR_TEXTO
        ) +
        scale_color_manual(values = COLORES_CLASES[c("Prediabetes", "Diabetes")]) +
        scale_y_continuous(labels = etiqueta_eje_porcentaje, limits = c(0, NA),
                           expand = expansion(mult = c(0, 0.12))) +
        labs(x = "Grupo de edad (años)", y = "% de personas del grupo") +
        tema_dashboard()

      a_plotly(g)
    })

    output$nota_edad <- renderUI({
      diabetes <- resumen_edad |> filter(clase == "Diabetes")
      maximo <- diabetes[which.max(diabetes$pct), ]
      frase(
        punto_clase("Diabetes"), "La proporción de diabetes pasa de ",
        strong(formatear_porcentaje(diabetes$pct[1])), " en el grupo de ", as.character(diabetes$grupo[1]),
        " años a un máximo de ", strong(formatear_porcentaje(maximo$pct)), " en el grupo de ",
        as.character(maximo$grupo), " años."
      )
    })

    # =========================================================================
    # EDA · Correlación
    # =========================================================================
    correlaciones <- reactive({
      req(input$metodo_cor)
      calcular_correlaciones(datos, input$metodo_cor)
    })

    output$heatmap_cor <- renderPlotly({
      matriz <- correlaciones()
      variables <- colnames(matriz)

      # Triángulo inferior: cada par de variables aparece una sola vez
      celdas <- expand.grid(fila = variables, columna = variables, stringsAsFactors = FALSE)
      celdas$r <- as.vector(matriz)
      celdas <- celdas[match(celdas$fila, variables) > match(celdas$columna, variables), ]

      color_celda <- scales::div_gradient_pal(
        ESCALA_DIVERGENTE[["negativo"]], ESCALA_DIVERGENTE[["neutro"]], ESCALA_DIVERGENTE[["positivo"]]
      )((celdas$r + 1) / 2)
      celdas <- celdas |>
        mutate(
          fila = factor(fila, levels = rev(variables[-1])),
          columna = factor(columna, levels = variables[-length(variables)]),
          etiqueta = ifelse(abs(r) >= 0.15, formatear_numero(r, 2), ""),
          color_texto = color_texto_sobre(color_celda),
          tooltip = paste0("<b>", columna, " · ", fila, "</b><br>",
                           etiqueta_variable(as.character(columna)), " y ",
                           tolower(etiqueta_variable(as.character(fila))),
                           "<br>r = ", formatear_numero(r, 3))
        )

      g <- ggplot(celdas, aes(x = columna, y = fila, fill = r, text = tooltip)) +
        geom_tile(color = "white", linewidth = 0.6) +
        geom_text(aes(label = etiqueta, color = color_texto), size = 2.8) +
        scale_fill_gradient2(
          low = ESCALA_DIVERGENTE[["negativo"]], mid = ESCALA_DIVERGENTE[["neutro"]],
          high = ESCALA_DIVERGENTE[["positivo"]], midpoint = 0, limits = c(-1, 1), name = "r",
          labels = function(x) formatear_numero(x, 1)
        ) +
        scale_color_identity() +
        labs(x = NULL, y = NULL) +
        tema_dashboard(tamano_base = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

      a_plotly(g, leyenda = FALSE)
    })

    output$barras_cor <- renderPlotly({
      matriz <- correlaciones()
      r <- matriz[VARIABLES_MODELO, VARIABLE_OBJETIVO]
      barras <- data.frame(variable = names(r), r = unname(r)) |>
        arrange(abs(r)) |>
        mutate(
          variable = factor(variable, levels = variable),
          sentido = ifelse(r >= 0, "Positiva", "Negativa"),
          posicion_texto = r + ifelse(r >= 0, 1, -1) * max(abs(r)) * 0.09,
          tooltip = paste0("<b>", variable, "</b><br>", etiqueta_variable(as.character(variable)),
                           "<br>r = ", formatear_numero(r, 3))
        )

      g <- ggplot(barras, aes(x = variable, y = r, fill = sentido, text = tooltip)) +
        geom_hline(yintercept = 0, color = COLOR_REFERENCIA) +
        geom_col(width = 0.6) +
        geom_text(aes(y = posicion_texto, label = formatear_numero(r, 2)), size = 3, color = COLOR_TEXTO) +
        scale_fill_manual(values = c(Positiva = ESCALA_DIVERGENTE[["positivo"]],
                                     Negativa = ESCALA_DIVERGENTE[["negativo"]])) +
        scale_y_continuous(labels = function(x) formatear_numero(x, 1), expand = expansion(mult = 0.12)) +
        coord_flip() +
        labs(x = NULL, y = "Correlación con Diabetes_012") +
        tema_dashboard() +
        theme(panel.grid.major.y = element_blank())

      a_plotly(g)
    })

    # =========================================================================
    # Modelo · Desempeño
    # =========================================================================
    output$explicacion_promedio <- renderUI({
      texto <- if (identical(input$tipo_promedio, "weighted")) {
        "Weighted: media ponderada por el número de casos reales de cada clase."
      } else {
        "Macro: media simple de las tres clases; todas pesan igual."
      }
      span(class = "texto-pequeno", texto)
    })

    output$kpis_modelo <- renderUI({
      tipo <- if (is.null(input$tipo_promedio)) "macro" else input$tipo_promedio
      ic <- metricas$accuracy_ic95
      auc <- valor_promedio(metricas, "auc", tipo)

      layout_column_wrap(
        width = "190px", fill = FALSE, class = "fila-kpi",
        tarjeta_kpi("Accuracy", formatear_porcentaje(metricas$accuracy), "bullseye", "lavanda",
                    detalle = paste0("IC 95 %: ", formatear_porcentaje(ic[1]), " – ", formatear_porcentaje(ic[2]))),
        tarjeta_kpi(paste("Precision", tipo), formatear_porcentaje(valor_promedio(metricas, "precision", tipo)),
                    "crosshairs", "azul", detalle = "VP / (VP + FP)"),
        tarjeta_kpi(paste("Recall", tipo), formatear_porcentaje(valor_promedio(metricas, "recall", tipo)),
                    "magnifying-glass", "verde", detalle = "VP / (VP + FN)"),
        tarjeta_kpi(paste("F1-score", tipo), formatear_porcentaje(valor_promedio(metricas, "f1", tipo)),
                    "scale-balanced", "amarillo", detalle = "media armónica de precision y recall"),
        tarjeta_kpi(paste("ROC-AUC OvR", tipo), formatear_numero(auc, 3), "chart-line", "rosa",
                    detalle = interpretar_auc(auc))
      )
    })

    output$nota_accuracy <- renderUI({
      caja_aviso(
        frase(
          "Las métricas se calcularon con ", strong(formatear_numero(metricas$n)), " registros de prueba que el
          modelo no vio al entrenar. El accuracy debe compararse con la ", strong("tasa sin información"), " (",
          formatear_porcentaje(metricas$tasa_sin_informacion), "): es lo que acertaría un modelo que siempre
          dijera «No diabetes». Por el desbalance, las métricas macro describen mejor el desempeño en las tres clases."
        ),
        tipo = "info", icono = "circle-info"
      )
    })

    output$matriz_confusion <- renderPlotly({
      por_fila <- !identical(input$escala_matriz, "conteo")

      celdas <- as.data.frame(metricas$matriz_confusion, stringsAsFactors = FALSE) |>
        group_by(Real) |>
        mutate(pct_fila = Freq / sum(Freq)) |>
        ungroup()
      celdas$valor <- if (por_fila) celdas$pct_fila else celdas$Freq

      color_celda <- scales::seq_gradient_pal(ESCALA_SECUENCIAL[["bajo"]], ESCALA_SECUENCIAL[["alto"]])(
        celdas$valor / max(celdas$valor)
      )
      celdas <- celdas |>
        mutate(
          acierto = Real == Predicho,
          etiqueta = if (por_fila) {
            paste0("<b>", formatear_porcentaje(pct_fila), "</b><br>", formatear_numero(Freq))
          } else {
            paste0("<b>", formatear_numero(Freq), "</b><br>", formatear_porcentaje(pct_fila))
          },
          color_texto = color_texto_sobre(color_celda),
          tooltip = paste0("Real: <b>", Real, "</b><br>Predicho: <b>", Predicho, "</b><br>",
                           formatear_numero(Freq), " personas<br>",
                           formatear_porcentaje(pct_fila), " de los casos reales de ", Real,
                           ifelse(acierto, "<br><b>Acierto</b>", "<br>Error")),
          Real = factor(Real, levels = rev(CLASES_DIABETES)),
          Predicho = factor(Predicho, levels = CLASES_DIABETES)
        )

      g <- ggplot(celdas, aes(x = Predicho, y = Real, fill = valor, text = tooltip)) +
        geom_tile(color = "white", linewidth = 1.2) +
        geom_text(aes(label = etiqueta, color = color_texto), size = 3.8) +
        scale_fill_gradient(
          low = ESCALA_SECUENCIAL[["bajo"]], high = ESCALA_SECUENCIAL[["alto"]],
          labels = if (por_fila) etiqueta_eje_porcentaje else etiqueta_eje_numero,
          name = if (por_fila) "% fila" else "Conteo"
        ) +
        scale_color_identity() +
        labs(x = "Clase predicha", y = "Clase real") +
        tema_dashboard() +
        theme(panel.grid = element_blank())

      a_plotly(g, leyenda = FALSE)
    })

    output$curvas_roc <- renderPlotly({
      curvas <- modelo_info$curvas_roc
      nombres_series <- vapply(CLASES_DIABETES, function(clase) {
        paste0(clase, " (AUC = ", formatear_numero(valor_clase(metricas, "auc", clase), 3), ")")
      }, character(1))

      curvas <- curvas |>
        mutate(
          serie = factor(nombres_series[clase], levels = nombres_series),
          tooltip = paste0("<b>", clase, " vs resto</b><br>Falsos positivos: ", formatear_porcentaje(fpr),
                           "<br>Verdaderos positivos: ", formatear_porcentaje(tpr))
        )

      g <- ggplot(curvas, aes(x = fpr, y = tpr, color = serie, group = serie, text = tooltip)) +
        geom_abline(slope = 1, intercept = 0, color = COLOR_REFERENCIA, linewidth = 0.5) +
        geom_line(linewidth = 0.75) +
        scale_color_manual(values = stats::setNames(unname(COLORES_CLASES[CLASES_DIABETES]), nombres_series)) +
        scale_x_continuous(labels = etiqueta_eje_porcentaje, limits = c(0, 1), expand = expansion(mult = 0.01)) +
        scale_y_continuous(labels = etiqueta_eje_porcentaje, limits = c(0, 1), expand = expansion(mult = 0.01)) +
        labs(x = "Tasa de falsos positivos (1 − especificidad)", y = "Tasa de verdaderos positivos (recall)") +
        tema_dashboard()

      a_plotly(g)
    })

    output$tabla_metricas <- renderDT({
      por_clase <- metricas$por_clase
      promedios <- metricas$promedios
      tabla <- data.frame(
        Clase = c(por_clase$clase, "Promedio macro", "Promedio weighted"),
        Soporte = formatear_numero(c(por_clase$soporte, rep(sum(por_clase$soporte), 2))),
        Precision = formatear_porcentaje(c(por_clase$precision, promedios$precision)),
        Recall = formatear_porcentaje(c(por_clase$recall, promedios$recall)),
        `F1-score` = formatear_porcentaje(c(por_clase$f1, promedios$f1)),
        `ROC-AUC (OvR)` = formatear_numero(c(por_clase$auc, promedios$auc), 3),
        check.names = FALSE
      )

      datatable(
        tabla, rownames = FALSE, class = "compact",
        options = list(dom = "t", ordering = FALSE, language = IDIOMA_DT,
                       columnDefs = list(list(className = "dt-right", targets = 1:5)))
      ) |>
        formatStyle(
          "Clase", target = "row",
          fontWeight = styleEqual(c("Promedio macro", "Promedio weighted"), c("700", "700")),
          backgroundColor = styleEqual(c("Promedio macro", "Promedio weighted"), c("#F6F3FB", "#F6F3FB"))
        )
    })

    output$tabla_comparacion <- renderDT({
      fila <- function(nombre, base, final, formato) {
        data.frame(`Métrica` = nombre, `Sin pesos` = formato(base), `Con pesos (final)` = formato(final),
                   check.names = FALSE)
      }
      auc3 <- function(x) formatear_numero(x, 3)
      tabla <- rbind(
        fila("Accuracy", metricas_base$accuracy, metricas$accuracy, formatear_porcentaje),
        fila("Recall Prediabetes", valor_clase(metricas_base, "recall", "Prediabetes"),
             valor_clase(metricas, "recall", "Prediabetes"), formatear_porcentaje),
        fila("Recall Diabetes", valor_clase(metricas_base, "recall", "Diabetes"),
             valor_clase(metricas, "recall", "Diabetes"), formatear_porcentaje),
        fila("Recall macro", valor_promedio(metricas_base, "recall", "macro"),
             valor_promedio(metricas, "recall", "macro"), formatear_porcentaje),
        fila("F1-score macro", valor_promedio(metricas_base, "f1", "macro"),
             valor_promedio(metricas, "f1", "macro"), formatear_porcentaje),
        fila("ROC-AUC macro", valor_promedio(metricas_base, "auc", "macro"),
             valor_promedio(metricas, "auc", "macro"), auc3)
      )

      datatable(
        tabla, rownames = FALSE, class = "compact",
        options = list(dom = "t", ordering = FALSE, language = IDIOMA_DT,
                       columnDefs = list(list(className = "dt-right", targets = 1:2)))
      ) |>
        formatStyle("Con pesos (final)", fontWeight = "700", backgroundColor = "#F6F3FB")
    })

    output$nota_comparacion <- renderUI({
      p(
        class = "texto-pequeno",
        frase(
          "El modelo sin pesos logra más accuracy porque casi siempre predice «No diabetes», pero solo detecta el ",
          strong(formatear_porcentaje(valor_clase(metricas_base, "recall", "Diabetes"))), " de los casos de diabetes y el ",
          strong(formatear_porcentaje(valor_clase(metricas_base, "recall", "Prediabetes"))), " de prediabetes. ",
          "Con pesos de clase el modelo sacrifica accuracy a cambio de reconocer las clases minoritarias."
        )
      )
    })

    # =========================================================================
    # Modelo · Interpretación (odds ratios)
    # =========================================================================
    odds <- modelo_info$odds_ratios

    output$grafico_or <- renderPlotly({
      orden <- odds |> filter(clase == "Diabetes") |> arrange(odds_ratio) |> pull(variable)
      comparaciones <- c("Prediabetes vs No diabetes", "Diabetes vs No diabetes")
      puntos <- odds |>
        mutate(
          nombre = factor(paste0(etiqueta, " (", variable, ")"),
                          levels = paste0(etiqueta_variable(orden), " (", orden, ")")),
          comparacion = factor(comparacion, levels = comparaciones),
          tooltip = paste0("<b>", comparacion, "</b><br>", etiqueta, " (", variable, ")<br>",
                           unidad, "<br>OR = ", formatear_numero(odds_ratio, 3))
        )

      g <- ggplot(puntos, aes(x = nombre, y = odds_ratio, fill = comparacion, text = tooltip)) +
        geom_hline(yintercept = 1, color = COLOR_REFERENCIA, linewidth = 0.6) +
        geom_point(shape = 21, size = 3.4, color = "white", stroke = 0.8,
                   position = position_dodge(width = 0.55)) +
        scale_fill_manual(values = stats::setNames(unname(COLORES_CLASES[c("Prediabetes", "Diabetes")]), comparaciones)) +
        scale_y_log10(breaks = c(0.9, 1, 1.25, 1.5, 2, 2.5), labels = function(x) formatear_numero(x, 2)) +
        coord_flip() +
        labs(x = NULL, y = "Odds ratio (escala logarítmica)") +
        tema_dashboard()

      a_plotly(g)
    })

    output$resumen_or <- renderUI({
      diabetes <- odds |> filter(clase == "Diabetes")
      binarias <- diabetes |> filter(unidad %in% c("Sí vs No", "Hombre vs Mujer"))
      mayores <- binarias |> filter(odds_ratio > 1) |> arrange(desc(odds_ratio)) |> head(3)
      menores <- diabetes |> filter(odds_ratio < 1) |> arrange(odds_ratio) |> head(3)
      item <- function(f) {
        tags$li(frase(strong(f$etiqueta), " (", f$unidad, "): OR = ", formatear_numero(f$odds_ratio, 2)))
      }

      div(
        class = "resumen-or",
        h6("Diabetes vs No diabetes"),
        p(class = "texto-pequeno", "Variables binarias con mayor OR:"),
        tags$ul(lapply(seq_len(nrow(mayores)), function(i) item(mayores[i, ]))),
        p(class = "texto-pequeno", "Variables con OR menor que 1 (asociación inversa):"),
        tags$ul(lapply(seq_len(nrow(menores)), function(i) item(menores[i, ])))
      )
    })

    output$tabla_or <- renderDT({
      ancho <- odds |>
        select(variable, etiqueta, unidad, clase, coeficiente, odds_ratio) |>
        tabla_ancha_or()
      datatable(
        ancho, rownames = FALSE, class = "compact hover",
        options = list(dom = "ft", pageLength = nrow(ancho), language = IDIOMA_DT,
                       columnDefs = list(list(className = "dt-right", targets = 3:6)))
      )
    })
  })
}

# Pasa la tabla de odds ratios a formato ancho: una fila por variable y
# columnas para Prediabetes y Diabetes (sin depender del paquete tidyr).
tabla_ancha_or <- function(odds) {
  pre <- odds[odds$clase == "Prediabetes", ]
  dia <- odds[odds$clase == "Diabetes", ]
  dia <- dia[match(pre$variable, dia$variable), ]
  data.frame(
    Variable = pre$variable,
    `Descripción` = pre$etiqueta,
    `Unidad del cambio` = pre$unidad,
    `β Prediabetes` = formatear_numero(pre$coeficiente, 4),
    `OR Prediabetes` = formatear_numero(pre$odds_ratio, 3),
    `β Diabetes` = formatear_numero(dia$coeficiente, 4),
    `OR Diabetes` = formatear_numero(dia$odds_ratio, 3),
    check.names = FALSE
  )
}
