# =============================================================================
#  modules/metodologia.R  ·  Pestaña 6: Metodología
#  -----------------------------------------------------------------------------
#  Explica visualmente el flujo de trabajo (de Kaggle a la predicción) y
#  detalla el dataset, las variables, la variable objetivo, la división
#  train/test, el modelo y las métricas. Las cifras se leen del CSV y de
#  model/model.rds (no se escriben a mano).
# =============================================================================

# Etapas del flujo de trabajo: título, icono y explicación breve.
PASOS_METODOLOGIA <- list(
  list("Dataset Kaggle", "database",
       "Archivo diabetes_012_health_indicators_BRFSS2015.csv, construido a partir de la encuesta BRFSS 2015 del CDC."),
  list("Carga de datos", "file-csv",
       paste0("readr::read_csv() lee el archivo y se verifica que estén las ", nrow(DICCIONARIO_VARIABLES),
              " columnas esperadas.")),
  list("Exploración", "magnifying-glass-chart",
       "Tipos de datos, valores únicos, faltantes, duplicados y distribución de Diabetes_012."),
  list("Limpieza", "broom",
       "Control de calidad: valores faltantes, códigos fuera de rango y filas duplicadas."),
  list("Preparación", "sliders",
       paste0("Diabetes_012 pasa a factor de 3 niveles y se seleccionan ", length(VARIABLES_MODELO),
              " predictoras numéricas.")),
  list("Train / Test", "code-branch",
       "División estratificada en entrenamiento y prueba con semilla fija, usando caret::createDataPartition()."),
  list("Regresión logística multinomial", "gears",
       "nnet::multinom() con pesos de clase para compensar el desbalance."),
  list("Evaluación", "gauge-high",
       "Matriz de confusión, accuracy, precision, recall y F1 (macro y weighted) y ROC-AUC uno contra el resto."),
  list("Dashboard", "laptop-code",
       "Aplicación Shiny modular. El modelo se lee desde model/model.rds y no se reentrena."),
  list("Predicción", "wand-magic-sparkles",
       "Formulario con las mismas variables y codificaciones usadas en el entrenamiento.")
)

metodologia_ui <- function(id) {
  ns <- NS(id)

  linea_tiempo <- div(
    class = "linea-tiempo",
    lapply(seq_along(PASOS_METODOLOGIA), function(i) {
      paso <- PASOS_METODOLOGIA[[i]]
      div(
        class = "paso",
        div(class = "paso-marcador", span(class = "paso-numero", i)),
        div(
          class = "paso-contenido",
          div(class = "paso-titulo", icon(paso[[2]]), paso[[1]]),
          div(class = "paso-texto", paso[[3]])
        )
      )
    })
  )

  tagList(
    encabezado_pagina(
      "Metodología",
      "Proceso seguido desde la descarga del dataset hasta la predicción interactiva.",
      icono = "diagram-project", etiqueta = "Sección 6"
    ),

    layout_columns(
      col_widths = c(5, 7),
      card(
        class = "tarjeta-flujo",  # queda fija al hacer scroll (ver styles.css)
        card_header(icon("route"), "Flujo de trabajo"),
        card_body(linea_tiempo)
      ),
      div(
        class = "columna-detalles",
        uiOutput(ns("detalle_dataset")),
        uiOutput(ns("detalle_variables")),
        uiOutput(ns("detalle_objetivo")),
        uiOutput(ns("detalle_particion")),
        uiOutput(ns("detalle_modelo")),
        card(
          card_header(icon("gauge-high"), "Métricas"),
          card_body(
            p("El modelo se evalúa una sola vez en el conjunto de prueba, que no se usó para entrenar:"),
            tags$ul(
              class = "lista-icono",
              tags$li(icon("table-cells"), span(strong("Matriz de confusión 3 × 3"), " para ver aciertos y errores entre clases.")),
              tags$li(icon("bullseye"), span(strong("Accuracy"), " global, comparada con la tasa sin información (predecir siempre «No diabetes»).")),
              tags$li(icon("layer-group"), span(strong("Precision, recall y F1"), " por clase (uno contra el resto) y en promedio macro y weighted.")),
              tags$li(icon("chart-line"), span(strong("ROC-AUC uno contra el resto"), " para cada clase y en promedio."))
            )
          )
        )
      )
    )
  )
}

metodologia_server <- function(id, datos, modelo_info) {
  moduleServer(id, function(input, output, session) {

    columnas_csv <- setdiff(names(datos), COLUMNA_ESTADO)

    # ---- Dataset y control de calidad ------------------------------------------
    output$detalle_dataset <- renderUI({
      fuera_de_rango <- sum(vapply(VARIABLES_MODELO, function(v) {
        sum(datos[[v]] < modelo_info$rangos[[v]][1] | datos[[v]] > modelo_info$rangos[[v]][2])
      }, numeric(1)))

      card(
        card_header(icon("database"), "Dataset y control de calidad"),
        card_body(
          div(
            class = "mini-kpis",
            div(span(class = "mini-valor", formatear_numero(nrow(datos))), span("registros")),
            div(span(class = "mini-valor", length(columnas_csv)), span("columnas")),
            div(span(class = "mini-valor", formatear_numero(sum(is.na(datos[columnas_csv])))), span("faltantes")),
            div(span(class = "mini-valor", formatear_numero(sum(duplicated(datos[columnas_csv])))), span("filas duplicadas"))
          ),
          p(class = "texto-pequeno", frase(
            "Todas las columnas son numéricas y sus códigos coinciden con el libro de códigos del BRFSS
            (valores de las predictoras fuera de rango: ", strong(fuera_de_rango), "). Los duplicados se conservan:
            con respuestas tan resumidas, personas distintas pueden responder exactamente lo mismo."
          ))
        )
      )
    })

    # ---- Variables -----------------------------------------------------------------
    output$detalle_variables <- renderUI({
      no_usadas <- setdiff(columnas_csv, c(VARIABLE_OBJETIVO, VARIABLES_MODELO))
      card(
        card_header(icon("list-check"), "Variables"),
        card_body(
          p(strong(length(VARIABLES_MODELO), " predictoras"), " que cubren condiciones clínicas, hábitos,
            percepción de salud y datos sociodemográficos:"),
          div(class = "lista-chips", lapply(VARIABLES_MODELO, function(v) span(class = "chip", v))),
          p(class = "texto-pequeno",
            "Las binarias ya vienen codificadas como 0/1; las ordinales (GenHlth, Age, Education, Income) se usan
            como puntajes ordenados; BMI, MentHlth y PhysHlth son cuantitativas. No se usan ",
            paste(no_usadas, collapse = ", "),
            " para mantener un modelo y un formulario sencillos.")
        )
      )
    })

    # ---- Variable objetivo -------------------------------------------------------------
    output$detalle_objetivo <- renderUI({
      distribucion <- distribucion_clases(datos)
      card(
        card_header(icon("bullseye"), "Variable objetivo: Diabetes_012"),
        card_body(
          div(
            class = "barras-objetivo",
            lapply(seq_len(nrow(distribucion)), function(i) {
              clase <- as.character(distribucion$clase[i])
              div(
                class = "barra-objetivo",
                div(class = "barra-objetivo-etiqueta", punto_clase(clase), paste0(i - 1, " · ", clase)),
                div(class = "barra-objetivo-pista",
                    div(class = "barra-objetivo-relleno",
                        style = paste0("width:", round(100 * distribucion$pct[i], 1), "%; background:", COLORES_CLASES[[clase]], ";"))),
                div(class = "barra-objetivo-valor", formatear_porcentaje(distribucion$pct[i]))
              )
            })
          ),
          p(class = "texto-pequeno",
            "Se convierte en factor con niveles ordenados No diabetes, Prediabetes y Diabetes; ",
            strong("No diabetes"), " es la categoría de referencia del modelo.")
        )
      )
    })

    # ---- División entrenamiento / prueba ---------------------------------------------------
    output$detalle_particion <- renderUI({
      particion <- modelo_info$particion
      pct_train <- prop.table(particion$distribucion_train)
      pct_test <- prop.table(particion$distribucion_test)

      card(
        card_header(icon("code-branch"), "División entrenamiento / prueba"),
        card_body(
          div(
            class = "mini-kpis",
            div(span(class = "mini-valor", formatear_numero(particion$n_train)),
                span(paste0("entrenamiento (", formatear_porcentaje(particion$proporcion_train, 0), ")"))),
            div(span(class = "mini-valor", formatear_numero(particion$n_test)),
                span(paste0("prueba (", formatear_porcentaje(1 - particion$proporcion_train, 0), ")"))),
            div(span(class = "mini-valor", particion$semilla), span("semilla (set.seed)"))
          ),
          tags$table(
            class = "table table-sm tabla-simple",
            tags$thead(tags$tr(tags$th("Clase"), tags$th("Entrenamiento"), tags$th("Prueba"))),
            tags$tbody(lapply(CLASES_DIABETES, function(clase) {
              tags$tr(tags$td(punto_clase(clase), clase),
                      tags$td(formatear_porcentaje(pct_train[[clase]], 2)),
                      tags$td(formatear_porcentaje(pct_test[[clase]], 2)))
            }))
          ),
          p(class = "texto-pequeno",
            "La división es estratificada: ambos conjuntos conservan la proporción original de cada clase.")
        )
      )
    })

    # ---- Modelo ------------------------------------------------------------------------------
    output$detalle_modelo <- renderUI({
      pesos <- modelo_info$pesos_clase
      card(
        card_header(icon("gears"), "Modelo: regresión logística multinomial"),
        card_body(
          div(class = "formula formula-codigo", modelo_info$formula),
          tags$ul(
            class = "lista-icono",
            tags$li(icon("cube"), span(frase(
              "Ajustado con ", code("nnet::multinom()"), " (máximo ", modelo_info$max_iteraciones, " iteraciones). ",
              if (isTRUE(modelo_info$convergio[["final"]])) "El ajuste convergió." else "Atención: el ajuste no convergió."
            ))),
            tags$li(icon("scale-balanced"), span(frase(
              strong("Pesos de clase"), " w = N / (K · n", tags$sub("k"), "): ",
              paste(paste0(names(pesos), " ", formatear_numero(pesos, 2)), collapse = " · "),
              ". Así las tres clases aportan lo mismo al ajuste."
            ))),
            tags$li(icon("scale-unbalanced"), span(frase(
              "Como referencia también se entrenó el mismo modelo ", strong("sin pesos"),
              "; la comparación está en Resultados → Desempeño del modelo."
            ))),
            tags$li(icon("floppy-disk"), span(frase(
              "Se guarda en ", code("model/model.rds"), " (entrenado el ",
              format(modelo_info$fecha_entrenamiento, "%d/%m/%Y"), ")."
            )))
          )
        )
      )
    })
  })
}
