# =============================================================================
#  modules/marco_teorico.R  ·  Pestaña 5: Marco teórico
#  -----------------------------------------------------------------------------
#  Conceptos necesarios para entender el proyecto, organizados en subpestañas:
#    Diabetes · Factores de riesgo · Machine Learning ·
#    Regresión logística multinomial · Métricas · Operacionalización
#  El server construye la tabla de operacionalización con los valores que
#  realmente aparecen en el CSV.
# =============================================================================

# Paso de un diagrama de flujo sencillo (icono + texto).
paso_flujo <- function(icono, texto) {
  div(class = "flujo-paso", div(class = "flujo-icono", icon(icono)), span(texto))
}

marco_teorico_ui <- function(id) {
  ns <- NS(id)

  tagList(
    encabezado_pagina(
      "Marco teórico",
      "Conceptos de salud, Machine Learning y evaluación de modelos que sustentan el análisis.",
      icono = "book-open", etiqueta = "Sección 5"
    ),

    navset_pill(
      id = ns("subpestanas"),

      # ---- 1. Diabetes ---------------------------------------------------------
      nav_panel(
        "Diabetes", value = "diabetes", icon = icon("droplet"),
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header("Concepto general"),
            card_body(
              p("La ", strong("diabetes mellitus"), " es una enfermedad crónica que aparece cuando el páncreas no
                produce suficiente insulina o cuando el organismo no puede usarla de forma eficaz. La insulina
                permite que la glucosa entre a las células para producir energía; sin ella, la glucosa se acumula
                en la sangre (hiperglucemia) y con el tiempo daña vasos sanguíneos, nervios, riñones y ojos."),
              tags$ul(
                class = "lista-icono",
                tags$li(icon("circle-dot"), span(strong("Tipo 1: "), "el sistema inmunitario destruye las células que producen insulina; suele aparecer a edades tempranas.")),
                tags$li(icon("circle-dot"), span(strong("Tipo 2: "), "la más frecuente; el cuerpo usa mal la insulina. Se asocia con exceso de peso, inactividad física y edad.")),
                tags$li(icon("circle-dot"), span(strong("Gestacional: "), "aparece durante el embarazo.")),
                tags$li(icon("circle-dot"), span(strong("Prediabetes: "), "glucosa más alta de lo normal pero por debajo del umbral de diabetes. Cambiar hábitos en esta etapa puede prevenir o retrasar la diabetes tipo 2."))
              )
            )
          ),
          card(
            card_header("¿Cómo se mide en el dataset?"),
            card_body(
              p("La variable objetivo ", code("Diabetes_012"), " resume la respuesta de cada persona:"),
              div(
                class = "clases-objetivo",
                div(class = "clase-item", punto_clase("No diabetes"),
                    div(strong("0 · No diabetes"), span("Sin diagnóstico, o diabetes solo durante el embarazo."))),
                div(class = "clase-item", punto_clase("Prediabetes"),
                    div(strong("1 · Prediabetes"), span("Le dijeron que tiene prediabetes o diabetes «límite»."))),
                div(class = "clase-item", punto_clase("Diabetes"),
                    div(strong("2 · Diabetes"), span("Le diagnosticaron diabetes.")))
              ),
              caja_aviso("Es un diagnóstico autorreportado en una encuesta, no una medición de glucosa en sangre.",
                         tipo = "info", icono = "circle-info")
            )
          )
        )
      ),

      # ---- 2. Factores de riesgo -------------------------------------------------
      nav_panel(
        "Factores de riesgo", value = "factores", icon = icon("heart-pulse"),
        p(class = "texto-seccion", "Variables del dataset que la literatura relaciona con la diabetes tipo 2."),
        layout_column_wrap(
          width = "260px", fill = FALSE, heights_equal = "row",
          tarjeta_info(
            "Índice de masa corporal", icono = "weight-scale", color = "lavanda",
            span(class = "chip", "BMI · numérica"),
            p("Relaciona peso y estatura (kg/m²). El exceso de grasa corporal, sobre todo abdominal, aumenta la
              resistencia a la insulina. Según la OMS, hay sobrepeso desde 25 y obesidad desde 30.")
          ),
          tarjeta_info(
            "Presión arterial alta", icono = "heart-pulse", color = "rosa",
            span(class = "chip", "HighBP · 0 = No, 1 = Sí"),
            p("La hipertensión y la diabetes comparten causas (sobrepeso, sedentarismo) y suelen presentarse
              juntas; ambas dañan los vasos sanguíneos.")
          ),
          tarjeta_info(
            "Colesterol alto", icono = "vial", color = "amarillo",
            span(class = "chip", "HighChol · 0 = No, 1 = Sí"),
            p("Las alteraciones del colesterol son frecuentes en personas con resistencia a la insulina y elevan el
              riesgo cardiovascular.")
          ),
          tarjeta_info(
            "Tabaquismo", icono = "smoking", color = "azul",
            span(class = "chip", "Smoker · 0 = No, 1 = Sí"),
            p("Fumar se asocia con mayor resistencia a la insulina y con más complicaciones en quienes ya
              tienen diabetes. En el dataset indica haber fumado al menos 100 cigarrillos en la vida.")
          ),
          tarjeta_info(
            "Actividad física", icono = "person-running", color = "verde",
            span(class = "chip", "PhysActivity · 0 = No, 1 = Sí"),
            p("El ejercicio mejora la sensibilidad a la insulina y ayuda a controlar el peso; la inactividad
              física es un factor de riesgo.")
          ),
          tarjeta_info(
            "Salud general", icono = "stethoscope", color = "lavanda",
            span(class = "chip", "GenHlth · 1 = Excelente … 5 = Mala"),
            p("La autopercepción de la salud resume el estado general de la persona y suele empeorar cuando hay
              enfermedades crónicas.")
          ),
          tarjeta_info(
            "Edad", icono = "calendar-days", color = "azul",
            span(class = "chip", "Age · 13 grupos de 5 años"),
            p("El riesgo de diabetes tipo 2 aumenta con la edad. En el dataset la edad está agrupada desde
              18-24 años (código 1) hasta 80 años o más (código 13).")
          )
        )
      ),

      # ---- 3. Machine Learning -------------------------------------------------------
      nav_panel(
        "Machine Learning", value = "ml", icon = icon("brain"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Clasificación supervisada"),
            card_body(
              p("El ", strong("Machine Learning"), " (aprendizaje automático) permite que un algoritmo aprenda
                patrones a partir de datos. En el aprendizaje ", strong("supervisado"), " cada ejemplo trae la
                respuesta correcta (etiqueta) y el modelo aprende a relacionar las variables de entrada (X) con
                esa respuesta (y)."),
              p("Cuando la respuesta es una categoría, el problema es de ", strong("clasificación"), ". Aquí la
                entrada son ", length(VARIABLES_MODELO), " indicadores de salud y la salida es una de las tres
                clases de ", code("Diabetes_012"), "."),
              div(
                class = "flujo-ml",
                paso_flujo("table", "Datos etiquetados"),
                div(class = "flujo-flecha", icon("arrow-right")),
                paso_flujo("gears", "Entrenamiento"),
                div(class = "flujo-flecha", icon("arrow-right")),
                paso_flujo("cube", "Modelo"),
                div(class = "flujo-flecha", icon("arrow-right")),
                paso_flujo("wand-magic-sparkles", "Predicción")
              )
            )
          ),
          card(
            card_header("Entrenamiento, prueba y generalización"),
            card_body(
              tags$ul(
                class = "lista-icono",
                tags$li(icon("graduation-cap"), span(strong("Entrenamiento: "), "parte de los datos con la que el modelo estima sus coeficientes.")),
                tags$li(icon("vial-circle-check"), span(strong("Prueba: "), "datos que el modelo no vio durante el entrenamiento; permiten estimar cómo se comportará con personas nuevas.")),
                tags$li(icon("triangle-exclamation"), span(strong("Sobreajuste: "), "ocurre cuando el modelo memoriza el entrenamiento y falla con datos nuevos; evaluar en prueba ayuda a detectarlo.")),
                tags$li(icon("scale-unbalanced"), span(strong("Desbalance de clases: "), "si una clase es muy poco frecuente, el modelo tiende a ignorarla. Por eso se usan pesos de clase y métricas macro."))
              )
            )
          )
        )
      ),

      # ---- 4. Regresión logística multinomial --------------------------------------------
      nav_panel(
        "Regresión logística multinomial", value = "multinomial", icon = icon("diagram-project"),
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header("¿Cómo funciona?"),
            card_body(
              tags$ul(
                class = "lista-icono",
                tags$li(icon("tags"), span(strong("Variable respuesta categórica: "), "la salida es una categoría, no un número.")),
                tags$li(icon("layer-group"), span(strong("Más de dos categorías: "), "la regresión logística binaria trabaja con dos clases (sí / no); la multinomial generaliza a K ≥ 3 clases. Aquí K = 3.")),
                tags$li(icon("percent"), span(strong("Probabilidades: "), "para cada persona calcula la probabilidad de cada clase; las tres suman 1.")),
                tags$li(icon("check-double"), span(strong("Clasificación: "), "la clase predicha es la de mayor probabilidad."))
              ),
              h5(class = "subtitulo-teoria", "Ecuaciones del modelo"),
              p("Se toma una categoría de referencia (No diabetes) y se modela el logaritmo de las odds de cada
                clase frente a ella, con un conjunto de coeficientes β para cada clase:"),
              div(class = "formula", HTML(
                "log [ P(Y = k) / P(Y = No diabetes) ] = &eta;<sub>k</sub> = &beta;<sub>0k</sub> + &beta;<sub>1k</sub>&middot;x<sub>1</sub> + &hellip; + &beta;<sub>pk</sub>&middot;x<sub>p</sub>"
              )),
              p("para k = Prediabetes y k = Diabetes. Las probabilidades se obtienen con la función softmax
                (para la referencia, η = 0):"),
              div(class = "formula", HTML(
                "P(Y = k | x) = e<sup>&eta;<sub>k</sub></sup> / ( 1 + e<sup>&eta;<sub>Prediabetes</sub></sup> + e<sup>&eta;<sub>Diabetes</sub></sup> )"
              )),
              p("En R se ajusta con ", code("nnet::multinom()"), ", que estima los coeficientes por máxima verosimilitud.")
            )
          ),
          card(
            card_header("Interpretación y supuestos"),
            card_body(
              h5(class = "subtitulo-teoria", "Odds ratio (OR)"),
              div(class = "formula", HTML("OR = e<sup>&beta;</sup>")),
              p("Si OR > 1, la variable se asocia con mayores odds de esa clase frente a «No diabetes»; si
                OR < 1, con menores odds, manteniendo constantes las demás variables. Es una medida de
                asociación, no de causalidad."),
              h5(class = "subtitulo-teoria", "Supuestos principales"),
              tags$ul(
                class = "lista-icono",
                tags$li(icon("check"), span("Observaciones independientes entre sí.")),
                tags$li(icon("check"), span("Relación lineal entre cada predictora y el log-odds.")),
                tags$li(icon("check"), span("Independencia de alternativas irrelevantes (IIA): la razón entre dos clases no depende de la tercera.")),
                tags$li(icon("check"), span("Sin multicolinealidad severa entre predictoras."))
              )
            )
          )
        )
      ),

      # ---- 5. Métricas -----------------------------------------------------------------
      nav_panel(
        "Métricas", value = "metricas", icon = icon("gauge-high"),
        layout_column_wrap(
          width = "260px", fill = FALSE, heights_equal = "row",
          tarjeta_info(
            "Matriz de confusión", icono = "table-cells", color = "lavanda",
            p("Tabla que cruza la clase real (filas) con la predicha (columnas). La diagonal son los aciertos;
              fuera de ella están los errores entre clases. Con tres clases es de 3 × 3.")
          ),
          tarjeta_info(
            "Accuracy (exactitud)", icono = "bullseye", color = "azul",
            div(class = "formula formula-pequena", "aciertos / total"),
            p("Proporción de predicciones correctas. Con clases desbalanceadas puede ser engañosa.")
          ),
          tarjeta_info(
            "Precision", icono = "crosshairs", color = "verde",
            div(class = "formula formula-pequena", "VP / (VP + FP)"),
            p("De las personas que el modelo asignó a una clase, qué proporción realmente pertenece a ella.")
          ),
          tarjeta_info(
            "Recall (sensibilidad)", icono = "magnifying-glass", color = "amarillo",
            div(class = "formula formula-pequena", "VP / (VP + FN)"),
            p("De las personas que realmente pertenecen a una clase, qué proporción detecta el modelo.")
          ),
          tarjeta_info(
            "F1-score", icono = "scale-balanced", color = "rosa",
            div(class = "formula formula-pequena", "2 · P · R / (P + R)"),
            p("Media armónica de precision y recall: solo es alta si ambas lo son.")
          ),
          tarjeta_info(
            "ROC-AUC", icono = "chart-line", color = "lavanda",
            p("Área bajo la curva ROC: probabilidad de que el modelo asigne mayor probabilidad a un caso positivo
              que a uno negativo. 0,5 equivale al azar y 1 a una separación perfecta.")
          )
        ),
        card(
          card_header(icon("layer-group"), "¿Cómo se calculan con tres clases?"),
          card_body(
            layout_columns(
              col_widths = c(4, 4, 4),
              div(
                class = "estrategia",
                h5("Uno contra el resto (OvR)"),
                p("Cada clase se evalúa como un problema binario: esa clase frente a las otras dos. Así se obtienen
                  precision, recall, F1 y una curva ROC por clase. VP, FP y FN se cuentan para cada clase.")
              ),
              div(
                class = "estrategia",
                h5("Promedio macro"),
                div(class = "formula formula-pequena", HTML("(1/K) &Sigma; m&eacute;trica<sub>k</sub>")),
                p("Media simple de las tres clases: todas pesan igual, incluso la prediabetes, que es muy poco
                  frecuente. Es la más exigente con las clases minoritarias.")
              ),
              div(
                class = "estrategia",
                h5("Promedio weighted"),
                div(class = "formula formula-pequena", HTML("&Sigma; (n<sub>k</sub> / N) &middot; m&eacute;trica<sub>k</sub>")),
                p("Media ponderada por el número de casos reales de cada clase (soporte). Refleja más a la clase
                  mayoritaria. El recall weighted coincide con el accuracy.")
              )
            ),
            caja_aviso(
              "Las métricas binarias no se aplican directamente a tres clases: siempre hay que indicar si se
              reportan por clase (OvR), en promedio macro o en promedio weighted.",
              tipo = "advertencia", icono = "triangle-exclamation"
            )
          )
        )
      ),

      # ---- 6. Operacionalización de variables ----------------------------------------------
      nav_panel(
        "Operacionalización", value = "operacionalizacion", icon = icon("table-list"),
        card(
          card_header(icon("table-list"), "Tabla de operacionalización de variables"),
          card_body(
            fillable = FALSE,
            p(class = "texto-seccion",
              "Nombres exactos de las columnas del CSV. La columna «Valores en el CSV» se calcula a partir del
              archivo para verificar la codificación."),
            DTOutput(ns("tabla_operacionalizacion"), fill = FALSE)
          )
        )
      )
    )
  )
}

marco_teorico_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {

    output$tabla_operacionalizacion <- renderDT({
      # Valores que realmente aparecen en el CSV para cada variable
      valores_observados <- vapply(DICCIONARIO_VARIABLES$variable, function(v) {
        valores <- sort(unique(datos[[v]]))
        if (length(valores) <= 13) {
          paste(valores, collapse = ", ")
        } else {
          paste0(formatear_numero(min(valores)), " a ", formatear_numero(max(valores)),
                 " (", length(valores), " valores)")
        }
      }, character(1))

      rol <- ifelse(
        DICCIONARIO_VARIABLES$variable == VARIABLE_OBJETIVO, "Respuesta",
        ifelse(DICCIONARIO_VARIABLES$variable %in% VARIABLES_MODELO, "Predictora", "No usada en el modelo")
      )

      tabla <- data.frame(
        Variable = DICCIONARIO_VARIABLES$variable,
        Tipo = DICCIONARIO_VARIABLES$tipo,
        `Descripción` = DICCIONARIO_VARIABLES$descripcion,
        Escala = DICCIONARIO_VARIABLES$escala,
        `Valores en el CSV` = unname(valores_observados),
        Rol = rol,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      # Primero la respuesta, luego las predictoras y al final las no usadas
      tabla <- tabla[order(match(tabla$Rol, c("Respuesta", "Predictora", "No usada en el modelo"))), ]

      datatable(
        tabla,
        rownames = FALSE,
        class = "compact hover",
        options = list(
          pageLength = nrow(tabla), dom = "ft", ordering = FALSE,
          language = IDIOMA_DT, autoWidth = FALSE
        )
      ) |>
        formatStyle("Variable", fontWeight = "bold", fontFamily = "Consolas, monospace") |>
        formatStyle(
          "Rol",
          backgroundColor = styleEqual(
            c("Respuesta", "Predictora", "No usada en el modelo"),
            c("#FFE3EE", "#E4F4EA", "#F1F2F4")
          ),
          fontWeight = "600"
        )
    })
  })
}
