# =============================================================================
#  modules/prediccion.R  ·  Pestaña 8: Predicción
#  -----------------------------------------------------------------------------
#  Formulario para ingresar las características de una persona y estimar,
#  con el modelo ya entrenado (model/model.rds), la probabilidad de cada
#  estado de diabetes.
#
#  Garantías de consistencia con el entrenamiento:
#    * El formulario pide exactamente VARIABLES_MODELO (R/utils.R).
#    * Cada control devuelve el MISMO código del CSV (0/1, 1-5, 1-13, ...).
#    * preparar_nueva_observacion() arma un data.frame con los mismos nombres
#      y tipos usados al entrenar, y validar_observacion() revisa los rangos.
#  El modelo se cargó una sola vez al iniciar la app (app.R); aquí no se
#  entrena nada.
# =============================================================================

# Grupos de controles según su tipo
VARIABLES_INTERRUPTOR <- c("HighBP", "HighChol", "DiffWalk", "Smoker", "PhysActivity", "Fruits", "Veggies")
VARIABLES_LISTA       <- c("GenHlth", "Age", "Education", "Income")
VARIABLES_DESLIZADOR  <- c("PhysHlth", "MentHlth")

prediccion_ui <- function(id) {
  ns <- NS(id)

  # Interruptor Sí/No: encendido = 1, apagado = 0 (codificación del CSV)
  interruptor <- function(variable, texto) input_switch(ns(variable), texto, value = FALSE)

  tagList(
    encabezado_pagina(
      "Predicción interactiva",
      "Ingrese las características de una persona: el modelo estimará la probabilidad de cada estado de diabetes.",
      icono = "calculator", etiqueta = "Sección 8"
    ),

    caja_aviso(
      strong("Uso educativo. "),
      "Esta herramienta tiene fines educativos y de análisis de datos. La predicción del modelo no constituye
      un diagnóstico médico.",
      tipo = "advertencia", icono = "triangle-exclamation"
    ),

    layout_columns(
      col_widths = c(7, 5),

      # ---- Formulario -----------------------------------------------------------
      card(
        class = "tarjeta-formulario",
        card_header(
          div(
            div(class = "titulo-grafico", "Características de la persona"),
            div(class = "subtitulo-grafico",
                "Cada opción usa la misma codificación del CSV con la que se entrenó el modelo.")
          )
        ),
        card_body(
          fillable = FALSE,
          div(
            class = "barra-ejemplos",
            selectInput(ns("clase_ejemplo"), "Cargar un registro real del dataset:",
                        choices = c("Cualquier estado" = "todas", stats::setNames(CLASES_DIABETES, CLASES_DIABETES)),
                        width = "230px"),
            actionButton(ns("cargar_ejemplo"), "Cargar ejemplo", icon = icon("shuffle"),
                         class = "btn-outline-primary"),
            actionButton(ns("restablecer"), "Restablecer", icon = icon("rotate-left"),
                         class = "btn-outline-secondary")
          ),
          uiOutput(ns("info_ejemplo")),

          h5(class = "grupo-formulario", icon("heart-pulse"), "Condiciones de salud"),
          layout_column_wrap(
            width = "230px", fill = FALSE,
            interruptor("HighBP", "Presión arterial alta"),
            interruptor("HighChol", "Colesterol alto"),
            interruptor("DiffWalk", "Dificultad para caminar o subir escaleras")
          ),

          h5(class = "grupo-formulario", icon("person-running"), "Hábitos"),
          layout_column_wrap(
            width = "230px", fill = FALSE,
            interruptor("Smoker", "Fumador (100 o más cigarrillos en la vida)"),
            interruptor("PhysActivity", "Actividad física en los últimos 30 días"),
            interruptor("Fruits", "Consume frutas a diario"),
            interruptor("Veggies", "Consume verduras a diario")
          ),

          h5(class = "grupo-formulario", icon("stethoscope"), "Medidas y percepción de salud"),
          layout_columns(
            col_widths = c(6, 6),
            div(
              numericInput(ns("BMI"), "Índice de masa corporal (IMC, kg/m²)",
                           value = 27.5, min = 12, max = 98, step = 0.1, width = "100%"),
              accordion(
                open = FALSE, class = "calculadora-imc",
                accordion_panel(
                  "¿No conoce su IMC? Calcúlelo", icon = icon("calculator"),
                  layout_columns(
                    col_widths = c(6, 6),
                    numericInput(ns("peso"), "Peso (kg)", value = NA, min = 20, max = 300),
                    numericInput(ns("estatura"), "Estatura (cm)", value = NA, min = 100, max = 250)
                  ),
                  actionButton(ns("calcular_imc"), "Calcular y usar este IMC",
                               class = "btn-sm btn-outline-primary")
                )
              )
            ),
            selectInput(ns("GenHlth"), "Salud general (autopercibida)",
                        choices = opciones_select("GenHlth", con_codigo = TRUE), selected = "3")
          ),
          layout_columns(
            col_widths = c(6, 6),
            sliderInput(ns("PhysHlth"), "Días con mala salud física (últimos 30)",
                        min = 0, max = 30, value = 0, step = 1),
            sliderInput(ns("MentHlth"), "Días con mala salud mental (últimos 30)",
                        min = 0, max = 30, value = 0, step = 1)
          ),

          h5(class = "grupo-formulario", icon("user"), "Datos sociodemográficos"),
          layout_columns(
            col_widths = c(6, 6),
            radioButtons(ns("Sex"), "Sexo", choices = opciones_select("Sex"), inline = TRUE),
            selectInput(ns("Age"), "Grupo de edad (años)", choices = opciones_select("Age"), selected = "8")
          ),
          layout_columns(
            col_widths = c(6, 6),
            selectInput(ns("Education"), "Nivel educativo", choices = opciones_select("Education"), selected = "5"),
            selectInput(ns("Income"), "Ingreso anual del hogar", choices = opciones_select("Income"), selected = "7")
          ),

          actionButton(ns("predecir"), "PREDECIR ESTADO", icon = icon("wand-magic-sparkles"),
                       class = "btn-primary btn-lg w-100 boton-predecir")
        )
      ),

      # ---- Resultado --------------------------------------------------------------
      div(class = "columna-resultado", uiOutput(ns("resultado")))
    )
  )
}

prediccion_server <- function(id, datos, modelo_info) {
  moduleServer(id, function(input, output, session) {

    # Perfil mediano del dataset: valor inicial del formulario. En variables
    # 0/1 la mediana coincide con el valor más frecuente. type = 1 garantiza
    # que la mediana sea un valor que realmente existe en los datos.
    perfil_mediano <- vapply(datos[VARIABLES_MODELO], function(x) {
      unname(stats::quantile(x, probs = 0.5, type = 1))
    }, numeric(1))

    # Registro real cargado como ejemplo (NULL si no hay)
    ejemplo_cargado <- reactiveVal(NULL)

    # ---- Escribir un perfil en el formulario -----------------------------------
    aplicar_perfil <- function(perfil) {
      for (v in VARIABLES_INTERRUPTOR) update_switch(v, value = perfil[[v]] == 1, session = session)
      for (v in VARIABLES_LISTA) updateSelectInput(session, v, selected = as.character(perfil[[v]]))
      for (v in VARIABLES_DESLIZADOR) updateSliderInput(session, v, value = perfil[[v]])
      updateNumericInput(session, "BMI", value = perfil[["BMI"]])
      updateRadioButtons(session, "Sex", selected = as.character(perfil[["Sex"]]))
    }

    # ---- Leer el formulario con la codificación del CSV ----------------------------
    leer_formulario <- function() {
      valores <- list()
      for (v in VARIABLES_INTERRUPTOR) valores[[v]] <- as.numeric(isTRUE(input[[v]]))  # TRUE -> 1, FALSE -> 0
      for (v in c(VARIABLES_LISTA, "Sex")) valores[[v]] <- input[[v]]
      for (v in VARIABLES_DESLIZADOR) valores[[v]] <- input[[v]]
      valores$BMI <- input$BMI
      valores
    }

    # Al iniciar: rango válido del IMC (el del entrenamiento) y perfil mediano
    updateNumericInput(session, "BMI", min = modelo_info$rangos$BMI[1], max = modelo_info$rangos$BMI[2])
    aplicar_perfil(perfil_mediano)

    observeEvent(input$restablecer, {
      aplicar_perfil(perfil_mediano)
      ejemplo_cargado(NULL)
    })

    # ---- Cargar un registro real del dataset -----------------------------------------
    observeEvent(input$cargar_ejemplo, {
      candidatos <- if (identical(input$clase_ejemplo, "todas")) {
        seq_len(nrow(datos))
      } else {
        which(datos[[COLUMNA_ESTADO]] == input$clase_ejemplo)
      }
      fila <- candidatos[sample.int(length(candidatos), 1)]
      perfil <- unlist(datos[fila, VARIABLES_MODELO])

      aplicar_perfil(perfil)
      ejemplo_cargado(list(fila = fila, clase = as.character(datos[[COLUMNA_ESTADO]][fila]), perfil = perfil))
    })

    output$info_ejemplo <- renderUI({
      ejemplo <- ejemplo_cargado()
      if (is.null(ejemplo)) return(NULL)
      caja_aviso(
        frase(
          "Se cargó el registro n.º ", strong(formatear_numero(ejemplo$fila)), " del dataset. Estado registrado: ",
          punto_clase(ejemplo$clase), strong(ejemplo$clase),
          ". Presione «Predecir estado» para comparar (el registro pudo formar parte del entrenamiento)."
        ),
        tipo = "info", icono = "database"
      )
    })

    # ---- Calculadora de IMC -------------------------------------------------------------
    observeEvent(input$calcular_imc, {
      peso <- input$peso
      estatura <- input$estatura
      if (!is.numeric(peso) || !is.numeric(estatura) || is.na(peso) || is.na(estatura) ||
          peso <= 0 || estatura <= 0) {
        showNotification("Ingrese un peso (kg) y una estatura (cm) válidos.", type = "warning")
        return()
      }
      imc <- peso / (estatura / 100)^2
      rango <- modelo_info$rangos$BMI
      if (imc < rango[1] || imc > rango[2]) {
        showNotification(
          paste0("El IMC calculado (", formatear_numero(imc, 1), ") está fuera del rango del dataset (",
                 rango[1], " a ", rango[2], ")."),
          type = "warning"
        )
        return()
      }
      updateNumericInput(session, "BMI", value = round(imc, 1))
      showNotification(paste0("IMC calculado: ", formatear_numero(imc, 1), " kg/m²."), type = "message")
    })

    # ---- Predicción (solo al presionar el botón) ------------------------------------------
    prediccion <- eventReactive(input$predecir, {
      # 1. Nuevo data.frame con los mismos nombres y tipos del entrenamiento
      nueva <- preparar_nueva_observacion(leer_formulario())

      # 2. Validación de las entradas
      errores <- validar_observacion(nueva, modelo_info$rangos)
      if (length(errores) > 0) return(list(ok = FALSE, errores = errores))

      # 3. Predicción con el modelo cargado desde model/model.rds
      tryCatch({
        probabilidades <- predecir_probabilidades(modelo_info$modelo, nueva)[1, ]
        list(
          ok = TRUE,
          probabilidades = probabilidades,
          clase = names(probabilidades)[which.max(probabilidades)],
          valores = unlist(nueva[1, ])
        )
      }, error = function(e) {
        list(ok = FALSE, errores = paste("No se pudo calcular la predicción:", conditionMessage(e)))
      })
    })

    # ---- Tarjeta de resultado ----------------------------------------------------------------
    barra_probabilidad <- function(clase, probabilidad, destacada) {
      div(
        class = paste("prob-card", if (destacada) "prob-destacada"),
        div(
          class = "prob-fila",
          span(class = "prob-nombre", punto_clase(clase), clase),
          span(class = "prob-valor", formatear_porcentaje(probabilidad))
        ),
        div(
          class = "progress", role = "progressbar",
          `aria-label` = paste("Probabilidad de", clase),
          `aria-valuenow` = round(100 * probabilidad), `aria-valuemin` = 0, `aria-valuemax` = 100,
          div(class = "progress-bar",
              style = paste0("width:", round(100 * probabilidad, 1), "%; background-color:", COLORES_CLASES[[clase]], ";"))
        )
      )
    }

    output$resultado <- renderUI({
      # Antes de presionar el botón
      if (is.null(input$predecir) || input$predecir == 0) {
        return(div(
          class = "tarjeta-resultado resultado-vacio",
          div(class = "resultado-vacio-icono", icon("wand-magic-sparkles")),
          h4("Aún no hay predicción"),
          p(frase("Complete el formulario y presione ", strong("PREDECIR ESTADO"),
                  ". Aquí aparecerán la clase estimada y la probabilidad de cada estado."))
        ))
      }

      resultado <- prediccion()

      # Entradas inválidas o error del modelo
      if (!resultado$ok) {
        return(div(
          class = "tarjeta-resultado",
          caja_aviso(strong("Revise los datos ingresados:"), tags$ul(lapply(resultado$errores, tags$li)),
                     tipo = "error", icono = "circle-xmark")
        ))
      }

      clase <- resultado$clase
      probabilidades <- resultado$probabilidades

      # Si el formulario sigue igual al ejemplo cargado, se compara con su estado real
      ejemplo <- ejemplo_cargado()
      comparacion <- NULL
      if (!is.null(ejemplo) && isTRUE(all(abs(resultado$valores - ejemplo$perfil[VARIABLES_MODELO]) < 1e-9))) {
        acierto <- identical(ejemplo$clase, clase)
        comparacion <- caja_aviso(
          frase(
            "Estado registrado en el dataset: ", strong(ejemplo$clase), ". ",
            if (acierto) "El modelo coincide con el dato real." else "El modelo no coincide con el dato real."
          ),
          tipo = if (acierto) "exito" else "info", icono = if (acierto) "circle-check" else "circle-info"
        )
      }

      div(
        class = "tarjeta-resultado",
        div(class = "resultado-etiqueta", "RESULTADO"),
        div(
          class = paste("resultado-clase", paste0("clase-", gsub(" ", "-", tolower(clase)))),
          span(class = "resultado-clase-texto", "Predicción:"),
          span(class = "resultado-clase-valor", toupper(clase))
        ),
        h6(class = "prob-titulo", "Probabilidad estimada por el modelo"),
        div(
          class = "probabilidades",
          lapply(CLASES_DIABETES, function(k) barra_probabilidad(k, probabilidades[[k]], destacada = k == clase))
        ),
        p(
          class = "interpretacion",
          frase(
            "El modelo estima que la categoría con mayor probabilidad para las características ingresadas es: ",
            strong(clase), "."
          )
        ),
        comparacion,
        caja_aviso(
          "Esta herramienta tiene fines educativos y de análisis de datos. La predicción del modelo no
          constituye un diagnóstico médico.",
          tipo = "advertencia", icono = "triangle-exclamation"
        ),
        p(
          class = "nota-tecnica",
          icon("circle-info"),
          " El modelo se entrenó con pesos de clase para no ignorar la prediabetes y la diabetes. Por eso sus
          probabilidades describen un escenario con clases equilibradas: sirven para comparar perfiles, no
          como la probabilidad real de cada estado en la población."
        )
      )
    })
  })
}
