# =============================================================================
#  modules/conclusiones.R  ·  Pestaña 10: Conclusiones
#  -----------------------------------------------------------------------------
#  Conclusiones construidas con los resultados REALES: todas las cifras se
#  calculan en el server a partir del CSV y de model/model.rds. Si cambian los
#  datos o el modelo, el texto se actualiza solo.
#  Se distingue explícitamente entre asociación y causalidad.
# =============================================================================

conclusiones_ui <- function(id) {
  ns <- NS(id)

  tagList(
    encabezado_pagina(
      "Conclusiones",
      "Hallazgos principales construidos a partir de los resultados reales del análisis y del modelo.",
      icono = "flag-checkered", etiqueta = "Sección 10"
    ),

    # Conclusiones numeradas con cifras calculadas
    uiOutput(ns("conclusiones_principales")),

    # Asociación vs causalidad
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "tarjeta-asociacion",
        card_header(icon("link"), "Asociación: lo que sí muestran los datos"),
        card_body(uiOutput(ns("asociaciones")))
      ),
      card(
        class = "tarjeta-causalidad",
        card_header(icon("link-slash"), "Causalidad: lo que no se puede afirmar"),
        card_body(
          tags$ul(
            class = "lista-icono",
            tags$li(icon("xmark"), span("No se puede afirmar que la presión alta, el colesterol o el IMC ",
                                        strong("causen"), " diabetes: el diseño es transversal y no permite saber qué ocurrió primero.")),
            tags$li(icon("xmark"), span("Algunas asociaciones pueden ir en sentido inverso: tras un diagnóstico de diabetes una persona
                                        puede cambiar sus hábitos o percibir peor su salud.")),
            tags$li(icon("xmark"), span("Puede haber variables de confusión no medidas (genética, alimentación detallada,
                                        medicamentos, glucosa en sangre).")),
            tags$li(icon("xmark"), span("Los odds ratios del modelo son asociaciones ajustadas, no efectos causales. Para
                                        hablar de causalidad se necesitarían estudios longitudinales o experimentales."))
          )
        )
      )
    ),

    h2(class = "titulo-seccion", "Aporte del proyecto y próximos pasos"),
    layout_column_wrap(
      width = "300px", fill = FALSE, heights_equal = "row",
      tarjeta_info(
        "Utilidad del dashboard", icono = "laptop-code", color = "azul",
        p("Reúne en un solo lugar la exploración de los datos, el modelo y la predicción. Permite explicar el
          proceso completo de Ciencia de Datos de forma visual e interactiva, y comunicar resultados a personas
          sin formación técnica.")
      ),
      tarjeta_info(
        "Potencial del Machine Learning", icono = "brain", color = "lavanda",
        p("Los modelos de clasificación pueden ayudar a priorizar tamizajes (por ejemplo, a quién sugerir una prueba
          de glucosa) a partir de preguntas sencillas. Siempre como apoyo a la decisión, nunca como reemplazo del
          criterio clínico.")
      ),
      tarjeta_info(
        "Posibles mejoras futuras", icono = "rocket", color = "verde",
        tags$ul(
          class = "lista-compacta",
          tags$li("Comparar con modelos más flexibles (bosques aleatorios, gradient boosting) usando validación cruzada."),
          tags$li("Probar una regresión logística ordinal, que aprovecha el orden No diabetes < Prediabetes < Diabetes."),
          tags$li("Ajustar umbrales de decisión y calibrar las probabilidades."),
          tags$li("Incluir las variables no usadas, interacciones y los pesos muestrales del BRFSS."),
          tags$li("Validar con datos más recientes y de otras poblaciones, por ejemplo encuestas colombianas como la ENSIN.")
        )
      )
    ),

    div(
      class = "conclusion-general",
      div(class = "conclusion-general-icono", icon("graduation-cap")),
      div(
        h3("Conclusión general"),
        uiOutput(ns("conclusion_general"))
      )
    )
  )
}

conclusiones_server <- function(id, datos, modelo_info) {
  moduleServer(id, function(input, output, session) {

    # ---- Cifras calculadas -------------------------------------------------------
    estado <- datos[[COLUMNA_ESTADO]]
    distribucion <- distribucion_clases(datos)
    pct <- stats::setNames(distribucion$pct, distribucion$clase)

    metricas <- modelo_info$metricas
    metricas_base <- modelo_info$metricas_base
    valor_clase <- function(m, columna, clase) m$por_clase[[columna]][m$por_clase$clase == clase]
    macro <- function(m, columna) m$promedios[[columna]][m$promedios$promedio == "macro"]

    # Variables binarias del modelo ordenadas por la razón de tasas de diabetes
    binarias <- c("HighBP", "HighChol", "DiffWalk", "Smoker", "PhysActivity", "Fruits", "Veggies")
    tasas <- comparar_tasa_diabetes(datos, binarias)
    tasas <- tasas[order(tasas$razon, decreasing = TRUE), ]
    protectoras <- tasas[tasas$razon < 1, ]

    # Gradientes por edad y por salud general (% de diabetes)
    diabetes_edad <- resumen_por_factor(datos, "Age") |> dplyr::filter(clase == "Diabetes")
    diabetes_salud <- resumen_por_factor(datos, "GenHlth") |> dplyr::filter(clase == "Diabetes")
    edad_max <- diabetes_edad[which.max(diabetes_edad$pct), ]

    # IMC mediano por estado
    mediana_imc <- tapply(datos$BMI, estado, stats::median)

    # Correlaciones de Spearman con Diabetes_012
    cor_objetivo <- calcular_correlaciones(datos, "spearman")[VARIABLES_MODELO, VARIABLE_OBJETIVO]
    top_cor <- cor_objetivo[order(abs(cor_objetivo), decreasing = TRUE)][1:3]
    top_cor_general <- cor_objetivo[order(abs(cor_objetivo), decreasing = TRUE)][1:5]

    # Odds ratios del modelo (Diabetes vs No diabetes)
    or_diabetes <- modelo_info$odds_ratios[modelo_info$odds_ratios$clase == "Diabetes", ]
    or_binarias <- or_diabetes[or_diabetes$variable %in% binarias, ]
    or_binarias <- or_binarias[order(or_binarias$odds_ratio, decreasing = TRUE), ]
    or_de <- function(variable) or_diabetes$odds_ratio[or_diabetes$variable == variable]

    # Clases mejor y peor discriminadas (AUC uno contra el resto)
    por_clase <- metricas$por_clase
    mejor <- por_clase[which.max(por_clase$auc), ]
    peor <- por_clase[which.min(por_clase$auc), ]

    # "a, b y c"
    lista_texto <- function(x) {
      if (length(x) == 1) return(x)
      paste(paste(x[-length(x)], collapse = ", "), "y", x[length(x)])
    }

    # Primera letra en mayúscula
    capitalizar <- function(texto) paste0(toupper(substr(texto, 1, 1)), substr(texto, 2, nchar(texto)))

    # Párrafo que une textos y cifras sin espacios sobrantes (ver frase() en R/utils.R)
    parrafo <- function(...) p(frase(...))

    # ---- Conclusiones numeradas ------------------------------------------------------
    output$conclusiones_principales <- renderUI({
      tarjeta <- function(numero, titulo, icono, color, ...) {
        div(
          class = paste("conclusion-card", paste0("tono-", color)),
          div(class = "conclusion-encabezado",
              span(class = "objetivo-numero", numero),
              div(class = "kpi-icono", icon(icono)),
              h4(titulo)),
          div(class = "conclusion-texto", ...)
        )
      }

      layout_column_wrap(
        width = "520px", fill = FALSE, heights_equal = "row",

        tarjeta(
          1, "Distribución de la diabetes", "chart-pie", "azul",
          parrafo("El ", strong(formatear_porcentaje(pct[["No diabetes"]])), " de los registros corresponde a personas sin
            diabetes, el ", strong(formatear_porcentaje(pct[["Diabetes"]])), " a diabetes y apenas el ",
            strong(formatear_porcentaje(pct[["Prediabetes"]])), " a prediabetes."),
          parrafo("Este desbalance condiciona el modelado: un clasificador que siempre respondiera «No diabetes»
            acertaría el ", strong(formatear_porcentaje(metricas$tasa_sin_informacion)),
            " de las veces sin detectar a nadie con la enfermedad.")
        ),

        tarjeta(
          2, "Factores con mayor asociación", "heart-pulse", "rosa",
          parrafo("Entre las condiciones binarias, la mayor diferencia en la proporción de diabetes aparece en ",
            strong(tolower(tasas$etiqueta[1])), " (", formatear_porcentaje(tasas$tasa_con[1]), " frente a ",
            formatear_porcentaje(tasas$tasa_sin[1]), ", ", formatear_numero(tasas$razon[1], 1), " veces más), seguida de ",
            strong(tolower(tasas$etiqueta[2])), " y ", strong(tolower(tasas$etiqueta[3])), "."),
          parrafo("La diabetes también aumenta con la edad (de ", formatear_porcentaje(diabetes_edad$pct[1]), " en el grupo de ",
            as.character(diabetes_edad$grupo[1]), " años a ", formatear_porcentaje(edad_max$pct), " en el de ",
            as.character(edad_max$grupo), ") y con una peor salud general (de ", formatear_porcentaje(diabetes_salud$pct[1]),
            " con salud «", as.character(diabetes_salud$grupo[1]), "» a ",
            formatear_porcentaje(diabetes_salud$pct[nrow(diabetes_salud)]), " con salud «",
            as.character(diabetes_salud$grupo[nrow(diabetes_salud)]), "»). Las correlaciones de Spearman más altas con
            Diabetes_012 son las de ", strong(lista_texto(names(top_cor))), ".")
        ),

        tarjeta(
          3, "Comportamiento del modelo", "diagram-project", "lavanda",
          parrafo("Manteniendo constantes las demás variables, las condiciones binarias con mayor odds ratio para Diabetes
            frente a No diabetes son ", strong(tolower(or_binarias$etiqueta[1])), " (OR = ",
            formatear_numero(or_binarias$odds_ratio[1], 2), ") y ", strong(tolower(or_binarias$etiqueta[2])), " (OR = ",
            formatear_numero(or_binarias$odds_ratio[2], 2), "). Además, cada nivel de peor salud general multiplica
            los odds por ", formatear_numero(or_de("GenHlth"), 2), " y cada punto de IMC por ",
            formatear_numero(or_de("BMI"), 2), "."),
          parrafo("La clase mejor discriminada es ", strong(mejor$clase), " (AUC = ", formatear_numero(mejor$auc, 3),
            ") y la más difícil es ", strong(peor$clase), " (AUC = ", formatear_numero(peor$auc, 3), "; recall = ",
            formatear_porcentaje(peor$recall), "), coherente con su baja frecuencia y su posición intermedia entre
            los otros dos estados.")
        ),

        tarjeta(
          4, "Métricas", "gauge-high", "amarillo",
          parrafo("En el conjunto de prueba (", formatear_numero(metricas$n), " registros) el modelo final obtuvo
            accuracy de ", strong(formatear_porcentaje(metricas$accuracy)), ", recall macro de ",
            strong(formatear_porcentaje(macro(metricas, "recall"))), ", F1 macro de ",
            strong(formatear_porcentaje(macro(metricas, "f1"))), " y ROC-AUC macro de ",
            strong(formatear_numero(macro(metricas, "auc"), 3)), " (", interpretar_auc(macro(metricas, "auc")), ")."),
          parrafo("Frente al modelo sin pesos (accuracy de ", formatear_porcentaje(metricas_base$accuracy),
            "), pierde exactitud global, pero detecta el ", strong(formatear_porcentaje(valor_clase(metricas, "recall", "Diabetes"))),
            " de los casos de diabetes (frente a ", formatear_porcentaje(valor_clase(metricas_base, "recall", "Diabetes")),
            ") y el ", strong(formatear_porcentaje(valor_clase(metricas, "recall", "Prediabetes"))), " de prediabetes (frente a ",
            formatear_porcentaje(valor_clase(metricas_base, "recall", "Prediabetes")), "). La precisión en prediabetes sigue
            siendo baja (", formatear_porcentaje(valor_clase(metricas, "precision", "Prediabetes")), "): muchas alertas son falsas.")
        )
      )
    })

    # ---- Asociaciones observadas -----------------------------------------------------
    output$asociaciones <- renderUI({
      # Sentido de la asociación de ingresos y educación según el signo de la correlación
      sentido <- function(variable) if (cor_objetivo[[variable]] < 0) "menor" else "mayor"
      texto_socioeconomico <- if (sentido("Income") == sentido("Education")) {
        paste0("mayores ingresos y mayor nivel educativo se asocian con ", sentido("Income"), " proporción de diabetes.")
      } else {
        paste0("mayores ingresos se asocian con ", sentido("Income"), " proporción de diabetes y mayor nivel educativo con ",
               sentido("Education"), ".")
      }
      item <- function(...) tags$li(icon("check"), span(frase(...)))

      tags$ul(
        class = "lista-icono",
        lapply(1:3, function(i) {
          item(strong(tasas$etiqueta[i]), ": ", formatear_porcentaje(tasas$tasa_con[i]),
               " de diabetes con la condición frente a ", formatear_porcentaje(tasas$tasa_sin[i]), " sin ella.")
        }),
        item(strong("IMC"), ": la mediana pasa de ", formatear_numero(mediana_imc[["No diabetes"]]), " (no diabetes) a ",
             formatear_numero(mediana_imc[["Diabetes"]]), " kg/m² (diabetes)."),
        item(strong("Edad y salud general"), ": la proporción de diabetes pasa de ",
             formatear_porcentaje(diabetes_edad$pct[1]), " (", as.character(diabetes_edad$grupo[1]), " años) a ",
             formatear_porcentaje(edad_max$pct), " (", as.character(edad_max$grupo), " años), y de ",
             formatear_porcentaje(diabetes_salud$pct[1]), " (salud ", tolower(as.character(diabetes_salud$grupo[1])), ") a ",
             formatear_porcentaje(diabetes_salud$pct[nrow(diabetes_salud)]), " (salud ",
             tolower(as.character(diabetes_salud$grupo[nrow(diabetes_salud)])), ")."),
        if (nrow(protectoras) > 0) {
          item(strong(capitalizar(lista_texto(tolower(protectoras$etiqueta)))), ": se asocian con ",
               strong("menor"), " proporción de diabetes.")
        },
        item(strong("Nivel socioeconómico"), ": ", texto_socioeconomico)
      )
    })

    # ---- Conclusión general ------------------------------------------------------------
    output$conclusion_general <- renderUI({
      parrafo(
        "Este proyecto muestra cómo la Ciencia de Datos convierte las respuestas de una encuesta de salud en
        conocimiento útil: dimensiona el problema (", strong(formatear_porcentaje(pct[["Diabetes"]])),
        " de las personas reporta diabetes y ", formatear_porcentaje(pct[["Prediabetes"]]), " prediabetes),
        identifica los indicadores más asociados con la enfermedad (",
        lista_texto(tolower(etiqueta_variable(names(top_cor_general)))),
        ") y construye un modelo que discrimina entre estados mejor que el azar (ROC-AUC macro de ",
        strong(formatear_numero(macro(metricas, "auc"), 3)), "). El modelo no reemplaza un diagnóstico médico,
        pero sí ilustra cómo el análisis de datos, hecho con transparencia sobre sus métricas y limitaciones,
        puede apoyar la prevención y la toma de decisiones en salud pública."
      )
    })
  })
}
