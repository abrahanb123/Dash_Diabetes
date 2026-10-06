# =============================================================================
#  R/utils.R
#  -----------------------------------------------------------------------------
#  Funciones y constantes reutilizables del proyecto.
#
#  Este archivo lo cargan tanto la aplicación (app.R) como el script de
#  entrenamiento (model/train_model.R). Así, el entrenamiento y el dashboard
#  usan EXACTAMENTE las mismas variables, codificaciones, etiquetas y fórmulas.
#
#  Contenido
#    1. Rutas y constantes del proyecto
#    2. Paleta de colores
#    3. Diccionario de variables y etiquetas de las categorías
#    4. Formato de números (estilo colombiano: 1.234,5)
#    5. Carga y preparación de datos
#    6. Funciones del modelo (pesos, predicción, métricas, ROC, odds ratios)
#    7. Resúmenes descriptivos reutilizables
#    8. Componentes de interfaz (encabezados, tarjetas, avisos)
#    9. Funciones auxiliares de gráficos (tema ggplot2, estilo plotly)
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Rutas y constantes del proyecto
# -----------------------------------------------------------------------------

# Rutas relativas a la carpeta raíz del proyecto (diabetes_shiny/).
RUTA_DATOS  <- file.path("data", "diabetes_012_health_indicators_BRFSS2015.csv")
RUTA_MODELO <- file.path("model", "model.rds")
URL_KAGGLE  <- "https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset"

# Variable objetivo y nombre de sus tres categorías (códigos 0, 1 y 2 del CSV).
VARIABLE_OBJETIVO <- "Diabetes_012"
CLASES_DIABETES   <- c("No diabetes", "Prediabetes", "Diabetes")

# Variables predictoras del modelo: 15 de las 21 disponibles. Cubren las áreas
# priorizadas (presión arterial, colesterol, IMC, hábitos, salud general,
# física y mental, edad, sexo, educación e ingresos). El formulario de
# predicción pide exactamente estas variables.
VARIABLES_MODELO <- c(
  "HighBP", "HighChol", "BMI",                    # condiciones clínicas y peso
  "Smoker", "PhysActivity", "Fruits", "Veggies",  # hábitos de vida
  "GenHlth", "PhysHlth", "MentHlth", "DiffWalk",  # percepción de salud y movilidad
  "Sex", "Age", "Education", "Income"             # sociodemográficas
)

# Columna auxiliar que agrega cargar_datos() (no existe en el CSV original).
COLUMNA_ESTADO <- "Estado_diabetes"


# -----------------------------------------------------------------------------
# 2. Paleta de colores
# -----------------------------------------------------------------------------

# Paleta pastel de la interfaz: fondos, tarjetas e iconos.
PALETA_PASTEL <- c(
  azul     = "#A8DADC",
  verde    = "#B7E4C7",
  lavanda  = "#CDB4DB",
  rosa     = "#FFC8DD",
  amarillo = "#FFE5B4",
  fondo    = "#F8F9FA"
)

# Colores de las clases en los GRÁFICOS. Son tonos más profundos de la misma
# paleta (azul, amarillo y rosa): los pasteles puros son tan claros que sobre
# fondo blanco las barras y las líneas se pierden. Estos tres tonos se
# verificaron para que se distingan también con daltonismo (protanopia y
# deuteranopia). Cada clase conserva su color en todos los gráficos.
COLORES_CLASES <- c(
  "No diabetes" = "#5390CC",
  "Prediabetes" = "#D4912A",
  "Diabetes"    = "#C2639A"
)

COLOR_SERIE_UNICA <- "#8E77C2"  # lavanda: gráficos con una sola serie
COLOR_TEXTO       <- "#2B2D42"  # texto principal
COLOR_TEXTO_SUAVE <- "#5C6170"  # ejes y textos secundarios
COLOR_REJILLA     <- "#E9ECEF"  # líneas de referencia muy suaves
COLOR_REFERENCIA  <- "#C9CDD3"  # diagonal del azar en la curva ROC, línea OR = 1

# Escala secuencial (un solo tono, de claro a oscuro): matriz de confusión.
ESCALA_SECUENCIAL <- c(bajo = "#F4F0FA", alto = "#6F55A3")

# Escala divergente para correlaciones: negativo (azul), cero (gris), positivo (rosa).
ESCALA_DIVERGENTE <- c(negativo = "#4E8FC9", neutro = "#F0EFEC", positivo = "#C2639A")


# -----------------------------------------------------------------------------
# 3. Diccionario de variables y etiquetas de las categorías
# -----------------------------------------------------------------------------

# Descripción de las 22 columnas del CSV según el libro de códigos del BRFSS 2015.
DICCIONARIO_VARIABLES <- data.frame(
  variable = c(
    "Diabetes_012", "HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke",
    "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump",
    "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk",
    "Sex", "Age", "Education", "Income"
  ),
  etiqueta = c(
    "Estado de diabetes", "Presión arterial alta", "Colesterol alto",
    "Control de colesterol", "Índice de masa corporal", "Tabaquismo",
    "Accidente cerebrovascular", "Enfermedad coronaria o infarto",
    "Actividad física", "Consumo de frutas", "Consumo de verduras",
    "Consumo excesivo de alcohol", "Cobertura de salud",
    "Sin consulta médica por costo", "Salud general", "Días de mala salud mental",
    "Días de mala salud física", "Dificultad para caminar", "Sexo", "Grupo de edad",
    "Nivel educativo", "Nivel de ingresos"
  ),
  tipo = c(
    "Categórica", "Binaria", "Binaria", "Binaria", "Cuantitativa", "Binaria",
    "Binaria", "Binaria", "Binaria", "Binaria", "Binaria", "Binaria", "Binaria",
    "Binaria", "Ordinal", "Cuantitativa", "Cuantitativa", "Binaria", "Binaria",
    "Ordinal", "Ordinal", "Ordinal"
  ),
  descripcion = c(
    "Diagnóstico de diabetes reportado por la persona (la clase 0 incluye la diabetes que solo se presentó durante el embarazo)",
    "Un profesional de la salud le dijo que tiene la presión arterial alta",
    "Un profesional de la salud le dijo que tiene el colesterol alto",
    "Se revisó el colesterol en los últimos 5 años",
    "Peso en kilogramos dividido por la estatura en metros al cuadrado",
    "Ha fumado al menos 100 cigarrillos en toda su vida",
    "Alguna vez le dijeron que tuvo un accidente cerebrovascular",
    "Reporta enfermedad coronaria o infarto de miocardio",
    "Hizo actividad física en los últimos 30 días, sin contar el trabajo",
    "Consume fruta una o más veces al día",
    "Consume verduras una o más veces al día",
    "Hombres con más de 14 tragos por semana o mujeres con más de 7",
    "Tiene algún tipo de seguro o cobertura de salud",
    "En el último año necesitó un médico pero no pudo ir por el costo",
    "Cómo califica su salud en general",
    "Días de los últimos 30 en que su salud mental no fue buena",
    "Días de los últimos 30 en que su salud física no fue buena",
    "Tiene dificultad seria para caminar o subir escaleras",
    "Sexo de la persona encuestada",
    "Edad agrupada en intervalos de 5 años",
    "Máximo nivel educativo alcanzado",
    "Ingreso anual del hogar en dólares estadounidenses"
  ),
  escala = c(
    "0 = No diabetes, 1 = Prediabetes, 2 = Diabetes", "0 = No, 1 = Sí",
    "0 = No, 1 = Sí", "0 = No, 1 = Sí", "Numérica (kg/m²)", "0 = No, 1 = Sí",
    "0 = No, 1 = Sí", "0 = No, 1 = Sí", "0 = No, 1 = Sí", "0 = No, 1 = Sí",
    "0 = No, 1 = Sí", "0 = No, 1 = Sí", "0 = No, 1 = Sí", "0 = No, 1 = Sí",
    "1 = Excelente … 5 = Mala", "Días (0 a 30)", "Días (0 a 30)", "0 = No, 1 = Sí",
    "0 = Mujer, 1 = Hombre", "1 = 18-24 años … 13 = 80 años o más",
    "1 = Nunca asistió … 6 = Universidad completa",
    "1 = Menos de USD 10.000 … 8 = USD 75.000 o más"
  ),
  stringsAsFactors = FALSE
)

# Etiquetas de las categorías (código del CSV -> texto legible).
ETIQUETAS_SI_NO <- c("0" = "No", "1" = "Sí")

ETIQUETAS_CATEGORIAS <- list(
  Diabetes_012 = c("0" = "No diabetes", "1" = "Prediabetes", "2" = "Diabetes"),
  Sex = c("0" = "Mujer", "1" = "Hombre"),
  GenHlth = c("1" = "Excelente", "2" = "Muy buena", "3" = "Buena", "4" = "Regular", "5" = "Mala"),
  Age = c(
    "1" = "18-24", "2" = "25-29", "3" = "30-34", "4" = "35-39", "5" = "40-44",
    "6" = "45-49", "7" = "50-54", "8" = "55-59", "9" = "60-64", "10" = "65-69",
    "11" = "70-74", "12" = "75-79", "13" = "80 o más"
  ),
  Education = c(
    "1" = "Nunca asistió o solo preescolar",
    "2" = "Grados 1 a 8 (primaria)",
    "3" = "Grados 9 a 11 (secundaria incompleta)",
    "4" = "Grado 12 o GED (secundaria completa)",
    "5" = "1 a 3 años de universidad o técnica",
    "6" = "4 o más años de universidad"
  ),
  Income = c(
    "1" = "Menos de USD 10.000", "2" = "USD 10.000 a 14.999",
    "3" = "USD 15.000 a 19.999", "4" = "USD 20.000 a 24.999",
    "5" = "USD 25.000 a 34.999", "6" = "USD 35.000 a 49.999",
    "7" = "USD 50.000 a 74.999", "8" = "USD 75.000 o más"
  )
)

# Cómo se lee el odds ratio de cada predictora (unidad del cambio).
UNIDAD_CAMBIO <- c(
  HighBP = "Sí vs No", HighChol = "Sí vs No", BMI = "por cada punto de IMC",
  Smoker = "Sí vs No", PhysActivity = "Sí vs No", Fruits = "Sí vs No",
  Veggies = "Sí vs No", GenHlth = "por cada nivel de peor salud",
  PhysHlth = "por cada día", MentHlth = "por cada día", DiffWalk = "Sí vs No",
  Sex = "Hombre vs Mujer", Age = "por cada grupo de 5 años",
  Education = "por cada nivel educativo", Income = "por cada nivel de ingresos"
)

# Devuelve las etiquetas de las categorías de una variable.
etiquetas_de <- function(variable) {
  if (variable %in% names(ETIQUETAS_CATEGORIAS)) ETIQUETAS_CATEGORIAS[[variable]] else ETIQUETAS_SI_NO
}

# Convierte los códigos numéricos de una variable en un factor con etiquetas.
etiquetar <- function(x, variable) {
  etiquetas <- etiquetas_de(variable)
  factor(as.character(x), levels = names(etiquetas), labels = unname(etiquetas))
}

# Nombre legible de una variable ("HighBP" -> "Presión arterial alta").
etiqueta_variable <- function(variable) {
  DICCIONARIO_VARIABLES$etiqueta[match(variable, DICCIONARIO_VARIABLES$variable)]
}

# Opciones para un selectInput: el usuario ve el texto y Shiny recibe el código.
opciones_select <- function(variable, con_codigo = FALSE) {
  etiquetas <- etiquetas_de(variable)
  textos <- if (con_codigo) paste(names(etiquetas), "·", etiquetas) else unname(etiquetas)
  stats::setNames(names(etiquetas), textos)
}

# Categorías de IMC de la Organización Mundial de la Salud.
categoria_imc <- function(bmi) {
  cut(
    bmi,
    breaks = c(-Inf, 18.5, 25, 30, 35, 40, Inf),
    right = FALSE,
    labels = c("Bajo peso", "Normal", "Sobrepeso", "Obesidad I", "Obesidad II", "Obesidad III")
  )
}


# -----------------------------------------------------------------------------
# 4. Formato de números (separador de miles "." y decimal ",")
# -----------------------------------------------------------------------------

formatear_numero <- function(x, decimales = 0) {
  formatC(x, format = "f", digits = decimales, big.mark = ".", decimal.mark = ",")
}

formatear_porcentaje <- function(x, decimales = 1) {
  paste0(formatear_numero(100 * x, decimales), " %")  #   = espacio que no se parte
}

# Etiquetas de ejes para ggplot2 con el mismo formato.
etiqueta_eje_numero <- function(x) formatear_numero(x)
etiqueta_eje_porcentaje <- function(x) paste0(formatear_numero(100 * x), " %")


# -----------------------------------------------------------------------------
# 5. Carga y preparación de datos
# -----------------------------------------------------------------------------

# Lee el CSV de Kaggle, verifica que tenga las columnas esperadas y agrega una
# columna auxiliar con el nombre de la clase. Las columnas originales NO se
# modifican.
cargar_datos <- function(ruta = RUTA_DATOS) {
  if (!file.exists(ruta)) {
    stop(
      "No se encontró el archivo de datos '", ruta, "'.\n",
      "Descárguelo de Kaggle (", URL_KAGGLE, ")\n",
      "y copie el CSV en la carpeta data/ del proyecto.",
      call. = FALSE
    )
  }

  # Todas las columnas del CSV son numéricas (vienen escritas como 0.0, 1.0, ...).
  datos <- readr::read_csv(
    ruta,
    col_types = readr::cols(.default = readr::col_double()),
    progress = FALSE
  )

  columnas_faltantes <- setdiff(DICCIONARIO_VARIABLES$variable, names(datos))
  if (length(columnas_faltantes) > 0) {
    stop("Al CSV le faltan columnas esperadas: ",
         paste(columnas_faltantes, collapse = ", "), call. = FALSE)
  }
  if (!all(datos[[VARIABLE_OBJETIVO]] %in% c(0, 1, 2))) {
    stop("La columna Diabetes_012 debe contener solo los valores 0, 1 y 2.", call. = FALSE)
  }

  datos[[COLUMNA_ESTADO]] <- factor(
    datos[[VARIABLE_OBJETIVO]],
    levels = c(0, 1, 2),
    labels = CLASES_DIABETES
  )
  datos
}

# Número de columnas que trae el CSV original (sin la columna auxiliar).
contar_columnas_csv <- function(datos) {
  length(setdiff(names(datos), COLUMNA_ESTADO))
}

# Prepara la tabla que usa el modelo:
#   * Diabetes_012 pasa a factor con 3 niveles ("No diabetes" es la referencia).
#   * Las predictoras se dejan numéricas: las binarias ya vienen como 0/1 y las
#     ordinales (GenHlth, Age, Education, Income) se usan como puntajes.
#   * Se descartan filas con valores faltantes (el CSV de Kaggle no tiene).
preparar_datos_modelo <- function(datos) {
  datos_modelo <- as.data.frame(datos[, c(VARIABLE_OBJETIVO, VARIABLES_MODELO)])
  datos_modelo <- datos_modelo[stats::complete.cases(datos_modelo), ]
  datos_modelo[[VARIABLE_OBJETIVO]] <- factor(
    datos_modelo[[VARIABLE_OBJETIVO]],
    levels = c(0, 1, 2),
    labels = CLASES_DIABETES
  )
  datos_modelo
}

# Convierte los valores del formulario en un data.frame de una fila con las
# mismas columnas y tipos (numéricos) que se usaron en el entrenamiento.
preparar_nueva_observacion <- function(valores) {
  faltantes <- setdiff(VARIABLES_MODELO, names(valores))
  if (length(faltantes) > 0) {
    stop("Faltan variables en el formulario: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }
  fila <- lapply(VARIABLES_MODELO, function(v) suppressWarnings(as.numeric(valores[[v]])))
  names(fila) <- VARIABLES_MODELO
  as.data.frame(fila)
}

# Revisa que cada valor exista y esté dentro del rango observado en el
# entrenamiento. Devuelve un vector de mensajes (vacío si todo está bien).
validar_observacion <- function(nueva, rangos) {
  errores <- character(0)
  for (v in VARIABLES_MODELO) {
    x <- nueva[[v]]
    if (length(x) != 1 || is.na(x)) {
      errores <- c(errores, paste0(etiqueta_variable(v), " (", v, "): falta el valor."))
    } else if (x < rangos[[v]][1] || x > rangos[[v]][2]) {
      errores <- c(errores, paste0(
        etiqueta_variable(v), " (", v, "): debe estar entre ",
        formatear_numero(rangos[[v]][1]), " y ", formatear_numero(rangos[[v]][2]), "."
      ))
    }
  }
  errores
}


# -----------------------------------------------------------------------------
# 6. Funciones del modelo
# -----------------------------------------------------------------------------

# Pesos de clase "balanceados": w_k = N / (K * n_k).
# Una clase poco frecuente (prediabetes) recibe más peso, de modo que las tres
# clases aportan lo mismo a la función de verosimilitud durante el ajuste.
calcular_pesos_clase <- function(y) {
  conteo <- table(y)
  pesos <- length(y) / (length(conteo) * as.numeric(conteo))
  stats::setNames(pesos, names(conteo))
}

# Probabilidades de las tres clases. Siempre devuelve una matriz (filas =
# personas, columnas = clases), incluso cuando se predice una sola persona.
predecir_probabilidades <- function(modelo, nuevos_datos) {
  prob <- stats::predict(modelo, newdata = nuevos_datos, type = "probs")
  if (is.null(dim(prob))) {
    prob <- matrix(prob, nrow = 1, dimnames = list(NULL, names(prob)))
  }
  prob[, CLASES_DIABETES, drop = FALSE]
}

# Clase con mayor probabilidad para cada fila.
clase_predicha <- function(probabilidades) {
  indice <- max.col(probabilidades, ties.method = "first")
  factor(colnames(probabilidades)[indice], levels = CLASES_DIABETES)
}

# Curva ROC "uno contra el resto" (OvR): la clase indicada frente a las otras dos.
roc_uno_contra_resto <- function(observado, probabilidades, clase) {
  pROC::roc(
    response  = as.integer(observado == clase),
    predictor = probabilidades[, clase],
    levels    = c(0, 1),
    direction = "<",
    quiet     = TRUE
  )
}

# Métricas para clasificación con tres clases.
#   * Por clase (uno contra el resto): precision, recall, F1 y ROC-AUC.
#   * Promedio macro: media simple de las tres clases (todas pesan igual).
#   * Promedio weighted: media ponderada por el soporte (casos reales de cada clase).
#   * Accuracy global, su intervalo de confianza del 95 %, kappa de Cohen y la
#     tasa sin información (acierto de predecir siempre la clase mayoritaria).
calcular_metricas_multiclase <- function(observado, probabilidades) {
  observado <- factor(observado, levels = CLASES_DIABETES)
  predicho  <- clase_predicha(probabilidades)

  # Matriz de confusión: filas = clase real, columnas = clase predicha
  matriz <- table(Real = observado, Predicho = predicho)
  total  <- sum(matriz)

  vp <- diag(matriz)               # verdaderos positivos de cada clase
  fp <- colSums(matriz) - vp       # falsos positivos
  fn <- rowSums(matriz) - vp       # falsos negativos
  soporte <- rowSums(matriz)       # casos reales de cada clase

  precision <- ifelse(vp + fp > 0, vp / (vp + fp), 0)  # si nunca se predice la clase -> 0
  recall    <- ifelse(vp + fn > 0, vp / (vp + fn), 0)
  f1        <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)
  auc <- vapply(CLASES_DIABETES, function(clase) {
    as.numeric(pROC::auc(roc_uno_contra_resto(observado, probabilidades, clase)))
  }, numeric(1))

  por_clase <- data.frame(
    clase = CLASES_DIABETES,
    soporte = as.integer(soporte),
    precision = as.numeric(precision),
    recall = as.numeric(recall),
    f1 = as.numeric(f1),
    auc = as.numeric(auc),
    stringsAsFactors = FALSE
  )

  peso <- por_clase$soporte / sum(por_clase$soporte)
  promedios <- data.frame(
    promedio  = c("macro", "weighted"),
    precision = c(mean(por_clase$precision), sum(peso * por_clase$precision)),
    recall    = c(mean(por_clase$recall), sum(peso * por_clase$recall)),
    f1        = c(mean(por_clase$f1), sum(peso * por_clase$f1)),
    auc       = c(mean(por_clase$auc), sum(peso * por_clase$auc)),
    stringsAsFactors = FALSE
  )

  aciertos <- sum(vp)
  acuerdo_azar <- sum(rowSums(matriz) * colSums(matriz)) / total^2
  accuracy <- aciertos / total

  list(
    accuracy = accuracy,
    accuracy_ic95 = as.numeric(stats::binom.test(aciertos, total)$conf.int),
    kappa = (accuracy - acuerdo_azar) / (1 - acuerdo_azar),
    tasa_sin_informacion = max(soporte) / total,
    n = total,
    matriz_confusion = matriz,
    por_clase = por_clase,
    promedios = promedios
  )
}

# Puntos de las curvas ROC (OvR) de las tres clases, resumidos en una malla
# regular de tasas de falsos positivos para que el gráfico sea liviano.
calcular_curvas_roc <- function(observado, probabilidades, n_puntos = 201) {
  observado <- factor(observado, levels = CLASES_DIABETES)
  malla_fpr <- seq(0, 1, length.out = n_puntos)

  curvas <- lapply(CLASES_DIABETES, function(clase) {
    curva <- roc_uno_contra_resto(observado, probabilidades, clase)
    fpr <- 1 - curva$specificities
    tpr <- curva$sensitivities
    tpr_malla <- stats::approx(fpr, tpr, xout = malla_fpr, ties = max, rule = 2)$y
    data.frame(
      clase = clase,
      fpr = c(0, malla_fpr),
      tpr = c(0, tpr_malla),
      auc = as.numeric(pROC::auc(curva)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, curvas)
}

# Odds ratios (exp(beta)) de cada predictora para cada clase frente a la
# categoría de referencia (No diabetes).
calcular_odds_ratios <- function(modelo) {
  coeficientes <- stats::coef(modelo)  # filas: Prediabetes y Diabetes
  filas <- lapply(rownames(coeficientes), function(clase) {
    beta <- coeficientes[clase, VARIABLES_MODELO]
    data.frame(
      clase = clase,
      comparacion = paste(clase, "vs No diabetes"),
      variable = VARIABLES_MODELO,
      etiqueta = etiqueta_variable(VARIABLES_MODELO),
      unidad = unname(UNIDAD_CAMBIO[VARIABLES_MODELO]),
      coeficiente = unname(beta),
      odds_ratio = unname(exp(beta)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

# Quita del modelo los componentes con una fila por persona de entrenamiento
# (valores ajustados, residuos y pesos). No se necesitan para predecir y
# hacen que el archivo .rds pese decenas de megabytes.
aligerar_modelo <- function(modelo) {
  # Se deja una matriz de 0 filas: predict() toma de ella los nombres de las clases.
  modelo$fitted.values <- modelo$fitted.values[0, , drop = FALSE]
  modelo$residuals <- NULL
  modelo$weights <- NULL
  attr(modelo$terms, ".Environment") <- globalenv()
  modelo
}

# Lee model/model.rds y comprueba que sea coherente con este código.
cargar_modelo <- function(ruta = RUTA_MODELO) {
  if (!file.exists(ruta)) {
    stop(
      "No se encontró el modelo entrenado '", ruta, "'.\n",
      "Ejecute primero (desde la carpeta del proyecto):  source(\"model/train_model.R\")",
      call. = FALSE
    )
  }
  modelo_info <- readRDS(ruta)
  if (!inherits(modelo_info$modelo, "multinom")) {
    stop("El archivo ", ruta, " no contiene un modelo multinom válido. ",
         "Vuelva a ejecutar model/train_model.R.", call. = FALSE)
  }
  if (!identical(modelo_info$variables, VARIABLES_MODELO)) {
    stop("Las variables del modelo guardado no coinciden con VARIABLES_MODELO (R/utils.R). ",
         "Vuelva a ejecutar model/train_model.R.", call. = FALSE)
  }
  modelo_info
}

# Interpretación verbal del ROC-AUC (escala de Hosmer y Lemeshow).
interpretar_auc <- function(auc) {
  if (auc >= 0.9) return("discriminación excepcional")
  if (auc >= 0.8) return("discriminación excelente")
  if (auc >= 0.7) return("discriminación aceptable")
  if (auc > 0.5) return("discriminación pobre")
  "sin capacidad de discriminación (equivalente al azar)"
}


# -----------------------------------------------------------------------------
# 7. Resúmenes descriptivos reutilizables
# -----------------------------------------------------------------------------

# Conteo y porcentaje de cada estado de diabetes.
distribucion_clases <- function(datos) {
  datos |>
    dplyr::count(.data[[COLUMNA_ESTADO]], name = "n") |>
    dplyr::rename(clase = 1) |>
    dplyr::mutate(pct = n / sum(n))
}

# Porcentaje de cada estado de diabetes dentro de cada grupo.
#   grupo: vector (factor) con el grupo de cada persona.
resumen_tasas <- function(grupo, estado) {
  data.frame(grupo = grupo, clase = estado) |>
    dplyr::count(grupo, clase, name = "n") |>
    dplyr::group_by(grupo) |>
    dplyr::mutate(total = sum(n), pct = n / total) |>
    dplyr::ungroup()
}

# Igual que resumen_tasas(), usando una variable del dataset con sus etiquetas.
resumen_por_factor <- function(datos, variable) {
  resumen_tasas(etiquetar(datos[[variable]], variable), datos[[COLUMNA_ESTADO]])
}

# Para variables binarias (0/1): % de diabetes con y sin la condición.
comparar_tasa_diabetes <- function(datos, variables) {
  filas <- lapply(variables, function(v) {
    es_diabetes <- datos[[COLUMNA_ESTADO]] == "Diabetes"
    tasa_con <- mean(es_diabetes[datos[[v]] == 1])
    tasa_sin <- mean(es_diabetes[datos[[v]] == 0])
    data.frame(
      variable = v,
      etiqueta = etiqueta_variable(v),
      tasa_con = tasa_con,
      tasa_sin = tasa_sin,
      razon = tasa_con / tasa_sin,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

# Matriz de correlación de la variable objetivo y las predictoras del modelo.
calcular_correlaciones <- function(datos, metodo = "pearson") {
  variables <- c(VARIABLE_OBJETIVO, VARIABLES_MODELO)
  stats::cor(as.matrix(datos[, variables]), method = metodo)
}


# -----------------------------------------------------------------------------
# 8. Componentes de interfaz reutilizables
# -----------------------------------------------------------------------------

# Encabezado de cada pestaña: icono, etiqueta pequeña, título y subtítulo.
encabezado_pagina <- function(titulo, subtitulo, icono, etiqueta = NULL) {
  div(
    class = "encabezado-pagina",
    div(class = "encabezado-icono", icon(icono)),
    div(
      class = "encabezado-texto",
      if (!is.null(etiqueta)) span(class = "encabezado-etiqueta", etiqueta),
      h1(class = "encabezado-titulo", titulo),
      p(class = "encabezado-subtitulo", subtitulo)
    )
  )
}

# Tarjeta KPI: título pequeño, número grande, icono y detalle opcional.
#   color: "azul", "verde", "lavanda", "rosa" o "amarillo" (paleta pastel).
tarjeta_kpi <- function(titulo, valor, icono, color = "azul", detalle = NULL) {
  div(
    class = paste("kpi-card", paste0("tono-", color)),
    div(class = "kpi-icono", icon(icono)),
    div(
      class = "kpi-cuerpo",
      div(class = "kpi-titulo", titulo),
      div(class = "kpi-valor", valor),
      if (!is.null(detalle)) div(class = "kpi-detalle", detalle)
    )
  )
}

# Tarjeta informativa: icono, título y contenido breve.
tarjeta_info <- function(titulo, ..., icono = "circle-info", color = "azul") {
  div(
    class = paste("tarjeta-info", paste0("tono-", color)),
    div(class = "tarjeta-info-icono", icon(icono)),
    h4(class = "tarjeta-info-titulo", titulo),
    div(class = "tarjeta-info-texto", ...)
  )
}

# Caja de aviso. tipo: "info", "advertencia", "exito" o "error".
caja_aviso <- function(..., tipo = "info", icono = "circle-info") {
  div(
    class = paste("caja-aviso", paste0("aviso-", tipo)),
    div(class = "caja-aviso-icono", icon(icono)),
    div(class = "caja-aviso-texto", ...)
  )
}

# Tarjeta para un gráfico: título, subtítulo, el gráfico y una nota al pie.
# full_screen = TRUE agrega un botón para ampliar el gráfico (útil al presentar).
tarjeta_grafico <- function(titulo, subtitulo = NULL, ..., nota = NULL, controles = NULL) {
  bslib::card(
    full_screen = TRUE,
    class = "tarjeta-grafico",
    bslib::card_header(
      div(
        class = "encabezado-grafico",
        div(
          div(class = "titulo-grafico", titulo),
          if (!is.null(subtitulo)) div(class = "subtitulo-grafico", subtitulo)
        ),
        if (!is.null(controles)) div(class = "controles-grafico", controles)
      )
    ),
    bslib::card_body(...),
    if (!is.null(nota)) bslib::card_footer(class = "nota-grafico", nota)
  )
}

# Une textos y etiquetas HTML (strong, code, ...) en una sola frase.
# Se usa cuando un valor calculado va pegado a un signo de puntuación: si se
# pasan como elementos separados, Shiny inserta un espacio entre ellos y se
# vería "( 13,9 % )" en lugar de "(13,9 %)".
frase <- function(...) {
  partes <- list(...)
  partes <- partes[!vapply(partes, is.null, logical(1))]
  texto <- vapply(partes, function(x) {
    if (inherits(x, c("shiny.tag", "shiny.tag.list", "html"))) {
      as.character(x)
    } else {
      htmltools::htmlEscape(paste(x, collapse = ""))
    }
  }, character(1))
  HTML(paste0(texto, collapse = ""))
}

# Punto de color que acompaña el nombre de una clase (la identidad no depende
# solo del color del texto).
punto_clase <- function(clase) {
  span(class = "punto-clase", style = paste0("background:", COLORES_CLASES[[clase]], ";"))
}

# Traducción al español de los textos de las tablas DT.
IDIOMA_DT <- list(
  search = "Buscar:",
  lengthMenu = "Mostrar _MENU_ filas",
  info = "Mostrando _START_ a _END_ de _TOTAL_ filas",
  infoEmpty = "Sin filas para mostrar",
  infoFiltered = "(filtrado de _MAX_ filas)",
  zeroRecords = "No se encontraron coincidencias",
  paginate = list(previous = "Anterior", `next` = "Siguiente")
)


# -----------------------------------------------------------------------------
# 9. Funciones auxiliares de gráficos
#    (ggplot2 y plotly los carga app.R)
# -----------------------------------------------------------------------------

# Tema de ggplot2 limpio: rejilla muy suave, textos grises y leyenda arriba.
tema_dashboard <- function(tamano_base = 12) {
  theme_minimal(base_size = tamano_base) +
    theme(
      text = element_text(color = COLOR_TEXTO),
      axis.text = element_text(color = COLOR_TEXTO_SUAVE),
      axis.title = element_text(color = COLOR_TEXTO_SUAVE, size = rel(0.92)),
      panel.grid.major = element_line(color = COLOR_REJILLA, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold", color = COLOR_TEXTO, hjust = 0),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(6, 12, 6, 6)
    )
}

# Estilo común de los gráficos plotly: tipografía, separadores en español
# (decimal "," y miles "."), tooltip blanco, leyenda horizontal y barra de
# herramientas reducida.
estilo_plotly <- function(p, leyenda = TRUE) {
  p |>
    plotly::layout(
      font = list(family = "Nunito, 'Segoe UI', Roboto, sans-serif", color = COLOR_TEXTO, size = 12),
      separators = ",.",
      hoverlabel = list(
        bgcolor = "#FFFFFF",
        bordercolor = "#D9DCE1",
        font = list(family = "Nunito, 'Segoe UI', Roboto, sans-serif", color = COLOR_TEXTO, size = 12)
      ),
      showlegend = leyenda,
      legend = list(
        orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom",
        title = list(text = ""), font = list(size = 12)
      ),
      margin = list(l = 10, r = 10, t = if (leyenda) 40 else 20, b = 10),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    ) |>
    plotly::config(
      displaylogo = FALSE,
      locale = "es",
      modeBarButtonsToRemove = c(
        "select2d", "lasso2d", "autoScale2d", "toggleSpikelines",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )
}

# Color de texto (blanco o tinta oscura) con mejor contraste sobre cada fondo.
# Se usa para escribir valores dentro de las celdas de los mapas de calor.
color_texto_sobre <- function(colores_fondo) {
  luminancia <- function(hex) {
    rgb <- grDevices::col2rgb(hex) / 255
    lineal <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    0.2126 * lineal[1, ] + 0.7152 * lineal[2, ] + 0.0722 * lineal[3, ]
  }
  l_fondo <- luminancia(colores_fondo)
  contraste_blanco <- 1.05 / (l_fondo + 0.05)
  contraste_tinta <- (l_fondo + 0.05) / (luminancia(COLOR_TEXTO) + 0.05)
  ifelse(contraste_blanco > contraste_tinta, "#FFFFFF", COLOR_TEXTO)
}

# Convierte un ggplot en gráfico interactivo con el estilo del dashboard.
# El texto del tooltip se toma de la estética "text" del ggplot.
a_plotly <- function(grafico, leyenda = TRUE) {
  plotly::ggplotly(grafico, tooltip = "text") |>
    estilo_plotly(leyenda = leyenda)
}

# Texto del tooltip para un resumen de tasas (salida de resumen_tasas()).
tooltip_tasas <- function(resumen, nombre_grupo) {
  paste0(
    "<b>", nombre_grupo, ": ", resumen$grupo, "</b><br>",
    resumen$clase, ": ", formatear_porcentaje(resumen$pct), "<br>",
    formatear_numero(resumen$n), " de ", formatear_numero(resumen$total), " personas"
  )
}

# Barras agrupadas con el % de prediabetes y de diabetes en cada grupo.
# ("No diabetes" es el complemento y se omite para ver mejor las diferencias.)
grafico_tasas <- function(resumen, nombre_grupo, titulo_eje_x = nombre_grupo) {
  datos_grafico <- resumen |>
    dplyr::filter(clase != "No diabetes") |>
    dplyr::mutate(
      clase = factor(clase, levels = c("Prediabetes", "Diabetes")),
      tooltip = tooltip_tasas(dplyr::pick(dplyr::everything()), nombre_grupo),
      etiqueta = formatear_porcentaje(pct)
    )
  separacion <- position_dodge(width = 0.62)

  g <- ggplot(datos_grafico, aes(x = grupo, y = pct, fill = clase, text = tooltip)) +
    geom_col(position = separacion, width = 0.56) +
    geom_text(
      aes(y = pct + max(pct) * 0.06, label = etiqueta),
      position = separacion, size = 3.3, color = COLOR_TEXTO
    ) +
    scale_fill_manual(values = COLORES_CLASES[c("Prediabetes", "Diabetes")]) +
    scale_y_continuous(labels = etiqueta_eje_porcentaje, expand = expansion(mult = c(0, 0.15))) +
    labs(x = titulo_eje_x, y = "% de personas del grupo") +
    tema_dashboard() +
    theme(panel.grid.major.x = element_blank())

  a_plotly(g)
}

# Barras horizontales 100 % apiladas: composición del estado de diabetes en
# cada grupo. "Diabetes" queda pegada al eje para comparar su tamaño entre
# grupos; su porcentaje se escribe dentro de la barra cuando cabe.
grafico_composicion <- function(resumen, nombre_grupo, alto_px = 260) {
  datos_grafico <- resumen |>
    dplyr::mutate(
      clase = factor(clase, levels = CLASES_DIABETES),
      tooltip = tooltip_tasas(dplyr::pick(dplyr::everything()), nombre_grupo)
    )
  etiquetas <- datos_grafico |>
    dplyr::filter(clase == "Diabetes", pct >= 0.09)

  # Grosor de barra aproximado de 26 px, sin importar cuántos grupos haya.
  n_grupos <- dplyr::n_distinct(datos_grafico$grupo)
  ancho <- min(0.7, 26 * n_grupos / max(alto_px - 110, 60))

  g <- ggplot(datos_grafico, aes(x = grupo, y = pct, fill = clase, text = tooltip)) +
    geom_col(width = ancho, color = "white", linewidth = 0.5) +
    geom_text(
      data = etiquetas,
      aes(y = pct / 2, label = formatear_porcentaje(pct, 0)),
      color = "white", size = 3.2, fontface = "bold"
    ) +
    scale_fill_manual(values = COLORES_CLASES, breaks = CLASES_DIABETES) +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(labels = etiqueta_eje_porcentaje, expand = c(0, 0)) +
    coord_flip() +
    labs(x = NULL, y = "% de personas del grupo") +
    tema_dashboard() +
    theme(panel.grid.major.y = element_blank())

  a_plotly(g)
}
