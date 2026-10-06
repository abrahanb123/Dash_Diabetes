# =============================================================================
#  model/train_model.R
#  -----------------------------------------------------------------------------
#  Entrena la REGRESIÓN LOGÍSTICA MULTINOMIAL que clasifica Diabetes_012 en
#  tres categorías (0 = No diabetes, 1 = Prediabetes, 2 = Diabetes) y guarda
#  todo lo que necesita el dashboard en model/model.rds.
#
#  ¿Cuándo se ejecuta?
#    Una sola vez, ANTES de abrir la aplicación. Solo hay que repetirlo si
#    cambian los datos o la lista de variables del modelo (R/utils.R).
#
#  ¿Cómo se ejecuta?
#    Con el proyecto abierto en RStudio (diabetes_shiny.Rproj):
#        source("model/train_model.R")
#
#  El dashboard NUNCA reentrena el modelo: solo lee model/model.rds.
#
#  Pasos
#    1. Carga de datos
#    2. Inspección (tipos, faltantes, valores únicos, duplicados, objetivo)
#    3. Preparación de los datos
#    4. División entrenamiento / prueba (80 % / 20 %, estratificada)
#    5. Pesos de clase (para compensar el desbalance)
#    6. Entrenamiento: modelo final (ponderado) y modelo de referencia
#    7. Evaluación en el conjunto de prueba
#    8. Guardado de model/model.rds
# =============================================================================

# ---- 0. Paquetes y funciones compartidas ------------------------------------
library(readr)   # lectura del CSV
library(dplyr)   # manipulación de datos
library(nnet)    # multinom(): regresión logística multinomial
library(caret)   # createDataPartition(): división estratificada
library(pROC)    # curvas ROC y área bajo la curva (AUC)

if (!file.exists(file.path("R", "utils.R"))) {
  stop(
    "Ejecute este script desde la carpeta raíz del proyecto (diabetes_shiny/).\n",
    "Sugerencia: abra diabetes_shiny.Rproj en RStudio y vuelva a correr\n",
    "source(\"model/train_model.R\").",
    call. = FALSE
  )
}
source(file.path("R", "utils.R"), encoding = "UTF-8")

SEMILLA <- 123           # semilla para que la división sea reproducible
PROPORCION_TRAIN <- 0.8  # 80 % entrenamiento, 20 % prueba
MAX_ITERACIONES <- 500   # iteraciones máximas del optimizador de multinom()

inicio <- Sys.time()

titulo <- function(texto) {
  cat("\n", strrep("=", 72), "\n ", texto, "\n", strrep("=", 72), "\n", sep = "")
}


# ---- 1. Carga de datos ---------------------------------------------------------
titulo("1. Carga de datos")

datos <- cargar_datos(RUTA_DATOS)
columnas_csv <- setdiff(names(datos), COLUMNA_ESTADO)

cat("Archivo:   ", RUTA_DATOS, "\n")
cat("Registros: ", formatear_numero(nrow(datos)), "\n")
cat("Columnas:  ", length(columnas_csv), "\n")


# ---- 2. Inspección del dataset -------------------------------------------------
titulo("2. Inspección del dataset")

cat("Tipos de datos de las columnas:\n")
print(table(vapply(datos[columnas_csv], function(x) class(x)[1], character(1))))

faltantes <- colSums(is.na(datos[columnas_csv]))
cat("\nValores faltantes en total:", sum(faltantes), "\n")
if (sum(faltantes) > 0) print(faltantes[faltantes > 0])

cat("\nValores únicos de cada columna:\n")
for (v in columnas_csv) {
  valores <- sort(unique(datos[[v]]))
  resumen <- if (length(valores) <= 13) {
    paste(valores, collapse = ", ")
  } else {
    paste0("de ", min(valores), " a ", max(valores), " (", length(valores), " valores distintos)")
  }
  cat(sprintf("  %-22s %s\n", v, resumen))
}

n_duplicados <- sum(duplicated(datos[columnas_csv]))
cat("\nFilas duplicadas:", formatear_numero(n_duplicados),
    paste0("(", formatear_porcentaje(n_duplicados / nrow(datos)), ")"), "\n")
cat("Se conservan: con variables tan resumidas, personas distintas pueden\n",
    "dar exactamente las mismas respuestas.\n", sep = "")

cat("\nDistribución de la variable objetivo (Diabetes_012):\n")
distribucion <- distribucion_clases(datos)
print(data.frame(
  clase = distribucion$clase,
  registros = formatear_numero(distribucion$n),
  porcentaje = formatear_porcentaje(distribucion$pct, 2)
), row.names = FALSE)


# ---- 3. Preparación de los datos ----------------------------------------------
titulo("3. Preparación de los datos")

# preparar_datos_modelo() (R/utils.R):
#   * convierte Diabetes_012 en factor de 3 niveles (referencia: "No diabetes");
#   * conserva las 15 predictoras como numéricas (binarias 0/1, ordinales como
#     puntajes y BMI, MentHlth y PhysHlth como cantidades);
#   * elimina filas con faltantes (en este CSV no hay).
datos_modelo <- preparar_datos_modelo(datos)

cat("Niveles de Diabetes_012:", paste(levels(datos_modelo$Diabetes_012), collapse = " | "), "\n")
cat("Categoría de referencia: ", levels(datos_modelo$Diabetes_012)[1], "\n")
cat("Predictoras (", length(VARIABLES_MODELO), "): ",
    paste(VARIABLES_MODELO, collapse = ", "), "\n", sep = "")
cat("Filas descartadas por faltantes:", nrow(datos) - nrow(datos_modelo), "\n")


# ---- 4. División entrenamiento / prueba ----------------------------------------
titulo("4. División entrenamiento / prueba (80 % / 20 %)")

# createDataPartition() hace un muestreo ESTRATIFICADO: mantiene en ambos
# conjuntos la misma proporción de cada clase de Diabetes_012.
set.seed(SEMILLA)
indices_train <- createDataPartition(datos_modelo$Diabetes_012, p = PROPORCION_TRAIN, list = FALSE)
datos_train <- datos_modelo[indices_train, ]
datos_test  <- datos_modelo[-indices_train, ]

cat("Entrenamiento:", formatear_numero(nrow(datos_train)), "registros\n")
cat("Prueba:       ", formatear_numero(nrow(datos_test)), "registros\n\n")
print(rbind(
  entrenamiento = round(100 * prop.table(table(datos_train$Diabetes_012)), 2),
  prueba        = round(100 * prop.table(table(datos_test$Diabetes_012)), 2)
))


# ---- 5. Pesos de clase ----------------------------------------------------------
titulo("5. Pesos de clase")

# El conjunto está muy desbalanceado (pocos casos de prediabetes). Sin
# corrección, el modelo casi nunca predice las clases minoritarias. Con pesos
# w_k = N / (K * n_k) las tres clases aportan lo mismo al ajuste.
pesos_clase <- calcular_pesos_clase(datos_train$Diabetes_012)
datos_train$peso <- unname(pesos_clase[as.character(datos_train$Diabetes_012)])

print(round(pesos_clase, 3))


# ---- 6. Entrenamiento -----------------------------------------------------------
titulo("6. Entrenamiento de la regresión logística multinomial")

formula_modelo <- reformulate(VARIABLES_MODELO, response = "Diabetes_012")
cat("Fórmula:", paste(deparse(formula_modelo, width.cutoff = 500), collapse = " "), "\n\n")

# Modelo FINAL: con pesos de clase. Es el que usa el dashboard para predecir.
cat("Ajustando el modelo final (con pesos de clase)...\n")
modelo <- multinom(formula_modelo, data = datos_train, weights = peso,
                   maxit = MAX_ITERACIONES, trace = FALSE)

# Modelo de REFERENCIA: sin pesos. Solo se usa para comparar métricas y
# mostrar por qué el accuracy es engañoso cuando las clases están desbalanceadas.
cat("Ajustando el modelo de referencia (sin pesos)...\n")
modelo_base <- multinom(formula_modelo, data = datos_train,
                        maxit = MAX_ITERACIONES, trace = FALSE)

for (m in list(list("final", modelo), list("de referencia", modelo_base))) {
  if (m[[2]]$convergence == 0) {
    cat("El modelo", m[[1]], "convergió correctamente.\n")
  } else {
    warning("El modelo ", m[[1]], " no convergió: aumente maxit.", call. = FALSE)
  }
}


# ---- 7. Evaluación en el conjunto de prueba --------------------------------------
titulo("7. Evaluación en el conjunto de prueba")

prob_test <- predecir_probabilidades(modelo, datos_test)
metricas <- calcular_metricas_multiclase(datos_test$Diabetes_012, prob_test)

prob_test_base <- predecir_probabilidades(modelo_base, datos_test)
metricas_base <- calcular_metricas_multiclase(datos_test$Diabetes_012, prob_test_base)

curvas_roc <- calcular_curvas_roc(datos_test$Diabetes_012, prob_test)
odds_ratios <- calcular_odds_ratios(modelo)

cat("Matriz de confusión (filas = real, columnas = predicho):\n")
print(metricas$matriz_confusion)

cat("\nMétricas por clase (uno contra el resto):\n")
tabla_clases <- metricas$por_clase
tabla_clases[, c("precision", "recall", "f1", "auc")] <- round(tabla_clases[, c("precision", "recall", "f1", "auc")], 3)
print(tabla_clases, row.names = FALSE)

cat("\nPromedios (macro = media simple; weighted = ponderada por soporte):\n")
tabla_promedios <- metricas$promedios
tabla_promedios[, -1] <- round(tabla_promedios[, -1], 3)
print(tabla_promedios, row.names = FALSE)

cat("\nAccuracy:", round(metricas$accuracy, 4),
    "| IC 95 %:", paste(round(metricas$accuracy_ic95, 4), collapse = " - "),
    "| Kappa:", round(metricas$kappa, 4), "\n")
cat("Tasa sin información (predecir siempre la clase mayoritaria):",
    round(metricas$tasa_sin_informacion, 4), "\n")

cat("\nComparación con el modelo sin pesos:\n")
print(data.frame(
  metrica = c("Accuracy", "Recall macro", "F1 macro", "ROC-AUC macro", "Recall Prediabetes", "Recall Diabetes"),
  sin_pesos = round(c(metricas_base$accuracy, metricas_base$promedios$recall[1], metricas_base$promedios$f1[1],
                      metricas_base$promedios$auc[1], metricas_base$por_clase$recall[2:3]), 3),
  con_pesos = round(c(metricas$accuracy, metricas$promedios$recall[1], metricas$promedios$f1[1],
                      metricas$promedios$auc[1], metricas$por_clase$recall[2:3]), 3)
), row.names = FALSE)


# ---- 8. Guardado ------------------------------------------------------------------
titulo("8. Guardado de model/model.rds")

# Se guarda una lista con el modelo y todo lo que el dashboard muestra, para
# que la aplicación no tenga que reentrenar ni recalcular nada pesado.
resultado_modelo <- list(
  modelo = aligerar_modelo(modelo),  # objeto multinom listo para predict()
  formula = paste(deparse(formula_modelo, width.cutoff = 500), collapse = " "),
  variables = VARIABLES_MODELO,
  clases = CLASES_DIABETES,
  rangos = lapply(datos_train[VARIABLES_MODELO], range),  # para validar el formulario
  pesos_clase = pesos_clase,
  particion = list(
    semilla = SEMILLA,
    proporcion_train = PROPORCION_TRAIN,
    n_train = nrow(datos_train),
    n_test = nrow(datos_test),
    distribucion_train = table(datos_train$Diabetes_012),
    distribucion_test = table(datos_test$Diabetes_012)
  ),
  max_iteraciones = MAX_ITERACIONES,
  convergio = c(final = modelo$convergence == 0, referencia = modelo_base$convergence == 0),
  metricas = metricas,
  metricas_base = metricas_base,
  curvas_roc = curvas_roc,
  odds_ratios = odds_ratios,
  fecha_entrenamiento = Sys.time(),
  version_r = R.version.string
)

saveRDS(resultado_modelo, RUTA_MODELO)

cat("Modelo guardado en:", RUTA_MODELO, "\n")
cat("Tamaño del archivo:", formatear_numero(file.size(RUTA_MODELO) / 1024, 1), "KB\n")
cat("Tiempo total:", formatear_numero(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1), "segundos\n")
cat("\nListo. Ahora puede ejecutar la aplicación con shiny::runApp()\n")
