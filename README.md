# Dashboard de Diabetes · BRFSS 2015 (R Shiny)

Dashboard académico de Ciencia de Datos para **explorar indicadores de salud** asociados con la diabetes y
**clasificar el estado de diabetes** de una persona (No diabetes, Prediabetes o Diabetes) con una
**regresión logística multinomial**.

> **Advertencia.** Esta herramienta tiene fines educativos y de análisis de datos. La predicción del
> modelo **no constituye un diagnóstico médico**.

---

## 1. Descripción

La aplicación está organizada como un informe interactivo de 10 pestañas:

| # | Pestaña | Contenido |
|---|---------|-----------|
| 1 | Introducción | Qué es la diabetes, qué es un indicador de salud y qué busca el proyecto. |
| 2 | Contexto | La diabetes como problema de salud pública, fuente de datos e indicadores generales del dataset. |
| 3 | Problema | Distribución de `Diabetes_012` y su relación con presión arterial, colesterol e IMC. |
| 4 | Objetivos | Objetivo general y ocho objetivos específicos (con enlaces a la sección donde se cumplen). |
| 5 | Marco teórico | Diabetes, factores de riesgo, Machine Learning, regresión multinomial, métricas y tabla de operacionalización. |
| 6 | Metodología | Flujo de trabajo de Kaggle a la predicción, división train/test y detalles del modelo. |
| 7 | Resultados | EDA (dona, barras, histogramas, edad, correlación) y desempeño del modelo (KPIs, matriz de confusión, ROC multiclase, odds ratios). |
| 8 | Predicción | Formulario interactivo que estima la probabilidad de cada estado. |
| 9 | Limitaciones | Sesgos, representatividad, supuestos del modelo y calidad de los datos. |
| 10 | Conclusiones | Conclusiones generadas con los resultados reales, distinguiendo asociación y causalidad. |

**Ninguna cifra está escrita a mano**: todos los valores que muestra el dashboard se calculan a partir del
CSV o de `model/model.rds`.

## 2. Dataset utilizado

- **Nombre:** Diabetes Health Indicators Dataset (Alex Teboul, Kaggle).
- **Enlace:** <https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset>
- **Archivo usado:** `diabetes_012_health_indicators_BRFSS2015.csv` (el zip de Kaggle trae otros dos
  archivos binarios que **no** se usan).
- **Origen:** encuesta telefónica *Behavioral Risk Factor Surveillance System* (BRFSS) 2015 del CDC de
  Estados Unidos.
- **Estructura verificada:** 253.680 filas, 22 columnas numéricas, sin valores faltantes y 23.899 filas
  duplicadas.
- **Variable objetivo:** `Diabetes_012` → `0 = No diabetes` (incluye diabetes solo durante el embarazo),
  `1 = Prediabetes`, `2 = Diabetes`. Está muy desbalanceada (≈ 84 % / 2 % / 14 %).

## 3. Objetivo

Desarrollar un dashboard interactivo en R Shiny que permita explorar indicadores de salud y utilizar un
modelo de regresión logística multinomial para clasificar el estado de diabetes de una persona.

## 4. Arquitectura

```
CSV (data/) ──► model/train_model.R ──► model/model.rds
                    ▲                         │
                    │ usa                     │ se lee una sola vez
                R/utils.R ◄───────────── app.R ──► modules/*.R (10 Shiny Modules)
                                              └──► www/styles.css
```

- **`R/utils.R`** es el corazón compartido: rutas, lista de variables del modelo, codificaciones,
  etiquetas, preparación de datos, métricas multiclase, componentes visuales y estilo de los gráficos.
  Lo usan **tanto el entrenamiento como la app**, lo que garantiza que el formulario de predicción use
  exactamente las mismas variables y codificaciones del entrenamiento.
- **`model/train_model.R`** entrena el modelo **una sola vez** y guarda en `model/model.rds` el modelo y
  todos sus resultados (métricas, matriz de confusión, curvas ROC, odds ratios, rangos válidos).
- **`app.R`** solo carga librerías, datos, modelo y módulos; arma la barra de navegación e inicializa cada
  módulo. **Nunca entrena el modelo.**
- **`modules/`**: cada pestaña es un Shiny Module independiente con `nombre_ui(id)` y
  `nombre_server(id, ...)`. Los servidores reciben lo que necesitan como argumentos (`datos`,
  `modelo_info` o la función `ir_a()` para cambiar de pestaña).

## 5. Estructura de carpetas

```
diabetes_shiny/
├── app.R                      # punto de entrada
├── diabetes_shiny.Rproj       # proyecto de RStudio (fija la carpeta de trabajo)
├── data/
│   └── diabetes_012_health_indicators_BRFSS2015.csv
├── model/
│   ├── train_model.R          # entrenamiento y evaluación
│   └── model.rds              # se genera al ejecutar train_model.R
├── R/
│   └── utils.R                # funciones y constantes compartidas
├── modules/
│   ├── introduccion.R   ├── contexto.R      ├── problema.R
│   ├── objetivos.R      ├── marco_teorico.R ├── metodologia.R
│   ├── resultados.R     ├── prediccion.R    ├── limitaciones.R
│   └── conclusiones.R
├── www/
│   └── styles.css             # diseño pastel (cards, navbar, botones, sombras)
└── README.md
```

## 6. Paquetes

| Paquete | Uso |
|---------|-----|
| `shiny` | Aplicación web y Shiny Modules |
| `bslib` | Tema Bootstrap 5, barra de navegación, cards, layouts e interruptores |
| `dplyr` | Manipulación y resumen de datos |
| `ggplot2` | Construcción de los gráficos |
| `plotly` | Gráficos interactivos (`ggplotly`) y dona |
| `readr` | Lectura del CSV |
| `nnet` | `multinom()`: regresión logística multinomial |
| `caret` | `createDataPartition()`: división train/test estratificada |
| `pROC` | Curvas ROC y AUC uno contra el resto |
| `DT` | Tablas interactivas |

No se necesitan paquetes adicionales. `scales` (escalas de color) y `htmltools` se instalan
automáticamente como dependencias de `ggplot2` y `shiny`. Los iconos son de Font Awesome, incluidos en
`shiny`.

## 7. Instalación

**Entorno recomendado:** R 4.3 o superior y RStudio.

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "ggplot2",
  "plotly",
  "readr",
  "nnet",
  "caret",
  "pROC",
  "DT"
))
```

Versiones con las que se probó: R 4.5.3, shiny 1.14.0, bslib 0.12.0, dplyr 1.2.1, ggplot2 4.0.3,
plotly 4.12.1, DT 0.34.0. Se necesita **bslib ≥ 0.9** (usa `navbar_options()`) y, si tiene ggplot2 4.x,
**plotly ≥ 4.11**.

### Opcional: controlar dependencias con `renv`

`renv` guarda las versiones exactas de los paquetes del proyecto, para que funcione igual en otro
computador:

```r
install.packages("renv")
renv::init()       # crea la librería del proyecto y el archivo renv.lock
renv::snapshot()   # guarda las versiones actuales
# En otro computador:
renv::restore()    # instala exactamente las mismas versiones
```

## 8. Entrenamiento del modelo

Antes de abrir la aplicación se debe ejecutar **una vez** (con el proyecto abierto en RStudio):

```r
source("model/train_model.R")
```

El script:

1. Carga el CSV y verifica las 22 columnas.
2. Inspecciona tipos de datos, faltantes, valores únicos, duplicados y la distribución de la variable objetivo.
3. Prepara los datos: `Diabetes_012` pasa a factor de 3 niveles (referencia: *No diabetes*).
4. Divide en **80 % entrenamiento / 20 % prueba** de forma estratificada con `set.seed(123)`.
5. Calcula **pesos de clase** `w_k = N / (K · n_k)` para compensar el desbalance.
6. Entrena el modelo final (con pesos) y un modelo de referencia (sin pesos) con `nnet::multinom()`.
7. Evalúa en el conjunto de prueba y guarda todo en **`model/model.rds`** (≈ 8 KB; tarda ≈ 30 s).

Solo hay que repetirlo si cambian los datos o la lista de variables (`VARIABLES_MODELO` en `R/utils.R`).

## 9. Ejecución

```r
shiny::runApp()
```

O abra `app.R` en RStudio y presione **Run App**. La aplicación lee el modelo ya entrenado.

## 10. Descripción del modelo

- **Tipo:** regresión logística multinomial (`nnet::multinom`), categoría de referencia *No diabetes*.
- **Ecuaciones:** `log[P(Y = k) / P(Y = No diabetes)] = β0k + β1k·x1 + … + βpk·xp` para
  k = Prediabetes y k = Diabetes; las probabilidades se obtienen con softmax y la clase predicha es la de
  mayor probabilidad.
- **15 predictoras** (de las 21 disponibles), elegidas por su relación con la diabetes y para mantener un
  formulario sencillo:
  - Condiciones clínicas: `HighBP`, `HighChol`, `BMI`
  - Hábitos: `Smoker`, `PhysActivity`, `Fruits`, `Veggies`
  - Percepción de salud y movilidad: `GenHlth`, `PhysHlth`, `MentHlth`, `DiffWalk`
  - Sociodemográficas: `Sex`, `Age`, `Education`, `Income`
- **Codificación:** las binarias ya vienen como 0/1; las ordinales (`GenHlth`, `Age`, `Education`,
  `Income`) se usan como puntajes ordenados; `BMI`, `MentHlth` y `PhysHlth` son cuantitativas.
- **No se usan:** `CholCheck`, `Stroke`, `HeartDiseaseorAttack`, `HvyAlcoholConsump`, `AnyHealthcare`,
  `NoDocbcCost` (se proponen como mejora futura).
- **Desbalance de clases:** sin corrección, el modelo casi nunca predice prediabetes. Por eso el modelo
  final usa pesos de clase. El dashboard compara ambos modelos (Resultados → Desempeño del modelo).
  Consecuencia: las probabilidades del modelo final describen un escenario con clases equilibradas;
  sirven para comparar perfiles, no como la probabilidad real en la población.

## 11. Métricas

Con tres clases las métricas binarias no se aplican directamente:

- **Por clase (uno contra el resto, OvR):** precision = VP / (VP + FP), recall = VP / (VP + FN), F1 y
  ROC-AUC de cada clase frente a las otras dos.
- **Promedio macro:** media simple de las tres clases (todas pesan igual).
- **Promedio weighted:** media ponderada por el número de casos reales de cada clase.
- **Accuracy** con intervalo de confianza del 95 %, comparada con la **tasa sin información** (acierto de
  predecir siempre la clase mayoritaria).
- **Matriz de confusión 3 × 3** y **curvas ROC OvR** para cada clase.

Resultados de referencia obtenidos al ejecutar `train_model.R` (conjunto de prueba, 50.735 registros;
el dashboard siempre muestra los valores calculados desde `model.rds`):

| Métrica | Sin pesos (referencia) | Con pesos (modelo final) |
|---------|-----------------------:|-------------------------:|
| Accuracy | 84,5 % | 64,6 % |
| Recall Prediabetes | 0,0 % | 28,8 % |
| Recall Diabetes | 16,5 % | 59,1 % |
| Recall macro | 38,0 % | 51,4 % |
| F1-score macro | 38,8 % | 42,4 % |
| ROC-AUC macro (OvR) | 0,776 | 0,773 |

El modelo sin pesos tiene más accuracy porque casi siempre responde «No diabetes» (la tasa sin
información es 84,2 %), pero no detecta ningún caso de prediabetes. El modelo final sacrifica accuracy
para reconocer las clases minoritarias.

## 12. Limitaciones

- Datos de **encuesta autorreportada** (BRFSS): posibles errores de memoria y de deseabilidad social.
- **Sesgos** de no respuesta, cobertura y memoria; la versión de Kaggle excluyó respuestas incompletas.
- **Representatividad:** adultos de EE. UU. en 2015, sin pesos muestrales; extrapolar a otras poblaciones
  (por ejemplo, Colombia) requiere cautela.
- Las variables son **indicadores**, no diagnósticos clínicos (no hay mediciones de glucosa).
- **Correlación no implica causalidad:** el diseño es transversal.
- El modelo **depende de las variables disponibles** y de sus supuestos (linealidad en el log-odds,
  independencia, IIA; las ordinales se tratan como puntajes).
- **Desbalance** fuerte: la prediabetes es difícil de detectar.
- **Calidad de datos:** 9,4 % de filas duplicadas y algunos IMC extremos.

## 13. Advertencia sobre el uso de las predicciones

Las predicciones son estimaciones estadísticas con **fines educativos**. No sustituyen la valoración de
un profesional de la salud ni una prueba de glucosa, y **no deben usarse para tomar decisiones médicas**.

---

## 14. Guía de ejecución paso a paso

**PASO 1**
Descargar el dataset desde Kaggle:
<https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset>

**PASO 2**
Descomprimir y colocar el CSV dentro de:

```
data/
```

El archivo debe llamarse `diabetes_012_health_indicators_BRFSS2015.csv`.

**PASO 3**
Instalar los paquetes (sección 7).

**PASO 4**
Abrir el proyecto en RStudio: doble clic en `diabetes_shiny.Rproj`.

**PASO 5**
Ejecutar:

```r
source("model/train_model.R")
```

**PASO 6**
Verificar que exista:

```
model/model.rds
```

(En la consola: `file.exists("model/model.rds")` debe devolver `TRUE`.)

**PASO 7**
Ejecutar:

```r
shiny::runApp()
```

**PASO 8**
Abrir el dashboard en el navegador (RStudio lo abre solo; si no, copie la dirección
`http://127.0.0.1:XXXX` que aparece en la consola).

## 15. Solución de problemas

| Error | Causa probable | Solución |
|-------|----------------|----------|
| `there is no package called 'xxx'` | Falta un paquete | `install.packages("xxx")` y vuelva a ejecutar. |
| `could not find function "navbar_options"` | bslib desactualizado | `install.packages("bslib")` (se necesita ≥ 0.9). |
| Gráficos vacíos o error de plotly con ggplot2 4.x | plotly desactualizado | `install.packages("plotly")` (≥ 4.11). |
| `cannot open file 'R/utils.R'` o rutas que no existen | La carpeta de trabajo no es la del proyecto | Abra `diabetes_shiny.Rproj` o use `setwd("ruta/a/diabetes_shiny")`. Compruebe con `getwd()`. |
| `No se encontró el archivo de datos 'data/...csv'` | El CSV no está en `data/` o tiene otro nombre | Copie el CSV en `data/` con el nombre exacto `diabetes_012_health_indicators_BRFSS2015.csv` (o cambie `RUTA_DATOS` en `R/utils.R`). |
| `Al CSV le faltan columnas esperadas` | Se usó otro archivo del zip (por ejemplo, el binario 50/50) | Use `diabetes_012_health_indicators_BRFSS2015.csv`, que contiene `Diabetes_012`. |
| `No se encontró el modelo entrenado 'model/model.rds'` | No se ha entrenado | Ejecute `source("model/train_model.R")`. |
| `Las variables del modelo guardado no coinciden...` | Se cambió `VARIABLES_MODELO` sin reentrenar | Vuelva a ejecutar `source("model/train_model.R")`. |
| El modelo .rds no carga en otro computador | Versión distinta de R o de `nnet` | Reentrene en ese computador con `train_model.R` (toma menos de un minuto). |
