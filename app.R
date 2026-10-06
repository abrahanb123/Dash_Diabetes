# =============================================================================
#  app.R  ·  Punto de entrada del dashboard
#  -----------------------------------------------------------------------------
#  Dashboard de Ciencia de Datos sobre diabetes con el dataset
#  "Diabetes Health Indicators" (BRFSS 2015, Kaggle).
#
#  Este archivo solo se encarga de:
#    1. Cargar las librerías.
#    2. Cargar las funciones auxiliares (R/utils.R) y los módulos (modules/).
#    3. Cargar los datos (CSV) y el modelo YA ENTRENADO (model/model.rds).
#    4. Definir el tema visual.
#    5. Construir la interfaz: barra de navegación con las 10 pestañas.
#    6. Inicializar el servidor de cada módulo.
#
#  Importante: aquí NO se entrena el modelo. Antes de abrir la app, ejecute
#  una vez:  source("model/train_model.R")
# =============================================================================

# ---- 1. Librerías ------------------------------------------------------------
library(shiny)    # aplicación web
library(bslib)    # tema Bootstrap 5, barra de navegación, cards y layouts
library(dplyr)    # manipulación de datos
library(ggplot2)  # construcción de gráficos
library(plotly)   # gráficos interactivos
library(readr)    # lectura del CSV
library(nnet)     # necesario para predecir con el modelo multinom guardado
library(pROC)     # curvas ROC (funciones de R/utils.R)
library(caret)    # paquete del entrenamiento (división estratificada)
library(DT)       # tablas interactivas


# ---- 2. Funciones auxiliares y módulos ----------------------------------------
source("R/utils.R", encoding = "UTF-8")

archivos_modulos <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
for (archivo in archivos_modulos) {
  source(archivo, encoding = "UTF-8")
}


# ---- 3. Datos y modelo ------------------------------------------------------------
datos <- cargar_datos(RUTA_DATOS)          # data/diabetes_012_health_indicators_BRFSS2015.csv
modelo_info <- cargar_modelo(RUTA_MODELO)  # model/model.rds (generado por train_model.R)


# ---- 4. Tema visual ----------------------------------------------------------------
# Colores base del tema Bootstrap. El resto del diseño (tarjetas, sombras,
# navbar, botones) está en www/styles.css.
fuente <- font_collection("Nunito", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif")

tema_app <- bs_theme(
  version = 5,
  bg = "#F8F9FA",         # fondo claro
  fg = "#2B2D42",         # texto principal
  primary = "#7A67AE",    # lavanda profunda: botones, enlaces y pestaña activa
  secondary = "#A8DADC",  # azul pastel
  info = "#5390CC",
  success = "#4E9A6E",
  warning = "#D4912A",
  danger = "#C2639A",
  base_font = fuente,
  heading_font = fuente,
  "border-radius" = "0.75rem"
)


# ---- 5. Interfaz -------------------------------------------------------------------
# El id de cada pestaña (value) coincide con el id de su módulo. Así los
# módulos pueden pedir "ir a" otra pestaña (ver ir_a() en el servidor).
ui <- page_navbar(
  id = "menu",
  title = span(
    class = "marca",
    span(class = "marca-icono", icon("droplet")),
    span(class = "marca-texto", "Diabetes", span(class = "marca-subtitulo", "BRFSS 2015"))
  ),
  window_title = "Dashboard de Diabetes · BRFSS 2015",
  theme = tema_app,
  fillable = FALSE,
  navbar_options = navbar_options(bg = "#FFFFFF", theme = "light", collapsible = TRUE, underline = FALSE),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  nav_panel("Introducción",  value = "introduccion",  icon = icon("house"),                introduccion_ui("introduccion")),
  nav_panel("Contexto",      value = "contexto",      icon = icon("earth-americas"),       contexto_ui("contexto")),
  nav_panel("Problema",      value = "problema",      icon = icon("circle-question"),      problema_ui("problema")),
  nav_panel("Objetivos",     value = "objetivos",     icon = icon("bullseye"),             objetivos_ui("objetivos")),
  nav_panel("Marco teórico", value = "marco_teorico", icon = icon("book-open"),            marco_teorico_ui("marco_teorico")),
  nav_panel("Metodología",   value = "metodologia",   icon = icon("diagram-project"),      metodologia_ui("metodologia")),
  nav_panel("Resultados",    value = "resultados",    icon = icon("chart-column"),         resultados_ui("resultados")),
  nav_panel("Predicción",    value = "prediccion",    icon = icon("calculator"),           prediccion_ui("prediccion")),
  nav_panel("Limitaciones",  value = "limitaciones",  icon = icon("triangle-exclamation"), limitaciones_ui("limitaciones")),
  nav_panel("Conclusiones",  value = "conclusiones",  icon = icon("flag-checkered"),       conclusiones_ui("conclusiones")),

  footer = div(
    class = "pie-pagina",
    span(frase(
      "Datos: CDC · Behavioral Risk Factor Surveillance System 2015, vía ",
      a("Kaggle", href = URL_KAGGLE, target = "_blank", rel = "noopener"), "."
    )),
    span("Proyecto académico de Ciencia de Datos · Herramienta educativa, no diagnóstica.")
  )
)


# ---- 6. Servidor --------------------------------------------------------------------
server <- function(input, output, session) {

  # Cambia de pestaña desde un módulo. Si se indica una subpestaña, también la
  # selecciona (por ejemplo, ir_a("resultados", "desempeno")).
  ir_a <- function(pestana, subpestana = NULL) {
    nav_select("menu", selected = pestana, session = session)
    if (!is.null(subpestana)) {
      nav_select(NS(pestana, "subpestanas"), selected = subpestana, session = session)
    }
  }

  introduccion_server("introduccion", datos = datos, ir_a = ir_a)
  contexto_server("contexto", datos = datos)
  problema_server("problema", datos = datos)
  objetivos_server("objetivos", ir_a = ir_a)
  marco_teorico_server("marco_teorico", datos = datos)
  metodologia_server("metodologia", datos = datos, modelo_info = modelo_info)
  resultados_server("resultados", datos = datos, modelo_info = modelo_info)
  prediccion_server("prediccion", datos = datos, modelo_info = modelo_info)
  limitaciones_server("limitaciones", datos = datos, modelo_info = modelo_info)
  conclusiones_server("conclusiones", datos = datos, modelo_info = modelo_info)
}

shinyApp(ui = ui, server = server)
