# ==========================================================
# ui.R — Interfaz de usuario (usa objetos de global.R)
# ==========================================================
ui <- navbarPage(
  title = div(icon("smile"), "World Happiness Dashboard"),
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  position = "fixed-top",
  collapsible = TRUE,
  header = tags$head(tags$style(HTML("body { padding-top: 80px; }"))),
  
  # TAB 0: Inicio
  tabPanel(
    "Inicio",
    div(style = "padding: 20px;",
        h2("World Happiness Report - Descripción del conjunto de datos"),
        br(),
        p("Este dashboard utiliza los World Happiness Reports 2015–2019."),
        h3("Variables principales"),
        tags$ul(
          tags$li(strong("country, region, year")),
          tags$li(strong("happiness_score (0–10)")),
          tags$li(strong("gdp_per_capita, social_support, healthy_life_expectancy, freedom, generosity, corruption")),
          tags$li(strong("dystopia_residual"))
        ),
        h3("Alcance del dashboard"),
        tags$ul(
          tags$li("Evolución del Happiness Score por país"),
          tags$li("Comparación por regiones"),
          tags$li("Mapa mundial"),
          tags$li("PCA y modelo con validación cruzada")
        ),
        br(), br(),
        tags$small("Fuente: World Happiness Report (Gallup World Poll).")
    )
  ),
  
  # TAB 1: Resultados Generales
  tabPanel(
    "Resultados Generales",
    div(
      style = "padding: 50px;",
      h2("Resultados generales (2015–2019)"),
      h3("Top 10 países más felices (promedio)"),
      plotOutput("plot_top10", height = 350),
      tableOutput("tabla_top10"),
      br(),
      h3("Top 10 países menos felices (promedio)"),
      plotOutput("plot_bottom10", height = 350),
      tableOutput("tabla_bottom10"),
      br(),
      h3("Países con mayor cambio en el ranking"),
      h4("Mejoran"), tableOutput("tabla_mejoran_rank"),
      h4("Empeoran"), tableOutput("tabla_empeoran_rank"),
      br(),
      h3("Correlaciones entre variables"),
      plotOutput("corrplot_global", height = 450)
    )
  ),
  
  # TAB 2: Comparar países
  tabPanel(
    "Comparar países",
    div(
      style = "padding: 50px;",
      sidebarLayout(
        sidebarPanel(
          h4("Serie de tiempo por país"),
          selectInput(
            inputId = "paises",
            label   = "Selecciona países:",
            choices = sort(unique(Data$country)),
            selected = c("Mexico", "United States"),
            multiple = TRUE
          ),
          tags$hr(),
          h4("Distribución por región"),
          selectInput(
            inputId = "year_box",
            label   = "Año:",
            choices = sort(unique(Data$year)),
            selected = max(Data$year)
          ),
          checkboxInput("todas_regiones", "Mostrar todas las regiones", value = TRUE),
          uiOutput("selector_regiones")
        ),
        mainPanel(
          plotOutput("ts_paises_plot", height = 350),
          br(), br(),
          plotOutput("boxplot_region", height = 450)
        )
      )
    )
  ),
  
  # TAB 3: Mapa mundial
  tabPanel(
    "Mapa mundial",
    div(
      style = "padding: 70px;",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "year",
            label   = "Año:",
            choices  = sort(unique(Data_iso$year)),
            selected = max(Data_iso$year)
          )
        ),
        mainPanel(
          plotOutput("mapa_felicidad", height = 500)
        )
      )
    )
  ),
  
  # TAB 4: PCA
  tabPanel(
    "PCA",
    div(
      style = "padding: 50px;",
      h3("Componentes principales de factores (estandarizados)"),
      fluidRow(
        column(4,
               selectInput("pca_year", "Año para PC1–PC2:", choices = sort(unique(Data$year)),
                           selected = max(Data$year)),
               checkboxInput("pca_all_years", "Ajustar PCA con todos los años", TRUE)
        ),
        column(8, p("Variables: gdp_log, social_support, healthy_life_expectancy, freedom, generosity, corruption."))
      ),
      hr(),
      h4("Varianza explicada (Scree)"),
      plotOutput("pca_scree", height = 250),
      br(),
      h4("PC1–PC2 por región"),
      plotOutput("pca_scatter", height = 420),
      br(),
      h4("Loadings (tabla)"),
      tableOutput("pca_loadings_tbl")
    )
  ),
  
  # TAB 5: Modelo (CV)
  tabPanel(
    "Modelo (CV)",
    div(
      style = "padding: 50px;",
      h3("LASSO con validación cruzada (k=10)"),
      fluidRow(
        column(6, h4("Curva de CV"), plotOutput("plot_cv_glmnet", height = 300)),
        column(6, h4("Resumen"), verbatimTextOutput("cv_summary"))
      ),
      br(),
      h4("Coeficientes λ.1se"), tableOutput("coef_1se_tbl"),
      h4("Coeficientes λ.min"), tableOutput("coef_min_tbl")
    )
  )
)
