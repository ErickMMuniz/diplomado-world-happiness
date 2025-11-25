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
        p("Este dashboard utiliza los World Happiness Reports 2015 al 2019."),
        p("El World Happiness Report es un estudio internacional que analiza el bienestar subjetivo 
   de más de 150 países mediante encuestas del Gallup World Poll. Su objetivo es identificar 
   qué dimensiones económicas y sociales explican por qué algunas naciones reportan mayores 
   niveles de satisfacción con la vida que otras."),
        
        p("El puntaje de felicidad (Happiness Score) se construye a partir de la valoración que las 
   personas hacen de su propia vida en una escala del 0 al 10. A modo explicativo, el reporte 
   descompone este puntaje en varios factores relacionados con calidad de vida, capital social 
   y funcionamiento institucional."),
        
        p("Este dashboard integra los datos de los reportes 2015-2019 y permite explorar la evolución 
   del bienestar a escala global, así como las diferencias entre países y regiones. La 
   visualización facilita identificar patrones, contrastar situaciones regionales y analizar 
   cómo los determinantes sociales, económicos y políticos influyen en la percepción de 
   felicidad de las poblaciones."),
        
        
        h3("Variables principales"),
        tags$ul(
          tags$li(
            strong("country:"), 
            " nombre del país incluido en el World Happiness Report."
          ),
          tags$li(
            strong("region:"), 
            " agrupación geográfica a la que pertenece el país (por ejemplo, Western Europe, Latin America and Caribbean, etc.)."
          ),
          tags$li(
            strong("year:"), 
            " año del reporte considerado en la base integrada (2015-2019)."
          ),
          tags$li(
            strong("happiness_score:"), 
            " puntaje de satisfacción con la vida de 0 a 10, basado en la escala de la 'Cantril Ladder' que responde la población encuestada."
          ),
          tags$li(
            strong("gdp_per_capita:"), 
            " contribución del PIB per cápita (en términos ajustados por paridad de poder de compra) al nivel de felicidad; representa el nivel material de vida."
          ),
          tags$li(
            strong("social_support:"), 
            " medida de apoyo social percibido: refleja la proporción de personas que declaran tener a alguien (familia, amigos o redes cercanas) en quien apoyarse en momentos difíciles."
          ),
          tags$li(
            strong("healthy_life_expectancy:"), 
            " contribución de la esperanza de vida saludable al puntaje de felicidad; captura tanto la longevidad como la calidad de salud."
          ),
          tags$li(
            strong("freedom:"), 
            " percepción de libertad para tomar decisiones importantes en la vida (por ejemplo, elegir trabajo, lugar de residencia o estilo de vida)."
          ),
          tags$li(
            strong("generosity:"), 
            " aproximación a la generosidad medida a partir de donaciones y conductas prosociales reportadas, ajustadas por nivel de ingreso."
          ),
          tags$li(
            strong("corruption:"), 
            " percepción de ausencia de corrupción en gobierno y empresas; valores más altos implican mayor confianza institucional."
          ),
          tags$li(
            strong("dystopia_residual:"), 
            " componente de referencia y parte no explicada del modelo: representa cuánto del Happiness Score se atribuye al país hipotético 'Dystopia' más el residuo estadístico."
          )
        ),
        h3("Alcance del dashboard"),
        tags$ul(
          tags$li("Evoluciónn del Happiness Score por país"),
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
      p("Se muestra el promedio del Happiness Score por país y región, ordenado de mayor a menor. 
   Los países mejor posicionados pertenecen principalmente a Europa Occidental y los países nórdicos, 
   donde se combinan altos niveles de apoyo social, instituciones confiables y políticas públicas que 
   garantizan acceso amplio a salud, educación y seguridad económica. Esta combinación de estabilidad, 
   cohesión social y calidad de vida se refleja en evaluaciones de bienestar consistentemente superiores 
   a las de otras regiones."),
      br(),
      h3("Top 10 países menos felices (promedio)"),
      plotOutput("plot_bottom10", height = 350),
      tableOutput("tabla_bottom10"),
      p("Los países que aparecen con los niveles más bajos de felicidad comparten desafíos estructurales profundos.
        En varias naciones del Árica Subsahariana y regiones afectadas por conflicto,
        la inestabilidad política, la falta de servicios básicos y los bajos ingresos limitan
        significativamente el bienestar cotidiano. Factores como acceso insuficiente a salud,
        inseguridad alimentaria, tensiones étnicas o territoriales, instituciones frágiles y
        oportunidades económicas reducidas influyen directamente en las percepciones de vida de la población."),
      p("En este contexto, la felicidad reportada refleja menos una valoración subjetiva aislada y más la acumulación de condiciones adversas persistentes.
        La evidencia muestra que, sin estabilidad, servicios públicos funcionales ni redes sólidas de protección social,
        resulta difícil que las personas evalúen su vida de forma positiva, aun cuando existan esfuerzos locales de resiliencia o apoyo comunitario."),
      br(),
      h3("Países con mayor cambio en el ranking"),
      p("Se muestra la variación en la posición de cada país entre el primer y el último año de la muestra. 
       Un delta positivo indica que el país subió posiciones en el ranking (mejoró su lugar relativo); 
       un delta negativo indica que perdió posiciones."),
      h4("Mejoran"), tableOutput("tabla_mejoran_rank"),
      h4("Empeoran"), tableOutput("tabla_empeoran_rank"),
      p("Venezuela destaca como el país con el mayor deterioro en el ranking de felicidad durante el periodo analizado. 
   Su caída está asociada a una combinación de inestabilidad económica, inflación persistente, contracción del ingreso 
   real, deterioro en servicios públicos esenciales y un contexto político complejo. Estos factores han afectado 
   directamente la percepción de bienestar de la población, reflejándose en una disminución sostenida en su posición 
   relativa frente al resto del mundo."),
      br(),
      h3("Correlaciones entre variables"),
      p("El siguiente correlograma muestra la relación lineal entre el Happiness Score 
       y sus principales componentes explicativos: ingreso, apoyo social, salud, libertad, 
       generosidad, corrupción y el componente de distopía residual."),
      plotOutput("corrplot_global", height = 450),
      p("Dado que el Happiness Score es la suma de sus componentes, las correlaciones más altas 
   (PIB per cápita, esperanza de vida saludable y apoyo social) reflejan directamente su peso 
   dentro del modelo del World Happiness Report. Los factores con menor contribución, como la 
   generosidad, muestran asociaciones más débiles. Además, varios de los componentes están 
   fuertemente correlacionados entre sí -como ingreso, salud y apoyo social- lo que indica que 
   estos elementos del bienestar suelen avanzar juntos a nivel país.")
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
          plotOutput("mapa_felicidad", height = 500),
          br(),
          p("El mapa mundial permite visualizar rápidamente la distribución geográfica del Happiness Score. 
          Los tonos más altos de felicidad se concentran en Europa Occidental y los países nórdicos, un patrón que 
          coincide con los resultados ya mostrados en la sección de Resultados Generales. En contraste, buena parte 
          de África Subsahariana y algunas regiones de Medio Oriente presentan los niveles más bajos de bienestar. 
          Esta representación espacial complementa el análisis previo y confirma que la felicidad tiende a agruparse 
          en regiones con mayor estabilidad institucional, cohesión social y calidad de vida."),
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
               selectInput("pca_year", "Año para PC1 - PC2:", choices = sort(unique(Data$year)),
                           selected = max(Data$year)),
               checkboxInput("pca_all_years", "Ajustar PCA con todos los años", TRUE)
        ),
        column(8, p("Variables: gdp_log, social_support, healthy_life_expectancy, freedom, generosity, corruption."))
      ),
      hr(),
      h4("Varianza explicada (Scree)"),
      p("El Análisis de Componentes Principales permite combinar los seis factores del World Happiness Report 
       en indicadores sintéticos (PC1, PC2, etc.) que capturan la mayor parte de la variación del bienestar entre países. 
       Aunque los componentes no agregan información nueva, sí facilitan visualizar patrones globales al reducir 
       la dimensionalidad del conjunto de variables."),
      plotOutput("pca_scree", height = 250),
      br(),
      h4("PC1 - PC2 por región"),
      plotOutput("pca_scatter", height = 420),
      p("En el plano PC1-PC2 se observan agrupamientos claros entre regiones. 
       Europa Occidental, Norteamérica y Australia/Nueva Zelanda presentan valores altos 
       en PC1, indicando simultáneamente mayor ingreso, salud, apoyo social y menor corrupción. 
       África Subsahariana se ubica en la zona opuesta, reflejando rezagos estructurales en esos mismos factores. 
       América Latina y partes de Asia aparecen en posiciones intermedias, pero dispersas en PC2, lo que indica 
       diferencias en cohesión social, confianza institucional o generosidad."),
      br(),
      h4("Loadings (tabla)"),
      tableOutput("pca_loadings_tbl"),
      p("Los loadings permiten interpretar cada componente:"),
      tags$ul(
        tags$li(strong("PC1:"), 
                " combina principalmente ingreso (gdp_log), apoyo social, esperanza de vida saludable, libertad 
               y baja corrupción. Es un eje de bienestar estructural que distingue claramente a los países 
               más desarrollados."),
        tags$li(strong("PC2:"), 
                " captura variación asociada a generosidad, corrupción y libertad, diferenciando matices en 
               capital social e instituciones entre países con niveles de desarrollo similares.")
      ),
      
      br(),
      h4("Conclusión general del PCA"),
      p("El PCA confirma que la felicidad no depende de un solo factor aislado, sino de la combinación de 
       desarrollo económico, salud, apoyo social y calidad institucional. Los dos primeros componentes 
       resumen eficientemente ese patrón y permiten visualizar cómo se agrupan las regiones según su perfil 
       de bienestar, reforzando los hallazgos mostrados previamente en tablas, boxplots y mapas."),
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
      h4("Coeficientes ??.1se"), tableOutput("coef_1se_tbl"),
      h4("Coeficientes ??.min"), tableOutput("coef_min_tbl"),
      br(),
      h3("Conclusiones del modelo LASSO"),
      p("El modelo LASSO obtiene un error de validaci�n cruzada cercano a 0.37, lo que indica 
       que, pese a la variabilidad propia de datos sociales, logra capturar de manera razonable 
       los determinantes del Happiness Score."),
      
      p("En ambos ajustes (??.min y ??.1se), los predictores más relevantes son el ingreso (gdp_log), 
       la libertad, la ausencia de corrupción, la expectativa de vida saludable y el apoyo social, 
       todos con coeficientes positivos. Es decir, mejores condiciones materiales, instituciones 
       más sólidas y redes de apoyo más fuertes se asocian sistemáticamente con mayor felicidad."),
      
      p("Las variables de región muestran que, controlando por estos factores, algunas áreas como 
       América Latina y Norteamérica presentan niveles de felicidad ligeramente superiores a lo 
       esperado, mientras que regiones como Asia Meridional, Sudeste Asiático y África Subsahariana 
       se ubican por debajo. El modelo confirma así un patrón regional consistente con el análisis 
       descriptivo del dashboard.")
    )
  )
)

