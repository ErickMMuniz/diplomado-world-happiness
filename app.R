# ==========================================================
# global.R  — Paquetes, datos, limpieza, funciones y modelos
# ==========================================================
options(encoding = "UTF-8")

# Paquetes
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(corrplot)
library(ggplot2)
library(reshape2)
library(rworldmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(bslib)
library(countrycode)
library(glmnet)
library(tidyr)

# ---------------------- Lectura + homologación ----------------------
leer_happiness <- function(file, year) {
  df <- read_csv(file) |> clean_names()
  if ("country_or_region" %in% names(df)) df <- df |> rename(country = country_or_region)
  df |> mutate(year = year)
}
h15 <- leer_happiness("2015.csv", 2015)
h16 <- leer_happiness("2016.csv", 2016)
h17 <- leer_happiness("2017.csv", 2017)
h18 <- leer_happiness("2018.csv", 2018)
h19 <- leer_happiness("2019.csv", 2019)

# Diccionario país–región desde 2015
Pais_region <- h15 %>% select(country, region) %>% distinct(country, .keep_all = TRUE)

merge_region <- function(df, diccionario) {
  temp <- merge(df, diccionario, by = "country", all.x = TRUE, suffixes = c(".x", ".y"))
  if ("region.x" %in% names(temp) && "region.y" %in% names(temp)) {
    temp <- temp |> rename(region = region.x) |> select(-region.y)
  } else if ("region.y" %in% names(temp)) {
    temp <- temp |> rename(region = region.y)
  } else if ("region.x" %in% names(temp)) {
    temp <- temp |> rename(region = region.x)
  }
  temp
}
h15_m <- merge_region(h15, Pais_region)
h16_m <- merge_region(h16, Pais_region)
h17_m <- merge_region(h17, Pais_region)
h18_m <- merge_region(h18, Pais_region)
h19_m <- merge_region(h19, Pais_region)

convert_auto_numeric <- function(df) {
  df %>% mutate(across(
    .cols = -c(country, region),
    .fns  = ~ {
      x <- ifelse(. %in% c("N/A", "NA", "", "NULL"), NA, .)
      type.convert(str_trim(as.character(x)), as.is = TRUE)
    }
  ))
}
h15_m <- convert_auto_numeric(h15_m)
h16_m <- convert_auto_numeric(h16_m)
h17_m <- convert_auto_numeric(h17_m)
h18_m <- convert_auto_numeric(h18_m)
h19_m <- convert_auto_numeric(h19_m)

homologar_nombres <- function(df) {
  rn <- names(df)
  equivalencias <- list(
    happiness_rank               = "rank",
    overall_rank                 = "rank",
    happiness_score              = "happiness_score",
    score                        = "happiness_score",
    economy_gdp_per_capita       = "gdp_per_capita",
    gdp_per_capita               = "gdp_per_capita",
    family                       = "social_support",
    social_support               = "social_support",
    health_life_expectancy       = "healthy_life_expectancy",
    healthy_life_expectancy      = "healthy_life_expectancy",
    freedom                      = "freedom",
    freedom_to_make_life_choices = "freedom",
    trust_government_corruption  = "corruption",
    perceptions_of_corruption    = "corruption",
    lower_confidence_interval    = "se_lower",
    upper_confidence_interval    = "se_upper",
    whisker_high                 = "whisker_high",
    whisker_low                  = "whisker_low"
  )
  equivalencias_validas <- equivalencias[names(equivalencias) %in% rn]
  df %>% rename(!!!setNames(names(equivalencias_validas), unlist(equivalencias_validas)))
}
h15_mh <- homologar_nombres(h15_m)
h16_mh <- homologar_nombres(h16_m)
h17_mh <- homologar_nombres(h17_m)
h18_mh <- homologar_nombres(h18_m)
h19_mh <- homologar_nombres(h19_m)

# Dystopia ausente en 2018–2019 → crear y luego recalcular
h18_mh <- h18_mh %>% mutate(dystopia_residual = 0)
h19_mh <- h19_mh %>% mutate(dystopia_residual = 0)

seleccionar_columnas <- function(Datos){
  Datos %>%
    select(country, region, rank, happiness_score, gdp_per_capita, social_support,
           healthy_life_expectancy, freedom, corruption, generosity, dystopia_residual, year)
}
h15_mh <- seleccionar_columnas(h15_mh)
h16_mh <- seleccionar_columnas(h16_mh)
h17_mh <- seleccionar_columnas(h17_mh)
h18_mh <- seleccionar_columnas(h18_mh)
h19_mh <- seleccionar_columnas(h19_mh)

Data <- bind_rows(h15_mh, h16_mh, h17_mh, h18_mh, h19_mh)

# Imputación por promedio histórico del país y recalcular dystopia si faltaba
imputar_na_promedio_pais <- function(Datos,
                                     vars_imputar = c("gdp_per_capita","social_support","healthy_life_expectancy",
                                                      "freedom","corruption","generosity","dystopia_residual")) {
  Datos %>% group_by(country) %>%
    mutate(across(all_of(vars_imputar), ~ { m <- mean(., na.rm = TRUE); ifelse(is.na(.), m, .) })) %>%
    ungroup()
}
Data <- imputar_na_promedio_pais(Data)

recalcular_dystopia <- function(df) {
  df %>% mutate(
    dystopia_residual = ifelse(dystopia_residual == 0 | is.na(dystopia_residual),
                               happiness_score - (gdp_per_capita + social_support + healthy_life_expectancy + freedom + generosity + corruption),
                               dystopia_residual)
  )
}
Data <- recalcular_dystopia(Data)

# Features útiles
Data <- Data %>% mutate(
  gdp_log = log1p(gdp_per_capita),
  iso3    = countrycode(country, "country.name", "iso3c")
)

# ---------------------- Funciones auxiliares ----------------------
plot_ts_paises <- function(df, paises) {
  df_paises <- df[df$country %in% paises, , drop = FALSE]
  ggplot(df_paises, aes(x = year, y = happiness_score, color = country)) +
    geom_line(size = 1.2) + geom_point(size = 3) +
    geom_text(aes(label = round(happiness_score, 2)), vjust = -0.8, size = 3.8,
              fontface = "bold", show.legend = FALSE) +
    scale_x_continuous(breaks = sort(unique(df_paises$year))) +
    labs(title = "Evolución del Happiness Score por país", x = "Año", y = "Happiness Score", color = "País") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "gray40"), plot.title = element_text(face = "bold"))
}
plot_box_region <- function(df) {
  df %>% mutate(region = reorder(region, happiness_score, median, na.rm = TRUE)) %>%
    ggplot(aes(x = region, y = happiness_score, fill = region)) +
    geom_boxplot(alpha = 0.8, outlier.color = "black") +
    coord_flip() + scale_fill_brewer(palette = "Set3") +
    labs(title = "Distribución del Happiness Score por región", x = "Región", y = "Happiness Score") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "gray40"), plot.title = element_text(face = "bold", size = 16))
}

# Cambios ranking
cambios_rank <- Data %>% group_by(country) %>%
  summarise(rank_primero = rank[year == min(year)][1],
            rank_ultimo  = rank[year == max(year)][1],
            delta_rank   = rank_primero - rank_ultimo, .groups = "drop") %>%
  filter(!is.na(rank_primero), !is.na(rank_ultimo)) %>% arrange(desc(delta_rank))

# ---------------------- Objetos de mapa ----------------------
Data_iso <- Data %>% mutate(iso3 = countrycode(country, "country.name", "iso3c"))
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% mutate(iso3 = iso_a3)

# ---------------------- Modelado (LASSO con CV) ----------------------
preds <- c("gdp_log","social_support","healthy_life_expectancy","freedom","generosity","corruption","region")

Data_model <- Data %>%
  select(happiness_score, all_of(preds)) %>%
  mutate(region = factor(region)) %>%
  filter(!is.na(happiness_score)) %>%
  drop_na(all_of(setdiff(preds,"region"))) %>%
  drop_na(region)

Xm <- model.matrix(
  happiness_score ~ gdp_log + social_support + healthy_life_expectancy +
    freedom + generosity + corruption + region,
  data = Data_model
)[, -1]
y <- Data_model$happiness_score
stopifnot(nrow(Xm) == length(y))

set.seed(123)
cvfit <- cv.glmnet(Xm, y, alpha = 1, nfolds = 10, standardize = TRUE)
lambda_min <- cvfit$lambda.min
lambda_1se <- cvfit$lambda.1se
coef_min <- as.matrix(coef(cvfit, s = "lambda.min"))
coef_1se <- as.matrix(coef(cvfit, s = "lambda.1se"))

# ---------------------- Parámetros PCA ----------------------
pca_vars <- c("gdp_log","social_support","healthy_life_expectancy","freedom","generosity","corruption")

# ==========================================================
# server.R — Lógica de servidor (usa objetos de global.R)
# ==========================================================
server <- function(input, output, session) {
  
  # -------- Resultados Generales --------
  top10_felices <- reactive({
    Data %>% filter(!is.na(happiness_score)) %>%
      group_by(country, region) %>%
      summarise(Felicidad_promedio = mean(happiness_score, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Felicidad_promedio)) %>% slice(1:10) %>%
      mutate(Felicidad_promedio = round(Felicidad_promedio, 2))
  })
  output$tabla_top10 <- renderTable(top10_felices())
  output$plot_top10 <- renderPlot({
    df <- top10_felices()
    ggplot(df, aes(x=reorder(country, Felicidad_promedio), y=Felicidad_promedio, fill=region)) +
      geom_col(width=.7, alpha=.9) +
      geom_text(aes(label=Felicidad_promedio), hjust=-.1, size=4) +
      coord_flip() + expand_limits(y = max(df$Felicidad_promedio)+.3) +
      labs(title="Top 10 pa�ses m�s felices (promedio 2015 - 2019)", x=NULL, y="Happiness Score", fill="Regi�n") +
      theme_minimal(base_size=13) + theme(legend.position="bottom", panel.grid.major.y=element_blank())
  })
  
  bottom10_felices <- reactive({
    Data %>% filter(!is.na(happiness_score)) %>% group_by(country, region) %>%
      summarise(Felicidad_promedio = mean(happiness_score, na.rm = TRUE), .groups = "drop") %>%
      arrange(Felicidad_promedio) %>% slice(1:10) %>%
      mutate(Felicidad_promedio = round(Felicidad_promedio, 2))
  })
  output$tabla_bottom10 <- renderTable(bottom10_felices())
  output$plot_bottom10 <- renderPlot({
    df <- bottom10_felices()
    ggplot(df, aes(x=reorder(country, Felicidad_promedio), y=Felicidad_promedio, fill=region)) +
      geom_col(width=.7, alpha=.9) + geom_text(aes(label=Felicidad_promedio), hjust=-.1, size=4) +
      coord_flip() + expand_limits(y = max(df$Felicidad_promedio)+.3) +
      labs(title="Top 10 países menos felices (promedio 2015–2019)", x=NULL, y="Happiness Score", fill="Región") +
      theme_minimal(base_size=13) + theme(legend.position="bottom", panel.grid.major.y=element_blank())
  })
  
  output$tabla_mejoran_rank  <- renderTable(cambios_rank %>% slice_head(n=10))
  output$tabla_empeoran_rank <- renderTable(cambios_rank %>% slice_tail(n=10))
  
  output$corrplot_global <- renderPlot({
    vars_cor <- c("happiness_score","gdp_per_capita","social_support","healthy_life_expectancy",
                  "freedom","corruption","generosity","dystopia_residual")
    mat_cor <- Data %>% select(all_of(vars_cor)) %>% cor(use="complete.obs")
    corrplot(mat_cor, method="color", type="upper", addCoef.col="black", tl.col="black", tl.srt=45)
  })
  
  # -------- Comparar países --------
  output$ts_paises_plot <- renderPlot({ req(input$paises); plot_ts_paises(Data, input$paises) })
  output$selector_regiones <- renderUI({
    req(!input$todas_regiones)
    selectInput("regiones","Selecciona regiones:", choices=sort(unique(Data$region)), multiple=TRUE)
  })
  datos_box_region <- reactive({
    df <- Data %>% filter(year == input$year_box)
    if (!input$todas_regiones) { req(input$regiones); df <- df %>% filter(region %in% input$regiones) }
    df
  })
  output$boxplot_region <- renderPlot({ req(nrow(datos_box_region()) > 0); plot_box_region(datos_box_region()) })
  
  # -------- Mapa mundial --------
  datos_mapa <- reactive({
    Data_iso %>%
      filter(year == input$year) %>%
      group_by(iso3) %>%
      summarise(happiness_score = mean(happiness_score, na.rm = TRUE), .groups = "drop")
  })
  world_year <- reactive({ world %>% left_join(datos_mapa(), by = "iso3") })
  output$mapa_felicidad <- renderPlot({
    df_map <- world_year()
    mid_val <- mean(df_map$happiness_score, na.rm = TRUE)
    ggplot(df_map) +
      geom_sf(aes(fill = happiness_score), color = "gray40", size = 0.1) +
      scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=mid_val,
                           na.value="lightgray", name="Happiness\nScore") +
      labs(title = paste("Happiness Score por país -", input$year), x=NULL, y=NULL) +
      theme_minimal() + theme(panel.grid = element_blank(), legend.position = "right")
  })
  
  # -------- PCA (reactivo) --------
  pca_fit <- reactive({
    df_pca <- if (isTRUE(input$pca_all_years)) Data else dplyr::filter(Data, year == input$pca_year)
    X <- df_pca %>% dplyr::select(all_of(pca_vars)) %>% tidyr::drop_na(all_of(pca_vars)) %>%
      scale(center = TRUE, scale = TRUE)
    prcomp(X, center = FALSE, scale. = FALSE)
  })
  pca_scores <- reactive({
    fit <- pca_fit()
    df_meta <- if (isTRUE(input$pca_all_years)) Data else dplyr::filter(Data, year == input$pca_year)
    df_meta <- df_meta %>% dplyr::select(country, region, year, dplyr::all_of(pca_vars)) %>%
      tidyr::drop_na(dplyr::all_of(pca_vars))
    as.data.frame(fit$x) %>% dplyr::bind_cols(df_meta %>% dplyr::select(country, region, year))
  })
  output$pca_scree <- renderPlot({
    fit <- pca_fit(); ve <- fit$sdev^2; prop <- ve/sum(ve)
    plot(seq_along(prop), prop, type = "b",
         xlab = "Componente", ylab = "Proporción de varianza", main = "Scree plot")
  })
  output$pca_scatter <- renderPlot({
    sc <- pca_scores() %>% dplyr::filter(year == input$pca_year)
    ggplot(sc, aes(PC1, PC2, color = region)) +
      geom_point(alpha = .9) +
      theme_minimal() +
      labs(title = paste("PC1 vs PC2 — 2019", input$pca_year), x = "PC1", y = "PC2", color = "Región")
  })
  output$pca_loadings_tbl <- renderTable({ round(pca_fit()$rotation, 3) }, rownames = TRUE)
  
  # -------- Modelo (CV) --------
  output$plot_cv_glmnet <- renderPlot({ plot(cvfit) })
  output$cv_summary <- renderText({
    paste0("MSE-CV (media ± sd): ", round(mean(cvfit$cvm), 3), " ± ", round(sd(cvfit$cvm), 3),
           "\nλ.min = ", signif(lambda_min, 3),
           "\nλ.1se = ", signif(lambda_1se, 3))
  })
  output$coef_1se_tbl <- renderTable({
    data.frame(Parametro = rownames(coef_1se), Coef = round(as.numeric(coef_1se), 4)) %>%
      dplyr::filter(Parametro != "(Intercept)")
  })
  output$coef_min_tbl <- renderTable({
    data.frame(Parametro = rownames(coef_min), Coef = round(as.numeric(coef_min), 4)) %>%
      dplyr::filter(Parametro != "(Intercept)")
  })
}


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
      # br(),
      # h4("Coeficientes ??.1se"), tableOutput("coef_1se_tbl"),
      # h4("Coeficientes ??.min"), tableOutput("coef_min_tbl"),
      br(),
      h3("Conclusiones del modelo LASSO"),
      p("El modelo LASSO obtiene un error de validación cruzada cercano a 0.37, lo que indica 
       que, pese a la variabilidad propia de datos sociales, logra capturar de manera razonable 
       los determinantes del Happiness Score."),
      
      p("En ambos ajustes, los predictores más relevantes son el ingreso (gdp_log), 
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



shinyApp(ui = ui, server = server)