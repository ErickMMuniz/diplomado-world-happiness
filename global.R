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
