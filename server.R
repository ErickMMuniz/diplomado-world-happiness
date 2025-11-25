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
