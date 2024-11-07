# 4ª Pagina (por Produto) ----

## Gráfico de barras ----
generate_bar_product <- function(data, measure, product_filter, year_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_411
  product_region <- data %>%
    filter((Ano == year_filter | year_filter == "Todos") &
             (Nome_ISIC_Secao == product_filter |
                product_filter == "Todos")
    ) %>%
    group_by(Nome_Regiao, Cor) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(Measure)) 
  
  # definição de cor
  Cor <- product_region %>%
    select(Nome_Regiao, Cor) %>%
    distinct() %>%
    arrange(Nome_Regiao)
  
  # definir formatador para eixo y
  y_formatter <- if (measure == "value") {
    JS(
      "function() {
          if (this.value >= 1e6) {
            return (this.value / 1e6).toFixed(0) + ' tri';
          } else if (this.value >= 1e3) {
            return (this.value / 1e3).toFixed(0) + ' bi';
          } else {
            return this.value + ' mi';
          }
        }"
    )
  } else {
    JS(
      "function() {
          if (this.value >= 1e9) {
            return (this.value / 1e6).toFixed(0) + ' bi';
          } else if (this.value >= 1e6) {
            return (this.value / 1e6).toFixed(0) + ' mi';
          } else if (this.value >= 1e3) {
            return (this.value / 1e3).toFixed(0) + ' mil';
          } else {
            return this.value;
          }
        }"
    )
  }
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <-
    paste("Top Regiões Atividade", product_filter, "por", title_measure)
  title_subtitle <-
    paste(
      "Atividade Econômica:",       product_filter,
      "| Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter)
    )
  tooltip_text <- ifelse(
    measure == "value",
    "<b>Valor Exportado:</b> US$ {point.y:,.1f} mi",
    "<b>Peso Exportado:</b> {point.y:,.1f} toneladas"
  )
  
  # gráfico de barras com highchart
  highchart() %>%
    
    # configuração inicial do tipo de gráfico
    hc_chart(type = "bar", backgroundColor = "#fff") %>% 
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    
    # configuração do eixo X (eixo horizontal)
    hc_xAxis(
      categories = product_region$Nome_Regiao,
      gridLineColor = NULL,
      lineColor = "#999999",
      tickColor = "#999999"
    ) %>%
    
    # Configuração do eixo Y (eixo vertical)
    hc_yAxis(
      title = list(text = title_measure),
      gridLineColor = NULL,
      lineColor = "#999999",
      tickColor = "#999999",
      labels = list(formatter = y_formatter)
    ) %>%
    
    # configuração da série de dados
    hc_add_series(
      name = title_measure,
      data = product_region$Measure,
      colorByPoint = TRUE
    ) %>%
    
    # definição da cor do gráfico
    hc_colors(Cor$Cor) %>%
    
    # configuração do tooltip
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_text
    ) %>%
    
    # configuração da legenda
    hc_legend(enabled = FALSE,
              align = 'center',
              layout = 'horizontal') %>%
    
    # configurações avançadas
    hc_plotOptions(series = list(
      groupPadding = 0.05,
      borderWidth = 0
    )) %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_exporting(enabled = TRUE)
}


## Gráfico treemap ----
generate_treemap_product <- function(data, measure, year_filter,
                                     threshold = 1) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_413
  treemap_data <- data %>%
    filter((Ano == year_filter | year_filter == "Todos")
    ) %>%
    group_by(Nome_ISIC_Secao, Nome_SH2, Nome_SH4, Cor) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE),
              .groups = "drop") %>%
    mutate(Nome_SH4lvl3 = Nome_SH4) %>%
    ungroup()
  
  total_measure <- sum(treemap_data$Measure)
  
  treemap_data <- treemap_data %>%
    mutate(Percent = Measure / total_measure * 100) %>%
    mutate(Nome_SH4 = ifelse(Percent < threshold, "Outros", Nome_SH4)) %>%
    group_by(Nome_ISIC_Secao, Nome_SH2, Nome_SH4, Nome_SH4lvl3, Cor) %>%
    summarise(Measure = sum(Measure), .groups = "drop") %>%
    arrange(Nome_ISIC_Secao)
  
  treemap_data <- treemap_data %>%
    rename(value = Measure)
  
  # Configuração dos níveis para mostrar labels no nível 1
  lvl_opts <- list(
    list(
      level = 1,
      borderWidth = 0,
      borderColor = "transparent",
      dataLabels = list(
        enabled = TRUE,
        format = "{point.name}",  # Mostra nome e valor
        align = "left",
        verticalAlign = "top",
        padding = 0.5,
        style = list(
          fontSize = "12px",
          textOutline = FALSE,
          color = "white",
          fontWeight = "normal"
        )
      )
    ),
    list(
      level = 2,
      borderWidth = 1,
      borderColor = "transparent",
      colorVariation = list(key = "brightness", to = 0.500),
      dataLabels = list(enabled = FALSE),
      style = list(
        fontSize = "8px",
        textOutline = FALSE, 
        color = "white", 
        fontWeight = "normal"
      )
    ),
    list(
      level = 3,
      borderWidth = 1,
      borderColor = "white",
      colorVariation = list(key = "brightness", to = 0.250),
      dataLabels = list(enabled = FALSE),
      style = list(
        fontSize = "8px",
        textOutline = FALSE, 
        color = "white", 
        fontWeight = "normal"
      )
    ),
    list(
      level = 4,
      borderWidth = 1,
      borderColor = "white",
      colorVariation = list(key = "brightness", to = 0.125),
      dataLabels = list(enabled = FALSE),
      style = list(
        fontSize = "8px",
        textOutline = FALSE,
        color = "white",
        fontWeight = "normal"
      )
    )
  )
  
  # Definir cores com base nos blocos únicos
  Cor <- treemap_data %>%
    select(Nome_ISIC_Secao, Cor) %>%
    distinct() %>%
    arrange(Nome_ISIC_Secao)
  
  treemap_hierarchical <- data_to_hierarchical(
    treemap_data,
    c("Nome_ISIC_Secao", "Nome_SH2", "Nome_SH4", "Nome_SH4lvl3"),
    "value", 
    colors = Cor$Cor
  )
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste0(
    "Produtos Exportados por Origem e Categoria por ",
    title_measure
  )
  title_subtitle <- paste(
    "Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter)
  )
  tooltip_text <- ifelse(
    measure == "value",
    "<b>{point.name}</b><br><b>Valor Exportado:</b> US$ {point.value:,.1f} mi",
    "<b>{point.name}</b><br><b>Peso Exportado:</b> {point.value:,.1f} toneladas"
  )
  
  # gráfico de treemap com highchart
  highchart() %>%
    
    # configuração inicial do tipo de gráfico
    hc_chart(
      type = "treemap",
      layoutAlgorithm = 'squarified',
      backgroundColor = "#fff"
    ) %>%
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    
    # configuração da série de dados
    hc_add_series(
      name = title_measure,
      type = "treemap",
      allowTraversingTree = TRUE,
      levelIsConstant = FALSE,
      data = treemap_hierarchical,
      allowDrillToNode = TRUE,
      levels = lvl_opts
    ) %>%
    
    # configuração do tooltip
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_text
    ) %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}