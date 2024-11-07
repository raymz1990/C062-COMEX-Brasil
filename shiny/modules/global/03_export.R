# 3ª Pagina (por Destino) ----

## Gráfico de barras ----
generate_bar_export <- function(data, measure, block_filter, year_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_311
  top_countries <- data %>%
    group_by(Nome_Pais) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE)) %>%
    arrange(desc(Measure)) %>%
    mutate(rank = row_number()) %>%
    mutate(Nome_Pais = ifelse(rank > 10, "Demais Países", Nome_Pais)) %>%
    ungroup() %>%
    group_by(Nome_Pais) %>%
    summarise(Measure = sum(Measure, na.rm = TRUE))
  
  # "Demais Países" como a última opção
  top_countries <- top_countries %>%
    arrange(Nome_Pais == "Demais Países", desc(Measure))  
  
  # definição de cor
  top_countries <- top_countries %>%
    left_join(data %>%
                select(Nome_Pais, Nome_Bloco) %>%
                distinct(), by = "Nome_Pais") %>%
    mutate(Color = ifelse(
      Nome_Pais == "Demais Países", "#999999",
      sapply(Nome_Bloco, get_region_color))
    )
  
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
    paste("Top 10 países por ", title_measure)
  title_subtitle <-
    paste(
      "Bloco Econômico:", block_filter,
      " | Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter)
    )
  tooltip_text <- ifelse(
    measure == "value",
    "<b>Valor Exportado:</b> US$ {point.y:,.1f} mi",
    "<b>Peso Exportado:</b> {point.y:,.1f} toneladas"
  )
  
  # gráfico de barras com highchart
  highchart() %>%
    
    # configuração inicial do tipo de gráfico
    hc_chart(type = "bar",
             backgroundColor = "#fff") %>%
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    # hc_caption(text= "Fonte de dados: ") %>% 
    
    # configuração do eixo X (eixo horizontal)
    hc_xAxis(categories = top_countries$Nome_Pais,
             gridLineColor = NULL,
             lineColor = "#999999",
             tickColor = "#999999") %>%
    
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
      data = top_countries$Measure,  
      colorByPoint = TRUE,
      colors = top_countries$Color
    ) %>%
    
    # configuração do tooltip
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_text
    ) %>%
    
    # configuração da legenda
    hc_legend(enabled= FALSE) %>%
    
    # configurações avançadas
    hc_plotOptions(series = list(groupPadding = 0.05, borderWidth = 0)) %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

## Gráfico de rosca ----
generate_donut_export <- function(data, measure, block_filter, year_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_312
  pie_data <- data %>%
    group_by(Nome_Regiao) %>%
    summarise(Measure = sum(!!as.symbol(measure_column), na.rm = TRUE)) %>%
    mutate(Color = sapply(Nome_Regiao, get_region_color)) %>%
    mutate(name = Nome_Regiao, y = Measure)
  
  total_value <- round(sum(pie_data$Measure),1)
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <-
    paste("Representatividade da Região por", title_measure)
  title_subtitle <-
    paste(
      "Bloco Econômico:", block_filter,
      " | Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter)
    )
  tooltip_text <- ifelse(
    measure == "value",
    "<b>Valor Exportado:</b> US$ {point.y:,.1f} mi",
    "<b>Peso Exportado:</b> {point.y:,.1f} toneladas"
  ) 
  
  # gráfico donut com highchart
  highchart() %>%
    
    # configuração inicial do tipo de gráfico
    hc_chart(type = "pie",
             backgroundColor = "#fff") %>%  
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>% 
    
    # configuração da série de dados
    hc_add_series(
      name = title_measure,
      data = list_parse2(pie_data),
      # size = "60%",
      innerSize = "60%",
      colorByPoint = TRUE
    ) %>%
    
    
    hc_colors(
      c("#613F00",  # Centro Oeste
        "#0C3661",  # Nordeste
        "#24033D",  # Norte
        "#5C0010",  # Sudeste
        "#5C5C5C"   # Sul
      )
    ) %>% 
    
    # configuração do tooltip
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_text
    ) %>%
    
    # configuração da legenda
    hc_legend(
      enabled = TRUE,
      align = "center", 
      verticalAlign = "bottom", 
      layout = "horizontal"
      # itemStyle = list(fontWeight = "bold", color = "#000")
    ) %>%
    
    #configurações avançadas
    hc_plotOptions(
      pie = list(
        dataLabels = list(
          enabled = TRUE,
          distance = -30,
          format =  '{point.percentage:.1f}%',
          style = list(color = 'contrast', textOutline = 'none')
        ),
        showInLegend = TRUE
      )
    ) %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
  
}