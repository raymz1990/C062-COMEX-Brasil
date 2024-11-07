# 1ª Pagina (Overview) ----

## Gráficos ----

### Gráfico de Linhas ----
generate_region_evolution_plot <- function(data, measure) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # Agrupar dados por Ano e Nome_Regiao, somando tanto o valor quanto o peso
  line_data <- data %>%
    group_by(Ano, Nome_Regiao) %>%
    summarise(
      US_FOB = sum(US_FOB, na.rm = TRUE),         # Valor exportado
      Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),  # Peso exportado
      Measure = sum(.data[[measure_column]], na.rm = TRUE),  # Medida selecionada
      .groups = "drop"
    )
  
  # Definir cores por região
  Cor <- data %>%
    select(Nome_Regiao, Cor) %>%
    distinct() %>%
    arrange(Nome_Regiao)
  
  # Formatação do eixo Y
  y_formatter <- if (measure == "value") {
    JS("function() {
          if (this.value >= 1e6) {
            return (this.value / 1e9).toFixed(0) + ' bi';
          } else if (this.value >= 1e3) {
            return (this.value / 1e6).toFixed(0) + ' mi';
          } else {
            return this.value + ' ';
          }
        }")
  } else {
    JS("function() {
          if (this.value >= 1e9) {
            return (this.value / 1e9).toFixed(0) + ' bi';
          } else if (this.value >= 1e6) {
            return (this.value / 1e6).toFixed(0) + ' mi';
          } else if (this.value >= 1e3) {
            return (this.value / 1e3).toFixed(0) + ' mil';
          } else {
            return this.value;
          }
        }")
  }
  
  # Definir títulos e tooltip
  title_measure <- ifelse(measure == "value", "Valor Exportado (US$)", "Peso Exportado (Toneladas)")
  title_title <- paste0("Exportações por Ano e Região por ", title_measure)
  
  # Tooltip formatado para exibir valor exportado e peso
  tooltip_text <- JS(
    "function() {
      var y = this.y;
      var value = this.point.US_FOB;  // Valor exportado
      var weight = this.point.Peso_Liquido;  // Peso exportado
      var formattedY, formattedValue, formattedWeight;

      // Formatação do valor 'y'
      if (y >= 1e9) {
        formattedY = (y / 1e9).toFixed(1) + ' bi';
      } else if (y >= 1e6) {
        formattedY = (y / 1e6).toFixed(1) + ' mi';
      } else if (y >= 1e3) {
        formattedY = (y / 1e3).toFixed(1) + ' mil';
      } else {
        formattedY = y.toFixed(0);
      }
      
      // Formatação do valor exportado (US$)
      if (value >= 1e9) {
        formattedValue = (value / 1e9).toFixed(1) + ' bi';
      } else if (value >= 1e6) {
        formattedValue = (value / 1e6).toFixed(1) + ' mi';
      } else if (value >= 1e3) {
        formattedValue = (value / 1e3).toFixed(1) + ' mil';
      } else {
        formattedValue = value.toFixed(0);
      }
      
      // Formatação do peso exportado (Toneladas)
      if (weight >= 1e9) {
        formattedWeight = (weight / 1e9).toFixed(1) + ' bi';
      } else if (weight >= 1e6) {
        formattedWeight = (weight / 1e6).toFixed(1) + ' mi';
      } else if (weight >= 1e3) {
        formattedWeight = (weight / 1e3).toFixed(1) + ' mil';
      } else {
        formattedWeight = weight.toFixed(0);
      }

      return '<b>' + this.series.name + '</b><br/>' +
             '<b>Ano</b>: ' + this.x + '<br/>' +
             '<b>Valor Exportado</b>: US$ ' + formattedValue + ' <br/>' +
             '<b>Peso Exportado</b>: ' + formattedWeight + ' toneladas';
    }"
  )
  
  # Gráfico de linhas com Highchart
  highchart() %>%
    
    # Configuração inicial do gráfico
    hc_chart(type = "line",
             zoomType = "x",
             backgroundColor = "#fff") %>% 
    
    # Título
    hc_title(text = title_title) %>%
    
    # Eixo X (Ano)
    hc_xAxis(
      title = list(text = "Ano"),
      tickInterval = 1,
      gridLineColor = NULL,
      lineColor = "#999999",
      tickColor = "#999999"
    ) %>%
    
    # Eixo Y (Valor/Peso)
    hc_yAxis(
      title = list(text = title_measure),
      gridLineColor = "#999999",
      labels = list(formatter = y_formatter)
    ) %>% 
    
    # Adicionar a série de dados
    hc_add_series(
      data = line_data,
      type = "line",
      mapping = hcaes(x = Ano, y = Measure, group = Nome_Regiao),  
      lineWidth = 2,
      marker = list(
        radius = 3, 
        symbol = "circle",  
        enabled = TRUE   
      ),
      color = Cor$Cor,
      showInLegend = TRUE
    ) %>%
    
    # Tooltip personalizado
    hc_tooltip(
      useHTML = TRUE,
      formatter = tooltip_text
    ) %>%
    
    # Legenda
    hc_legend(
      enabled = TRUE,
      align = 'center',
      layout = 'horizontal'
    ) %>%
    
    # Tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

### Mapa das exportações ----
generate_export_map <- function(data) {
  
  map_data <- data %>%
    group_by(CO_Pais_ISOA3, Nome_Pais) %>%
    summarise(
      US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
      .groups = 'drop')   
  
  tooltip_text <- "<b>Valor Exportado</b>: US$ {point.value:,.1f} mi"
  
  hcmap(
    "custom/world-robinson-lowres",
    data = map_data,
    name = "Exportações",
    value = "US_FOB",
    joinBy = c("iso-a3", "CO_Pais_ISOA3"),
    borderWidth = 0,
    nullColor = "#f0f0f0"  # cor para áreas sem dados
  ) %>%
    hc_title(text = "Fluxo de Exportações") %>%
    hc_subtitle(text = "Ano: 2020") %>%
    hc_tooltip(pointFormat = paste('<b>{point.Nome_Pais}</b> <br>', tooltip_text)) %>%
    hc_legend(layout = "horizontal",
              align = "center",
              valueDecimals = 0)  %>%
    hc_colorAxis(
      stops = color_stops(colors = brewer.pal(9, "Greens")),  # ajusta as cores para usar a paleta "Greens"
      type = "logarithmic"
    ) %>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_chart(zoomType = "xy") %>% 
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}