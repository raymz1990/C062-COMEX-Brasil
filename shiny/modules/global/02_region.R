

# 2ª Pagina (Por Região) ----

## Gráficos ----

### Dependency Wheel ----
generate_wheel_region <- function(data, measure, region_filter, year_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")

  # base de dados g_543a
  wheel_data <- data %>%
    group_by(Nome_Regiao, Nome_Bloco) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE),
              .groups = "drop")

  connections <- data %>%
    transmute(from = Nome_Regiao, to = Nome_Bloco, weight = .data[[measure_column]], Ano = Ano)

  Cor1 <- data %>%
    select(Nome_Regiao) %>%
    distinct() %>%
    mutate(Cor = sapply(Nome_Regiao, function(x)
      get_color("region", x))) %>%
    rename(Nome = Nome_Regiao)

  Cor2 <- data %>%
    select(Nome_Bloco) %>%
    distinct() %>%
    mutate(Cor = sapply(Nome_Bloco, function(x)
      get_color("region", x))) %>%
    rename(Nome = Nome_Bloco)

  Cor <- bind_rows(Cor1, Cor2) %>%
    arrange()

  color_map <- setNames(Cor$Cor, Cor$Nome)

  nodes <- unique(c(connections$from, connections$to)) |>
    lapply(function(x) {
      list(
        id = x,
        color = ifelse(!is.null(color_map[x]), color_map[x], "gray"),
        name = gsub("\\d+\\.\\s?", "", x)
      )
    })

  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- "Relações de Exportação entre Regiões e Blocos Econômicos"
  title_subtitle <-
    paste(
      "Região:", region_filter,
      " | Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter)
    )
  tooltip_text <- paste(
      "<b>{point.from}</b> → <b>{point.to}</b> <br>",
      "<b> Ano </b>: {point.Ano} <br>",
      ifelse(measure == "value",
        "<b> Valor Exportado </b>: US$ {point.weight:,.1f} mi",
        "<b> Peso Exportado </b>: {point.weight:,.1f} toneladas"
      )
    )
  tooltip_node <- paste(
    "<b>{point.name}</b> <br/>",
    ifelse(measure == "value",
           "<b> Valor Exportado </b>: US$ {point.sum:,.1f} mi",
           "<b> Peso Exportado </b>: {point.sum:,.1f} toneladas"
    )
  )

  # gráfico de roda com highchart
  highchart() %>%

    # configuração inicial do tipo de gráfico
    hc_chart(type = "dependencywheel", backgroundColor = "white") %>%

    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%

    # configuração da série de dados
    hc_add_series(
      data = connections,
      name = "Exportações",
      hcaes(from = from, to = to, weight = weight),
      type = "dependencywheel",
      nodes = nodes
    ) %>%

    # configuração do tooltip
    hc_tooltip(useHTML = TRUE,
               pointFormat = tooltip_text,
               nodeFormat = tooltip_node) %>%

    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

### Donut por região ----
generate_donut_region <- function(data, measure, region_filter, year_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "peso")
  
  # base de dados g_212
  donut_region <- data %>%
    group_by(Nome_Estado) %>%
    summarise(Measure = sum(!!as.symbol(measure_column), na.rm = TRUE)) %>%
    arrange(desc(Measure)) %>% 
    mutate(name = Nome_Estado, y = Measure)
  
  # ordem da legenda
  legend_order <- donut_region %>% arrange(name)
  
  # definição de cores
  colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Dark2"))(n_distinct(donut_region$name))
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <-
    paste("Representatividade Estado por ", title_measure)
  title_subtitle <-
    paste(
      "Região:", region_filter,
      " | Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter)
    )
  tooltip_text <- ifelse(
    measure == "value",
    "<b>Valor Exportado:</b> US$ {point.y:,.1f} mi",
    "<b>Peso Exportado:</b> {point.y:,.1f} toneladas"
  ) 
  
  # gráfico donut com highchart
  highchart() %>%
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>% 
    
    # configuração inicial do tipo de gráfico
    hc_chart(type = "pie",
             backgroundColor = "#fff") %>%
    
    # configuração da série de dados
    hc_add_series(
      name = title_measure,
      data = list_parse2(donut_region),
      innerSize = "60%",
      colorByPoint = TRUE,
      colors = colors
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
          format = '{point.percentage:.1f}%',
          style = list(color = 'contrast', textOutline = 'none')
        ),
        showInLegend = TRUE
      )
    ) %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}