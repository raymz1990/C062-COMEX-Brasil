# 5ª Pagina ----

## por Municipio ----

### Mapa principais destinos ----
generate_conection_city <- function(data, measure, year_filter, city_filter) {
  measure_column <-
    ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_512
  top_destinations <- data %>%
    filter(Ano == year_filter | year_filter == "Todos") %>%
    group_by(Cidade, Nome_Pais, Lat_Municipio, Long_Municipio, Lat_PAIS, Long_PAIS) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(Measure)) %>%
    slice(1:10)
  
  # mapa mundial em formato GeoJSON
  worldgeojson <- giscoR::gisco_countries %>%
    geojsonio::geojson_json()
  
  # criando ponto de origem como objeto sf
  src_point_sf <-
    top_destinations %>%
    summarise(
      name = city_filter,
      Measure = sum(Measure),
      lat = first(Lat_Municipio),
      lon = first(Long_Municipio)
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = "WGS84")
  
  # criando pontos de destino como objeto sf
  dest_points_sf <-
    top_destinations %>%
    st_as_sf(coords = c("Long_PAIS", "Lat_PAIS"), crs = "WGS84") %>%
    select(name = Nome_Pais, Measure)
  
  # criando linhas de conexão entre origem e cada destino
  connections_sf <-
    lapply(st_geometry(dest_points_sf),
           function(to)
             st_union(c(st_geometry(src_point_sf)[[1]], to)) %>% st_cast("LINESTRING")) %>%
    st_as_sfc(crs = st_crs(dest_points_sf)) %>%
    st_sf(geometry = .) %>%
    st_segmentize(dfMaxLength = 1000 * 1000)
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste0(
    "Principais Destinos de Exportação de ", city_filter, " por ",
    title_measure
  )
  title_subtitle <- paste( 
    "Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter)
  )
  tooltip_text <- if (measure == "value") {
    JS("function() {
        var measure = this.point.properties.Measure;
        if (measure >= 1e6) {
          return '<b>Cidade</b>: ' + this.point.name + '<br><b>Valor Exportado</b>: US$ ' + Highcharts.numberFormat(measure / 1e6, 1, ',', '.') + ' mi';
        } else if (measure >= 1e3) {
          return '<b>Cidade</b>: ' + this.point.name + '<br><b>Valor Exportado</b>: US$ ' + Highcharts.numberFormat(measure / 1e3, 1, ',', '.') + ' mil';
        } else {
          return '<b>Cidade</b>: ' + this.point.name + '<br><b>Valor Exportado</b>: US$ ' + Highcharts.numberFormat(measure, 0, ',', '.');
        }
      }")
  } else {
    JS("function() {
        var measure = this.point.properties.Measure;
        return '<b>Cidade</b>: ' + this.point.name + '<br><b>Peso Exportado</b>: ' + Highcharts.numberFormat(measure, 1, ',', '.') + ' toneladas';
      }")
  }
  
  # gráfico de mapa com highchart
  highchart(type = "map") %>%
    hc_mapNavigation(enabled = TRUE) %>%
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    
    # configuração da série de dados
    hc_add_series(
      mapData = worldgeojson,
      showInLegend = FALSE,
      nullColor = "#E0E0E0",
      borderWidth = 0
    ) %>%
    
    hc_add_series(
      data = geojson_list(src_point_sf),
      type = "mappoint",
      name = "Origem",
      color = "#3d9970",
      marker = list(symbol = "circle", radius = 4)
    ) %>%
    
    hc_add_series(
      data = geojson_list(dest_points_sf),
      type = "mappoint",
      name = "Destinos",
      color = "#d35400",
      marker = list(symbol = "circle", radius = 3)
    ) %>%
    
    hc_add_series(
      data = geojson_list(connections_sf),
      type = "mapline",
      color = "#d35400",
      lineWidth = 1,
      # zIndex = -1,
      opacity = 0.8,
      name = "Conexões",
      enableMouseTracking = FALSE
    ) %>%
    
    # configuração do tooltip
    hc_tooltip(
      useHTML = TRUE,
      formatter = tooltip_text
    ) %>%
    
    # configuração da legenda
    hc_legend(enabled = FALSE,
              align = "center",
              layout = "horizontal") %>%
    
    # Tema e Exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

### Sankey ----
generate_sankey_city <- function(data, measure, year_filter, city) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_513
  sankey_data <- data %>%
    filter(Cidade == city) %>%  
    group_by(Cidade, Nome_ISIC_Secao, Nome_Bloco) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE),
              .groups = "drop") %>%
    mutate(
      from1 = Cidade,
      to1 = Nome_ISIC_Secao
    ) %>%
    mutate(
      from2 = Nome_ISIC_Secao,
      to2 = Nome_Bloco
    ) %>%
    select(from1, to1, from2, to2, Measure)
  
  # unindo os dois níveis de ligação no gráfico Sankey
  sankey_data <- bind_rows(
    sankey_data %>% select(from = from1, to = to1, weight = Measure),
    sankey_data %>% select(from = from2, to = to2, weight = Measure)
  ) 
  
  sankey_data <- sankey_data %>%
    group_by(from, to) %>%
    summarise(weight = sum(weight, na.rm = TRUE), .groups = 'drop')
  
  # criando a paleta de cores com base no número de nós
  unique_nodes <- unique(c(sankey_data$from, sankey_data$to))
  colors <- brewer.pal(n = min(length(unique_nodes), 8), name = "Dark2")
  
  # criando os nós para o gráfico
  nodes <- unique_nodes %>%
    lapply(function(x) {
      list(
        id = x,
        name = gsub("^\\d+\\.\\s?", "", x),
        color = colors[match(x, unique_nodes) %% length(colors) + 1] # Atribuindo cores com base na paleta
      )
    })
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste(
    "Fluxo de Exportações de ", city, " por ",
    title_measure
  )
  title_subtitle <- paste0(
    "Município: ", city, 
    " | Ano: ", ifelse(year_filter == "Todos", "2011-2020", year_filter))
  tooltip_text <- paste(
    "<b>{point.from}</b> → <b>{point.to}</b> <br>",
    ifelse(measure == "value",
           "<b> Valor Exportado </b> US$ {point.weight:,.1f} mi",
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
  
  # gráfico sankey com highchart
  highchart() %>%
    
    # configuração inicial do tipo de gráfico
    hc_chart(type = "sankey", backgroundColor = "#fff") %>%
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    
    # configuração da série de dados
    hc_add_series(data = sankey_data,
                  name = "Fluxo de Exportação",
                  nodes = nodes)  %>%
    
    # configuração do tooltip
    hc_tooltip(useHTML = TRUE,
               pointFormat = tooltip_text,
               nodeFormat = tooltip_node) %>%
    
    # configurações avançadas
    hc_plotOptions(
      series = list(dataLabels = list(style = list(fontSize = "10px"))),
      sankey = list(curveFactor = 0.5, linkOpacity = 0.33)
    ) %>% 
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

## por Estado ----

### Mapa Cloroplético ----
generate_cloropletic_state <- function(data, measure, year_filter, product_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")

  # Filtrar e agrupar os dados
  cloropletic_data <- data %>%
    group_by(Sigla_UF, Nome_Estado) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE),
              .groups = "drop")
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste0(
    "Fluxo de Exportações por ",
    title_measure
  )
  title_subtitle <- paste(
    "Atividade Econômica:", product_filter, 
    " | Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter)
  )
  tooltip_text <- ifelse(
    measure == "value",
    "<b>{point.Nome_Estado}</b> <br> <b>Valor Exportado</b>: US$ {point.value:,.1f} mi", 
    "<b>{point.Nome_Estado}</b> <br> <b>Peso Exportado</b>: {point.value:,.1f} toneladas" 
  )
   
  # gráfico de mapa com highchart 
  
  hcmap(
    "countries/br/br-all",
    data = cloropletic_data,
    value = "Measure",
    joinBy = c("hc-a2", "Sigla_UF"),
    name = "Exportações",
    dataLabels = list(enabled = TRUE, format = '{point.Sigla_UF}'),
    borderColor = "#FAFAFA",
    borderWidth = "0.1"
  ) %>% 
    
    # configuração inicial do tipo de gráfico
    hc_chart(zoomType = "xy") %>%
    
    # configuração cor
    hc_colorAxis(minColor = "#e5f5e0", maxColor = "#00441b") %>%
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    
    # configuração do tooltip
    hc_tooltip(pointFormat = tooltip_text) %>%
    
    hc_legend(layout = "vertical",
              align = "right",
              valueDecimals = 1)  %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

### Sunburst ----
generate_sunburst_state <- function(data, measure, year_filter, state_filter, 
                                    threshold = 2) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_522
  sunburst_data <- data %>%
    filter(Ano == year_filter | year_filter == "Todos") %>%
    filter(Nome_Estado == state_filter | state_filter == "Todos") %>%
    group_by(Nome_Estado, Nome_ISIC_Secao, Nome_SH2) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE) ,
              .groups = "drop")
  
  total_measure <- sum(sunburst_data$Measure)
  
  sunburst_data <- sunburst_data %>%
    mutate(Percent = Measure / total_measure * 100) %>%
    mutate(Nome_SH2 = ifelse(Percent < threshold, "Outros", Nome_SH2)) %>%
    group_by(Nome_Estado, Nome_ISIC_Secao, Nome_SH2) %>%
    summarise(Measure = sum(Measure), .groups = "drop")
  
  # Configuração dos níveis para mostrar labels no nível 1
  lvl_opts <- list(
    list(
      level = 1,
      borderWidth = 0,
      borderColor = "transparent",
      dataLabels = list(
        enabled = TRUE,
        format = "{point.name}",  
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
      colorVariation = list(key = "brightness", to = 0.500)
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
    )
  )
  
  # definição de cor
  colors <- RColorBrewer::brewer.pal(length(data$Nome_ISIC_Secao), "Dark2")
  
  sunburst_hierarchy <- data_to_hierarchical(
    sunburst_data,
    c("Nome_Estado", "Nome_ISIC_Secao", "Nome_SH2"),
    "Measure",
    colors
  )
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste0(
    "Distribuição das Exportações do ", state_filter, " por ",
    title_measure
  )
  title_subtitle <- paste(
    "Estado: ", state_filter, 
    " | Ano: ", ifelse(year_filter == "Todos", "2011-2020", year_filter)
  )
  tooltip_text <- ifelse(
    measure == "value",
    "<b>{point.name}</b> <br> <b>Valor Exportado</b>: US$ {point.value:,.1f} mi", 
    "<b>{point.name}</b> <br> <b>Peso Exportado</b>: {point.value:,.1f} toneladas"
  )
  
  # gráfico de sunburst com highchart
  highchart() %>%
    
    # configuração inicial do tipo de gráfico
    hc_chart(type = "sunburst") %>%
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    
    # configuração da série de dados
    hc_add_series(
      name = state_filter,
      data = sunburst_hierarchy,
      type = "sunburst",
      allowDrillToNode = TRUE,
      levelIsConstant = FALSE,
      levels = lvl_opts
    ) %>%
    
    # configuração do tooltip
    hc_tooltip(pointFormat = tooltip_text) %>%
    
    # configurações avançadas
    hc_plotOptions(
      sunburst = list(
        dataLabels = list(
          format = "{point.name}",
          filter = list(
            property = "outerArcLength",
            operator = ">",
            value = 64
          ),
          style = list(textOutline = "none")
        )
      )
    ) %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

## por País ----

### Racebar ----
# Preparando os dados com saldo acumulado
prepare_race_data <- function(data) {
  data %>%
    group_by(Nome_Pais) %>%
    arrange(Ano) %>%
    mutate(Saldo_Acumulado = cumsum(US_FOB)) %>%
    ungroup() %>%
    group_by(Ano) %>%
    mutate(
      rank = rank(-Saldo_Acumulado),
      Measure_rel = Saldo_Acumulado / Saldo_Acumulado[rank == 1],
      Measure_lbl = paste0(" US$", scales::label_number(big.mark = ".", decimal.mark = ",")(Saldo_Acumulado), " mi")
    ) %>%
    filter(rank <= 10) %>%
    ungroup()
}

generate_bar_race <- function(race_data) {
  
  # gráfico de barras com ggplot
  staticplot <-
    ggplot(race_data,
           aes(
             x = rank,
             y = Saldo_Acumulado,
             group = Nome_Pais,
             fill = Nome_Pais
           )) +
    geom_col(width = 0.8, color = "black") +
    theme_minimal() +
    geom_text(aes(y = 0, label = paste(Nome_Pais, " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y = Saldo_Acumulado, label = Measure_lbl, hjust = 0)) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_reverse() +
    guides(color = FALSE, fill = FALSE) +
    theme(
      axis.title.y = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0, face = "bold"),
      plot.background = element_blank(),
      plot.margin = margin(0, 2, 0, 3, "cm")
    ) +
    labs(title = '{closest_state}', x = " ", y = " Valor (US$)")
  
  # criação da animação
  anim <- staticplot + 
    transition_states(Ano, transition_length = 4, state_length = 1) +
    view_follow(fixed_x = TRUE)
  
  # Salvando como .gif
  animate(anim, fps = 10, width = 800, height = 400, renderer = gifski_renderer("www/bar_race.gif"))
}

### Treemap ----
generate_treemap_country <- function(data, measure, year_filter, product_filter,
                                     threshold = 0.1) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_531
  treemap_data <- data %>%
    filter((year_filter == "Todos" | Ano == year_filter) &
             (product_filter == "Todos" |
                Nome_ISIC_Secao == product_filter)
    ) %>%
    group_by(Nome_Bloco, Nome_Pais, Cor) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE), .groups = "drop") %>%
    mutate(Nome_Paislvl3 = Nome_Pais)
  
  total_measure <- sum(treemap_data$Measure)
  treemap_data <- treemap_data %>%
    mutate(Percent = Measure / total_measure * 100) %>%
    mutate(Nome_Pais = ifelse(Percent < threshold, "Outros", Nome_Pais)) %>%
    group_by(Nome_Bloco, Nome_Pais, Nome_Paislvl3, Cor) %>%
    summarise(Measure = sum(Measure), .groups = "drop") %>%
    arrange(Nome_Bloco) %>%
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
    )
  )
  
  # Definir cores com base nos blocos únicos
  Cor <- data %>%
    select(Nome_Bloco, Cor) %>%
    distinct() %>%
    arrange(Nome_Bloco)
  
  treemap_hierarchical <- data_to_hierarchical(
    treemap_data,
    c("Nome_Bloco", "Nome_Pais", "Nome_Paislvl3"),
    "value", 
    colors = Cor$Cor
  )
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste0(
    "Exportações dos Países por ",
    title_measure
  )
  title_subtitle <- paste(
    "Atividade Econômica:", product_filter, 
    " | Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter)
  )
  tooltip_text <- ifelse(
    measure == "value",
    "<b>{point.name}</b> <br> <b>Valor Exportado</b>: US$ {point.value:,.1f} mi", 
    "<b>{point.name}</b> <br> <b>Peso Exportado</b>: {point.value:,.1f} toneladas" 
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
      name = "Measure",
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

### Packed Bubble ----
generate_bubble_country <- function(data, measure, year_filter, product_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_533
  bubble_data <- data %>%
    filter((Ano == year_filter | year_filter == "Todos") &
             (Nome_ISIC_Secao == product_filter |
                product_filter == "Todos")
    ) %>%
    group_by(Nome_Bloco, Nome_Pais) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE) ,
              .groups = "drop")
  
  # definição de limite de nomes nas bolhas
  q95 <- as.numeric(quantile(bubble_data$Measure, .95))
  
  # definição de cor
  unique_blocks <- sort(unique(bubble_data$Nome_Bloco))
  colors <- RColorBrewer::brewer.pal(length(unique_blocks), "Dark2")
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste("Representatividade das Exportações por País por ",
                       title_measure)
  title_subtitle <- paste(
    "Atividade Econômica:", product_filter,
    " | Ano:", ifelse(year_filter == "Todos", "2011-2020", year_filter))
  tooltip_text <- ifelse(
    measure == "value",
    "<b>{point.name}</b> <br> <b>Valor Exportado</b>: US$ {point.value:,.1f} mi",
    "<b>{point.name}</b> <br> <b>Peso Exportado</b>: {point.value:,.1f} toneladas"
  )
  
  # gráfico de bolhas com highchart
  highchart() %>%
    
    # configuração inicial do tipo de gráfico
    hc_chart(type = "packedbubble", backgroundColor = "#fff") %>% 
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    
    # configuração da série de dados
    hc_add_series(
      data = bubble_data,
      type = "packedbubble",
      hcaes(
        name = Nome_Pais,
        value = Measure,
        group = Nome_Bloco
      ),
      colorByPoint = FALSE
    ) %>% 
    
    # definição da cor do gráfico
    hc_colors(colors) %>%
    
    # configuração do tooltip
    hc_tooltip(useHTML = TRUE, pointFormat = tooltip_text) %>% 
    
    # configurações avançadas
    hc_plotOptions(
      packedbubble = list(
        maxSize = "500%",
        zMin = 0,
        layoutAlgorithm = list(
          gravitationalConstant = 0.01,
          splitSeries = FALSE,
          seriesInteraction = TRUE,
          dragBetweenSeries = TRUE,
          parentNodeLimit = TRUE
        ),
        dataLabels = list(
          enabled = TRUE,
          format = "{point.name}",
          filter = list(
            property = "y",
            operator = ">",
            value = q95
          ),
          style = list(
            color = "black",
            textOutline = "none",
            fontWeight = "normal"
          )
        )
      )
    ) %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

### por Produto ----

#### scatterplot ----
generate_scatterplot_product <- function(data, year_filter, product_filter) {
   
  # base de dados g_541
  scatter_data <- data %>%
    filter((Ano == year_filter | year_filter == "Todos") &
             (Nome_ISIC_Secao == product_filter |
                product_filter == "Todos")
    )
  
  # definição de cor
  Cor <- scatter_data %>%
    select(Nome_ISIC_Secao, Cor) %>%
    distinct() %>%
    arrange(Nome_ISIC_Secao)
  
  # Calcula a linha de tendência usando lm()
  trend_model <- lm(Peso_Liquido ~ US_FOB, data = scatter_data)
  trend_data <- data.frame(
    US_FOB = scatter_data$US_FOB,
    Peso_Liquido = predict(trend_model)
  )
  
  # formatação do eixo x
  x_formatter <- JS("function() {return this.value + ' mi';}")
  
  # formatação do eixo y
  y_formatter <- JS("function() {return (this.value / 1e6).toFixed(0) + ' mi';}")
  
  # definição de títulos e tooltip
  title_title <- paste0("Relação Valor x Peso Líquido das Exportações por Regiao")
  title_subtitle <- paste0(
      "Atividade Econômica: ", product_filter,
      " | Ano: ", ifelse(year_filter == "Todos", "2011-2020", year_filter)
    )
  tooltip_text <- "<b>{point.Nome_Regiao} </b><br>
                   <b>{point.name}</b><br>
                   <b>Ano</b>: {point.Ano} <br>
                   <b>Valor</b>: US$ {point.x:,.1f} mi <br>
                   <b>Peso</b>: {point.y:,.1f} toneladas <br>
                   <b>Preço Médio</b>: {point.Mean_Price:,.1f} US$/tonelada"
  
  # gráfico scatterplot com highchart
  highchart() %>%
    
    hc_chart(type = "scatter", backgroundColor = "white") %>%
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    
    # configuração do eixo X (eixo horizontal)
    hc_xAxis(
      title = list(text = "Valor (US$)"),
      min = 0,
      gridLineColor = NULL,
      lineColor = "#999999",
      tickColor = "#999999",
      labels = list(formatter = x_formatter)
    ) %>%
    
    # configuração do eixo Y (eixo vertical)
    hc_yAxis(
      title = list(text = "Peso Líquido (Toneladas)"),
      min = 0,
      gridLineColor = NULL,
      lineColor = "#999999",
      tickColor = "#999999",
      labels = list(formatter = y_formatter)
    ) %>% 
    
    # configuração da série de dados
    hc_add_series(
      data = scatter_data,
      mapping = hcaes(
        x = US_FOB,
        y = Peso_Liquido,
        group = Nome_ISIC_Secao,
        name = Nome_SH2
      ),
      type = "scatter",
      color = Cor$Cor,
      marker = list(radius = 4, symbol = "circle") 
    ) %>%
    
    # Série da linha de tendência
    hc_add_series(
      data = trend_data,
      hcaes(x = US_FOB, y = Peso_Liquido),
      type = "line",
      color = "#3d9970",
      name = "Linha de Tendência",
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE
    ) %>%
    
    # configuração do tooltip
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_text
    ) %>%
    
    # configuração da legenda
    hc_legend(
      enabled = TRUE,
      align = 'center',
      layout = 'horizontal'
    ) %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

#### Areachart ----
generate_area_product <- function(data, measure, state_filter, country_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_542
  area_data <-  data %>%
    filter((Nome_Estado == state_filter |
              state_filter == "Todos") &
             (Nome_Pais == country_filter |
                country_filter == "Todos")
    ) %>%
    group_by(Ano, Nome_ISIC_Secao, Cor) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE),
              .groups = "drop")
  
  # definição de cor
  Cor <- data %>%
    select(Nome_ISIC_Secao, Cor) %>%
    distinct() %>%
    arrange(Nome_ISIC_Secao)
  
  # formatação do eixo y
  y_formatter <- 
    JS(
      "function() {
            return this.value + ' %';
        }"
    )
  
  # definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste0(
    "Distribuição Anual de Exportações por ",
    title_measure
  )
  title_subtitle <-
    paste("País:", country_filter, " | Estado:", state_filter)
  tooltip_text <- ifelse(
    measure == "value",
    "<b>{point.Nome_ISIC_Secao}</b><br> <b> Valor Exportado</b>: US$ {point.Measure:,.1f} mi",
    "<b>{point.Nome_ISIC_Secao}</b><br> <b> Peso Exportado</b>: {point.Measure:,.1f} toneladas"
  )
  
  # gráfico de área com highchart
  highchart() %>%
    
    # configuração inicial do tipo de gráfico
    hc_chart(type = "areaspline") %>%
    
    # configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%
    
    # configuração do eixo X (eixo horizontal)
    hc_xAxis(
      type = "datetime",
      categories = unique(area_data$Ano),
      title = list(text = "Ano"),
      gridLineColor = NULL,
      lineColor = "#999999",
      tickColor = "#999999"
    ) %>%
    
    # configuração do eixo Y (eixo vertical)
    hc_yAxis(
      title = list(text = title_measure),
      gridLineColor = NULL,
      LineColor = "#999999",
      tickColor = "#999999",
      labels = list(formatter = y_formatter)
    ) %>% 
    
    # configuração da série de dados
    hc_add_series(
      data = area_data,
      hcaes(
        x = Ano,
        y = Measure, 
        group = Nome_ISIC_Secao
      ),
      type = "areaspline",
      # colorByPoint = TRUE,
      color = Cor$Cor,
      showInLegend = TRUE
    ) %>%
    
    # configuração do tooltip
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_text
    ) %>%
    
    # configuração da legenda
    hc_legend(
      enabled = TRUE,
      align = 'center',
      layout = 'horizontal'
    ) %>%
    
    # configurações avançadas
    hc_plotOptions(
      series = list(stacking = "percent"),
      marker = list(lineWidth = 1,
                    lineColor = "#ffffff")
    ) %>% 
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

#### Dependency Wheel ----

##### por Região ----
# generate_wheel_region <- function(data, measure, year_filter) {
#   measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
#   
#   # base de dados g_543a
#   wheel_data <- data %>%
#     group_by(Nome_Regiao, Nome_ISIC_Secao) %>%
#     summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE),
#               .groups = "drop")
#   
#   connections <- data %>%
#     transmute(from = Nome_ISIC_Secao, to = Nome_Regiao, weight = US_FOB)
#   
#   Cor1 <- data %>%
#     select(Nome_ISIC_Secao) %>%
#     distinct() %>%
#     mutate(Cor = sapply(Nome_ISIC_Secao, function(x)
#       get_color("isic", x))) %>%
#     rename(Nome = Nome_ISIC_Secao)
#   
#   Cor2 <- data %>%
#     select(Nome_Regiao) %>%
#     distinct() %>%
#     mutate(Cor = sapply(Nome_Regiao, function(x)
#       get_color("region", x))) %>%
#     rename(Nome = Nome_Regiao)
#   
#   Cor <- bind_rows(Cor1, Cor2) %>%
#     arrange()
#   
#   color_map <- setNames(Cor$Cor, Cor$Nome)
#   
#   nodes <- unique(c(connections$from, connections$to)) |>
#     lapply(function(x) {
#       list(
#         id = x,
#         color = ifelse(!is.null(color_map[x]), color_map[x], "gray"),
#         name = gsub("\\d+\\.\\s?", "", x)
#       )
#     })
#   
#   # definição de títulos e tooltip
#   title_measure <-
#     ifelse(measure == "value",
#            "Valor Exportado (US$)",
#            "Peso Exportado (Toneladas)")
#   title_title <- "Relações de Exportação entre Regiões e Setores Econômicos"
#   title_subtitle <- paste("Ano:", year_filter)
#   tooltip_text <-paste(
#       "<b>{point.from}</b> → <b>{point.to}</b> <br>",
#       ifelse(measure == "value",
#         "<b> Valor Exportado </b> US$ {point.weight:,.1f} mi",
#         "<b> Peso Exportado </b>: {point.weight:,.1f} toneladas"
#       )
#     )
#   
#   # gráfico de roda com highchart
#   highchart() %>%
#     
#     # configuração inicial do tipo de gráfico
#     hc_chart(type = "dependencywheel", backgroundColor = "white") %>%
#     
#     # configuração do título e subtítulo
#     hc_title(text = title_title) %>%
#     hc_subtitle(text = title_subtitle) %>%
#     
#     # configuração da série de dados
#     hc_add_series(
#       data = connections,
#       hcaes(from = from, to = to, weight = weight),
#       type = "dependencywheel",
#       nodes = nodes
#     ) %>% 
#     
#     # configuração do tooltip
#     hc_tooltip(useHTML = TRUE,
#                pointFormat = tooltip_text) %>% 
#     
#     # tema e exportação
#     hc_add_theme(hc_theme_smpl()) %>%
#     hc_exporting(enabled = TRUE)
# }

##### por Bloco Econômico ----
# generate_wheel_block <- function(data, measure, year_filter) {
#   measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
#   
#   # base de dados g_543b
#   wheel_data <- data %>%
#     group_by(Nome_Bloco, Nome_ISIC_Secao) %>%
#     summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE), .groups = "drop")
#   
#   connections <- data %>% 
#     transmute(from = Nome_ISIC_Secao, to = Nome_Bloco, weight = US_FOB)
#   
#   Cor1 <- data %>%
#     select(Nome_ISIC_Secao) %>%
#     distinct() %>%
#     mutate(Cor = sapply(Nome_ISIC_Secao, function(x) get_color("isic", x))) %>%
#     rename(Nome = Nome_ISIC_Secao)
#   
#   Cor2 <- data %>%
#     select(Nome_Bloco) %>%
#     distinct() %>%
#     mutate(Cor = sapply(Nome_Bloco, function(x) get_color("region", x))) %>%
#     rename(Nome = Nome_Bloco)
# 
#   Cor <- bind_rows(Cor1, Cor2) %>%
#     arrange()
#   
#   color_map <- setNames(Cor$Cor, Cor$Nome)
#   
#   nodes <- unique(c(connections$from, connections$to)) |>
#     lapply(function(x) {
#       list(
#         id = x,
#         color = ifelse(!is.null(color_map[x]), color_map[x], "gray"), 
#         name = gsub("\\d+\\.\\s?", "", x)
#       )
#     })
#   
#   # definição de títulos e tooltip
#   title_measure <-
#     ifelse(measure == "value",
#            "Valor Exportado (US$)",
#            "Peso Exportado (Toneladas)")
#   title_title <- "Relações de Exportação entre Bloco e Setores Econômicos"
#   title_subtitle <- paste("Ano:", year_filter)
#   tooltip_text <-paste(
#     "<b>{point.from}</b> → <b>{point.to}</b> <br>",
#     ifelse(measure == "value",
#            "<b> Valor Exportado </b> US$ {point.weight:,.1f} mi",
#            "<b> Peso Exportado </b>: {point.weight:,.1f} toneladas"
#     )
#   )
#   
#   # gráfico de roda com highchart
#   highchart() %>%
#     
#     # configuração inicial do tipo de gráfico
#     hc_chart(type = "dependencywheel", backgroundColor = "white") %>%
#     
#     # configuração do título e subtítulo
#     hc_title(text = title_title) %>%
#     hc_subtitle(text = title_subtitle) %>%
#     
#     # configuração da série de dados
#     hc_add_series(
#       data = connections,
#       hcaes(from = from, to = to, weight = weight),
#       type = "dependencywheel",
#       name = "Exportações",
#       # colorByPoint = TRUE,
#       nodes = nodes
#     ) %>%
#     
#     # configuração do tooltip
#     hc_tooltip(useHTML = TRUE,
#                pointFormat = tooltip_text) %>% 
#     
#     # tema e exportação
#     hc_add_theme(hc_theme_smpl()) %>%
#     hc_exporting(enabled = TRUE)
# }

# ### Comparabilidade -----
# 
# #### por Municipio ----
generate_city_analytics <- function(data, measure, city_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")

  # base de dados g_552
  city_data <- data %>%
    filter(Cidade %in% city_filter) %>%
    group_by(Ano, Cidade) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE),
              .groups = 'drop') %>%
    ungroup() %>%
    complete(Ano, Cidade, fill = list(Measure = NA))%>%
    mutate(Cidade = factor(Cidade, city_filter))

  # definição de cor
  colors <- RColorBrewer::brewer.pal(length(city_filter), "Dark2")

  # formatação do eixo y
  y_formatter <- if (measure == "value") {
    JS("function() {
          if (this.value >= 1e6) {
            return (this.value / 1e6).toFixed(0) + ' tri';
          } else if (this.value >= 1e3) {
            return (this.value / 1e3).toFixed(0) + ' bi';
          } else {
            return this.value + ' mi';
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

  ## definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste0(
    "Exportações por Ano e Cidade por ",
    title_measure
  )
  tooltip_text <- paste0(
    "<b>{point.Cidade}</b><br>",
    ifelse(
      measure == "value",
      "<b> Valor Exportado</b>: US$ {point.y:,.2f} mi",
      "<b> Peso Exportado</b>: {point.y:,.2f} toneladas"
    )
  )

  # gráfico de linhas com highchart
  highchart() %>%

    # configuração inicial do tipo de gráfico
    hc_chart(zoomType = "x", backgroundColor = "#fff") %>%

    # configuração do título e subtítulo
    hc_title(text = "Exportações por Ano e Cidade") %>%

    # configuração do eixo X (eixo horizontal)
    hc_xAxis(
      title = list(text = "Ano"),
      categories = unique(city_data$Ano),
      gridLineColor  = NULL,
      lineColor = "#999999",
      tickColor = "#999999"
    ) %>%

    # configuração do eixo Y (eixo vertical)
    hc_yAxis(
      title = list(text = title_measure),
      gridLineColor  = NULL,
      lineColor = "#999999",
      tickColor = "#999999",
      labels = list(formatter = y_formatter)
    ) %>%

    # configuração da série de dados
    hc_add_series(
      data = city_data,
      type = "spline",
      hcaes(
        x = Ano,
        y = Measure,
        group = Cidade
      ),
      marker = list(
        enable = TRUE,
        symbol = "circle",
        radius = 3
        # lineWidth = 1, lineColor = "grey"
      ),
      name = city_filter
    ) %>%

    # definição da cor do gráfico
    hc_colors(colors) %>%

    # configuração do tooltip
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_text
    ) %>%

    # configuração da legenda
    hc_legend(enabled = TRUE,
              align = 'center',
              layout = 'horizontal') %>%

    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

#### por Estado ----
generate_state_analytics <- function(data, measure, state_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
  
  # base de dados g_552
  state_data <- data %>%
    filter(Nome_Estado %in% state_filter) %>%
    group_by(Ano, Nome_Estado) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE),
              .groups = 'drop') %>%
    ungroup() %>%
    complete(Ano, Nome_Estado, fill = list(Measure = NA)) %>%
    mutate(Nome_Estado = factor(Nome_Estado, state_filter))
  
  # definição de cor
  colors <- RColorBrewer::brewer.pal(length(state_filter), "Dark2")
  
  # formatação do eixo y
  y_formatter <- if (measure == "value") {
    JS("function() {
          if (this.value >= 1e6) {
            return (this.value / 1e6).toFixed(0) + ' tri';
          } else if (this.value >= 1e3) {
            return (this.value / 1e3).toFixed(0) + ' bi';
          } else {
            return this.value + ' mi';
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
  
  ## definição de títulos e tooltip
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  title_title <- paste0(
    "Exportações por Ano e Estado por ",
    title_measure
  )
  tooltip_text <- paste0(
    "<b>{point.Nome_Estado}</b><br>",
    ifelse(
    measure == "value",
    "<b> Valor Exportado</b>: US$ {point.y:,.2f} mi",
    "<b> Peso Exportado</b>: {point.y:,.2f} toneladas"
    )
  )
  
  # gráfico de linhas com highchart
  highchart() %>%
    
    # configuração inicial do tipo de gráfico
    hc_chart(zoomType = "x", backgroundColor = "#fff") %>% 
    
    # configuração do título e subtítulo
    hc_title(text = "Exportações por Ano e Estado") %>%
    
    # configuração do eixo X (eixo horizontal)
    hc_xAxis(
      title = list(text = "Ano"),
      categories = unique(state_data$Ano),
      gridLineColor  = NULL,
      lineColor = "#999999",
      tickColor = "#999999"
    ) %>%
    
    # configuração do eixo Y (eixo vertical)
    hc_yAxis(
      title = list(text = title_measure),
      gridLineColor  = NULL,
      lineColor = "#999999",
      tickColor = "#999999",
      labels = list(formatter = y_formatter)
    ) %>%
    
    # configuração da série de dados
    hc_add_series(
      data = state_data,
      type = "spline",
      hcaes(
        x = Ano,
        y = Measure,
        group = Nome_Estado
      ),
      marker = list(
        enable = TRUE,
        symbol = "circle",
        radius = 3
        # lineWidth = 1, lineColor = "grey"
      ),
      name = state_filter
    ) %>%
    
    # definição da cor do gráfico
    hc_colors(colors) %>%
    
    # configuração do tooltip
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_text
    ) %>%
    
    # configuração da legenda
    hc_legend(enabled = TRUE,
              align = 'center',
              layout = 'horizontal') %>%
    
    # tema e exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

#### por País ----
generate_country_analytics <- function(data, measure, country_filter) {
    measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
    
    # base de dados g_553
    country_data <- data %>%
      filter(Nome_Pais %in% country_filter) %>%
      group_by(Ano, Nome_Pais) %>%
      summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      complete(Ano, Nome_Pais, fill = list(Measure = NA)) %>%
      mutate(Nome_Pais = factor(Nome_Pais, country_filter))
  
    # definição de cor
    colors <- RColorBrewer::brewer.pal(length(country_filter), "Dark2")
    
    # formatação do eixo y
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
            return (this.value / 1e9).toFixed(0) + ' bi';
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
    title_title <- paste0(
      "Exportações por Ano e País por ",
      title_measure
    )
    tooltip_text <- paste(
      "<b>{point.Nome_Pais}</b><br>",
      ifelse(
        measure == "value",
        "<b> Valor Exportado</b>: US$ {point.y:,.2f} mi",
        "<b> Peso Exportado</b>: {point.y:,.2f} toneladas"
      )
    )
  
  # gráfico de linhas com highchart
    highchart() %>%
      
      # configuração inicial do tipo de gráfico
      hc_chart(zoomType = "x", backgroundColor = "#fff") %>%
      
      # configuração do título e subtítulo
      hc_title(text = title_title) %>%
      
      # configuração do eixo X (eixo horizontal)
      hc_xAxis(
        title = list(text = "Ano"),
        categories = unique(country_data$Ano),
        gridLineColor  = NULL,
        lineColor = "#999999",
        tickColor = "#999999"
      ) %>%
      
      # configuração do eixo Y (eixo vertical)
      hc_yAxis(
        title = list(text = title_measure),
        gridLineColor  = NULL,
        lineColor = "#999999",
        tickColor = "#999999",
        labels = list(formatter = y_formatter)
      ) %>%
      
      # configuração da série de dados
      hc_add_series(
        data = country_data,
        type = "spline",
        hcaes(x = Ano,
              y = Measure,
              group = Nome_Pais),
        marker = list(
          enable = TRUE,
          symbol = "circle",
          radius = 3
          # lineWidth = 1, lineColor = "grey"
        ),
        name = country_filter
      ) %>%
      
      # definição da cor do gráfico
      hc_colors(colors) %>%
      
      # configuração do tooltip
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = tooltip_text
      ) %>%
      
      # configuração da legenda
      hc_legend(enabled = TRUE,
                align = 'center',
                layout = 'horizontal') %>%
      
      # tema e exportação
      hc_add_theme(hc_theme_smpl()) %>%
      hc_exporting(enabled = TRUE)
}

### Análise ----

#### Gráfico Heatmap ----
# generate_heatmap_analytics2 <- function(data, measure, year_filter, isic_filter) {
#   measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")
#
#   ## base de dados g_561
#   heatmap_data <- data %>%
#     group_by(Ano, Nome_Estado) %>%
#     mutate(Total_Anual = sum(!!as.symbol(measure_column), na.rm = TRUE)) %>%
#     ungroup() %>%
#     group_by(Ano, Trimestre, Ano_Trimestre, Nome_Estado) %>%
#     summarise(
#       Measure = sum(!!as.symbol(measure_column), na.rm = TRUE),
#       Total_Anual = first(Total_Anual),
#       .groups = "drop"
#     ) %>%
#     mutate(Percentual = (Measure / Total_Anual) * 100) %>%
#     ungroup() %>%
#     mutate(x = as.numeric(factor(Ano_Trimestre, levels = unique(Ano_Trimestre))) - 1,
#            y = as.numeric(factor(Nome_Estado, levels = unique(Nome_Estado))) - 1)
#
#   ## definição de títulos e tooltip
#   title_title <- paste0(
#     "Heatmap de Exportações dos Estados por ",
#     ifelse(
#       measure == "value",
#       "Valor Exportado (US$)",
#       "Peso Exportado (Toneladas)"
#     )
#   )
#   title_subtitle <- paste(
#     "Atividade Econômica:", isic_filter,
#     " | Ano:", year_filter)
#   title_measure <-
#     ifelse(measure == "value",
#            "Valor Exportado (US$)",
#            "Peso Exportado (Toneladas)")
#   tooltip_text <- paste0(
#     "<b>Estado:</b> {point.Nome_Estado}<br>",
#     "<b>Ano:</b> {point.Ano}<br>",
#     "<b>Trimestre:</b> {point.Trimestre}<br>",
#     "<b>Percentual:</b> {point.Percentual:.1f}%<br>",
#     ifelse(
#       measure == "value",
#       "<b>Valor Exportado:</b> US$ {point.Measure:,.1f} mi",
#       "<b>Peso Exportado:</b> {point.Measure:,.1f} toneladas"
#     )
#   )
#
#   # gráfico heatmap com highchart
#   highchart() %>%
#
#     # configuração inicial do tipo de gráfico
#     hc_chart(type = "heatmap", backgroundColor = "#fff") %>%
#
#     # configuração do título e subtítulo
#     hc_title(text = title_title) %>%
#     hc_subtitle(text = title_subtitle) %>%
#
#     # configuração do eixo X (eixo horizontal)
#     hc_xAxis(
#       title = list(text = "Ano e Trimestre"),
#       categories = unique(heatmap_data$Ano_Trimestre),
#       labels = list(rotation = -45)
#     ) %>%
#
#     # configuração do eixo Y (eixo vertical)
#     hc_yAxis(
#       categories = unique(heatmap_data$Nome_Estado),
#       reversed = TRUE,
#       tickInterval = 1,
#       labels = list(
#         style = list(fontSize = '8px'),
#         align = 'right'
#       )
#     ) %>%
#
#     # configuração da série de dados
#     hc_add_series(
#       data = heatmap_data,
#       type = "heatmap",
#       mapping = hcaes(x = x,
#                       y = y,
#                       value = Percentual),
#       name = "Percentual Trimestral (%)",
#       borderWidth = 0.5
#     ) %>%
#
#     # definição da cor do gráfico
#     hc_colorAxis(
#       stops = color_stops(n = 10, colors = RColorBrewer::brewer.pal(9, "Reds")),
#       min = 0,
#       max = 100
#     ) %>%
#
#     # configuração do tooltip
#     hc_tooltip(useHTML = TRUE, pointFormat = tooltip_text) %>%
#
#     # configuração da legenda
#     hc_legend(layout = "horizontal",
#               align = "center",
#               verticalAlign = "bottom") %>%
#
#     # tema e exportação
#     hc_add_theme(hc_theme_smpl()) %>%
#     hc_exporting(enabled = TRUE)
# }

#### Gráfico de Pareto ----
generate_pareto_analytics2 <- function(data, measure, year_filter, state_filter) {
  measure_column <- ifelse(measure == "value", "US_FOB", "Peso_Liquido")

  ## base de dados g_562
  column_data <- data %>%
    filter((Ano == year_filter | year_filter == "Todos") &
             (Nome_Estado == state_filter | state_filter == "Todos")) %>%
    group_by(Nome_SH2) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    arrange(desc(Measure)) %>%
    slice_head(n = 10)

  line_data <- data %>%
    filter((Ano == year_filter | year_filter == "Todos") &
             (Nome_Estado == state_filter | state_filter == "Todos")) %>%
    group_by(Nome_SH2) %>%
    summarise(Measure = sum(.data[[measure_column]], na.rm = TRUE), .groups = "drop") %>%  ##
    arrange(desc(Measure))  %>%
    mutate(
      Cumulative_Percent = cumsum(Measure) / sum(Measure) * 100
    ) %>%
    slice_head(n = 10)

  ## formatação eixo y
  y_formatter <- if (measure == "value") {
    JS("function() {
          if (this.value >= 1e6) {
            return (this.value / 1e6).toFixed(0) + ' tri';
          } else if (this.value >= 1e3) {
            return (this.value / 1e3).toFixed(0) + ' bi';
          } else {
            return this.value + ' mi';
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

  ## definição títulos e tooltip
  title_title <- paste0(
    "Distribuição Produtos por ",
    ifelse(
      measure == "value",
      "Valor Exportado (US$)",
      "Peso Exportado (Toneladas)"
    ),
    " por Estado"
  )
  title_subtittle <- paste0(
    "Estado: ", state_filter,
    " | Ano: ", ifelse(year_filter == "Todos", "2011-2020", year_filter)
  )
  title_measure <- ifelse(measure == "value", "Valor Exportado (US$)", "Peso Exportado (Toneladas)")
  tooltip_text <- paste0(
    ifelse(measure == "value",
           "<b>Valor Exportado:</b> US$ {point.Measure:,.1f} mi",
           "<b>Peso Exportado:</b> {point.Measure:,.1f} toneladas")
  )

  ## gráfico de pareto com highchart
  highchart() %>%

    # Configuração inicial do tipo de gráfico
    hc_chart(
      backgroundColor = "#fff") %>%

    # Configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtittle) %>%

    # Configuração do eixo X (eixo horizontal)
    hc_xAxis(
      categories = column_data$Nome_SH2,
      gridLineColor  = NULL,
      lineColor = "#999999",
      tickColor = "#999999",
      gridLineWidth = 1,
      labels = list(rotation = -45)
    ) %>%

    # Configuração do eixo Y (eixo vertical)
    hc_yAxis_multiples(
      list(
        title = list(text = title_measure),
        gridLineColor  = NULL,
        lineColor = "#999999",
        tickColor = "#999999",
        labels = list(formatter = y_formatter),
        opposite = FALSE
      ),
      list(
        title = list(text = "% Acumulado"),
        max = 100,
        labels = list(format = "{value}%"),
        opposite = TRUE
      )
    ) %>%

    # Configuração da série de dados
    hc_add_series(
      name = title_measure,
      data = column_data,
      type = "column",
      mapping =
        hcaes(
          x = Nome_SH2,
          y = Measure
        ),
      dataLabels = list(
        enable = TRUE,
        format = "{point.y:,.0f}"
      ),
      color = "#5BF586",
      showInLegend = TRUE,
      tooltip = list(
        useHTML = TRUE,
        pointFormat = tooltip_text
      )
    ) %>%
    hc_add_series(
      name = "% Acumulado",
      data = line_data,
      mapping = hcaes(
        x = Nome_SH2,
        y = Cumulative_Percent),
      type = "line",
      yAxis = 1,
      color = "#f45b5b",
      marker = list(enabled = TRUE, radius = 3),
      tooltip = list(pointFormat = "<b>% Acumulado:</b> {point.y:.1f}%")
    ) %>%

    # Configuração do Tooltip
    hc_tooltip(useHTML = TRUE,
               pointFormat = tooltip_text) %>%

    # Configuração da legenda
    hc_legend(
      enabled = TRUE,
      align = "center",
      layout = "horizontal"
    ) %>%

    # Configurações avançadas
    hc_plotOptions(
      column = list(
        stacking = NULL,
        pointWidth = 30
      )
    ) %>%

    # Tema e Exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

#### Gráfico de Radar ----
generate_radar_analytics2 <- function(data, measure, sh2_filter) {
  measure_column <-
    ifelse(measure == "value", "US_FOB", "Peso_Liquido")

  ## base de dados g_563
  radar_data <- data %>%
    filter(Nome_SH2 == sh2_filter | sh2_filter == "Todos") %>%
    group_by(Ano, Nome_Regiao) %>%
    summarise(Measure = sum(!!as.symbol(measure_column) / 12, na.rm = TRUE),
              .groups = 'drop')

  ## definição de cor
  Cor <- data %>%
    select(Nome_Regiao, Cor) %>%
    distinct()

  ## formatação eixo y
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
            return (this.value / 1e9).toFixed(0) + ' bi';
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

  ## definição títulos e tooltip
  title_title <- paste0(
    "Média Anual do ",
    ifelse(
      measure == "value",
      "Valor Exportado (US$)",
      "Peso Exportado (Toneladas)"
    ),
    " por Região"
  )
  title_subtitle <-
    paste0("Produto: ", sh2_filter)
  title_measure <-
    ifelse(measure == "value",
           "Valor Exportado (US$)",
           "Peso Exportado (Toneladas)")
  tooltip_text <- paste0(
    "<b>Região:</b> {point.Nome_Regiao}<br>",
    ifelse(
      measure == "value",
      "<b>Valor Exportado:</b> US$ {point.Measure:,.2f} mi",
      "<b>Peso Exportado:</b> {point.Measure:,.2f} toneladas"
    )
  )

  ## gráfico de radar com highchart
  highchart() %>%
    # Configuração inicial do tipo de gráfico
    hc_chart(polar = T) %>%

    # Configuração do título e subtítulo
    hc_title(text = title_title) %>%
    hc_subtitle(text = title_subtitle) %>%

    # Configuração do eixo X (eixo horizontal)
    hc_xAxis(
      categories = sort(2011:2020),
      gridLineColor  = "#999999",
      lineColor = "#999999",
      tickColor = "#999999",
      gridLineWidth = 1
    ) %>%

    # Configuração do eixo Y (eixo vertical)
    hc_yAxis(labels = list(formatter = y_formatter)) %>%

    # Configuração da série de dados
    hc_add_series(
      data = radar_data,
      mapping = hcaes(y = Measure,
                      name = Ano,
                      group = Nome_Regiao),
      type = "line",
      marker = list(symbol = "circle"),
      color = Cor$Cor
    ) %>%

    # Configuração do Tooltip
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_text) %>%

    # Configuração da legenda
    hc_legend(enabled = TRUE,
              align = "center",
              layout = "horizontal") %>%

    # Tema e Exportação
    hc_add_theme(hc_theme_smpl()) %>%
    hc_exporting(enabled = TRUE)
}

