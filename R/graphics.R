# Carregar as bibliotecas necessárias
library(highcharter)
library(dplyr)

# Agrupar por Ano e Bloco Econômico (ou pode ser por Estado, dependendo do que você quer visualizar)
g1 <- g1 %>%
  group_by(Ano, Nome_Bloco) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop')


# Gerar o gráfico
# Gerar o gráfico com séries separadas e cores por região
hc_g1 <- highchart() %>%
  hc_chart(
    zoomType = "x",
    backgroundColor = "#D3D8DF"
  ) %>%
  hc_add_series_list(
    lapply(split(g1_economic_block, g1_economic_block$Nome_Regiao), function(data) {
      list(
        data = list_parse(data.frame(
          x = as.numeric(data$Ano),
          y = round(data$US_FOB, 2)
        )),
        name = unique(data$Nome_Regiao),
        color = get_region_color(unique(data$Nome_Regiao)),
        marker = list(symbol = 'circle'),
        type = "line",
        lineWidth = 2.5
      )
    })
  ) %>%
  hc_tooltip(
    pointFormatter = JS("function() {
      var y = this.y;
      if (y >= 1e9) {
        return '<b>' + this.series.name + '</b><br/>Ano: ' + this.x + '<br/>Valor (US$): ' + (y / 1e9).toFixed(1) + ' bi';
      } else if (y >= 1e6) {
        return '<b>' + this.series.name + '</b><br/>Ano: ' + this.x + '<br/>Valor (US$): ' + (y / 1e6).toFixed(1) + ' mi';
      } else if (y >= 1e3) {
        return '<b>' + this.series.name + '</b><br/>Ano: ' + this.x + '<br/>Valor (US$): ' + (y / 1e3).toFixed(1) + ' mil';
      } else {
        return '<b>' + this.series.name + '</b><br/>Ano: ' + this.x + '<br/>Valor (US$): ' + y.toFixed(0);
      }
    }")
  ) %>%
  hc_title(text = "Exportações por Ano e Bloco Econômico") %>%
  hc_xAxis(
    title = list(text = "Ano"),
    tickInterval = 1,
    lineColor = "#999999"
  ) %>%
  hc_yAxis(
    title = list(text = "Valor das Exportações (US$)"),
    gridLineColor = "#999999",
    labels = list(
      formatter = JS("function() {
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
    )
  ) %>%
  hc_exporting(enabled = TRUE)

# Exibir o gráfico
hc_g1



ggplot() +
  geom_sf(
    data = g2_country,
    aes(fill = log_US_FOB),
    color = "gray",
    size = 0.1
  ) +  # Mapa dos países
  geom_sf(
    data = g2_city,
    aes(size = log_US_FOB, color = log_US_FOB),
    alpha = 0.7) +  # Mapa das cidades brasileiras
  scale_fill_gradientn(colors = rev(pal_world(g2_country$log_US_FOB))) +  # Usando paleta contínua para países
  scale_color_gradientn(colors = rev(pal_brazil(g2_city$log_US_FOB))) +  # Usando paleta contínua para cidades
  coord_sf(crs = "+proj=laea +lat_0=-15 +lon_0=-55") +  # Projeção Mollweide com Brasil centralizado
  theme_void() +
  theme(legend.position = "none")

ggplot() +
  geom_sf(
    data = g2_country,
    aes(fill = log_US_FOB),
    color = "gray",
    size = 0.1
  ) +  # Mapa dos países
  geom_sf(
    data = g2_city,
    aes(size = log_US_FOB, color = log_US_FOB),
    alpha = 0.7) +  # Mapa das cidades brasileiras
  scale_fill_gradientn(colors = rev(pal_world(g2_country$log_US_FOB))) +  # Usando paleta contínua para países
  scale_color_gradientn(colors = rev(pal_brazil(g2_city$log_US_FOB))) +  # Usando paleta contínua para cidades
  # coord_sf(crs = "+proj=moll +lon_0=-55") +  # Projeção Mollweide com Brasil centralizado
  theme_void() +
  theme(legend.position = "none")



# Supondo que você já tenha um dataframe chamado g_21

# Filtrando o dataframe para Ano == 2020 e Nome_Regiao == "Sul"
g_21_filtered <- g_21[g_21$Ano == 2020 & g_21$Nome_Regiao == "Sul", ]

# Exibindo o dataframe filtrado
print(g_21_filtered)
str(g_21_filtered)

# Carregar os pacotes necessários
library(leaflet)
library(sf)

# Supondo que g_21_filtered seja um objeto sf (Simple Features)
# Certifique-se de que o objeto tenha a classe correta sf
if (!inherits(g_21_filtered, "sf")) {
  g_21_filtered <- st_as_sf(g_21_filtered)
}

# Criar o mapa com o leaflet
leaflet(data = g_21_filtered) %>%
  # Adiciona uma camada base (mapa)
  addTiles() %>%
  # Adiciona as geometrias (polígonos) das regiões filtradas
  addPolygons(
    fillColor = "blue",            # Usar a cor definida no dataframe
    color = ~cor,             # Cor da borda dos polígonos
    weight = 1,                  # Espessura da borda
    opacity = 1,                 # Opacidade da borda
    fillOpacity = 0.7,            # Opacidade do preenchimento
  
    popup = ~paste(
      "<strong>Estado:</strong>", Nome_Estado, "<br>",
      "<strong>Exportações (US$ FOB):</strong>", US_FOB, "<br>",
      "<strong>Peso Líquido (kg):</strong>", Peso_Liquido
    )  # Pop-up ao clicar nos polígonos
  ) %>%
  # Definir a visualização inicial baseada nas coordenadas das geometrias
  setView(lng = -51, lat = -23, zoom = 4)

