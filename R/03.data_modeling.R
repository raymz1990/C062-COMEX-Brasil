# Carregando dados anteriores ----
source("./R/00.functions.R")
source("./R/01.data.R")
source("./R/02.support_tables.R")

# Tratamento tabelas ----

## tb_product ----
colnames(tb_product)

### Contagem de números de linhas. Serão feitas novas tabelas para valores únicos
qtde_linhas(tb_product)

### Criação de novas tabelas ----
# Lista das tabelas a serem tratadas
tb_list <- list(
  tb_ncm = select(tb_product, CO_NCM, NO_NCM_POR),
  tb_sh6 = select(tb_product, CO_SH6, NO_SH6_POR),
  tb_sh4 = select(tb_product, CO_SH4, NO_SH4_POR),
  tb_sh2 = select(tb_product, CO_SH2, NO_SH2_POR)
)

# Aplicar distinct e contar linhas para cada tabela
tb_list <- lapply(tb_list, function(tb) {
  tb <- tb %>% distinct()
  cat("Número de linhas únicas:", nrow(tb), "\n")
  return(tb)
})

# Extraindo tabelas individuais de volta
tb_ncm <- tb_list$tb_ncm
tb_sh6 <- tb_list$tb_sh6
tb_sh4 <- tb_list$tb_sh4
tb_sh2 <- tb_list$tb_sh2

### tb_product ----
#### será reduzido o número de colunas, deixando somente os códigos de identificação.
#### ****PK é o CO_NCM****
colnames(tb_product)

tb_product <- tb_product %>% 
  select(CO_NCM, CO_SH6, CO_SH4, CO_SH2)

## tb_divisao ----
colnames(tb_divisao)

### Contagem de números de linhas. Serão feitas novas tabelas para valores únicos
qtde_linhas(tb_divisao)

### tb_section ----
tb_section <- tb_divisao %>%
  select(CO_ISIC_SECAO, NO_ISIC_SECAO) 

#### Remoção de linhas duplicadas e contagem de número de linhas
tb_section <- remover_duplicatas(tb_section)

### tb_divisao ----
tb_divisao <- select(
  tb_divisao, CO_NCM, CO_ISIC_SECAO) 

#### Remoção de linhas duplicadas e contagem de número de linhas
tb_divisao <- remover_duplicatas(tb_divisao)

## tb_city ----
### Adicionando colunas 'City"
tb_city <- tb_city %>%
  mutate(City = paste(NO_MUN_MIN, SG_UF, sep = " - "))

### Adicionando LAT e LONG ----
#### Carregando mapas de library(geobr)
map_city <- read_municipality(
  year = 2020,
  showProgress = FALSE
)

# Calcular os centroides (pontos centrais de cada polígono)
centroides <- st_centroid(map_city)

# Extrair a latitude e longitude dos centroides
centroides_coords <- st_coordinates(centroides)

# Adicionar as colunas de latitude e longitude ao objeto 'centroides'
map_city$longitude <- centroides_coords[, 1]  # Longitude
map_city$latitude <- centroides_coords[, 2]   # Latitude

#### Selecionando colunas
colnames(map_city)

map_city <- map_city %>%
  st_drop_geometry() %>%
  select(code_muni, latitude, longitude)

#### convertendo as colunas corretamente para character
map_city <- map_city %>%
  mutate(code_muni = as.character(code_muni))

tb_city <- tb_city %>%
  mutate(CO_MUN_GEO = as.character(CO_MUN_GEO))

### Left_join entre tb_city e map_city
tb_city <- tb_city %>%
  left_join(map_city,
            by = c("CO_MUN_GEO" = "code_muni"))

## tb_state ----
### Adicionando LAT e LONG ----
#### Carregando mapas
map_state <- read_state(
  year = 2020,
  showProgress = FALSE
)

#### Selecionando colunas
head(map_state)

#### Por conter as mesmas informações, a tb_state será alterada com as 
## informações de map_state
tb_state <- map_state %>%
  st_drop_geometry() %>%
  select(code_state, abbrev_state, name_state, code_region, name_region)


#### convertendo as colunas corretamente para character
tb_state <- tb_state %>%
  mutate(code_state = as.character(code_state)) %>%
  mutate(code_region = as.character(code_region))

### tb_state_region tb_state1 <- tb_state

# Função para capitalizar a primeira letra de cada palavra, exceto palavras como "do", "da", etc.
capitalize <- function(x) {
  x <- tolower(x)
  words <- unlist(strsplit(x, " "))
  exceptions <- c("do", "da", "de", "dos", "das")
  words <- ifelse(words %in% exceptions, words, paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = ""))
  paste(words, collapse = " ")
}

# Aplicar a função à coluna NO_MUN
tb_state$name_state <- sapply(tb_state$name_state, capitalize)

## tb_country ----

### Adicionando LAT e LONG ----
## Carregar shapefile dos países do mundo
world_sf <- read_sf("./maps/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")
world_sf <- select(world_sf, ISO3, LON, LAT) %>%
  st_drop_geometry()

#### Alteração Nome País ----
# Alterar os valores da coluna ISOA3 com base em NO_PAIS
tb_country <- tb_country %>%
  mutate(NO_PAIS = case_when(
    NO_PAIS == "Países Baixos (Holanda)" ~ "Países Baixos",
    NO_PAIS == "Taiwan (Formosa)" ~ "Taiwan",
    TRUE ~ NO_PAIS  # Manter os outros valores como estão
  ))

#### Alteração ISOA3 ----
# Alterar os valores da coluna ISOA3 com base em NO_PAIS
tb_country <- tb_country %>%
  mutate(CO_PAIS_ISOA3 = case_when(
    NO_PAIS == "Mayotte" ~ "MYT",
    NO_PAIS == "São Martinho, Ilha de (parte francesa)" ~ "MAF",
    TRUE ~ CO_PAIS_ISOA3  # Manter os outros valores como estão
  ))

#### Left_join entre tb_country e world_sf
tb_country <- tb_country %>%
  left_join(world_sf, by = c("CO_PAIS_ISOA3" = "ISO3"))

# #### Filtrar para manter apenas geometrias que não são vazias
tb_country <- tb_country %>%
  filter(!NO_PAIS %in% c("Canal, Ilhas do (Guernsey)", "Johnston, Ilhas", "Wake, Ilha"))

### Remoção de dados duplicados ----
#### Encontrar os valores duplicados na coluna CO_PAIS_ISOA3
duplicated_iso <- tb_country %>%
  group_by(CO_PAIS_ISOA3) %>%
  filter(n() > 1) %>%
  ungroup()

duplicated_name <- tb_country %>%
  group_by(NO_PAIS_ING) %>%
  filter(n() > 1) %>%
  ungroup()

tb_country <- tb_country %>%
  filter(CO_PAIS_ISOA3 != "ZZZ")

## tb_block ----
qtde_linhas(tb_block)

### Exclusão de regiões em duplicidade ----
tb_block <- tb_block %>%
  filter(
    !NO_BLOCO_ING %in% c(
      "Association Of Southeast Asian Nations (ASEAN)",
      "European Union (EU)",
      "Andean Community",
      "Southern Common Market (MERCOSUL)"
    ))

### Alteração de nome dos blocos ----
unique(tb_block$NO_BLOCO)

tb_block$NO_BLOCO <-
  gsub("Ásia \\(Exclusive Oriente Médio\\)", "Ásia", tb_block$NO_BLOCO)

tb_block$NO_BLOCO <-
  gsub("América Central e Caribe", "América Central", tb_block$NO_BLOCO)

# tb_block <- tb_block %>%
#   mutate(NO_BLOCO = ifelse(
#     NO_BLOCO == "América Central e Caribe",
#     "América Central",
#     NO_BLOCO
#   ))

unique(tb_block$NO_BLOCO)

### tb_region ----
colnames(tb_block)

tb_region <- tb_block %>%
  select(CO_BLOCO, NO_BLOCO, NO_BLOCO_ING) %>%
  distinct()

### tb_block ----
tb_block <- tb_block %>%
  select(CO_PAIS, CO_BLOCO, NO_BLOCO)

## tb_data ----
head(data)

### Codificando Country ----
data_country1 <- tb_country %>%
  select(CO_PAIS_ISOA3, NO_PAIS_ING)

data <- data %>%
  left_join(data_country1, by = c("Country" = "NO_PAIS_ING"))

# #### Exibir as linhas onde CO_PAIS é NA
# country_empty <- data[is.na(data$CO_PAIS), ]
# unique(country_empty$Country)

### Codificando Cidade ----
data <- data %>%
  left_join(tb_city, by = "City")

### Codificando Economic.Block ----
data <- data %>%
  left_join(tb_region, by = c("Economic Block" = "NO_BLOCO_ING"))

### Reorganizando as colunas ----
colnames(data)

data <- data %>%
  select(Year,
         Quarter,
         CO_MUN_GEO,
         CO_PAIS_ISOA3,
         `SH2 Code`,
         `SH4 Code`,
         CO_BLOCO,
         US_FOB,
         Net_Weight)

#### Remover todas as linhas com NA em CO_PAIS_ISOA3
data <- data %>%
  filter(!is.na(CO_PAIS_ISOA3))

## Verificando valores `NA` ----

# Aplicar a função para as tabelas
# verificar_na(tb_country, "CO_PAIS")
# verificar_na(tb_state, "CO_Estado")
# verificar_na(tb_city, "CO_MUN_GEO")
# verificar_na(data, "CO_PAIS_ISOA3")

## Tratamento final ----
### Alteração do nomes das colunas e exclusão de linhas

# Aplicando a função para várias tabelas

### data ----
data <-
  renomear_colunas(
    data, c(
      "Ano",              # Year
      "Trimestre",        # Quarter
      "CO_Municipio",     # CO_MUN_GEO
      "CO_Pais_ISOA3",    # CO_PAIS_ISOA3
      "CO_SH2",           # SH2.Code
      "CO_SH4",           # SH2.Code
      "CO_Bloco",         # CO_BLOCO
      "US_FOB",           # US_FOB
      "Peso_Liquido"      # Net_Weight
      ))

#### Alteração coluna 'Ano'
data$Ano <- as.character(data$Ano)

#### Verificação dos dados da tabela
class(data)

### tb_city ----
tb_city <-
  renomear_colunas(
    tb_city, c(
      "CO_Municipio",   # CO_MUN_GEO
      "Nome_Municipio", # NO_MUN_MIN
      "Sigla_UF",       # SG_UF
      "Cidade",         # City
      "Lat_Municipio",
      "Long_Municipio"#,         
      # "Geometria_Mun"   # geom
      ))

#### Verificação dos dados da tabela
class(tb_city)

### tb_state ----
tb_state <-
  renomear_colunas(
    tb_state, c(
      "CO_Estado",    # code_state
      "Sigla_UF", # abbrev_state
      "Nome_Estado",  # name_state
      "CO_Regiao",    # code_region
      "Nome_Regiao"#,  # name_region
      # "Geometria_Est" # geom
      )) 

# st_geometry(tb_state) <- "Geometria_Est"

#### Verificação dos dados da tabela
class(tb_state)

### tb_country ----
tb_country <-
  renomear_colunas(
    tb_country, c(
      "CO_Pais",         # CO_PAIS
      "CO_Pais_ISON3",   # CO_PAIS_ISON3
      "CO_Pais_ISOA3",   # CO_PAIS_ISOA3
      "Nome_Pais",       # NO_PAIS
      "Nome_Pais_ING",   # NO_PAIS_ING
      "Long_PAIS",
      "Lat_PAIS"#,
      # "Geometria_Pais"   # geometry
    ))

### áreas de controle. Sendo a ISO referente ao país do qual fazem parte.
tb_country <- tb_country %>%
  filter(!Nome_Pais %in% c(
    "Dubai",
    "Alemanha Oriental",
    "Alboran-Perejil, Ilhas",
    "Canárias, Ilhas",
    "Canal, Ilhas do (Jersey)",
    "Inglaterra",
    "Saint Kitts e Nevis",
    "Lebuan, Ilhas",
    "Papua, Território de",
    "Madeira, Ilha da",
    "Pacífico, Ilhas do (EUA)",
    "Pacífico, Ilhas do (Território Fideicomisso EUA)",
    "Johnston, Ilhas",
    "Pacífico, Ilhas do (Adminiclassação dos EUA)",
    "Wake, Ilha",
    "Midway, Ilhas",
    "Território Antártico Britânico",
    "Internação na Zona Franca de Manaus"))

# #### Filtrar para manter apenas geometrias que não são vazias
# tb_country1 <- subset(tb_country, !st_is_empty(geometry))

data_country1 <- tb_country %>%
  group_by(Nome_Pais, CO_Pais_ISOA3) %>%
  summarise(Total = n(), .groups = 'drop')

data_country1 %>%
  group_by(CO_Pais_ISOA3) %>%
  summarise(Total = n(), .groups = 'drop') %>%
  filter(Total > 1) %>%
  {
    if (nrow(.) == 0) {
      cat("Não há países em duplicidade.\n")
    } else {
      cat("Países com mais de uma ocorrência:\n", .$CO_Pais_ISOA3, sep = "\n")
    }
  }

#### Verificação dos dados da tabela
class(tb_country)

### tb_block ----
tb_block <- tb_block %>%
  select(CO_BLOCO, NO_BLOCO) %>%
  distinct()

tb_block <-
  renomear_colunas(
    tb_block, c(
      # "CO_Pais",   # CO_PAIS
      "CO_Bloco",  # CO_BLOCO
      "Nome_Bloco" # NO_BLOCO
    ))

#### Verificação dos dados da tabela
class(tb_block)

### tb_region ----
tb_region <-
  renomear_colunas(
    tb_region, c(
      "CO_Bloco",        # CO_BLOCO
      "Nome_Bloco",      # NO_BLOCO
      "Nome_Bloco_ING"   # NO_BLOCO_ING
    ))

#### Verificação dos dados da tabela
class(tb_region)

### tb_product ----
tb_product <-
  renomear_colunas(
    tb_product, c(
      "CO_NCM",   # CO_NCM
      "CO_SH6",   # CO_SH6
      "CO_SH4",   # CO_SH4
      "CO_SH2"    # CO_SH2
    ))

#### Verificação dos dados da tabela
class(tb_product)

### tb_ncm ----
tb_ncm <-
  renomear_colunas(
    tb_ncm, c(
      "CO_NCM",     # CO_NCM
      "Nome_NCM"    # NO_NCM_POR
    ))

#### Verificação dos dados da tabela
class(tb_ncm)

### tb_sh2 ----
tb_sh2 <-
  renomear_colunas(
    tb_sh2, c(
      "CO_SH2", # CO_SH2
      "Nome_SH2" # NO_SH2_POR
    ))

#### Verificação dos dados da tabela
class(tb_sh2)

### tb_sh4 ----
tb_sh4 <-
  renomear_colunas(
    tb_sh4, c(
      "CO_SH4",  # CO_SH4
      "Nome_SH4" # NO_SH4_POR
    ))

#### Verificação dos dados da tabela
class(tb_sh4)

### tb_sh6 ----
tb_sh6 <-
  renomear_colunas(
    tb_sh6, c(
      "CO_SH6",  # CO_SH6
      "Nome_SH6" # NO_SH6_POR
    ))

#### Verificação dos dados da tabela
class(tb_sh6)

### tb_divisao ----
tb_divisao <-
  renomear_colunas(
    tb_divisao, c(
      "CO_NCM",         # CO_NCM
      "CO_ISIC_Secao"   # CO_ISIC_SECAO
    ))

#### Verificação dos dados da tabela
class(tb_divisao)

### tb_section ----
tb_section <-
  renomear_colunas(
    tb_section, c(
      "CO_ISIC_Secao",   # CO_ISIC_SECAO
      "Nome_ISIC_Secao"  # NO_ISIC_SECAO
    ))

#### Verificação dos dados da tabela
class(tb_section)

## Remover objetos temporários ----
rm(
  list = c(
    "centroides",
    "word_sf",
    "map_city",
    "duplicated_iso",
    "duplicated_name",
    "map_state",
    "data_country1"
  )
)

