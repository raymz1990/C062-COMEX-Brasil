# Carregando bibliotecas ----
source("./R/00.library.R")

# Carregando funções -----
source("./R/00.functions.R")

# Extração dos arquivos ----

## dataset principal ----
data <- read_csv(unz("./data/archive.zip", "exportacao_full.csv"))

str(data)

# Transformação data ----

## Escolhendo colunas ----

### Seleção de colunas e exclusão dos dados de 2010 
data <- data %>%
  select(Year,
         Month,
         Country,
         City,
         `SH2 Code`,
         `SH4 Code`,
         `Economic Block`,
         `US$ FOB`,
         `Net Weight`) %>%
  filter(Year != 2010)

## Formatação ----

### Coluna `SH2 Code` em 'char' e adicionando zero à esquerda quando necessário
data <- data %>%
  mutate(`SH2 Code` = sprintf("%02d", as.integer(`SH2 Code`)))

### Coluna SH4.Code em 'char' e adicionando zero à esquerda se necessário
data <- data %>%
  mutate(`SH4 Code` = sprintf("%04d", as.integer(`SH4 Code`)))

## Novas colunas ----

### Coluna Trimestre
data <- mutate(data, Quarter = paste0(0, ceiling(Month / 3)))

## Exclusão de dados duplicados ----

### Percebe-se que o bloco economico está duplicando os dados. Será verificado
### os blocos economicos e países que compõe cada um, para validação da exclusão.

# verificação de números de países únicos para detectar se nenhum país foi excluido a mais depois
cat("O dataset tem", n_distinct(data$Country), "países únicos.\n")

unique(data$`Economic Block`)

### Países Asiáticos ----

#### `Economic Block` = "Asia (minus MIDDLE EAST)"
data1 <- process_economic_block(data, "Asia (minus MIDDLE EAST)")

#### `Economic Block` = "Association Of Southeast Asian Nations (ASEAN)"
data2 <- process_economic_block(data, "Association Of Southeast Asian Nations (ASEAN)")

##### Exclusão de "Association Of Southeast Asian Nations (ASEAN)"
data <- subset(data,
               `Economic Block` != "Association Of Southeast Asian Nations (ASEAN)")

### Países Europeus ----

#### `Economic Block` = "Europe"
data3 <- process_economic_block(data, "Europe")

#### `Economic Block` = "European Union (EU)"
data4 <- process_economic_block(data, "European Union (EU)")

##### Exclusão de "European Union (EU)"
data <- subset(data,
               `Economic Block` != "European Union (EU)")

### Países Africanos ----

#### `Economic Block` = "Africa (minus MIDDLE EAST)"
data5 <- process_economic_block(data, "Africa (minus MIDDLE EAST)")

### Países América Central e Caribe ----

#### `Economic Block` = "Central America and Caribbean"
data6 <- process_economic_block(data, "Central America and Caribbean")

### Países Oceania ----

#### `Economic Block` = "Oceania"
data7 <- process_economic_block(data, "Oceania")

### Países Oriente Médio ----

#### `Economic Block` = "Middle East"
data8 <- process_economic_block(data, "Middle East")

### Países América do Norte ----

##### `Economic Block` = "North America"
data9 <- process_economic_block(data, "North America")

### Países América do Sul ----

#### `Economic Block` = "South America"
data10 <- process_economic_block(data, "South America")

#### `Economic Block` = "Andean Community"
data11 <- process_economic_block(data, "Andean Community")

##### Exclusão de "Andean Community"
data <- subset(data, 
               `Economic Block` != "Andean Community")

#### `Economic Block` = "Southern Common Market (MERCOSUL)"
data12 <- process_economic_block(data, "Southern Common Market (MERCOSUL)")

##### Exclusão de "Southern Common Market (MERCOSUL)"
data <- subset(data, 
               `Economic Block` != "Southern Common Market (MERCOSUL)")

### Resultado final ----

cat("O dataset tem", n_distinct(data$Country), "países únicos.\n")

#### Verificando duplicidade de dados

data13 <- data %>%
  group_by(Country, `Economic Block`) %>%
  summarise(Total = n(), .groups = 'drop')

data13 %>%
  group_by(Country) %>%
  summarise(Total = n(), .groups = 'drop') %>%
  filter(Total > 1) %>%
  {
    if (nrow(.) == 0) {
      cat("Não há países em duplicidade.\n")
    } else {
      cat("Países com mais de uma ocorrência:\n", .$Country, sep = "\n")
    }
  }

### Município Não Declarado ----

#### City = "Município Não Declarado - ND"
data_ND <- subset(data,
                  City == "Município Não Declarado - ND")

### Município Exterior - EX ----

##### City = "Exterior - EX"
data_EX <- subset(data, 
                  City == "Exterior - EX")

#### Exclusão de variáveis relacionadas a "Município Não Declarado" e "Exterior"
data <- data %>%
  filter(City != "Município Não Declarado - ND" &
           City != "Exterior - EX")

## Exclusão de valores nulos em `US$ FOB` e `Net Weight` ----

data_valor <- subset(data, `US$ FOB` == 0)

data_peso <- subset(data, `Net Weight` == 0)

#### Exclusão de linhas com valor = 0 ----
data <- data %>%
  filter(`US$ FOB` != 0, `Net Weight` != 0)

## Agrupamento de dados ----

data <- data %>%
  group_by(Year, Quarter, Country, City, `SH2 Code`, `SH4 Code`, `Economic Block`) %>%
  summarise(
    US_FOB = sum(`US$ FOB`, na.rm = TRUE),
    Net_Weight = sum(`Net Weight`, na.rm = TRUE),
    .groups = 'drop') 

str(data)

## Limpeza do ambiente ----
rm(list = ls(pattern = "data[1-9]|data1[0-3]|data_EX|data_ND|data_valor|data_peso"))

