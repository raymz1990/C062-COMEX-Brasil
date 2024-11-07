# Carregando pacotes ----
source("./R/00.library.R")

# Carregando arquivos ----
source("./R/05.loading_files.R")

data$Peso_Liquido <- data$Peso_Liquido / 1000

## carregando geometria ----
# geom_city <- tb_city %>%
#   select(CO_Municipio, Geometria_Mun) %>%
#   distinct()
# 
# geom_state <- tb_state %>%
#   select(CO_Estado, Geometria_Est) %>%
#   distinct()
# 
# geom_country <- tb_country %>%
#   filter(CO_Pais_ISOA3 != "BRA") %>%
#   select(CO_Pais_ISOA3, Latitude, Longitude, Geometria_Pais) %>%
#   distinct()

# Criando tabelas ----

## 1ª Pagina (Overview) ----

### dataset principal ----

# Pré-processamento das tabelas auxiliares para reduzir o número de colunas
data1_city <- tb_city %>%
  left_join(tb_state, by = "Sigla_UF") %>%
  select(CO_Municipio, Nome_Regiao) %>%
  distinct()

data1_country <- tb_country %>%
  select(CO_Pais_ISOA3, Nome_Pais) %>%
  distinct()

# data1_state <- tb_state %>%
#   select(Sigla_UF, Nome_Estado)

data1_block <- tb_block %>%
  select(CO_Bloco, Nome_Bloco) %>%
  distinct()

data1 <- data %>%
  left_join(data1_city, by = "CO_Municipio") %>%
  left_join(data1_country, by = "CO_Pais_ISOA3") %>%
  left_join(data1_block, by = "CO_Bloco") %>%
  select(Ano, CO_Municipio, Nome_Pais, Nome_Regiao, CO_Pais_ISOA3, Nome_Bloco, US_FOB, Peso_Liquido)

# Operação principal com junções e agregação
data1 <- data1 %>%
  group_by(Ano, CO_Municipio, CO_Pais_ISOA3, Nome_Pais, Nome_Bloco, Nome_Regiao) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

## 2ª Pagina (Por Região) ----
# Pré-processamento das tabelas auxiliares para reduzir o número de colunas
data2_state <- tb_city %>%
  left_join(tb_state, by = "Sigla_UF") %>%
  select(CO_Municipio, CO_Estado, Sigla_UF, Nome_Estado, Nome_Regiao) %>%
  distinct()

data2_block <- tb_block %>%
  select(CO_Bloco, Nome_Bloco) %>%
  distinct()

# Operação principal com junções e agregação
data2 <- data %>%
  group_by(Ano, CO_Municipio, CO_Bloco) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

data2 <- data2 %>%
  left_join(data2_state, by = "CO_Municipio") %>%
  #  left_join(data1_state, by = "Sigla_UF") %>%
  left_join(data2_block, by = "CO_Bloco") %>%
  select(Ano, CO_Estado, Nome_Estado, Sigla_UF, Nome_Regiao, US_FOB, Peso_Liquido)

## 3ª Pagina (Por Destinos Internacionais) ----
# Pré-processamento das tabelas auxiliares para reduzir o número de colunas
data3_city <- tb_city %>%
  left_join(tb_state, by = "Sigla_UF") %>%
  select(CO_Municipio, Nome_Regiao) %>%
  distinct()

data3_block <- tb_block %>%
  select(CO_Bloco, Nome_Bloco) %>%
  distinct()

data3_country <- tb_country %>%
  select(CO_Pais_ISOA3, Nome_Pais) %>%
  distinct()

# Operação principal com junções e agregação
data3 <- data %>%
  group_by(Ano, CO_Municipio, CO_Bloco, CO_Pais_ISOA3) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )
  
data3 <- data3 %>%  
  left_join(data3_city, by = "CO_Municipio") %>%
  left_join(data3_block, by = "CO_Bloco") %>%
  left_join(data3_country, by ="CO_Pais_ISOA3") %>%
  # left_join(tb_sh2, by = "CO_SH2") %>%
  select(Ano, Nome_Regiao, Nome_Bloco, Nome_Pais, US_FOB, Peso_Liquido) 

## 4ª Pagina (Por Produto) ----
data4_product <- tb_product %>%
  left_join(tb_sh2, by = "CO_SH2") %>%
  left_join(tb_sh4, by = "CO_SH4") %>%
  left_join(tb_divisao, by = "CO_NCM") %>%
  select(CO_SH2, Nome_SH2, CO_SH4, Nome_SH4, CO_ISIC_Secao)

data4_product <- data4_product %>%
  left_join(tb_section, by = "CO_ISIC_Secao") %>%
  select(CO_SH2, Nome_SH2, CO_SH4, Nome_SH4, Nome_ISIC_Secao) %>%
  distinct()
  
# Ver quais linhas têm duplicatas na coluna CO_SH4
duplicated_rows <- data4_product[duplicated(data4_product$CO_SH4) | duplicated(data4_product$CO_SH4, fromLast = TRUE), ]

# Remover a segunda ocorrência das duplicatas na coluna CO_SH4
data4_product <- data4_product[!duplicated(data4_product$CO_SH4), ]

data4_block <- tb_block %>%
  select(CO_Bloco, Nome_Bloco) %>%
  distinct()

data4 <- data %>%
  left_join(data1_city, by = "CO_Municipio") %>%
  left_join(data4_block, by = "CO_Bloco") %>%
  left_join(data4_product, by = "CO_SH4") %>%
  select(Ano, Nome_Regiao, Nome_Bloco, Nome_SH2, Nome_SH4, Nome_ISIC_Secao,
         US_FOB, Peso_Liquido)

data4 <- data4 %>%
  group_by(Ano, Nome_Regiao, Nome_Bloco, Nome_SH2, Nome_SH4, Nome_ISIC_Secao) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  ) 
  


## 5ª Pagina ----

### por municipio ----
data5a_city <- tb_city %>%
  select(CO_Municipio, Cidade, Lat_Municipio, Long_Municipio) %>%
  distinct()

data5a_country <- tb_country %>%
  select(CO_Pais_ISOA3, Nome_Pais, Lat_PAIS, Long_PAIS) %>%
  distinct()

data5a_block <- tb_block %>%
  select(CO_Bloco, Nome_Bloco) %>%
  distinct()

data5a_isic <- data4_product %>%
  select(CO_SH4, Nome_ISIC_Secao) %>%
  distinct()

data5a <- data %>%
  group_by(Ano, CO_Municipio, CO_Pais_ISOA3, CO_SH4, CO_Bloco) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

data5a <- data5a %>%
  left_join(data5a_city, by = "CO_Municipio") %>%
  left_join(data5a_country, by = "CO_Pais_ISOA3") %>%
  left_join(data5a_block, by = "CO_Bloco") %>%
  left_join(data5a_isic, by = "CO_SH4") %>%
  select(Ano, 
         Cidade, Lat_Municipio, Long_Municipio,
         Nome_Pais, Nome_Bloco, Lat_PAIS, Long_PAIS,
         Nome_ISIC_Secao,  
         US_FOB, Peso_Liquido)

### por estado ----
data5b_state <- tb_city %>%
  left_join(tb_state, by = "Sigla_UF") %>%
  select(CO_Municipio, Sigla_UF, Nome_Estado) %>%
  distinct()

data5b_country <- tb_country %>%
  select(CO_Pais_ISOA3, Nome_Pais) %>%
  distinct()

# data5b_block <- tb_block %>%
#   select(CO_Bloco, Nome_Bloco) %>%
#   distinct()

data5b_isic <- data4_product %>%
  select(CO_SH4, Nome_ISIC_Secao) %>%
  distinct()

data5b_sh2 <- data4_product %>%
  select(CO_SH2, Nome_SH2) %>%
  distinct()

data5b <- data %>%
  group_by(Ano, CO_Municipio, CO_Pais_ISOA3, CO_SH2, CO_SH4) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

data5b <- data5b %>%
  left_join(data5b_state, by = "CO_Municipio") %>%
  left_join(data5b_country, by = "CO_Pais_ISOA3") %>%
  # left_join(data5b_block, by = "CO_Bloco") %>%
  left_join(data5b_isic, by = "CO_SH4") %>%
  left_join(data5b_sh2, by = "CO_SH2") %>%
  group_by(Ano, Sigla_UF, Nome_Estado, Nome_Pais, Nome_ISIC_Secao, Nome_SH2) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

### por país ----
data5c_country <- tb_country %>%
  select(CO_Pais_ISOA3, Nome_Pais) %>%
  distinct()

data5c_block <- tb_block %>%
  select(CO_Bloco, Nome_Bloco) %>%
  distinct()

data5c_isic <- data4_product %>%
  select(CO_SH4, Nome_ISIC_Secao) %>%
  distinct()

data5c <- data %>%
  group_by(Ano, CO_Pais_ISOA3, CO_SH4, CO_Bloco) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

data5c <- data5c %>%
  left_join(data5c_country, by = "CO_Pais_ISOA3") %>%
  left_join(data5c_block, by = "CO_Bloco") %>%
  left_join(data5c_isic, by = "CO_SH4") %>%
  group_by(Ano, Nome_Pais, Nome_ISIC_Secao, Nome_Bloco) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

### por produto ----
data5d_state <- tb_city %>%
  left_join(tb_state, by = "Sigla_UF") %>%
  select(CO_Municipio, Cidade, Nome_Estado, Nome_Regiao) %>%
  distinct()

data5d_country <- tb_country %>%
  select(CO_Pais_ISOA3, Nome_Pais) %>%
  distinct()

data5d_block <- tb_block %>%
  select(CO_Bloco, Nome_Bloco) %>%
  distinct()

data5d_isic <- data4_product %>%
  select(CO_SH4, Nome_ISIC_Secao) %>%
  distinct()

data5d_sh2 <- tb_sh2 %>%
  select(CO_SH2, Nome_SH2) %>%
  distinct()

data5d_sh4 <- tb_sh4 %>%
  select(CO_SH4, Nome_SH4) %>%
  distinct()

data5d <- data %>%
  group_by(Ano, CO_Municipio, CO_Pais_ISOA3, CO_SH2, CO_SH4, CO_Bloco) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

data5d <- data5d %>%
  left_join(data5d_state, by = "CO_Municipio") %>%
  left_join(data5d_country, by = "CO_Pais_ISOA3") %>%
  left_join(data5d_block, by = "CO_Bloco") %>%
  left_join(data5d_isic, by = "CO_SH4") %>%
  left_join(data5d_sh2, by = "CO_SH2") %>%
  left_join(data5d_sh4, by = "CO_SH4") %>%
  group_by(Ano, Nome_Estado, Nome_Regiao, Nome_Pais, Nome_ISIC_Secao, Nome_SH2, Nome_Bloco) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

### comparatividade ----
data5e_state <- tb_city %>%
  left_join(tb_state, by = "Sigla_UF") %>%
  select(CO_Municipio, Cidade, Nome_Estado) %>%
  distinct()

data5e_country <- tb_country %>%
  select(CO_Pais_ISOA3, Nome_Pais) %>%
  distinct()

data5e <- data %>%
  group_by(Ano, CO_Municipio, CO_Pais_ISOA3) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

data5e <- data5e %>%
  left_join(data5e_state, by = "CO_Municipio") %>%
  left_join(data5e_country, by = "CO_Pais_ISOA3") %>%
  group_by(Ano, Cidade, Nome_Estado, Nome_Pais) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

### análise ----
data5f_state <- tb_city %>%
  left_join(tb_state, by = "Sigla_UF") %>%
  select(CO_Municipio, Nome_Estado, Nome_Regiao) %>%
  distinct()

data5f_block <- tb_block %>%
  select(CO_Bloco, Nome_Bloco) %>%
  distinct()

data5f_sh2 <- tb_sh2 %>%
  select(CO_SH2, Nome_SH2) %>%
  distinct()

data5f_isic <- data4_product %>%
  select(CO_SH4, Nome_ISIC_Secao) %>%
  distinct()

data5f <- data %>%
  group_by(Ano, Trimestre, CO_Municipio, CO_Bloco, CO_SH2, CO_SH4) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

data5f <- data5f %>%
  left_join(data5f_state, by = "CO_Municipio") %>%
  left_join(data5f_block, by = "CO_Bloco") %>%
  left_join(data5f_sh2, by = "CO_SH2") %>%
  left_join(data5f_isic, by = "CO_SH4") %>%
  group_by(Ano, Trimestre, Nome_Estado, Nome_Regiao, Nome_Bloco, Nome_SH2, Nome_ISIC_Secao) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )

## 6ª Página ----
data6_city <- tb_city %>%
  select(CO_Municipio, Nome_Municipio) %>%
  distinct()

data6_state <- tb_city %>%
  left_join(tb_state, by = "Sigla_UF") %>%
  select(CO_Municipio, Nome_Estado, Nome_Regiao) %>%
  distinct()

data6_country <- tb_country %>%
  select(CO_Pais_ISOA3, Nome_Pais) %>%
  distinct()

data6_block <- tb_block %>%
  select(CO_Bloco, Nome_Bloco) %>%
  distinct()

data6_sh2 <- tb_sh2 %>%
  select(CO_SH2, Nome_SH2) %>%
  distinct()

data6_isic <- data4_product %>%
  select(CO_SH4, Nome_ISIC_Secao) %>%
  distinct()

data6 <- data %>%
  left_join(data6_city, by = "CO_Municipio") %>%
  left_join(data6_state, by = "CO_Municipio") %>%
  left_join(data6_country, by = "CO_Pais_ISOA3") %>%
  left_join(data6_sh2, by = "CO_SH2") %>%
  left_join(data6_isic, by = "CO_SH4") %>%
  left_join(data6_block, by = "CO_Bloco") %>%
  group_by(# Ano, 
           # Trimestre, 
           Nome_Municipio,
           Nome_Estado,  
           Nome_Pais,  
           Nome_SH2) %>%
  summarise(
    US_FOB = sum(US_FOB, na.rm = TRUE),
    Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
    .groups = 'drop'
  )


# Salvando Arquivos ----
saveRDS(geom_city, file = "./files/shiny_files/geom_city.rds")
saveRDS(geom_state, file = "./files/shiny_files/geom_state.rds", compress = TRUE)
saveRDS(geom_country, file = "./files/shiny_files/geom_country.rds")
saveRDS(data1, file = "./files/shiny_files/data1.rds", compress = TRUE)
saveRDS(data2, file = "./files/shiny_files/data2.rds", compress = TRUE)
saveRDS(data3, file = "./files/shiny_files/data3.rds", compress = TRUE)
saveRDS(data4, file = "./files/shiny_files/data4.rds", compress = TRUE)
saveRDS(data5a, file = "./files/shiny_files/data5a.rds", compress = TRUE)
saveRDS(data5b, file = "./files/shiny_files/data5b.rds", compress = TRUE)
saveRDS(data5c, file = "./files/shiny_files/data5c.rds", compress = TRUE)
saveRDS(data5d, file = "./files/shiny_files/data5d.rds", compress = TRUE)
saveRDS(data5e, file = "./files/shiny_files/data5e.rds", compress = TRUE)
saveRDS(data5f, file = "./files/shiny_files/data5f.rds", compress = TRUE)
saveRDS(data6, file = "./files/shiny_files/data6.rds", compress = TRUE)

