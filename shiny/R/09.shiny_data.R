# Carregando pacotes
source("./R/00.library.R")

# Carregando arquivos
source("./R/07.shiny_loadingfiles.R")
source("./R/08.shiny_functions.R")

# # 1ª Pagina (Overview) ----
# 
# ## Gráficos ----
# 
# ### Gráfico de linhas  (g_11) ----
# 
# g_111 <- data1 %>%
#   group_by(Ano, Nome_Regiao, Nome_Bloco) %>%
#   summarise(
#     US_FOB = sum(US_FOB, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop') %>%
#   mutate(Ano = as.numeric(Ano)) %>%
#   ungroup()   
# 
# g_111a <- g_111 %>%
#   group_by(Ano, Nome_Regiao) %>%
#   summarise(
#     US_FOB = sum(US_FOB, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop') %>%
#   mutate(Cor = sapply(Nome_Regiao, function(x) get_color("region", x))) %>%
#   ungroup()
# 
# g_111b <- g_111 %>%
#   group_by(Ano, Nome_Bloco) %>%
#   summarise(
#     US_FOB = sum(US_FOB, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   rename(Nome_Regiao = Nome_Bloco) %>%
#   mutate( Cor = sapply(Nome_Regiao, function(x) get_color("region", x))) %>%
#   ungroup()
# 
# ### Gráfico maps das exportações ----
# g_121 <- data1 %>%
#   filter(Ano == 2020) %>%
#   group_by(CO_Pais_ISOA3, Nome_Pais) %>%
#   summarise(
#     US_FOB = sum(US_FOB, na.rm = TRUE),
#     .groups = 'drop') %>%
#   ungroup()   
# 
# # 2ª Pagina (Por Região) ----
# 
# ### Gráfico maps das exportações ----
# 
# g_211 <- data2 %>%
#   group_by(Ano, Nome_Regiao, Nome_Bloco) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop') %>%
#   ungroup()
# 
# g_212 <- data2 %>%
#   group_by(Ano, Nome_Estado, Nome_Regiao) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     peso = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop') %>%
#   ungroup()
# 
# 
# # 3ª Pagina (por Destino) ----
# g_311 <- data3 %>%
#   group_by(Ano, Nome_Pais, Nome_Bloco) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
#     
# g_312 <- data3 %>%
#   group_by(Ano, Nome_Regiao, Nome_Bloco) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
#   
# # 4ª Página ----
# 
# ### Gráfico de evolução das exportações por Região  (g_411) ----
# 
# g_411 <- data4 %>%
#   group_by(Ano, Nome_Regiao, Nome_ISIC_Secao) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido / 1e6, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(Ano = as.numeric(Ano),
#          Cor = sapply(Nome_Regiao, function(x)
#            get_color("region", x))) %>%
#   ungroup()
# 
# g_412 <- data4 %>%
#   group_by(Ano, Nome_Bloco, Nome_ISIC_Secao) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup() %>%
#   mutate(Ano = as.numeric(Ano),
#          Cor = sapply(Nome_Bloco, function(x)
#            get_color("region", x))) %>%
#   rename(Nome_Regiao = Nome_Bloco)  %>%
#   ungroup()
# 
# ### Treemanp ----
# g_413 <- data4 %>%
#   group_by(Ano, Nome_ISIC_Secao, Nome_SH2, Nome_SH4) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(Cor = sapply(Nome_ISIC_Secao, function(x)
#     get_color("isic", x))) %>%
#   ungroup()
# 
# # 5ª Pagina ----
# 
# ## por Município ----
# # g_511 <- data5a %>%
# #   group_by(Ano, CO_Municipio, Cidade, Nome_ISIC_Secao) %>%
# #   summarise(
# #     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
# #     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
# #     .groups = 'drop') 
# 
# g_512 <- data5a %>%
#   group_by(Ano,
#            Cidade,
#            Nome_Pais,
#            Lat_Municipio,
#            Long_Municipio,
#            Lat_PAIS,
#            Long_PAIS) %>%
#   summarise(
#     US_FOB = sum(US_FOB, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup() 
# 
# g_513 <- data5a %>%
#   group_by(Ano, Cidade, Nome_Bloco, Nome_ISIC_Secao) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
# 
# ## por Estado ----
# g_521 <- data5b %>%
#   group_by(Ano, Sigla_UF, Nome_Estado, Nome_ISIC_Secao) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
# 
# g_522 <- data5b %>%
#   group_by(Ano, Nome_Estado, Nome_ISIC_Secao, Nome_SH2) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
# 
# # g_523 <- data5b %>% 
# #   group_by(Ano, Nome_Estado, Nome_Bloco, Nome_Pais, Nome_SH2) %>% ### Nome_SH4
# #   summarise(
# #     US_FOB = sum(US_FOB, na.rm = TRUE),
# #     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
# #     .groups = 'drop'
# #   )
# 
# ## por País ----
# # g_531 <- data5c %>%
# #   group_by(Ano, Nome_Pais) %>%
# #   summarise(
# #     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
# #     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
# #     .groups = 'drop'
# #   ) %>%
# #   ungroup()
# 
# g_532 <- data5c %>%
#   group_by(Ano, Nome_Bloco, Nome_Pais, Nome_ISIC_Secao) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(Cor = sapply(Nome_Bloco, function(x)
#     get_color("region", x))) %>%
#   ungroup()
# 
# g_533 <- data5c %>%
#   group_by(Ano, Nome_Pais, Nome_ISIC_Secao, Nome_Bloco) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(Cor = sapply(Nome_Bloco, function(x)
#     get_color("region", x))) %>%
#   ungroup()
# 
# ## por Produto ----
# 
# g_541 <- data5d %>%
#   group_by(Ano, Nome_Regiao, Nome_ISIC_Secao, Nome_SH2) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     Mean_Price = ifelse(Peso_Liquido > 0, (US_FOB * 1e6) / Peso_Liquido, NA),
#     .groups = 'drop'
#   ) %>%
#   mutate(Cor = sapply(Nome_ISIC_Secao, function(x)
#     get_color("isic", x))) %>%
#   ungroup()
# 
# g_542 <- data5d %>%
#   group_by(Ano, Nome_Estado, Nome_Pais, Nome_ISIC_Secao) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(Cor = sapply(Nome_ISIC_Secao, function(x)
#     get_color("isic", x))) %>%
#   ungroup()
# 
# # g_543a <- data5d %>%
# #   group_by(Ano, Nome_ISIC_Secao, Nome_Regiao) %>%
# #   summarise(
# #     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
# #     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
# #     .groups = 'drop'
# #   ) %>%
# #   ungroup()
# # 
# # g_543b <- data5d %>%
# #   group_by(Ano, Nome_ISIC_Secao, Nome_Bloco) %>%
# #   summarise(
# #     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
# #     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
# #     .groups = 'drop'
# #   ) %>%
# #   ungroup()
# 
# ## Comparabilidade ----
# 
# g_551 <- data5e %>%
#   group_by(Ano, Cidade) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
# 
# g_552 <- data5e %>%
#   group_by(Ano, Nome_Estado) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
# 
# g_553 <- data5e %>%
#   group_by(Ano, Nome_Pais) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
# 
# ## Analise ----
# # g_561 <- data5f %>%
# #   group_by(Ano, Trimestre, Nome_Estado, Nome_ISIC_Secao) %>%
# #   summarise(US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
# #             .groups = 'drop') %>%
# #   mutate(Ano_Trimestre = paste(Ano, Trimestre, sep = "-")) %>%
# #   ungroup()
# 
# g_562 <- data5f %>%
#   group_by(Ano, Nome_Estado, Nome_SH2) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   ungroup()
# 
# g_563 <- data5f %>%
#   group_by(Ano, Nome_Regiao, Nome_SH2) %>%
#   summarise(
#     US_FOB = sum(US_FOB / 1e6, na.rm = TRUE),
#     Peso_Liquido = sum(Peso_Liquido, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   mutate(Cor = sapply(Nome_Regiao, function(x)
#     get_color("region", x))) %>%
#   ungroup()

# Salvando Arquivos ----
# saveRDS(g_111a, file = "./files/shiny_files/g_111a.rds")
# saveRDS(g_111b, file = "./files/shiny_files/g_111b.rds")
# saveRDS(g_121, file = "./files/shiny_files/g_121.rds")
# saveRDS(g_211, file = "./files/shiny_files/g_211.rds")
# saveRDS(g_212, file = "./files/shiny_files/g_212.rds")
# saveRDS(g_311, file = "./files/shiny_files/g_311.rds")
# saveRDS(g_312, file = "./files/shiny_files/g_312.rds")
# saveRDS(g_411, file = "./files/shiny_files/g_411.rds")
# saveRDS(g_412, file = "./files/shiny_files/g_412.rds")
# saveRDS(g_413, file = "./files/shiny_files/g_413.rds")
# saveRDS(g_512, file = "./files/shiny_files/g_512.rds")
# saveRDS(g_513, file = "./files/shiny_files/g_513.rds")
# saveRDS(g_521, file = "./files/shiny_files/g_521.rds")
# saveRDS(g_522, file = "./files/shiny_files/g_522.rds")
# saveRDS(g_532, file = "./files/shiny_files/g_532.rds")
# saveRDS(g_533, file = "./files/shiny_files/g_533.rds")
# saveRDS(g_541, file = "./files/shiny_files/g_541.rds")
# saveRDS(g_542, file = "./files/shiny_files/g_542.rds")
# saveRDS(g_551, file = "./files/shiny_files/g_551.rds")
# saveRDS(g_552, file = "./files/shiny_files/g_552.rds")
# saveRDS(g_553, file = "./files/shiny_files/g_553.rds")
# saveRDS(g_562, file = "./files/shiny_files/g_562.rds")
# saveRDS(g_563, file = "./files/shiny_files/g_563.rds")
