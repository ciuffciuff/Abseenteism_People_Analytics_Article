library(sparklyr)
library(dplyr)
library(readxl)
library(lubridate)
library(tidyverse)

# Conectando ao Spark
sc <- spark_connect(master = "local")

# Carregar os arquivos Excel para data frames do R
BD_APEX_SUM_R <- read_excel("MDM/BD_APEX_SUM.xlsx", 
                            col_types = c("skip", "skip", "skip", 
                                          "skip", "skip", "skip", "text", "date", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "text", "text", "date", 
                                          "numeric", "text", "text", "text", 
                                          "numeric", "numeric", "skip", "text", 
                                          "text", "skip", "skip", "skip", "skip", 
                                          "skip", "skip", "skip", "skip", "skip", 
                                          "skip", "skip", "skip", "skip", "skip", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric"))
ATESTADOS_2019_2023_R <- read_excel("MDM/ATESTADOS_2019_2023.xlsx", 
                                    col_types = c("skip", "text", "skip", 
                                                  "skip", "text", "skip", "skip", "text", 
                                                  "text", "text", "text", "text", 
                                                  "date", "date", "skip", "skip", "skip", 
                                                  "numeric"))

# Copiar os data frames para Spark
BD_APEX_SUM <- copy_to(sc, BD_APEX_SUM_R, "BD_APEX_SUM", overwrite = TRUE)
ATESTADOS_2019_2023 <- copy_to(sc, ATESTADOS_2019_2023_R, "ATESTADOS_2019_2023", overwrite = TRUE)


# Limpeza e transformação de BD_APEX_SUM
BD_APEX_SUM <- BD_APEX_SUM %>%
  mutate(GENERO = as.factor(GENERO),
         PONTO_ELETRONICO = as.factor(PONTO_ELETRONICO),
         CONJUGE_COMPANHEIRO = ifelse(is.na(CONJUGE) | is.na(COMPANHEIRO), 0, CONJUGE + COMPANHEIRO),
         CONJUGE_COMPANHEIRO = ifelse(CONJUGE_COMPANHEIRO > 1, 1, CONJUGE_COMPANHEIRO),
         FAIXAS_MINUTOS = case_when(
           TOTAL_MINUTOS <= 5000 ~ "0-5000",
           TOTAL_MINUTOS <= 10000 ~ "5001-10000",
           TOTAL_MINUTOS <= 15000 ~ "10001-15000",
           TOTAL_MINUTOS <= 20000 ~ "15001-20000",
           TRUE ~ "20001-25000"
         )) %>%
  mutate(FAIXAS_SALARIOS = ntile(SALARIO_MENSAL, 5)) %>%
  mutate(FAIXAS_SALARIOS = recode(FAIXAS_SALARIOS,
                                  `1` = "Low",
                                  `2` = "Low-Medium",
                                  `3` = "Medium",
                                  `4` = "Medium-High",
                                  `5` = "High"),
         FAIXA_ETARIA = case_when(
           IDADE < 20 ~ "0-20",
           IDADE < 30 ~ "21-30",
           IDADE < 40 ~ "31-40",
           IDADE < 50 ~ "41-50",
           IDADE < 60 ~ "51-60",
           IDADE < 70 ~ "61-70",
           IDADE < 80 ~ "71-80",
           TRUE ~ "81-100"
         ))

# Remover duplicados e preencher NAs
BD_APEX_SUM <- BD_APEX_SUM %>%
  distinct() %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ifelse(is.na(.), "Desconhecido", .)))

# Limpeza e transformação de ATESTADOS_2019_2023
ATESTADOS_2019_2023 <- ATESTADOS_2019_2023 %>%
  distinct() %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ifelse(is.na(.), "Desconhecido", .)))

# Converta as colunas de data para o formato apropriado
ATESTADOS_2019_2023 <- ATESTADOS_2019_2023 %>%
  mutate(DATA_INICIO = as.Date(DATA_INICIO),
         DATA_TERMINO = as.Date(DATA_TERMINO))


# Salvar os dados transformados em arquivos CSV
spark_write_csv(BD_APEX_SUM, "BD_APEX_SUM_transformed.csv")
spark_write_csv(ATESTADOS_2019_2023, "ATESTADOS_2019_2023_transformed.csv")

# Desconectar do Spark
spark_disconnect(sc)
