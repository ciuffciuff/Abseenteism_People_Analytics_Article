##################################################################################################
#CLASSIFICAÇÃO DAS VARIÁVEIS

# Classificar variáveis
numeric_vars <- names(BD_APEX_SUM)[sapply(BD_APEX_SUM, is.numeric)]
categorical_vars <- names(BD_APEX_SUM)[sapply(BD_APEX_SUM, is.character) | sapply(BD_APEX_SUM, is.factor)]
variaveis_categoricas <- c("GENERO", "TIPO_CARGO", "FAIXA_ETARIA", "DIRETORIA","HORARIO_TRABALHO","PONTO_ELETRONICO","FAIXAS_MINUTOS","FAIXA_SALARIOS")

# Exibir variáveis numéricas e categóricas
print("Variáveis Numéricas:")
print(numeric_vars)
print("Variáveis Categóricas:")
print(categorical_vars)

# Classificar variáveis
numeric_vars_2 <- names(ATESTADOS_2019_2023)[sapply(ATESTADOS_2019_2023, is.numeric)]
categorical_vars_2 <- names(ATESTADOS_2019_2023)[sapply(ATESTADOS_2019_2023, is.character) | sapply(ATESTADOS_2019_2023, is.factor)]

# Exibir variáveis numéricas e categóricas
print("Variáveis Numéricas:")
print(numeric_vars_2)
print("Variáveis Categóricas:")
print(categorical_vars_2)

variaveis <- c("IDADE", "CONJUGE_COMPANHEIRO", "TOTAL_DEPENDENTES", "TEMPO_DE_CASA", "SALARIO_MENSAL", "TOTAL_MINUTOS")

variaveis2 <- c("GENERO", "TIPO_CARGO", "FAIXA_ETARIA", "DIRETORIA","HORARIO_TRABALHO","PONTO_ELETRONICO","FAIXAS_MINUTOS","FAIXAS_SALARIOS")


# Selecionar apenas as colunas relevantes do dataframe
df_selecionado <- BD_APEX_SUM[, variaveis]

df_selecionado2 <- BD_APEX_SUM[, variaveis2]

summary(df_selecionado2)

##################################################################################################
#ANÁLISE EXPLORATÓRIA DOS DADOS

#Análise Descritiva das Variáveis Numéricas


describe(df_selecionado)

# Carregar pacotes necessários
library(ggplot2)
library(gridExtra)

# Supondo que seu dataframe se chama df_selecionado

# Plotar o boxplot para cada variável
boxplot_idade <- ggplot(df_selecionado, aes(y = IDADE)) +
  geom_boxplot() +
  ggtitle("Boxplot of Age")

boxplot_total_dependentes <- ggplot(df_selecionado, aes(y = TOTAL_DEPENDENTES)) +
  geom_boxplot() +
  ggtitle("Boxplot of Dependents")

boxplot_tempo_de_casa <- ggplot(df_selecionado, aes(y = TEMPO_DE_CASA)) +
  geom_boxplot() +
  ggtitle("Boxplot of Length of employment")

boxplot_salario_mensal <- ggplot(df_selecionado, aes(y = SALARIO_MENSAL)) +
  geom_boxplot() +
  ggtitle("Boxplot de Monthly Salary")

boxplot_total_minutos <- ggplot(df_selecionado, aes(y = TOTAL_MINUTOS)) +
  geom_boxplot() +
  ggtitle("Boxplot de Total Minutes of Absence")

boxplot_conjuge_companheiro <- ggplot(df_selecionado, aes(y = CONJUGE_COMPANHEIRO)) +
  geom_boxplot() +
  ggtitle("Boxplot of Civil Union")

# Combinar os gráficos em um único layout
grid.arrange(boxplot_idade, boxplot_total_dependentes, boxplot_tempo_de_casa,
             boxplot_salario_mensal, boxplot_total_minutos, boxplot_conjuge_companheiro, ncol = 2)

######################################################################################################################
#Correlação e Covariância

#Variáveis Numéricas

# Para efeitos deste estudo e para não expor os empregados da Apex-Brasil, na fase de transformação de dados foram consolidadas todos os tipos de ausência na Coluna "Total Minutos". 


# Análise de Correlação

# Calcular a matriz de correlação
cor_matrix <- cor(df_selecionado, use = "complete.obs")

# Carregar os pacotes necessários
library(ggplot2)
library(reshape2)

# Transformar a matriz de correlação em um formato longo para ggplot2
cor_matrix_melt <- melt(cor_matrix)


# Criar o heatmap com valores do índice de correlação
ggplot(data = cor_matrix_melt, aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       space = "Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = '', y = '') + 
  ggtitle("Heatmap of Correlation Matrix") +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3)


###################################################################################################################

#Análise Univariada

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

# Análise de Idade

# Carregar os dados
idade <- df_selecionado$IDADE

# Executar o teste de Shapiro-Wilk
shapiro_test <- shapiro.test(idade)

# Exibir o resultado do teste
print(shapiro_test)

# Visualizações

# Histograma para a variável 'IDADE'
hist_plot <- ggplot(BD_APEX_SUM, aes(x = IDADE)) + 
  geom_histogram(binwidth = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
  theme_minimal() +
  labs(title = "Histogram of Age Distribution", x = "Age", y = "Frequency")

# Gráfico de densidade para a variável 'IDADE'
density_plot <- ggplot(BD_APEX_SUM, aes(x = IDADE)) +
  geom_density(adjust = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
  labs(title = "Age Probability Density", x = "Age", y = "Density") +
  theme_minimal()

# Q-Q plot para a variável 'IDADE'
qq_plot <- ggplot(BD_APEX_SUM, aes(sample = IDADE)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot of Age Distribution", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Combinar os três gráficos em uma única figura vertical
combined_plot <- hist_plot / density_plot / qq_plot

# Mostrar a figura combinada
print(combined_plot)


#############################################################################################################################
# Análise de Dependentes

# Carregar os dados
dependents <- df_selecionado$TOTAL_DEPENDENTES

# Executar o teste de Shapiro-Wilk
shapiro_test_2 <- shapiro.test(dependents)

# Exibir o resultado do teste
print(shapiro_test_2)


#Shapiro-Wilk normality test

#data:  dependents
#W = 0.70279, p-value < 2.2e-16


# Histograma para a variável 'Dependents'
hist_plot2 <- ggplot(BD_APEX_SUM, aes(x = TOTAL_DEPENDENTES)) + 
  geom_histogram(binwidth = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
  theme_minimal() +
  labs(title = "Histogram of Dependents Distribution", x = "Dependents", y = "Frequency")

# Gráfico de densidade para a variável 'Dependents'
density_plot2 <- ggplot(BD_APEX_SUM, aes(x = TOTAL_DEPENDENTES)) +
  geom_density(adjust = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
  labs(title = "Age Probability Density", x = "Dependents", y = "Density") +
  theme_minimal()

# Q-Q plot para a variável 'Dependents'
qq_plot2 <- ggplot(BD_APEX_SUM, aes(sample = TOTAL_DEPENDENTES)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot of Dependents Distribution", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Combinar os três gráficos em uma única figura vertical
combined_plot2 <- hist_plot2 / density_plot2 / qq_plot2

# Mostrar a figura combinada
print(combined_plot2)



##############################################################################################################################
# Análise de Tempo de Casa

# Carregar os dados
casa <- df_selecionado$TEMPO_DE_CASA

# Executar o teste de Shapiro-Wilk
shapiro_test_3 <- shapiro.test(casa)

# Exibir o resultado do teste
print(shapiro_test_3)


#Shapiro-Wilk normality test

#data:  casa
#W = 0.93312, p-value = 4.003e-11


# Histograma para a variável 'Length of employment'
hist_plot3 <- ggplot(BD_APEX_SUM, aes(x = TEMPO_DE_CASA)) + 
  geom_histogram(binwidth = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
  theme_minimal() +
  labs(title = "Histogram of Length of employment Distribution", x = "Length of employment", y = "Frequency")

# Gráfico de densidade para a variável 'Length of employment'
density_plot3 <- ggplot(BD_APEX_SUM, aes(x = TEMPO_DE_CASA)) +
  geom_density(adjust = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
  labs(title = "Length of employment Probability Density", x = "Length of employment", y = "Density") +
  theme_minimal()

# Q-Q plot para a variável 'Length of employment'
qq_plot3 <- ggplot(BD_APEX_SUM, aes(sample = TEMPO_DE_CASA)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot of Length of employment Distribution", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Combinar os três gráficos em uma única figura vertical
combined_plot3 <- hist_plot3 / density_plot3 / qq_plot3

# Mostrar a figura combinada
print(combined_plot3)

##########################################################################################################################
# Análise de Estado Civil

# Carregar os dados
casamento <- df_selecionado$CONJUGE_COMPANHEIRO

# Executar o teste de Shapiro-Wilk
shapiro_test_4 <- shapiro.test(casamento)

# Exibir o resultado do teste
print(shapiro_test_4)


#	Shapiro-Wilk normality test

#data:  casamento
#W = 0.58721, p-value < 2.2e-16


# Histograma para a variável 'Type of civil union'
hist_plot4 <- ggplot(BD_APEX_SUM, aes(x = TEMPO_DE_CASA)) + 
  geom_histogram(binwidth = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
  theme_minimal() +
  labs(title = "Histogram of Type of civil union Distribution", x = "Type of civil union", y = "Frequency")

# Gráfico de densidade para a variável 'Type of civil uniont'
density_plot4 <- ggplot(BD_APEX_SUM, aes(x = TEMPO_DE_CASA)) +
  geom_density(adjust = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
  labs(title = "Type of civil union Probability Density", x = "Type of civil union", y = "Density") +
  theme_minimal()

# Q-Q plot para a variável 'Type of civil union'
qq_plot4 <- ggplot(BD_APEX_SUM, aes(sample = TEMPO_DE_CASA)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot of Type of civil union Distribution", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Combinar os três gráficos em uma única figura vertical
combined_plot4 <- hist_plot4 / density_plot4 / qq_plot4

# Mostrar a figura combinada
print(combined_plot4)

##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################

# Análise de Pagamento

# Carregar os dados
#salario <- df_selecionado$SALARIO_MENSAL

# Executar o teste de Shapiro-Wilk
#shapiro_test_5 <- shapiro.test(salario)

# Exibir o resultado do teste
#print(shapiro_test_5)


#		Shapiro-Wilk normality test

#data:  salario
#W = 0.92253, p-value = 3.767e-12


# Histograma para a variável 'Monthly salary'
#hist_plot5 <- ggplot(BD_APEX_SUM, aes(x = SALARIO_MENSAL)) + 
#  geom_histogram(binwidth = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
#  theme_minimal() +
#  labs(title = "Monthly salary union Distribution", x = "Monthly salary", y = "Frequency")

# Gráfico de densidade para a variável 'Monthly salary'
#density_plot5 <- ggplot(BD_APEX_SUM, aes(x = SALARIO_MENSAL)) +
#  geom_density(adjust = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
#  labs(title = "Monthly salary Probability Density", x = "Monthly salary", y = "Density") +
#  theme_minimal()

# Q-Q plot para a variável 'Monthly salary'
#qq_plot5 <- ggplot(BD_APEX_SUM, aes(sample = SALARIO_MENSAL)) +
# stat_qq() +
#  stat_qq_line() +
# labs(title = "Q-Q Plot of Monthly salary Distribution", x = "Theoretical Quantiles", y = "Sample Quantiles") +
#  theme_minimal()

# Combinar os três gráficos em uma única figura vertical
#combined_plot5 <- hist_plot5 / density_plot5 / qq_plot5

# Mostrar a figura combinada
#print(combined_plot5)

##############################################################################################################################

# Análise de Minutos de Absenteísmo

# Carregar os dados
#minutos <- df_selecionado$TOTAL_MINUTOS

# Executar o teste de Shapiro-Wilk
#shapiro_test_6 <- shapiro.test(minutos)

# Exibir o resultado do teste
#print(shapiro_test_6)


#ggplot(BD_APEX_SUM, aes(x = TOTAL_MINUTOS)) + 
# geom_histogram(binwidth = 1, fill = brewer.pal(n = 3, name = "Pastel1")[2]) +
#  theme_minimal() +
#  labs(title = "Histogram of Consolidated Total minutes of absenteeism Distribution", x = "Consolidated Total minutes of absenteeism", y = "Frequency")

# Q-Q plot
#ggplot(df_selecionado, aes(sample = TOTAL_MINUTOS)) +
#  stat_qq() +
#  stat_qq_line() +
#  labs(title = "Q-Q Plot of Consolidated Total minutes of absenteeism Distribution", x = "Theoretical Quantiles", y = "Sample Quantiles") +
#  theme_minimal()

##############################################################################################################################
# Análise de variáveis categóricas

# Teste de qui-quadrado para as variáveis categóricas

library(dplyr)
library(tidyverse)
library(DescTools)
library(GGally)

# Função para realizar o teste de qui-quadrado e coletar os resultados em uma tabela
realizar_teste_qui2 <- function(var1, var2, data) {
  # Verificar se as variáveis contêm valores não-NA
  if (sum(!is.na(data[[var1]])) > 0 & sum(!is.na(data[[var2]])) > 0) {
    # Remover NAs nas variáveis categóricas
    data <- data %>% 
      filter(!is.na(.[[var1]]) & !is.na(.[[var2]]))
    
    tabela_contingencia <- table(data[[var1]], data[[var2]])
    
    # Verificar se a tabela de contingência não é vazia
    if (all(dim(tabela_contingencia) > 1)) {
      resultado_teste <- chisq.test(tabela_contingencia)
      
      # Determinar a interpretação com base no valor p
      interpretacao <- ifelse(resultado_teste$p.value < 0.05, "Significative", "Non significative")
      
      # Coletar os resultados
      resultado <- data.frame(
        Variavel1 = var1,
        Variavel2 = var2,
        Valor_Chi2 = resultado_teste$statistic,
        Graus_Liberdade = resultado_teste$parameter,
        Valor_p = resultado_teste$p.value,
        Interpretacao = interpretacao,
        stringsAsFactors = FALSE
      )
    } else {
      resultado <- data.frame(
        Variavel1 = var1,
        Variavel2 = var2,
        Valor_Chi2 = NA,
        Graus_Liberdade = NA,
        Valor_p = NA,
        Interpretacao = "Tabela de contingência vazia ou inválida",
        stringsAsFactors = FALSE
      )
    }
  } else {
    resultado <- data.frame(
      Variavel1 = var1,
      Variavel2 = var2,
      Valor_Chi2 = NA,
      Graus_Liberdade = NA,
      Valor_p = NA,
      Interpretacao = "Variable with only NAs",
      stringsAsFactors = FALSE
    )
  }
  
  return(resultado)
}

# Criar uma lista para armazenar os resultados dos testes
resultados_teste <- list()

# Realizar o teste de qui-quadrado para cada par de variáveis categóricas e armazenar os resultados
for (i in 1:(length(variaveis_categoricas) - 1)) {
  for (j in (i + 1):length(variaveis_categoricas)) {
    resultado <- realizar_teste_qui2(variaveis_categoricas[i], variaveis_categoricas[j], BD_APEX_SUM)
    resultados_teste[[length(resultados_teste) + 1]] <- resultado
  }
}

# Unir todos os resultados em um único data frame
tabela_resultados <- do.call(rbind, resultados_teste)

# Ordenar a tabela por Valor_p para ver os testes mais significativos no topo
tabela_resultados <- tabela_resultados %>%
  arrange(Valor_p)

# Mostrar a tabela de resultados
print(tabela_resultados)

# Verificar se a tabela tem dados antes de salvar
if (nrow(tabela_resultados) > 0) {
  # Salvar a tabela de resultados em um arquivo CSV
  write_csv(tabela_resultados, "resultados_teste_qui2.csv", row.names = FALSE)
  print("Tabela salva com sucesso!")
} else {
  print("Nenhum resultado para salvar.")
}

##############################################################################################################################
# Boxplot

ggplot(BD_APEX_SUM, aes(x = GENERO, y = TOTAL_MINUTOS, fill = GENERO)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal() +
  labs(x = "Gender", y = "Total Minutes of Absence")


ggplot(BD_APEX_SUM, aes(x = FAIXAS_SALARIOS, y = TOTAL_MINUTOS, fill = FAIXAS_SALARIOS)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal() +
  labs(x = "Salary Range", y = "Total Minutes of Absence")


# Garantir que as faixas de salário estejam ordenadas corretamente
BD_APEX_SUM$FAIXAS_SALARIOS <- factor(BD_APEX_SUM$FAIXAS_SALARIOS, levels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# Plotar o boxplot dos TOTAL_MINUTOS para cada faixa salarial
ggplot(BD_APEX_SUM, aes(x = FAIXAS_SALARIOS, y = TOTAL_MINUTOS)) + 
  geom_boxplot() +
  labs(title = "Distribuição dos Total Minutos por Faixas de Salário", x = "Faixas de Salário", y = "Total Minutos") +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal()


# Gráfico de dispersão entre 'IDADE' e 'TOTAL_MINUTOS'
ggplot(BD_APEX_SUM, aes(x = IDADE, y = TOTAL_MINUTOS, color = FAIXAS_MINUTOS)) + 
  geom_point() +
  scale_color_brewer(palette = "Pastel2", name = "Faixas de Minutos") +
  theme_minimal()


# Gráfico de barras empilhadas para 'GENERO' e 'CONJUGE'
ggplot(BD_APEX_SUM, aes(x = GENERO, fill = as.factor(CONJUGE_COMPANHEIRO))) + 
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Pastel2", 
                    labels = c("0" = "Single", "1" = "Spouse or Partner"),
                    name = "Civil State") +
  theme_minimal()


#Salários
ggplot(BD_APEX_SUM, aes(x = IDADE, y = SALARIO_MENSAL, color = FAIXAS_MINUTOS)) + 
  geom_point() +
  scale_color_brewer(palette = "Pastel2", name = "Faixas de Minutos") +
  theme_minimal()

#################################################################################################################
# Histograma em Facetas
ggplot(gather(dados_selecionados3), aes(value)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ key, scales = 'free_x') +
  labs(title = "Histograms of Numeric Variables", x = "Valor", y = "Frequency")

# Boxplot em Facetas
ggplot(gather(dados_selecionados3), aes(key, value)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  facet_wrap(~ key, scales = 'free') +
  labs(title = "Boxplots of Numeric Variables", x = "Variables", y = "Value")

# Matriz de Dispersão
ggpairs(dados_selecionados3) +
  labs(title = "Dispersion Matrix of Numeric Variables")

#################################################################################################################
#Análise Bivariada GENERO e Total de Minutos

library(ggplot2)
library(dplyr)
library(car)


# Resumo estatístico da variável numérica por categoria
BD_APEX_SUM %>%
  group_by(GENERO) %>%
  summarise(
    media = mean(TOTAL_MINUTOS, na.rm = TRUE),
    mediana = median(TOTAL_MINUTOS, na.rm = TRUE),
    sd = sd(TOTAL_MINUTOS, na.rm = TRUE)
  )

# Boxplot
ggplot(BD_APEX_SUM, aes(x = GENERO, y = TOTAL_MINUTOS)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot de Total of Minutes by Gender",
       x = "Gender",
       y = "Total of Minutes of Absenteeism")

# Gráfico de barras com médias

ggplot(BD_APEX_SUM, aes(x = GENERO, y = TOTAL_MINUTOS)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "skyblue", color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Mean of Total Minutes by  Gender",
       x = "Gender",
       y = "Mean of Total Minutes")

# ANOVA
anova_result <- aov(TOTAL_MINUTOS ~ GENERO, data = BD_APEX_SUM)
summary(anova_result)

# Se ANOVA for significativa, realizar teste post-hoc de Tukey
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

#######################################################################################################
# Análise Bivariada Faixa Etária e Total de Minutos

library(ggplot2)
library(dplyr)
library(car)

# Resumo estatístico da variável numérica por categoria
BD_APEX_SUM %>%
  group_by(FAIXA_ETARIA) %>%
  summarise(
    media = mean(TOTAL_MINUTOS, na.rm = TRUE),
    mediana = median(TOTAL_MINUTOS, na.rm = TRUE),
    sd = sd(TOTAL_MINUTOS, na.rm = TRUE)
  )

# Boxplot
ggplot(BD_APEX_SUM, aes(x = FAIXA_ETARIA, y = TOTAL_MINUTOS)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot -  Total of Minutes and Age Group",
       x = "Age Group",
       y = "Total of Minutes")

# Gráfico de barras com médias
ggplot(BD_APEX_SUM, aes(x = FAIXA_ETARIA, y = TOTAL_MINUTOS)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "skyblue", color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Mean of Total Minutes by  Age Group",
       x = "Age",
       y = "Mean of Total Minutes")

# ANOVA
anova_result <- aov(TOTAL_MINUTOS ~ FAIXA_ETARIA, data = BD_APEX_SUM)
summary(anova_result)

# Se ANOVA for significativa, realizar teste post-hoc de Tukey
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

#######################################################################################################
# Análise Bivariada Faixa Salários e Total de Minutos

# Resumo estatístico da variável numérica por categoria
BD_APEX_SUM %>%
  group_by(FAIXAS_SALARIOS) %>%
  summarise(
    media = mean(TOTAL_MINUTOS, na.rm = TRUE),
    mediana = median(TOTAL_MINUTOS, na.rm = TRUE),
    sd = sd(TOTAL_MINUTOS, na.rm = TRUE)
  )


# Definir paleta de cores pastel
pastel_colors <- brewer.pal(8, "Pastel2")

# Boxplot de TOTAL_MINUTOS por FAIXAS_SALARIOS
ggplot(BD_APEX_SUM, aes(x = FAIXAS_SALARIOS, y = TOTAL_MINUTOS)) +
  geom_boxplot(fill = pastel_colors[1], color = "black") +
  theme_minimal() +
  labs(title = "Boxplot Total of Minutes by Salary Range",
       x = "Salary Range",
       y = "Total of Minutes")

# Gráfico de Barras com Médias de TOTAL_MINUTOS por FAIXAS_SALARIOS
ggplot(BD_APEX_SUM, aes(x = FAIXAS_SALARIOS, y = TOTAL_MINUTOS)) +
  stat_summary(fun = mean, geom = "bar", fill = pastel_colors[2], color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Mean of Total Minutes by Salary Range",
       x = "Salary Range",
       y = "Mean of Total Minutes")

# Boxplot
ggplot(BD_APEX_SUM, aes(x = FAIXAS_SALARIOS, y = TOTAL_MINUTOS)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot de Total de Minutos por Faixa Salarial",
       x = "Faixa Salarial",
       y = "Total de Minutos")

# Gráfico de barras com médias
ggplot(BD_APEX_SUM, aes(x = FAIXAS_SALARIOS, y = TOTAL_MINUTOS)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "skyblue", color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "Média de Total de Minutos por Faixa Salarial",
       x = "Faixa Salarial",
       y = "Média de Total de Minutos")

# ANOVA
anova_result <- aov(TOTAL_MINUTOS ~ FAIXAS_SALARIOS, data = BD_APEX_SUM)
summary(anova_result)

# Se ANOVA for significativa, realizar teste post-hoc de Tukey
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}



#######################################################################################################
# Outras Análises

# Carregar pacotes necessários
library(readxl)
library(ggplot2)
library(dplyr)

# Carregar dados do arquivo Excel
df <- read_excel("caminho/para/seu/arquivo.xlsx")

# Boxplot de TOTAL_MINUTOS por FAIXAS_SALARIOS e GENERO
ggplot(BD_APEX_SUM, aes(x = FAIXAS_SALARIOS, y = TOTAL_MINUTOS, fill = GENERO)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot de Total de Minutos por Faixa Salarial e Gênero",
       x = "Faixa Salarial",
       y = "Total de Minutos")
# Boxplot de TOTAL_MINUTOS por CARGO
ggplot(BD_APEX_SUM, aes(x = TIPO_CARGO, y = TOTAL_MINUTOS)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot de Total de Minutos por Cargo",
       x = "Cargo",
       y = "Total de Minutos") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Gráfico de dispersão de TOTAL_MINUTOS vs IDADE
ggplot(BD_APEX_SUM, aes(x = IDADE, y = TOTAL_MINUTOS)) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(title = "Dispersão de Total de Minutos por Idade",
       x = "Idade",
       y = "Total de Minutos")
#############################################################################################
# Modelagem de regressão linear

model <- lm(TOTAL_MINUTOS ~ IDADE + FAIXAS_SALARIOS + GENERO, data = BD_APEX_SUM)
summary(model)


# Normalidade dos Resíduos
qqPlot(model, main="Q-Q Plot")

# Verificar os pontos específicos
outliers <- c(9, 61)
df[outliers, ]

# Distância de Cook
cooksd <- cooks.distance(model)
plot(cooksd, pch = "*", cex = 2, main = "Distância de Cook")
abline(h = 4/length(cooksd), col = "red")
text(x = 1:length(cooksd), y = cooksd, labels = ifelse(cooksd > 4/length(cooksd), names(cooksd), ""), col = "red", pos = 4)

# Alavancagem
hatvalues <- hatvalues(model)
plot(hatvalues, main = "Alavancagem", ylab = "Valores de alavancagem", xlab = "Observações")
abline(h = 2*mean(hatvalues), col = "red")

# Resíduos Padronizados
stdres <- rstandard(model)
plot(stdres, main = "Standardized Residuals", ylab = "Standardized Residuals", xlab = "Observations")
abline(h = c(-3, 3), col = "red")


# Homoscedasticidade
plot(model$fitted.values, model$residuals)
abline(h = 0, col = "red")
title("Resíduos vs Valores Ajustados")

# Multicolinearidade
vif(model)

#######################################################################################################################
# Instalar e carregar pacotes necessários
library(car)


# Homoscedasticidade
plot(model$fitted.values, model$residuals)
abline(h = 0, col = "red")
title("Resíduos vs Valores Ajustados")



# Incluir Interações
model_interaction <- lm(TOTAL_MINUTOS ~ IDADE * FAIXAS_SALARIOS + GENERO, data = BD_APEX_SUM)
summary(model_interaction)




# Verificar se o data frame está carregado corretamente
str(BD_APEX_SUM)

# Aplicar a transformação logarítmica
BD_APEX_SUM$LOG_TOTAL_MINUTOS <- log(BD_APEX_SUM$TOTAL_MINUTOS + 1) # Adicionar 1 para evitar log(0)

# Visualizar as primeiras linhas para confirmar a transformação
head(BD_APEX_SUM)


# Transformação de Variáveis (exemplo: log-transformação)
BD_APEX_SUM$LOG_TOTAL_MINUTOS <- log(df$TOTAL_MINUTOS + 1) # Adicionar 1 para evitar log(0)
model_log <- lm(LOG_TOTAL_MINUTOS ~ IDADE + FAIXAS_SALARIOS + GENERO, data = BD_APEX_SUM)
summary(model_log)


# Normalidade dos Resíduos
qqPlot(model_log, main="Q-Q Plot")

# Homoscedasticidade
plot(model_log$fitted.values, model_log$residuals)
abline(h = 0, col = "red")
title("Resíduos vs Valores Ajustados")

# Multicolinearidade
vif(model_log)


######################################################################################################################

# Instalar e carregar pacotes necessários
library(caret)


# Verificar a estrutura dos dados
str(BD_APEX_SUM)

# Criar a variável transformada
#BD_APEX_SUM$LOG_TOTAL_MINUTOS <- log(BD_APEX_SUM$TOTAL_MINUTOS + 1)

# Verificar a criação da variável
head(BD_APEX_SUM)

# Definir a fórmula do modelo
#formula <- LOG_TOTAL_MINUTOS ~ IDADE + FAIXAS_SALARIOS + GENERO

# Configurar o método de controle para validação cruzada
train_control <- trainControl(method = "cv", number = 10)

# Treinar o modelo usando validação cruzada
set.seed(123) # Definir uma semente para reprodutibilidade
model_cv <- train(formula, data = BD_APEX_SUM, method = "lm", trControl = train_control)

# Exibir os resultados da validação cruzada
print(model_cv)

###############################################################################################

# Matriz de correlação
numeric_vars <- BD_APEX_SUM %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

# Visualização da matriz de correlação
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "circle")


# Converter DATA_NASCIMENTO para objeto Date
BD_APEX_SUM$DATA_NASCIMENTO <- as.Date(BD_APEX_SUM$DATA_NASCIMENTO, format = "%Y-%m-%d")

# Agregar TOTAL_MINUTOS por ano de nascimento
BD_APEX_SUM$ANO_NASCIMENTO <- format(BD_APEX_SUM$DATA_NASCIMENTO, "%Y")
agg_data <- BD_APEX_SUM %>%
  group_by(ANO_NASCIMENTO) %>%
  summarise(TOTAL_MINUTOS = mean(TOTAL_MINUTOS, na.rm = TRUE))

# Gráfico de linha de TOTAL_MINUTOS por ano de nascimento
ggplot(agg_data, aes(x = ANO_NASCIMENTO, y = TOTAL_MINUTOS)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Total de Minutos por Ano de Nascimento",
       x = "Ano de Nascimento",
       y = "Total de Minutos")

