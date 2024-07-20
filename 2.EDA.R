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

#> describe(df_selecionado)
#vars   n     mean       sd   median  trimmed      mad    min      max    range skew kurtosis     se
#IDADE                  1 335    42.84     9.98    42.00    42.17    10.38   25.0     79.0     54.0 0.63     0.35   0.55
#TOTAL_DEPENDENTES      2 335     0.83     1.26     0.00     0.60     0.00    0.0      6.0      6.0 1.42     1.17   0.07
#TEMPO_DE_CASA          3 335     8.51     5.70     8.00     8.38     5.93    0.0     23.0     23.0 0.27    -1.04   0.31
#SALARIO_MENSAL         4 335 19451.71 11313.97 17804.05 18709.65 12903.59 3259.9  71740.1  68480.2 0.76     0.94 618.15
#TOTAL_MINUTOS          5 335 13913.59 18089.77  9596.00 10465.91  9623.56    0.0 149134.0 149134.0 3.67    18.49 988.35
#CONJUGE_COMPANHEIRO    6 335     0.32     0.47     0.00     0.28     0.00    0.0      1.0      1.0 0.77    -1.41   0.03


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


# Exibindo a matriz de correlação
print(cor_matrix)


#                       IDADE     TOTAL_DEPENDENTES TEMPO_DE_CASA SALARIO_MENSAL TOTAL_MINUTOS CONJUGE_COMPANHEIRO
#IDADE               1.00000000        0.12114406     0.4128034     0.46625954    0.01054697          0.09245702
#TOTAL_DEPENDENTES   0.12114406        1.00000000     0.2820533     0.07537511    0.16110528          0.79089789
#TEMPO_DE_CASA       0.41280336        0.28205326     1.0000000     0.39700208    0.16622984          0.27590323
#SALARIO_MENSAL      0.46625954        0.07537511     0.3970021     1.00000000   -0.18167551          0.04939555
#TOTAL_MINUTOS       0.01054697        0.16110528     0.1662298    -0.18167551    1.00000000          0.13826186
#CONJUGE_COMPANHEIRO 0.09245702        0.79089789     0.2759032     0.04939555    0.13826186          1.00000000

#Percebe-se que as variáveis Conjuge e Total de Dependentes tem maior correlação, com índice de correlação de pearson  aproximado de 0.79. 
#Salário e Idade fica em segunda posição com Índice de 0.47, seguido de Idade e Tempo de Casa, com Índice de 0.41. 
#As demais variáveis não apresentam índice relevante.


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

#Shapiro-Wilk normality test

#data:  idade
#W = 0.9674, p-value = 7.75e-07

#Análise do Resultado: Valor de W (0.9674)

#O valor de W é uma estatística que mede a normalidade. Valores de W mais próximos de 1 indicam que os dados são mais próximos de uma distribuição normal.

#p-value (7.75e-07):   O valor p é muito pequeno (menor que 0.05, que é um nível de significância comum). Neste caso, o valor p é 7.75e-07, o que é muito menor que 0.05.

#Conclusão:
#Hipótese Nula: A hipótese nula do teste de Shapiro-Wilk é que a amostra segue uma distribuição normal.
#Resultado: Dado que o valor p é muito menor que 0.05, rejeitamos a hipótese nula.
#Interpretação: Há evidências estatisticamente significativas para concluir que a distribuição da idade não é normal.


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


#		Shapiro-Wilk normality test

#data:  salario
#W = 0.92253, p-value = 3.767e-12

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

#Graus de liberdade (Df): Existem 1 grau de liberdade para GENERO e 333 para os resíduos.
#Soma dos quadrados (Sum Sq): A soma dos quadrados entre os grupos (GENERO) é 3.459e+09 e a soma dos quadrados dentro dos grupos (resíduos) é 1.058e+11.
#Quadrado médio (Mean Sq): O quadrado médio entre os grupos é 3.459e+09 e o quadrado médio dentro dos grupos é 3.178e+08.
#Valor F (F value): O valor F é 10.88, indicando uma variabilidade significativa entre os grupos comparada à variabilidade dentro dos grupos.
#p-valor (Pr(>F)): O p-valor é 0.00107, indicando que a diferença na média de TOTAL_MINUTOS entre os gêneros é estatisticamente significativa ao nível de significância de 0.01.


#Diferença (diff): A diferença média entre F e M é de 6442.161 minutos, com F tendo uma média maior de TOTAL_MINUTOS em comparação com M.
#Limite Inferior (lwr): O limite inferior do intervalo de confiança de 95% é 2600.994 minutos.
#Limite Superior (upr): O limite superior do intervalo de confiança de 95% é 10283.33 minutos.
#p-valor ajustado (p adj): O p-valor ajustado é 0.0010748, indicando que a diferença na média de TOTAL_MINUTOS entre F e M é estatisticamente significativa.
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

#Graus de liberdade (Df): Existem 5 graus de liberdade para FAIXA_ETARIA e 329 para os resíduos.
#Soma dos quadrados (Sum Sq): A soma dos quadrados entre os grupos é 2.324e+09 e a soma dos quadrados dentro dos grupos (resíduos) é 1.070e+11.
#Quadrado médio (Mean Sq): O quadrado médio entre os grupos é 464804915 e o quadrado médio dentro dos grupos é 325149182.
#Valor F (F value): O valor F é 1.43, o que indica a razão da variabilidade entre os grupos pela variabilidade dentro dos grupos.
#p-valor (Pr(>F)): O p-valor é 0.213, indicando que não há evidência suficiente para rejeitar a hipótese nula de que as médias de TOTAL_MINUTOS são iguais entre as diferentes faixas etárias.

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

#Graus de liberdade (Df): Existem 4 graus de liberdade para FAIXAS_SALARIOS e 330 para os resíduos.
#Soma dos quadrados (Sum Sq): A soma dos quadrados entre os grupos (FAIXAS_SALARIOS) é 4.600e+09 e a soma dos quadrados dentro dos grupos (resíduos) é 1.047e+11.
#Quadrado médio (Mean Sq): O quadrado médio entre os grupos é 1.150e+09 e o quadrado médio dentro dos grupos é 3.173e+08.
#Valor F (F value): O valor F é 3.625, indicando uma variabilidade significativa entre os grupos comparada à variabilidade dentro dos grupos.
#p-valor (Pr(>F)): O p-valor é 0.00659, indicando que a diferença na média de TOTAL_MINUTOS entre as faixas salariais é estatisticamente significativa ao nível de significância de 0.01.


#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = TOTAL_MINUTOS ~ FAIXAS_SALARIOS, data = BD_APEX_SUM)

#$FAIXAS_SALARIOS
#diff        lwr        upr     p adj
#Low-Medium-Low          -8711.4776 -17152.656  -270.2997 0.0392876
#Medium-Low              -5954.9851 -14396.163  2486.1929 0.3008535
#Medium-High-Low         -6135.3582 -14576.536  2305.8197 0.2713405
#High-Low               -11099.9254 -19541.103 -2658.7474 0.0032742
#Medium-Low-Medium        2756.4925  -5684.685 11197.6705 0.8983754
#Medium-High-Low-Medium   2576.1194  -5865.059 11017.2974 0.9188921
#High-Low-Medium         -2388.4478 -10829.626  6052.7302 0.9373633
#Medium-High-Medium       -180.3731  -8621.551  8260.8048 0.9999973
#High-Medium             -5144.9403 -13586.118  3296.2377 0.4528644
#High-Medium-High        -4964.5672 -13405.745  3476.6108 0.4899663

#Interpretação dos Resultados:
# Low-Medium vs. Low:

#  Diferença (diff): -8711.4776 minutos
#Limite Inferior (lwr): -17152.656 minutos
#Limite Superior (upr): -270.2997 minutos
#p-valor ajustado (p adj): 0.0392876
#Interpretação: A diferença média de TOTAL_MINUTOS entre as faixas salariais "Low-Medium" e "Low" é significativa, com a faixa "Low-Medium" tendo uma média menor de minutos em comparação com a faixa "Low".
#High vs. Low:

#  Diferença (diff): -11099.9254 minutos
#Limite Inferior (lwr): -19541.103 minutos
#Limite Superior (upr): -2658.7474 minutos
#p-valor ajustado (p adj): 0.0032742
#Interpretação: A diferença média de TOTAL_MINUTOS entre as faixas salariais "High" e "Low" é significativa, com a faixa "High" tendo uma média menor de minutos em comparação com a faixa "Low".
#Outras Comparações:

#  As outras comparações não mostraram diferenças significativas, com p-valores ajustados muito maiores que 0.05.

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

#Analise Call:
# lm(formula = TOTAL_MINUTOS ~ IDADE + FAIXAS_SALARIOS + GENERO, 
#   data = BD_APEX_SUM)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-19743  -8459  -3752   2964 137279 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  9119.2     4861.5   1.876 0.061573 .  
#IDADE                         211.6      111.7   1.894 0.059061 .  
#FAIXAS_SALARIOSLow-Medium   -8005.8     3030.9  -2.641 0.008651 ** 
#  FAIXAS_SALARIOSMedium       -7017.8     3118.8  -2.250 0.025101 *  
#  FAIXAS_SALARIOSMedium-High  -7644.7     3127.2  -2.445 0.015028 *  
#  FAIXAS_SALARIOSHigh        -13629.0     3367.9  -4.047 6.48e-05 ***
#  GENEROF                      6421.5     1929.0   3.329 0.000971 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 17500 on 328 degrees of freedom
#Multiple R-squared:  0.08067,	Adjusted R-squared:  0.06386 
#F-statistic: 4.797 on 6 and 328 DF,  p-value: 0.0001044

#Coeficientes:
#Intercepto (Intercept): 9119.2

#Este é o valor médio de TOTAL_MINUTOS quando todas as variáveis independentes são iguais a zero.
#IDADE: 211.6

#A cada aumento de um ano na idade, espera-se um aumento de 211.6 minutos em TOTAL_MINUTOS, mantendo todas as outras variáveis constantes. Este coeficiente é marginalmente significativo com um valor p de 0.059.
#FAIXAS_SALARIOS (Referência: Low):

#  Low-Medium: -8005.8
#Em comparação com a faixa salarial "Low", espera-se que a faixa "Low-Medium" tenha, em média, 8005.8 minutos a menos em TOTAL_MINUTOS.
#Medium: -7017.8
#Em comparação com a faixa salarial "Low", espera-se que a faixa "Medium" tenha, em média, 7017.8 minutos a menos em TOTAL_MINUTOS.
#Medium-High: -7644.7
#Em comparação com a faixa salarial "Low", espera-se que a faixa "Medium-High" tenha, em média, 7644.7 minutos a menos em TOTAL_MINUTOS.
#High: -13629.0
#Em comparação com a faixa salarial "Low", espera-se que a faixa "High" tenha, em média, 13629.0 minutos a menos em TOTAL_MINUTOS.
#GENERO (Referência: M):

#  F: 6421.5
#Em comparação com o gênero masculino, espera-se que o gênero feminino tenha, em média, 6421.5 minutos a mais em TOTAL_MINUTOS.
#Significância dos Coeficientes:
#  Intercepto e IDADE têm valores p próximos a 0.05, indicando significância marginal.
#FAIXAS_SALARIOS:
#  As faixas "Low-Medium", "Medium", "Medium-High" e "High" são significativamente diferentes da faixa "Low", com p-valores menores que 0.05.
#GENERO:
#  O gênero feminino é significativamente diferente do masculino com um p-valor menor que 0.001.
#Estatísticas do Modelo:
#  Residual standard error: 17500
#Mede a variabilidade dos resíduos.
#Multiple R-squared: 0.08067
#Indica que aproximadamente 8.1% da variação em TOTAL_MINUTOS é explicada pelas variáveis independentes no modelo.
#Adjusted R-squared: 0.06386
#Ajusta o R-squared para o número de variáveis no modelo.
#F-statistic: 4.797
#Testa a significância global do modelo, com um p-valor de 0.0001044 indicando que o modelo é significativamente melhor do que um modelo sem preditores.

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

#Analise                     GVIF Df GVIF^(1/(2*Df))
#IDADE           1.354820  1        1.163967
#FAIXAS_SALARIOS 1.358170  4        1.039009
#GENERO          1.012488  1        1.006224


#Interpretação dos Valores GVIF
#Coeficientes de GVIF:
#  IDADE: 1.354820
#GVIF^(1/(2*Df)): 1.163967
#FAIXAS_SALARIOS: 1.358170
#GVIF^(1/(2*Df)): 1.039009
#GENERO: 1.012488
#GVIF^(1/(2*Df)): 1.006224
#Análise dos Valores:
#  IDADE: GVIF^(1/(2*Df)) = 1.164
#FAIXAS_SALARIOS: GVIF^(1/(2*Df)) = 1.039
#GENERO: GVIF^(1/(2*Df)) = 1.006
#Interpretação dos Resultados:
#  Valores de GVIF^(1/(2*Df)) inferiores a 2 geralmente indicam que não há problemas significativos de multicolinearidade.
#Todos os valores de GVIF^(1/(2*Df)) estão abaixo de 2, indicando que a multicolinearidade não é um problema significativo para este modelo.

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


#Call:
#  lm(formula = TOTAL_MINUTOS ~ IDADE * FAIXAS_SALARIOS + GENERO, 
#     data = BD_APEX_SUM)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-23084  -8298  -3980   3129 135994 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)                        4629.92    9394.46   0.493  0.62246   
#IDADE                               332.53     244.15   1.362  0.17415   
#FAIXAS_SALARIOSLow-Medium         -4502.05   13975.09  -0.322  0.74755   
#FAIXAS_SALARIOSMedium              -151.55   14978.31  -0.010  0.99193   
#FAIXAS_SALARIOSMedium-High       -16258.79   14577.67  -1.115  0.26554   
#FAIXAS_SALARIOSHigh                8906.19   15246.32   0.584  0.55952   
#GENEROF                            6348.04    1945.17   3.263  0.00122 **
#  IDADE:FAIXAS_SALARIOSLow-Medium     -93.73     365.54  -0.256  0.79780   
#IDADE:FAIXAS_SALARIOSMedium        -173.94     356.37  -0.488  0.62581   
#IDADE:FAIXAS_SALARIOSMedium-High    173.98     346.56   0.502  0.61600   
#IDADE:FAIXAS_SALARIOSHigh          -476.02     337.87  -1.409  0.15983   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 17500 on 324 degrees of freedom
#Multiple R-squared:  0.0922,	Adjusted R-squared:  0.06418 
#F-statistic:  3.29 on 10 and 324 DF,  p-value: 0.0004455


#O modelo de regressão linear ajustado inclui interações entre IDADE e FAIXAS_SALARIOS, além de GENERO. Vamos interpretar os resultados detalhadamente.

#Resíduos:
#  Min: -23084
#1Q: -8298
#Mediana: -3980
#3Q: 3129
#Max: 135994
#Coeficientes:
#  Intercepto: 4629.92 (não significativo)
#IDADE: 332.53 (não significativo)
#FAIXAS_SALARIOS:
#  Low-Medium: -4502.05 (não significativo)
#Medium: -151.55 (não significativo)
#Medium-High: -16258.79 (não significativo)
#High: 8906.19 (não significativo)
#GENERO (F): 6348.04 (significativo ao nível de 0.01)
#Interações (IDADE
#):
#  Low-Medium: -93.73 (não significativo)
#Medium: -173.94 (não significativo)
#Medium-High: 173.98 (não significativo)
#High: -476.02 (não significativo)
#Estatísticas do Modelo:
#  Residual standard error: 17500
#Multiple R-squared: 0.0922
#Adjusted R-squared: 0.06418
#F-statistic: 3.29 (p-valor: 0.0004455)
#Interpretação:
#  Intercepto e coeficientes individuais (exceto GENERO (F)) não são significativos. Isso sugere que, individualmente, IDADE e FAIXAS_SALARIOS (e suas interações) não têm uma relação estatisticamente significativa com TOTAL_MINUTOS.
#GENERO (F) é significativo, indicando que o gênero feminino tem, em média, 6348.04 minutos a mais em TOTAL_MINUTOS comparado ao masculino.
#A interação entre IDADE e FAIXAS_SALARIOS não é significativa, sugerindo que a influência da idade em TOTAL_MINUTOS não varia significativamente entre as diferentes faixas salariais.
#Multiple R-squared de 0.0922 indica que aproximadamente 9.2% da variabilidade em TOTAL_MINUTOS é explicada pelo modelo, que é relativamente baixo.

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


#Call:
#  lm(formula = LOG_TOTAL_MINUTOS ~ IDADE + FAIXAS_SALARIOS + GENERO, 
#     data = BD_APEX_SUM)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-9.1180 -0.5028  0.3594  1.2438  4.6597 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 9.219458   0.645554  14.281  < 2e-16 ***
#  IDADE                      -0.002273   0.014830  -0.153 0.878256    
#FAIXAS_SALARIOSLow-Medium  -0.634239   0.402464  -1.576 0.116016    
#FAIXAS_SALARIOSMedium      -0.212922   0.414141  -0.514 0.607509    
#FAIXAS_SALARIOSMedium-High -1.412850   0.415258  -3.402 0.000751 ***
#  FAIXAS_SALARIOSHigh        -2.499361   0.447217  -5.589 4.82e-08 ***
#  GENEROF                     0.639671   0.256150   2.497 0.013006 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.324 on 328 degrees of freedom
#Multiple R-squared:  0.1523,	Adjusted R-squared:  0.1368 
#F-statistic: 9.823 on 6 and 328 DF,  p-value: 5.768e-10

#Interpretação dos Resultados:
#  Resíduos:
#  Min: -9.1180
#  1Q: -0.5028
#  Mediana: 0.3594
#  3Q: 1.2438
#  Max: 4.6597
#  Coeficientes:
#    Intercepto: 9.219458 (muito significativo, p < 2e-16)
#  IDADE: -0.002273 (não significativo, p = 0.878256)
#  FAIXAS_SALARIOS:
#    Low-Medium: -0.634239 (marginalmente significativo, p = 0.116016)
#  Medium: -0.212922 (não significativo, p = 0.607509)
#  Medium-High: -1.412850 (muito significativo, p = 0.000751)
#  High: -2.499361 (muito significativo, p = 4.82e-08)
#  GENERO (F): 0.639671 (significativo, p = 0.013006)
#  Estatísticas do Modelo:
#   Residual standard error: 2.324
#  Multiple R-squared: 0.1523
#  Adjusted R-squared: 0.1368
#  F-statistic: 9.823 (p-valor: 5.768e-10)
#  Interpretação:
#    Intercepto é muito significativo, indicando que o valor médio de LOG_TOTAL_MINUTOS quando todas as outras variáveis são zero é 9.219458.
#  IDADE não é significativa, sugerindo que não há uma relação estatisticamente significativa entre IDADE e LOG_TOTAL_MINUTOS.
#  FAIXAS_SALARIOS:
#    Faixas salariais "Medium-High" e "High" são significativamente diferentes da faixa salarial "Low", com efeitos negativos significativos no LOG_TOTAL_MINUTOS.
#  Faixa salarial "Low-Medium" é marginalmente significativa.
#  Faixa salarial "Medium" não é significativa.
# GENERO (F) é significativo, indicando que mulheres têm, em média, um valor de LOG_TOTAL_MINUTOS maior em comparação aos homens.

# Normalidade dos Resíduos
qqPlot(model_log, main="Q-Q Plot")

# Homoscedasticidade
plot(model_log$fitted.values, model_log$residuals)
abline(h = 0, col = "red")
title("Resíduos vs Valores Ajustados")

# Multicolinearidade
vif(model_log)


#Conclusão:
#  Coeficientes Significativos:
#  FAIXAS_SALARIOS (Medium-High e High) e GENERO (F) são variáveis significativas.
#Modelo Explicativo:
#  O modelo explica aproximadamente 15.23% da variação em LOG_TOTAL_MINUTOS (Multiple R-squared).
#Assunções do Modelo:
#  Verificar a normalidade dos resíduos e a homoscedasticidade para garantir que as assunções do modelo são atendidas.

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

#Linear Regression 

#335 samples
#3 predictor

#No pre-processing
#Resampling: Cross-Validated (10 fold) 
#Summary of sample sizes: 302, 301, 302, 303, 301, 302, ... 
#Resampling results:

#  RMSE      Rsquared   MAE     
#2.304887  0.1465433  1.496761

#Tuning parameter 'intercept' was held constant at a value of TRUE

#Resultados da Validação Cruzada
#RMSE (Root Mean Squared Error): 2.304887
#R-squared: 0.1465433
#MAE (Mean Absolute Error): 1.496761
#Interpretação dos Resultados
#RMSE:
#  O RMSE mede o desvio padrão dos resíduos (diferença entre os valores observados e os valores previstos). Um RMSE de 2.304887 indica que, em média, as previsões do modelo estão a cerca de 2.3 unidades de log(TOTAL_MINUTOS) do valor real.
#R-squared:
#  O R-squared de 0.1465433 indica que aproximadamente 14.65% da variação nos valores de log(TOTAL_MINUTOS) é explicada pelo modelo. Este valor é relativamente baixo, sugerindo que o modelo não captura todas as variáveis relevantes ou que há uma alta variabilidade nos dados que não é explicada pelas variáveis preditoras incluídas.
#MAE:
#  O MAE mede a média dos erros absolutos entre as previsões e os valores reais. Um MAE de 1.496761 indica que, em média, as previsões do modelo estão a cerca de 1.5 unidades de log(TOTAL_MINUTOS) do valor real.



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

