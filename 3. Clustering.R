library(doParallel)
library(rpart)
library(nnet)
library(corrplot)
library(factoextra)
library(mclust)
library(cluster)
library(reshape2)
library(caret)

# Configurar o treinamento paralelo
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

# Tratar valores faltantes
dados <- BD_APEX_SUM %>%
  mutate_if(is.numeric, ~replace(., is.na(.), median(., na.rm = TRUE))) %>%
  mutate_if(is.character, ~replace(., is.na(.), "Desconhecido"))

colnames(dados)

# Verificar se ainda há valores ausentes
sum(is.na(dados))

# Selecionar variáveis para análise
dados_selecionados <- dados %>% 
  select(IDADE, TEMPO_DE_CASA, TOTAL_DEPENDENTES, GENERO, TOTAL_MINUTOS)

# Converter variável categórica (GENERO) para numérica
dados_selecionados$GENERO <- as.numeric(factor(dados_selecionados$GENERO))

# Normalizar os dados para clusterização
dados_normalizados <- scale(dados_selecionados)
##################################################################################
# Matriz de Correlação

# Seleção de variáveis numéricas para a análise de correlação
variaveis_numericas <- dados_selecionados %>% 
  select(IDADE, TEMPO_DE_CASA, TOTAL_DEPENDENTES, TOTAL_MINUTOS)

# Cálculo da matriz de correlação
cor_matrix <- cor(variaveis_numericas)

# Visualização da matriz de correlação

corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix", mar=c(0,0,1,0))

##################################################################################
#cLUSTERIZAÇÃO

# Método do Cotovelo para determinar o número de clusters
fviz_nbclust(dados_normalizados, kmeans, method = "wss") +
  ggtitle("Elbow Method for Determining the Number of Clusters")

# Índice de Silhueta para determinar o número de clusters
fviz_nbclust(dados_normalizados, kmeans, method = "silhouette") +
  ggtitle("Silhouette Index for Determining the Number of Clusters")
####################################################################################

# Clusterização com K-means com Sete clusters
set.seed(123)
kmeans_model <- kmeans(dados_normalizados, centers = 7, nstart = 25)
dados_selecionados$cluster <- as.factor(kmeans_model$cluster)

# Visualização dos clusters
fviz_cluster(kmeans_model, data = dados_normalizados, geom = "point") +
  ggtitle("Clusters Visualization wiht K-means")

silhouette_kmeans <- silhouette(kmeans_model$cluster, dist_matrix)
cat("Silhouette Score - K-means: ", mean(silhouette_kmeans[, 3]), "\n")
fviz_silhouette(silhouette_kmeans) + ggtitle("Silhouette Plot - K-means")

#Otimização:

# Tentar diferentes valores de k
fviz_nbclust(dados_normalizados, kmeans, method = "silhouette")

# Aplicar K-means com o número de clusters ajustado
optimal_k <- 6  # Exemplo, ajuste conforme resultado do fviz_nbclust
kmeans_model_optimized <- kmeans(dados_normalizados, centers = optimal_k)
silhouette_kmeans_optimized <- silhouette(kmeans_model_optimized$cluster, dist(dados_normalizados))
cat("Silhouette Score - K-means Optimized: ", mean(silhouette_kmeans_optimized[, 3]), "\n")

fviz_cluster(kmeans_model_optimized, data = dados_normalizados, geom = "point") +
  ggtitle("Optimized Clusters Visualization wiht K-means")

cat("Optimized Silhouette Score - K-means: ", mean(silhouette_kmeans_optimized[, 3]), "\n")

fviz_silhouette(silhouette_kmeans_optimized) + ggtitle("Optimized Silhouette Plot - K-means")

#################################################################################################################

# Hierarchical Clustering
dist_matrix <- dist(dados_normalizados)
hclust_model <- hclust(dist_matrix, method = "ward.D2")
hclust_clusters <- cutree(hclust_model, k = 7)

# Plot the dendrogram
plot(hclust_model, labels = FALSE, hang = -1, main = "Dendrogram of Hierarchical Clustering")

# Plotando o dendrograma
plot(hclust_model, cex = 0.6, hang = -1)
rect.hclust(hclust_model, k = 7, border = 2:4)

fviz_silhouette(silhouette_hclust) + ggtitle("Silhouette Plot - Hierarchical")
cat("Silhouette Score - Hierarchical: ", mean(silhouette_hclust[, 3]), "\n")
silhouette_hclust <- silhouette(hclust_clusters, dist_matrix)


#Otimização

# Experimentar diferentes métodos de ligação
hierarchical_model_average <- hclust(dist(dados_normalizados), method = "average")
hierarchical_clusters_average <- cutree(hierarchical_model_average, k = 7)
silhouette_hclust_average <- silhouette(hierarchical_clusters_average, dist(dados_normalizados))

plot(hierarchical_model_average, cex = 0.6, hang = -1)
rect.hclust(hierarchical_model_average, k = 7, border = 2:4)


cat("Optimized Silhouette Score - Hierarchical (Average Linkage): ", mean(silhouette_hclust_average[, 3]), "\n")

fviz_silhouette(silhouette_hclust_average) + ggtitle("Optimized Silhouette Plot - Hierarchical")


#################################################################################################################

# DBSCAN
# Correção da chamada DBSCAN usando o pacote fpc
dbscan_model_fpc <- fpc::dbscan(dados_normalizados, eps = 0.5, MinPts = 5)

# Plot the DBSCAN result using fpc
fviz_cluster(list(data = dados_normalizados, cluster = as.factor(dbscan_model_fpc$cluster)), geom = "point") +
  ggtitle("DBSCAN clustering (fpc)")

silhouette_dbscan <- silhouette(dbscan_model_fpc$cluster, dist_matrix)
cat("Silhouette Score - DBSCAN: ", mean(silhouette_dbscan[, 3]), "\n")
fviz_silhouette(silhouette_dbscan) + ggtitle("Silhouette Plot - DBSCAN")


#Otimização:

# Plotar gráfico de k-distância para determinar melhor eps
kNNdistplot(dados_normalizados, k = 5)
abline(h = 0.4, col = "red", lty = 2)  # Ajustar conforme necessário após análise do gráfico

# Aplicar DBSCAN com valores ajustados
novo_eps <- 0.4
novo_MinPts <- 5
dbscan_model_fpc_adjusted <- fpc::dbscan(dados_normalizados, eps = novo_eps, MinPts = novo_MinPts)

fviz_cluster(list(data = dados_normalizados, cluster = as.factor(dbscan_model_fpc_adjusted$cluster)), geom = "point") +
  ggtitle("Optimized DBSCAN clustering (fpc)")

# Calcular Silhouette Score para DBSCAN ajustado
silhouette_dbscan_adjusted <- silhouette(dbscan_model_fpc_adjusted$cluster, dist(dados_normalizados))
cat("Silhouette Score - DBSCAN Adjusted: ", mean(silhouette_dbscan_adjusted[, 3]), "\n")

fviz_silhouette(silhouette_dbscan) + ggtitle("Optimized Silhouette Plot - DBSCAN")

#################################################################################################################
# Avaliar modelos usando Silhouette Score
silhouette_kmeans <- silhouette(kmeans_model$cluster, dist_matrix)
silhouette_hclust <- silhouette(hclust_clusters, dist_matrix)
silhouette_dbscan <- silhouette(dbscan_model_fpc$cluster, dist_matrix)

cat("Silhouette Score - K-means: ", mean(silhouette_kmeans[, 3]), "\n")
cat("Silhouette Score - Hierarchical: ", mean(silhouette_hclust[, 3]), "\n")
cat("Silhouette Score - DBSCAN: ", mean(silhouette_dbscan[, 3]), "\n")


# Visualizar Silhouette Plots
fviz_silhouette(silhouette_kmeans) + ggtitle("Silhouette Plot - K-means")
fviz_silhouette(silhouette_hclust) + ggtitle("Silhouette Plot - Hierarchical")
fviz_silhouette(silhouette_dbscan) + ggtitle("Silhouette Plot - DBSCAN")

#######################################################################################

#Análise de Perfis de Cluster

# Adicionar os clusters aos dados originais
dados_selecionados$cluster <- as.factor(kmeans_model$cluster)

# Agrupar dados por cluster e calcular médias
cluster_profiles <- dados_selecionados %>%
  group_by(cluster) %>%
  summarise(across(everything(), list(mean = mean, sd = sd)))

# Visualizar o resumo das características de cada cluster
print(cluster_profiles)

# Criar gráficos para comparar as características médias de cada cluster

# Melt the data for ggplot
melted_data <- melt(cluster_profiles, id.vars = "cluster")

# Plotting
ggplot(melted_data, aes(x = variable, y = value, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  ggtitle("Cluster Profiles: Comparison of Average Characteristics") +
  xlab("Variable") +
  ylab("Mean Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#######################################################################################

# Aplicação do PCA
pca_model <- prcomp(dados_normalizados, center = TRUE, scale. = TRUE)

# Visualizar o resumo do PCA
summary(pca_model)

# Variabilidade explicada por cada componente
variancia_explicada <- pca_model$sdev^2 / sum(pca_model$sdev^2)
variancia_explicada

# Plotar a variabilidade explicada
plot(variancia_explicada, xlab = "Componente Principal", ylab = "Proporção da Variância Explicada", type = "b")

# Plotar os dois primeiros componentes principais
pca_data <- as.data.frame(pca_model$x)
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = dados_selecionados$cluster), alpha = 0.5) +
  ggtitle("PCA - Dois Primeiros Componentes Principais") +
  xlab(paste("PC1 - ", round(variancia_explicada[1] * 100, 2), "% Variância Explicada", sep = "")) +
  ylab(paste("PC2 - ", round(variancia_explicada[2] * 100, 2), "% Variância Explicada", sep = "")) +
  theme_minimal()
