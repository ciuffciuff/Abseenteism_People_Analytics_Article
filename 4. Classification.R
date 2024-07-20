library(caret)
library(ggplot2)
library(pROC)
library(nnet)
  library(doParallel)
library(gridExtra)

# Configurar paralelização
numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Divisão dos dados em treino e teste para modelagem
set.seed(123)
indice_treino <- createDataPartition(dados_selecionados$cluster, p = 0.7, list = FALSE)
dados_treino <- dados_selecionados[indice_treino, ]
dados_teste <- dados_selecionados[-indice_treino, ]

# Garantir que não há valores ausentes nos dados de treino e teste
stopifnot(sum(is.na(dados_treino)) == 0)
stopifnot(sum(is.na(dados_teste)) == 0)

# Função para calcular métricas de avaliação para múltiplas classes
calcular_metricas_multiclasse <- function(predicoes, verdadeiros) {
  matriz_confusao <- confusionMatrix(predicoes, verdadeiros)
  
  # Cálculo da precisão, recall e F1-score para cada classe
  precisao_classes <- matriz_confusao$byClass[, "Precision"]
  recall_classes <- matriz_confusao$byClass[, "Recall"]
  f1_classes <- matriz_confusao$byClass[, "F1"]
  
  # Média das métricas para todas as classes
  precision_media <- mean(precisao_classes, na.rm = TRUE)
  recall_media <- mean(recall_classes, na.rm = TRUE)
  f1_media <- mean(f1_classes, na.rm = TRUE)
  
  list(
    matriz_confusao = matriz_confusao,
    precision_media = precision_media,
    recall_media = recall_media,
    f1_media = f1_media
  )
}

# Treinamento de um modelo de Regressão Logística Multinomial
multinom_model <- train(cluster ~ ., data = dados_treino, method = "multinom", trControl = trainControl(method = "cv", number = 10, allowParallel = TRUE))
multinom_predicoes <- predict(multinom_model, dados_teste)
multinom_metricas <- calcular_metricas_multiclasse(multinom_predicoes, dados_teste$cluster)

# Treinamento de um modelo de Árvores de Decisão
rpart_model <- train(cluster ~ ., data = dados_treino, method = "rpart", trControl = trainControl(method = "cv", number = 10, allowParallel = TRUE))
rpart_predicoes <- predict(rpart_model, dados_teste)
rpart_metricas <- calcular_metricas_multiclasse(rpart_predicoes, dados_teste$cluster)

# Treinamento de um modelo de Random Forest
rf_model <- train(cluster ~ ., data = dados_treino, method = "rf", trControl = trainControl(method = "cv", number = 10, allowParallel = TRUE))
rf_predicoes <- predict(rf_model, dados_teste)
rf_metricas <- calcular_metricas_multiclasse(rf_predicoes, dados_teste$cluster)

# Definindo uma gama mais ampla de valores de k
knn_grid <- expand.grid(k = seq(1, 20, by = 1))
knn_model <- train(cluster ~ ., data = dados_treino, method = "knn", tuneGrid = knn_grid, trControl = trainControl(method = "cv", number = 10, allowParallel = TRUE))
knn_predicoes <- predict(knn_model, dados_teste)
knn_metricas <- calcular_metricas_multiclasse(knn_predicoes, dados_teste$cluster)

# Avaliação dos modelos
cat("Regressão Logística Multinomial - Accuracy: ", multinom_metricas$matriz_confusao$overall['Accuracy'], 
    " Precisão Média: ", multinom_metricas$precision_media, 
    " Recall Médio: ", multinom_metricas$recall_media, 
    " F1-score Médio: ", multinom_metricas$f1_media, "\n")
cat("Árvores de Decisão - Accuracy: ", rpart_metricas$matriz_confusao$overall['Accuracy'], 
    " Precisão Média: ", rpart_metricas$precision_media, 
    " Recall Médio: ", rpart_metricas$recall_media, 
    " F1-score Médio: ", rpart_metricas$f1_media, "\n")
cat("Random Forest - Accuracy: ", rf_metricas$matriz_confusao$overall['Accuracy'], 
    " Precisão Média: ", rf_metricas$precision_media, 
    " Recall Médio: ", rf_metricas$recall_media, 
    " F1-score Médio: ", rf_metricas$f1_media, "\n")
cat("KNN - Accuracy: ", knn_metricas$matriz_confusao$overall['Accuracy'], 
    " Precisão Média: ", knn_metricas$precision_media, 
    " Recall Médio: ", knn_metricas$recall_media, 
    " F1-score Médio: ", knn_metricas$f1_media, "\n")

# Gráficos da Matriz de Confusão
multinom_plot <- ggplot(as.data.frame(multinom_metricas$matriz_confusao$table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  ggtitle("Multinomial Logistic Regression")

rpart_plot <- ggplot(as.data.frame(rpart_metricas$matriz_confusao$table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  ggtitle("Decision Tree")

rf_plot <- ggplot(as.data.frame(rf_metricas$matriz_confusao$table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  ggtitle("Random Forest")

knn_plot <- ggplot(as.data.frame(knn_metricas$matriz_confusao$table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  ggtitle("KNN")

# Mostrar todos os gráficos juntos
grid.arrange(multinom_plot, rpart_plot, rf_plot, knn_plot, nrow = 2, ncol = 2)

# Curva ROC para Random Forest como exemplo
rf_roc <- multiclass.roc(as.numeric(dados_teste$cluster), as.numeric(rf_predicoes))
plot.roc(rf_roc$rocs[[1]], main = "ROC Curve for Random Forest")

# Visualizar os resultados da validação cruzada para KNN
ggplot(knn_model) + ggtitle("KNN performance with different k values")


# Finalizar paralelização
stopCluster(cl)
registerDoSEQ()
