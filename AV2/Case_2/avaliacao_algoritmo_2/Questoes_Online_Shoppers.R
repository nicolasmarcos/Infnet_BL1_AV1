# De posse destes dados crie um modelo que indique a propensão de
# compra de um novo cliente com este mesmo conjunto de atributos.
# Documente seu processo de análise em um relatório com as escolhas,
# justificativas, testes, gráficos explicativos e qualquer outra informação que
# ache pertinente.

# R: Analisando o problema proposto, percebe-se tratar de um problema de classificação.
# O dataframe será submetido a diferentes algoritmos de classificação, onde posteriormente
# serão analisadas as acurácias e escolhida a melhor solução para se aplicar.

# Setando working directory
setwd("/media/n/6DEFCED45A78BAB8/workspace/Infnet/Bl1/AV2/Case_2/avaliacao_algoritmo_2")

# Importação e instalação de pacotes necessários

install.packages('e1071')
install.packages("caTools")
install.packages("caret")
install.packages('rpart')
install.packages('rpart.plot')
install.packages("randomForest",dependencies = TRUE)

library(e1071)
library(caTools)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

# Leitura inicial do data_frame

df <- read.csv("online_shoppers_intention.csv",sep = ",", stringsAsFactors = FALSE)

# Conhecendo o dataframe

# Verifica a estrutura importada
str(df)

# Verifica os valores
summary(df)

# Verifica valores faltantes
apply(df, 2, function(x) any(is.na(x) | is.nan(x)))

########## TESTES POR NAIVE BAYES 1 ##########

########## CENÁRIO 1.1 - PRÉ-PROCESSAMENTO C/ ESCALONAMENTO E ENCODE ##########

# Pré-Processamento

# Tratamento dos campos categóricos

df_bayes <- df

unique(df_bayes$Month)
df_bayes$Month = factor(df_bayes$Month, levels = c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep','Oct','Nov','Dec'), labels = c(2, 3, 5, 6, 7,8,9,10,11,12))

unique(df_bayes$VisitorType)
df_bayes$VisitorType = factor(df_bayes$VisitorType, levels = c('New_Visitor','Returning_Visitor','Other'), labels = c(0,1,2))

df_bayes$Weekend = factor(df_bayes$Weekend, levels = c(FALSE,TRUE), labels = c(0,1))
df_bayes$Revenue = factor(df_bayes$Revenue, levels = c(FALSE,TRUE), labels = c(0,1))

# Escalonamento para melhor processamento no nayve bayes
df_bayes_escalonado <- df_bayes
df_bayes_escalonado[,1:10] <- scale(df_bayes_escalonado[,1:10])
df_bayes_escalonado[,12:15] <- scale(df_bayes_escalonado[,12:15])

str(df_bayes)

# Realiza a divisão da base de treinamento e teste, setando seed

set.seed(1234)
divisao = sample.split(df_bayes_escalonado$Revenue, SplitRatio = 0.75)
base_treinamento_bayes = subset(df_bayes_escalonado, divisao == TRUE)
base_teste_bayes = subset(df_bayes_escalonado, divisao == FALSE)

# Realiza classificação de naive bayes

classificador_bayes <- naiveBayes(x = base_treinamento_bayes[-18], y = base_treinamento_bayes$Revenue )
print(classificador_bayes)

# Realiza previsões com base no classificador

previsoes <- predict(classificador_bayes, newdata = base_teste_bayes[-18])

# Verifica a acurácia e matriz de confusão

matriz_confusao_bayes <- table(base_teste_bayes[,18],previsoes)
print(matriz_confusao_bayes)
confusionMatrix(matriz_confusao_bayes)

# Acurácia: 0.80

########## CENÁRIO 1.2 - PRÉ-PROCESSAMENTO C/ ENCODE ##########

# Pré-Processamento

# Tratamento dos campos categóricos

df_bayes <- df

unique(df_bayes$Month)
df_bayes$Month = factor(df_bayes$Month, levels = c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep','Oct','Nov','Dec'), labels = c(2, 3, 5, 6, 7,8,9,10,11,12))

unique(df_bayes$VisitorType)
df_bayes$VisitorType = factor(df_bayes$VisitorType, levels = c('New_Visitor','Returning_Visitor','Other'), labels = c(0,1,2))

df_bayes$Weekend = factor(df_bayes$Weekend, levels = c(FALSE,TRUE), labels = c(0,1))
df_bayes$Revenue = factor(df_bayes$Revenue, levels = c(FALSE,TRUE), labels = c(0,1))

str(df_bayes)

# Realiza a divisão da base de treinamento e teste, setando seed

set.seed(1234)
divisao = sample.split(df_bayes$Revenue, SplitRatio = 0.75)
base_treinamento_bayes = subset(df_bayes, divisao == TRUE)
base_teste_bayes = subset(df_bayes, divisao == FALSE)

# Realiza classificação de naive bayes

classificador_bayes <- naiveBayes(x = base_treinamento_bayes[-18], y = base_treinamento_bayes$Revenue )
print(classificador_bayes)

# Realiza previsões com base no classificador

previsoes <- predict(classificador_bayes, newdata = base_teste_bayes[-18])

# Verifica a acurácia e matriz de confusão

matriz_confusao_bayes <- table(base_teste_bayes[,18],previsoes)
print(matriz_confusao_bayes)
confusionMatrix(matriz_confusao_bayes)

# Acurácia 0,80

########## CENÁRIO 1.3 - SEM PRÉ-PROCESSAMENTO ##########

df_bayes <- df

str(df_bayes)

# Realiza a divisão da base de treinamento e teste, setando seed

set.seed(1234)
divisao = sample.split(df_bayes$Revenue, SplitRatio = 0.75)
base_treinamento_bayes = subset(df_bayes, divisao == TRUE)
base_teste_bayes = subset(df_bayes, divisao == FALSE)

# Realiza classificação de naive bayes

classificador_bayes <- naiveBayes(x = base_treinamento_bayes[-18], y = base_treinamento_bayes$Revenue )
print(classificador_bayes)

# Realiza previsões com base no classificador

previsoes <- predict(classificador_bayes, newdata = base_teste_bayes[-18])

# Verifica a acurácia e matriz de confusão

matriz_confusao_bayes <- table(base_teste_bayes[,18],previsoes)
print(matriz_confusao_bayes)
confusionMatrix(matriz_confusao_bayes)

# Acurácia: 0,80

########## TESTES POR ÁRVORE DE DECISÃO 2 ##########

########## CENÁRIO 2.1 ÁRVORE COM PRÉ-PROCESSAMENTO DE ENCODING E ESCALONAMENTO ##########

# Pré-Processamento

# Tratamento dos campos categóricos

df_arvore <- df

str(df_arvore)

unique(df_arvore$Month)
df_arvore$Month = factor(df_arvore$Month, levels = c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep','Oct','Nov','Dec'), labels = c(2, 3, 5, 6, 7,8,9,10,11,12))

unique(df_arvore$VisitorType)
df_arvore$VisitorType = factor(df_arvore$VisitorType, levels = c('New_Visitor','Returning_Visitor','Other'), labels = c(0,1,2))


df_arvore$Weekend = factor(df_arvore$Weekend, levels = c(FALSE,TRUE), labels = c(0,1))
df_arvore$Revenue = factor(df_arvore$Revenue, levels = c(FALSE,TRUE), labels = c(0,1))

# Escalonamento para melhor processamento
df_arvore_escalonado <- df_arvore
df_arvore_escalonado[,1:10] <- scale(df_arvore_escalonado[,1:10])
df_arvore_escalonado[,12:15] <- scale(df_arvore_escalonado[,12:15])

str(df_arvore_escalonado)


# Realiza a divisão da base de treinamento e teste, setando seed

set.seed(1234)
divisao = sample.split(df_arvore_escalonado$Revenue, SplitRatio = 0.75)
base_treinamento_arvore= subset(df_arvore_escalonado, divisao == TRUE)
base_teste_arvore = subset(df_arvore_escalonado, divisao == FALSE)

# Realiza classificação

classificador_arvore <- rpart(formula = Revenue ~ ., data = base_treinamento_arvore)
print(classificador_arvore)
plot(classificador_arvore)

rpart.plot(classificador_arvore)

# Realiza previsões com base no classificador

previsoes <- predict(classificador_arvore, newdata = base_teste_arvore[,-18], type = 'class')
print(previsoes)
# Verifica a acurácia e matriz de confusão

matriz_confusao_arvore <- table(base_teste_arvore[,18],previsoes)
print(matriz_confusao_arvore)
confusionMatrix(matriz_confusao_arvore)

# R: Acurácia: 0,8969

########## CENÁRIO 2.2 ÁRVORE COM PRÉ-PROCESSAMENTO DE ENCODING ##########


df_arvore <- df

str(df_arvore)

unique(df_arvore$Month)
df_arvore$Month = factor(df_arvore$Month, levels = c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep','Oct','Nov','Dec'), labels = c(2, 3, 5, 6, 7,8,9,10,11,12))

unique(df_arvore$VisitorType)
df_arvore$VisitorType = factor(df_arvore$VisitorType, levels = c('New_Visitor','Returning_Visitor','Other'), labels = c(0,1,2))


df_arvore$Weekend = factor(df_arvore$Weekend, levels = c(FALSE,TRUE), labels = c(0,1))
df_arvore$Revenue = factor(df_arvore$Revenue, levels = c(FALSE,TRUE), labels = c(0,1))

# Realiza a divisão da base de treinamento e teste, setando seed

set.seed(1234)
divisao = sample.split(df_arvore$Revenue, SplitRatio = 0.75)
base_treinamento_arvore= subset(df_arvore, divisao == TRUE)
base_teste_arvore = subset(df_arvore, divisao == FALSE)

# Realiza classificação

classificador_arvore <- rpart(formula = Revenue ~ ., data = base_treinamento_arvore)
print(classificador_arvore)
plot(classificador_arvore)

rpart.plot(classificador_arvore)

# Realiza previsões com base no classificador

previsoes <- predict(classificador_arvore, newdata = base_teste_arvore[,-18], type = 'class')
print(previsoes)
# Verifica a acurácia e matriz de confusão

matriz_confusao_arvore <- table(base_teste_arvore[,18],previsoes)
print(matriz_confusao_arvore)
confusionMatrix(matriz_confusao_arvore)

# Acurácia: 0,8969

########## CENÁRIO 2.3 ÁRVORE SEM PRÉ-PROCESSAMENTO, SALVO CLASSE ##########


df_arvore <- df

str(df_arvore)

df_arvore$Revenue = factor(df_arvore$Revenue, levels = c(FALSE,TRUE), labels = c(0,1))

# Realiza a divisão da base de treinamento e teste, setando seed

set.seed(1234)
divisao = sample.split(df_arvore$Revenue, SplitRatio = 0.75)
base_treinamento_arvore= subset(df_arvore, divisao == TRUE)
base_teste_arvore = subset(df_arvore, divisao == FALSE)

# Realiza classificação

classificador_arvore <- rpart(formula = Revenue ~ ., data = base_treinamento_arvore)
print(classificador_arvore)
plot(classificador_arvore)

rpart.plot(classificador_arvore)

# Realiza previsões com base no classificador

previsoes <- predict(classificador_arvore, newdata = base_teste_arvore[,-18], type = 'class')
print(previsoes)
# Verifica a acurácia e matriz de confusão

matriz_confusao_arvore <- table(base_teste_arvore[,18],previsoes)
print(matriz_confusao_arvore)
confusionMatrix(matriz_confusao_arvore)

# Acurácia: 0,8969

########## TESTES POR RANDOM FOREST 3 ##########

########## CENÁRIO 3.1 ÁRVORE COM PRÉ-PROCESSAMENTO DE ENCODING E ESCALONAMENTO ##########

# Pré-Processamento

# Tratamento dos campos categóricos

df_rf <- df

str(df_rf)

unique(df_rf$Month)
df_rf$Month = factor(df_rf$Month, levels = c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep','Oct','Nov','Dec'), labels = c(2, 3, 5, 6, 7,8,9,10,11,12))

unique(df_rf$VisitorType)
df_rf$VisitorType = factor(df_rf$VisitorType, levels = c('New_Visitor','Returning_Visitor','Other'), labels = c(0,1,2))


df_rf$Weekend = factor(df_rf$Weekend, levels = c(FALSE,TRUE), labels = c(0,1))
df_rf$Revenue = factor(df_rf$Revenue, levels = c(FALSE,TRUE), labels = c(0,1))

# Escalonamento para melhor processamento
df_rf_escalonado <- df_rf
df_rf_escalonado[,1:10] <- scale(df_rf_escalonado[,1:10])
df_rf_escalonado[,12:15] <- scale(df_rf_escalonado[,12:15])

str(df_rf_escalonado)


# Realiza a divisão da base de treinamento e teste, setando seed

set.seed(1234)
divisao = sample.split(df_rf_escalonado$Revenue, SplitRatio = 0.75)
base_treinamento_rf= subset(df_rf_escalonado, divisao == TRUE)
base_teste_rf = subset(df_rf_escalonado, divisao == FALSE)

# Realiza classificação

classificador_rf <- randomForest(x = base_treinamento_rf[,-18],y = base_treinamento_rf$Revenue,ntree = 30)
print(classificador_rf)
plot(classificador_rf)

# Realiza previsões com base no classificador

previsoes <- predict(classificador_rf, newdata = base_teste_rf[,-18])
print(previsoes)
# Verifica a acurácia e matriz de confusão

matriz_confusao_rf <- table(base_teste_rf[,18],previsoes)
print(matriz_confusao_rf)
confusionMatrix(matriz_confusao_rf)

# R: Acurácia: 0,9046


########## CENÁRIO 3.2 ÁRVORE COM PRÉ-PROCESSAMENTO DE ENCODING ##########

# Pré-Processamento

# Tratamento dos campos categóricos

df_rf <- df

str(df_rf)

unique(df_rf$Month)
df_rf$Month = factor(df_rf$Month, levels = c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep','Oct','Nov','Dec'), labels = c(2, 3, 5, 6, 7,8,9,10,11,12))

unique(df_rf$VisitorType)
df_rf$VisitorType = factor(df_rf$VisitorType, levels = c('New_Visitor','Returning_Visitor','Other'), labels = c(0,1,2))


df_rf$Weekend = factor(df_rf$Weekend, levels = c(FALSE,TRUE), labels = c(0,1))
df_rf$Revenue = factor(df_rf$Revenue, levels = c(FALSE,TRUE), labels = c(0,1))

# Realiza a divisão da base de treinamento e teste, setando seed

set.seed(1234)
divisao = sample.split(df_rf$Revenue, SplitRatio = 0.75)
base_treinamento_rf= subset(df_rf, divisao == TRUE)
base_teste_rf = subset(df_rf, divisao == FALSE)

# Realiza classificação

classificador_rf <- randomForest(x = base_treinamento_rf[,-18],y = base_treinamento_rf$Revenue,ntree = 30)
print(classificador_rf)
plot(classificador_rf)

# Realiza previsões com base no classificador

previsoes <- predict(classificador_rf, newdata = base_teste_rf[,-18])
print(previsoes)
# Verifica a acurácia e matriz de confusão

matriz_confusao_rf <- table(base_teste_rf[,18],previsoes)
print(matriz_confusao_rf)
confusionMatrix(matriz_confusao_rf)

# R: Acurácia: 0,903


