setwd("C:\\DATA SCIENCE\\INFNET\\AVALIACAO ALGORITMOS 1 BLOCO")
install.packages("cluster")
install.packages("fpc")
library(fpc)
library(cluster)
attach(tripadvisor_review)
summary(tripadvisor_review)
set.seed(1234) 
trip <- tripadvisor_review[,-1]
(kmeans.result <- kmeans(trip, 5)) ## coloquei 5 pois sao 5 tipos de avaliacoes
plot(trip, col = kmeans.result$cluster) ## salvei o grafico para ter visao maior e comparar individualmente
# Visualizando os grupos e seus centros.
plot(trip[c("Category 2", "Category 7")], 
     col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Category 2",
                                "Category 7")], col=1:3, pch=8, cex=2)
## a visualizacao dos centros fica dificil interpretacao pelo PLOT. Escolhi a categoria 2 e 7 pelo fato do meu ponto
## de vista serem respectivamente a categoria menos consistente e a mais consistente
boxplot(trip, col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Category 2",
                                "Category 7")], col=1:3, pch=8, cex=2)
points(kmeans.result$centers[,c("Category 7",
                                "Category 2")], col=1:3, pch=8, cex=2)
## observa-se que a categoria 7,8 e 10 se destacam com as maiores notas e melhores distribuidas logo responde a 1 e a 3 
##pergunta da avaliacao. Os pontos centrais podem ser vistos mais facilmente pelo boxplot.
##salvei o grafico para ter visao maior e comparar individualmente
table(tripadvisor_review$`User ID`, kmeans.result$cluster) ## gerou uma tabela indicando o grupo de cada usuario
table(trip$`Category 7`, kmeans.result$cluster) ## por observacao no boxplot a categoria 7 ? a mais consistente e 
## possui uma variacao menor de notas e ficou mais facil de observar pela tabela como foi distribuido
plotcluster(trip[1:10],kmeans.result$cluster) ## com esse plot conseguimos visializar a distribuicao dos clusters
clusplot(trip[1:10],kmeans.result$cluster) ## ele coloca uma circulo em volta dos clusters. cada cluster ? represen-
## tado por um simbolo
plot(trip[,7:8], col = trip$`Category 10`, pch = kmeans.result$cluster ) 
hist(trip$`Category 1`, 
     col = kmeans.result$cluster)

## fazendo o histograma e olhando a tabela feita conseguimos ver a distribuicao dos clusters e interpretar melhor
## a tabela


