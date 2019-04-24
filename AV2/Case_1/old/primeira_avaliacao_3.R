## 3 Parte

usuarios_class <- table(tripadvisor_review$`User ID`, kmeans.result$cluster)
usuarios <- usuarios_class[,]
write.csv(usuarios, file = "usuarios.csv") ## transformei a tabela dos usuarios em dataframe
usuarios <- read_csv("usuarios.csv", col_types = cols(`1` = col_number(), 
                                                      `2` = col_number(), `3` = col_number(), 
                                                      `4` = col_number(), `5` = col_number(), 
                                                      X1 = col_number())) ## importei o arquivo
View(usuarios)
names(usuarios)[1:6] <- c("usuario","cluster 1", "cluster 2", "cluster 3","cluster 4", "cluster 5") ## alterei os
## nomes das colunas
sum(usuarios$`cluster 1`) ## somando os grupos
sum(usuarios$`cluster 2`) ## somando os grupos
sum(usuarios$`cluster 3`) ## somando os grupos
sum(usuarios$`cluster 4`) ## somando os grupos
sum(usuarios$`cluster 5`) ## somando os grupos
sum(usuarios)
usuariosClass <- usuarios[order(usuarios$usuario),]## ordenando crescente por usuarios
write.csv(usuariosClass, file = "usuariosClass.csv") ## salvando e transformando em dataframe
usuariosClass <- usuariosClass[,-1] ## eliminando a 1 coluna
usuariosCluster <- c(tripadvisor_review, usuariosClass) ## unindo os dataframes
write.csv(usuariosCluster, file = "usuariosCluster.csv") ## salvando o dataframe
usuariosCluster <- read_csv("usuariosCluster.csv", 
                            +     col_types = cols(Category.1 = col_number(), 
                                                   +         Category.10 = col_number(), Category.2 = col_number(), 
                                                   +         Category.3 = col_number(), Category.4 = col_number(), 
                                                   +         Category.5 = col_number(), Category.6 = col_number(), 
                                                   +         Category.7 = col_number(), Category.8 = col_number(), 
                                                   +         Category.9 = col_number(), cluster.1 = col_number(), 
                                                   +         cluster.2 = col_number(), cluster.3 = col_number(), 
                                                   +         cluster.4 = col_number(), cluster.5 = col_number(), 
                                                   +         usuario = col_number()))

usuariosCluster <- usuariosCluster[,-1] ## eliminando a 1 coluna
usuariosCluster <- usuariosCluster[,-12] ## eliminando a 12 coluna. Agora os usuarios podem ser definidos por grupos
usuariosCluster1 <- with(usuariosCluster, cluster.1 > 0) ## separando o grupo 1
sum(usuariosCluster1)
usuariosCluster2 <- with(usuariosCluster, cluster.2 > 0) ## separando o grupo 2
sum(usuariosCluster2)
usuariosCluster3 <- with(usuariosCluster, cluster.3 > 0) ## ## separando o grupo 3
sum(usuariosCluster3)
usuariosCluster4 <- with(usuariosCluster, cluster.4 > 0) ## ## separando o grupo 4
sum(usuariosCluster4)
usuariosCluster5 <- with(usuariosCluster, cluster.5 > 0) ## ## separando o grupo 5
sum(usuariosCluster5)
162+190+183+321+124
usuariosCluster1 <- usuariosCluster[usuariosCluster1,]
summary(usuariosCluster1)
usuariosCluster2 <- usuariosCluster[usuariosCluster2,]
summary(usuariosCluster2)
usuariosCluster3 <- usuariosCluster[usuariosCluster3,]
summary(usuariosCluster3)
usuariosCluster4 <- usuariosCluster[usuariosCluster4,]
summary(usuariosCluster4)
usuariosCluster5 <- usuariosCluster[usuariosCluster5,]
summary(usuariosCluster5)
## observando os grupos detalhadamente podemos observar que alem do grupo 4 possuir mais usuarios agrupados, é o grupo
## que possui as maiores notas no grupo mais consistente que é o 10. Possui maior mediana e maior media
## alem do boxplot mostrar que a consistencia permaneceu no grupo 4
boxplot(usuariosCluster1$Category.7,usuariosCluster1$Category.8,usuariosCluster1$Category.10)
boxplot(usuariosCluster2$Category.7,usuariosCluster2$Category.8,usuariosCluster2$Category.10)
boxplot(usuariosCluster3$Category.7,usuariosCluster3$Category.8,usuariosCluster3$Category.10)
boxplot(usuariosCluster4$Category.7,usuariosCluster4$Category.8,usuariosCluster4$Category.10)
boxplot(usuariosCluster5$Category.7,usuariosCluster5$Category.8,usuariosCluster5$Category.10)
