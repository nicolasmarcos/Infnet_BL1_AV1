install.packages("e1071", dependencies = TRUE)
library(e1071)
online_shoppers_intention <- read_csv("C:/DATA SCIENCE/INFNET/AVALIACAO ALGORITMOS 1 BLOCO/online_shoppers_intention.csv", 
                                      +     col_types = cols(Administrative = col_number(), 
                                                             +         Administrative_Duration = col_number(), 
                                                             +         BounceRates = col_number(), Browser = col_number(), 
                                                             +         ExitRates = col_number(), Informational = col_number(), 
                                                             +         Informational_Duration = col_number(), 
                                                             +         OperatingSystems = col_number(), 
                                                             +         PageValues = col_number(), ProductRelated = col_number(), 
                                                             +         ProductRelated_Duration = col_number(), 
                                                             +         Region = col_number(), SpecialDay = col_number(), 
                                                             +         TrafficType = col_number()))
> View(online_shoppers_intention)
setwd("C:\\DATA SCIENCE\\INFNET\\avaliacao_algoritmo_2")
save(online_shoppers_intention, file = "online_intention.csv")
list.files()
head(online_shoppers_intention)
dim(online_shoppers_intention)
set.seed(123)
onlineAmostra <- sample(2,12330, replace=T, prob=c(0.7, 0.3))
onlineAmostra
onlineTreino <- online_shoppers_intention[onlineAmostra==1,]
onlineteste <- online_shoppers_intention[onlineAmostra==2,]
dim(trainData)
dim(testData)
onlineModelo <- naiveBayes(Revenue ~ Administrative	+ Administrative_Duration +	Informational	+ Informational_Duration	+ ProductRelated + ProductRelated_Duration	+ BounceRates	+ ExitRates	+ PageValues	+ SpecialDay  + OperatingSystems + Browser	+ Region	+ TrafficType, onlineTreino)
onlineModelo
onlinePrevisao <- predict(onlineModelo, onlineteste)
onlinePrevisao
onlineConfusao <- table(onlineteste$Revenue, onlinePrevisao)
onlineAcertos <- (onlineConfusao[1] + onlineConfusao[4]) / sum(onlineConfusao) 
onlineAcertos
onlineErros <- (onlineConfusao[2] + onlineConfusao[3]) / sum(onlineConfusao) 
onlineErros
