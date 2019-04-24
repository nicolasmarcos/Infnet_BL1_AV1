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
install.packages("party", dependencies=TRUE)
Instalando os pacotes
library(party) # Árvores de decisão.
getwd()
setwd("C:\\DATA SCIENCE\\INFNET\\avaliacao_algoritmo_2")
save(online_shoppers_intention, file = "online_intention.csv")
list.files()
set.seed(123)
online <- sample(2, nrow(online_shoppers_intention), replace=TRUE, prob=c(0.7, 0.3))
trainData <- online_shoppers_intention[online==1,]
testData <- online_shoppers_intention[online==2,]
online2 <- Revenue ~ Administrative	+ Administrative_Duration +	Informational	+ Informational_Duration	+ ProductRelated + ProductRelated_Duration	+ BounceRates	+ ExitRates	+ PageValues	+ SpecialDay  + OperatingSystems + Browser	+ Region	+ TrafficType
online_ctree <- ctree(online2, data=trainData)
table(predict(online_ctree), trainData$Revenue)
print(online_ctree)
plot(online_ctree)
plot(online_ctree, type="simple")
# Testando o modelo no conjunto de testes
testPred <- predict(online_ctree, newdata = testData)
table(testPred, testData$Revenue)
