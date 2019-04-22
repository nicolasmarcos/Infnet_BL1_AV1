setwd("/cloud/project/Case_chickwts")

df_galinhas <- chickwts

# Pré-Requisitos
install.packages("dplyr", dependencies = TRUE)
library(dplyr)


# Conhecendo o df

View(df_galinhas)
str(df_galinhas)
summary(df_galinhas)
hist(df_galinhas$weight)


# Diante dessa base, faça:
# • Qual dieta apresentou maior média de peso?

  # Resposta: Sunflower foi a dieta que apresentou maior média de peso 
media_peso_dieta <- df_galinhas %>% group_by(feed) %>% summarize(Media_Peso = mean(weight))
View(media_peso_dieta)


#  • Qual dieta apresentou maior homogeneidade dos pesos?

  # Uitilizou-se inicialmente histogramas para uma análise geral dos dados

hist(df_galinhas$weight[df_galinhas$feed == "casein"])
hist(df_galinhas$weight[df_galinhas$feed == "horsebean"])
hist(df_galinhas$weight[df_galinhas$feed == "linseed"])
hist(df_galinhas$weight[df_galinhas$feed == "meatmeal"])
hist(df_galinhas$weight[df_galinhas$feed == "soybean"])
hist(df_galinhas$weight[df_galinhas$feed == "sunflower"])

  # Resposta: Foi utilizado um standard-deviation com group by das dietas, onde se identificou
  # que a dieta horsebean teve a melhor homogeniedade
df_galinhas %>% group_by(feed) %>% summarize(Desvio_Padrao = sd(weight))


#  • Qual dieta seria escolhida para aumento de peso?

  # As dietas casein poderia ser a melhor escolha, visto que a distribuição da mesma 
  # gerou uma curva normal, focada entre 350-400 de peso e, diferentemente da sunflower,
  # não houveram tantos outliers
hist(df_galinhas$weight[df_galinhas$feed == "casein"])
df_galinhas %>% group_by(feed) %>% summarize(Soma_Peso = sum(weight))


