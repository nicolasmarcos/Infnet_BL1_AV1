# Respostas

setwd("/media/n/6DEFCED45A78BAB8/workspace/Infnet/Bl1/AV1/Case_Titanic")


# Quantas variáveis e observações possui o arquivo?

df_titanic <- read.csv("titanic_train.csv",stringsAsFactors = FALSE)

# Resposta: O arquivo possui 12 variáveis e 891 observações

  
# Quais são as classes das variáveis?

# Resposta: Executar o comando abaixo:

sapply(df_titanic,class)

  #Prova Real

  str(df_titanic$PassengerId)
  str(df_titanic$Survived)
  str(df_titanic$Pclass)
  str(df_titanic$Name)
  str(df_titanic$Sex)
  str(df_titanic$Age)
  str(df_titanic$SibSp)
  str(df_titanic$Parch)
  str(df_titanic$Ticket)
  str(df_titanic$Fare)
  str(df_titanic$Cabin)
  str(df_titanic$Embarked)

# Qual é a média das dos preços dos tickets?

  #Valida se existe valor inválido ou nulo
  preco_invalido <- df_titanic[df_titanic$Fare < 0 | is.na(df_titanic$Fare)] 
  isTRUE(df_titanic[df_titanic$Fare < 0 | is.na(df_titanic$Fare)] )

# Resposta:  32.20421, Execute o código abaixo
mean(df_titanic$Fare)

# Faça um filtro na tabela e crie dois outros data frames. Um para o gênero masculino e o outro para o gênero feminino.

  #Verifa quais são os valores únicos de sexo
  unique(df_titanic$Sex)

  # Resposta: Divide os dataframes
  df_titanic_mas <- df_titanic[df_titanic$Sex == "male",]
  df_titanic_fem <- df_titanic[df_titanic$Sex == "female",]

# Crie duas listas, uma para informações do dataframe do gênero feminino e outra para o gênero masculino. Cada lista deve ser composta de:
# o Número total de passageiros
# o Número de sobreviventes
# o Número de passageiros na primeira classe
# o Preço do ticket
# o Número de parentes∖filhos

  total_pass <- nrow(df_titanic_mas)
  total_sobr <- nrow(df_titanic_mas[df_titanic_mas$Survived == 1,])
  total_pric <- nrow(df_titanic_mas[df_titanic_mas$Pclass == 1,])
  preco_medt <- median(df_titanic_mas$Fare)
  
  lista_masc <- c(total_pass,total_sobr,total_pric,preco_medt)
  
# • Com base nas listas criadas, responda:
#  o Qual gênero teve o maior número de pessoas embarcadas?
#  o Qual gênero sobreviveu mais?
#  o Qual gênero teve a maior média do número de parentes?