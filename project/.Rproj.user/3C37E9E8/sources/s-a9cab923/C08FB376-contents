# Respostas

setwd("/media/n/6DEFCED45A78BAB8/workspace/Infnet/Bl1/AV1/Case_Titanic")


# Quantas variáveis e observações possui o arquivo?

df_titanic <- read.csv("titanic_train.csv",stringsAsFactors = FALSE)

# Resposta: O arquivo possui 12 variáveis e 891 observações
# Observação: O arquivo foi importado sem factors para não atrapalhar tratativas futuras
  
# Quais são as classes das variáveis?

# Resposta: Executar o comando abaixo, que aplica a função class() para todos os elementos do df
# e retorna suas classes:

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

  #Valida se existe valor inválido ou nulo, inserindo em um df preco_invalido
  preco_invalido <- df_titanic[df_titanic$Fare < 0 | is.na(df_titanic$Fare)] 
  # Realiza a prova real da validação anterior
  isTRUE(df_titanic[df_titanic$Fare < 0 | is.na(df_titanic$Fare)] )

# Resposta:  média dos preços dos tickets é: 32.20421, Execute o código abaixo
mean(df_titanic$Fare)

# Faça um filtro na tabela e crie dois outros data frames. Um para o gênero masculino e o outro para o gênero feminino.

  #Verifa quais são os valores únicos de sexo
  unique(df_titanic$Sex)

  # Resposta: Divide os dataframes
  df_titanic_mas <- df_titanic[df_titanic$Sex == "male",]
  df_titanic_fem <- df_titanic[df_titanic$Sex == "female",]

# Crie duas listas, uma para informações do dataframe do gênero feminino e outra para o gênero masculino. Cada lista deve ser composta de:
# Número total de passageiros
# Número de sobreviventes
# Número de passageiros na primeira classe
# Preço do ticket
# Número de parentes∖filhos

#Lista Masculina
  
  # o Número total de passageiros
  total_pass <- nrow(df_titanic_mas)
  
  # o Número de sobreviventes
  total_sobr <- nrow(df_titanic_mas[df_titanic_mas$Survived == 1,])
  
  # o Número de passageiros na primeira classe
  total_pric <- nrow(df_titanic_mas[df_titanic_mas$Pclass == 1,])
  
  # o Preço do ticket
  preco_medt <- median(df_titanic_mas$Fare)
  
  # Média de Número de parentes∖filhos
  summary(df_titanic_mas$Parch)
  statmod <- function(x) {z <- table(as.vector(x)); names(z)[z == max(z)]}
  statmod(df_titanic_mas$Parch)
  
  # media = 0.0000  
  # mediana = 0.2357
  # moda = 0
  # Média e mediana não pareceram para este caso boas formas de se alcançar a tendência central.
  # De modo a identificar a quantitade de filhos mais comuns de passageiros, foi utilizada a moda
  # Que por coincidência, assemelhou-se a média
  
  media_filhos <- statmod(df_titanic_mas$Parch)
  
  
  
  lista_masc <- c(total_pass,total_sobr,total_pric,preco_medt,media_filhos)
  names(lista_masc) <- c("Número total de passageiros","Número de sobreviventes",
                         "Número de passageiros na primeira classe",
                         "Preço do ticket","Média de Número de parentes∖filhos")
  #Lista Masculina
  lista_masc

################################
    
  #Lista Feminina
  
  # o Número total de passageiros
  total_pass <- nrow(df_titanic_fem)
  
  # o Número de sobreviventes
  total_sobr <- nrow(df_titanic_fem[df_titanic_fem$Survived == 1,])
  
  # o Número de passageiros na primeira classe
  total_pric <- nrow(df_titanic_fem[df_titanic_fem$Pclass == 1,])
  
  # o Preço do ticket
  preco_medt <- median(df_titanic_fem$Fare)
  
  # Média de Número de parentes∖filhos
  summary(df_titanic_fem$Parch)
  statmod <- function(x) {z <- table(as.vector(x)); names(z)[z == max(z)]}
  statmod(df_titanic_fem$Parch)
  
  # media = 0.0000  
  # mediana = 0.6497
  # moda = 0
  # Média e mediana não pareceram para este caso boas formas de se alcançar a tendência central.
  # De modo a identificar a quantitade de filhos mais comuns de passageiros, foi utilizada a moda
  # Que por coincidência, assemelhou-se a média
  
  media_filhos <- statmod(df_titanic_fem$Parch)
  
  
  
  lista_fem <- c(total_pass,total_sobr,total_pric,preco_medt,media_filhos)
  names(lista_fem) <- c("Número total de passageiros","Número de sobreviventes",
                         "Número de passageiros na primeira classe",
                         "Preço do ticket","Média de Número de parentes∖filhos")
  #Lista Feminina
  lista_fem
  
  
# • Com base nas listas criadas, responda:
  
#  o Qual gênero teve o maior número de pessoas embarcadas?
  
  # Resposta:  "Homens"
  
  if(lista_masc[1] > lista_fem[1]) { "Homens" } else {"Mulheres"}
  
#  o Qual gênero sobreviveu mais?
  
  # Resposta: Mulheres
  
  if(lista_masc[2] > lista_fem[2]) { "Homens" } else {"Mulheres"}
  
  
#  o Qual gênero teve a maior média do número de parentes?
  
  # Resposta: Iguais
  
  if(lista_masc[5] == lista_fem[5]) { "Iguais" } else {
    if(lista_masc[5] > lista_fem[5]) { "Homens" } else { "Mulheres" } 
    
    }
  