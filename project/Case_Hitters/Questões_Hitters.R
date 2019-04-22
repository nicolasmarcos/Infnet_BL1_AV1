setwd("/cloud/project/Case_Hitters")

#Para executar esse exercício você precisa:
#  • Instalar o pacote ISLR.

install.packages('ISLR',dependencies = TRUE)
library(ISLR)

install.packages("corrplot")
library(corrplot)


# • chamar a base “Hitters”.

df <- Hitters;

# • Descrição da base.

# A partir desta base, suponha que você precise fazer um rápido estudo
# sobre o salário dos jogadores de baseball. A intenção é entender quais
# seriam os fatores que mais influenciam no salário dos jogadores. Atenção!
#  É um relatório para a diretoria, portanto tem que ser visual.
# Crie um relatório simples com as seguintes informações:
#  • Histograma e boxplot dos salários.

  # Resposta:
hist(df$Salary)
boxplot(df$Salary)

# • Analise os valores faltantes

summary(df)

any(is.nan(df$AtBat) | is.na(df$AtBat))
any(is.nan(df$Hits) | is.na(df$Hits))
any(is.nan(df$HmRun) | is.na(df$HmRun))
any(is.nan(df$Runs) | is.na(df$Runs))
any(is.nan(df$RBI) | is.na(df$RBI))
any(is.nan(df$Walks) | is.na(df$Walks))
any(is.nan(df$Years) | is.na(df$Years))
any(is.nan(df$CAtBat) | is.na(df$CAtBat))
any(is.nan(df$CHits) | is.na(df$CHits))
any(is.nan(df$CHmRun) | is.na(df$CHmRun))
any(is.nan(df$CRuns) | is.na(df$CRuns))
any(is.nan(df$CRBI) | is.na(df$CRBI))
any(is.nan(df$CWalks) | is.na(df$CWalks))
any(is.nan(df$League) | is.na(df$League))
any(is.nan(df$Division) | is.na(df$Division))
any(is.nan(df$PutOuts) | is.na(df$PutOuts))
any(is.nan(df$Assists) | is.na(df$Assists))
any(is.nan(df$Errors) | is.na(df$Errors))
any(is.nan(df$Salary) | is.na(df$Salary))
any(is.nan(df$NewLeague) | is.na(df$NewLeague))

View(df$Salary)

nrow(df[is.nan(df$Salary) | is.na(df$Salary),])
(59/322)*100
#18.32%

summary(df$Salary)

# Para respostas às questões seguintes, se faz necessário uma tratativa de valores nulos.
# Por se tratar de uma distribuição irregular, não pareceu devido aplicar média para 
# preencher os valores nulos, pois poderia levar até a uma tomada de decisão enganosa,
# Podendo haver jogadores que ganham muito ou bem pouco e que não informaram.
# Decidiu-se então retirar os valores nulos. 
# Ainda, é possível notar que o terceiro quartil está bem longe do valor máximo.
# Optou-se por não retirar os outliers.

df_tratado <- df[!is.nan(df$Salary) & !is.na(df$Salary),]

# • Qual liga apresenta os maiores salários?

# Foi considerada a liga dos jogadores em 1986
df_tratado %>% group_by(League) %>% summarise(AVG_SALARY = mean(Salary))

# Resposta: Liga A apresenta a maior média de salários.

#  • Qual divisão apresenta os maiores salários?

# Foi considerada a divisão dos jogadores em 1986
df_tratado %>% group_by(Division) %>% summarise(AVG_SALARY = mean(Salary))

# Resposta: Divisão E apresenta a maior média de salários.

#  • Quais variáveis quantitativas apresentam maior correlação com o salário?

# Resposta: CRBI, CRuns, CHmRun, CHits, CAtBats são as que possuem maior correção com salário.

df_analise_correlacao <- df_tratado
df_analise_correlacao$League <- NULL
df_analise_correlacao$Division <- NULL
df_analise_correlacao$NewLeague <- NULL

matriz_correlacao <- cor(df_analise_correlacao)
corrplot(matriz_correlacao, method="number")

# Apresente suas conclusões, relacionadas à questão da influência
# dos fatores apresentados no salário.

# Resposta: As variváveis CRBI, CRuns, CHmRun, CHits e CAtBats 
# Apresentam certa correção com os salários de cerca de 50%. Todavia, pela amostragem de dados
# não parece ser o suficiente para assegurar qualidade em uma tomada de decisão.