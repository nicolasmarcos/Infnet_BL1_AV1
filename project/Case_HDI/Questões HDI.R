setwd("/cloud/project/Case_HDI")

# Crie uma função que classifique os países (em uma coluna extra) em
# 2014 de acordo com a tabela acima.
#   • Qual país cresceu mais em relação à 2013?
#   • Qual país caiu mais em relação à 2013?
#   • Quantos países estão com classificação baixa?
#   • Qual é a posição do Brasil?

df_hdi <- read.csv("Human_development_index_HDI.csv", sep = ";")


# Crie uma função que classifique os países (em uma coluna extra) em
# 2014 de acordo com a tabela acima.

  # Resposta: Gerada uma nova coluna com os valores, usando cut()

df_hdi$Classificacao <- cut(df_hdi$Ano_2014 ,
                            breaks= c(-Inf,0.534,0.710,0.796, Inf),
                            labels=c("Baixo","Médio","Alto","Muito Alto") )

#   • Qual país cresceu mais em relação à 2013?

  # Criada uma nova coluna com índice
df_hdi$indices_crescimento <- df_hdi$Ano_2014 - df_hdi$Ano_2013

  # Colhido o maior índice
  # Resposta: Togo
df_hdi[df_hdi$indices_crescimento == max(df_hdi$indices_crescimento),]

#   • Qual país caiu mais em relação à 2013?

  # Colhido os menores índices
  # Resposta: Libya e Syrian Arab Republic
df_hdi[df_hdi$indices_crescimento == min(df_hdi$indices_crescimento),]


#   • Quantos países estão com classificação baixa?

  # Resposta: 40. Aplicado um nrow() com filtro
nrow(df_hdi[df_hdi$Classificacao == "Baixo",])

#   • Qual é a posição do Brasil?

  # Resposta: 75º. Aplicado um grep() com a string procurada como filtro
df_hdi$HDI.Rank[grep("Brazil",df_hdi$Country)]


