setwd("/cloud/project/Case_ANP")

# Pré-Requisitos
install.packages("dplyr", dependencies = TRUE)
library(dplyr)


# Dica 1: na leitura use o argumento stringsAsFactors = FALSE. 

  # Realiza leitura dos dados
df_anp <- read.csv("dados_anp2.csv",sep=";",stringsAsFactors = FALSE, na.strings = "")

# Dica 2: transforme a coluna PRECO_COMPRA em númerica, com a função as.numeric(). 

  # Realiza a transformação em PRECO_COMPRA
df_anp$PRECO_COMPRA <- as.numeric(df_anp$PRECO_COMPRA)

  # Limpa as vírgulas de PRECO_VENDA para também "normalizá-lo"
df_anp$PRECO_VENDA <- gsub(",",".",df_anp$PRECO_VENDA)

  # Realiza a transformação em PRECO_VENDA
df_anp$PRECO_VENDA <- as.numeric(df_anp$PRECO_VENDA)

# Dica 3: faça os valores nulos de PRECO_COMPRA receberem NA. 

  # Transforma Not a Numbers em Not Avaliables de PRECO_COMPRA, se houver
any(is.nan(df_anp$PRECO_COMPRA))
df_anp$PRECO_COMPRA[is.nan(df_anp$PRECO_COMPRA)] <- NA

# Transforma Not a Numbers em Not Avaliables de PRECO_VENDA, se houver
any(is.nan(df_anp$PRECO_VENDA))
#df_anp$PRECO_VENDA[is.nan(df_anp$PRECO_VENDA)] <- NA

# • Use as funções summary() e str() para entender a base. Faça o que se pede: 

summary(df_anp) # não existe PRECO_VENDA ou PRECO_COMPRA negativo, ou outliers
str(df_anp)

# • Quantos preços foram coletados? 

  # Resposta: Foram coletados 20413 preços. Não há registros duplicados
nrow(df_anp)
any(duplicated(df_anp))

# • Crie uma tabela com a frequência de postos por combustível, 
# atribua essa tabela à variável “quantidade_postos”.

df_quantidade_postos <- (count(df_anp, COMBUSTIVEL,name = "quantidade_postos"))

df_anp <- merge(df_anp,df_quantidade_postos)

# Calcular a frequência de quantos postos possuem cada combustível e, no df, para cada registro onde combustível for X colocar o quantitativo

# • Qual combustível teve menos preços coletados? (Interpretar este resultado!) 

  # Resposta: GNV
count(df_anp,df_anp$COMBUSTIVEL,sort = TRUE)

  # Hipóteses Possíveis:
  # 1) O GNV ainda representa pouca parte do consumo de combustíveis no Brasil por automóveis;
  # 2) Este combustível poderia não ser o objeto de estudo alvo

# • Qual é o posto com menor preço de venda? 

  # Resposta: 
  df_anp[df_anp$PRECO_VENDA == min(df_anp$PRECO_VENDA),]

# É confiável essa fonte (Dica: olhe para o fornecedor e a bandeira.) 
  
  # Resposta:
  hist(df_anp$PRECO_VENDA)
  hist(df_anp$PRECO_VENDA[df_anp$COMBUSTIVEL == "GNV"])
  
  hist(df_anp$PRECO_VENDA[df_anp$BANDEIRA == "BRANCA" & df_anp$COMBUSTIVEL == "GNV"])
  
  # Pode-se duvidar dessa fonte pelos seguintes motivos:
  # 1) Este caso especificamente demonstra um outlier da distribuição, tanto de combustíveis gerais
  # quanto do combustível que pertence a esse caso.
  # 2) O outlier não é fundamentado por uma informação de PRECO_COMPRA que permita analisar veracidade
  # do PRECO_VENDA
  
# • Crie o dataframe “dados_etanol”, como um filtro do dataframe anp. Apresente “dados_etanol” por UF 
# e média dos preços de venda do etanol. 
  
  # Resposta: Criação df dados_etanol
  
  dados_etanol <- df_anp[df_anp$COMBUSTIVEL == "Etanol",]
  
  # Resposta: Apresentação
  
  dados_etanol %>% group_by(UF) %>% summarize(Preco_Vend_Medio = mean(PRECO_VENDA))
  
# • Qual é o estado com a menor média de preços de venda do etanol. Isso faz sentido? 
  
  media_etanol <- dados_etanol %>% group_by(UF) %>% summarize(Preco_Vend_Medio = mean(PRECO_VENDA)) 
  View(media_etanol) 
  
  # Resposta: São Paulo é o Estado com o menor preço médio. Isso é duvidoso, visto o ICMS do Estado e por ser a capital econômica do país. 
  
  
# • Exporte este último dataframe no formato CSV. 
  
  # Resposta: O último dataframe SOLICITADO foi o dados_etanol
  write.csv(dados_etanol, "dados_etanol.csv")

