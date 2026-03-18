# Usar rminer (desativar inicialização do RGL devido ao sistema operativo usado)
options(rgl.useNULL = TRUE)
library(rminer)

# Carregar ficheiro csv e guardar na variável ph
ph = read.csv("philadelphia.csv")

# head(ph)        # Primeiras linhas
# str(ph)         # Estrutura e tipos de dados
# summary(ph)     # Estatísticas básicas

# Converter Date para Date
ph$Date <- as.Date(ph$Date)
# Converter TouristEvent para Factor após verificar que apenas tem 2 valores distintos
# unique(ph$TouristEvent)   # Resultado = Yes/No
ph$TouristEvent <- as.factor(ph$TouristEvent)


# ANÁLISE DOS OUTLIERS
# TouristEvent não faz sentido calcular outliers pois é uma variável categórica

# boxplot(ph$Date, main="Date") # Apenas para garantir que não existem datas anormais
# boxplot(ph$Num_Employees, main="Num_Employees")
# boxplot(ph$Num_Customers, main="Num_Customers")
# boxplot(ph$Pct_On_Sale, main="Pct_On_Sale")
# boxplot(ph$Sales, main="Sales")

# Identificar número de outliers
# length(boxplot.stats(ph$Num_Employees)$out)
# length(boxplot.stats(ph$Num_Customers)$out)
# length(boxplot.stats(ph$Pct_On_Sale)$out)
# length(boxplot.stats(ph$Sales)$out)

# Existirem zeros no Num_Employees é estranho neste contexto
# Verificar essas linhas detalhadamente
# ph[which(ph$Num_Employees == 0), ]

# Essas linhas referem-se ao dia 25 Dez, em que a loja esteve fechada
# Mesmo assim, devem-se remover de forma a não distorcer o modelo
ph <- ph[ph$Num_Employees != 0, ]

# Analisar as Sales tendo em conta se existe evento turístico ou não
# summary(ph[ph$TouristEvent == "Yes", "Sales"])
# summary(ph[ph$TouristEvent == "No", "Sales"])

# Há um número de vendas anormalmente alto sem haver evento turístico
# ph[which(ph$Sales == 252073), ]   # Idx: 146
# Vamos comparar com registos anteriores e seguintes
# ph[143:148,]
# Esse registo é do dia 23-11-2012 (Black Friday)
# Vamos alterar o campo TouristEvent para YES
ph[ph$Date == "2012-11-23", "TouristEvent"] <- "Yes"

# O mesmo se repete para a Black Friday 2013
# ph[which(ph$Sales == 241814), ]
ph[ph$Date == "2013-11-29", "TouristEvent"] <- "Yes"

# Substituir um valor nulo de Pct_On_Sale pela média
ph$Pct_On_Sale[is.na(ph$Pct_On_Sale)] <- mean(ph$Pct_On_Sale, na.rm=TRUE)

# Matriz de Correlações com outras variáveis numéricas
# cor(ph[, c("Num_Employees", "Num_Customers", "Pct_On_Sale", "Sales")])

# Correlacionar a variável target com o TouristicEvent
# summary(ph[ph$TouristEvent == "Yes", "Num_Customers"])
# summary(ph[ph$TouristEvent == "No", "Num_Customers"])

# Vai-se também entender o target dependendo da data
# Dia da semana
ph$WeekDay <- weekdays(ph$Date)
ph$WeekDay <- as.factor(ph$WeekDay)
# Mês
ph$Month <- months(ph$Date)
ph$Month <- as.factor(ph$Month)
# Dia do mês (não relevante o suficiente para se usar )
# ph$DiaMes <- as.integer(format(ph$Date, "%d"))

# Remover linhas com Num_Customers = 0 (domingo de Páscoa)
ph <- ph[ph$Num_Customers != 0, ]

# Perceber a distruibição de valores para 2 variáveis
# hist(ph$Num_Customers, main="Distribuição Num_Customers", xlab="Clientes")
# hist(ph$Pct_On_Sale, main="Distribuição Pct_On_Sale", xlab="% Promoção")

# Finalmente, ordenar o dataset por data
ph <- ph[order(ph$Date), ]
