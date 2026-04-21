# Usar o script 1-sunspots.R como referência

# Usar rminer
options(rgl.useNULL = TRUE)
library(rminer)

# Usar o dataset já tratado
source("eda_philadelphia.R")

# Armazenar o Num_Customers ordenado por Data em S (time series)
S <- ph$Num_Customers

# Número de previsões 7 (7 dias da semana)
NPRED <- 7

# Calcular o range de S
srange <- diff(range(S))

# Converter o dataset com lags de 7 (7 dias atrás)
D <- CasesSeries(S, c(1:7))

# Dividir dataset entre treino/teste
N <- nrow(D) # Total de valores
NTR <- N-NPRED # Total (N) - 7 (NPRED)
TR <- 1:NTR # Valores de treino serão todos os valores de início até há 7 dias atrás
TS <- (NTR+1):N # Valores de teste serão os valores de há 6 dias atrás até ao fim dos valores

# Treinar o modelo usando neural networks (multilayer perceptron ensemble)
NN <- fit(y~.,D[TR,],model="mlpe",search="heuristic")
# Treinar o modelo usando random forest
RF <- fit(y~.,D[TR,],model="randomForest",search="heuristic")

LTS <- length(TS) # Tamanho do dataset de teste
START <- nrow(D) - LTS+1 # Linha do dataset D onde começa o período de testes

# Fazer a previsão usando o modelo NN, já treinado
# Cada previsão é usada como input para a seguinte
PNN <- lforecast(NN,D,start=START,horizon=LTS)

# Fazer a previsão usando o modelo RF, já treinado
PRF <- lforecast(RF,D,start=START,horizon=LTS)

Y <- D[TS,]$y  # valores reais do período de teste para serem usados na avaliação do modelo

# Mostrar resultados da previsão do modelo NN
print("(mlpe) Previsão a 7 dias:")
print(PNN)

# Avaliar o modelo NN
cat("MAE:", mmetric(Y, PNN, metric="MAE"), "\n")
cat("NMAE:", mmetric(Y, PNN, metric="NMAE", val=srange), "\n")
cat("RMSE:", mmetric(Y, PNN, metric="RMSE"), "\n")
cat("R2:", mmetric(Y, PNN, metric="R22"), "\n")

# Mostrar resultados da previsão do modelo RF
print("(randomForest) Previsão a 7 dias:")
print(PRF)

# Avaliar o modelo RF
cat("MAE:", mmetric(Y, PRF, metric="MAE"), "\n")
cat("NMAE:", mmetric(Y, PRF, metric="NMAE", val=srange), "\n")
cat("RMSE:", mmetric(Y, PRF, metric="RMSE"), "\n")
cat("R2:", mmetric(Y, PRF, metric="R22"), "\n")

mgraph(Y, PNN, graph="REG", Grid=10, col=c("black","blue"),
       leg=list(pos="topright", leg=c("real","previsto")),main = "MLPE")

mgraph(Y, PRF, graph="REG", Grid=10, col=c("black","blue"),
       leg=list(pos="topright", leg=c("real","previsto")),main = "RandomForest")
