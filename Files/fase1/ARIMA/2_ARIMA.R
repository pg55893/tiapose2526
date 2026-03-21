# ============================================================
# FASE II - BACKTESTING COM GROWING WINDOW
# Estilo Stor
# Modelo: ARIMA
# Target: Num_Customers
# ============================================================

library(forecast)
library(rminer)

# ------------------------------------------------------------
# 0. PASTA DE TRABALHO
# ------------------------------------------------------------
setwd("~/TIAPOSE_projeto/tiapose2526/Files/fase1")

# ------------------------------------------------------------
# 1. CARREGAR DADOS TRATADOS
# ------------------------------------------------------------
source("~/TIAPOSE_projeto/tiapose2526/Files/tratamentoDeDados.R")

# ------------------------------------------------------------
# 2. BALTIMORE
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("BACKTESTING - BALTIMORE\n")
cat("============================================================\n")

dados=baltimore
dados$Date=as.Date(dados$Date)
dados=dados[order(dados$Date),]
dados=dados[,c("Date","Num_Customers")]
dados=na.omit(dados)

d1=dados[,2]
datas=dados[,1]

L=length(d1)
K=7
print("incremental (growing) window training demonstration:")

Test=7
S=7
Runs=12
W=(L-Test)-(Runs-1)*S

YR=diff(range(d1))
if(YR==0) YR=1

MAE_v=vector(length=Runs)
NMAE_v=vector(length=Runs)
RMSE_v=vector(length=Runs)
RRSE_v=vector(length=Runs)

res_baltimore=data.frame()

for(b in 1:Runs)
{
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K)
  Y=d1[H$ts]
  
  print("model> auto.arima")
  AR=auto.arima(dtr)
  F=forecast(AR,h=length(H$ts))
  Pred=F$mean[1:Test]
  
  MAE_v[b]=mmetric(y=Y,x=Pred,metric="MAE")
  NMAE_v[b]=mmetric(y=Y,x=Pred,metric="NMAE",val=YR)
  RMSE_v[b]=mmetric(y=Y,x=Pred,metric="RMSE")
  RRSE_v[b]=mmetric(y=Y,x=Pred,metric="RRSE")
  
  ord=arimaorder(AR)
  
  linha=data.frame(
    Loja="Baltimore",
    Iteracao=b,
    Train_Start=datas[H$tr[1]],
    Train_End=datas[H$tr[length(H$tr)]],
    Train_Size=length(H$tr),
    Test_Start=datas[H$ts[1]],
    Test_End=datas[H$ts[length(H$ts)]],
    Test_Size=length(H$ts),
    Modelo=paste0("ARIMA(",ord[1],",",ord[2],",",ord[3],")"),
    MAE=round(MAE_v[b],4),
    NMAE=round(NMAE_v[b],4),
    RMSE=round(RMSE_v[b],4),
    RRSE=round(RRSE_v[b],4)
  )
  
  res_baltimore=rbind(res_baltimore,linha)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "MAE:",round(MAE_v[b],4),
      "NMAE:",round(NMAE_v[b],4),
      "RMSE:",round(RMSE_v[b],4),
      "RRSE:",round(RRSE_v[b],4),"\n")
  
  mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),
         leg=list(pos="topleft",leg=c("target","ARIMA pred.")))
  mpause()
}

med_baltimore=data.frame(
  Loja="Baltimore",
  Target="Num_Customers",
  Metodo="Growing Window",
  Iteracoes=Runs,
  Horizonte=Test,
  Incremento=S,
  Mediana_MAE=round(median(MAE_v),4),
  Mediana_NMAE=round(median(NMAE_v),4),
  Mediana_RMSE=round(median(RMSE_v),4),
  Mediana_RRSE=round(median(RRSE_v),4)
)

cat("median values for Baltimore:\n")
cat("MAE median:",median(MAE_v),"\n")
cat("NMAE median:",median(NMAE_v),"\n")
cat("RMSE median:",median(RMSE_v),"\n")
cat("RRSE median:",median(RRSE_v),"\n")

# ------------------------------------------------------------
# 3. LANCASTER
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("BACKTESTING - LANCASTER\n")
cat("============================================================\n")

dados=lancaster
dados$Date=as.Date(dados$Date)
dados=dados[order(dados$Date),]
dados=dados[,c("Date","Num_Customers")]
dados=na.omit(dados)

d1=dados[,2]
datas=dados[,1]

L=length(d1)
K=7
print("incremental (growing) window training demonstration:")

Test=7
S=7
Runs=12
W=(L-Test)-(Runs-1)*S

YR=diff(range(d1))
if(YR==0) YR=1

MAE_v=vector(length=Runs)
NMAE_v=vector(length=Runs)
RMSE_v=vector(length=Runs)
RRSE_v=vector(length=Runs)

res_lancaster=data.frame()

for(b in 1:Runs)
{
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K)
  Y=d1[H$ts]
  
  print("model> auto.arima")
  AR=auto.arima(dtr)
  F=forecast(AR,h=length(H$ts))
  Pred=F$mean[1:Test]
  
  MAE_v[b]=mmetric(y=Y,x=Pred,metric="MAE")
  NMAE_v[b]=mmetric(y=Y,x=Pred,metric="NMAE",val=YR)
  RMSE_v[b]=mmetric(y=Y,x=Pred,metric="RMSE")
  RRSE_v[b]=mmetric(y=Y,x=Pred,metric="RRSE")
  
  ord=arimaorder(AR)
  
  linha=data.frame(
    Loja="Lancaster",
    Iteracao=b,
    Train_Start=datas[H$tr[1]],
    Train_End=datas[H$tr[length(H$tr)]],
    Train_Size=length(H$tr),
    Test_Start=datas[H$ts[1]],
    Test_End=datas[H$ts[length(H$ts)]],
    Test_Size=length(H$ts),
    Modelo=paste0("ARIMA(",ord[1],",",ord[2],",",ord[3],")"),
    MAE=round(MAE_v[b],4),
    NMAE=round(NMAE_v[b],4),
    RMSE=round(RMSE_v[b],4),
    RRSE=round(RRSE_v[b],4)
  )
  
  res_lancaster=rbind(res_lancaster,linha)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "MAE:",round(MAE_v[b],4),
      "NMAE:",round(NMAE_v[b],4),
      "RMSE:",round(RMSE_v[b],4),
      "RRSE:",round(RRSE_v[b],4),"\n")
  
  mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),
         leg=list(pos="topleft",leg=c("target","ARIMA pred.")))
  mpause()
}

med_lancaster=data.frame(
  Loja="Lancaster",
  Target="Num_Customers",
  Metodo="Growing Window",
  Iteracoes=Runs,
  Horizonte=Test,
  Incremento=S,
  Mediana_MAE=round(median(MAE_v),4),
  Mediana_NMAE=round(median(NMAE_v),4),
  Mediana_RMSE=round(median(RMSE_v),4),
  Mediana_RRSE=round(median(RRSE_v),4)
)

cat("median values for Lancaster:\n")
cat("MAE median:",median(MAE_v),"\n")
cat("NMAE median:",median(NMAE_v),"\n")
cat("RMSE median:",median(RMSE_v),"\n")
cat("RRSE median:",median(RRSE_v),"\n")

# ------------------------------------------------------------
# 4. PHILADELPHIA
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("BACKTESTING - PHILADELPHIA\n")
cat("============================================================\n")

dados=philadelphia
dados$Date=as.Date(dados$Date)
dados=dados[order(dados$Date),]
dados=dados[,c("Date","Num_Customers")]
dados=na.omit(dados)

d1=dados[,2]
datas=dados[,1]

L=length(d1)
K=7
print("incremental (growing) window training demonstration:")

Test=7
S=7
Runs=12
W=(L-Test)-(Runs-1)*S

YR=diff(range(d1))
if(YR==0) YR=1

MAE_v=vector(length=Runs)
NMAE_v=vector(length=Runs)
RMSE_v=vector(length=Runs)
RRSE_v=vector(length=Runs)

res_philadelphia=data.frame()

for(b in 1:Runs)
{
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K)
  Y=d1[H$ts]
  
  print("model> auto.arima")
  AR=auto.arima(dtr)
  F=forecast(AR,h=length(H$ts))
  Pred=F$mean[1:Test]
  
  MAE_v[b]=mmetric(y=Y,x=Pred,metric="MAE")
  NMAE_v[b]=mmetric(y=Y,x=Pred,metric="NMAE",val=YR)
  RMSE_v[b]=mmetric(y=Y,x=Pred,metric="RMSE")
  RRSE_v[b]=mmetric(y=Y,x=Pred,metric="RRSE")
  
  ord=arimaorder(AR)
  
  linha=data.frame(
    Loja="Philadelphia",
    Iteracao=b,
    Train_Start=datas[H$tr[1]],
    Train_End=datas[H$tr[length(H$tr)]],
    Train_Size=length(H$tr),
    Test_Start=datas[H$ts[1]],
    Test_End=datas[H$ts[length(H$ts)]],
    Test_Size=length(H$ts),
    Modelo=paste0("ARIMA(",ord[1],",",ord[2],",",ord[3],")"),
    MAE=round(MAE_v[b],4),
    NMAE=round(NMAE_v[b],4),
    RMSE=round(RMSE_v[b],4),
    RRSE=round(RRSE_v[b],4)
  )
  
  res_philadelphia=rbind(res_philadelphia,linha)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "MAE:",round(MAE_v[b],4),
      "NMAE:",round(NMAE_v[b],4),
      "RMSE:",round(RMSE_v[b],4),
      "RRSE:",round(RRSE_v[b],4),"\n")
  
  mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),
         leg=list(pos="topleft",leg=c("target","ARIMA pred.")))
  mpause()
}

med_philadelphia=data.frame(
  Loja="Philadelphia",
  Target="Num_Customers",
  Metodo="Growing Window",
  Iteracoes=Runs,
  Horizonte=Test,
  Incremento=S,
  Mediana_MAE=round(median(MAE_v),4),
  Mediana_NMAE=round(median(NMAE_v),4),
  Mediana_RMSE=round(median(RMSE_v),4),
  Mediana_RRSE=round(median(RRSE_v),4)
)

cat("median values for Philadelphia:\n")
cat("MAE median:",median(MAE_v),"\n")
cat("NMAE median:",median(NMAE_v),"\n")
cat("RMSE median:",median(RMSE_v),"\n")
cat("RRSE median:",median(RRSE_v),"\n")

# ------------------------------------------------------------
# 5. RICHMOND
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("BACKTESTING - RICHMOND\n")
cat("============================================================\n")

dados=richmond
dados$Date=as.Date(dados$Date)
dados=dados[order(dados$Date),]
dados=dados[,c("Date","Num_Customers")]
dados=na.omit(dados)

d1=dados[,2]
datas=dados[,1]

L=length(d1)
K=7
print("incremental (growing) window training demonstration:")

Test=7
S=7
Runs=12
W=(L-Test)-(Runs-1)*S

YR=diff(range(d1))
if(YR==0) YR=1

MAE_v=vector(length=Runs)
NMAE_v=vector(length=Runs)
RMSE_v=vector(length=Runs)
RRSE_v=vector(length=Runs)

res_richmond=data.frame()

for(b in 1:Runs)
{
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K)
  Y=d1[H$ts]
  
  print("model> auto.arima")
  AR=auto.arima(dtr)
  F=forecast(AR,h=length(H$ts))
  Pred=F$mean[1:Test]
  
  MAE_v[b]=mmetric(y=Y,x=Pred,metric="MAE")
  NMAE_v[b]=mmetric(y=Y,x=Pred,metric="NMAE",val=YR)
  RMSE_v[b]=mmetric(y=Y,x=Pred,metric="RMSE")
  RRSE_v[b]=mmetric(y=Y,x=Pred,metric="RRSE")
  
  ord=arimaorder(AR)
  
  linha=data.frame(
    Loja="Richmond",
    Iteracao=b,
    Train_Start=datas[H$tr[1]],
    Train_End=datas[H$tr[length(H$tr)]],
    Train_Size=length(H$tr),
    Test_Start=datas[H$ts[1]],
    Test_End=datas[H$ts[length(H$ts)]],
    Test_Size=length(H$ts),
    Modelo=paste0("ARIMA(",ord[1],",",ord[2],",",ord[3],")"),
    MAE=round(MAE_v[b],4),
    NMAE=round(NMAE_v[b],4),
    RMSE=round(RMSE_v[b],4),
    RRSE=round(RRSE_v[b],4)
  )
  
  res_richmond=rbind(res_richmond,linha)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "MAE:",round(MAE_v[b],4),
      "NMAE:",round(NMAE_v[b],4),
      "RMSE:",round(RMSE_v[b],4),
      "RRSE:",round(RRSE_v[b],4),"\n")
  
  mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),
         leg=list(pos="topleft",leg=c("target","ARIMA pred.")))
  mpause()
}

med_richmond=data.frame(
  Loja="Richmond",
  Target="Num_Customers",
  Metodo="Growing Window",
  Iteracoes=Runs,
  Horizonte=Test,
  Incremento=S,
  Mediana_MAE=round(median(MAE_v),4),
  Mediana_NMAE=round(median(NMAE_v),4),
  Mediana_RMSE=round(median(RMSE_v),4),
  Mediana_RRSE=round(median(RRSE_v),4)
)

cat("median values for Richmond:\n")
cat("MAE median:",median(MAE_v),"\n")
cat("NMAE median:",median(NMAE_v),"\n")
cat("RMSE median:",median(RMSE_v),"\n")
cat("RRSE median:",median(RRSE_v),"\n")

# ------------------------------------------------------------
# 6. JUNTAR RESULTADOS
# ------------------------------------------------------------
resultados_iteracoes=rbind(
  res_baltimore,
  res_lancaster,
  res_philadelphia,
  res_richmond
)

resultados_mediana=rbind(
  med_baltimore,
  med_lancaster,
  med_philadelphia,
  med_richmond
)

cat("\n============================================================\n")
cat("RESULTADOS FINAIS - MEDIANA DAS MÉTRICAS POR LOJA\n")
cat("============================================================\n")
print(resultados_mediana)

# ------------------------------------------------------------
# 7. GUARDAR CSV
# ------------------------------------------------------------
write.csv(resultados_iteracoes,
          "backtesting_arima_iteracoes.csv",
          row.names=FALSE)

write.csv(resultados_mediana,
          "backtesting_arima_mediana_lojas.csv",
          row.names=FALSE)