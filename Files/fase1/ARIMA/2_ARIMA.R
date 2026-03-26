# ============================================================
# FASE II - BACKTESTING COM GROWING WINDOW
# Modelo: ARIMA
# Target: Num_Customers
# ============================================================

library(forecast)
library(rminer)

# ------------------------------------------------------------
# 0. PASTA DE TRABALHO
# ------------------------------------------------------------
# --- Criar e ir para a pasta ARIMA para guardar resultados ---
dir.create("~/TIAPOSE2526/Files/fase1/ARIMA", showWarnings=FALSE)
setwd("~/TIAPOSE2526/Files/fase1/ARIMA")

# ------------------------------------------------------------
# 1. CARREGAR DADOS TRATADOS
# ------------------------------------------------------------
source("~/TIAPOSE2526/Files/tratamentoDeDados.R")

# --- Voltar para fase1 para guardar resultados ---
setwd("~/TIAPOSE2526/Files/fase1")

# ------------------------------------------------------------
# 2. DATAS EM QUE A LOJA ESTA FECHADA (Natal e Pascoa)
# ------------------------------------------------------------
datas_fecho <- as.Date(c("2012-12-25","2013-12-25","2013-03-31","2014-04-20"))

# ------------------------------------------------------------
# 3. BALTIMORE
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
R2_v=vector(length=Runs)

res_baltimore=data.frame()

for(b in 1:Runs)
{
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K)
  Y=d1[H$ts]
  
  print("model> auto.arima")
  AR=auto.arima(dtr)
  fc=forecast(AR,h=length(H$ts))
  Pred=as.numeric(fc$mean[1:Test])
  
  # --- Pos-processamento ---
  Pred[Pred < 0] <- 0
  datas_teste=datas[H$ts]
  Pred[datas_teste %in% datas_fecho] <- 0
  
  MAE_v[b]=mmetric(y=Y,x=Pred,metric="MAE")
  NMAE_v[b]=mmetric(y=Y,x=Pred,metric="NMAE",val=YR)
  RMSE_v[b]=mmetric(y=Y,x=Pred,metric="RMSE")
  RRSE_v[b]=mmetric(y=Y,x=Pred,metric="RRSE")
  R2_v[b]=1-(sum((Y-Pred)^2)/sum((Y-mean(Y))^2))
  
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
    RRSE=round(RRSE_v[b],4),
    R2=round(R2_v[b],4)
  )
  
  res_baltimore=rbind(res_baltimore,linha)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "MAE:",round(MAE_v[b],4),
      "NMAE:",round(NMAE_v[b],4),
      "RMSE:",round(RMSE_v[b],4),
      "RRSE:",round(RRSE_v[b],4),
      "R2:",round(R2_v[b],4),"\n")
  
  mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),
         leg=list(pos="topleft",leg=c("target","ARIMA pred.")))
  mpause()
}

# --- Boxplot das metricas ---

pdf("boxplot_arima_baltimore.pdf", width=7, height=5)
boxplot(NMAE_v,RRSE_v,R2_v,
        names=c("NMAE","RRSE","R2"),
        main="Baltimore - ARIMA - Distribuicao das Metricas (12 iteracoes)",
        col="steelblue")
dev.off()

med_baltimore=data.frame(
  Loja="Baltimore",
  Target="Num_Customers",
  Metodo="Growing Window",
  Iteracoes=Runs,
  Horizonte=Test,
  Incremento=S,
  Media_MAE=round(mean(MAE_v),4),
  Media_NMAE=round(mean(NMAE_v),4),
  Media_RMSE=round(mean(RMSE_v),4),
  Media_RRSE=round(mean(RRSE_v),4),
  Media_R2=round(mean(R2_v),4)
)

cat("mean values for Baltimore:\n")
cat("MAE mean:",mean(MAE_v),"\n")
cat("NMAE mean:",mean(NMAE_v),"\n")
cat("RMSE mean:",mean(RMSE_v),"\n")
cat("RRSE mean:",mean(RRSE_v),"\n")
cat("R2 mean:",mean(R2_v),"\n")

# ------------------------------------------------------------
# 4. LANCASTER
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
R2_v=vector(length=Runs)

res_lancaster=data.frame()

for(b in 1:Runs)
{
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K)
  Y=d1[H$ts]
  
  print("model> auto.arima")
  AR=auto.arima(dtr)
  fc=forecast(AR,h=length(H$ts))
  Pred=as.numeric(fc$mean[1:Test])
  
  # --- Pos-processamento ---
  Pred[Pred < 0] <- 0
  datas_teste=datas[H$ts]
  Pred[datas_teste %in% datas_fecho] <- 0
  
  MAE_v[b]=mmetric(y=Y,x=Pred,metric="MAE")
  NMAE_v[b]=mmetric(y=Y,x=Pred,metric="NMAE",val=YR)
  RMSE_v[b]=mmetric(y=Y,x=Pred,metric="RMSE")
  RRSE_v[b]=mmetric(y=Y,x=Pred,metric="RRSE")
  R2_v[b]=1-(sum((Y-Pred)^2)/sum((Y-mean(Y))^2))
  
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
    RRSE=round(RRSE_v[b],4),
    R2=round(R2_v[b],4)
  )
  
  res_lancaster=rbind(res_lancaster,linha)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "MAE:",round(MAE_v[b],4),
      "NMAE:",round(NMAE_v[b],4),
      "RMSE:",round(RMSE_v[b],4),
      "RRSE:",round(RRSE_v[b],4),
      "R2:",round(R2_v[b],4),"\n")
  
  mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),
         leg=list(pos="topleft",leg=c("target","ARIMA pred.")))
  mpause()
}

# --- Boxplot das metricas ---
boxplot(NMAE_v,RRSE_v,R2_v,
        names=c("NMAE","RRSE","R2"),
        main="Lancaster - ARIMA - Distribuicao das Metricas (12 iteracoes)",
        col="tomato")

med_lancaster=data.frame(
  Loja="Lancaster",
  Target="Num_Customers",
  Metodo="Growing Window",
  Iteracoes=Runs,
  Horizonte=Test,
  Incremento=S,
  Media_MAE=round(mean(MAE_v),4),
  Media_NMAE=round(mean(NMAE_v),4),
  Media_RMSE=round(mean(RMSE_v),4),
  Media_RRSE=round(mean(RRSE_v),4),
  Media_R2=round(mean(R2_v),4)
)

cat("mean values for Lancaster:\n")
cat("MAE mean:",mean(MAE_v),"\n")
cat("NMAE mean:",mean(NMAE_v),"\n")
cat("RMSE mean:",mean(RMSE_v),"\n")
cat("RRSE mean:",mean(RRSE_v),"\n")
cat("R2 mean:",mean(R2_v),"\n")

# ------------------------------------------------------------
# 5. PHILADELPHIA
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
R2_v=vector(length=Runs)

res_philadelphia=data.frame()

for(b in 1:Runs)
{
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K)
  Y=d1[H$ts]
  
  print("model> auto.arima")
  AR=auto.arima(dtr)
  fc=forecast(AR,h=length(H$ts))
  Pred=as.numeric(fc$mean[1:Test])
  
  # --- Pos-processamento ---
  Pred[Pred < 0] <- 0
  datas_teste=datas[H$ts]
  Pred[datas_teste %in% datas_fecho] <- 0
  
  MAE_v[b]=mmetric(y=Y,x=Pred,metric="MAE")
  NMAE_v[b]=mmetric(y=Y,x=Pred,metric="NMAE",val=YR)
  RMSE_v[b]=mmetric(y=Y,x=Pred,metric="RMSE")
  RRSE_v[b]=mmetric(y=Y,x=Pred,metric="RRSE")
  R2_v[b]=1-(sum((Y-Pred)^2)/sum((Y-mean(Y))^2))
  
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
    RRSE=round(RRSE_v[b],4),
    R2=round(R2_v[b],4)
  )
  
  res_philadelphia=rbind(res_philadelphia,linha)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "MAE:",round(MAE_v[b],4),
      "NMAE:",round(NMAE_v[b],4),
      "RMSE:",round(RMSE_v[b],4),
      "RRSE:",round(RRSE_v[b],4),
      "R2:",round(R2_v[b],4),"\n")
  
  mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),
         leg=list(pos="topleft",leg=c("target","ARIMA pred.")))
  mpause()
}

# --- Boxplot das metricas ---
boxplot(NMAE_v,RRSE_v,R2_v,
        names=c("NMAE","RRSE","R2"),
        main="Philadelphia - ARIMA - Distribuicao das Metricas (12 iteracoes)",
        col="seagreen")

med_philadelphia=data.frame(
  Loja="Philadelphia",
  Target="Num_Customers",
  Metodo="Growing Window",
  Iteracoes=Runs,
  Horizonte=Test,
  Incremento=S,
  Media_MAE=round(mean(MAE_v),4),
  Media_NMAE=round(mean(NMAE_v),4),
  Media_RMSE=round(mean(RMSE_v),4),
  Media_RRSE=round(mean(RRSE_v),4),
  Media_R2=round(mean(R2_v),4)
)

cat("mean values for Philadelphia:\n")
cat("MAE mean:",mean(MAE_v),"\n")
cat("NMAE mean:",mean(NMAE_v),"\n")
cat("RMSE mean:",mean(RMSE_v),"\n")
cat("RRSE mean:",mean(RRSE_v),"\n")
cat("R2 mean:",mean(R2_v),"\n")

# ------------------------------------------------------------
# 6. RICHMOND
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
R2_v=vector(length=Runs)

res_richmond=data.frame()

for(b in 1:Runs)
{
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K)
  Y=d1[H$ts]
  
  print("model> auto.arima")
  AR=auto.arima(dtr)
  fc=forecast(AR,h=length(H$ts))
  Pred=as.numeric(fc$mean[1:Test])
  
  # --- Pos-processamento ---
  Pred[Pred < 0] <- 0
  datas_teste=datas[H$ts]
  Pred[datas_teste %in% datas_fecho] <- 0
  
  MAE_v[b]=mmetric(y=Y,x=Pred,metric="MAE")
  NMAE_v[b]=mmetric(y=Y,x=Pred,metric="NMAE",val=YR)
  RMSE_v[b]=mmetric(y=Y,x=Pred,metric="RMSE")
  RRSE_v[b]=mmetric(y=Y,x=Pred,metric="RRSE")
  R2_v[b]=1-(sum((Y-Pred)^2)/sum((Y-mean(Y))^2))
  
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
    RRSE=round(RRSE_v[b],4),
    R2=round(R2_v[b],4)
  )
  
  res_richmond=rbind(res_richmond,linha)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "MAE:",round(MAE_v[b],4),
      "NMAE:",round(NMAE_v[b],4),
      "RMSE:",round(RMSE_v[b],4),
      "RRSE:",round(RRSE_v[b],4),
      "R2:",round(R2_v[b],4),"\n")
  
  mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),
         leg=list(pos="topleft",leg=c("target","ARIMA pred.")))
  mpause()
}

# --- Boxplot das metricas ---
boxplot(NMAE_v,RRSE_v,R2_v,
        names=c("NMAE","RRSE","R2"),
        main="Richmond - ARIMA - Distribuicao das Metricas (12 iteracoes)",
        col="hotpink")

med_richmond=data.frame(
  Loja="Richmond",
  Target="Num_Customers",
  Metodo="Growing Window",
  Iteracoes=Runs,
  Horizonte=Test,
  Incremento=S,
  Media_MAE=round(mean(MAE_v),4),
  Media_NMAE=round(mean(NMAE_v),4),
  Media_RMSE=round(mean(RMSE_v),4),
  Media_RRSE=round(mean(RRSE_v),4),
  Media_R2=round(mean(R2_v),4)
)

cat("mean values for Richmond:\n")
cat("MAE mean:",mean(MAE_v),"\n")
cat("NMAE mean:",mean(NMAE_v),"\n")
cat("RMSE mean:",mean(RMSE_v),"\n")
cat("RRSE mean:",mean(RRSE_v),"\n")
cat("R2 mean:",mean(R2_v),"\n")

# ------------------------------------------------------------
# 7. JUNTAR RESULTADOS
# ------------------------------------------------------------
resultados_iteracoes=rbind(
  res_baltimore,
  res_lancaster,
  res_philadelphia,
  res_richmond
)

resultados_media=rbind(
  med_baltimore,
  med_lancaster,
  med_philadelphia,
  med_richmond
)

cat("\n============================================================\n")
cat("RESULTADOS FINAIS - MEDIA DAS METRICAS POR LOJA\n")
cat("============================================================\n")
print(resultados_media)

# ------------------------------------------------------------
# 8. GUARDAR CSV
# ------------------------------------------------------------
write.csv(resultados_iteracoes,
          "backtesting_arima_iteracoes.csv",
          row.names=FALSE)

write.csv(resultados_media,
          "backtesting_arima_media_lojas.csv",
          row.names=FALSE)