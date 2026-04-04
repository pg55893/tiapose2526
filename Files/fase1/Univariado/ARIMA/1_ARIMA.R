# ============================================================
# PROJETO - FORECASTING UNIVARIADO COM ARIMA
# Target: Num_Customers
# Treino: todos os dados exceto os últimos 7 dias
# Teste : últimos 7 dias
# Métricas: NMAE, RRSE, R²
# ============================================================

# ------------------------------------------------------------
# 0. DEFINIR PASTA DE TRABALHO
# ------------------------------------------------------------
setwd("~/TIAPOSE2526/Files/csv")
getwd()
list.files()

# ------------------------------------------------------------
# 1. PACOTES
# ------------------------------------------------------------
library(forecast)
library(rminer)

# ------------------------------------------------------------
# 2. CARREGAR DADOS TRATADOS
# ------------------------------------------------------------
<<<<<<< HEAD
source("~/TIAPOSE_projeto/tiapose2526/Files/EDA/tratamentoDeDados.R")
=======
source("~/TIAPOSE2526/Files/tratamentoDeDados.R")

# --- Criar e ir para a pasta ARIMA para guardar resultados ---
dir.create("~/TIAPOSE2526/Files/fase1/ARIMA", showWarnings=FALSE)
setwd("~/TIAPOSE2526/Files/fase1/ARIMA")
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f

if(!exists("baltimore") || !exists("lancaster") ||
   !exists("philadelphia") || !exists("richmond")){
  stop("Os dados tratados nao foram carregados corretamente.")
}

# ------------------------------------------------------------
# 3. DATAS EM QUE A LOJA ESTA FECHADA (Natal e Pascoa)
# ------------------------------------------------------------
datas_fecho <- as.Date(c("2012-12-25","2013-12-25","2013-03-31","2014-04-20"))

# ------------------------------------------------------------
# 4. FUNCAO PARA AVALIAR ARIMA
# ------------------------------------------------------------
avaliar_arima=function(dados,nome,target="Num_Customers",H=7)
{
  cat("\n============================================================\n")
  cat("LOJA:",nome,"\n")
  cat("============================================================\n")
  
  # ordenar e preparar dados
  dados$Date=as.Date(dados$Date)
  dados=dados[order(dados$Date),]
  dados=dados[,c("Date",target)]
  dados=na.omit(dados)
  
  y=dados[,target]
  dates=dados$Date
  L=length(y)
  
  if(L<=H){
    stop(paste("A loja",nome,"nao tem observacoes suficientes para H =",H))
  }
  
  # divisao treino/teste
  LTR=L-H
  TR=y[1:LTR]
  Y=y[(LTR+1):L]
  D=dates[(LTR+1):L]
  
  # serie temporal
  TS=ts(TR,frequency=7)
  
  # modelo ARIMA
  AR=auto.arima(TS)
  
  cat("\nModelo ARIMA:\n")
  print(AR)
  
<<<<<<< HEAD
  # previsao ARIMA
  F2=forecast(AR,h=H)
  Pred=as.numeric(F2$mean[1:H])
  
  # previsao SNaive
  SN=snaive(TS,h=H)
  Pred_naive=as.numeric(SN$mean[1:H])
=======
  # previsao
  fc=forecast(AR,h=H)
  Pred=as.numeric(fc$mean[1:H])
  
  # --- Pos-processamento ---
  Pred[Pred < 0] <- 0
  Pred[D %in% datas_fecho] <- 0
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  
  # metricas
  srange=diff(range(y,na.rm=TRUE))
  if(srange==0) srange=1
  
  mae=mmetric(Y,Pred,metric="MAE")
  nmae=mmetric(Y,Pred,metric="NMAE",val=srange)
  rmse=mmetric(Y,Pred,metric="RMSE")
  rrse=mmetric(Y,Pred,metric="RRSE")
  r2=1-(sum((Y-Pred)^2)/sum((Y-mean(Y))^2))
  
  naive_mae=mmetric(Y,Pred_naive,metric="MAE")
  naive_nmae=mmetric(Y,Pred_naive,metric="NMAE",val=srange)
  naive_rmse=mmetric(Y,Pred_naive,metric="RMSE")
  naive_rrse=mmetric(Y,Pred_naive,metric="RRSE")
  
  cat("\nMetricas ARIMA:\n")
  cat("MAE =",round(mae,4),"\n")
  cat("NMAE =",round(nmae,4),"\n")
  cat("RMSE =",round(rmse,4),"\n")
  cat("RRSE =",round(rrse,4),"\n")
  cat("R2 =",round(r2,4),"\n")
  
  cat("\nMetricas SNaive:\n")
  cat("MAE =",round(naive_mae,4),"\n")
  cat("NMAE =",round(naive_nmae,4),"\n")
  cat("RMSE =",round(naive_rmse,4),"\n")
  cat("RRSE =",round(naive_rrse,4),"\n")
  
  # tabela real vs previsto
  tabela=data.frame(
    Date=D,
    Real=Y,
    Previsto_ARIMA=round(Pred,2),
    Previsto_SNaive=round(Pred_naive,2),
    Erro_Absoluto_ARIMA=round(abs(Y-Pred),2),
    Erro_Absoluto_SNaive=round(abs(Y-Pred_naive),2)
  )
  
  cat("\nTabela previsoes:\n")
  print(tabela)
  
  # No fim da função avaliar_arima, substitui o bloco do grafico por:
  
  # grafico
  pdf(paste0("arima_fase1_",tolower(nome),".pdf"), width=8, height=5)
  plot(D,Y,type="o",pch=16,lwd=2,
       xlab="Data",ylab=target,
<<<<<<< HEAD
       main=paste("ARIMA vs SNaive -",nome),
       ylim=range(c(Y,Pred,Pred_naive)))
  
  lines(D,Pred,type="o",pch=17,lwd=2,col="blue")
  lines(D,Pred_naive,type="o",pch=15,lwd=2,col="red")
  
  legend("topleft",
         legend=c("Real","ARIMA","SNaive"),
         col=c("black","blue","red"),
         lty=1,lwd=2,pch=c(16,17,15))
=======
       main=paste("ARIMA -",nome,"- Real vs Previsto"),
       ylim=range(c(Y,Pred)))
  lines(D,Pred,type="o",pch=17,lwd=2,col="blue")
  legend("topleft",
         legend=c("Real","Previsto"),
         col=c("black","blue"),
         lty=1,lwd=2,pch=c(16,17))
  dev.off()
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  
  # guardar metricas
  resultados=data.frame(
    Loja=nome,
    Modelo=paste0("ARIMA(",
                  arimaorder(AR)[1],",",
                  arimaorder(AR)[2],",",
                  arimaorder(AR)[3],")"),
    MAE=round(mae,4),
    NMAE=round(nmae,4),
    RMSE=round(rmse,4),
    RRSE=round(rrse,4),
<<<<<<< HEAD
    Naive_MAE=round(naive_mae,4),
    Naive_NMAE=round(naive_nmae,4),
    Naive_RMSE=round(naive_rmse,4),
    Naive_RRSE=round(naive_rrse,4)
=======
    R2=round(r2,4)
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  )
  
  return(list(modelo=AR,previsoes=tabela,metricas=resultados))
}

# ------------------------------------------------------------
# 5. CORRER O MODELO PARA CADA LOJA
# ------------------------------------------------------------
res1=avaliar_arima(baltimore,"Baltimore")
res2=avaliar_arima(lancaster,"Lancaster")
res3=avaliar_arima(philadelphia,"Philadelphia")
res4=avaliar_arima(richmond,"Richmond")

# ------------------------------------------------------------
# 6. JUNTAR RESULTADOS FINAIS
# ------------------------------------------------------------
resultados=rbind(
  res1$metricas,
  res2$metricas,
  res3$metricas,
  res4$metricas
)

cat("\n============================================================\n")
cat("RESULTADOS FINAIS - TODAS AS LOJAS\n")
cat("============================================================\n")
print(resultados)

# ------------------------------------------------------------
# 7. GUARDAR RESULTADOS EM CSV
# ------------------------------------------------------------
write.csv(resultados,"resultados_arima_lojas.csv",row.names=FALSE)
write.csv(res1$previsoes,"arima_previsoes_baltimore.csv",row.names=FALSE)
write.csv(res2$previsoes,"arima_previsoes_lancaster.csv",row.names=FALSE)
write.csv(res3$previsoes,"arima_previsoes_philadelphia.csv",row.names=FALSE)
write.csv(res4$previsoes,"arima_previsoes_richmond.csv",row.names=FALSE)