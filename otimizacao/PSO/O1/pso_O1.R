# Autora: Carolina
# Previsao: ARIMAX Multivariado Cenario 2
# Otimizacao: PSO - O1 (maximizar lucro total)
# Multi-run: 20 runs, mediana do profit
# Adaptado de: P. Cortez, Modern Optimization with R, 2021, Springer.

# you need to install these packages:
# install.packages("pso")
# install.packages("forecast")
# install.packages("rminer")
library(pso)
library(forecast)
library(rminer)

# -----------------------------------------------------------------------------
# carregar dados e config
# -----------------------------------------------------------------------------
setwd("~/TIAPOSE_projeto/tiapose2526/data")
source("~/TIAPOSE_projeto/tiapose2526/utils/tratamentoDeDados.R")
source("~/TIAPOSE_projeto/tiapose2526/utils/config_otimizacao.R")

output_dir="~/TIAPOSE_projeto/tiapose2526/otimizacao/PSO/O1"
dir.create(output_dir,showWarnings=FALSE,recursive=TRUE)

# -----------------------------------------------------------------------------
# funcoes auxiliares do ARIMAX
# -----------------------------------------------------------------------------
to_numeric_safe=function(x)
{ if(is.numeric(x)) return(x)
  if(is.logical(x)) return(as.numeric(x))
  if(is.factor(x))  return(as.numeric(as.character(x)))
  return(as.numeric(x))
}

build_df=function(dados)
{ d=dados[,c("Date","Num_Customers","Sales","TouristEvent","Num_Employees","Pct_On_Sale")]
d$Date=as.Date(d$Date)
d=d[order(d$Date),]
rownames(d)=NULL
if(is.character(d$TouristEvent)||is.factor(d$TouristEvent))
  d$TouristEvent=as.numeric(d$TouristEvent=="Yes")
else
  d$TouristEvent=to_numeric_safe(d$TouristEvent)
d$Num_Customers=to_numeric_safe(d$Num_Customers)
d$Sales=to_numeric_safe(d$Sales)
d$Num_Employees=to_numeric_safe(d$Num_Employees)
d$Pct_On_Sale=to_numeric_safe(d$Pct_On_Sale)
return(d)
}

arimax_prev=function(dados,H=7,lags_sales=c(1,7))
{ d=build_df(dados)
N=nrow(d)
tr=1:N
te=(N-H+1):N
fit_s=auto.arima(ts(d$Sales[tr],frequency=7),xreg=as.matrix(d[tr,c("TouristEvent","Num_Employees","Pct_On_Sale")]))
pred_s=pmax(as.numeric(forecast(fit_s,h=H,xreg=as.matrix(d[te,c("TouristEvent","Num_Employees","Pct_On_Sale")]))$mean),0)
vi=tr[tr>max(lags_sales)]
xtr=data.frame(TouristEvent=d$TouristEvent[vi],Num_Employees=d$Num_Employees[vi],Pct_On_Sale=d$Pct_On_Sale[vi])
for(lag in lags_sales) xtr[[paste0("Sales_lag",lag)]]=d$Sales[vi-lag]
fit_c=auto.arima(ts(d$Num_Customers[vi],frequency=7),xreg=as.matrix(xtr))
sales_ext=c(d$Sales,pred_s)
cnames=c("TouristEvent","Num_Employees","Pct_On_Sale",paste0("Sales_lag",lags_sales))
xte=matrix(NA,nrow=H,ncol=length(cnames),dimnames=list(NULL,cnames))
for(step in 1:H)
{ i=te[step]
xte[step,"TouristEvent"]=d$TouristEvent[i]
xte[step,"Num_Employees"]=d$Num_Employees[i]
xte[step,"Pct_On_Sale"]=d$Pct_On_Sale[i]
for(lag in lags_sales) xte[step,paste0("Sales_lag",lag)]=sales_ext[(N+step)-lag]
}
pred_c=pmax(as.numeric(forecast(fit_c,h=H,xreg=xte)$mean),0)
return(pred_c)
}

# -----------------------------------------------------------------------------
# extrair PREV do ARIMAX para as 4 lojas
# -----------------------------------------------------------------------------
H=7; LAGS_SALES=c(1,7)
stores=list(Baltimore=baltimore,Lancaster=lancaster,Philadelphia=philadelphia,Richmond=richmond)

cat("=== a extrair PREV do ARIMAX ===\n")
PREV_carolina=c()
for(nome in names(stores))
{ cat(" -",nome,"...\n")
  PREV_carolina=c(PREV_carolina,arimax_prev(stores[[nome]],H=H,lags_sales=LAGS_SALES))
}

PREV=round(PREV_carolina)
upper=calc_upper(PREV)

cat("\n=== PREV ARIMAX ===\n")
cat("Baltimore   :",PREV[1:7],"\n")
cat("Lancaster   :",PREV[8:14],"\n")
cat("Philadelphia:",PREV[15:21],"\n")
cat("Richmond    :",PREV[22:28],"\n\n")

# -----------------------------------------------------------------------------
# configuracao do PSO
# adaptado de: opt-3-rastrigin-2.R (P. Cortez)
# -----------------------------------------------------------------------------
D=length(lower)
popSize=max(20,10+round(2*sqrt(D)))
maxit=500
report=50
RUNS=20  # numero de runs independentes

idx_J=seq(2,D,by=3)
idx_X=seq(3,D,by=3)

normalize=function(S)
{ S[idx_J]=round(S[idx_J])
S[idx_X]=round(S[idx_X])
return(pmin(pmax(S,lower),upper))
}

# funcao auxiliar de convergencia
# adaptado de: opt-4-convergence-2demos.R (P. Cortez)
g_best=function(val1,val2,type="min")
{ if(type=="min") return(min(c(val1,val2)))
  else return(max(c(val1,val2)))
}

# -----------------------------------------------------------------------------
# PSO - O1: 20 runs, mediana do profit
# adaptado de: opt-3-rastrigin-2.R (P. Cortez)
# -----------------------------------------------------------------------------
cat("=== PSO - O1: maximizar lucro (",RUNS,"runs) ===\n")
cat("D=",D,"| popSize=",popSize,"| maxit=",maxit,"| runs=",RUNS,"\n\n")

MAXIT=maxit*popSize  # maximo de avaliacoes por run

lucros_runs=numeric(RUNS)      # lucro final de cada run
F_runs=matrix(NA,nrow=RUNS,ncol=MAXIT) # convergencia de cada run
S_best_global=NULL             # melhor solucao global
lucro_best_global=-Inf         # melhor lucro global

t0_total=proc.time()

for(run in 1:RUNS)
{ cat("run",run,"/",RUNS,"...\n")
  
  # variaveis globais para a funcao monitorizada:
  TYPE="max"
  EV=0
  BEST=0
  F=rep(NA,MAXIT)
  
  m_eval=function(S)
  { S=normalize(S)
  res=profit(S)
  EV<<-EV+1
  BEST<<-g_best(BEST,res,TYPE)
  if(EV<=MAXIT) F[EV]<<-BEST
  return(-res)
  }
  
  set.seed(run)  # seed diferente por run para reproducibilidade
  s0=lower+(upper-lower)*runif(D)
  
  ps=psoptim(par=s0,fn=m_eval,lower=lower,upper=upper,
             control=list(trace=0,REPORT=report,maxit=maxit,s=popSize,
                          w=0.729,c.p=1.494,c.g=1.494,vectorize=FALSE))
  
  S_run=normalize(ps$par)
  lucro_run=profit(S_run)
  lucros_runs[run]=lucro_run
  F_runs[run,]=F
  
  # atualizar melhor global
  if(lucro_run>lucro_best_global)
  { lucro_best_global=lucro_run
  S_best_global=S_run
  }
  
  cat("  lucro run",run,":",round(lucro_run),"$\n")
}

tempo_total=(proc.time()-t0_total)["elapsed"]

# --- metricas finais ---
lucro_mediana=median(lucros_runs)
lucro_media=mean(lucros_runs)
lucro_max=max(lucros_runs)
lucro_min=min(lucros_runs)

cat("\n=== RESULTADO O1 (",RUNS,"runs) ===\n")
cat("Mediana do lucro :",round(lucro_mediana),"$\n")
cat("Media do lucro   :",round(lucro_media),  "$\n")
cat("Melhor lucro     :",round(lucro_max),    "$\n")
cat("Pior lucro       :",round(lucro_min),    "$\n")
cat("Tempo total      :",round(tempo_total,1),"s\n")

# melhor solucao global
unidades_best=total_units(S_best_global)
total_HR_best=sum(S_best_global[idx_J])+sum(S_best_global[idx_X])
cat("\nMelhor solucao global:\n")
cat("Lucro            :",round(lucro_best_global),"$\n")
cat("Unidades vendidas:",round(unidades_best),"\n")
cat("Total HR         :",total_HR_best,"\n")

# -----------------------------------------------------------------------------
# grafico 1 — curva de convergencia (mediana entre runs)
# adaptado de: opt-4-convergence-2demos.R (P. Cortez)
# eixo X = numero de avaliacoes da funcao profit()
# -----------------------------------------------------------------------------
F_mediana=apply(F_runs,2,function(col) median(col,na.rm=TRUE))
F_mediana_clean=F_mediana[!is.na(F_mediana)]
avaliacoes=1:length(F_mediana_clean)

pdf(file.path(output_dir,"convergencia_PSO_O1.pdf"),width=10,height=6)
plot(avaliacoes,F_mediana_clean,
     col="blue",type="l",lwd=2,
     main=paste("Convergencia PSO O1 (D=",D,", runs=",RUNS,")"),
     xlab="Numero de avaliacoes da funcao profit()",
     ylab="Mediana do melhor lucro acumulado ($)")
abline(h=lucro_mediana,col="red",lty=2)
legend("bottomright",bty="n",
       legend=c("Mediana entre runs",paste0("Mediana final: $",round(lucro_mediana))),
       col=c("blue","red"),lty=c(1,2),lwd=2)
dev.off()
cat("\nPDF guardado:",file.path(output_dir,"convergencia_PSO_O1.pdf"),"\n")

# -----------------------------------------------------------------------------
# grafico 2 — boxplot dos lucros dos 20 runs
# -----------------------------------------------------------------------------
pdf(file.path(output_dir,"boxplot_PSO_O1.pdf"),width=7,height=6)
boxplot(lucros_runs,
        col="steelblue",
        main=paste("PSO O1 — Distribuicao do lucro (",RUNS,"runs)"),
        ylab="Lucro ($)",
        xlab="PSO")
abline(h=lucro_mediana,col="red",lty=2,lwd=2)
legend("bottomright",bty="n",
       legend=paste0("Mediana: $",round(lucro_mediana)),
       col="red",lty=2,lwd=2)
dev.off()
cat("PDF guardado:",file.path(output_dir,"boxplot_PSO_O1.pdf"),"\n")

# -----------------------------------------------------------------------------
# tabela resumo - CSV
# -----------------------------------------------------------------------------
tabela_resumo=data.frame(
  Objetivo="O1",
  Metodo="PSO",
  Previsao="ARIMAX",
  Runs=RUNS,
  Lucro_Mediana=round(lucro_mediana,2),
  Lucro_Media=round(lucro_media,2),
  Lucro_Max=round(lucro_max,2),
  Lucro_Min=round(lucro_min,2),
  Unidades_Best=round(unidades_best,2),
  Total_HR_Best=total_HR_best,
  Tempo_Total_s=round(tempo_total,2)
)
write.csv(tabela_resumo,file=file.path(output_dir,"tabela_resumo_PSO_O1.csv"),row.names=FALSE)
cat("CSV guardado:",file.path(output_dir,"tabela_resumo_PSO_O1.csv"),"\n")

# lucros por run - CSV
write.csv(data.frame(Run=1:RUNS,Lucro=round(lucros_runs,2)),
          file=file.path(output_dir,"lucros_runs_PSO_O1.csv"),row.names=FALSE)
cat("CSV guardado:",file.path(output_dir,"lucros_runs_PSO_O1.csv"),"\n")

# convergencia mediana - CSV
write.csv(data.frame(Avaliacao=avaliacoes,Lucro_Mediana=round(F_mediana_clean,2)),
          file=file.path(output_dir,"convergencia_PSO_O1.csv"),row.names=FALSE)
cat("CSV guardado:",file.path(output_dir,"convergencia_PSO_O1.csv"),"\n")

# -----------------------------------------------------------------------------
# plano semanal da melhor solucao global - CSV
# -----------------------------------------------------------------------------
nomes_lojas=c("Baltimore","Lancaster","Philadelphia","Richmond")
dias=c("Dom","Seg","Ter","Qua","Qui","Sex","Sab")
plano=data.frame()

cat("\n=== PLANO SEMANAL OTIMO - O1 (melhor run) ===\n")
for(s in 1:4)
{ cat("\nLoja:",nomes_lojas[s],"\n")
  cat(sprintf("%-5s %5s %4s %4s %8s\n","Dia","PR","J","X","Lucro_d"))
  for(d in 1:7)
  { idx=(s-1)*21+(d-1)*3+1
  PR=S_best_global[idx]; J=S_best_global[idx+1]; X=S_best_global[idx+2]
  loja=lojas[[s]]; C=PREV[(s-1)*7+d]
  As=min(7*X+6*J,C); n_X=min(7*X,As); n_J=As-n_X
  soma_P=0
  if(n_X>0) soma_P=soma_P+n_X*round(round(loja$Fx*10/log(2-PR))*(1-PR)*1.07)
  if(n_J>0) soma_P=soma_P+n_J*round(round(loja$Fj*10/log(2-PR))*(1-PR)*1.07)
  tipo_dia=ifelse(IS_WEEKDAY[d],"weekday","weekend")
  lucro_d=soma_P-J*hr_cost$J[tipo_dia]-X*hr_cost$X[tipo_dia]
  cat(sprintf("%-5s %5.2f %4d %4d %8.0f\n",dias[d],PR,J,X,lucro_d))
  plano=rbind(plano,data.frame(
    Loja=nomes_lojas[s],Dia=dias[d],PREV_Clientes=C,
    PR=round(PR,4),J=J,X=X,
    Clientes_Assistidos=As,Lucro_Diario=round(lucro_d,2)))
  }
}
write.csv(plano,file=file.path(output_dir,"plano_semanal_PSO_O1.csv"),row.names=FALSE)
cat("\nCSV guardado:",file.path(output_dir,"plano_semanal_PSO_O1.csv"),"\n")

cat("\n=== CONCLUIDO ===\n")
cat("Ficheiros em:",output_dir,"\n")
cat(" - convergencia_PSO_O1.pdf  (curva mediana)\n")
cat(" - boxplot_PSO_O1.pdf       (distribuicao 20 runs)\n")
cat(" - tabela_resumo_PSO_O1.csv\n")
cat(" - lucros_runs_PSO_O1.csv\n")
cat(" - convergencia_PSO_O1.csv\n")
cat(" - plano_semanal_PSO_O1.csv\n")