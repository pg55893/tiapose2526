# Autora: Carolina
# Previsao: ARIMAX Multivariado Cenario 2
# Otimizacao: Differential Evolution - O1 e O2
# Multi-run: 20 runs, mediana do profit
# Adaptado de: P. Cortez, Modern Optimization with R, 2021, Springer.
# Referencia demo: opt-3-rastrigin-2.R (P. Cortez)

# you need to install these packages:
# install.packages("DEoptim")
# install.packages("forecast")
# install.packages("rminer")
library(DEoptim)
library(forecast)
library(rminer)

# -----------------------------------------------------------------------------
# carregar dados e config
# -----------------------------------------------------------------------------
setwd("~/TIAPOSE_projeto/tiapose2526/data")
source("~/TIAPOSE_projeto/tiapose2526/utils/tratamentoDeDados.R")
source("~/TIAPOSE_projeto/tiapose2526/utils/config_otimizacao.R")

output_dir_O1="~/TIAPOSE_projeto/tiapose2526/otimizacao/DE/O1"
output_dir_O2="~/TIAPOSE_projeto/tiapose2526/otimizacao/DE/O2"
dir.create(output_dir_O1,showWarnings=FALSE,recursive=TRUE)
dir.create(output_dir_O2,showWarnings=FALSE,recursive=TRUE)

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
# configuracao do DE
# adaptado de: opt-3-rastrigin-2.R (P. Cortez)
# de=DEoptim(fn=rastrigin,lower=lower,upper=upper,
#            DEoptim.control(NP=popSize,itermax=iter,trace=report))
# -----------------------------------------------------------------------------
D=length(lower)
popSize=max(20,10+round(2*sqrt(D)))  # mesmo criterio do PSO
maxit=500                             # igual ao PSO para comparacao justa
report=50
RUNS=20
LIMIT_UNITS=10000

idx_J=seq(2,D,by=3)
idx_X=seq(3,D,by=3)
idx_PR=seq(1,D,by=3)

# funcao auxiliar de convergencia
# adaptado de: opt-4-convergence-2demos.R (P. Cortez)
g_best=function(val1,val2,type="max")
{ if(type=="max") return(max(c(val1,val2)))
  else return(min(c(val1,val2)))
}

normalize=function(S)
{ S[idx_J]=round(S[idx_J])
S[idx_X]=round(S[idx_X])
return(pmin(pmax(S,lower),upper))
}

# repair com binary search — mesmo do PSO O2
repair=function(S)
{ S=normalize(S)
u=total_units(S)
if(is.na(u)||u<=LIMIT_UNITS) return(S)
S_orig=S; lo=0; hi=1
for(it in 1:25)
{ mid=(lo+hi)/2
S_try=S_orig
S_try[idx_J]=round(S_orig[idx_J]*mid)
S_try[idx_X]=round(S_orig[idx_X]*mid)
S_try=normalize(S_try)
u_try=total_units(S_try)
if(is.na(u_try)||u_try<=LIMIT_UNITS) lo=mid
else hi=mid
}
S_out=S_orig
S_out[idx_J]=round(S_orig[idx_J]*lo)
S_out[idx_X]=round(S_orig[idx_X]*lo)
S_out=normalize(S_out)
u_out=total_units(S_out)
if(!is.na(u_out)&&u_out>LIMIT_UNITS){ S_out[idx_J]=0; S_out[idx_X]=0; S_out=normalize(S_out) }
return(S_out)
}

# -----------------------------------------------------------------------------
# funcoes auxiliares de grafico — mesmo estilo do PSO O2
# adaptado de: opt-4-convergence-2demos.R (P. Cortez)
# -----------------------------------------------------------------------------
build_mat=function(hists)
{ valid=Filter(function(h) !is.null(h)&&length(h)>0,hists)
if(length(valid)==0) return(NULL)
max_len=max(sapply(valid,length))
sapply(valid,function(h) c(h,rep(h[length(h)],max_len-length(h))))
}

plot_runs=function(F_mat,titulo,pdf_out,csv_out,cor_med="steelblue")
{ 
  mat=build_mat(F_mat)
  if(is.null(mat)){ cat("Sem dados para grafico:",titulo,"\n"); return(NULL) }
  
  med_curve=apply(mat,1,median,na.rm=TRUE)
  x_fes=seq_len(nrow(mat))
  
  vals=mat[is.finite(mat)]
  
  if(grepl("O2 - Death Penalty",titulo))
    y_rng=c(-5000,max(vals,na.rm=TRUE))
  else
    y_rng=range(vals,na.rm=TRUE)
  
  pdf(pdf_out,width=9,height=6)
  
  for(j in seq_len(ncol(mat)))
  { 
    if(j==1) plot(x_fes,mat[,j],type="l",col="grey80",lwd=1,ylim=y_rng,
                  xlab="Numero de Avaliacoes (FES)",ylab="Melhor Lucro",main=titulo)
    else lines(x_fes,mat[,j],col="grey80",lwd=1)
  }
  
  lines(x_fes,med_curve,col=cor_med,lwd=2.5)
  
  legend("bottomright",bty="n",
         legend=c("Runs individuais","Mediana"),
         col=c("grey70",cor_med),lwd=c(1,2.5))
  
  dev.off()
  
  write.csv(data.frame(Avaliacao=x_fes,Lucro_Mediana=round(med_curve,2)),
            file=csv_out,row.names=FALSE)
  
  cat("PDF guardado:",pdf_out,"\n")
  invisible(med_curve)
}

plot_dp_rep=function(F_death,F_repair,med_death,med_repair,pdf_out)
{ 
  mat_dp=build_mat(F_death)
  mat_rep=build_mat(F_repair)
  
  y_all=c(if(!is.null(mat_dp)) mat_dp[is.finite(mat_dp)],
          if(!is.null(mat_rep)) mat_rep[is.finite(mat_rep)])
  
  if(length(y_all)==0){ cat("Sem dados comparativo.\n"); return() }
  
  y_rng=c(-5000,max(y_all,na.rm=TRUE))
  
  pdf(pdf_out,width=10,height=6)
  
  if(!is.null(mat_dp))
  { 
    curve_dp=apply(mat_dp,1,median,na.rm=TRUE)
    
    for(j in seq_len(ncol(mat_dp)))
    { 
      if(j==1) plot(seq_len(nrow(mat_dp)),mat_dp[,j],type="l",col="#FFAAAA",lwd=1,
                    xlab="Numero de Avaliacoes (FES)",ylab="Melhor Lucro",
                    main="DE O2: Death Penalty vs Repair (runs + mediana)",ylim=y_rng)
      else lines(seq_len(nrow(mat_dp)),mat_dp[,j],col="#FFAAAA",lwd=1)
    }
    
    lines(seq_along(curve_dp),curve_dp,col="firebrick",lwd=2.5)
  } else plot.new()
  
  if(!is.null(mat_rep))
  { 
    curve_rep=apply(mat_rep,1,median,na.rm=TRUE)
    
    for(j in seq_len(ncol(mat_rep)))
      lines(seq_len(nrow(mat_rep)),mat_rep[,j],col="#AADDAA",lwd=1)
    
    lines(seq_along(curve_rep),curve_rep,col="forestgreen",lwd=2.5)
  }
  
  legend("bottomright",bty="n",
         legend=c(sprintf("DP runs (med=%.0f)",med_death),
                  sprintf("Repair runs (med=%.0f)",med_repair),
                  "Mediana DP","Mediana Repair"),
         col=c("#FFAAAA","#AADDAA","firebrick","forestgreen"),
         lwd=c(1,1,2.5,2.5))
  
  dev.off()
  cat("PDF guardado:",pdf_out,"\n")
}

# =============================================================================
# DE - O1: maximizar lucro sem restricoes
# adaptado de: opt-3-rastrigin-2.R (P. Cortez)
# de=DEoptim(fn=rastrigin,lower=lower,upper=upper,
#            DEoptim.control(NP=popSize,itermax=iter,trace=report))
# =============================================================================
cat("=== DE - O1: maximizar lucro (",RUNS,"runs) ===\n")
cat("D=",D,"| popSize=",popSize,"| maxit=",maxit,"| runs=",RUNS,"\n\n")

MAXIT_O1=maxit*popSize
lucros_O1=numeric(RUNS)
F_mat_O1=vector("list",RUNS)
S_best_O1=NULL; lucro_best_O1=-Inf

t0_O1=proc.time()

for(run in 1:RUNS)
{ cat("run",run,"/",RUNS,"...\n")
  
  # variaveis globais para monitorizacao
  # adaptado de: opt-4-convergence-2demos.R (P. Cortez)
  TYPE="max"; EV=0; BEST=0; FHIST=rep(NA,MAXIT_O1)
  
  m_eval_O1=function(S)
  { S=normalize(S)
  res=profit(S)
  EV<<-EV+1
  BEST<<-g_best(BEST,res,TYPE)
  if(EV<=MAXIT_O1) FHIST[EV]<<-BEST
  return(-res)  # DEoptim minimiza — negamos o lucro
  }
  
  set.seed(run)
  # adaptado de: opt-3-rastrigin-2.R (P. Cortez)
  de=DEoptim(fn=m_eval_O1,lower=lower,upper=upper,
             DEoptim.control(NP=popSize,itermax=maxit,trace=FALSE))
  
  # get the best solution (DEoptim pattern do stor):
  S_run=normalize(de$optim$bestmem)
  lucro_run=profit(S_run)
  lucros_O1[run]=lucro_run
  F_mat_O1[[run]]=FHIST
  
  if(lucro_run>lucro_best_O1){ lucro_best_O1=lucro_run; S_best_O1=S_run }
  cat("  lucro run",run,":",round(lucro_run),"$\n")
}

tempo_O1=(proc.time()-t0_O1)["elapsed"]
med_O1=median(lucros_O1)

cat("\n=== RESULTADO DE O1 ===\n")
cat("Mediana do lucro :",round(med_O1),"$\n")
cat("Melhor lucro     :",round(lucro_best_O1),"$\n")
cat("Unidades (melhor):",round(total_units(S_best_O1)),"\n")
cat("Total HR (melhor):",sum(S_best_O1[idx_J])+sum(S_best_O1[idx_X]),"\n")
cat("Tempo total      :",round(tempo_O1,1),"s\n")

# graficos e CSV O1
plot_runs(F_mat_O1,
          titulo=paste0("DE O1 - ",RUNS," runs"),
          pdf_out=file.path(output_dir_O1,"convergencia_DE_O1.pdf"),
          csv_out=file.path(output_dir_O1,"convergencia_DE_O1.csv"),
          cor_med="darkorange")

pdf(file.path(output_dir_O1,"boxplot_DE_O1.pdf"),width=7,height=6)
boxplot(lucros_O1,col="darkorange",
        main=paste("DE O1 - Distribuicao lucros (",RUNS,"runs)"),ylab="Lucro ($)")
abline(h=med_O1,col="red",lty=2,lwd=2)
legend("bottomright",bty="n",legend=paste0("Mediana: $",round(med_O1)),col="red",lty=2,lwd=2)
dev.off()
cat("PDF guardado: boxplot_DE_O1.pdf\n")

write.csv(data.frame(
  Objetivo="O1",Metodo="DE",Previsao="ARIMAX",Runs=RUNS,
  Lucro_Mediana=round(med_O1,2),Lucro_Max=round(max(lucros_O1),2),
  Lucro_Min=round(min(lucros_O1),2),
  Unidades_Best=round(total_units(S_best_O1),2),
  Total_HR_Best=sum(S_best_O1[idx_J])+sum(S_best_O1[idx_X]),
  Tempo_Total_s=round(tempo_O1,2)),
  file=file.path(output_dir_O1,"tabela_resumo_DE_O1.csv"),row.names=FALSE)
cat("CSV guardado: tabela_resumo_DE_O1.csv\n")

write.csv(data.frame(Run=1:RUNS,Lucro=round(lucros_O1,2)),
          file=file.path(output_dir_O1,"lucros_runs_DE_O1.csv"),row.names=FALSE)
cat("CSV guardado: lucros_runs_DE_O1.csv\n")

# =============================================================================
# DE - O2: Death Penalty e Repair
# =============================================================================
cat("\n=== DE - O2: Death Penalty e Repair (",RUNS,"runs cada) ===\n")

MAXIT_O2=maxit*popSize

# --- Death Penalty ---
cat("\n--- Death Penalty ---\n")
lucros_dp=numeric(RUNS); F_mat_dp=vector("list",RUNS)
S_best_dp=NULL; lucro_best_dp=-Inf

t0_O2=proc.time()

for(run in 1:RUNS)
{ cat("run",run,"/",RUNS,"...\n")
  TYPE="max"; EV=0; BEST=-Inf; FHIST=rep(NA,MAXIT_O2)
  
  eval_death=function(S)
  { S=normalize(S)
  EV<<-EV+1
  u=total_units(S)
  if(!is.na(u)&&u>LIMIT_UNITS)
  { excess=(u-LIMIT_UNITS)/LIMIT_UNITS
  res=profit(S)-1e5*excess
  if(EV<=MAXIT_O2) FHIST[EV]<<-ifelse(is.finite(BEST),BEST,res)
  return(-res)
  }
  res=profit(S)
  BEST<<-g_best(BEST,res,TYPE)
  if(EV<=MAXIT_O2) FHIST[EV]<<-BEST
  return(-res)
  }
  
  set.seed(run)
  de=DEoptim(fn=eval_death,lower=lower,upper=upper,
             DEoptim.control(NP=popSize,itermax=maxit,trace=FALSE))
  
  S_run=normalize(de$optim$bestmem)
  if(total_units(S_run)>LIMIT_UNITS) S_run=repair(S_run)
  lucro_run=profit(S_run)
  lucros_dp[run]=lucro_run
  F_mat_dp[[run]]=FHIST
  if(lucro_run>lucro_best_dp){ lucro_best_dp=lucro_run; S_best_dp=S_run }
  cat("  lucro:",round(lucro_run),"$ | unidades:",round(total_units(S_run)),"\n")
}

# --- Repair ---
cat("\n--- Repair ---\n")
lucros_rep=numeric(RUNS); F_mat_rep=vector("list",RUNS)
S_best_rep=NULL; lucro_best_rep=-Inf

for(run in 1:RUNS)
{ cat("run",run,"/",RUNS,"...\n")
  TYPE="max"; EV=0; BEST=-Inf; FHIST=rep(NA,MAXIT_O2)
  
  eval_repair=function(S)
  { S=repair(S)
  EV<<-EV+1
  res=profit(S)
  BEST<<-g_best(BEST,res,TYPE)
  if(EV<=MAXIT_O2) FHIST[EV]<<-BEST
  return(-res)
  }
  
  set.seed(run)
  de=DEoptim(fn=eval_repair,lower=lower,upper=upper,
             DEoptim.control(NP=popSize,itermax=maxit,trace=FALSE))
  
  S_run=repair(de$optim$bestmem)
  lucro_run=profit(S_run)
  lucros_rep[run]=lucro_run
  F_mat_rep[[run]]=FHIST
  if(lucro_run>lucro_best_rep){ lucro_best_rep=lucro_run; S_best_rep=S_run }
  cat("  lucro:",round(lucro_run),"$ | unidades:",round(total_units(S_run)),"\n")
}

tempo_O2=(proc.time()-t0_O2)["elapsed"]
med_dp=median(lucros_dp); med_rep=median(lucros_rep)

cat("\n=== RESULTADOS DE O2 ===\n")
cat(sprintf("%-20s %10s %10s %10s\n","Metodo","Mediana","Max","Min"))
cat(sprintf("%-20s %10.0f %10.0f %10.0f\n","DE DeathPenalty",med_dp,max(lucros_dp),min(lucros_dp)))
cat(sprintf("%-20s %10.0f %10.0f %10.0f\n","DE Repair",med_rep,max(lucros_rep),min(lucros_rep)))
cat("Tempo total O2:",round(tempo_O2,1),"s\n")

# graficos O2
plot_runs(F_mat_dp,
          titulo=paste0("DE O2 - Death Penalty - ",RUNS," runs"),
          pdf_out=file.path(output_dir_O2,"convergencia_DE_O2_DeathPenalty.pdf"),
          csv_out=file.path(output_dir_O2,"convergencia_DE_O2_DeathPenalty.csv"),
          cor_med="steelblue")

plot_runs(F_mat_rep,
          titulo=paste0("DE O2 - Repair - ",RUNS," runs"),
          pdf_out=file.path(output_dir_O2,"convergencia_DE_O2_Repair.pdf"),
          csv_out=file.path(output_dir_O2,"convergencia_DE_O2_Repair.csv"),
          cor_med="darkgreen")

plot_dp_rep(F_mat_dp,F_mat_rep,med_dp,med_rep,
            pdf_out=file.path(output_dir_O2,"comparacao_DE_O2.pdf"))

pdf(file.path(output_dir_O2,"boxplot_DE_O2.pdf"),width=8,height=6)
boxplot(list(DeathPenalty=lucros_dp,Repair=lucros_rep),
        col=c("steelblue","darkgreen"),
        main=paste("DE O2 - Distribuicao lucros (",RUNS,"runs)"),ylab="Lucro ($)")
abline(h=med_dp,col="steelblue",lty=2,lwd=1.5)
abline(h=med_rep,col="darkgreen",lty=2,lwd=1.5)
dev.off()
cat("PDF guardado: boxplot_DE_O2.pdf\n")

# CSV O2
write.csv(data.frame(
  Objetivo=c("O2","O2"),Metodo=c("DE_DeathPenalty","DE_Repair"),
  Previsao=c("ARIMAX","ARIMAX"),Runs=c(RUNS,RUNS),
  Lucro_Mediana=round(c(med_dp,med_rep),2),
  Lucro_Max=round(c(max(lucros_dp),max(lucros_rep)),2),
  Lucro_Min=round(c(min(lucros_dp),min(lucros_rep)),2),
  Unidades_Best=round(c(total_units(S_best_dp),total_units(S_best_rep)),2),
  Tempo_Total_s=rep(round(tempo_O2,2),2)),
  file=file.path(output_dir_O2,"tabela_resumo_DE_O2.csv"),row.names=FALSE)
cat("CSV guardado: tabela_resumo_DE_O2.csv\n")

write.csv(data.frame(Run=1:RUNS,DeathPenalty=round(lucros_dp,2),Repair=round(lucros_rep,2)),
          file=file.path(output_dir_O2,"lucros_runs_DE_O2.csv"),row.names=FALSE)
cat("CSV guardado: lucros_runs_DE_O2.csv\n")

cat("\n=== CONCLUIDO ===\n")
cat("Ficheiros O1 em:",output_dir_O1,"\n")
cat(" - convergencia_DE_O1.pdf\n - boxplot_DE_O1.pdf\n")
cat(" - tabela_resumo_DE_O1.csv\n - lucros_runs_DE_O1.csv\n - convergencia_DE_O1.csv\n")
cat("Ficheiros O2 em:",output_dir_O2,"\n")
cat(" - convergencia_DE_O2_DeathPenalty.pdf\n - convergencia_DE_O2_Repair.pdf\n")
cat(" - comparacao_DE_O2.pdf\n - boxplot_DE_O2.pdf\n")
cat(" - tabela_resumo_DE_O2.csv\n - lucros_runs_DE_O2.csv\n")