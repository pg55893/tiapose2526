# Previsao: ARIMAX Multivariado Cenario 2
# Otimizacao: PSO - O1 (maximizar lucro total)


library(pso)
library(forecast)
library(rminer)

# -----------------------------------------------------------------------------
# carregar dados e config
# -----------------------------------------------------------------------------
setwd("~/TIAPOSE_projeto/tiapose2526/Files/csv")
source("~/TIAPOSE_projeto/tiapose2526/Files/tratamentoDeDados.R")
source("~/TIAPOSE_projeto/tiapose2526/Files/otimizacao/config_otimizacao.R")

output_dir="~/TIAPOSE_projeto/tiapose2526/Files/otimizacao/PSO/resultados_O1"
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

# extrair previsao ARIMAX para os proximos H dias
arimax_prev=function(dados,H=7,lags_sales=c(1,7))
{ d=build_df(dados)
N=nrow(d)
tr=1:N
te=(N-H+1):N # proxy das exogenas futuras

# modelo Sales
fit_s=auto.arima(ts(d$Sales[tr],frequency=7),xreg=as.matrix(d[tr,c("TouristEvent","Num_Employees","Pct_On_Sale")]))
pred_s=pmax(as.numeric(forecast(fit_s,h=H,xreg=as.matrix(d[te,c("TouristEvent","Num_Employees","Pct_On_Sale")]))$mean),0)

# modelo Customers com lags de Sales
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

PREV=round(PREV_carolina)  # substituir PREV da config
upper=calc_upper(PREV)     # recalcular bounds

cat("\n=== PREV ARIMAX ===\n")
cat("Baltimore   :",PREV[1:7],"\n")
cat("Lancaster   :",PREV[8:14],"\n")
cat("Philadelphia:",PREV[15:21],"\n")
cat("Richmond    :",PREV[22:28],"\n\n")

# -----------------------------------------------------------------------------
# configuracao do PSO
# adaptado de: opt-3-rastrigin-2.R (P. Cortez)
# -----------------------------------------------------------------------------
D=length(lower)         # dimensao = 84
popSize=max(20,10+round(2*sqrt(D))) # tamanho do enxame
maxit=1000             # numero maximo de iteracoes
report=50               # reportar progresso a cada N iteracoes

idx_J=seq(2,D,by=3)
idx_X=seq(3,D,by=3)

# normalize solution: arredonda J e X e garante bounds
normalize=function(S)
{ S[idx_J]=round(S[idx_J])
S[idx_X]=round(S[idx_X])
return(pmin(pmax(S,lower),upper))
}

set.seed(42)
s0=lower+(upper-lower)*runif(D) # ponto de partida

# -----------------------------------------------------------------------------
# funcao de avaliacao monitorizada
# adaptado de: opt-4-convergence-2demos.R (P. Cortez)
# global variables: EV, BEST, F
# -----------------------------------------------------------------------------
g_best=function(val1,val2,type="min")
{ if(type=="min") return(min(c(val1,val2)))
  else return(max(c(val1,val2)))
}

m_eval=function(S)
{ S=normalize(S)
res=profit(S)
# global assignment: <<-
EV<<-EV+1
BEST<<-g_best(BEST,res,TYPE)
if(EV<=MAXIT) F[EV]<<-BEST
return(-res) # PSO minimiza, lucro e positivo -> negamos
}

# -----------------------------------------------------------------------------
# PSO - O1: maximizar lucro sem restricoes
# adaptado de: opt-3-rastrigin-2.R (P. Cortez)
# -----------------------------------------------------------------------------
cat("=== PSO - O1: maximizar lucro ===\n")
cat("D=",D,"| popSize=",popSize,"| maxit=",maxit,"\n\n")

# variaveis globais para a funcao monitorizada:
TYPE="max"
EV=0
BEST=0      # inicial: pior lucro possivel
MAXIT=maxit*popSize # maximo de avaliacoes
F=rep(NA,MAXIT)

t0=proc.time()

ps=psoptim(par=s0,fn=m_eval,lower=lower,upper=upper,
           control=list(trace=1,REPORT=report,maxit=maxit,s=popSize,
                        w=0.729,c.p=1.494,c.g=1.494,vectorize=FALSE))

tempo=(proc.time()-t0)["elapsed"]

# get the best solution:
S_best=normalize(ps$par)
lucro=profit(S_best)
unidades=total_units(S_best)
total_HR=sum(S_best[idx_J])+sum(S_best[idx_X])

cat("\n=== RESULTADO O1 ===\n")
cat("Lucro total      :",round(lucro),"$\n")
cat("Unidades vendidas:",round(unidades),"\n")
cat("Total HR         :",total_HR,"\n")
cat("Tempo execucao   :",round(tempo,1),"s\n")
cat("Avaliacoes       :",ps$counts["function"],"\n")

# -----------------------------------------------------------------------------
# grafico de convergencia em PDF
# adaptado de: opt-4-convergence-2demos.R (P. Cortez)
# -----------------------------------------------------------------------------
pdf(file.path(output_dir,"convergencia_PSO_O1.pdf"),width=10,height=6)
F_clean=F[!is.na(F)]
plot(F_clean,col="blue",type="l",lwd=2,
     main=paste("Convergência do PSO O1 (D=",D,")"),
     xlab="Avaliações",ylab="Lucro ($)")
abline(h=lucro,col="red",lty=2)
legend("bottomright",bty="n",
       legend=c("Melhor lucro","Lucro final"),
       col=c("blue","red"),lty=c(1,2),lwd=2)
dev.off()
cat("\nPDF guardado:",file.path(output_dir,"convergencia_PSO_O1.pdf"),"\n")

# -----------------------------------------------------------------------------
# tabela resumo - CSV
# -----------------------------------------------------------------------------
tabela_resumo=data.frame(
  Objetivo="O1",Metodo="PSO",Previsao="ARIMAX",
  Lucro_Total=round(lucro,2),
  Unidades_Vendidas=round(unidades,2),
  Total_HR=total_HR,
  Tempo_Segundos=round(tempo,2),
  Avaliacoes=as.numeric(ps$counts["function"])
)
write.csv(tabela_resumo,file=file.path(output_dir,"tabela_resumo_PSO_O1.csv"),row.names=FALSE)
cat("CSV guardado:",file.path(output_dir,"tabela_resumo_PSO_O1.csv"),"\n")

# historico de convergencia - CSV
write.csv(data.frame(Avaliacao=1:length(F_clean),Lucro=round(F_clean,2)),
          file=file.path(output_dir,"convergencia_PSO_O1.csv"),row.names=FALSE)
cat("CSV guardado:",file.path(output_dir,"convergencia_PSO_O1.csv"),"\n")

# -----------------------------------------------------------------------------
# plano semanal 
# -----------------------------------------------------------------------------
nomes_lojas=c("Baltimore","Lancaster","Philadelphia","Richmond")
dias=c("Dom","Seg","Ter","Qua","Qui","Sex","Sab")
plano=data.frame()

cat("\n=== PLANO SEMANAL OTIMO - O1 ===\n")
for(s in 1:4)
{ cat("\nLoja:",nomes_lojas[s],"\n")
  cat(sprintf("%-5s %5s %4s %4s %8s\n","Dia","PR","J","X","Lucro_d"))
  for(d in 1:7)
  { idx=(s-1)*21+(d-1)*3+1
  PR=S_best[idx]; J=S_best[idx+1]; X=S_best[idx+2]
  loja=lojas[[s]]; C=PREV[(s-1)*7+d]
  As=min(7*X+6*J,C); n_X=min(7*X,As); n_J=As-n_X
  soma_P=0
  if(n_X>0) soma_P=soma_P+n_X*round(round(loja$Fx*10/log(2-PR))*(1-PR)*1.07)
  if(n_J>0) soma_P=soma_P+n_J*round(round(loja$Fj*10/log(2-PR))*(1-PR)*1.07)
  tipo_dia=ifelse(IS_WEEKDAY[d],"weekday","weekend")
  lucro_d=soma_P-J*hr_cost$J[tipo_dia]-X*hr_cost$X[tipo_dia]
  cat(sprintf("%-5s %5.2f %4d %4d %8.0f\n",dias[d],PR,J,X,lucro_d))
  plano=rbind(plano,data.frame(Loja=nomes_lojas[s],Dia=dias[d],
                               PREV_Clientes=C,PR=round(PR,4),J=J,X=X,
                               Clientes_Assistidos=As,Lucro_Diario=round(lucro_d,2)))
  }
}
write.csv(plano,file=file.path(output_dir,"plano_semanal_PSO_O1.csv"),row.names=FALSE)
cat("\nCSV guardado:",file.path(output_dir,"plano_semanal_PSO_O1.csv"),"\n")

cat("\n=== CONCLUIDO ===\n")
cat("Ficheiros em:",output_dir,"\n")
cat(" - convergencia_PSO_O1.pdf\n")
cat(" - tabela_resumo_PSO_O1.csv\n")
cat(" - convergencia_PSO_O1.csv\n")
cat(" - plano_semanal_PSO_O1.csv\n")