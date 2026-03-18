# ============================================================
# EDA GENERICA - Projecto DSS USA Stores (TADA)
# Prof. Paulo Cortez - Universidade do Minho
#
# USO:
#   Basta alterar o parametro STORE em baixo.
#   O script gera dois ficheiros:
#     EDA_<store>_log.txt   -> resultados em texto
#     EDA_<store>_plots.pdf -> todos os graficos
#
# STORES DISPONIVEIS: "baltimore" | "lancaster" | "philadelphia" | "richmond"
# ============================================================

# ---- WORKING DIRECTORY (automatico) ----
# Garante que o script corre na pasta onde esta guardado
# O CSV deve estar na mesma pasta que este ficheiro .R
if (requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
cat("Working directory:", getwd(), "\n")

# ---- PACKAGES ----
if (!requireNamespace("corrplot", quietly=TRUE)) install.packages("corrplot")
library(corrplot)

# ---- PARAMETRO UNICO A ALTERAR ----
STORE = "lancaster"   # <-- altera aqui para mudar de loja
# ------------------------------------

# ---- CONFIGURACAO POR LOJA ----
store_configs = list(
  baltimore    = list(Fj=1.00, Fx=1.15, Ws=700),
  lancaster    = list(Fj=1.05, Fx=1.20, Ws=730),
  philadelphia = list(Fj=1.10, Fx=1.15, Ws=760),
  richmond     = list(Fj=1.15, Fx=1.25, Ws=800)
)

cfg      = store_configs[[STORE]]
csv_file = paste0(STORE, ".csv")
log_file = paste0("EDA_", STORE, "_log.txt")
pdf_file = paste0("EDA_", STORE, "_plots.pdf")

# ============================================================
# INICIAR OUTPUTS
# ============================================================

sink(log_file, split=TRUE)
pdf(pdf_file, width=10, height=7)

tryCatch({
  
  cat("============================================================\n")
  cat("EDA -", toupper(STORE), "| Executado em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Fj:", cfg$Fj, "| Fx:", cfg$Fx, "| Ws:", cfg$Ws, "\n")
  cat("============================================================\n\n")
  
  # ============================================================
  # 1. CARREGAR E PREPARAR DADOS
  # ============================================================
  
  cat("========================================\n")
  cat("1. CARREGAR E PREPARAR DADOS\n")
  cat("========================================\n")
  
  d = read.table(csv_file, sep=",", header=TRUE)
  d$Date         = as.Date(d$Date)
  d$TouristEvent = as.factor(d$TouristEvent)
  d$DayOfWeek    = factor(weekdays(d$Date),
                          levels=c("Monday","Tuesday","Wednesday",
                                   "Thursday","Friday","Saturday","Sunday"))
  d$Month        = factor(months(d$Date),
                          levels=c("January","February","March","April",
                                   "May","June","July","August",
                                   "September","October","November","December"))
  
  cat("Ficheiro lido:", csv_file, "\n")
  cat("Dimensao:", nrow(d), "linhas x", ncol(d), "colunas\n")
  cat("Periodo:", as.character(min(d$Date)), "a", as.character(max(d$Date)), "\n")
  cat("Duracao (dias):", as.numeric(max(d$Date) - min(d$Date)), "\n\n")
  
  # ============================================================
  # 2. ESTRUTURA E RESUMO
  # ============================================================
  
  cat("========================================\n")
  cat("2. ESTRUTURA E RESUMO GERAL\n")
  cat("========================================\n")
  
  str(d)
  cat("\n")
  print(summary(d))
  cat("\n")
  
  # ============================================================
  # 3. QUALIDADE DOS DADOS - NAs, ZEROS, DUPLICADOS
  # ============================================================
  
  cat("========================================\n")
  cat("3. QUALIDADE DOS DADOS\n")
  cat("========================================\n")
  
  na_counts = colSums(is.na(d))
  cat("--- Valores em Falta (NAs) ---\n")
  print(na_counts)
  cat("Total NAs:", sum(na_counts), "\n\n")
  
  cat("--- Zeros nas variaveis numericas ---\n")
  num_vars = c("Num_Customers","Num_Employees","Pct_On_Sale","Sales")
  for (v in num_vars) {
    cat(v, ":", sum(d[[v]] == 0, na.rm=TRUE), "zeros\n")
  }
  cat("\n")
  
  cat("--- Datas duplicadas ---\n")
  n_dup = sum(duplicated(d$Date))
  cat("Datas duplicadas:", n_dup, "\n")
  if (n_dup > 0) print(d$Date[duplicated(d$Date)])
  cat("\n")
  
  # ============================================================
  # 4. ESTATISTICAS DO TARGET (Num_Customers)
  # ============================================================
  
  cat("========================================\n")
  cat("4. ESTATISTICAS DO TARGET (Num_Customers)\n")
  cat("========================================\n")
  
  target = d$Num_Customers
  q25    = quantile(target, 0.25, na.rm=TRUE)
  q75    = quantile(target, 0.75, na.rm=TRUE)
  iqr    = IQR(target, na.rm=TRUE)
  
  cat("Media:          ", round(mean(target, na.rm=TRUE), 2), "\n")
  cat("Mediana:        ", median(target, na.rm=TRUE), "\n")
  cat("Desvio Padrao:  ", round(sd(target, na.rm=TRUE), 2), "\n")
  cat("Minimo:         ", min(target, na.rm=TRUE), "\n")
  cat("Maximo:         ", max(target, na.rm=TRUE), "\n")
  cat("IQR:            ", iqr, "\n")
  cat("Coef. Variacao: ", round(sd(target, na.rm=TRUE)/mean(target, na.rm=TRUE)*100, 2), "%\n")
  cat("Outliers (IQR): ",
      sum(target < q25 - 1.5*iqr | target > q75 + 1.5*iqr, na.rm=TRUE), "\n\n")
  
  # ============================================================
  # 5. HISTOGRAMAS
  # ============================================================
  
  cat("========================================\n")
  cat("5. HISTOGRAMAS\n")
  cat("========================================\n\n")
  
  par(mfrow=c(2,2))
  hist(d$Num_Customers, col="steelblue", breaks=30,
       main=paste(toupper(STORE), "- Num_Customers (TARGET)"),
       xlab="Clientes")
  hist(d$Num_Employees, col="gray70", breaks=30,
       main="Num_Employees", xlab="Empregados")
  hist(d$Pct_On_Sale,   col="gray70", breaks=30,
       main="Pct_On_Sale",   xlab="% Promocao")
  hist(d$Sales,         col="gray70", breaks=30,
       main="Sales (endogena)", xlab="Vendas")
  
  # ============================================================
  # 6. BOXPLOTS
  # ============================================================
  
  cat("========================================\n")
  cat("6. BOXPLOTS\n")
  cat("========================================\n\n")
  
  par(mfrow=c(2,2))
  boxplot(d$Num_Customers, main="Num_Customers (TARGET)", col="steelblue")
  boxplot(d$Num_Employees, main="Num_Employees",           col="gray70")
  boxplot(d$Pct_On_Sale,   main="Pct_On_Sale",             col="gray70")
  boxplot(d$Sales,         main="Sales",                   col="gray70")
  
  # ============================================================
  # 7. DISTRIBUICAO DE TouristEvent
  # ============================================================
  
  cat("========================================\n")
  cat("7. DISTRIBUICAO DE TouristEvent\n")
  cat("========================================\n")
  
  te_table = table(d$TouristEvent)
  print(te_table)
  cat("Proporcao Yes:", round(te_table["Yes"]/sum(te_table)*100, 1), "%\n\n")
  
  par(mfrow=c(1,1))
  barplot(te_table,
          main=paste(toupper(STORE), "- TouristEvent (Yes vs No)"),
          col=c("gray70","steelblue"),
          ylab="Nr de dias")
  
  # ============================================================
  # 8. DIA DA SEMANA
  # ============================================================
  
  cat("========================================\n")
  cat("8. DIA DA SEMANA - Num_Customers e Sales\n")
  cat("========================================\n")
  
  cat("Media de Num_Customers por dia da semana:\n")
  print(round(tapply(d$Num_Customers, d$DayOfWeek, mean, na.rm=TRUE), 1))
  cat("\nMedia de Sales por dia da semana:\n")
  print(round(tapply(d$Sales, d$DayOfWeek, mean, na.rm=TRUE), 1))
  cat("\n")
  
  par(mfrow=c(1,2))
  boxplot(Num_Customers ~ DayOfWeek, data=d,
          main="Clientes por Dia da Semana",
          col="steelblue", las=2, xlab="")
  boxplot(Sales ~ DayOfWeek, data=d,
          main="Vendas por Dia da Semana",
          col="gray70", las=2, xlab="")
  
  # ============================================================
  # 9. ANALISE POR MES
  # ============================================================
  
  cat("========================================\n")
  cat("9. ANALISE POR MES\n")
  cat("========================================\n")
  
  cat("Media de Num_Customers por mes:\n")
  print(round(tapply(d$Num_Customers, d$Month, mean, na.rm=TRUE), 1))
  cat("\nMedia de Sales por mes:\n")
  print(round(tapply(d$Sales, d$Month, mean, na.rm=TRUE), 1))
  cat("\n")
  
  par(mfrow=c(1,2))
  boxplot(Num_Customers ~ Month, data=d,
          main="Clientes por Mes",
          col="steelblue", las=2, xlab="")
  boxplot(Sales ~ Month, data=d,
          main="Vendas por Mes",
          col="gray70", las=2, xlab="")
  
  # ============================================================
  # 10. MATRIZ DE CORRELACAO (Pearson)
  # ============================================================
  
  cat("========================================\n")
  cat("10. MATRIZ DE CORRELACAO (Pearson)\n")
  cat("========================================\n")
  
  nums       = d[, c("Num_Customers","Num_Employees","Pct_On_Sale","Sales")]
  cor_matrix = round(cor(nums, use="complete.obs"), 3)
  print(cor_matrix)
  cat("\n")
  
  par(mfrow=c(1,1))
  corrplot(cor_matrix,
           method      = "color",
           type        = "upper",
           tl.col      = "black",
           tl.srt      = 45,
           col         = colorRampPalette(c("#6D9EC1","white","#E46726"))(200),
           addCoef.col = "black",
           number.cex  = 0.8,
           title       = paste(toupper(STORE), "- Correlacao entre atributos"),
           mar         = c(0,0,2,0))
  
  # ============================================================
  # 11. INPUTS vs TARGET
  # ============================================================
  
  cat("========================================\n")
  cat("11. INPUTS vs TARGET\n")
  cat("========================================\n")
  
  cat("Correlacao Num_Customers ~ Sales:        ", cor_matrix["Num_Customers","Sales"], "\n")
  cat("Correlacao Num_Customers ~ Num_Employees:", cor_matrix["Num_Customers","Num_Employees"], "\n")
  cat("Correlacao Num_Customers ~ Pct_On_Sale:  ", cor_matrix["Num_Customers","Pct_On_Sale"], "\n\n")
  
  cat("Media Num_Customers SEM evento:", round(mean(d$Num_Customers[d$TouristEvent=="No"],  na.rm=TRUE), 2), "\n")
  cat("Media Num_Customers COM evento:", round(mean(d$Num_Customers[d$TouristEvent=="Yes"], na.rm=TRUE), 2), "\n\n")
  
  par(mfrow=c(2,2))
  plot(d$Num_Employees, d$Num_Customers,
       xlab="Num_Employees", ylab="Num_Customers",
       main="Empregados vs Clientes")
  abline(lm(Num_Customers ~ Num_Employees, data=d), col="red", lwd=2)
  
  plot(d$Pct_On_Sale, d$Num_Customers,
       xlab="Pct_On_Sale", ylab="Num_Customers",
       main="% Promocao vs Clientes")
  abline(lm(Num_Customers ~ Pct_On_Sale, data=d), col="red", lwd=2)
  
  plot(d$Sales, d$Num_Customers,
       xlab="Sales", ylab="Num_Customers",
       main="Vendas vs Clientes")
  abline(lm(Num_Customers ~ Sales, data=d), col="red", lwd=2)
  
  boxplot(Num_Customers ~ TouristEvent, data=d,
          main="Clientes vs TouristEvent",
          col=c("gray70","steelblue"))
  
  # ============================================================
  # 12. SERIE TEMPORAL DO TARGET
  # ============================================================
  
  cat("========================================\n")
  cat("12. SERIE TEMPORAL - Num_Customers\n")
  cat("========================================\n\n")
  
  par(mfrow=c(1,1))
  plot(d$Date, d$Num_Customers, type="l", col="steelblue",
       xlab="Data", ylab="Num_Customers",
       main=paste(toupper(STORE), "- Serie Temporal Num_Customers"))
  
  # ============================================================
  # 13. SERIES TEMPORAIS DAS VARIAVEIS EXOGENAS
  # ============================================================
  
  cat("========================================\n")
  cat("13. SERIES TEMPORAIS - VARIAVEIS EXOGENAS\n")
  cat("========================================\n\n")
  
  par(mfrow=c(2,2))
  plot(d$Date, d$Num_Employees, type="l", col="darkgray",
       main="Num_Employees ao longo do tempo",
       xlab="Data", ylab="Empregados")
  plot(d$Date, d$Pct_On_Sale, type="l", col="darkgray",
       main="Pct_On_Sale ao longo do tempo",
       xlab="Data", ylab="% Promocao")
  plot(d$Date, d$Sales, type="l", col="darkgray",
       main="Sales ao longo do tempo",
       xlab="Data", ylab="Vendas")
  plot(d$Date, as.numeric(d$TouristEvent)-1, type="h", col="steelblue",
       main="TouristEvent ao longo do tempo",
       xlab="Data", ylab="0=No | 1=Yes")
  
  # ============================================================
  # 14. ACF e PACF - lag.max=28 (4 ciclos semanais)
  # confirma sazonalidade K=7, alinhado com 3-passengers.R do professor
  # ============================================================
  
  cat("========================================\n")
  cat("14. ACF e PACF - Num_Customers\n")
  cat("========================================\n\n")
  
  # ts necessario para decompose (bloco 15)
  ts_customers = ts(d$Num_Customers, frequency=7)
  
  # ACF e PACF com vector directo para o eixo x ficar em dias (nao em semanas)
  par(mfrow=c(1,2))
  
  # --- ACF ---
  acf_result = acf(d$Num_Customers, lag.max=28, plot=FALSE)
  plot(acf_result,
       main="ACF - Num_Customers",
       xlab="Lag (dias)",
       col="steelblue", lwd=2)
  # linhas de referencia nas semanas (K=7)
  abline(v=c(7,14,21,28), col="red", lty=2, lwd=1)
  legend("topright",
         legend=c("Autocorrelacao","Intervalo 95%","Ciclo semanal (7 dias)"),
         col=c("steelblue","blue","red"),
         lty=c(1,2,2), lwd=c(2,1,1),
         cex=0.7, bg="white")
  mtext("Picos em lag=7,14,21,28 confirmam sazonalidade semanal (K=7)",
        side=3, line=0.3, cex=0.7, col="darkred")
  
  # --- PACF ---
  pacf_result = pacf(d$Num_Customers, lag.max=28, plot=FALSE)
  plot(pacf_result,
       main="PACF - Num_Customers",
       xlab="Lag (dias)",
       col="steelblue", lwd=2)
  abline(v=c(7,14,21,28), col="red", lty=2, lwd=1)
  legend("topright",
         legend=c("Autocorr. Parcial","Intervalo 95%","Ciclo semanal (7 dias)"),
         col=c("steelblue","blue","red"),
         lty=c(1,2,2), lwd=c(2,1,1),
         cex=0.7, bg="white")
  mtext("Lag 1 domina; lag 7 confirma dependencia no mesmo dia da semana anterior",
        side=3, line=0.3, cex=0.7, col="darkred")
  
  # ============================================================
  # 15. DECOMPOSICAO DA SERIE TEMPORAL
  # ============================================================
  
  cat("========================================\n")
  cat("15. DECOMPOSICAO - Num_Customers\n")
  cat("========================================\n")
  
  par(mfrow=c(1,1))
  dec = decompose(ts_customers)
  plot(dec)
  
  cat("Amplitude sazonal semanal:",
      round(max(dec$seasonal, na.rm=TRUE) - min(dec$seasonal, na.rm=TRUE), 2), "\n\n")
  
  # ============================================================
  # 16. ANALISE DETALHADA - Pct_On_Sale
  # ============================================================
  
  cat("========================================\n")
  cat("16. ANALISE DETALHADA - Pct_On_Sale\n")
  cat("========================================\n")
  
  cat("Estatisticas Pct_On_Sale:\n")
  print(summary(d$Pct_On_Sale))
  cat("Desvio Padrao:", round(sd(d$Pct_On_Sale, na.rm=TRUE), 3), "\n")
  cat("Zeros:        ", sum(d$Pct_On_Sale == 0, na.rm=TRUE), "\n")
  cat("NAs:          ", sum(is.na(d$Pct_On_Sale)), "\n\n")
  
  par(mfrow=c(1,2))
  hist(d$Pct_On_Sale, col="steelblue", breaks=20,
       main="Distribuicao Pct_On_Sale", xlab="% Promocao")
  boxplot(d$Pct_On_Sale, main="Boxplot Pct_On_Sale", col="steelblue")
  
  # ============================================================
  # 17. CORRPLOT - Correlacao entre atributos (Pearson)
  # ============================================================
  
  cat("========================================\n")
  cat("17. CORRPLOT - Correlacao entre atributos\n")
  cat("========================================\n\n")
  
  nums_corrplot = d[, c("Num_Employees","Num_Customers","Pct_On_Sale","Sales")]
  cor_full      = round(cor(nums_corrplot, use="complete.obs"), 3)
  
  cat("Matriz de correlacao:\n")
  print(cor_full)
  cat("\n")
  
  par(mfrow=c(1,1))
  corrplot(cor_full,
           method      = "color",
           type        = "upper",
           tl.col      = "black",
           tl.srt      = 45,
           col         = colorRampPalette(c("#6D9EC1","white","#E46726"))(200),
           addCoef.col = "black",
           number.cex  = 0.8,
           title       = paste(toupper(STORE), "- Correlacao entre atributos"),
           mar         = c(0,0,2,0))
  
  # ============================================================
  # 18. RESUMO FINAL
  # ============================================================
  
  cat("============================================================\n")
  cat("18. RESUMO FINAL -", toupper(STORE), "\n")
  cat("============================================================\n")
  cat("Registos:", nrow(d), "| Variaveis originais:", ncol(d) - 2, "(+ DayOfWeek, Month)\n")
  cat("Periodo:", as.character(min(d$Date)), "a", as.character(max(d$Date)), "\n")
  cat("Target (Num_Customers) - Media:", round(mean(target, na.rm=TRUE), 1),
      "| Sd:", round(sd(target, na.rm=TRUE), 1), "\n")
  cat("NAs totais:", sum(na_counts), "\n")
  cat("Correlacao Num_Customers ~ Sales:        ", cor_matrix["Num_Customers","Sales"], "\n")
  cat("Correlacao Num_Customers ~ Num_Employees:", cor_matrix["Num_Customers","Num_Employees"], "\n")
  cat("Correlacao Num_Customers ~ Pct_On_Sale:  ", cor_matrix["Num_Customers","Pct_On_Sale"], "\n")
  cat("Proporcao TouristEvent=Yes:", round(te_table["Yes"]/sum(te_table)*100, 1), "%\n")
  cat("Amplitude sazonal semanal:",
      round(max(dec$seasonal, na.rm=TRUE) - min(dec$seasonal, na.rm=TRUE), 2), "\n")
  cat("Fj:", cfg$Fj, "| Fx:", cfg$Fx, "| Ws:", cfg$Ws, "\n")
  cat("============================================================\n\n")
  
}, error = function(e) {
  cat("\n>> ERRO:", conditionMessage(e), "\n")
}, finally = {
  dev.off()
  sink()
})

cat("\n>> EDA concluida para:", toupper(STORE), "\n")
cat(">> Log:     ", log_file, "\n")
cat(">> Graficos:", pdf_file, "\n")