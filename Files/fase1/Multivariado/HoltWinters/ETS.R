# ============================================================
# Fase II — ETS Multivariado — Cenário 1
# Target: Num_Customers | H=7 | Growing Window | 12 iterações
# Método: tslm() (efeito exógenas) + ets() (resíduos)
# Exógenas: TouristEvent + Num_Employees (conhecidas no futuro)
# ============================================================

library(forecast)
options(rgl.useNULL = TRUE)
library(rminer)

# --- Carregar dados ---
setwd("/Users/edias/TIAPOSE2526/Files/csv")
source("../tratamentoDeDados.R")

# --- Mudar para pasta de output ---
setwd("/Users/edias/TIAPOSE2526")
output_dir <- "/Users/edias/TIAPOSE2526/Files/fase1/Multivariado/HoltWinters"
dir.create(output_dir, showWarnings = FALSE)

# --- Redirecionar output para ficheiro txt ---
sink(file.path(output_dir, "output_ets_c1.txt"), split = TRUE)

# --- Datas de fecho ---
datas_fecho <- as.Date(c("2012-12-25", "2013-12-25", "2013-03-31", "2014-04-20"))

# --- Configuração ---
K    <- 7
Test <- 7
S    <- 7
Runs <- 12

# --- Lojas ---
lojas_nomes <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
lojas_dados <- list(baltimore, lancaster, philadelphia, richmond)
lojas_cores <- c("steelblue", "tomato", "seagreen", "hotpink")

# --- Data frames para resultados ---
resultados_iteracoes <- data.frame()
resultados_media     <- data.frame()

cat("############################################################\n")
cat("# FASE II — ETS MULTIVARIADO C1 — GROWING WINDOW         #\n")
cat("# Exógenas: TouristEvent + Num_Employees                  #\n")
cat("# 12 iterações, H=7, Target: Num_Customers                #\n")
cat("############################################################\n\n")

for (i in seq_along(lojas_nomes)) {
  
  loja     <- lojas_nomes[i]
  dados    <- lojas_dados[[i]]
  cor_loja <- lojas_cores[i]
  
  cat("\n============================================================\n")
  cat("BACKTESTING -", toupper(loja), "\n")
  cat("============================================================\n")
  
  dados$Date <- as.Date(dados$Date)
  dados      <- dados[order(dados$Date), ]
  
  d1    <- dados$Num_Customers
  tour  <- as.numeric(dados$TouristEvent == "Yes")
  emp   <- dados$Num_Employees
  datas <- dados$Date
  L     <- length(d1)
  
  W  <- (L - Test) - (Runs - 1) * S
  YR <- diff(range(d1))
  if (YR == 0) YR <- 1
  
  MAE_v  <- numeric(Runs)
  NMAE_v <- numeric(Runs)
  RRSE_v <- numeric(Runs)
  R2_v   <- numeric(Runs)
  
  res_loja <- data.frame()
  
  for (b in 1:Runs) {
    
    H_idx <- holdout(d1, ratio = Test, mode = "incremental",
                     iter = b, window = W, increment = S)
    
    Y_tr    <- d1[H_idx$tr]
    tour_tr <- tour[H_idx$tr]
    emp_tr  <- emp[H_idx$tr]
    
    Y           <- d1[H_idx$ts]
    tour_ts     <- tour[H_idx$ts]
    emp_ts      <- emp[H_idx$ts]
    datas_teste <- datas[H_idx$ts]
    
    ts_tr   <- ts(Y_tr, frequency = K)
    xreg_tr <- data.frame(TouristEvent  = tour_tr,
                          Num_Employees = emp_tr)
    fit_lm  <- tslm(ts_tr ~ TouristEvent + Num_Employees, data = xreg_tr)
    fit_ets <- suppressWarnings(ets(residuals(fit_lm)))
    
    xreg_ts  <- data.frame(TouristEvent  = tour_ts,
                           Num_Employees = emp_ts)
    pred_lm  <- as.numeric(predict(fit_lm,  newdata = xreg_ts))
    pred_ets <- as.numeric(forecast(fit_ets, h = Test)$mean)
    Pred     <- pred_lm + pred_ets
    
    Pred[Pred < 0]                     <- 0
    Pred[datas_teste %in% datas_fecho] <- 0
    
    MAE_v[b]  <- mmetric(y = Y, x = Pred, metric = "MAE")
    NMAE_v[b] <- mmetric(y = Y, x = Pred, metric = "NMAE", val = YR)
    RRSE_v[b] <- mmetric(y = Y, x = Pred, metric = "RRSE")
    R2_v[b]   <- 1 - (sum((Y - Pred)^2) / sum((Y - mean(Y))^2))
    
    linha <- data.frame(
      Loja        = loja,
      Iteracao    = b,
      Train_Start = datas[H_idx$tr[1]],
      Train_End   = datas[H_idx$tr[length(H_idx$tr)]],
      Train_Size  = length(H_idx$tr),
      Test_Start  = datas_teste[1],
      Test_End    = datas_teste[length(datas_teste)],
      Test_Size   = length(H_idx$ts),
      Modelo      = "ETS_Multivariado_C1",
      ETS_Model   = as.character(fit_ets$method),
      MAE         = round(MAE_v[b],  4),
      NMAE        = round(NMAE_v[b], 4),
      RRSE        = round(RRSE_v[b], 4),
      R2          = round(R2_v[b],   4)
    )
    res_loja <- rbind(res_loja, linha)
    
    cat(sprintf("  iter %2d | TR: %s a %s (n=%d) | NMAE: %.4f | RRSE: %.4f | R2: %.4f\n",
                b,
                as.character(datas[H_idx$tr[1]]),
                as.character(datas[H_idx$tr[length(H_idx$tr)]]),
                length(H_idx$tr),
                NMAE_v[b], RRSE_v[b], R2_v[b]))
  }
  
  pdf(file.path(output_dir, paste0("boxplot_ets_c1_", tolower(loja), ".pdf")),
      width = 7, height = 5)
  boxplot(NMAE_v, RRSE_v, R2_v,
          names = c("NMAE", "RRSE", "R2"),
          main  = paste(loja, "- ETS Multi C1 - Distribuição das Métricas (12 iter)"),
          col   = cor_loja)
  dev.off()
  
  med_loja <- data.frame(
    Loja       = loja,
    Target     = "Num_Customers",
    Modelo     = "ETS_Multivariado_C1",
    Metodo     = "Growing Window",
    Iteracoes  = Runs,
    Horizonte  = Test,
    Incremento = S,
    Media_MAE  = round(mean(MAE_v),  4),
    Media_NMAE = round(mean(NMAE_v), 4),
    Media_RRSE = round(mean(RRSE_v), 4),
    Media_R2   = round(mean(R2_v),   4)
  )
  
  cat(sprintf("\n  Médias %s: MAE=%.4f | NMAE=%.4f | RRSE=%.4f | R2=%.4f\n\n",
              loja, mean(MAE_v), mean(NMAE_v), mean(RRSE_v), mean(R2_v)))
  
  resultados_iteracoes <- rbind(resultados_iteracoes, res_loja)
  resultados_media     <- rbind(resultados_media,     med_loja)
}

cat("\n============================================================\n")
cat("RESULTADOS FINAIS - ETS MULTIVARIADO C1\n")
cat("============================================================\n")
print(resultados_media)

sink()

write.csv(resultados_iteracoes,
          file.path(output_dir, "backtesting_ets_c1_iteracoes.csv"),
          row.names = FALSE)
write.csv(resultados_media,
          file.path(output_dir, "backtesting_ets_c1_media.csv"),
          row.names = FALSE)

# ============================================================
# PREVISÃO FINAL — Baltimore — próximos 7 dias (para PREV)
# ============================================================
cat("\n=== PREV Baltimore — ETS C1 ===\n")

dados_bal       <- baltimore
dados_bal$Date  <- as.Date(dados_bal$Date)
dados_bal       <- dados_bal[order(dados_bal$Date), ]

d1_bal   <- dados_bal$Num_Customers
tour_bal <- as.numeric(dados_bal$TouristEvent == "Yes")
emp_bal  <- dados_bal$Num_Employees

ts_all   <- ts(d1_bal, frequency = 7)
xreg_all <- data.frame(TouristEvent  = tour_bal,
                       Num_Employees = emp_bal)

fit_lm_final  <- tslm(ts_all ~ TouristEvent + Num_Employees, data = xreg_all)
fit_ets_final <- suppressWarnings(ets(residuals(fit_lm_final)))

xreg_fut <- data.frame(
  TouristEvent  = c(0, 0, 0, 0, 0, 0, 0),
  Num_Employees = rep(round(mean(emp_bal)), 7)
)

pred_lm_fut    <- as.numeric(predict(fit_lm_final,  newdata = xreg_fut))
pred_ets_fut   <- as.numeric(forecast(fit_ets_final, h = 7)$mean)
prev_baltimore <- round(pmax(pred_lm_fut + pred_ets_fut, 0))

cat("prev_baltimore <-", paste0("c(", paste(prev_baltimore, collapse = ", "), ")"), "\n")
cat("\nCSVs guardados em:", output_dir, "\n")