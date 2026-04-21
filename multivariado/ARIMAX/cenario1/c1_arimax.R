# =============================================================================
# CAROLINA — ARIMAX Multivariado — Cenário 1 (só exógenas)
# Target: Num_Customers, H=7
# Inputs: ARIMA(Num_Customers) + TouristEvent + Num_Employees + Pct_On_Sale
# Backtesting: growing window, 12 iterações
# Métricas: NMAE, RRSE, R2
# Pós-processamento: negativos -> 0 ; datas de fecho -> 0
# =============================================================================

options(rgl.useNULL = TRUE)

library(forecast)
library(rminer)

# -----------------------------------------------------------------------------
# 1. CARREGAR DADOS
# -----------------------------------------------------------------------------
setwd("~/TIAPOSE_projeto/tiapose2526/Files/csv")
source("~/TIAPOSE_projeto/tiapose2526/Files/tratamentoDeDados.R")

# -----------------------------------------------------------------------------
# 2. PASTA DE RESULTADOS
# -----------------------------------------------------------------------------
dir.create("~/TIAPOSE_projeto/tiapose2526/Files/fase1/Multivariado/ARIMA/cenário1", showWarnings = FALSE, recursive = TRUE)
setwd("~/TIAPOSE_projeto/tiapose2526/Files/fase1/Multivariado/ARIMA/cenário1")

# -----------------------------------------------------------------------------
# 3. CONFIGURAÇÃO
# -----------------------------------------------------------------------------
H    <- 7
RUNS <- 12
S    <- 7

datas_fecho <- as.Date(c("2012-12-25", "2013-12-25", "2013-03-31", "2014-04-20"))

stores <- list(
  Baltimore    = baltimore,
  Lancaster    = lancaster,
  Philadelphia = philadelphia,
  Richmond     = richmond
)

lojas_cores <- c("steelblue", "tomato", "seagreen", "hotpink")

# -----------------------------------------------------------------------------
# 4. BASELINE
# -----------------------------------------------------------------------------
seasonal_naive <- function(train, h = 7) tail(train, h)

# -----------------------------------------------------------------------------
# 5. AUX FUNCTIONS
# -----------------------------------------------------------------------------
to_numeric_safe <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.numeric(x))
  if (is.factor(x)) return(as.numeric(as.character(x)))
  return(as.numeric(x))
}

build_df_c1_arimax <- function(dados) {
  d <- dados[, c("Date", "Num_Customers", "TouristEvent", "Num_Employees", "Pct_On_Sale")]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  rownames(d) <- NULL
  
  # TouristEvent -> 0/1
  if (is.character(d$TouristEvent) || is.factor(d$TouristEvent)) {
    d$TouristEvent <- as.numeric(d$TouristEvent == "Yes")
  } else {
    d$TouristEvent <- to_numeric_safe(d$TouristEvent)
  }
  
  d$Num_Employees <- to_numeric_safe(d$Num_Employees)
  d$Pct_On_Sale   <- to_numeric_safe(d$Pct_On_Sale)
  d$Num_Customers <- to_numeric_safe(d$Num_Customers)
  
  return(d)
}

calc_r2_safe <- function(y, pred) {
  sst <- sum((y - mean(y))^2)
  if (sst == 0) return(NA_real_)
  1 - sum((y - pred)^2) / sst
}

# -----------------------------------------------------------------------------
# 6. BACKTESTING
# -----------------------------------------------------------------------------
cat("\n############################################################\n")
cat("# CENÁRIO 1 — ARIMAX MULTIVARIADO (SÓ EXÓGENAS)           #\n")
cat("############################################################\n")

resultados_iter  <- data.frame()
resultados_media <- data.frame()
idx_cor <- 1

for (nome in names(stores)) {
  
  cat("\n============================================================\n")
  cat("BACKTESTING -", toupper(nome), "\n")
  cat("============================================================\n")
  
  d <- build_df_c1_arimax(stores[[nome]])
  N <- nrow(d)
  
  YR <- diff(range(d$Num_Customers, na.rm = TRUE))
  if (YR == 0) YR <- 1
  
  W <- (N - H) - (RUNS - 1) * S
  if (W <= 0) stop(paste("Janela inicial inválida para", nome))
  
  nmae_arimax_v <- numeric(RUNS)
  rrse_arimax_v <- numeric(RUNS)
  r2_arimax_v   <- numeric(RUNS)
  
  nmae_naive_v <- numeric(RUNS)
  rrse_naive_v <- numeric(RUNS)
  r2_naive_v   <- numeric(RUNS)
  
  all_real <- c()
  all_pred_arimax <- c()
  all_pred_naive <- c()
  
  res_loja <- data.frame()
  
  for (b in 1:RUNS) {
    
    hd <- holdout(d$Num_Customers, ratio = H, mode = "incremental",
                  iter = b, window = W, increment = S)
    
    real <- d$Num_Customers[hd$ts]
    datas_teste <- d$Date[hd$ts]
    
    # Série target
    y_train <- ts(d$Num_Customers[hd$tr], frequency = 7)
    
    # Exógenas conhecidas no futuro
    xreg_train <- as.matrix(d[hd$tr, c("TouristEvent", "Num_Employees", "Pct_On_Sale")])
    xreg_test  <- as.matrix(d[hd$ts, c("TouristEvent", "Num_Employees", "Pct_On_Sale")])
    
    # --- ARIMAX ---
    fit_arimax <- auto.arima(y_train, xreg = xreg_train)
    fc_arimax  <- forecast(fit_arimax, h = H, xreg = xreg_test)
    Pred_arimax <- as.numeric(fc_arimax$mean)
    
    # --- Seasonal Naive ---
    train_y <- d$Num_Customers[hd$tr]
    Pred_naive <- seasonal_naive(train_y, h = H)
    
    # --- Pós-processamento ---
    Pred_arimax[Pred_arimax < 0] <- 0
    Pred_arimax[datas_teste %in% datas_fecho] <- 0
    
    Pred_naive[Pred_naive < 0] <- 0
    Pred_naive[datas_teste %in% datas_fecho] <- 0
    
    # --- Métricas ARIMAX ---
    nmae_arimax_v[b] <- mmetric(real, Pred_arimax, metric = "NMAE", val = YR)
    rrse_arimax_v[b] <- mmetric(real, Pred_arimax, metric = "RRSE")
    r2_arimax_v[b]   <- calc_r2_safe(real, Pred_arimax)
    
    # --- Métricas Naive ---
    nmae_naive_v[b] <- mmetric(real, Pred_naive, metric = "NMAE", val = YR)
    rrse_naive_v[b] <- mmetric(real, Pred_naive, metric = "RRSE")
    r2_naive_v[b]   <- calc_r2_safe(real, Pred_naive)
    
    all_real <- c(all_real, real)
    all_pred_arimax <- c(all_pred_arimax, Pred_arimax)
    all_pred_naive <- c(all_pred_naive, Pred_naive)
    
    cat(sprintf("  iter %2d | ARIMAX NMAE=%.4f RRSE=%.4f R2=%.4f | Naive NMAE=%.4f\n",
                b, nmae_arimax_v[b], rrse_arimax_v[b], r2_arimax_v[b], nmae_naive_v[b]))
    
    linha <- data.frame(
      Loja = nome,
      Iteracao = b,
      Test_Start = as.character(datas_teste[1]),
      Test_End = as.character(datas_teste[length(datas_teste)]),
      NMAE_arimax = round(nmae_arimax_v[b], 4),
      RRSE_arimax = round(rrse_arimax_v[b], 4),
      R2_arimax = round(r2_arimax_v[b], 4),
      NMAE_naive = round(nmae_naive_v[b], 4),
      RRSE_naive = round(rrse_naive_v[b], 4),
      R2_naive = round(r2_naive_v[b], 4)
    )
    
    res_loja <- rbind(res_loja, linha)
  }
  
  # --- Gráfico de linhas ---
  pdf(paste0("grafico_arimax_c1_", tolower(nome), ".pdf"), width = 12, height = 5)
  n_pts <- length(all_real)
  yrange <- range(all_real, all_pred_arimax, all_pred_naive, na.rm = TRUE)
  
  plot(1:n_pts, all_real, type = "l", col = "black", lwd = 1.5,
       ylim = yrange, xlab = "Observacao (teste acumulado)", ylab = "Num_Customers",
       main = paste("C1 -", nome, "| Real vs ARIMAX vs Naive"))
  lines(1:n_pts, all_pred_naive, col = "gray50", lty = 2, lwd = 1.2)
  lines(1:n_pts, all_pred_arimax, col = "purple", lty = 1, lwd = 1.5)
  for (j in 1:(RUNS - 1)) abline(v = j * H, col = "gray80", lty = 3)
  
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "ARIMAX C1"),
         col = c("black", "gray50", "purple"),
         lty = c(1, 2, 1), lwd = c(1.5, 1.2, 1.5))
  dev.off()
  
  # --- Boxplot ---
  pdf(paste0("boxplot_arimax_c1_", tolower(nome), ".pdf"), width = 7, height = 5)
  boxplot(nmae_arimax_v, rrse_arimax_v, r2_arimax_v,
          names = c("NMAE", "RRSE", "R2"),
          main = paste(nome, "- ARIMAX Multi C1 - Metricas (12 iter)"),
          col = lojas_cores[idx_cor])
  dev.off()
  
  # --- Médias ---
  cat(sprintf("\n  [ARIMAX C1] Mean: NMAE=%.4f | RRSE=%.4f | R2=%.4f\n",
              mean(nmae_arimax_v, na.rm = TRUE),
              mean(rrse_arimax_v, na.rm = TRUE),
              mean(r2_arimax_v, na.rm = TRUE)))
  cat(sprintf("  [Naive]     Mean: NMAE=%.4f | RRSE=%.4f | R2=%.4f\n",
              mean(nmae_naive_v, na.rm = TRUE),
              mean(rrse_naive_v, na.rm = TRUE),
              mean(r2_naive_v, na.rm = TRUE)))
  
  melhoria <- (mean(nmae_naive_v, na.rm = TRUE) - mean(nmae_arimax_v, na.rm = TRUE)) /
    mean(nmae_naive_v, na.rm = TRUE) * 100
  cat(sprintf("  Melhoria ARIMAX vs Naive: %.1f%%\n", melhoria))
  
  resultados_iter <- rbind(resultados_iter, res_loja)
  resultados_media <- rbind(resultados_media, data.frame(
    Loja = nome,
    Media_NMAE_arimax = round(mean(nmae_arimax_v, na.rm = TRUE), 4),
    Media_RRSE_arimax = round(mean(rrse_arimax_v, na.rm = TRUE), 4),
    Media_R2_arimax = round(mean(r2_arimax_v, na.rm = TRUE), 4),
    Media_NMAE_naive = round(mean(nmae_naive_v, na.rm = TRUE), 4),
    Media_RRSE_naive = round(mean(rrse_naive_v, na.rm = TRUE), 4),
    Media_R2_naive = round(mean(r2_naive_v, na.rm = TRUE), 4)
  ))
  
  idx_cor <- idx_cor + 1
}

# -----------------------------------------------------------------------------
# 7. RESULTADOS FINAIS
# -----------------------------------------------------------------------------
cat("\n============================================================\n")
cat("RESULTADOS FINAIS - CENÁRIO 1 (ARIMAX)\n")
cat("============================================================\n")
print(resultados_media, row.names = FALSE)

# -----------------------------------------------------------------------------
# 8. EXPORTAR CSV
# -----------------------------------------------------------------------------
write.csv(resultados_iter, "backtesting_arimax_c1_iteracoes.csv", row.names = FALSE)
write.csv(resultados_media, "backtesting_arimax_c1_media.csv", row.names = FALSE)

cat("\nCSVs guardados.\n")