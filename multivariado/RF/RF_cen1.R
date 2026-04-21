# =============================================================================
# EDU — RF Multivariado — Cenário 1 (só exógenas)
# Target: Num_Customers, H=7
# Inputs: lags NC c(1:3,7,28) + TouristEvent + Num_Employees + Pct_On_Sale
# =============================================================================

options(rgl.useNULL = TRUE)
library(rminer)

# --- Carregar dados tratados ---
setwd("~/TIAPOSE2526/Files/csv")
source("~/TIAPOSE2526/Files/tratamentoDeDados.R")

# --- Pasta de resultados ---
dir.create("~/TIAPOSE2526/Files/fase1/RF_multi", showWarnings=FALSE)
setwd("~/TIAPOSE2526/Files/fase1/RF_multi")

# --- Configuração ---
H      <- 7
RUNS   <- 12
S      <- 7
LAGS_NC <- c(1:3, 7, 28)

datas_fecho <- as.Date(c("2012-12-25","2013-12-25","2013-03-31","2014-04-20"))

stores <- list(
  Baltimore    = baltimore,
  Lancaster    = lancaster,
  Philadelphia = philadelphia,
  Richmond     = richmond
)
lojas_cores <- c("steelblue", "tomato", "seagreen", "hotpink")

# --- Seasonal Naive ---
seasonal_naive <- function(train, h = 7) tail(train, h)

# =============================================================================
# FUNCAO: construir data.frame com lags NC + exógenas
# =============================================================================
build_df_c1 <- function(dados) {
  nc  <- dados$Num_Customers
  te  <- as.numeric(dados$TouristEvent == "Yes")
  ne  <- dados$Num_Employees
  ps  <- dados$Pct_On_Sale
  n   <- length(nc)
  ml  <- max(LAGS_NC)
  
  df <- data.frame(matrix(NA, nrow = n - ml, ncol = 0))
  
  for (lag in LAGS_NC) {
    df[[paste0("NC_lag", lag)]] <- nc[(ml - lag + 1):(n - lag)]
  }
  
  idx <- (ml + 1):n
  df$TouristEvent  <- te[idx]
  df$Num_Employees <- ne[idx]
  df$Pct_On_Sale   <- ps[idx]
  df$y <- nc[idx]
  
  return(df)
}

# =============================================================================
# FUNCAO: previsão recursive multi-step (lags NC atualizados com previstos)
# =============================================================================
predict_recursive_c1 <- function(modelo, df, start_idx, h) {
  preds <- numeric(h)
  
  for (step in 1:h) {
    row <- df[start_idx + step, , drop = FALSE]
    
    for (lag in LAGS_NC) {
      if (step > lag) {
        row[[paste0("NC_lag", lag)]] <- preds[step - lag]
      }
    }
    
    preds[step] <- predict(modelo, row)
  }
  return(preds)
}

# =============================================================================
# BACKTESTING
# =============================================================================
cat("\n############################################################\n")
cat("# CENÁRIO 1 — RF MULTIVARIADO (SÓ EXÓGENAS)              #\n")
cat("############################################################\n")

resultados_iter <- data.frame()
resultados_media <- data.frame()
idx_cor <- 1

for (nome in names(stores)) {
  cat("\n============================================================\n")
  cat("BACKTESTING -", toupper(nome), "\n")
  cat("============================================================\n")
  
  d <- stores[[nome]]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  
  df <- build_df_c1(d)
  ND <- nrow(df)
  max_lag <- max(LAGS_NC)
  datas_df <- d$Date[(max_lag + 1):nrow(d)]
  
  YR <- diff(range(d$Num_Customers))
  if (YR == 0) YR <- 1
  
  W <- (ND - H) - (RUNS - 1) * S
  
  nmae_rf_v    <- numeric(RUNS)
  rrse_rf_v    <- numeric(RUNS)
  r2_rf_v      <- numeric(RUNS)
  nmae_naive_v <- numeric(RUNS)
  rrse_naive_v <- numeric(RUNS)
  r2_naive_v   <- numeric(RUNS)
  
  all_real <- c()
  all_pred_rf <- c()
  all_pred_naive <- c()
  
  res_loja <- data.frame()
  
  for (b in 1:RUNS) {
    hd <- holdout(df$y, ratio = H, mode = "incremental",
                  iter = b, window = W, increment = S)
    
    real <- df$y[hd$ts]
    datas_teste <- datas_df[hd$ts]
    
    # --- RF ---
    RF <- fit(y ~ ., df[hd$tr, ], model = "randomForest")
    Pred_rf <- predict_recursive_c1(RF, df, hd$ts[1] - 1, H)
    
    # --- Seasonal Naive ---
    train_y <- df$y[hd$tr]
    Pred_naive <- seasonal_naive(train_y, h = H)
    
    # --- Pós-processamento ---
    Pred_rf[Pred_rf < 0] <- 0
    Pred_rf[datas_teste %in% datas_fecho] <- 0
    Pred_naive[Pred_naive < 0] <- 0
    Pred_naive[datas_teste %in% datas_fecho] <- 0
    
    # --- Métricas RF ---
    nmae_rf_v[b] <- mmetric(real, Pred_rf, metric = "NMAE", val = YR)
    rrse_rf_v[b] <- mmetric(real, Pred_rf, metric = "RRSE")
    r2_rf_v[b]   <- 1 - (sum((real - Pred_rf)^2) / sum((real - mean(real))^2))
    
    # --- Métricas Naive ---
    nmae_naive_v[b] <- mmetric(real, Pred_naive, metric = "NMAE", val = YR)
    rrse_naive_v[b] <- mmetric(real, Pred_naive, metric = "RRSE")
    r2_naive_v[b]   <- 1 - (sum((real - Pred_naive)^2) / sum((real - mean(real))^2))
    
    all_real <- c(all_real, real)
    all_pred_rf <- c(all_pred_rf, Pred_rf)
    all_pred_naive <- c(all_pred_naive, Pred_naive)
    
    cat(sprintf("  iter %2d | RF NMAE=%.4f RRSE=%.4f R2=%.4f | Naive NMAE=%.4f\n",
                b, nmae_rf_v[b], rrse_rf_v[b], r2_rf_v[b], nmae_naive_v[b]))
    
    linha <- data.frame(
      Loja = nome, Iteracao = b,
      Test_Start = as.character(datas_teste[1]),
      Test_End = as.character(datas_teste[length(datas_teste)]),
      NMAE_rf = round(nmae_rf_v[b], 4),
      RRSE_rf = round(rrse_rf_v[b], 4),
      R2_rf = round(r2_rf_v[b], 4),
      NMAE_naive = round(nmae_naive_v[b], 4),
      RRSE_naive = round(rrse_naive_v[b], 4),
      R2_naive = round(r2_naive_v[b], 4)
    )
    res_loja <- rbind(res_loja, linha)
  }
  
  # --- Gráfico de linhas: real vs RF vs Naive (todas as iterações) ---
  pdf(paste0("grafico_rf_c1_", tolower(nome), ".pdf"), width = 12, height = 5)
  n_pts <- length(all_real)
  yrange <- range(all_real, all_pred_rf, all_pred_naive)
  plot(1:n_pts, all_real, type = "l", col = "black", lwd = 1.5,
       ylim = yrange, xlab = "Observacao (teste acumulado)", ylab = "Num_Customers",
       main = paste("C1 -", nome, "| Real vs RF Multi vs Naive"))
  lines(1:n_pts, all_pred_naive, col = "gray50", lty = 2, lwd = 1.2)
  lines(1:n_pts, all_pred_rf,    col = "purple",  lty = 1, lwd = 1.5)
  for (j in 1:(RUNS - 1)) abline(v = j * H, col = "gray80", lty = 3)
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "RF Multi C1"),
         col = c("black", "gray50", "purple"),
         lty = c(1, 2, 1), lwd = c(1.5, 1.2, 1.5))
  dev.off()
  
  # --- Boxplot ---
  pdf(paste0("boxplot_rf_c1_", tolower(nome), ".pdf"), width = 7, height = 5)
  boxplot(nmae_rf_v, rrse_rf_v, r2_rf_v,
          names = c("NMAE", "RRSE", "R2"),
          main = paste(nome, "- RF Multi C1 - Metricas (12 iter)"),
          col = lojas_cores[idx_cor])
  dev.off()
  
  # --- Médias ---
  cat(sprintf("\n  [RF C1] Mean: NMAE=%.4f | RRSE=%.4f | R2=%.4f\n",
              mean(nmae_rf_v), mean(rrse_rf_v), mean(r2_rf_v)))
  cat(sprintf("  [Naive] Mean: NMAE=%.4f | RRSE=%.4f | R2=%.4f\n",
              mean(nmae_naive_v), mean(rrse_naive_v), mean(r2_naive_v)))
  
  melhoria <- (mean(nmae_naive_v) - mean(nmae_rf_v)) / mean(nmae_naive_v) * 100
  cat(sprintf("  Melhoria RF vs Naive: %.1f%%\n", melhoria))
  
  resultados_iter <- rbind(resultados_iter, res_loja)
  resultados_media <- rbind(resultados_media, data.frame(
    Loja = nome,
    Media_NMAE_rf = round(mean(nmae_rf_v), 4),
    Media_RRSE_rf = round(mean(rrse_rf_v), 4),
    Media_R2_rf = round(mean(r2_rf_v), 4),
    Media_NMAE_naive = round(mean(nmae_naive_v), 4),
    Media_RRSE_naive = round(mean(rrse_naive_v), 4),
    Media_R2_naive = round(mean(r2_naive_v), 4)
  ))
  
  idx_cor <- idx_cor + 1
}

# --- Resultados ---
cat("\n============================================================\n")
cat("RESULTADOS FINAIS - CENÁRIO 1\n")
cat("============================================================\n")
print(resultados_media, row.names = FALSE)

# --- CSV ---
write.csv(resultados_iter, "backtesting_rf_c1_iteracoes.csv", row.names = FALSE)
write.csv(resultados_media, "backtesting_rf_c1_media.csv", row.names = FALSE)
cat("\nCSVs guardados.\n")