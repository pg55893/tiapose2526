# =============================================================================
# CAROLINA — ARIMAX Multivariado — Cenário 2 (Exógenas + Sales)
# Target: Num_Customers, H=7
# Inputs: ARIMA(Num_Customers) + TouristEvent + Num_Employees + Pct_On_Sale
#         + lags de Sales
# Sales é endógena: é prevista recursivamente ao longo do horizonte
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
dir.create("~/TIAPOSE_projeto/tiapose2526/Files/fase1/Multivariado/ARIMA/cenário2",
           showWarnings = FALSE, recursive = TRUE)
setwd("~/TIAPOSE_projeto/tiapose2526/Files/fase1/Multivariado/ARIMA/cenário2")

# -----------------------------------------------------------------------------
# 3. CONFIGURAÇÃO
# -----------------------------------------------------------------------------
H <- 7
RUNS <- 12
S <- 7
LAGS_SALES <- c(1, 7)

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

build_df_c2_arimax <- function(dados) {
  d <- dados[, c("Date", "Num_Customers", "Sales", "TouristEvent", "Num_Employees", "Pct_On_Sale")]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  rownames(d) <- NULL
  
  if (is.character(d$TouristEvent) || is.factor(d$TouristEvent)) {
    d$TouristEvent <- as.numeric(d$TouristEvent == "Yes")
  } else {
    d$TouristEvent <- to_numeric_safe(d$TouristEvent)
  }
  
  d$Num_Customers <- to_numeric_safe(d$Num_Customers)
  d$Sales         <- to_numeric_safe(d$Sales)
  d$Num_Employees <- to_numeric_safe(d$Num_Employees)
  d$Pct_On_Sale   <- to_numeric_safe(d$Pct_On_Sale)
  
  return(d)
}

calc_r2_safe <- function(y, pred) {
  sst <- sum((y - mean(y))^2)
  if (sst == 0) return(NA_real_)
  1 - sum((y - pred)^2) / sst
}

make_xreg_train_customers <- function(df, train_idx, lags_sales) {
  valid_idx <- train_idx[train_idx > max(lags_sales)]
  
  xreg <- data.frame(
    TouristEvent  = df$TouristEvent[valid_idx],
    Num_Employees = df$Num_Employees[valid_idx],
    Pct_On_Sale   = df$Pct_On_Sale[valid_idx]
  )
  
  for (lag in lags_sales) {
    xreg[[paste0("Sales_lag", lag)]] <- df$Sales[valid_idx - lag]
  }
  
  list(
    xreg = as.matrix(xreg),
    y = df$Num_Customers[valid_idx]
  )
}

predict_recursive_c2 <- function(df, train_idx, test_idx, h, lags_sales) {
  
  # ---------------------------------------------------------------------------
  # MODELO SALES
  # ---------------------------------------------------------------------------
  y_sales_train <- ts(df$Sales[train_idx], frequency = 7)
  
  xreg_sales_train <- as.matrix(df[train_idx, c("TouristEvent", "Num_Employees", "Pct_On_Sale")])
  xreg_sales_test  <- as.matrix(df[test_idx,  c("TouristEvent", "Num_Employees", "Pct_On_Sale")])
  
  fit_sales <- auto.arima(y_sales_train, xreg = xreg_sales_train)
  fc_sales  <- forecast(fit_sales, h = h, xreg = xreg_sales_test)
  
  pred_sales <- as.numeric(fc_sales$mean)
  pred_sales[pred_sales < 0] <- 0
  
  # ---------------------------------------------------------------------------
  # MODELO CUSTOMERS
  # ---------------------------------------------------------------------------
  train_obj <- make_xreg_train_customers(df, train_idx, lags_sales)
  
  y_cust_train <- ts(train_obj$y, frequency = 7)
  xreg_cust_train <- train_obj$xreg
  
  fit_cust <- auto.arima(y_cust_train, xreg = xreg_cust_train)
  
  sales_ext <- c(df$Sales, pred_sales)
  
  xreg_cust_test <- matrix(NA, nrow = h, ncol = 3 + length(lags_sales))
  colnames(xreg_cust_test) <- c(
    "TouristEvent", "Num_Employees", "Pct_On_Sale",
    paste0("Sales_lag", lags_sales)
  )
  
  for (step in 1:h) {
    idx_global <- test_idx[step]
    
    xreg_cust_test[step, "TouristEvent"]  <- df$TouristEvent[idx_global]
    xreg_cust_test[step, "Num_Employees"] <- df$Num_Employees[idx_global]
    xreg_cust_test[step, "Pct_On_Sale"]   <- df$Pct_On_Sale[idx_global]
    
    for (lag in lags_sales) {
      idx_lag <- idx_global - lag
      xreg_cust_test[step, paste0("Sales_lag", lag)] <- sales_ext[idx_lag]
    }
  }
  
  fc_cust <- forecast(fit_cust, h = h, xreg = xreg_cust_test)
  pred_cust <- as.numeric(fc_cust$mean)
  
  return(list(
    pred_customers = pred_cust,
    pred_sales = pred_sales
  ))
}

# -----------------------------------------------------------------------------
# 6. BACKTESTING
# -----------------------------------------------------------------------------
cat("\n############################################################\n")
cat("# CENÁRIO 2 — ARIMAX MULTIVARIADO (EXÓGENAS + SALES)      #\n")
cat("############################################################\n")

resultados_iter  <- data.frame()
resultados_media <- data.frame()
idx_cor <- 1

for (nome in names(stores)) {
  
  cat("\n============================================================\n")
  cat("BACKTESTING -", toupper(nome), "\n")
  cat("============================================================\n")
  
  d <- build_df_c2_arimax(stores[[nome]])
  N <- nrow(d)
  
  YR <- diff(range(d$Num_Customers, na.rm = TRUE))
  if (YR == 0) YR <- 1
  
  W <- (N - H) - (RUNS - 1) * S
  W <- max(W, max(LAGS_SALES) + 10)
  
  if (W <= max(LAGS_SALES)) {
    stop(paste("Janela inicial inválida para", nome))
  }
  
  nmae_c2_v <- numeric(RUNS)
  rrse_c2_v <- numeric(RUNS)
  r2_c2_v   <- numeric(RUNS)
  
  nmae_naive_v <- numeric(RUNS)
  rrse_naive_v <- numeric(RUNS)
  r2_naive_v   <- numeric(RUNS)
  
  all_real <- c()
  all_pred_c2 <- c()
  all_pred_naive <- c()
  
  res_loja <- data.frame()
  
  for (b in 1:RUNS) {
    
    hd <- holdout(d$Num_Customers, ratio = H, mode = "incremental",
                  iter = b, window = W, increment = S)
    
    real <- d$Num_Customers[hd$ts]
    datas_teste <- d$Date[hd$ts]
    
    preds <- predict_recursive_c2(
      df = d,
      train_idx = hd$tr,
      test_idx = hd$ts,
      h = H,
      lags_sales = LAGS_SALES
    )
    
    Pred_c2 <- preds$pred_customers
    
    # --- Seasonal Naive ---
    train_y <- d$Num_Customers[hd$tr]
    Pred_naive <- seasonal_naive(train_y, h = H)
    
    # --- Pós-processamento ---
    Pred_c2[Pred_c2 < 0] <- 0
    Pred_c2[datas_teste %in% datas_fecho] <- 0
    
    Pred_naive[Pred_naive < 0] <- 0
    Pred_naive[datas_teste %in% datas_fecho] <- 0
    
    # --- Métricas C2 ---
    nmae_c2_v[b] <- mmetric(real, Pred_c2, metric = "NMAE", val = YR)
    rrse_c2_v[b] <- mmetric(real, Pred_c2, metric = "RRSE")
    r2_c2_v[b]   <- calc_r2_safe(real, Pred_c2)
    
    # --- Métricas Naive ---
    nmae_naive_v[b] <- mmetric(real, Pred_naive, metric = "NMAE", val = YR)
    rrse_naive_v[b] <- mmetric(real, Pred_naive, metric = "RRSE")
    r2_naive_v[b]   <- calc_r2_safe(real, Pred_naive)
    
    all_real <- c(all_real, real)
    all_pred_c2 <- c(all_pred_c2, Pred_c2)
    all_pred_naive <- c(all_pred_naive, Pred_naive)
    
    cat(sprintf("  iter %2d | ARIMAX C2 NMAE=%.4f RRSE=%.4f R2=%.4f | Naive NMAE=%.4f\n",
                b, nmae_c2_v[b], rrse_c2_v[b], r2_c2_v[b], nmae_naive_v[b]))
    
    linha <- data.frame(
      Loja = nome,
      Iteracao = b,
      Test_Start = as.character(datas_teste[1]),
      Test_End = as.character(datas_teste[length(datas_teste)]),
      NMAE_c2 = round(nmae_c2_v[b], 4),
      RRSE_c2 = round(rrse_c2_v[b], 4),
      R2_c2   = round(r2_c2_v[b], 4),
      NMAE_naive = round(nmae_naive_v[b], 4),
      RRSE_naive = round(rrse_naive_v[b], 4),
      R2_naive   = round(r2_naive_v[b], 4)
    )
    
    res_loja <- rbind(res_loja, linha)
  }
  
  # --- Gráfico de linhas ---
  pdf(paste0("grafico_arimax_c2_", tolower(nome), ".pdf"), width = 12, height = 5)
  
  n_pts <- length(all_real)
  yrange <- range(all_real, all_pred_c2, all_pred_naive, na.rm = TRUE)
  
  plot(1:n_pts, all_real, type = "l", col = "black", lwd = 1.5,
       ylim = yrange, xlab = "Observacao (teste acumulado)", ylab = "Num_Customers",
       main = paste("C2 -", nome, "| Real vs ARIMAX C2 vs Naive"))
  
  lines(1:n_pts, all_pred_naive, col = "gray50", lty = 2, lwd = 1.2)
  lines(1:n_pts, all_pred_c2, col = "purple", lty = 1, lwd = 1.5)
  
  for (j in 1:(RUNS - 1)) abline(v = j * H, col = "gray80", lty = 3)
  
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "ARIMAX C2"),
         col = c("black", "gray50", "purple"),
         lty = c(1, 2, 1), lwd = c(1.5, 1.2, 1.5))
  
  dev.off()
  
  # --- Boxplot ---
  pdf(paste0("boxplot_arimax_c2_", tolower(nome), ".pdf"), width = 7, height = 5)
  boxplot(nmae_c2_v, rrse_c2_v, r2_c2_v,
          names = c("NMAE", "RRSE", "R2"),
          main = paste(nome, "- ARIMAX Multi C2 - Metricas (12 iter)"),
          col = lojas_cores[idx_cor])
  dev.off()
  
  # --- Médias ---
  cat(sprintf("\n  [ARIMAX C2] Mean: NMAE=%.4f | RRSE=%.4f | R2=%.4f\n",
              mean(nmae_c2_v, na.rm = TRUE),
              mean(rrse_c2_v, na.rm = TRUE),
              mean(r2_c2_v, na.rm = TRUE)))
  cat(sprintf("  [Naive]     Mean: NMAE=%.4f | RRSE=%.4f | R2=%.4f\n",
              mean(nmae_naive_v, na.rm = TRUE),
              mean(rrse_naive_v, na.rm = TRUE),
              mean(r2_naive_v, na.rm = TRUE)))
  
  melhoria <- (mean(nmae_naive_v, na.rm = TRUE) - mean(nmae_c2_v, na.rm = TRUE)) /
    mean(nmae_naive_v, na.rm = TRUE) * 100
  cat(sprintf("  Melhoria ARIMAX C2 vs Naive: %.1f%%\n", melhoria))
  
  resultados_iter <- rbind(resultados_iter, res_loja)
  resultados_media <- rbind(resultados_media, data.frame(
    Loja = nome,
    Media_NMAE_c2 = round(mean(nmae_c2_v, na.rm = TRUE), 4),
    Media_RRSE_c2 = round(mean(rrse_c2_v, na.rm = TRUE), 4),
    Media_R2_c2   = round(mean(r2_c2_v, na.rm = TRUE), 4),
    Media_NMAE_naive = round(mean(nmae_naive_v, na.rm = TRUE), 4),
    Media_RRSE_naive = round(mean(rrse_naive_v, na.rm = TRUE), 4),
    Media_R2_naive   = round(mean(r2_naive_v, na.rm = TRUE), 4)
  ))
  
  idx_cor <- idx_cor + 1
}

# -----------------------------------------------------------------------------
# 7. RESULTADOS FINAIS
# -----------------------------------------------------------------------------
cat("\n============================================================\n")
cat("RESULTADOS FINAIS - CENÁRIO 2 (ARIMAX + SALES)\n")
cat("============================================================\n")
print(resultados_media, row.names = FALSE)

# -----------------------------------------------------------------------------
# 8. EXPORTAR CSV
# -----------------------------------------------------------------------------
write.csv(resultados_iter, "backtesting_arimax_c2_iteracoes.csv", row.names = FALSE)
write.csv(resultados_media, "backtesting_arimax_c2_media.csv", row.names = FALSE)

cat("\nCSVs guardados.\n")