# =============================================================================
# EDU — RF Multivariado — Cenário 2 (exógenas + Sales endógena)
# Target: Num_Customers, H=7
# Endógenas: Num_Customers + Sales (previsão recursive via mfit/lforecastm)
# Exógenas: TouristEvent + Num_Employees + Pct_On_Sale
# Lags NC: c(1:3,7,28) | Lags Sales: c(1,7)
# =============================================================================

options(rgl.useNULL = TRUE)
library(rminer)
library(vars)
library(forecast)

# --- Carregar dados tratados + funções multivariadas do professor ---
setwd("~/TIAPOSE2526/Files/csv")
source("~/TIAPOSE2526/Files/tratamentoDeDados.R")
source("~/TIAPOSE2526/Files/fase1/Multivariado/multi-utils.R")

# --- Pasta de resultados ---
dir.create("~/TIAPOSE2526/Files/fase1/RF_multi", showWarnings=FALSE)
setwd("~/TIAPOSE2526/Files/fase1/Multivariado/RF/RF_multi_cen2")

# --- Configuração ---
H      <- 7
RUNS   <- 12
S      <- 7
LAGS_NC    <- c(1:3, 7, 28)
LAGS_SALES <- c(1, 7)

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

# --- VINP: lags para cada modelo ---
# VINP[[1]]: prever NC  → lags NC c(1:3,7,28), lags Sales c(1,7)
# VINP[[2]]: prever Sales → lags NC c(1,7), lags Sales c(1:3,7,28)
VINP <- vector("list", length = 2)
VINP[[1]] <- list(LAGS_NC, LAGS_SALES)
VINP[[2]] <- list(c(1, 7), c(1:3, 7, 28))

max_lag <- max(unlist(VINP))  # 28

# =============================================================================
# BACKTESTING
# =============================================================================
cat("\n############################################################\n")
cat("# CENÁRIO 2 — RF MULTIVARIADO (EXÓGENAS + SALES)          #\n")
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
  
  nc <- d$Num_Customers
  sa <- d$Sales
  L  <- length(nc)
  
  YR <- diff(range(nc))
  if (YR == 0) YR <- 1
  
  # Exógenas
  exogen_full <- cbind(
    TouristEvent  = as.numeric(d$TouristEvent == "Yes"),
    Num_Employees = d$Num_Employees,
    Pct_On_Sale   = d$Pct_On_Sale
  )
  
  # Datas apos perder max_lag linhas
  datas_mfit <- d$Date[(max_lag + 1):L]
  ND <- length(datas_mfit)
  
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
    # Indices no espaço "pos-lag"
    end_tr   <- W + (b - 1) * S
    start_ts <- end_tr + 1
    end_ts   <- start_ts + H - 1
    
    # Indices no vetor original
    tr_orig <- 1:(end_tr + max_lag)
    ts_orig <- (start_ts + max_lag):(end_ts + max_lag)
    
    real <- nc[ts_orig]
    datas_teste <- d$Date[ts_orig]
    
    # --- Treinar com mfit ---
    mtr_train <- cbind(nc[tr_orig], sa[tr_orig])
    exo_train <- exogen_full[tr_orig, ]
    exo_test  <- exogen_full[ts_orig, ]
    
    MRF <- mfit(mtr_train, "randomForest", VINP, exogen = exo_train)
    
    # --- Previsão recursive (lforecastm trata automaticamente) ---
    Pred_all <- lforecastm(MRF, h = H, exogen = exo_test)
    Pred_rf <- Pred_all[[1]]  # só Num_Customers
    
    # --- Seasonal Naive ---
    Pred_naive <- seasonal_naive(nc[tr_orig], h = H)
    
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
  
  # --- Gráfico de linhas: real vs RF vs Naive ---
  pdf(paste0("grafico_rf_c2_", tolower(nome), ".pdf"), width = 12, height = 5)
  n_pts <- length(all_real)
  yrange <- range(all_real, all_pred_rf, all_pred_naive)
  plot(1:n_pts, all_real, type = "l", col = "black", lwd = 1.5,
       ylim = yrange, xlab = "Observacao (teste acumulado)", ylab = "Num_Customers",
       main = paste("C2 -", nome, "| Real vs RF Multi (Sales) vs Naive"))
  lines(1:n_pts, all_pred_naive, col = "gray50", lty = 2, lwd = 1.2)
  lines(1:n_pts, all_pred_rf,    col = "purple",  lty = 1, lwd = 1.5)
  for (j in 1:(RUNS - 1)) abline(v = j * H, col = "gray80", lty = 3)
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "RF Multi C2"),
         col = c("black", "gray50", "purple"),
         lty = c(1, 2, 1), lwd = c(1.5, 1.2, 1.5))
  dev.off()
  
  # --- Boxplot ---
  pdf(paste0("boxplot_rf_c2_", tolower(nome), ".pdf"), width = 7, height = 5)
  boxplot(nmae_rf_v, rrse_rf_v, r2_rf_v,
          names = c("NMAE", "RRSE", "R2"),
          main = paste(nome, "- RF Multi C2 - Metricas (12 iter)"),
          col = lojas_cores[idx_cor])
  dev.off()
  
  # --- Médias ---
  cat(sprintf("\n  [RF C2] Mean: NMAE=%.4f | RRSE=%.4f | R2=%.4f\n",
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
cat("RESULTADOS FINAIS - CENÁRIO 2\n")
cat("============================================================\n")
print(resultados_media, row.names = FALSE)

# --- CSV ---
write.csv(resultados_iter, "backtesting_rf_c2_iteracoes.csv", row.names = FALSE)
write.csv(resultados_media, "backtesting_rf_c2_media.csv", row.names = FALSE)
cat("\nCSVs guardados.\n")

# ============================================================
# PREVISÃO FINAL — próximos 7 dias (para PREV da otimização)
# ============================================================
cat("\n=== PREV — RF Multivariado C2 ===\n")

prev_final <- list()

for (nome in names(stores)) {
  d <- stores[[nome]]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  
  nc <- d$Num_Customers
  sa <- d$Sales
  L  <- length(nc)
  
  exogen_full <- cbind(
    TouristEvent  = as.numeric(d$TouristEvent == "Yes"),
    Num_Employees = d$Num_Employees,
    Pct_On_Sale   = d$Pct_On_Sale
  )
  
  # Treinar com TODOS os dados
  mtr_all <- cbind(nc, sa)
  MRF_final <- mfit(mtr_all, "randomForest", VINP, exogen = exogen_full)
  
  # Próximos 7 dias — sem evento turístico, médias das exógenas
  exo_fut <- matrix(
    c(rep(0, 7),                        # TouristEvent
      rep(round(mean(d$Num_Employees)), 7), # Num_Employees
      rep(round(mean(d$Pct_On_Sale), 2), 7) # Pct_On_Sale
    ),
    nrow = 7, ncol = 3,
    dimnames = list(NULL, c("TouristEvent", "Num_Employees", "Pct_On_Sale"))
  )
  
  Pred_all  <- lforecastm(MRF_final, h = 7, exogen = exo_fut)
  Pred_7    <- round(pmax(Pred_all[[1]], 0))
  
  prev_final[[nome]] <- Pred_7
  cat(paste0("prev_", tolower(nome), " <- c(", paste(Pred_7, collapse = ", "), ")\n"))
}