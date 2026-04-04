# =============================================================================
# UNIVARIADO - ARIMA
# Target  : Num_Customers
# Horizon : H = 7 dias
# Baseline: Seasonal Naive
# Fase I  : Holdout simples (treino = tudo menos os ultimos 7 dias)
# Fase II : Backtesting growing window, 12 iteracoes
# Lojas   : Baltimore, Lancaster, Philadelphia, Richmond
# =============================================================================

options(rgl.useNULL = TRUE)

library(forecast)
library(rminer)

# -----------------------------------------------------------------------------
# 1. CARREGAR DADOS TRATADOS
# -----------------------------------------------------------------------------
setwd("~/TIAPOSE_projeto/tiapose2526/Files/csv")
source("~/TIAPOSE_projeto/tiapose2526/Files/tratamentoDeDados.R")

# -----------------------------------------------------------------------------
# 2. CRIAR PASTA DE RESULTADOS
# -----------------------------------------------------------------------------
dir.create("~/TIAPOSE_projeto/tiapose2526/Files/univariado/ARIMA", showWarnings = FALSE, recursive = TRUE)
setwd("~/TIAPOSE_projeto/tiapose2526/Files/univariado/ARIMA")

# -----------------------------------------------------------------------------
# 3. CONFIGURACAO GLOBAL
# -----------------------------------------------------------------------------
H    <- 7
RUNS <- 12
S    <- 7

# Datas em que a loja esta fechada
datas_fecho <- as.Date(c("2012-12-25", "2013-12-25", "2013-03-31", "2014-04-20"))

# Lojas
stores <- list(
  Baltimore    = baltimore,
  Lancaster    = lancaster,
  Philadelphia = philadelphia,
  Richmond     = richmond
)

lojas_cores <- c("steelblue", "tomato", "seagreen", "hotpink")

# -----------------------------------------------------------------------------
# 4. FUNCOES AUXILIARES
# -----------------------------------------------------------------------------

seasonal_naive <- function(train, h = 7) {
  pred <- tail(train, h)
  return(as.numeric(pred))
}

calc_r2 <- function(real, pred) {
  denom <- sum((real - mean(real))^2)
  if (denom == 0) return(NA)
  1 - (sum((real - pred)^2) / denom)
}

calc_srange <- function(y) {
  srange <- diff(range(y, na.rm = TRUE))
  if (is.na(srange) || srange == 0) srange <- 1
  return(srange)
}

ajustar_arima <- function(train) {
  ts_train <- ts(train, frequency = 7)
  auto.arima(ts_train)
}

prever_arima <- function(modelo, h = 7) {
  fc <- forecast(modelo, h = h)
  as.numeric(fc$mean[1:h])
}

# =============================================================================
# FASE I - HOLDOUT SIMPLES
# =============================================================================
cat("\n=============================================================\n")
cat("  FASE I - Holdout simples (ultimos 7 dias como teste)      \n")
cat("=============================================================\n")

pdf("graficos_arima_fase1.pdf", width = 10, height = 6)

resultados_fase1 <- list()
previsoes_fase1 <- list()

for (nome in names(stores)) {
  cat("\n--- Loja:", nome, "---\n")
  
  d <- stores[[nome]]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  
  TS <- d$Num_Customers
  datas <- d$Date
  L <- length(TS)
  
  if (L <= H) {
    warning(paste("Loja", nome, "nao tem observacoes suficientes."))
    next
  }
  
  YR <- calc_srange(TS)
  
  # Indices treino / teste
  TR_idx <- 1:(L - H)
  TS_idx <- (L - H + 1):L
  
  train <- TS[TR_idx]
  real <- TS[TS_idx]
  datas_teste <- datas[TS_idx]
  
  # ---- Seasonal Naive ----
  pred_naive <- seasonal_naive(train, h = H)
  pred_naive[pred_naive < 0] <- 0
  pred_naive[datas_teste %in% datas_fecho] <- 0
  
  mae_naive  <- mmetric(real, pred_naive, metric = "MAE")
  nmae_naive <- mmetric(real, pred_naive, metric = "NMAE", val = YR)
  rmse_naive <- mmetric(real, pred_naive, metric = "RMSE")
  rrse_naive <- mmetric(real, pred_naive, metric = "RRSE")
  r2_naive   <- calc_r2(real, pred_naive)
  
  cat(sprintf("  Seasonal Naive | MAE=%.4f | NMAE=%.4f | RMSE=%.4f | RRSE=%.4f | R2=%.4f\n",
              mae_naive, nmae_naive, rmse_naive, rrse_naive, r2_naive))
  
  # ---- ARIMA ----
  modelo_arima <- ajustar_arima(train)
  pred_arima <- prever_arima(modelo_arima, h = H)
  pred_arima[pred_arima < 0] <- 0
  pred_arima[datas_teste %in% datas_fecho] <- 0
  
  mae_arima  <- mmetric(real, pred_arima, metric = "MAE")
  nmae_arima <- mmetric(real, pred_arima, metric = "NMAE", val = YR)
  rmse_arima <- mmetric(real, pred_arima, metric = "RMSE")
  rrse_arima <- mmetric(real, pred_arima, metric = "RRSE")
  r2_arima   <- calc_r2(real, pred_arima)
  
  ord <- arimaorder(modelo_arima)
  modelo_txt <- paste0("ARIMA(", ord[1], ",", ord[2], ",", ord[3], ")")
  
  cat(sprintf("  %-15s | MAE=%.4f | NMAE=%.4f | RMSE=%.4f | RRSE=%.4f | R2=%.4f\n",
              modelo_txt, mae_arima, nmae_arima, rmse_arima, rrse_arima, r2_arima))
  
  # ---- Grafico Fase I ----
  yrange <- range(real, pred_naive, pred_arima, na.rm = TRUE)
  
  plot(1:H, real, type = "b", pch = 19, col = "black",
       ylim = yrange,
       xlab = "Dia (horizonte)",
       ylab = "Num_Customers",
       main = paste("Fase I -", nome, "| ARIMA vs Seasonal Naive"))
  lines(1:H, pred_naive, type = "b", pch = 2, col = "gray50", lty = 2)
  lines(1:H, pred_arima, type = "b", pch = 17, col = "blue", lty = 1)
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "ARIMA"),
         col = c("black", "gray50", "blue"),
         pch = c(19, 2, 17), lty = c(1, 2, 1))
  
  # ---- Guardar resultados ----
  resultados_fase1[[nome]] <- data.frame(
    Loja   = nome,
    Metodo = c("Seasonal_Naive", "ARIMA"),
    MAE    = c(mae_naive, mae_arima),
    NMAE   = c(nmae_naive, nmae_arima),
    RMSE   = c(rmse_naive, rmse_arima),
    RRSE   = c(rrse_naive, rrse_arima),
    R2     = c(r2_naive, r2_arima)
  )
  
  previsoes_fase1[[nome]] <- data.frame(
    Loja = nome,
    Date = datas_teste,
    Real = real,
    Previsto_ARIMA = round(pred_arima, 2),
    Previsto_SNaive = round(pred_naive, 2),
    Erro_Absoluto_ARIMA = round(abs(real - pred_arima), 2),
    Erro_Absoluto_SNaive = round(abs(real - pred_naive), 2)
  )
}

dev.off()

df_fase1 <- do.call(rbind, resultados_fase1)
cat("\n\n====== TABELA FASE I ======\n")
print(df_fase1, row.names = FALSE, digits = 4)
write.csv(df_fase1, "resultados_arima_fase1.csv", row.names = FALSE)

for (nome in names(previsoes_fase1)) {
  write.csv(previsoes_fase1[[nome]],
            paste0("arima_previsoes_", tolower(nome), ".csv"),
            row.names = FALSE)
}

# =============================================================================
# FASE II - BACKTESTING (GROWING WINDOW, 12 ITERACOES)
# =============================================================================
cat("\n\n=============================================================\n")
cat("  FASE II - Backtesting growing window (12 iteracoes)       \n")
cat("=============================================================\n")

resultados_fase2 <- list()
resultados_media <- data.frame()
idx_cor <- 1

for (nome in names(stores)) {
  cat("\n--- Loja:", nome, "---\n")
  
  d <- stores[[nome]]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  
  TS <- d$Num_Customers
  datas <- d$Date
  L <- length(TS)
  
  if (L <= (RUNS * S + H)) {
    warning(paste("Loja", nome, "pode nao ter observacoes suficientes para 12 iteracoes."))
  }
  
  YR <- calc_srange(TS)
  
  # Janela inicial de treino
  W2 <- (L - H) - (RUNS - 1) * S
  
  cat(sprintf("  Tamanho serie: %d | Janela inicial treino: %d\n", L, W2))
  
  mae_arima_v  <- numeric(RUNS)
  nmae_arima_v <- numeric(RUNS)
  rmse_arima_v <- numeric(RUNS)
  rrse_arima_v <- numeric(RUNS)
  r2_arima_v   <- numeric(RUNS)
  
  mae_naive_v  <- numeric(RUNS)
  nmae_naive_v <- numeric(RUNS)
  rmse_naive_v <- numeric(RUNS)
  rrse_naive_v <- numeric(RUNS)
  r2_naive_v   <- numeric(RUNS)
  
  all_pred_arima <- c()
  all_pred_naive <- c()
  all_real <- c()
  
  res_loja <- data.frame()
  
  for (b in 1:RUNS) {
    train_end <- W2 + (b - 1) * S
    test_start <- train_end + 1
    test_end <- test_start + H - 1
    
    if (test_end > L) {
      warning(paste("Iteracao", b, "ultrapassa o tamanho da serie para", nome))
      next
    }
    
    train <- TS[1:train_end]
    real <- TS[test_start:test_end]
    datas_teste <- datas[test_start:test_end]
    
    # ---- ARIMA ----
    modelo_arima <- ajustar_arima(train)
    pred_arima <- prever_arima(modelo_arima, h = H)
    pred_arima[pred_arima < 0] <- 0
    pred_arima[datas_teste %in% datas_fecho] <- 0
    
    # ---- Seasonal Naive ----
    pred_naive <- seasonal_naive(train, h = H)
    pred_naive[pred_naive < 0] <- 0
    pred_naive[datas_teste %in% datas_fecho] <- 0
    
    # ---- Metricas ARIMA ----
    mae_arima_v[b]  <- mmetric(real, pred_arima, metric = "MAE")
    nmae_arima_v[b] <- mmetric(real, pred_arima, metric = "NMAE", val = YR)
    rmse_arima_v[b] <- mmetric(real, pred_arima, metric = "RMSE")
    rrse_arima_v[b] <- mmetric(real, pred_arima, metric = "RRSE")
    r2_arima_v[b]   <- calc_r2(real, pred_arima)
    
    # ---- Metricas Naive ----
    mae_naive_v[b]  <- mmetric(real, pred_naive, metric = "MAE")
    nmae_naive_v[b] <- mmetric(real, pred_naive, metric = "NMAE", val = YR)
    rmse_naive_v[b] <- mmetric(real, pred_naive, metric = "RMSE")
    rrse_naive_v[b] <- mmetric(real, pred_naive, metric = "RRSE")
    r2_naive_v[b]   <- calc_r2(real, pred_naive)
    
    all_pred_arima <- c(all_pred_arima, pred_arima)
    all_pred_naive <- c(all_pred_naive, pred_naive)
    all_real <- c(all_real, real)
    
    cat(sprintf("  iter %2d | ARIMA NMAE=%.4f RRSE=%.4f R2=%.4f | Naive NMAE=%.4f\n",
                b, nmae_arima_v[b], rrse_arima_v[b], r2_arima_v[b], nmae_naive_v[b]))
    
    ord <- arimaorder(modelo_arima)
    modelo_txt <- paste0("ARIMA(", ord[1], ",", ord[2], ",", ord[3], ")")
    
    linha <- data.frame(
      Loja = nome,
      Iteracao = b,
      Modelo = modelo_txt,
      Train_Start = as.character(datas[1]),
      Train_End = as.character(datas[train_end]),
      Train_Size = train_end,
      Test_Start = as.character(datas_teste[1]),
      Test_End = as.character(datas_teste[length(datas_teste)]),
      Test_Size = length(real),
      MAE_ARIMA = round(mae_arima_v[b], 4),
      NMAE_ARIMA = round(nmae_arima_v[b], 4),
      RMSE_ARIMA = round(rmse_arima_v[b], 4),
      RRSE_ARIMA = round(rrse_arima_v[b], 4),
      R2_ARIMA = round(r2_arima_v[b], 4),
      MAE_Naive = round(mae_naive_v[b], 4),
      NMAE_Naive = round(nmae_naive_v[b], 4),
      RMSE_Naive = round(rmse_naive_v[b], 4),
      RRSE_Naive = round(rrse_naive_v[b], 4),
      R2_Naive = round(r2_naive_v[b], 4)
    )
    
    res_loja <- rbind(res_loja, linha)
  }
  
  # ---- Grafico predicted vs actual ----
  pdf(paste0("grafico_arima_fase2_", tolower(nome), ".pdf"), width = 10, height = 6)
  n_pts <- length(all_real)
  yrange <- range(all_real, all_pred_arima, all_pred_naive, na.rm = TRUE)
  
  plot(1:n_pts, all_real, type = "l", col = "black", lwd = 1.5,
       ylim = yrange,
       xlab = "Observacao (teste acumulado)",
       ylab = "Num_Customers",
       main = paste("Fase II -", nome, "| ARIMA vs Seasonal Naive"))
  lines(1:n_pts, all_pred_naive, col = "gray50", lty = 2, lwd = 1.2)
  lines(1:n_pts, all_pred_arima, col = "blue", lty = 1, lwd = 1.5)
  for (j in 1:(RUNS - 1)) abline(v = j * H, col = "gray80", lty = 3)
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "ARIMA"),
         col = c("black", "gray50", "blue"),
         lty = c(1, 2, 1), lwd = c(1.5, 1.2, 1.5))
  dev.off()
  
  # ---- Boxplot ARIMA ----
  pdf(paste0("boxplot_arima_", tolower(nome), ".pdf"), width = 7, height = 5)
  boxplot(nmae_arima_v, rrse_arima_v, r2_arima_v,
          names = c("NMAE", "RRSE", "R2"),
          main = paste(nome, "- ARIMA - Distribuicao das Metricas (12 iteracoes)"),
          col = lojas_cores[idx_cor])
  dev.off()
  
  # ---- Medias ----
  cat(sprintf("\n  [ARIMA] Mean.MAE=%.4f | Mean.NMAE=%.4f | Mean.RMSE=%.4f | Mean.RRSE=%.4f | Mean.R2=%.4f\n",
              mean(mae_arima_v, na.rm = TRUE), mean(nmae_arima_v, na.rm = TRUE),
              mean(rmse_arima_v, na.rm = TRUE), mean(rrse_arima_v, na.rm = TRUE),
              mean(r2_arima_v, na.rm = TRUE)))
  
  cat(sprintf("  [Naive] Mean.MAE=%.4f | Mean.NMAE=%.4f | Mean.RMSE=%.4f | Mean.RRSE=%.4f | Mean.R2=%.4f\n",
              mean(mae_naive_v, na.rm = TRUE), mean(nmae_naive_v, na.rm = TRUE),
              mean(rmse_naive_v, na.rm = TRUE), mean(rrse_naive_v, na.rm = TRUE),
              mean(r2_naive_v, na.rm = TRUE)))
  
  melhoria <- (mean(nmae_naive_v, na.rm = TRUE) - mean(nmae_arima_v, na.rm = TRUE)) /
    mean(nmae_naive_v, na.rm = TRUE) * 100
  
  cat(sprintf("  Melhoria ARIMA vs Naive: %.1f%%\n", melhoria))
  
  resultados_fase2[[nome]] <- res_loja
  
  med_loja <- data.frame(
    Loja = nome,
    Target = "Num_Customers",
    Metodo = "Growing Window",
    Iteracoes = RUNS,
    Media_MAE_ARIMA = round(mean(mae_arima_v, na.rm = TRUE), 4),
    Media_NMAE_ARIMA = round(mean(nmae_arima_v, na.rm = TRUE), 4),
    Media_RMSE_ARIMA = round(mean(rmse_arima_v, na.rm = TRUE), 4),
    Media_RRSE_ARIMA = round(mean(rrse_arima_v, na.rm = TRUE), 4),
    Media_R2_ARIMA = round(mean(r2_arima_v, na.rm = TRUE), 4),
    Media_MAE_Naive = round(mean(mae_naive_v, na.rm = TRUE), 4),
    Media_NMAE_Naive = round(mean(nmae_naive_v, na.rm = TRUE), 4),
    Media_RMSE_Naive = round(mean(rmse_naive_v, na.rm = TRUE), 4),
    Media_RRSE_Naive = round(mean(rrse_naive_v, na.rm = TRUE), 4),
    Media_R2_Naive = round(mean(r2_naive_v, na.rm = TRUE), 4)
  )
  
  resultados_media <- rbind(resultados_media, med_loja)
  idx_cor <- idx_cor + 1
}

# =============================================================================
# RESULTADOS FINAIS
# =============================================================================
cat("\n============================================================\n")
cat("RESULTADOS FINAIS - MEDIA DAS METRICAS POR LOJA\n")
cat("============================================================\n")
print(resultados_media, row.names = FALSE)

# =============================================================================
# GUARDAR CSV
# =============================================================================
df_fase2 <- do.call(rbind, resultados_fase2)

write.csv(df_fase2, "backtesting_arima_iteracoes.csv", row.names = FALSE)
write.csv(resultados_media, "backtesting_arima_media_lojas.csv", row.names = FALSE)

cat("\nCSVs e PDFs guardados na pasta ARIMA.\n")