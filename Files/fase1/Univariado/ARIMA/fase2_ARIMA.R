# =============================================================================
# UNIVARIADO - ARIMA
# FASE II : Backtesting growing window
# Target : Num_Customers
# Horizon: H = 7 dias
# Runs   : 12 iteracoes
# Step   : S = 7 dias
# Baseline: Seasonal Naive
# Lojas  : Baltimore, Lancaster, Philadelphia, Richmond
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
# 2. PASTA DE RESULTADOS
# -----------------------------------------------------------------------------
out_dir <- "~/TIAPOSE_projeto/tiapose2526/Files/fase1/Univariado/ARIMA"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
setwd(out_dir)

# -----------------------------------------------------------------------------
# 3. CONFIGURACAO GLOBAL
# -----------------------------------------------------------------------------
H    <- 7   # horizonte
RUNS <- 12  # numero de iteracoes
S    <- 7   # incremento da janela
K    <- 7   # sazonalidade semanal

datas_fecho <- as.Date(c(
  "2012-12-25",
  "2013-03-31",
  "2013-12-25",
  "2014-04-20"
))

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
calc_srange <- function(y) {
  srange <- diff(range(y, na.rm = TRUE))
  if (is.na(srange) || srange == 0) srange <- 1
  return(srange)
}

ajustar_arima <- function(y, frequency = 7) {
  ts_train <- ts(y, frequency = frequency)
  suppressWarnings(auto.arima(ts_train, seasonal = TRUE))
}

prever_arima <- function(modelo, h = 7) {
  as.numeric(forecast(modelo, h = h)$mean)
}

prever_snaive <- function(y, h = 7, frequency = 7) {
  ts_train <- ts(y, frequency = frequency)
  as.numeric(forecast(snaive(ts_train, h = h), h = h)$mean)
}

ajustar_previsoes <- function(pred, datas_teste, datas_fecho) {
  pred <- as.numeric(pred)
  pred[pred < 0] <- 0
  pred[datas_teste %in% datas_fecho] <- 0
  return(pred)
}

calcular_metricas <- function(real, pred, y_range) {
  c(
    MAE  = mmetric(real, pred, metric = "MAE"),
    NMAE = mmetric(real, pred, metric = "NMAE", val = y_range),
    RMSE = mmetric(real, pred, metric = "RMSE"),
    RRSE = mmetric(real, pred, metric = "RRSE"),
    R2   = mmetric(real, pred, metric = "R22")
  )
}

# -----------------------------------------------------------------------------
# 5. FASE II - BACKTESTING GROWING WINDOW
# -----------------------------------------------------------------------------
cat("\n=============================================================\n")
cat(" FASE II - Backtesting growing window (12 iteracoes)\n")
cat("=============================================================\n")

resultados_fase2 <- list()
resultados_media <- data.frame()

idx_cor <- 1

for (nome in names(stores)) {
  
  cat("\n--- Loja:", nome, "---\n")
  
  d <- stores[[nome]]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  
  y <- d$Num_Customers
  datas <- d$Date
  L <- length(y)
  
  # Janela inicial para growing window:
  W <- (L - H) - (RUNS - 1) * S
  
  if (W <= 0) {
    warning(paste("Loja", nome, "nao tem dados suficientes para", RUNS, "iteracoes."))
    next
  }
  
  y_range <- calc_srange(y)
  
  cat(sprintf("  Tamanho serie: %d | Janela inicial treino: %d\n", L, W))
  
  mae_arima_v  <- rep(NA, RUNS)
  nmae_arima_v <- rep(NA, RUNS)
  rmse_arima_v <- rep(NA, RUNS)
  rrse_arima_v <- rep(NA, RUNS)
  r2_arima_v   <- rep(NA, RUNS)
  
  mae_snaive_v  <- rep(NA, RUNS)
  nmae_snaive_v <- rep(NA, RUNS)
  rmse_snaive_v <- rep(NA, RUNS)
  rrse_snaive_v <- rep(NA, RUNS)
  r2_snaive_v   <- rep(NA, RUNS)
  
  all_real       <- c()
  all_pred_arima <- c()
  all_pred_snaive <- c()
  
  res_loja <- data.frame()
  
  for (b in 1:RUNS) {
    
    hd <- holdout(y, ratio = H, mode = "incremental", iter = b, window = W, increment = S)
    
    train <- y[hd$tr]
    real  <- y[hd$ts]
    datas_teste <- datas[hd$ts]
    
    # ---- ARIMA ----
    modelo_arima <- ajustar_arima(train, frequency = K)
    pred_arima   <- prever_arima(modelo_arima, h = H)
    pred_arima   <- ajustar_previsoes(pred_arima, datas_teste, datas_fecho)
    met_arima    <- calcular_metricas(real, pred_arima, y_range)
    
    # ---- Seasonal Naive ----
    pred_snaive <- prever_snaive(train, h = H, frequency = K)
    pred_snaive <- ajustar_previsoes(pred_snaive, datas_teste, datas_fecho)
    met_snaive  <- calcular_metricas(real, pred_snaive, y_range)
    
    mae_arima_v[b]  <- met_arima["MAE"]
    nmae_arima_v[b] <- met_arima["NMAE"]
    rmse_arima_v[b] <- met_arima["RMSE"]
    rrse_arima_v[b] <- met_arima["RRSE"]
    r2_arima_v[b]   <- met_arima["R2"]
    
    mae_snaive_v[b]  <- met_snaive["MAE"]
    nmae_snaive_v[b] <- met_snaive["NMAE"]
    rmse_snaive_v[b] <- met_snaive["RMSE"]
    rrse_snaive_v[b] <- met_snaive["RRSE"]
    r2_snaive_v[b]   <- met_snaive["R2"]
    
    all_real        <- c(all_real, real)
    all_pred_arima  <- c(all_pred_arima, pred_arima)
    all_pred_snaive <- c(all_pred_snaive, pred_snaive)
    
    ord <- arimaorder(modelo_arima)
    modelo_txt <- paste0("ARIMA(", ord[1], ",", ord[2], ",", ord[3], ")")
    
    cat(sprintf(
      "  iter %2d | ARIMA NMAE=%.4f RRSE=%.4f R2=%.4f | SNaive NMAE=%.4f\n",
      b, met_arima["NMAE"], met_arima["RRSE"], met_arima["R2"], met_snaive["NMAE"]
    ))
    
    linha <- data.frame(
      Loja         = nome,
      Iteracao     = b,
      Modelo_ARIMA = modelo_txt,
      Train_Start  = as.character(datas[min(hd$tr)]),
      Train_End    = as.character(datas[max(hd$tr)]),
      Train_Size   = length(hd$tr),
      Test_Start   = as.character(datas[min(hd$ts)]),
      Test_End     = as.character(datas[max(hd$ts)]),
      Test_Size    = length(hd$ts),
      MAE_ARIMA    = round(met_arima["MAE"], 4),
      NMAE_ARIMA   = round(met_arima["NMAE"], 4),
      RMSE_ARIMA   = round(met_arima["RMSE"], 4),
      RRSE_ARIMA   = round(met_arima["RRSE"], 4),
      R2_ARIMA     = round(met_arima["R2"], 4),
      MAE_SNaive   = round(met_snaive["MAE"], 4),
      NMAE_SNaive  = round(met_snaive["NMAE"], 4),
      RMSE_SNaive  = round(met_snaive["RMSE"], 4),
      RRSE_SNaive  = round(met_snaive["RRSE"], 4),
      R2_SNaive    = round(met_snaive["R2"], 4)
    )
    
    res_loja <- rbind(res_loja, linha)
  }
  
  # ---- Grafico acumulado ----
  pdf(paste0("grafico_fase2_", tolower(nome), ".pdf"), width = 10, height = 6)
  
  n_pts <- length(all_real)
  yrange <- range(all_real, all_pred_arima, all_pred_snaive, na.rm = TRUE)
  
  plot(1:n_pts, all_real, type = "l", col = "black", lwd = 1.5,
       ylim = yrange,
       xlab = "Observacao (teste acumulado)",
       ylab = "Num_Customers",
       main = paste("Fase II -", nome, "| ARIMA vs Seasonal Naive"))
  lines(1:n_pts, all_pred_snaive, col = "gray50", lty = 2, lwd = 1.2)
  lines(1:n_pts, all_pred_arima, col = "blue", lty = 1, lwd = 1.5)
  for (j in 1:(RUNS - 1)) abline(v = j * H, col = "gray80", lty = 3)
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "ARIMA"),
         col = c("black", "gray50", "blue"),
         lty = c(1, 2, 1), lwd = c(1.5, 1.2, 1.5))
  
  dev.off()
  
  # ---- Boxplot ARIMA ----
  pdf(paste0("boxplot_arima_", tolower(nome), ".pdf"), width = 7, height = 5)
  boxplot(
    nmae_arima_v, rrse_arima_v, r2_arima_v,
    names = c("NMAE", "RRSE", "R2"),
    main = paste(nome, "- ARIMA - Distribuicao das metricas"),
    col = lojas_cores[idx_cor]
  )
  dev.off()
  
  # ---- Medias ----
  cat(sprintf(
    "\n  [ARIMA]  Mean.MAE=%.4f | Mean.NMAE=%.4f | Mean.RMSE=%.4f | Mean.RRSE=%.4f | Mean.R2=%.4f\n",
    mean(mae_arima_v, na.rm = TRUE),
    mean(nmae_arima_v, na.rm = TRUE),
    mean(rmse_arima_v, na.rm = TRUE),
    mean(rrse_arima_v, na.rm = TRUE),
    mean(r2_arima_v, na.rm = TRUE)
  ))
  
  cat(sprintf(
    "  [SNaive] Mean.MAE=%.4f | Mean.NMAE=%.4f | Mean.RMSE=%.4f | Mean.RRSE=%.4f | Mean.R2=%.4f\n",
    mean(mae_snaive_v, na.rm = TRUE),
    mean(nmae_snaive_v, na.rm = TRUE),
    mean(rmse_snaive_v, na.rm = TRUE),
    mean(rrse_snaive_v, na.rm = TRUE),
    mean(r2_snaive_v, na.rm = TRUE)
  ))
  
  melhoria_nmae <- (
    mean(nmae_snaive_v, na.rm = TRUE) - mean(nmae_arima_v, na.rm = TRUE)
  ) / mean(nmae_snaive_v, na.rm = TRUE) * 100
  
  cat(sprintf("  Melhoria ARIMA vs SNaive (NMAE): %.1f%%\n", melhoria_nmae))
  
  resultados_fase2[[nome]] <- res_loja
  
  med_loja <- data.frame(
    Loja              = nome,
    Target            = "Num_Customers",
    Metodo            = "Growing Window",
    Iteracoes         = RUNS,
    Media_MAE_ARIMA   = round(mean(mae_arima_v, na.rm = TRUE), 4),
    Media_NMAE_ARIMA  = round(mean(nmae_arima_v, na.rm = TRUE), 4),
    Media_RMSE_ARIMA  = round(mean(rmse_arima_v, na.rm = TRUE), 4),
    Media_RRSE_ARIMA  = round(mean(rrse_arima_v, na.rm = TRUE), 4),
    Media_R2_ARIMA    = round(mean(r2_arima_v, na.rm = TRUE), 4),
    Media_MAE_SNaive  = round(mean(mae_snaive_v, na.rm = TRUE), 4),
    Media_NMAE_SNaive = round(mean(nmae_snaive_v, na.rm = TRUE), 4),
    Media_RMSE_SNaive = round(mean(rmse_snaive_v, na.rm = TRUE), 4),
    Media_RRSE_SNaive = round(mean(rrse_snaive_v, na.rm = TRUE), 4),
    Media_R2_SNaive   = round(mean(r2_snaive_v, na.rm = TRUE), 4),
    Melhoria_NMAE_pct = round(melhoria_nmae, 2)
  )
  
  resultados_media <- rbind(resultados_media, med_loja)
  idx_cor <- idx_cor + 1
}

# -----------------------------------------------------------------------------
# 6. RESULTADOS FINAIS
# -----------------------------------------------------------------------------
cat("\n============================================================\n")
cat("RESULTADOS FINAIS - MEDIA DAS METRICAS POR LOJA\n")
cat("============================================================\n")
print(resultados_media, row.names = FALSE)

# -----------------------------------------------------------------------------
# 7. EXPORTAR RESULTADOS
# -----------------------------------------------------------------------------
df_fase2 <- do.call(rbind, resultados_fase2)

write.csv(df_fase2, "backtesting_arima_iteracoes.csv", row.names = FALSE)
write.csv(resultados_media, "backtesting_arima_media_lojas.csv", row.names = FALSE)

cat("\nFase II concluida. CSVs e PDFs guardados em:", out_dir, "\n")