# =============================================================================
# JOÃO — Univariado ML: MLP Ensemble (mlpe) via rminer
# Target  : Num_Customers, H = 7 dias à frente
# Baseline: Seasonal Naive (repete a semana anterior)
# Fase I  : holdout simples (treino = tudo menos últimos 7 dias)
<<<<<<< HEAD
# Fase II : backtesting growing window, 12 iterações, guardar CSV
# 4 lojas : Philadelphia, Baltimore, Richmond, Lancaster
=======
# Fase II : backtesting growing window, 12 iterações
# 4 lojas : Baltimore, Lancaster, Philadelphia, Richmond
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
# =============================================================================

options(rgl.useNULL = TRUE)
library(rminer)

<<<<<<< HEAD
# --- Tratamento de dados ------------------------------------------------------
source("~/Desktop/Mestrado/TIAPOSE/project/Files/tratamentoDeDados.R")
=======
# --- Carregar dados tratados (fonte unica para todo o grupo) ---
setwd("~/TIAPOSE2526/Files/csv")
source("~/TIAPOSE2526/Files/tratamentoDeDados.R")
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f

# --- Criar pasta e definir como destino dos resultados ---
dir.create("~/TIAPOSE2526/Files/fase1/MLPE", showWarnings=FALSE)
setwd("~/TIAPOSE2526/Files/fase1/MLPE")

# --- Configuração global ---
H      <- 7
RUNS   <- 12
S      <- 7
LAGS   <- c(1:3, 7, 28)   # lags sugeridos pelo professor

# --- Datas em que a loja esta fechada (Natal e Pascoa) ---
datas_fecho <- as.Date(c("2012-12-25","2013-12-25","2013-03-31","2014-04-20"))

# --- Lojas ---
stores <- list(
  Baltimore    = baltimore,
  Lancaster    = lancaster,
  Philadelphia = philadelphia,
  Richmond     = richmond
)
lojas_cores <- c("steelblue", "tomato", "seagreen", "hotpink")

# --- Seasonal Naive ---
seasonal_naive <- function(train, h = 7) {
  tail(train, h)
}

# =============================================================================
# FASE I — Holdout simples
# =============================================================================
cat("\n=============================================================\n")
cat("  FASE I — Holdout simples (ultimos 7 dias como teste)      \n")
cat("=============================================================\n")

pdf("graficos_mlpe_fase1.pdf", width = 10, height = 6)

resultados_fase1 <- list()

for (nome in names(stores)) {
  cat("\n--- Loja:", nome, "---\n")
  
  d  <- stores[[nome]]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  TS <- d$Num_Customers
  datas <- d$Date
  L  <- length(TS)
  YR <- diff(range(TS))
  
  # Indices treino / teste
  TR_idx <- 1:(L - H)
  TS_idx <- (L - H + 1):L
  real <- TS[TS_idx]
  datas_teste <- datas[TS_idx]
  
  # ---- Seasonal Naive ----
  pred_naive <- seasonal_naive(TS[TR_idx], h = H)
  
  # Pos-processamento naive
  pred_naive[pred_naive < 0] <- 0
  pred_naive[datas_teste %in% datas_fecho] <- 0
  
  nmae_naive <- mmetric(real, pred_naive, metric = "NMAE", val = YR)
  rrse_naive <- mmetric(real, pred_naive, metric = "RRSE")
  r2_naive   <- 1 - (sum((real - pred_naive)^2) / sum((real - mean(real))^2))
  
  cat(sprintf("  Seasonal Naive | NMAE=%.4f | RRSE=%.4f | R2=%.4f\n",
              nmae_naive, rrse_naive, r2_naive))
  
  # ---- mlpe ----
  D   <- CasesSeries(TS, LAGS)
  LD  <- nrow(D)
  hd  <- holdout(D$y, ratio = H, mode = "order")
  
  model_mlpe <- fit(y~., D[hd$tr, ], model = "mlpe")
  pred_mlpe  <- lforecast(model_mlpe, D, start = hd$ts[1], horizon = H)
  
  # Pos-processamento mlpe
  pred_mlpe[pred_mlpe < 0] <- 0
  pred_mlpe[datas_teste %in% datas_fecho] <- 0
  
  nmae_mlpe <- mmetric(real, pred_mlpe, metric = "NMAE", val = YR)
  rrse_mlpe <- mmetric(real, pred_mlpe, metric = "RRSE")
  r2_mlpe   <- 1 - (sum((real - pred_mlpe)^2) / sum((real - mean(real))^2))
  
  cat(sprintf("  mlpe           | NMAE=%.4f | RRSE=%.4f | R2=%.4f\n",
              nmae_mlpe, rrse_mlpe, r2_mlpe))
  
  # ---- Gráfico Fase I ----
  yrange <- range(real, pred_naive, pred_mlpe)
  plot(1:H, real, type = "b", pch = 19, col = "black",
       ylim = yrange, xlab = "Dia (horizonte)", ylab = "Num_Customers",
       main = paste("Fase I -", nome, "| Predicted vs Actual"))
  lines(1:H, pred_naive, type = "b", pch = 2,  col = "gray50",  lty = 2)
  lines(1:H, pred_mlpe,  type = "b", pch = 17, col = "tomato",  lty = 1)
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "mlpe"),
         col    = c("black", "gray50", "tomato"),
         pch    = c(19, 2, 17), lty = c(1, 2, 1))
  
  resultados_fase1[[nome]] <- data.frame(
    Loja   = nome,
    Metodo = c("Seasonal_Naive", "mlpe"),
    NMAE   = c(nmae_naive, nmae_mlpe),
    RRSE   = c(rrse_naive, rrse_mlpe),
    R2     = c(r2_naive, r2_mlpe)
  )
}

dev.off()

df_fase1 <- do.call(rbind, resultados_fase1)
cat("\n\n====== TABELA FASE I ======\n")
print(df_fase1, row.names = FALSE, digits = 4)
write.csv(df_fase1, "resultados_mlpe_fase1.csv", row.names = FALSE)

# =============================================================================
# FASE II — Backtesting (growing window, 12 iterações)
# =============================================================================
<<<<<<< HEAD
cat("\n\n")
cat("=============================================================\n")
cat("  FASE II - Backtesting growing window (12 iterações)       \n")
=======
cat("\n\n=============================================================\n")
cat("  FASE II - Backtesting growing window (12 iteracoes)       \n")
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
cat("=============================================================\n")

resultados_fase2 <- list()
resultados_media <- data.frame()
idx_cor <- 1

for (nome in names(stores)) {
  cat("\n--- Loja:", nome, "---\n")
  
  d  <- stores[[nome]]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  TS <- d$Num_Customers
  datas <- d$Date
  L  <- length(TS)
  YR <- diff(range(TS))
  
  # Dataset com lags para mlpe
  max_lag <- max(LAGS)
  D  <- CasesSeries(TS, LAGS)
  LD <- nrow(D)
  
  # Datas correspondentes às linhas de D
  datas_D <- datas[(max_lag + 1):L]
  
  # Tamanho da janela inicial de treino
  W2 <- (LD - H) - (RUNS - 1) * S
  cat(sprintf("  Tamanho D: %d | Janela inicial treino: %d\n", LD, W2))
  
  nmae_mlpe_v  <- numeric(RUNS)
  rrse_mlpe_v  <- numeric(RUNS)
  r2_mlpe_v    <- numeric(RUNS)
  mae_mlpe_v   <- numeric(RUNS)
  
  nmae_naive_v <- numeric(RUNS)
  rrse_naive_v <- numeric(RUNS)
  r2_naive_v   <- numeric(RUNS)
  mae_naive_v  <- numeric(RUNS)
  
  all_pred_mlpe  <- c()
  all_pred_naive <- c()
  all_real       <- c()
  
  res_loja <- data.frame()
  
  for (b in 1:RUNS) {
<<<<<<< HEAD
    # Growing window em D
=======
    # Growing window (incremental)
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
    H2 <- holdout(D$y, ratio = H, mode = "incremental",
                  iter = b, window = W2, increment = S)
    
    real <- D[H2$ts, ]$y
    datas_teste <- datas_D[H2$ts]
    
    # ---- mlpe ----
    model <- fit(y ~ ., D[H2$tr, ], model = "mlpe")
    pred_mlpe <- lforecast(model, D, start = H2$ts[1], horizon = H)
    
    # ---- Seasonal Naive ----
    train_y    <- D[H2$tr, ]$y
    pred_naive <- seasonal_naive(train_y, h = H)
    
    # --- Pos-processamento mlpe ---
    pred_mlpe[pred_mlpe < 0] <- 0
    pred_mlpe[datas_teste %in% datas_fecho] <- 0
    
    # --- Pos-processamento naive ---
    pred_naive[pred_naive < 0] <- 0
    pred_naive[datas_teste %in% datas_fecho] <- 0
    
    # --- Metricas mlpe ---
    mae_mlpe_v[b]  <- mmetric(real, pred_mlpe, metric = "MAE")
    nmae_mlpe_v[b] <- mmetric(real, pred_mlpe, metric = "NMAE", val = YR)
    rrse_mlpe_v[b] <- mmetric(real, pred_mlpe, metric = "RRSE")
    r2_mlpe_v[b]   <- 1 - (sum((real - pred_mlpe)^2) / sum((real - mean(real))^2))
    
    # --- Metricas naive ---
    mae_naive_v[b]  <- mmetric(real, pred_naive, metric = "MAE")
    nmae_naive_v[b] <- mmetric(real, pred_naive, metric = "NMAE", val = YR)
    rrse_naive_v[b] <- mmetric(real, pred_naive, metric = "RRSE")
    r2_naive_v[b]   <- 1 - (sum((real - pred_naive)^2) / sum((real - mean(real))^2))
    
    all_pred_mlpe  <- c(all_pred_mlpe,  pred_mlpe)
    all_pred_naive <- c(all_pred_naive, pred_naive)
    all_real       <- c(all_real, real)
    
    cat(sprintf("  iter %2d | mlpe NMAE=%.4f RRSE=%.4f R2=%.4f | Naive NMAE=%.4f\n",
                b, nmae_mlpe_v[b], rrse_mlpe_v[b], r2_mlpe_v[b], nmae_naive_v[b]))
    
    # --- Guardar linha ---
    linha <- data.frame(
      Loja = nome,
      Iteracao = b,
      Train_Start = as.character(datas_D[H2$tr[1]]),
      Train_End = as.character(datas_D[H2$tr[length(H2$tr)]]),
      Train_Size = length(H2$tr),
      Test_Start = as.character(datas_teste[1]),
      Test_End = as.character(datas_teste[length(datas_teste)]),
      Test_Size = length(H2$ts),
      NMAE_mlpe = round(nmae_mlpe_v[b], 4),
      RRSE_mlpe = round(rrse_mlpe_v[b], 4),
      R2_mlpe = round(r2_mlpe_v[b], 4),
      NMAE_naive = round(nmae_naive_v[b], 4),
      RRSE_naive = round(rrse_naive_v[b], 4),
      R2_naive = round(r2_naive_v[b], 4)
    )
    res_loja <- rbind(res_loja, linha)
  }
  
  # ---- Gráfico predicted vs actual (todo o periodo de teste) ----
  pdf(paste0("grafico_mlpe_fase2_", tolower(nome), ".pdf"), width = 10, height = 6)
  n_pts  <- length(all_real)
  yrange <- range(all_real, all_pred_mlpe, all_pred_naive)
  plot(1:n_pts, all_real, type = "l", col = "black", lwd = 1.5,
       ylim = yrange, xlab = "Observacao (teste acumulado)", ylab = "Num_Customers",
       main = paste("Fase II -", nome, "| Predicted vs Actual (todas as iteracoes)"))
  lines(1:n_pts, all_pred_naive, col = "gray50", lty = 2, lwd = 1.2)
  lines(1:n_pts, all_pred_mlpe,  col = "tomato",  lty = 1, lwd = 1.5)
  for (j in 1:(RUNS - 1)) abline(v = j * H, col = "gray80", lty = 3)
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "mlpe"),
         col    = c("black", "gray50", "tomato"),
         lty    = c(1, 2, 1), lwd = c(1.5, 1.2, 1.5))
  dev.off()
  
  # ---- Boxplot das metricas (guardado em PDF) ----
  pdf(paste0("boxplot_mlpe_", tolower(nome), ".pdf"), width = 7, height = 5)
  boxplot(nmae_mlpe_v, rrse_mlpe_v, r2_mlpe_v,
          names = c("NMAE", "RRSE", "R2"),
          main = paste(nome, "- mlpe - Distribuicao das Metricas (12 iteracoes)"),
          col = lojas_cores[idx_cor])
  dev.off()
  
  # ---- Medias das metricas ----
  cat(sprintf("\n  [mlpe]  Mean.NMAE=%.4f | Mean.RRSE=%.4f | Mean.R2=%.4f\n",
              mean(nmae_mlpe_v), mean(rrse_mlpe_v), mean(r2_mlpe_v)))
  cat(sprintf("  [Naive] Mean.NMAE=%.4f | Mean.RRSE=%.4f | Mean.R2=%.4f\n",
              mean(nmae_naive_v), mean(rrse_naive_v), mean(r2_naive_v)))
  
  melhoria <- (mean(nmae_naive_v) - mean(nmae_mlpe_v)) / mean(nmae_naive_v) * 100
  cat(sprintf("  Melhoria mlpe vs Naive: %.1f%%\n", melhoria))
  
  # --- Acumular resultados ---
  resultados_fase2[[nome]] <- res_loja
  
  med_loja <- data.frame(
    Loja = nome,
    Target = "Num_Customers",
    Metodo = "Growing Window",
    Iteracoes = RUNS,
    Media_NMAE_mlpe = round(mean(nmae_mlpe_v), 4),
    Media_RRSE_mlpe = round(mean(rrse_mlpe_v), 4),
    Media_R2_mlpe = round(mean(r2_mlpe_v), 4),
    Media_NMAE_naive = round(mean(nmae_naive_v), 4),
    Media_RRSE_naive = round(mean(rrse_naive_v), 4),
    Media_R2_naive = round(mean(r2_naive_v), 4)
  )
  resultados_media <- rbind(resultados_media, med_loja)
  
  idx_cor <- idx_cor + 1
}

# ============================================================
# Resultados finais
# ============================================================
cat("\n============================================================\n")
cat("RESULTADOS FINAIS - MEDIA DAS METRICAS POR LOJA\n")
cat("============================================================\n")
print(resultados_media, row.names = FALSE)

# ============================================================
# Guardar CSV
# ============================================================
df_fase2 <- do.call(rbind, resultados_fase2)
write.csv(df_fase2, "backtesting_mlpe_iteracoes.csv", row.names = FALSE)
write.csv(resultados_media, "backtesting_mlpe_media_lojas.csv", row.names = FALSE)

cat("\nCSVs e PDFs guardados na pasta MLPE.\n")