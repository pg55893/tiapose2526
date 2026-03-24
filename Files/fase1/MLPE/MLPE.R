# =============================================================================
# JOÃO — Univariado ML: MLP Ensemble (mlpe) via rminer
# Target  : Num_Customers, H = 7 dias à frente
# Baseline: Seasonal Naive (repete a semana anterior)
# Fase I  : holdout simples (treino = tudo menos últimos 7 dias)
# Fase II : backtesting growing window, 12 iterações, guardar CSV
# 4 lojas : Philadelphia, Baltimore, Richmond, Lancaster
# =============================================================================

# install.packages("rminer")
# Usar rminer (desativar inicialização do RGL devido ao sistema operativo usado)
options(rgl.useNULL = TRUE)
library(rminer)

# --- Tratamento de dados ------------------------------------------------------
source("~/Desktop/Mestrado/TIAPOSE/project/Files/tratamentoDeDados.R")

# --- Configuração global ------------------------------------------------------
H      <- 7       # horizonte de previsão
RUNS   <- 12      # iterações de backtesting (Fase II)
S      <- 7       # passo entre janelas (1 semana)
LAGS   <- 1:7     # lags t-1 a t-7 (captura sazonalidade semanal)

# Usar directamente os data.frames já tratados
stores <- list(
  Philadelphia = philadelphia,
  Baltimore    = baltimore,
  Richmond     = richmond,
  Lancaster    = lancaster
)

# --- Seasonal Naive -----------------------------------------------------------
# Previsão: repete os últimos 7 valores observados (semana anterior)
seasonal_naive <- function(train, h = 7) {
  n <- length(train)
  tail(train, h)   # últimos h valores = "semana anterior"
}

# =============================================================================
# FASE I — Holdout simples
# =============================================================================
cat("\n\n")
cat("=============================================================\n")
cat("  FASE I — Holdout simples (últimos 7 dias como teste)      \n")
cat("=============================================================\n")

# Para guardar todos os plots em PDF
pdf("../MLPE_plots.pdf")

resultados_fase1 <- list()

for (nome in names(stores)) {
  cat("\n--- Loja:", nome, "---\n")
  
  d  <- stores[[nome]]
  TS <- d$Num_Customers
  L  <- length(TS)
  YR <- diff(range(TS))
  
  # Índices treino / teste
  TR <- 1:(L - H)
  TS_idx <- (L - H + 1):L
  real <- TS[TS_idx]
  
  # ---- Seasonal Naive ----
  pred_naive <- seasonal_naive(TS[TR], h = H)
  
  mae_naive  <- mmetric(real, pred_naive, metric = "MAE")
  nmae_naive <- mmetric(real, pred_naive, metric = "NMAE", val = YR)
  rmse_naive <- mmetric(real, pred_naive, metric = "RMSE")
  rrse_naive <- mmetric(real, pred_naive, metric = "RRSE")
  
  cat(sprintf("  Seasonal Naive | MAE=%.1f | NMAE=%.2f%% | RMSE=%.1f | RRSE=%.3f\n",
              mae_naive, nmae_naive, rmse_naive, rrse_naive))
  
  # ---- mlpe ----
  D   <- CasesSeries(TS, LAGS)
  LD  <- nrow(D)
  hd  <- holdout(D$y, ratio = H, mode = "order")  # split temporal simples
  
  model_mlpe <- fit(y~., D[hd$tr, ], model = "mlpe")
  pred_mlpe  <- lforecast(model_mlpe, D, start = hd$ts[1], horizon = H)
  
  mae_mlpe  <- mmetric(real, pred_mlpe, metric = "MAE")
  nmae_mlpe <- mmetric(real, pred_mlpe, metric = "NMAE", val = YR)
  rmse_mlpe <- mmetric(real, pred_mlpe, metric = "RMSE")
  rrse_mlpe <- mmetric(real, pred_mlpe, metric = "RRSE")
  
  cat(sprintf("  mlpe (lags 1:7)| MAE=%.1f | NMAE=%.2f%% | RMSE=%.1f | RRSE=%.3f\n",
              mae_mlpe, nmae_mlpe, rmse_mlpe, rrse_mlpe))
  
  # ---- Gráfico Fase I ----
  par(mfrow = c(1, 1))
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
    Loja  = nome,
    Metodo = c("Seasonal_Naive", "mlpe"),
    MAE    = c(mae_naive,  mae_mlpe),
    NMAE   = c(nmae_naive, nmae_mlpe),
    RMSE   = c(rmse_naive, rmse_mlpe),
    RRSE   = c(rrse_naive, rrse_mlpe)
  )
}

# Tabela resumo Fase I
df_fase1 <- do.call(rbind, resultados_fase1)
cat("\n\n====== TABELA FASE I ======\n")
print(df_fase1, row.names = FALSE, digits = 3)

# =============================================================================
# FASE II — Backtesting (growing window, 12 iterações)
# =============================================================================
cat("\n\n")
cat("=============================================================\n")
cat("  FASE II - Backtesting growing window (12 iterações)       \n")
cat("=============================================================\n")

resultados_fase2 <- list()

for (nome in names(stores)) {
  cat("\n--- Loja:", nome, "---\n")
  
  d  <- stores[[nome]]
  TS <- d$Num_Customers
  L  <- length(TS)
  YR <- diff(range(TS))
  
  # Dataset com lags para mlpe
  D  <- CasesSeries(TS, LAGS)
  LD <- nrow(D)
  
  # Tamanho da janela inicial de treino
  W2 <- LD - H - (RUNS - 1) * S
  cat(sprintf("  Tamanho D: %d | Janela inicial treino: %d\n", LD, W2))
  
  nmae_mlpe_v  <- numeric(RUNS)
  rmse_mlpe_v  <- numeric(RUNS)
  rrse_mlpe_v  <- numeric(RUNS)
  mae_mlpe_v   <- numeric(RUNS)
  
  nmae_naive_v <- numeric(RUNS)
  rmse_naive_v <- numeric(RUNS)
  rrse_naive_v <- numeric(RUNS)
  mae_naive_v  <- numeric(RUNS)
  
  all_pred_mlpe  <- c()
  all_pred_naive <- c()
  all_real        <- c()
  
  for (b in 1:RUNS) {
    # Growing window em D
    H2 <- holdout(D$y, ratio = H, mode = "incremental",
                  iter = b, window = W2, increment = S)
    
    real <- D[H2$ts, ]$y
    
    # ---- mlpe ----
    model <- fit(y ~ ., D[H2$tr, ], model = "mlpe")
    pred_mlpe <- lforecast(model, D, start = (length(H2$tr) + 1), horizon = H)
    
    # ---- Seasonal Naive: últimos H valores do treino (em D) ----
    train_y      <- D[H2$tr, ]$y
    pred_naive   <- seasonal_naive(train_y, h = H)
    
    mae_mlpe_v[b]   <- mmetric(real, pred_mlpe,  metric = "MAE")
    nmae_mlpe_v[b]  <- mmetric(real, pred_mlpe,  metric = "NMAE", val = YR)
    rmse_mlpe_v[b]  <- mmetric(real, pred_mlpe,  metric = "RMSE")
    rrse_mlpe_v[b]  <- mmetric(real, pred_mlpe,  metric = "RRSE")
    
    mae_naive_v[b]  <- mmetric(real, pred_naive,  metric = "MAE")
    nmae_naive_v[b] <- mmetric(real, pred_naive,  metric = "NMAE", val = YR)
    rmse_naive_v[b] <- mmetric(real, pred_naive,  metric = "RMSE")
    rrse_naive_v[b] <- mmetric(real, pred_naive,  metric = "RRSE")
    
    all_pred_mlpe  <- c(all_pred_mlpe,  pred_mlpe)
    all_pred_naive <- c(all_pred_naive, pred_naive)
    all_real       <- c(all_real, real)
    
    cat(sprintf("  iter %2d | mlpe NMAE=%.2f%% | Naive NMAE=%.2f%%\n",
                b, nmae_mlpe_v[b], nmae_naive_v[b]))
  }
  
  # ---- Resultados por loja ----
  cat(sprintf("\n  [mlpe]  Med.MAE=%.1f | Med.NMAE=%.2f%% | Med.RMSE=%.1f | Med.RRSE=%.3f\n",
              median(mae_mlpe_v), median(nmae_mlpe_v),
              median(rmse_mlpe_v), median(rrse_mlpe_v)))
  cat(sprintf("  [Naive] Med.MAE=%.1f | Med.NMAE=%.2f%% | Med.RMSE=%.1f | Med.RRSE=%.3f\n",
              median(mae_naive_v), median(nmae_naive_v),
              median(rmse_naive_v), median(rrse_naive_v)))
  
  melhoria <- (median(nmae_naive_v) - median(nmae_mlpe_v)) / median(nmae_naive_v) * 100
  cat(sprintf("  Melhoria mlpe vs Naive: %.1f%%\n", melhoria))
  
  # ---- Gráfico Fase II: predicted vs actual (todo o período de teste) ----
  n_pts  <- length(all_real)
  yrange <- range(all_real, all_pred_mlpe, all_pred_naive)
  plot(1:n_pts, all_real, type = "l", col = "black", lwd = 1.5,
       ylim = yrange, xlab = "Observação (teste acumulado)", ylab = "Num_Customers",
       main = paste("Fase II -", nome, "| Predicted vs Actual (todas as iterações)"))
  lines(1:n_pts, all_pred_naive, col = "gray50", lty = 2, lwd = 1.2)
  lines(1:n_pts, all_pred_mlpe,  col = "tomato",  lty = 1, lwd = 1.5)
  # Linhas verticais a separar as iterações
  for (i in 1:(RUNS - 1)) abline(v = i * H, col = "gray80", lty = 3)
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "mlpe"),
         col    = c("black", "gray50", "tomato"),
         lty    = c(1, 2, 1), lwd = c(1.5, 1.2, 1.5))
  
  # Guardar métricas por iteração para CSV
  resultados_fase2[[nome]] <- data.frame(
    Loja      = nome,
    Iteracao  = 1:RUNS,
    MAE_mlpe  = mae_mlpe_v,
    NMAE_mlpe = nmae_mlpe_v,
    RMSE_mlpe = rmse_mlpe_v,
    RRSE_mlpe = rrse_mlpe_v,
    MAE_naive  = mae_naive_v,
    NMAE_naive = nmae_naive_v,
    RMSE_naive = rmse_naive_v,
    RRSE_naive = rrse_naive_v
  )
}

# ---- Guardar CSV com resultados Fase II ----
df_fase2 <- do.call(rbind, resultados_fase2)
write.csv(df_fase2, "../MLPE_backtesting_results.csv", row.names = FALSE)
cat("\n\nResultados Fase II guardados em: resultados_joao_fase2.csv\n")

# ---- Tabela resumo Fase II (medianas) ----
cat("\n====== TABELA FASE II - Medianas por loja ======\n")
cat(sprintf("%-15s %6s %8s %8s %8s %8s\n",
            "Loja", "Método", "Med.MAE", "Med.NMAE", "Med.RMSE", "Med.RRSE"))
cat(strrep("-", 60), "\n")

for (nome in names(stores)) {
  df  <- resultados_fase2[[nome]]
  cat(sprintf("%-15s %6s %8.1f %8.2f%% %8.1f %8.3f\n",
              nome, "mlpe",
              median(df$MAE_mlpe), median(df$NMAE_mlpe),
              median(df$RMSE_mlpe), median(df$RRSE_mlpe)))
  cat(sprintf("%-15s %6s %8.1f %8.2f%% %8.1f %8.3f\n",
              "", "Naive",
              median(df$MAE_naive), median(df$NMAE_naive),
              median(df$RMSE_naive), median(df$RRSE_naive)))
  cat(strrep("-", 60), "\n")
}

cat("\nCSV guardado pronto para partilhar com o grupo na reunião final.\n")

dev.off()