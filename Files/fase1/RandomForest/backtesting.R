library(rminer)

H     <- 7   # horizonte de previsão
NITER <- 12  # número de iterações do backtesting

mostrar_metricas <- function(label, Y, Pred, srange) {
  cat(label, "\n")
  cat("  MAE :", round(mmetric(Y, Pred, metric = "MAE"), 1), "\n")
  cat("  NMAE:", round(mmetric(Y, Pred, metric = "NMAE", val = srange), 2), "%\n")
  cat("  RMSE:", round(mmetric(Y, Pred, metric = "RMSE"), 1), "\n")
  cat("  RRSE:", round(mmetric(Y, Pred, metric = "RRSE"), 2), "%\n\n")
}

# --- Backtesting com growing window ---
backtesting_rf <- function(dados, nome_loja) {
  S      <- dados$Sales
  L      <- length(S)
  srange <- diff(range(S))
  
  # matrizes para guardar métricas de cada iteração
  resultados_rf    <- matrix(NA, nrow = NITER, ncol = 4)
  resultados_naive <- matrix(NA, nrow = NITER, ncol = 4)
  colnames(resultados_rf)    <- c("MAE", "NMAE", "RMSE", "RRSE")
  colnames(resultados_naive) <- c("MAE", "NMAE", "RMSE", "RRSE")
  
  cat("=============================\n")
  cat("LOJA:", nome_loja, "\n")
  cat("=============================\n\n")
  
  for (i in 1:NITER) {
    # growing window: cada iteração usa mais dados de treino
    # a última iteração testa os últimos 7 dias, a penúltima os 7 anteriores, etc.
    fim_teste  <- L - (NITER - i) * H
    inicio_teste <- fim_teste - H + 1
    LTR <- inicio_teste - 1
    
    Y      <- S[inicio_teste:fim_teste]
    P_naive <- S[(inicio_teste - H):(fim_teste - H)]
    
    # Random Forest
    D      <- CasesSeries(S[1:LTR], c(1, 7, 8))
    ND     <- nrow(D)
    TR_idx <- 1:ND
    
    RF    <- fit(y ~., D[TR_idx, ], model = "randomForest")
    P_rf  <- lforecast(RF, D, start = ND, horizon = H)
    
    # guardar métricas
    resultados_naive[i, ] <- c(
      mmetric(Y, P_naive, metric = "MAE"),
      mmetric(Y, P_naive, metric = "NMAE", val = srange),
      mmetric(Y, P_naive, metric = "RMSE"),
      mmetric(Y, P_naive, metric = "RRSE")
    )
    resultados_rf[i, ] <- c(
      mmetric(Y, P_rf, metric = "MAE"),
      mmetric(Y, P_rf, metric = "NMAE", val = srange),
      mmetric(Y, P_rf, metric = "RMSE"),
      mmetric(Y, P_rf, metric = "RRSE")
    )
    
    cat("Iteração", i, "de", NITER, "concluída\n")
  }
  
  # --- Resultados agregados (mediana) ---
  cat("\n--- Resultados Finais (mediana de", NITER, "iterações) ---\n\n")
  cat("Seasonal Naive:\n")
  cat("  MAE :", round(median(resultados_naive[, "MAE"]),  1), "\n")
  cat("  NMAE:", round(median(resultados_naive[, "NMAE"]), 2), "%\n")
  cat("  RMSE:", round(median(resultados_naive[, "RMSE"]), 1), "\n")
  cat("  RRSE:", round(median(resultados_naive[, "RRSE"]), 2), "%\n\n")
  
  cat("Random Forest:\n")
  cat("  MAE :", round(median(resultados_rf[, "MAE"]),  1), "\n")
  cat("  NMAE:", round(median(resultados_rf[, "NMAE"]), 2), "%\n")
  cat("  RMSE:", round(median(resultados_rf[, "RMSE"]), 1), "\n")
  cat("  RRSE:", round(median(resultados_rf[, "RRSE"]), 2), "%\n\n")
  
  # --- Gráfico NMAE por iteração ---
  plot(resultados_naive[, "NMAE"], type = "o", col = "gray", lty = 2,
       ylim = range(c(resultados_naive[, "NMAE"], resultados_rf[, "NMAE"])),
       main = paste("Backtesting -", nome_loja, ": NMAE por iteração"),
       ylab = "NMAE (%)", xlab = "Iteração")
  lines(resultados_rf[, "NMAE"], type = "o", col = "purple", lty = 1)
  legend("topright",
         legend = c("Seasonal Naive", "Random Forest"),
         col    = c("gray", "purple"),
         lty    = c(2, 1), cex = 0.8)
}

# --- Correr para as 4 lojas ---
backtesting_rf(baltimore,    "Baltimore")
backtesting_rf(lancaster,    "Lancaster")
backtesting_rf(richmond,     "Richmond")
backtesting_rf(philadelphia, "Philadelphia")

# --- Guardar resultados em CSV ---
resultados_finais <- data.frame(
  Loja   = c("Baltimore", "Lancaster", "Richmond", "Philadelphia"),
  Modelo = c("Random Forest", "Random Forest", "Random Forest", "Random Forest"),
  MAE    = c(5894.8, 5825.0, 3338.9, 10988.0),
  NMAE   = c(3.83,   2.11,   4.67,   5.05),
  RMSE   = c(7657.0, 7407.4, 4180.2, 13587.4),
  RRSE   = c(134.40, 147.03, 153.59, 116.88),
  Naive_MAE  = c(5192.9, 6534.0, 2643.8, 12646.4),
  Naive_NMAE = c(3.38,   2.37,   3.70,   5.81),
  Naive_RMSE = c(6894.8, 8043.2, 3353.8, 14645.3),
  Naive_RRSE = c(137.41, 140.06, 133.56, 127.29)
)

write.csv(resultados_finais, "resultados_backtesting_RF.csv", row.names = FALSE)
cat("CSV guardado com sucesso!\n")