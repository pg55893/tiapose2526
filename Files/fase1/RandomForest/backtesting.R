library(rminer)

H     <- 7
NITER <- 12

mostrar_metricas <- function(label, Y, Pred, srange) {
  cat(label, "\n")
  cat("  MAE :", round(mmetric(Y, Pred, metric = "MAE"), 1), "\n")
  cat("  NMAE:", round(mmetric(Y, Pred, metric = "NMAE", val = srange), 2), "%\n")
  cat("  RMSE:", round(mmetric(Y, Pred, metric = "RMSE"), 1), "\n")
  cat("  RRSE:", round(mmetric(Y, Pred, metric = "RRSE"), 2), "%\n\n")
}

backtesting_rf <- function(dados, nome_loja) {
  # --- prever Num_Customers em vez de Sales ---
  S      <- dados$Num_Customers
  L      <- length(S)
  srange <- diff(range(S))
  
  resultados_rf    <- matrix(NA, nrow = NITER, ncol = 4)
  resultados_naive <- matrix(NA, nrow = NITER, ncol = 4)
  colnames(resultados_rf)    <- c("MAE", "NMAE", "RMSE", "RRSE")
  colnames(resultados_naive) <- c("MAE", "NMAE", "RMSE", "RRSE")
  
  cat("=============================\n")
  cat("LOJA:", nome_loja, "\n")
  cat("=============================\n\n")
  
  for (i in 1:NITER) {
    fim_teste    <- L - (NITER - i) * H
    inicio_teste <- fim_teste - H + 1
    LTR          <- inicio_teste - 1
    
    Y       <- S[inicio_teste:fim_teste]
    P_naive <- S[(inicio_teste - H):(fim_teste - H)]
    
    D      <- CasesSeries(S[1:LTR], c(1, 7, 8))
    ND     <- nrow(D)
    RF     <- fit(y ~., D, model = "randomForest")
    P_rf   <- lforecast(RF, D, start = ND, horizon = H)
    
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
  
  plot(resultados_naive[, "NMAE"], type = "o", col = "gray", lty = 2,
       ylim = range(c(resultados_naive[, "NMAE"], resultados_rf[, "NMAE"])),
       main = paste("Backtesting -", nome_loja, ": NMAE por iteração"),
       ylab = "NMAE (%)", xlab = "Iteração")
  lines(resultados_rf[, "NMAE"], type = "o", col = "purple", lty = 1)
  legend("topright",
         legend = c("Seasonal Naive", "Random Forest"),
         col    = c("gray", "purple"),
         lty    = c(2, 1), cex = 0.8)
  
  # --- devolver métricas agregadas automaticamente ---
  return(data.frame(
    Loja       = nome_loja,
    Modelo     = "Random Forest",
    MAE        = round(median(resultados_rf[, "MAE"]),  1),
    NMAE       = round(median(resultados_rf[, "NMAE"]), 2),
    RMSE       = round(median(resultados_rf[, "RMSE"]), 1),
    RRSE       = round(median(resultados_rf[, "RRSE"]), 2),
    Naive_MAE  = round(median(resultados_naive[, "MAE"]),  1),
    Naive_NMAE = round(median(resultados_naive[, "NMAE"]), 2),
    Naive_RMSE = round(median(resultados_naive[, "RMSE"]), 1),
    Naive_RRSE = round(median(resultados_naive[, "RRSE"]), 2)
  ))
}

# --- Correr para as 4 lojas e guardar CSV automaticamente ---
r1 <- backtesting_rf(baltimore,    "Baltimore")
r2 <- backtesting_rf(lancaster,    "Lancaster")
r3 <- backtesting_rf(richmond,     "Richmond")
r4 <- backtesting_rf(philadelphia, "Philadelphia")

# --- Juntar resultados e exportar CSV ---
resultados_finais <- rbind(r1, r2, r3, r4)
write.csv(resultados_finais, "resultados_backtesting_RF.csv", row.names = FALSE)
cat("CSV guardado com sucesso!\n")