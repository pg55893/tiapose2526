library(rminer)

H <- 7

mostrar_metricas <- function(label, Y, Pred, srange) {
  cat(label, "\n")
  cat("  MAE :", round(mmetric(Y, Pred, metric = "MAE"), 1), "\n")
  cat("  NMAE:", round(mmetric(Y, Pred, metric = "NMAE", val = srange), 2), "%\n")
  cat("  RMSE:", round(mmetric(Y, Pred, metric = "RMSE"), 1), "\n")
  cat("  RRSE:", round(mmetric(Y, Pred, metric = "RRSE"), 2), "%\n\n")
}

fase1_rf <- function(dados, nome_loja) {
  # --- prever Num_Customers em vez de Sales ---
  S      <- dados$Num_Customers
  L      <- length(S)
  LTR    <- L - H
  srange <- diff(range(S))
  Y      <- S[(LTR + 1):L]
  
  cat("=============================\n")
  cat("LOJA:", nome_loja, "\n")
  cat("=============================\n\n")
  
  P_naive <- S[(LTR - H + 1):LTR]
  mostrar_metricas("Seasonal Naive (baseline)", Y, P_naive, srange)
  
  D      <- CasesSeries(S, c(1, 7, 8))
  ND     <- nrow(D)
  TR_idx <- 1:(ND - H)
  TS_idx <- (ND - H + 1):ND
  
  RF    <- fit(y ~., D[TR_idx, ], model = "randomForest")
  P_rf  <- lforecast(RF, D, start = TS_idx[1], horizon = H)
  mostrar_metricas("Random Forest", Y, P_rf, srange)
  
  plot(Y, type = "o", col = "black",
       ylim = range(c(Y, P_naive, P_rf)),
       main = paste("Fase I -", nome_loja, ": RF vs Naive vs Real (Num_Customers)"),
       ylab = "Num_Customers", xlab = "Dia")
  lines(P_naive, col = "gray",   lty = 2)
  lines(P_rf,    col = "purple", lty = 1)
  legend("topright",
         legend = c("Real", "Naive", "Random Forest"),
         col    = c("black", "gray", "purple"),
         lty    = c(1, 2, 1), cex = 0.8)
}

fase1_rf(baltimore,    "Baltimore")
fase1_rf(lancaster,    "Lancaster")
fase1_rf(richmond,     "Richmond")
fase1_rf(philadelphia, "Philadelphia")