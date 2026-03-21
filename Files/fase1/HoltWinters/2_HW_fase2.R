# ============================================================
# Fase II — Holt-Winters — Backtesting (Growing + Rolling Window)
# Objetivo: Avaliação robusta com múltiplas iterações
# Método: Holt-Winters (pacote forecast)
# Lojas: Baltimore, Lancaster, Philadelphia, Richmond
# ============================================================

# --- Carregar bibliotecas ---
library(forecast)
options(rgl.useNULL = TRUE)
library(rminer)

# --- Configuração geral ---
K <- 7                          # período sazonal (semanal)
Test <- K                       # horizonte de previsão H=7
S <- round(K / 3)               # salto entre iterações (2 dias)
Runs <- 10                      # número de iterações de backtesting

# Lista das 4 lojas
lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")

# Data frame para guardar resultados finais (medianas por loja e estratégia)
resultados <- data.frame(
  Loja = character(),
  Estrategia = character(),
  Mediana_NMAE = numeric(),
  Mediana_RMSE = numeric(),
  Mediana_R2 = numeric(),
  stringsAsFactors = FALSE
)

cat("############################################################\n")
cat("# FASE II — HOLT-WINTERS — BACKTESTING (4 LOJAS)          #\n")
cat("############################################################\n\n")
cat("Configuração: K =", K, "| Test =", Test, "| S =", S, "| Runs =", Runs, "\n\n")

# ============================================================
# Ciclo principal: aplicar backtesting a cada loja
# ============================================================
for (loja in lojas) {
  
  cat("############################################################\n")
  cat("# LOJA:", toupper(loja), "\n")
  cat("############################################################\n\n")
  
  # --- Carregar dados ---
  dados <- read.csv(paste0("Files/csv/", loja, ".csv"))
  clientes <- dados$Num_Customers
  L <- length(clientes)
  W <- (L - Test) - (Runs - 1) * S  # janela inicial de treino
  YR <- diff(range(clientes))        # range global para NMAE
  
  cat("Observações:", L, "| W inicial:", W, "\n\n")
  
  # ========================================================
  # GROWING WINDOW
  # ========================================================
  cat("---------- GROWING WINDOW ----------\n")
  
  # vetores para guardar métricas de cada iteração
  gw_nmae <- vector(length = Runs)
  gw_rmse <- vector(length = Runs)
  gw_r2   <- vector(length = Runs)
  
  for (b in 1:Runs) {
    # holdout incremental (growing window)
    H <- holdout(clientes, ratio = Test, mode = "incremental",
                 iter = b, window = W, increment = S)
    
    # criar ts de treino e ajustar modelo
    dtr <- ts(clientes[H$tr], frequency = K)
    modelo <- suppressWarnings(HoltWinters(dtr))
    
    # prever H passos à frente
    pred <- forecast(modelo, h = length(H$ts))$mean[1:Test]
    
    # calcular métricas
    gw_nmae[b] <- mmetric(y = clientes[H$ts], x = pred, metric = "NMAE", val = YR)
    gw_rmse[b] <- mmetric(y = clientes[H$ts], x = pred, metric = "RMSE")
    gw_r2[b]   <- mmetric(y = clientes[H$ts], x = pred, metric = "R2")
    
    # output da iteração
    cat(sprintf("  Iter %2d | TR: %3d-%3d (n=%d) | TS: %3d-%3d | NMAE: %5.2f | RMSE: %6.2f | R2: %.4f\n",
                b, H$tr[1], H$tr[length(H$tr)], length(H$tr),
                H$ts[1], H$ts[length(H$ts)],
                gw_nmae[b], gw_rmse[b], gw_r2[b]))
  }
  
  # medianas growing window
  cat("\n  Medianas Growing Window:\n")
  cat("    NMAE:", round(median(gw_nmae), 2), "%\n")
  cat("    RMSE:", round(median(gw_rmse), 2), "\n")
  cat("    R2:  ", round(median(gw_r2), 4), "\n\n")
  
  # guardar resultados
  resultados <- rbind(resultados, data.frame(
    Loja = loja, Estrategia = "Growing Window",
    Mediana_NMAE = round(median(gw_nmae), 2),
    Mediana_RMSE = round(median(gw_rmse), 2),
    Mediana_R2   = round(median(gw_r2), 4),
    stringsAsFactors = FALSE
  ))
  
  # ========================================================
  # ROLLING WINDOW
  # ========================================================
  cat("---------- ROLLING WINDOW ----------\n")
  
  # vetores para guardar métricas de cada iteração
  rw_nmae <- vector(length = Runs)
  rw_rmse <- vector(length = Runs)
  rw_r2   <- vector(length = Runs)
  
  for (b in 1:Runs) {
    # holdout rolling (janela fixa)
    H <- holdout(clientes, ratio = Test, mode = "rolling",
                 iter = b, window = W, increment = S)
    
    # criar ts de treino e ajustar modelo
    dtr <- ts(clientes[H$tr], frequency = K)
    modelo <- suppressWarnings(HoltWinters(dtr))
    
    # prever H passos à frente
    pred <- forecast(modelo, h = length(H$ts))$mean[1:Test]
    
    # calcular métricas
    rw_nmae[b] <- mmetric(y = clientes[H$ts], x = pred, metric = "NMAE", val = YR)
    rw_rmse[b] <- mmetric(y = clientes[H$ts], x = pred, metric = "RMSE")
    rw_r2[b]   <- mmetric(y = clientes[H$ts], x = pred, metric = "R2")
    
    # output da iteração
    cat(sprintf("  Iter %2d | TR: %3d-%3d (n=%d) | TS: %3d-%3d | NMAE: %5.2f | RMSE: %6.2f | R2: %.4f\n",
                b, H$tr[1], H$tr[length(H$tr)], length(H$tr),
                H$ts[1], H$ts[length(H$ts)],
                rw_nmae[b], rw_rmse[b], rw_r2[b]))
  }
  
  # medianas rolling window
  cat("\n  Medianas Rolling Window:\n")
  cat("    NMAE:", round(median(rw_nmae), 2), "%\n")
  cat("    RMSE:", round(median(rw_rmse), 2), "\n")
  cat("    R2:  ", round(median(rw_r2), 4), "\n\n")
  
  # guardar resultados
  resultados <- rbind(resultados, data.frame(
    Loja = loja, Estrategia = "Rolling Window",
    Mediana_NMAE = round(median(rw_nmae), 2),
    Mediana_RMSE = round(median(rw_rmse), 2),
    Mediana_R2   = round(median(rw_r2), 4),
    stringsAsFactors = FALSE
  ))
  
  # ========================================================
  # Gráfico da última iteração (real vs previsto)
  # ========================================================
  cat("  Gráfico da última iteração (ver painel Plots)\n\n")
  mgraph(y = clientes[H$ts], x = pred, graph = "REG", Grid = 10,
         col = c("black", "blue"),
         leg = list(pos = "topleft", leg = c("Real", "Holt-Winters")),
         main = paste("Fase II — Rolling Window — Última Iter. —", toupper(loja)))
  mpause()
  
} # fim do ciclo de lojas

# ============================================================
# Resumo comparativo final
# ============================================================
cat("############################################################\n")
cat("# RESUMO COMPARATIVO — FASE II (4 LOJAS x 2 ESTRATÉGIAS) #\n")
cat("############################################################\n\n")
print(resultados)

cat("\n------------------------------------------------------------\n")
# Melhor combinação por NMAE
idx_best <- which.min(resultados$Mediana_NMAE)
cat("Melhor combinação (NMAE):", resultados$Loja[idx_best],
    "-", resultados$Estrategia[idx_best],
    "(", resultados$Mediana_NMAE[idx_best], "% )\n")

# Melhor combinação por R2
idx_best_r2 <- which.max(resultados$Mediana_R2)
cat("Melhor combinação (R2):  ", resultados$Loja[idx_best_r2],
    "-", resultados$Estrategia[idx_best_r2],
    "(", resultados$Mediana_R2[idx_best_r2], ")\n")
cat("------------------------------------------------------------\n\n")

cat("CONCLUSÃO FASE II:\n")
cat("O Holt-Winters foi avaliado com backtesting em 10 iterações usando\n")
cat("growing window e rolling window para as 4 lojas. A mediana das métricas\n")
cat("oferece uma avaliação mais robusta do que o holdout simples da Fase I.\n")
cat("Estes resultados podem agora ser comparados com os outros métodos de\n")
cat("forecasting implementados pelo grupo (ex: ARIMA) sob as mesmas condições.\n")

# ============================================================
# Exportar resultados para CSV
# ============================================================
write.csv(resultados, "Files/fase1/HoltWinters/resultados_hw_fase2.csv", row.names = FALSE)
cat("\nResultados exportados para: Files/fase1/HoltWinters/resultados_hw_fase2.csv\n")