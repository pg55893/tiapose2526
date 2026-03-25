# ============================================================
# Fase II — Holt-Winters — Backtesting (Growing + Rolling Window)
# Objetivo: Avaliação robusta com múltiplas iterações
# Método: Holt-Winters (pacote forecast)
# Baseline: Seasonal Naive (repete a semana anterior)
# Lojas: Baltimore, Lancaster, Philadelphia, Richmond
# Nota: Dados pré-tratados (Natal, Black Friday, NA em 2014-04-20)
# ============================================================

# --- Carregar bibliotecas ---
library(forecast)
options(rgl.useNULL = TRUE)
library(rminer)

# ============================================================
# Função de tratamento dos dados (do script do grupo)
# ============================================================
preparar_dados <- function(df) {
  df$Date <- as.Date(df$Date)
  
  # --- Natal: substituir valores pela mediana do mesmo dia da semana ---
  natais <- as.Date(c("2012-12-25", "2013-12-25"))
  for (data_natal in natais) {
    data_natal <- as.Date(data_natal, origin = "1970-01-01")
    idx   <- which(df$Date == data_natal)
    wday  <- weekdays(data_natal)
    antes <- df[df$Date < data_natal, ]
    mesmo <- antes[weekdays(antes$Date) == wday, ]
    df$Sales[idx]         <- median(mesmo$Sales)
    df$Num_Customers[idx] <- median(mesmo$Num_Customers)
    df$Num_Employees[idx] <- median(mesmo$Num_Employees)
    df$Pct_On_Sale[idx]   <- median(mesmo$Pct_On_Sale)
  }
  
  # --- 2014-04-20: corrigir NA em Pct_On_Sale e 0 suspeito em Num_Customers ---
  idx_na  <- which(df$Date == as.Date("2014-04-20"))
  wday_na <- weekdays(as.Date("2014-04-20"))
  antes_na <- df[df$Date < as.Date("2014-04-20"), ]
  mesmo_na <- antes_na[weekdays(antes_na$Date) == wday_na, ]
  df$Pct_On_Sale[idx_na]   <- median(antes_na$Pct_On_Sale, na.rm = TRUE)
  df$Num_Customers[idx_na] <- median(mesmo_na$Num_Customers)
  
  # --- Black Friday: marcar como TouristEvent ---
  black_fridays <- as.Date(c("2012-11-23", "2013-11-29"))
  df$TouristEvent[df$Date %in% black_fridays] <- "Yes"
  
  return(df)
}

# --- Configuração geral ---
K <- 7                          # período sazonal (semanal)
Test <- K                       # horizonte de previsão H=7
S <- 7                          # salto entre iterações (1 semana, sem sobreposição)
Runs <- 12                      # número de iterações de backtesting

# Lista das 4 lojas
lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")

# Data frame para guardar resultados finais
resultados <- data.frame(
  Loja         = character(),
  Estrategia   = character(),
  Mediana_MAE  = numeric(),
  Mediana_NMAE = numeric(),
  Mediana_RMSE = numeric(),
  Mediana_RRSE = numeric(),
  Mediana_R2   = numeric(),
  Naive_MAE    = numeric(),     # mediana MAE do baseline Seasonal Naive
  Naive_NMAE   = numeric(),     # mediana NMAE do baseline Seasonal Naive
  Naive_RMSE   = numeric(),     # mediana RMSE do baseline Seasonal Naive
  Naive_RRSE   = numeric(),     # mediana RRSE do baseline Seasonal Naive
  stringsAsFactors = FALSE
)

cat("############################################################\n")
cat("# FASE II — HOLT-WINTERS — BACKTESTING (4 LOJAS)          #\n")
cat("# Dados com tratamento aplicado                            #\n")
cat("############################################################\n\n")
cat("Configuração: K =", K, "| Test =", Test, "| S =", S, "| Runs =", Runs, "\n\n")

# --- Abrir PDF para guardar todos os gráficos ---
pdf("Files/fase1/HoltWinters/graficos_fase2.pdf", width = 10, height = 6)

# ============================================================
# Ciclo principal: aplicar backtesting a cada loja
# ============================================================
for (loja in lojas) {
  
  cat("############################################################\n")
  cat("# LOJA:", toupper(loja), "\n")
  cat("############################################################\n\n")
  
  # --- Carregar e tratar dados ---
  dados <- preparar_dados(read.csv(paste0("Files/csv/", loja, ".csv")))
  clientes <- dados$Num_Customers
  L <- length(clientes)
  W <- (L - Test) - (Runs - 1) * S  # janela inicial de treino
  YR <- diff(range(clientes))        # range global para NMAE
  
  cat("Observações:", L, "| W inicial:", W, "\n\n")
  
  # ========================================================
  # GROWING WINDOW
  # ========================================================
  cat("---------- GROWING WINDOW ----------\n")
  
  gw_mae   <- numeric(Runs)           # MAE do HW
  gw_nmae  <- numeric(Runs)           # NMAE do HW
  gw_rmse  <- numeric(Runs)           # RMSE do HW
  gw_rrse  <- numeric(Runs)           # RRSE do HW
  gw_r2    <- numeric(Runs)           # R2 do HW
  gw_n_mae  <- numeric(Runs)          # MAE do Naive
  gw_n_nmae <- numeric(Runs)          # NMAE do Naive
  gw_n_rmse <- numeric(Runs)          # RMSE do Naive
  gw_n_rrse <- numeric(Runs)          # RRSE do Naive
  
  for (b in 1:Runs) {
    H <- holdout(clientes, ratio = Test, mode = "incremental",
                 iter = b, window = W, increment = S)
    
    # --- HW ---
    dtr <- ts(clientes[H$tr], frequency = K)
    modelo <- suppressWarnings(HoltWinters(dtr))
    pred <- forecast(modelo, h = length(H$ts))$mean[1:Test]
    
    # --- Naive: repete os últimos K valores do treino (semana anterior) ---
    naive_pred <- clientes[H$tr[(length(H$tr) - K + 1):length(H$tr)]]
    
    gw_mae[b]   <- mmetric(y = clientes[H$ts], x = pred,       metric = "MAE")
    gw_nmae[b]  <- mmetric(y = clientes[H$ts], x = pred,       metric = "NMAE", val = YR)
    gw_rmse[b]  <- mmetric(y = clientes[H$ts], x = pred,       metric = "RMSE")
    gw_rrse[b]  <- mmetric(y = clientes[H$ts], x = pred,       metric = "RRSE")
    gw_r2[b]    <- mmetric(y = clientes[H$ts], x = pred,       metric = "R2")
    gw_n_mae[b]  <- mmetric(y = clientes[H$ts], x = naive_pred, metric = "MAE")
    gw_n_nmae[b] <- mmetric(y = clientes[H$ts], x = naive_pred, metric = "NMAE", val = YR)
    gw_n_rmse[b] <- mmetric(y = clientes[H$ts], x = naive_pred, metric = "RMSE")
    gw_n_rrse[b] <- mmetric(y = clientes[H$ts], x = naive_pred, metric = "RRSE")
    
    cat(sprintf("  Iter %2d | TR: %3d-%3d (n=%d) | TS: %3d-%3d | NMAE: %5.2f | Naive NMAE: %5.2f\n",
                b, H$tr[1], H$tr[length(H$tr)], length(H$tr),
                H$ts[1], H$ts[length(H$ts)],
                gw_nmae[b], gw_n_nmae[b]))
  }
  
  cat(sprintf("\n  Medianas Growing Window (HW):    MAE=%.1f | NMAE=%.2f%% | RMSE=%.1f | RRSE=%.2f%% | R2=%.4f\n",
              median(gw_mae), median(gw_nmae), median(gw_rmse), median(gw_rrse), median(gw_r2)))
  cat(sprintf("  Medianas Growing Window (Naive): MAE=%.1f | NMAE=%.2f%% | RMSE=%.1f | RRSE=%.2f%%\n\n",
              median(gw_n_mae), median(gw_n_nmae), median(gw_n_rmse), median(gw_n_rrse)))
  
  resultados <- rbind(resultados, data.frame(
    Loja         = loja,
    Estrategia   = "Growing Window",
    Mediana_MAE  = round(median(gw_mae), 1),
    Mediana_NMAE = round(median(gw_nmae), 2),
    Mediana_RMSE = round(median(gw_rmse), 1),
    Mediana_RRSE = round(median(gw_rrse), 2),
    Mediana_R2   = round(median(gw_r2), 4),
    Naive_MAE    = round(median(gw_n_mae), 1),
    Naive_NMAE   = round(median(gw_n_nmae), 2),
    Naive_RMSE   = round(median(gw_n_rmse), 1),
    Naive_RRSE   = round(median(gw_n_rrse), 2),
    stringsAsFactors = FALSE
  ))
  
  # --- Gráfico da última iteração (Growing) ---
  mgraph(y = clientes[H$ts], x = pred, graph = "REG", Grid = 10,
         col = c("black", "blue"),
         leg = list(pos = "topleft", leg = c("Real", "Holt-Winters")),
         main = paste("Fase II - Growing Window - Ultima Iter. -", toupper(loja)))
  
  # ========================================================
  # ROLLING WINDOW
  # ========================================================
  cat("---------- ROLLING WINDOW ----------\n")
  
  rw_mae   <- numeric(Runs)           # MAE do HW
  rw_nmae  <- numeric(Runs)           # NMAE do HW
  rw_rmse  <- numeric(Runs)           # RMSE do HW
  rw_rrse  <- numeric(Runs)           # RRSE do HW
  rw_r2    <- numeric(Runs)           # R2 do HW
  rw_n_mae  <- numeric(Runs)          # MAE do Naive
  rw_n_nmae <- numeric(Runs)          # NMAE do Naive
  rw_n_rmse <- numeric(Runs)          # RMSE do Naive
  rw_n_rrse <- numeric(Runs)          # RRSE do Naive
  
  for (b in 1:Runs) {
    H <- holdout(clientes, ratio = Test, mode = "rolling",
                 iter = b, window = W, increment = S)
    
    # --- HW ---
    dtr <- ts(clientes[H$tr], frequency = K)
    modelo <- suppressWarnings(HoltWinters(dtr))
    pred <- forecast(modelo, h = length(H$ts))$mean[1:Test]
    
    # --- Naive: repete os últimos K valores do treino (semana anterior) ---
    naive_pred <- clientes[H$tr[(length(H$tr) - K + 1):length(H$tr)]]
    
    rw_mae[b]   <- mmetric(y = clientes[H$ts], x = pred,       metric = "MAE")
    rw_nmae[b]  <- mmetric(y = clientes[H$ts], x = pred,       metric = "NMAE", val = YR)
    rw_rmse[b]  <- mmetric(y = clientes[H$ts], x = pred,       metric = "RMSE")
    rw_rrse[b]  <- mmetric(y = clientes[H$ts], x = pred,       metric = "RRSE")
    rw_r2[b]    <- mmetric(y = clientes[H$ts], x = pred,       metric = "R2")
    rw_n_mae[b]  <- mmetric(y = clientes[H$ts], x = naive_pred, metric = "MAE")
    rw_n_nmae[b] <- mmetric(y = clientes[H$ts], x = naive_pred, metric = "NMAE", val = YR)
    rw_n_rmse[b] <- mmetric(y = clientes[H$ts], x = naive_pred, metric = "RMSE")
    rw_n_rrse[b] <- mmetric(y = clientes[H$ts], x = naive_pred, metric = "RRSE")
    
    cat(sprintf("  Iter %2d | TR: %3d-%3d (n=%d) | TS: %3d-%3d | NMAE: %5.2f | Naive NMAE: %5.2f\n",
                b, H$tr[1], H$tr[length(H$tr)], length(H$tr),
                H$ts[1], H$ts[length(H$ts)],
                rw_nmae[b], rw_n_nmae[b]))
  }
  
  cat(sprintf("\n  Medianas Rolling Window (HW):    MAE=%.1f | NMAE=%.2f%% | RMSE=%.1f | RRSE=%.2f%% | R2=%.4f\n",
              median(rw_mae), median(rw_nmae), median(rw_rmse), median(rw_rrse), median(rw_r2)))
  cat(sprintf("  Medianas Rolling Window (Naive): MAE=%.1f | NMAE=%.2f%% | RMSE=%.1f | RRSE=%.2f%%\n\n",
              median(rw_n_mae), median(rw_n_nmae), median(rw_n_rmse), median(rw_n_rrse)))
  
  resultados <- rbind(resultados, data.frame(
    Loja         = loja,
    Estrategia   = "Rolling Window",
    Mediana_MAE  = round(median(rw_mae), 1),
    Mediana_NMAE = round(median(rw_nmae), 2),
    Mediana_RMSE = round(median(rw_rmse), 1),
    Mediana_RRSE = round(median(rw_rrse), 2),
    Mediana_R2   = round(median(rw_r2), 4),
    Naive_MAE    = round(median(rw_n_mae), 1),
    Naive_NMAE   = round(median(rw_n_nmae), 2),
    Naive_RMSE   = round(median(rw_n_rmse), 1),
    Naive_RRSE   = round(median(rw_n_rrse), 2),
    stringsAsFactors = FALSE
  ))
  
  # --- Gráfico da última iteração (Rolling) ---
  cat("  Graficos guardados no PDF\n\n")
  mgraph(y = clientes[H$ts], x = pred, graph = "REG", Grid = 10,
         col = c("black", "blue"),
         leg = list(pos = "topleft", leg = c("Real", "Holt-Winters")),
         main = paste("Fase II - Rolling Window - Ultima Iter. -", toupper(loja)))
}

# --- Fechar PDF ---
dev.off()
cat("Graficos guardados em: Files/fase1/HoltWinters/graficos_fase2.pdf\n\n")

# ============================================================
# Resumo comparativo final
# ============================================================
cat("############################################################\n")
cat("# RESUMO COMPARATIVO — FASE II (4 LOJAS x 2 ESTRATEGIAS) #\n")
cat("############################################################\n\n")
print(resultados)

cat("\n------------------------------------------------------------\n")
idx_best <- which.min(resultados$Mediana_NMAE)
cat("Melhor combinação (NMAE):", resultados$Loja[idx_best],
    "-", resultados$Estrategia[idx_best],
    "(", resultados$Mediana_NMAE[idx_best], "% )\n")
idx_best_r2 <- which.max(resultados$Mediana_R2)
cat("Melhor combinação (R2):  ", resultados$Loja[idx_best_r2],
    "-", resultados$Estrategia[idx_best_r2],
    "(", resultados$Mediana_R2[idx_best_r2], ")\n")
cat("------------------------------------------------------------\n")

# ============================================================
# Exportar resultados
# ============================================================
write.csv(resultados, "Files/fase1/HoltWinters/resultados_hw_fase2.csv", row.names = FALSE)
cat("\nResultados exportados para: Files/fase1/HoltWinters/resultados_hw_fase2.csv\n")