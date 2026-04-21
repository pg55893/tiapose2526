# ============================================================
<<<<<<< HEAD
# Fase II — Holt-Winters — Backtesting (Growing + Rolling Window)
# Objetivo: Avaliação robusta com múltiplas iterações
# Método: Holt-Winters (pacote forecast)
# Baseline: Seasonal Naive (repete a semana anterior)
# Lojas: Baltimore, Lancaster, Philadelphia, Richmond
# Nota: Dados pré-tratados (Natal, Black Friday, NA em 2014-04-20)
=======
# Fase II — Backtesting com Growing Window — Holt-Winters
# Target: Num_Customers
# 12 iterações, H=7
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
# ============================================================

# --- Carregar bibliotecas ---
library(forecast)
options(rgl.useNULL = TRUE)
library(rminer)

# --- Carregar dados tratados ---
setwd("~/TIAPOSE2526/Files/csv")
source("~/TIAPOSE2526/Files/tratamentoDeDados.R")

# --- Criar pasta e definir como destino dos resultados ---
dir.create("~/TIAPOSE2526/Files/fase1/HoltWinters", showWarnings=FALSE)
setwd("~/TIAPOSE2526/Files/fase1/HoltWinters")

# --- Datas em que a loja esta fechada (Natal e Pascoa) ---
datas_fecho <- as.Date(c("2012-12-25","2013-12-25","2013-03-31","2014-04-20"))

# --- Configuração geral ---
<<<<<<< HEAD
K <- 7                          # período sazonal (semanal)
Test <- K                       # horizonte de previsão H=7
S <- 7                          # salto entre iterações (1 semana, sem sobreposição)
Runs <- 12                      # número de iterações de backtesting
=======
K <- 7
Test <- 7
S <- 7
Runs <- 12
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f

# --- Lojas ---
lojas_nomes <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
lojas_dados <- list(baltimore, lancaster, philadelphia, richmond)
lojas_cores <- c("steelblue", "tomato", "seagreen", "hotpink")

<<<<<<< HEAD
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
=======
# --- Data frames para resultados globais ---
resultados_iteracoes <- data.frame()
resultados_media <- data.frame()
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f

cat("############################################################\n")
cat("# FASE II — HOLT-WINTERS — BACKTESTING GROWING WINDOW     #\n")
cat("# 12 iteracoes, H=7, Target: Num_Customers                #\n")
cat("############################################################\n\n")

# ============================================================
# Ciclo principal: backtesting para cada loja
# ============================================================
for (i in seq_along(lojas_nomes)) {
  
  loja <- lojas_nomes[i]
  dados <- lojas_dados[[i]]
  cor_loja <- lojas_cores[i]
  
  cat("\n============================================================\n")
  cat("BACKTESTING -", toupper(loja), "\n")
  cat("============================================================\n")
  
  # ----------------------------------------------------------
  # 1. Preparar dados
  # ----------------------------------------------------------
  dados$Date <- as.Date(dados$Date)
  dados <- dados[order(dados$Date), ]
  d1 <- dados$Num_Customers
  datas <- dados$Date
  L <- length(d1)
  
  W <- (L - Test) - (Runs - 1) * S
  
<<<<<<< HEAD
  gw_mae   <- numeric(Runs)           # MAE do HW
  gw_nmae  <- numeric(Runs)           # NMAE do HW
  gw_rmse  <- numeric(Runs)           # RMSE do HW
  gw_rrse  <- numeric(Runs)           # RRSE do HW
  gw_r2    <- numeric(Runs)           # R2 do HW
  gw_n_mae  <- numeric(Runs)          # MAE do Naive
  gw_n_nmae <- numeric(Runs)          # NMAE do Naive
  gw_n_rmse <- numeric(Runs)          # RMSE do Naive
  gw_n_rrse <- numeric(Runs)          # RRSE do Naive
=======
  YR <- diff(range(d1))
  if (YR == 0) YR <- 1
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  
  # --- Vetores de metricas ---
  MAE_v <- vector(length = Runs)
  NMAE_v <- vector(length = Runs)
  RRSE_v <- vector(length = Runs)
  R2_v <- vector(length = Runs)
  
  res_loja <- data.frame()
  
  # ----------------------------------------------------------
  # 2. Loop de backtesting
  # ----------------------------------------------------------
  for (b in 1:Runs) {
<<<<<<< HEAD
    H <- holdout(clientes, ratio = Test, mode = "incremental",
                 iter = b, window = W, increment = S)
    
    # --- HW ---
    dtr <- ts(clientes[H$tr], frequency = K)
=======
    
    H <- holdout(d1, ratio = Test, mode = "incremental", iter = b, window = W, increment = S)
    trinit <- H$tr[1]
    dtr <- ts(d1[H$tr], frequency = K)
    Y <- d1[H$ts]
    datas_teste <- datas[H$ts]
    
    # --- Ajustar Holt-Winters ---
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
    modelo <- suppressWarnings(HoltWinters(dtr))
    fc <- forecast(modelo, h = length(H$ts))
    Pred <- as.numeric(fc$mean[1:Test])
    
<<<<<<< HEAD
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
=======
    # --- Pos-processamento ---
    Pred[Pred < 0] <- 0
    Pred[datas_teste %in% datas_fecho] <- 0
    
    # --- Metricas ---
    MAE_v[b] <- mmetric(y = Y, x = Pred, metric = "MAE")
    NMAE_v[b] <- mmetric(y = Y, x = Pred, metric = "NMAE", val = YR)
    RRSE_v[b] <- mmetric(y = Y, x = Pred, metric = "RRSE")
    R2_v[b] <- 1 - (sum((Y - Pred)^2) / sum((Y - mean(Y))^2))
    
    # --- Guardar linha de resultados ---
    linha <- data.frame(
      Loja = loja,
      Iteracao = b,
      Train_Start = datas[H$tr[1]],
      Train_End = datas[H$tr[length(H$tr)]],
      Train_Size = length(H$tr),
      Test_Start = datas_teste[1],
      Test_End = datas_teste[length(datas_teste)],
      Test_Size = length(H$ts),
      Modelo = "HoltWinters",
      Variante = ifelse(modelo$seasonal == "additive", "Aditiva", "Multiplicativa"),
      MAE = round(MAE_v[b], 4),
      NMAE = round(NMAE_v[b], 4),
      RRSE = round(RRSE_v[b], 4),
      R2 = round(R2_v[b], 4)
    )
    
    res_loja <- rbind(res_loja, linha)
    
    cat("iter:", b,
        "TR:", as.character(datas[H$tr[1]]), "a", as.character(datas[H$tr[length(H$tr)]]),
        "size:", length(H$tr),
        "TS:", as.character(datas_teste[1]), "a", as.character(datas_teste[length(datas_teste)]),
        "size:", length(H$ts),
        "NMAE:", round(NMAE_v[b], 4),
        "RRSE:", round(RRSE_v[b], 4),
        "R2:", round(R2_v[b], 4), "\n")
    
    mgraph(Y, Pred, graph = "REG", Grid = 10, col = c("black", "blue"),
           leg = list(pos = "topleft", leg = c("target", "HW pred.")))
    mpause()
  }
  
  # ----------------------------------------------------------
  # 3. Boxplot das metricas (guardado em PDF)
  # ----------------------------------------------------------
  pdf(paste0("boxplot_hw_", tolower(loja), ".pdf"), width = 7, height = 5)
  boxplot(NMAE_v, RRSE_v, R2_v,
          names = c("NMAE", "RRSE", "R2"),
          main = paste(loja, "- Holt-Winters - Distribuicao das Metricas (12 iteracoes)"),
          col = cor_loja)
  dev.off()
  
  # ----------------------------------------------------------
  # 4. Medias das metricas
  # ----------------------------------------------------------
  med_loja <- data.frame(
    Loja = loja,
    Target = "Num_Customers",
    Metodo = "Growing Window",
    Iteracoes = Runs,
    Horizonte = Test,
    Incremento = S,
    Media_MAE = round(mean(MAE_v), 4),
    Media_NMAE = round(mean(NMAE_v), 4),
    Media_RRSE = round(mean(RRSE_v), 4),
    Media_R2 = round(mean(R2_v), 4)
  )
  
  cat("\nmean values for", loja, ":\n")
  cat("MAE mean:", mean(MAE_v), "\n")
  cat("NMAE mean:", mean(NMAE_v), "\n")
  cat("RRSE mean:", mean(RRSE_v), "\n")
  cat("R2 mean:", mean(R2_v), "\n")
  
  # --- Acumular resultados ---
  resultados_iteracoes <- rbind(resultados_iteracoes, res_loja)
  resultados_media <- rbind(resultados_media, med_loja)
}

# ============================================================
# Resultados finais
# ============================================================
cat("\n============================================================\n")
cat("RESULTADOS FINAIS - MEDIA DAS METRICAS POR LOJA\n")
cat("============================================================\n")
print(resultados_media)
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f

# ============================================================
# Guardar CSV
# ============================================================
<<<<<<< HEAD
cat("############################################################\n")
cat("# RESUMO COMPARATIVO — FASE II (4 LOJAS x 2 ESTRATEGIAS) #\n")
cat("############################################################\n\n")
print(resultados)
=======
write.csv(resultados_iteracoes,
          "backtesting_hw_iteracoes.csv",
          row.names = FALSE)
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f

write.csv(resultados_media,
          "backtesting_hw_media_lojas.csv",
          row.names = FALSE)

cat("\nCSVs guardados na pasta HoltWinters.\n")