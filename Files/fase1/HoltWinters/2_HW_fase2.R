# ============================================================
# Fase II — Backtesting com Growing Window — Holt-Winters
# Target: Num_Customers
# 12 iterações, H=7
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
K <- 7
Test <- 7
S <- 7
Runs <- 12

# --- Lojas ---
lojas_nomes <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
lojas_dados <- list(baltimore, lancaster, philadelphia, richmond)
lojas_cores <- c("steelblue", "tomato", "seagreen", "hotpink")

# --- Data frames para resultados globais ---
resultados_iteracoes <- data.frame()
resultados_media <- data.frame()

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
  
  YR <- diff(range(d1))
  if (YR == 0) YR <- 1
  
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
    
    H <- holdout(d1, ratio = Test, mode = "incremental", iter = b, window = W, increment = S)
    trinit <- H$tr[1]
    dtr <- ts(d1[H$tr], frequency = K)
    Y <- d1[H$ts]
    datas_teste <- datas[H$ts]
    
    # --- Ajustar Holt-Winters ---
    modelo <- suppressWarnings(HoltWinters(dtr))
    fc <- forecast(modelo, h = length(H$ts))
    Pred <- as.numeric(fc$mean[1:Test])
    
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

# ============================================================
# Guardar CSV
# ============================================================
write.csv(resultados_iteracoes,
          "backtesting_hw_iteracoes.csv",
          row.names = FALSE)

write.csv(resultados_media,
          "backtesting_hw_media_lojas.csv",
          row.names = FALSE)

cat("\nCSVs guardados na pasta HoltWinters.\n")