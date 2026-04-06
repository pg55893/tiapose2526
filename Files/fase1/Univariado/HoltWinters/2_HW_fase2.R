# ============================================================
# Fase II — Holt-Winters — Backtesting Growing Window
# Target: Num_Customers | H=7 | 12 iterações
# Baseline: Seasonal Naive (repete a semana anterior)
# Nota: Dados pré-tratados (Natal, Páscoa, Black Friday)
# ============================================================

library(forecast)
options(rgl.useNULL = TRUE)
library(rminer)

# --- Carregar dados ---
setwd("Files/csv")
source("../tratamentoDeDados.R")
setwd("../../")

# --- Output ---
output_dir <- file.path(getwd(), "Files/fase1/Univariado/HoltWinters")
dir.create(output_dir, showWarnings = FALSE)

# --- Sink para txt ---
sink(file.path(output_dir, "output_hw_fase2.txt"), split = TRUE)

# --- Datas de fecho ---
datas_fecho <- as.Date(c("2012-12-25", "2013-12-25", "2013-03-31", "2014-04-20"))

# --- Configuração ---
K    <- 7    # sazonalidade semanal
Test <- 7    # horizonte H=7
S    <- 7    # salto entre iterações
Runs <- 12   # número de iterações

# --- Lojas ---
lojas_nomes <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
lojas_dados <- list(baltimore, lancaster, philadelphia, richmond)
lojas_cores <- c("steelblue", "tomato", "seagreen", "hotpink")

# --- Data frames para resultados ---
resultados_iteracoes <- data.frame()
resultados_media     <- data.frame()

cat("############################################################\n")
cat("# FASE II — HOLT-WINTERS — BACKTESTING GROWING WINDOW     #\n")
cat("# 12 iterações, H=7, Target: Num_Customers                #\n")
cat("############################################################\n\n")

# ============================================================
# Ciclo principal por loja
# ============================================================
for (i in seq_along(lojas_nomes)) {
  
  loja     <- lojas_nomes[i]
  dados    <- lojas_dados[[i]]
  cor_loja <- lojas_cores[i]
  
  cat("\n============================================================\n")
  cat("BACKTESTING -", toupper(loja), "\n")
  cat("============================================================\n")
  
  # ----------------------------------------------------------
  # 1. Preparar dados
  # ----------------------------------------------------------
  dados$Date <- as.Date(dados$Date)
  dados      <- dados[order(dados$Date), ]
  d1         <- dados$Num_Customers
  datas      <- dados$Date
  L          <- length(d1)
  
  W  <- (L - Test) - (Runs - 1) * S   # janela inicial de treino
  YR <- diff(range(d1))                # range para NMAE
  if (YR == 0) YR <- 1
  
  # --- Vetores de métricas HW ---
  MAE_v  <- numeric(Runs)
  NMAE_v <- numeric(Runs)
  RRSE_v <- numeric(Runs)
  R2_v   <- numeric(Runs)
  
  # --- Vetores de métricas Naive ---
  N_MAE_v  <- numeric(Runs)
  N_NMAE_v <- numeric(Runs)
  N_RRSE_v <- numeric(Runs)
  
  res_loja <- data.frame()
  
  # ----------------------------------------------------------
  # 2. Loop de backtesting (Growing Window)
  # ----------------------------------------------------------
  for (b in 1:Runs) {
    
    H_idx <- holdout(d1, ratio = Test, mode = "incremental",
                     iter = b, window = W, increment = S)
    
    dtr         <- ts(d1[H_idx$tr], frequency = K)   # treino como ts
    Y           <- d1[H_idx$ts]                       # valores reais de teste
    datas_teste <- datas[H_idx$ts]
    
    # --- Holt-Winters ---
    modelo <- suppressWarnings(HoltWinters(dtr))
    Pred   <- as.numeric(forecast(modelo, h = Test)$mean)
    
    # --- Seasonal Naive ---
    naive_pred <- d1[H_idx$tr[(length(H_idx$tr) - K + 1):length(H_idx$tr)]]
    
    # --- Pós-processamento HW ---
    Pred[Pred < 0]                     <- 0
    Pred[datas_teste %in% datas_fecho] <- 0
    
    # --- Pós-processamento Naive ---
    naive_pred[datas_teste %in% datas_fecho] <- 0
    
    # --- Métricas HW ---
    MAE_v[b]  <- mmetric(y = Y, x = Pred,       metric = "MAE")
    NMAE_v[b] <- mmetric(y = Y, x = Pred,       metric = "NMAE", val = YR)
    RRSE_v[b] <- mmetric(y = Y, x = Pred,       metric = "RRSE")
    R2_v[b]   <- 1 - (sum((Y - Pred)^2)       / sum((Y - mean(Y))^2))
    
    # --- Métricas Naive ---
    N_MAE_v[b]  <- mmetric(y = Y, x = naive_pred, metric = "MAE")
    N_NMAE_v[b] <- mmetric(y = Y, x = naive_pred, metric = "NMAE", val = YR)
    N_RRSE_v[b] <- mmetric(y = Y, x = naive_pred, metric = "RRSE")
    
    # --- Guardar linha ---
    linha <- data.frame(
      Loja        = loja,
      Iteracao    = b,
      Train_Start = datas[H_idx$tr[1]],
      Train_End   = datas[H_idx$tr[length(H_idx$tr)]],
      Train_Size  = length(H_idx$tr),
      Test_Start  = datas_teste[1],
      Test_End    = datas_teste[length(datas_teste)],
      Test_Size   = length(H_idx$ts),
      Modelo      = "HoltWinters",
      Variante    = ifelse(modelo$seasonal == "additive", "Aditiva", "Multiplicativa"),
      MAE         = round(MAE_v[b],    4),
      NMAE        = round(NMAE_v[b],   4),
      RRSE        = round(RRSE_v[b],   4),
      R2          = round(R2_v[b],     4),
      Naive_MAE   = round(N_MAE_v[b],  4),
      Naive_NMAE  = round(N_NMAE_v[b], 4),
      Naive_RRSE  = round(N_RRSE_v[b], 4)
    )
    res_loja <- rbind(res_loja, linha)
    
    cat(sprintf("  iter %2d | TR: %s a %s (n=%d) | NMAE: %.4f | RRSE: %.4f | R2: %.4f | Naive NMAE: %.4f\n",
                b,
                as.character(datas[H_idx$tr[1]]),
                as.character(datas[H_idx$tr[length(H_idx$tr)]]),
                length(H_idx$tr),
                NMAE_v[b], RRSE_v[b], R2_v[b], N_NMAE_v[b]))
  }
  
  # ----------------------------------------------------------
  # 3. Boxplot das métricas
  # ----------------------------------------------------------
  pdf(file.path(output_dir, paste0("boxplot_hw_", tolower(loja), ".pdf")),
      width = 7, height = 5)
  boxplot(NMAE_v, RRSE_v, R2_v,
          names = c("NMAE", "RRSE", "R2"),
          main  = paste(loja, "- Holt-Winters - Distribuicao das Metricas (12 iteracoes)"),
          col   = cor_loja)
  dev.off()
  
  # ----------------------------------------------------------
  # 4. Médias das métricas
  # ----------------------------------------------------------
  med_loja <- data.frame(
    Loja        = loja,
    Target      = "Num_Customers",
    Modelo      = "HoltWinters",
    Metodo      = "Growing Window",
    Iteracoes   = Runs,
    Horizonte   = Test,
    Incremento  = S,
    Media_MAE   = round(mean(MAE_v),    4),
    Media_NMAE  = round(mean(NMAE_v),   4),
    Media_RRSE  = round(mean(RRSE_v),   4),
    Media_R2    = round(mean(R2_v),     4),
    Naive_NMAE  = round(mean(N_NMAE_v), 4),
    Naive_RRSE  = round(mean(N_RRSE_v), 4)
  )
  
  cat(sprintf("\n  Médias %s | HW: NMAE=%.4f RRSE=%.4f R2=%.4f | Naive: NMAE=%.4f RRSE=%.4f\n\n",
              loja,
              mean(NMAE_v), mean(RRSE_v), mean(R2_v),
              mean(N_NMAE_v), mean(N_RRSE_v)))
  
  resultados_iteracoes <- rbind(resultados_iteracoes, res_loja)
  resultados_media     <- rbind(resultados_media,     med_loja)
}

# ============================================================
# Resultados finais
# ============================================================
cat("\n============================================================\n")
cat("RESULTADOS FINAIS - MÉDIA DAS MÉTRICAS POR LOJA\n")
cat("============================================================\n")
print(resultados_media)

sink()

# --- Guardar CSVs ---
write.csv(resultados_iteracoes,
          file.path(output_dir, "backtesting_hw_iteracoes.csv"),
          row.names = FALSE)
write.csv(resultados_media,
          file.path(output_dir, "backtesting_hw_media_lojas.csv"),
          row.names = FALSE)

cat("\nCSVs guardados em:", output_dir, "\n")