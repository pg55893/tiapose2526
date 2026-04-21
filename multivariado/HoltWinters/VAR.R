# ============================================================
# Fase II — VAR Multivariado
# Target: Num_Customers | H=7 | Growing Window | 12 iterações
# Endógenas: Num_Customers + Sales
# Sem exógenas (limitação do predict.varest)
# Pacote: vars (via autoVAR/forecastVAR do professor)
# ============================================================

library(vars)
library(forecast)
options(rgl.useNULL = TRUE)
library(rminer)

# --- Carregar dados ---
setwd("Files/csv")
source("../tratamentoDeDados.R")
setwd("../../")

# --- Carregar funções do professor ---
source("Files/fase1/Multivariado/multi-utils.R")

# --- Output ---
output_dir <- file.path(getwd(), "Files/fase1/Multivariado/HoltWinters")
dir.create(output_dir, showWarnings = FALSE)

# --- Sink para txt ---
sink(file.path(output_dir, "output_var.txt"), split = TRUE)

# --- Datas de fecho ---
datas_fecho <- as.Date(c("2012-12-25", "2013-12-25", "2013-03-31", "2014-04-20"))

# --- Configuração ---
Test <- 7
S    <- 7
Runs <- 12

# --- Lojas ---
lojas_nomes <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
lojas_dados <- list(baltimore, lancaster, philadelphia, richmond)
lojas_cores <- c("steelblue", "tomato", "seagreen", "hotpink")

# --- Data frames para resultados ---
resultados_iteracoes <- data.frame()
resultados_media     <- data.frame()

cat("############################################################\n")
cat("# FASE II — VAR MULTIVARIADO — GROWING WINDOW             #\n")
cat("# Endógenas: Num_Customers + Sales                        #\n")
cat("# Nota: sem exógenas (limitação do predict.varest)        #\n")
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
  
  nc    <- dados$Num_Customers
  sa    <- dados$Sales
  datas <- dados$Date
  L     <- length(nc)
  
  YR <- diff(range(nc))
  if (YR == 0) YR <- 1
  
  W <- (L - Test) - (Runs - 1) * S
  
  # --- Vetores de métricas ---
  MAE_v  <- numeric(Runs)
  NMAE_v <- numeric(Runs)
  RRSE_v <- numeric(Runs)
  R2_v   <- numeric(Runs)
  
  res_loja <- data.frame()
  
  # ----------------------------------------------------------
  # 2. Loop de backtesting (Growing Window)
  # ----------------------------------------------------------
  for (b in 1:Runs) {
    
    H_idx <- holdout(nc, ratio = Test, mode = "incremental",
                     iter = b, window = W, increment = S)
    
    tr <- H_idx$tr
    ts <- H_idx$ts
    
    datas_teste <- datas[ts]
    Y           <- nc[ts]
    
    # --- Matriz endógena de treino ---
    mtr_train <- cbind(Num_Customers = nc[tr],
                       Sales         = sa[tr])
    
    # --- Ajustar VAR (lags via BIC, dummies sazonais semanais) ---
    modelo_var <- suppressWarnings(
      autoVAR(mtr_train, lag.max = 14, season = 7)
    )
    
    # --- Previsão H=7 ---
    Pred_all <- forecastVAR(modelo_var, h = Test)
    Pred     <- Pred_all[[1]]   # coluna 1 = Num_Customers
    
    # --- Pós-processamento ---
    Pred[Pred < 0]                     <- 0
    Pred[datas_teste %in% datas_fecho] <- 0
    
    # --- Métricas ---
    MAE_v[b]  <- mmetric(y = Y, x = Pred, metric = "MAE")
    NMAE_v[b] <- mmetric(y = Y, x = Pred, metric = "NMAE", val = YR)
    RRSE_v[b] <- mmetric(y = Y, x = Pred, metric = "RRSE")
    R2_v[b]   <- 1 - (sum((Y - Pred)^2) / sum((Y - mean(Y))^2))
    
    lag_sel <- modelo_var$p
    
    # --- Guardar linha ---
    linha <- data.frame(
      Loja        = loja,
      Iteracao    = b,
      Train_Start = datas[tr[1]],
      Train_End   = datas[tr[length(tr)]],
      Train_Size  = length(tr),
      Test_Start  = datas_teste[1],
      Test_End    = datas_teste[length(datas_teste)],
      Test_Size   = length(ts),
      Modelo      = "VAR",
      Lag_VAR     = lag_sel,
      MAE         = round(MAE_v[b],  4),
      NMAE        = round(NMAE_v[b], 4),
      RRSE        = round(RRSE_v[b], 4),
      R2          = round(R2_v[b],   4)
    )
    res_loja <- rbind(res_loja, linha)
    
    cat(sprintf("  iter %2d | TR: %s a %s (n=%d) | lag=%d | NMAE: %.4f | RRSE: %.4f | R2: %.4f\n",
                b,
                as.character(datas[tr[1]]),
                as.character(datas[tr[length(tr)]]),
                length(tr),
                lag_sel,
                NMAE_v[b], RRSE_v[b], R2_v[b]))
  }
  
  # ----------------------------------------------------------
  # 3. Boxplot das métricas
  # ----------------------------------------------------------
  pdf(file.path(output_dir, paste0("boxplot_var_", tolower(loja), ".pdf")),
      width = 7, height = 5)
  boxplot(NMAE_v, RRSE_v, R2_v,
          names = c("NMAE", "RRSE", "R2"),
          main  = paste(loja, "- VAR - Distribuição das Métricas (12 iter)"),
          col   = cor_loja)
  dev.off()
  
  # ----------------------------------------------------------
  # 4. Médias das métricas
  # ----------------------------------------------------------
  med_loja <- data.frame(
    Loja       = loja,
    Target     = "Num_Customers",
    Modelo     = "VAR",
    Metodo     = "Growing Window",
    Iteracoes  = Runs,
    Horizonte  = Test,
    Incremento = S,
    Media_MAE  = round(mean(MAE_v),  4),
    Media_NMAE = round(mean(NMAE_v), 4),
    Media_RRSE = round(mean(RRSE_v), 4),
    Media_R2   = round(mean(R2_v),   4)
  )
  
  cat(sprintf("\n  Médias %s: MAE=%.4f | NMAE=%.4f | RRSE=%.4f | R2=%.4f\n\n",
              loja, mean(MAE_v), mean(NMAE_v), mean(RRSE_v), mean(R2_v)))
  
  resultados_iteracoes <- rbind(resultados_iteracoes, res_loja)
  resultados_media     <- rbind(resultados_media,     med_loja)
}

# ============================================================
# Resultados finais
# ============================================================
cat("\n============================================================\n")
cat("RESULTADOS FINAIS - VAR MULTIVARIADO\n")
cat("============================================================\n")
print(resultados_media)

sink()

# --- Guardar CSVs ---
write.csv(resultados_iteracoes,
          file.path(output_dir, "backtesting_var_iteracoes.csv"),
          row.names = FALSE)
write.csv(resultados_media,
          file.path(output_dir, "backtesting_var_media.csv"),
          row.names = FALSE)

cat("\nCSVs guardados em:", output_dir, "\n")