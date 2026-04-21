# =============================================================================
# UNIVARIADO - ARIMA
# FASE I : Holdout simples
# Target : Num_Customers
# Horizon: H = 7 dias
# Baseline: Seasonal Naive
# Lojas  : Baltimore, Lancaster, Philadelphia, Richmond
# =============================================================================

options(rgl.useNULL = TRUE)

library(forecast)
library(rminer)

# -----------------------------------------------------------------------------
# 1. CARREGAR DADOS TRATADOS
# -----------------------------------------------------------------------------
setwd("~/TIAPOSE_projeto/tiapose2526/Files/csv")
source("~/TIAPOSE_projeto/tiapose2526/Files/tratamentoDeDados.R")

# -----------------------------------------------------------------------------
# 2. PASTA DE RESULTADOS
# -----------------------------------------------------------------------------
out_dir <- "~/TIAPOSE_projeto/tiapose2526/Files//fase1/Univariado/ARIMA"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
setwd(out_dir)

# -----------------------------------------------------------------------------
# 3. CONFIGURACAO GLOBAL
# -----------------------------------------------------------------------------
H <- 7                 # horizonte de previsao
K <- 7                 # sazonalidade semanal

datas_fecho <- as.Date(c(
  "2012-12-25",
  "2013-03-31",
  "2013-12-25",
  "2014-04-20"
))

stores <- list(
  Baltimore    = baltimore,
  Lancaster    = lancaster,
  Philadelphia = philadelphia,
  Richmond     = richmond
)

# -----------------------------------------------------------------------------
# 4. FUNCOES AUXILIARES
# -----------------------------------------------------------------------------
calc_srange <- function(y) {
  srange <- diff(range(y, na.rm = TRUE))
  if (is.na(srange) || srange == 0) srange <- 1
  return(srange)
}

ajustar_arima <- function(y, frequency = 7) {
  ts_train <- ts(y, frequency = frequency)
  suppressWarnings(auto.arima(ts_train, seasonal = TRUE))
}

prever_arima <- function(modelo, h = 7) {
  as.numeric(forecast(modelo, h = h)$mean)
}

prever_snaive <- function(y, h = 7, frequency = 7) {
  ts_train <- ts(y, frequency = frequency)
  as.numeric(forecast(snaive(ts_train, h = h), h = h)$mean)
}

ajustar_previsoes <- function(pred, datas_teste, datas_fecho) {
  pred <- as.numeric(pred)
  pred[pred < 0] <- 0
  pred[datas_teste %in% datas_fecho] <- 0
  return(pred)
}

calcular_metricas <- function(real, pred, y_range) {
  data.frame(
    MAE  = mmetric(real, pred, metric = "MAE"),
    NMAE = mmetric(real, pred, metric = "NMAE", val = y_range),
    RMSE = mmetric(real, pred, metric = "RMSE"),
    RRSE = mmetric(real, pred, metric = "RRSE"),
    R2   = mmetric(real, pred, metric = "R22")
  )
}

# -----------------------------------------------------------------------------
# 5. FASE I - HOLDOUT SIMPLES
# -----------------------------------------------------------------------------
cat("\n=============================================================\n")
cat(" FASE I - Holdout simples (ultimos 7 dias como teste)\n")
cat("=============================================================\n")

pdf("graficos_arima_fase1.pdf", width = 10, height = 6)

resultados_fase1 <- list()
previsoes_fase1  <- list()

for (nome in names(stores)) {
  
  cat("\n--- Loja:", nome, "---\n")
  
  d <- stores[[nome]]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  
  y <- d$Num_Customers
  datas <- d$Date
  
  if (length(y) <= H) {
    warning(paste("Loja", nome, "nao tem observacoes suficientes."))
    next
  }
  
  y_range <- calc_srange(y)
  
  # Holdout simples ordenado: treino = dados antigos; teste = ultimos H
  hd <- holdout(y, ratio = H, mode = "order")
  
  train <- y[hd$tr]
  real  <- y[hd$ts]
  datas_teste <- datas[hd$ts]
  
  # ---- Seasonal Naive ----
  pred_snaive <- prever_snaive(train, h = H, frequency = K)
  pred_snaive <- ajustar_previsoes(pred_snaive, datas_teste, datas_fecho)
  met_snaive  <- calcular_metricas(real, pred_snaive, y_range)
  
  cat(sprintf(
    "  Seasonal Naive | MAE=%.4f | NMAE=%.4f | RMSE=%.4f | RRSE=%.4f | R2=%.4f\n",
    met_snaive$MAE, met_snaive$NMAE, met_snaive$RMSE, met_snaive$RRSE, met_snaive$R2
  ))
  
  # ---- ARIMA ----
  modelo_arima <- ajustar_arima(train, frequency = K)
  pred_arima   <- prever_arima(modelo_arima, h = H)
  pred_arima   <- ajustar_previsoes(pred_arima, datas_teste, datas_fecho)
  met_arima    <- calcular_metricas(real, pred_arima, y_range)
  
  ord <- arimaorder(modelo_arima)
  modelo_txt <- paste0("ARIMA(", ord[1], ",", ord[2], ",", ord[3], ")")
  
  cat(sprintf(
    "  %-15s | MAE=%.4f | NMAE=%.4f | RMSE=%.4f | RRSE=%.4f | R2=%.4f\n",
    modelo_txt, met_arima$MAE, met_arima$NMAE, met_arima$RMSE, met_arima$RRSE, met_arima$R2
  ))
  
  # ---- Grafico ----
  yrange <- range(real, pred_snaive, pred_arima, na.rm = TRUE)
  
  plot(1:H, real, type = "b", pch = 19, col = "black",
       ylim = yrange,
       xlab = "Dia do horizonte",
       ylab = "Num_Customers",
       main = paste("Fase I -", nome, "| ARIMA vs Seasonal Naive"))
  lines(1:H, pred_snaive, type = "b", pch = 2, col = "gray50", lty = 2)
  lines(1:H, pred_arima, type = "b", pch = 17, col = "blue", lty = 1)
  legend("topright", bty = "n",
         legend = c("Real", "Seasonal Naive", "ARIMA"),
         col = c("black", "gray50", "blue"),
         pch = c(19, 2, 17), lty = c(1, 2, 1))
  
  # ---- Guardar resultados ----
  resultados_fase1[[nome]] <- rbind(
    data.frame(
      Loja   = nome,
      Metodo = "Seasonal_Naive",
      Modelo = "SNaive(7)",
      met_snaive
    ),
    data.frame(
      Loja   = nome,
      Metodo = "ARIMA",
      Modelo = modelo_txt,
      met_arima
    )
  )
  
  previsoes_fase1[[nome]] <- data.frame(
    Loja                 = nome,
    Date                 = datas_teste,
    Real                 = real,
    Previsto_ARIMA       = round(pred_arima, 2),
    Previsto_SNaive      = round(pred_snaive, 2),
    Erro_Absoluto_ARIMA  = round(abs(real - pred_arima), 2),
    Erro_Absoluto_SNaive = round(abs(real - pred_snaive), 2)
  )
}

dev.off()

# -----------------------------------------------------------------------------
# 6. EXPORTAR RESULTADOS
# -----------------------------------------------------------------------------
df_fase1 <- do.call(rbind, resultados_fase1)

cat("\n\n====== TABELA FASE I ======\n")
print(df_fase1, row.names = FALSE, digits = 4)

write.csv(df_fase1, "resultados_arima_fase1.csv", row.names = FALSE)

for (nome in names(previsoes_fase1)) {
  write.csv(
    previsoes_fase1[[nome]],
    paste0("previsoes_fase1_", tolower(nome), ".csv"),
    row.names = FALSE
  )
}

cat("\nFase I concluida. CSVs e PDF guardados em:", out_dir, "\n")