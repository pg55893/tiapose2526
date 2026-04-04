# ============================================================
# Fase I — Holt-Winters para as 4 lojas (USA Stores)
# Objetivo: Previsão univariada de Num_Customers (H=7 dias)
# Método: Holt-Winters (pacote forecast)
# Avaliação: Holdout simples (treino com tudo exceto últimos 7 dias)
<<<<<<< HEAD
# Baseline: Seasonal Naive (repete a semana anterior)
# Nota: Dados pré-tratados (Natal, Black Friday, NA em 2014-04-20)
=======
# Nota: Dados pré-tratados (Natal, Páscoa, Black Friday, NA em 2014-04-20)
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
# ============================================================

# --- Carregar bibliotecas ---
library(forecast)
options(rgl.useNULL = TRUE)
library(rminer)

# --- Carregar dados tratados (fonte unica para todo o grupo) ---
setwd("~/TIAPOSE2526/Files/csv")
source("~/TIAPOSE2526/Files/tratamentoDeDados.R")

# --- Criar pasta e definir como destino dos resultados ---
dir.create("~/TIAPOSE2526/Files/fase1/HoltWinters", showWarnings=FALSE)
setwd("~/TIAPOSE2526/Files/fase1/HoltWinters")

# --- Datas em que a loja esta fechada (Natal e Pascoa) ---
datas_fecho <- as.Date(c("2012-12-25","2013-12-25","2013-03-31","2014-04-20"))

# --- Configuração geral ---
K <- 7
Test <- K

# Lista das 4 lojas (nomes dos objectos ja carregados)
lojas_nomes <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
lojas_dados <- list(baltimore, lancaster, philadelphia, richmond)

# Data frame para guardar os resultados de todas as lojas
resultados <- data.frame(
<<<<<<< HEAD
  Loja        = character(),
  MAE         = numeric(),
  NMAE        = numeric(),
  RMSE        = numeric(),
  RRSE        = numeric(),
  R2          = numeric(),
  Alpha       = numeric(),
  Beta        = numeric(),
  Gamma       = numeric(),
  Variante    = character(),
  Naive_MAE   = numeric(),     # MAE do baseline Seasonal Naive
  Naive_NMAE  = numeric(),     # NMAE do baseline Seasonal Naive
  Naive_RMSE  = numeric(),     # RMSE do baseline Seasonal Naive
  Naive_RRSE  = numeric(),     # RRSE do baseline Seasonal Naive
=======
  Loja = character(),
  NMAE = numeric(),
  RRSE = numeric(),
  R2 = numeric(),
  Alpha = numeric(),
  Beta = numeric(),
  Gamma = numeric(),
  Variante = character(),
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  stringsAsFactors = FALSE
)

cat("############################################################\n")
cat("# FASE I — HOLT-WINTERS — HOLDOUT SIMPLES (4 LOJAS)       #\n")
cat("# Dados com tratamento aplicado                            #\n")
cat("############################################################\n\n")

# --- Abrir PDF para guardar todos os gráficos ---
pdf("graficos_hw_fase1.pdf", width = 10, height = 6)

# ============================================================
# Ciclo principal: aplicar HW a cada loja
# ============================================================
for (i in seq_along(lojas_nomes)) {
  
  loja <- lojas_nomes[i]
  dados <- lojas_dados[[i]]
  
  cat("============================================================\n")
  cat("LOJA:", toupper(loja), "\n")
  cat("============================================================\n\n")
  
  # ----------------------------------------------------------
  # 1. Preparar dados
  # ----------------------------------------------------------
  dados$Date <- as.Date(dados$Date)
  dados <- dados[order(dados$Date), ]
  clientes <- dados$Num_Customers
  L <- length(clientes)
  
  cat("1. CARREGAMENTO DOS DADOS (tratados)\n")
  cat("   Observacoes:", L, "\n")
  cat("   Datas:", as.character(dados$Date[1]), "a", as.character(dados$Date[L]), "\n")
  cat("   Range Num_Customers:", min(clientes), "a", max(clientes), "\n\n")
  
  # ----------------------------------------------------------
  # 2. Divisão treino/teste (Holdout temporal)
  # ----------------------------------------------------------
  treino <- clientes[1:(L - Test)]
  teste <- clientes[(L - Test + 1):L]
  datas_teste <- dados$Date[(L - Test + 1):L]
  treino_ts <- ts(treino, frequency = K)
  
  cat("2. DIVISAO TREINO/TESTE\n")
  cat("   Treino:", length(treino), "obs |",
      as.character(dados$Date[1]), "a", as.character(dados$Date[L - Test]), "\n")
  cat("   Teste:", length(teste), "obs |",
      as.character(datas_teste[1]), "a", as.character(datas_teste[Test]), "\n\n")
  
  # ----------------------------------------------------------
  # 3. Ajustar modelo Holt-Winters
  # ----------------------------------------------------------
  modelo <- suppressWarnings(HoltWinters(treino_ts))
  variante <- ifelse(modelo$seasonal == "additive", "Aditiva", "Multiplicativa")
  
  cat("3. MODELO HOLT-WINTERS\n")
  cat("   Variante:", variante, "\n")
  cat("   alpha (nivel):", round(modelo$alpha, 4), "\n")
  cat("   beta (tendencia):", round(modelo$beta, 4), "\n")
  cat("   gamma (sazonalidade):", round(modelo$gamma, 4), "\n\n")
  
  # ----------------------------------------------------------
<<<<<<< HEAD
  # 4. Gerar previsões HW
=======
  # 4. Gerar previsões + Pos-processamento
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  # ----------------------------------------------------------
  previsoes <- as.numeric(forecast(modelo, h = Test)$mean)
  
<<<<<<< HEAD
  # ----------------------------------------------------------
  # 4b. Baseline: Seasonal Naive
  # Previsão = últimos K valores do treino (semana anterior)
  # ----------------------------------------------------------
  naive_pred <- treino[(length(treino) - K + 1):length(treino)]  # repete a última semana do treino
  
  cat("4. PREVISÕES vs VALORES REAIS\n")
  cat("   Dia | Real | HW Prev | HW Erro | Naive Prev | Naive Erro\n")
  cat("   ----+------+---------+---------+------------+-----------\n")
  for (i in 1:Test) {
    cat(sprintf("    %d  | %4d |  %6.1f | %7.1f |     %6.1f |     %5.1f\n",
                i, teste[i],
                previsoes[i], round(teste[i] - previsoes[i], 1),
                naive_pred[i], round(teste[i] - naive_pred[i], 1)))
  }
  
  neg <- sum(previsoes < 0)
  if (neg > 0) cat("\n   AVISO:", neg, "previsão(ões) HW negativa(s).\n")
=======
  # --- Pos-processamento ---
  previsoes[previsoes < 0] <- 0
  previsoes[datas_teste %in% datas_fecho] <- 0
  
  cat("4. PREVISOES vs VALORES REAIS\n")
  cat("   Dia | Real | Previsto |  Erro\n")
  cat("   ----+------+----------+------\n")
  for (j in 1:Test) {
    erro <- round(teste[j] - previsoes[j], 1)
    cat(sprintf("    %d  | %4d |  %6.1f  | %5.1f\n", j, teste[j], previsoes[j], erro))
  }
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  cat("\n")
  
  # ----------------------------------------------------------
  # 5. Métricas de avaliação
  # ----------------------------------------------------------
  YR <- diff(range(clientes))   # range global para NMAE
  
  mae  <- mmetric(y = teste, x = previsoes, metric = "MAE")
  nmae <- mmetric(y = teste, x = previsoes, metric = "NMAE", val = YR)
<<<<<<< HEAD
  rmse <- mmetric(y = teste, x = previsoes, metric = "RMSE")
  rrse <- mmetric(y = teste, x = previsoes, metric = "RRSE")
  r2   <- mmetric(y = teste, x = previsoes, metric = "R2")
  
  # Métricas do Naive
  n_mae  <- mmetric(y = teste, x = naive_pred, metric = "MAE")
  n_nmae <- mmetric(y = teste, x = naive_pred, metric = "NMAE", val = YR)
  n_rmse <- mmetric(y = teste, x = naive_pred, metric = "RMSE")
  n_rrse <- mmetric(y = teste, x = naive_pred, metric = "RRSE")
  
  cat("5. METRICAS\n")
  cat("                HW        Naive\n")
  cat(sprintf("   MAE:   %8.1f   %8.1f\n", mae,  n_mae))
  cat(sprintf("   NMAE:  %7.2f%%  %7.2f%%\n", nmae, n_nmae))
  cat(sprintf("   RMSE:  %8.2f   %8.2f\n", rmse, n_rmse))
  cat(sprintf("   RRSE:  %7.2f%%  %7.2f%%\n", rrse, n_rrse))
  cat(sprintf("   R2:    %8.4f\n", r2))
  cat("\n")
=======
  rrse <- mmetric(y = teste, x = previsoes, metric = "RRSE")
  r2 <- 1 - (sum((teste - previsoes)^2) / sum((teste - mean(teste))^2))
  
  cat("5. METRICAS\n")
  cat("   NMAE:", round(nmae, 4), "\n")
  cat("   RRSE:", round(rrse, 4), "\n")
  cat("   R2:  ", round(r2, 4), "\n\n")
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  
  # ----------------------------------------------------------
  # 6. Gráfico: real vs previsão
  # ----------------------------------------------------------
<<<<<<< HEAD
  cat("6. GRAFICO (ver painel Plots)\n\n")
=======
  cat("6. GRAFICO (ver painel Plots / PDF)\n\n")
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  mgraph(y = teste, x = previsoes, graph = "REG", Grid = 10,
         col = c("black", "blue"),
         leg = list(pos = "topleft", leg = c("Real", "Holt-Winters")),
         main = paste("Fase I - Holt-Winters -", toupper(loja)))
  
  # ----------------------------------------------------------
  # 7. Guardar resultados
  # ----------------------------------------------------------
  resultados <- rbind(resultados, data.frame(
<<<<<<< HEAD
    Loja      = loja,
    MAE       = round(mae, 1),
    NMAE      = round(nmae, 2),
    RMSE      = round(rmse, 2),
    RRSE      = round(rrse, 2),
    R2        = round(r2, 4),
    Alpha     = round(modelo$alpha, 4),
    Beta      = round(modelo$beta, 4),
    Gamma     = round(modelo$gamma, 4),
    Variante  = variante,
    Naive_MAE  = round(n_mae, 1),
    Naive_NMAE = round(n_nmae, 2),
    Naive_RMSE = round(n_rmse, 2),
    Naive_RRSE = round(n_rrse, 2),
    stringsAsFactors = FALSE
=======
    Loja = loja, NMAE = round(nmae, 4), RRSE = round(rrse, 4),
    R2 = round(r2, 4), Alpha = round(modelo$alpha, 4),
    Beta = round(modelo$beta, 4), Gamma = round(modelo$gamma, 4),
    Variante = variante, stringsAsFactors = FALSE
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f
  ))
}

# --- Fechar PDF ---
dev.off()
<<<<<<< HEAD
cat("Graficos guardados em: Files/fase1/HoltWinters/graficos_fase1.pdf\n\n")
=======
cat("Graficos guardados em: graficos_hw_fase1.pdf\n\n")
>>>>>>> 063fc5650e7ab46d49e5f41192bd262e2cb3095f

# ============================================================
# Resumo comparativo das 4 lojas
# ============================================================
cat("\n############################################################\n")
cat("# RESUMO COMPARATIVO — FASE I — HOLT-WINTERS (4 LOJAS)   #\n")
cat("############################################################\n\n")
print(resultados)

cat("\n------------------------------------------------------------\n")
cat("Melhor loja por NMAE:", resultados$Loja[which.min(resultados$NMAE)],
    "(", min(resultados$NMAE), ")\n")
cat("Melhor loja por R2:  ", resultados$Loja[which.max(resultados$R2)],
    "(", max(resultados$R2), ")\n")
cat("------------------------------------------------------------\n")

# ============================================================
# Exportar resultados
# ============================================================
write.csv(resultados, "resultados_hw_fase1.csv", row.names = FALSE)
cat("\nResultados exportados para: resultados_hw_fase1.csv\n")