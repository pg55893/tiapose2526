# ============================================================
# Fase I — Holt-Winters para as 4 lojas (USA Stores)
# Objetivo: Previsão univariada de Num_Customers (H=7 dias)
# Método: Holt-Winters (pacote forecast)
# Avaliação: Holdout simples (treino com tudo exceto últimos 7 dias)
# ============================================================

# --- Carregar bibliotecas ---
library(forecast)              # HoltWinters, forecast
options(rgl.useNULL = TRUE)    # workaround para macOS (rgl sem OpenGL)
library(rminer)                # mmetric, mgraph

# --- Configuração geral ---
K <- 7                         # período sazonal (semanal)
Test <- K                      # horizonte de previsão H=7

# Lista das 4 lojas (nomes dos ficheiros CSV sem extensão)
lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")

# Data frame para guardar os resultados de todas as lojas
resultados <- data.frame(
  Loja = character(),          # nome da loja
  NMAE = numeric(),            # normalized mean absolute error (%)
  RMSE = numeric(),            # root mean squared error (nº clientes)
  R2 = numeric(),              # coeficiente de determinação
  Alpha = numeric(),           # parâmetro de suavização (nível)
  Beta = numeric(),            # parâmetro de suavização (tendência)
  Gamma = numeric(),           # parâmetro de suavização (sazonalidade)
  Variante = character(),      # aditiva ou multiplicativa
  stringsAsFactors = FALSE
)

cat("############################################################\n")
cat("# FASE I — HOLT-WINTERS — HOLDOUT SIMPLES (4 LOJAS)       #\n")
cat("############################################################\n\n")

# ============================================================
# Ciclo principal: aplicar HW a cada loja
# ============================================================
for (loja in lojas) {
  
  cat("============================================================\n")
  cat("LOJA:", toupper(loja), "\n")
  cat("============================================================\n\n")
  
  # ----------------------------------------------------------
  # 1. Carregar e preparar os dados
  # ----------------------------------------------------------
  caminho <- paste0("Files/csv/", loja, ".csv")  # caminho do CSV
  dados <- read.csv(caminho)                      # carregar dataset
  clientes <- dados$Num_Customers                 # extrair variável alvo
  L <- length(clientes)                           # tamanho da série
  
  cat("1. CARREGAMENTO DOS DADOS\n")
  cat("   Observações:", L, "\n")
  cat("   Datas:", as.character(dados$Date[1]), "a", as.character(dados$Date[L]), "\n")
  cat("   Range Num_Customers:", min(clientes), "a", max(clientes), "\n\n")
  
  # ----------------------------------------------------------
  # 2. Divisão treino/teste (Holdout temporal)
  # ----------------------------------------------------------
  treino <- clientes[1:(L - Test)]            # tudo exceto últimos 7 dias
  teste <- clientes[(L - Test + 1):L]         # últimos 7 dias
  treino_ts <- ts(treino, frequency = K)      # objeto ts com sazonalidade semanal
  
  cat("2. DIVISÃO TREINO/TESTE\n")
  cat("   Treino:", length(treino), "obs |",
      as.character(dados$Date[1]), "a", as.character(dados$Date[L - Test]), "\n")
  cat("   Teste:", length(teste), "obs |",
      as.character(dados$Date[L - Test + 1]), "a", as.character(dados$Date[L]), "\n\n")
  
  # ----------------------------------------------------------
  # 3. Ajustar modelo Holt-Winters
  # ----------------------------------------------------------
  # suppressWarnings: o otimizador pode ter dificuldades com séries
  # irregulares (picos de TouristEvent), mas o modelo é criado na mesma
  modelo <- suppressWarnings(HoltWinters(treino_ts))
  
  # Identificar variante selecionada
  variante <- ifelse(modelo$seasonal == "additive", "Aditiva", "Multiplicativa")
  
  cat("3. MODELO HOLT-WINTERS\n")
  cat("   Variante:", variante, "\n")
  cat("   alpha (nível):", round(modelo$alpha, 4), "\n")
  cat("   beta (tendência):", round(modelo$beta, 4), "\n")
  cat("   gamma (sazonalidade):", round(modelo$gamma, 4), "\n\n")
  
  # ----------------------------------------------------------
  # 4. Gerar previsões
  # ----------------------------------------------------------
  previsoes <- forecast(modelo, h = Test)$mean  # prever 7 dias à frente
  
  cat("4. PREVISÕES vs VALORES REAIS\n")
  cat("   Dia | Real | Previsto |  Erro\n")
  cat("   ----+------+----------+------\n")
  for (i in 1:Test) {
    erro <- round(teste[i] - previsoes[i], 1)
    cat(sprintf("    %d  | %4d |  %6.1f  | %5.1f\n", i, teste[i], previsoes[i], erro))
  }
  
  # Verificar se existem previsões negativas
  neg <- sum(previsoes < 0)
  if (neg > 0) {
    cat("\n   AVISO:", neg, "previsão(ões) negativa(s) — não faz sentido para nº clientes.\n")
    cat("   Causa provável: variante aditiva com nível baixo e efeito sazonal negativo.\n")
  }
  cat("\n")
  
  # ----------------------------------------------------------
  # 5. Métricas de avaliação
  # ----------------------------------------------------------
  YR <- diff(range(clientes))  # range global (para NMAE)
  nmae <- mmetric(y = teste, x = previsoes, metric = "NMAE", val = YR)
  rmse <- mmetric(y = teste, x = previsoes, metric = "RMSE")
  r2 <- mmetric(y = teste, x = previsoes, metric = "R2")
  
  cat("5. MÉTRICAS\n")
  cat("   NMAE:", round(nmae, 2), "% (erro médio em % da escala total)\n")
  cat("   RMSE:", round(rmse, 2), "(erro médio em nº de clientes)\n")
  cat("   R2:  ", round(r2, 4), "(proporção de variância explicada)\n\n")
  
  # ----------------------------------------------------------
  # 6. Gráfico: real vs previsão
  # ----------------------------------------------------------
  cat("6. GRÁFICO (ver painel Plots)\n\n")
  mgraph(y = teste, x = previsoes, graph = "REG", Grid = 10,
         col = c("black", "blue"),
         leg = list(pos = "topleft", leg = c("Real", "Holt-Winters")),
         main = paste("Fase I — Holt-Winters —", toupper(loja)))
  
  # Pausa para visualizar o gráfico antes de avançar para a próxima loja
  mpause()
  
  # ----------------------------------------------------------
  # 7. Guardar resultados desta loja
  # ----------------------------------------------------------
  resultados <- rbind(resultados, data.frame(
    Loja = loja,
    NMAE = round(nmae, 2),
    RMSE = round(rmse, 2),
    R2 = round(r2, 4),
    Alpha = round(modelo$alpha, 4),
    Beta = round(modelo$beta, 4),
    Gamma = round(modelo$gamma, 4),
    Variante = variante,
    stringsAsFactors = FALSE
  ))
}

# ============================================================
# Resumo comparativo das 4 lojas
# ============================================================
cat("\n############################################################\n")
cat("# RESUMO COMPARATIVO — FASE I — HOLT-WINTERS (4 LOJAS)   #\n")
cat("############################################################\n\n")
print(resultados)

cat("\n------------------------------------------------------------\n")
cat("Melhor loja por NMAE:", resultados$Loja[which.min(resultados$NMAE)],
    "(", min(resultados$NMAE), "% )\n")
cat("Melhor loja por R2:  ", resultados$Loja[which.max(resultados$R2)],
    "(", max(resultados$R2), ")\n")
cat("------------------------------------------------------------\n\n")

cat("CONCLUSÃO FASE I:\n")
cat("O Holt-Winters com sazonalidade semanal (K=7) foi aplicado às 4 lojas\n")
cat("usando holdout temporal simples (treino: 707 dias, teste: 7 dias).\n")
cat("Os resultados variam entre lojas, refletindo diferenças na regularidade\n")
cat("do padrão semanal e na influência de fatores como TouristEvent.\n")
cat("A Fase II aplicará backtesting (growing/rolling window) para uma\n")
cat("avaliação mais robusta com múltiplas iterações.\n")

# ============================================================
# Exportar resultados para CSV
# ============================================================
write.csv(resultados, "Files/fase1/HoltWinters/resultados_hw_fase1.csv", row.names = FALSE)
cat("\nResultados exportados para: Files/fase1/HoltWinters/resultados_hw_fase1.csv\n")