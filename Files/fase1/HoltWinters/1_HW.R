# ============================================================
# Fase I — Holt-Winters para as 4 lojas (USA Stores)
# Objetivo: Previsão univariada de Num_Customers (H=7 dias)
# Método: Holt-Winters (pacote forecast)
# Avaliação: Holdout simples (treino com tudo exceto últimos 7 dias)
# Baseline: Seasonal Naive (repete a semana anterior)
# Nota: Dados pré-tratados (Natal, Black Friday, NA em 2014-04-20)
# ============================================================

# --- Carregar bibliotecas ---
library(forecast)              # HoltWinters, forecast
options(rgl.useNULL = TRUE)    # workaround para macOS (rgl sem OpenGL)
library(rminer)                # mmetric, mgraph

# ============================================================
# Função de tratamento dos dados (do script do grupo)
# ============================================================
preparar_dados <- function(df) {
  df$Date <- as.Date(df$Date)  # converter coluna Date para tipo Date
  
  # --- Natal: substituir valores pela mediana do mesmo dia da semana ---
  natais <- as.Date(c("2012-12-25", "2013-12-25"))
  for (data_natal in natais) {
    data_natal <- as.Date(data_natal, origin = "1970-01-01")
    idx   <- which(df$Date == data_natal)          # índice do dia de Natal
    wday  <- weekdays(data_natal)                  # dia da semana do Natal
    antes <- df[df$Date < data_natal, ]            # dados anteriores ao Natal
    mesmo <- antes[weekdays(antes$Date) == wday, ] # mesmo dia da semana
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
K <- 7                         # período sazonal (semanal)
Test <- K                      # horizonte de previsão H=7

# Lista das 4 lojas
lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")

# Data frame para guardar os resultados de todas as lojas
resultados <- data.frame(
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
  stringsAsFactors = FALSE
)

cat("############################################################\n")
cat("# FASE I — HOLT-WINTERS — HOLDOUT SIMPLES (4 LOJAS)       #\n")
cat("# Dados com tratamento aplicado                            #\n")
cat("############################################################\n\n")

# --- Abrir PDF para guardar todos os gráficos ---
pdf("Files/fase1/HoltWinters/graficos_fase1.pdf", width = 10, height = 6)

# ============================================================
# Ciclo principal: aplicar HW a cada loja
# ============================================================
for (loja in lojas) {
  
  cat("============================================================\n")
  cat("LOJA:", toupper(loja), "\n")
  cat("============================================================\n\n")
  
  # ----------------------------------------------------------
  # 1. Carregar e preparar os dados (com tratamento)
  # ----------------------------------------------------------
  caminho <- paste0("Files/csv/", loja, ".csv")    # caminho do CSV
  dados <- preparar_dados(read.csv(caminho))        # carregar + tratar
  clientes <- dados$Num_Customers                   # extrair variável alvo
  L <- length(clientes)                             # tamanho da série
  
  cat("1. CARREGAMENTO DOS DADOS (tratados)\n")
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
  modelo <- suppressWarnings(HoltWinters(treino_ts))
  variante <- ifelse(modelo$seasonal == "additive", "Aditiva", "Multiplicativa")
  
  cat("3. MODELO HOLT-WINTERS\n")
  cat("   Variante:", variante, "\n")
  cat("   alpha (nível):", round(modelo$alpha, 4), "\n")
  cat("   beta (tendência):", round(modelo$beta, 4), "\n")
  cat("   gamma (sazonalidade):", round(modelo$gamma, 4), "\n\n")
  
  # ----------------------------------------------------------
  # 4. Gerar previsões HW
  # ----------------------------------------------------------
  previsoes <- forecast(modelo, h = Test)$mean
  
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
  cat("\n")
  
  # ----------------------------------------------------------
  # 5. Métricas de avaliação
  # ----------------------------------------------------------
  YR <- diff(range(clientes))   # range global para NMAE
  
  mae  <- mmetric(y = teste, x = previsoes, metric = "MAE")
  nmae <- mmetric(y = teste, x = previsoes, metric = "NMAE", val = YR)
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
  
  # ----------------------------------------------------------
  # 6. Gráfico: real vs previsão
  # ----------------------------------------------------------
  cat("6. GRAFICO (ver painel Plots)\n\n")
  mgraph(y = teste, x = previsoes, graph = "REG", Grid = 10,
         col = c("black", "blue"),
         leg = list(pos = "topleft", leg = c("Real", "Holt-Winters")),
         main = paste("Fase I - Holt-Winters -", toupper(loja)))
  
  # ----------------------------------------------------------
  # 7. Guardar resultados
  # ----------------------------------------------------------
  resultados <- rbind(resultados, data.frame(
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
  ))
}

# --- Fechar PDF ---
dev.off()
cat("Graficos guardados em: Files/fase1/HoltWinters/graficos_fase1.pdf\n\n")

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
cat("------------------------------------------------------------\n")

# ============================================================
# Exportar resultados
# ============================================================
write.csv(resultados, "Files/fase1/HoltWinters/resultados_hw_fase1.csv", row.names = FALSE)
cat("\nResultados exportados para: Files/fase1/HoltWinters/resultados_hw_fase1.csv\n")