# ============================================================
# Fase I — Holt-Winters para as 4 lojas (USA Stores)
# Objetivo: Previsão univariada de Num_Customers (H=7 dias)
# Método: Holt-Winters (pacote forecast)
# Avaliação: Holdout simples (treino com tudo exceto últimos 7 dias)
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
  Loja = character(),
  NMAE = numeric(),
  RMSE = numeric(),
  R2 = numeric(),
  Alpha = numeric(),
  Beta = numeric(),
  Gamma = numeric(),
  Variante = character(),
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
  # 4. Gerar previsões
  # ----------------------------------------------------------
  previsoes <- forecast(modelo, h = Test)$mean
  
  cat("4. PREVISÕES vs VALORES REAIS\n")
  cat("   Dia | Real | Previsto |  Erro\n")
  cat("   ----+------+----------+------\n")
  for (i in 1:Test) {
    erro <- round(teste[i] - previsoes[i], 1)
    cat(sprintf("    %d  | %4d |  %6.1f  | %5.1f\n", i, teste[i], previsoes[i], erro))
  }
  
  neg <- sum(previsoes < 0)
  if (neg > 0) {
    cat("\n   AVISO:", neg, "previsão(ões) negativa(s).\n")
  }
  cat("\n")
  
  # ----------------------------------------------------------
  # 5. Métricas de avaliação
  # ----------------------------------------------------------
  YR <- diff(range(clientes))
  nmae <- mmetric(y = teste, x = previsoes, metric = "NMAE", val = YR)
  rmse <- mmetric(y = teste, x = previsoes, metric = "RMSE")
  r2 <- mmetric(y = teste, x = previsoes, metric = "R2")
  
  cat("5. MÉTRICAS\n")
  cat("   NMAE:", round(nmae, 2), "%\n")
  cat("   RMSE:", round(rmse, 2), "\n")
  cat("   R2:  ", round(r2, 4), "\n\n")
  
  # ----------------------------------------------------------
  # 6. Gráfico: real vs previsão
  # ----------------------------------------------------------
  cat("6. GRÁFICO (ver painel Plots)\n\n")
  mgraph(y = teste, x = previsoes, graph = "REG", Grid = 10,
         col = c("black", "blue"),
         leg = list(pos = "topleft", leg = c("Real", "Holt-Winters")),
         main = paste("Fase I — Holt-Winters —", toupper(loja)))
  
  # ----------------------------------------------------------
  # 7. Guardar resultados
  # ----------------------------------------------------------
  resultados <- rbind(resultados, data.frame(
    Loja = loja, NMAE = round(nmae, 2), RMSE = round(rmse, 2),
    R2 = round(r2, 4), Alpha = round(modelo$alpha, 4),
    Beta = round(modelo$beta, 4), Gamma = round(modelo$gamma, 4),
    Variante = variante, stringsAsFactors = FALSE
  ))
}

# --- Fechar PDF ---
dev.off()
cat("Gráficos guardados em: Files/fase1/HoltWinters/graficos_fase1.pdf\n\n")

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