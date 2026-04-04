# =============================================================================
# JOÃO — MLPE Multivariado: Cenário 1 (exógenas) e Cenário 2 (exógenas + Sales)
# Target  : Num_Customers, H = 7 dias à frente
# Baseline: Seasonal Naive (repete a semana anterior)
# Fase II : backtesting growing window, 12 iterações
# Métricas: NMAE, RRSE, R² (agregadas via mediana)
# Pós-proc: negativos → 0, Natal/Páscoa → 0
# 4 lojas : Philadelphia, Baltimore, Richmond, Lancaster
# =============================================================================

# --- Pacotes ------------------------------------------------------------------
# install.packages("rminer")
options(rgl.useNULL = TRUE) # macOS Tahoe OpenGL fix
library(rminer)

# --- Tratamento de dados ------------------------------------------------------
source("~/Desktop/Mestrado/TIAPOSE/project/Files/tratamentoDeDados.R")

# --- Carregar multi-utils.R do professor ---
source("~/Desktop/Mestrado/TIAPOSE/project/Files/fase1/Multivariado/multi-utils.R")

# --- Configuração global ------------------------------------------------------
H    <- 7       # horizonte de previsão
RUNS <- 12      # iterações de backtesting
S    <- 7       # passo entre janelas

# Lags definidos pela análise CCF:
LAGS_NC    <- c(1, 2, 3, 7, 28)  # lags de Num_Customers (igual ao univariado)
LAGS_SALES <- c(1, 7)            # lags de Sales (CCF: lag1=0.44, lag7=0.23)

# Usar directamente os data.frames já tratados (do tratamentoDeDados.R)
stores <- list(
  Philadelphia = philadelphia,
  Baltimore    = baltimore,
  Richmond     = richmond,
  Lancaster    = lancaster
)

# --- Funções auxiliares -------------------------------------------------------

# Seasonal Naive: repete os últimos H valores (semana anterior)
seasonal_naive <- function(train, h = 7) {
  tail(train, h)
}

# Pós-processamento: negativos → 0, Natal e Páscoa → 0
post_process <- function(pred, dates_pred) {
  pred[pred < 0] <- 0
  # Natal: 25 Dez
  natal <- format(dates_pred, "%m-%d") == "12-25"
  # Páscoa: datas fixas nos anos dos dados (2012-2014)
  pascoa_dates <- as.Date(c("2013-03-31", "2014-04-20", "2015-04-05",
                            "2012-04-08"))
  pascoa <- dates_pred %in% pascoa_dates
  pred[natal | pascoa] <- 0
  return(pred)
}

# Previsão multi-step recursiva para dataset com exógenas (Cenário 1).
# O lforecast do rminer não sabe lidar com colunas exógenas misturadas com lags,
# por isso fazemos o recursive forecasting manualmente:
#   - Passo a passo, prevemos y(t+1), depois actualizamos os lags com y previsto,
#     mantendo as exógenas do dia futuro (que são conhecidas).
lforecast_exo <- function(model, D_full, start, horizon, lags_used, lag_cols) {
  preds <- numeric(horizon)
  
  # Histórico de y: últimos max(lags_used) valores reais antes do período de teste
  maxlag <- max(lags_used)
  y_history <- D_full$y[(start - maxlag):(start - 1)]
  
  for (step in 1:horizon) {
    # Construir a linha de input para este passo
    inp <- D_full[start + step - 1, , drop = FALSE]
    
    # Actualizar as colunas de lags: usar reais + previsões anteriores
    if (step == 1) {
      full_hist <- y_history
    } else {
      full_hist <- c(y_history, preds[1:(step - 1)])
    }
    n_hist <- length(full_hist)
    
    for (i in seq_along(lags_used)) {
      lag_val <- lags_used[i]
      idx <- n_hist - lag_val + 1
      if (idx > 0 && idx <= n_hist) {
        inp[, lag_cols[i]] <- full_hist[idx]
      }
    }
    
    # Exógenas: mantemos os valores reais de D_full (conhecidos no futuro)
    preds[step] <- predict(model, inp)
  }
  return(preds)
}

# =============================================================================
# CENÁRIO 1 — MLPE + Exógenas puras (TouristEvent, Num_Employees, Pct_On_Sale)
# Abordagem "flat": CasesSeries(lags NC) + colunas exógenas do dia actual.
# Exógenas são conhecidas no futuro → usamos lforecast_exo (recursivo manual).
# =============================================================================

run_cenario1 <- function(store_name, d) {
  cat("\n============================================================\n")
  cat(" CENÁRIO 1 — mlpe + exógenas | Loja:", store_name, "\n")
  cat("============================================================\n")
  
  d$Date <- as.Date(d$Date)
  L   <- nrow(d)
  TS  <- d$Num_Customers
  YR  <- diff(range(TS)) # para NMAE normalização global
  
  # Tratar TouristEvent como dummy numérica
  d$TouristDummy <- ifelse(d$TouristEvent == "Yes", 1, 0)
  
  # Tratar NAs em Pct_On_Sale (se existirem)
  d$Pct_On_Sale[is.na(d$Pct_On_Sale)] <- median(d$Pct_On_Sale, na.rm = TRUE)
  
  # Construir dataset: lags de Num_Customers + exógenas
  maxlag <- max(LAGS_NC) # = 28
  D_lags <- CasesSeries(TS, LAGS_NC)
  n_D    <- nrow(D_lags) # = L - maxlag
  
  # Identificar nomes das colunas de lags
  lag_col_names <- names(D_lags)[names(D_lags) != "y"]
  exo_col_names <- c("Num_Employees", "Pct_On_Sale", "TouristDummy")
  
  # Alinhar exógenas com D_lags (tirar as primeiras maxlag linhas)
  exo_aligned <- d[(maxlag + 1):L, exo_col_names]
  rownames(exo_aligned) <- NULL
  dates_aligned <- d$Date[(maxlag + 1):L]
  
  # Dataset completo: lags + exógenas + target
  D_full <- cbind(D_lags[, lag_col_names], exo_aligned, y = D_lags$y)
  
  # Growing window: janela inicial de treino
  W_init <- n_D - H * RUNS
  cat("  Tamanho D_full:", n_D, "| Janela inicial treino:", W_init, "\n")
  
  # Vectores para métricas
  nmae_mlpe  <- numeric(RUNS); nmae_naive  <- numeric(RUNS)
  rrse_mlpe  <- numeric(RUNS); rrse_naive  <- numeric(RUNS)
  r2_mlpe    <- numeric(RUNS); r2_naive    <- numeric(RUNS)
  all_pred   <- c(); all_naive <- c(); all_y <- c()
  
  for (b in 1:RUNS) {
    tr_end   <- W_init + (b - 1) * H
    ts_start <- tr_end + 1
    ts_end   <- tr_end + H
    
    tr_idx <- 1:tr_end
    ts_idx <- ts_start:ts_end
    
    real <- D_full[ts_idx, ]$y
    dates_test <- dates_aligned[ts_idx]
    
    # ---- mlpe (previsão recursiva com exógenas) ----
    model <- fit(y ~ ., D_full[tr_idx, ], model = "mlpe")
    pred  <- lforecast_exo(model, D_full, start = ts_start, horizon = H,
                           lags_used = LAGS_NC, lag_cols = lag_col_names)
    pred  <- post_process(pred, dates_test)
    
    # ---- Seasonal Naive ----
    train_y    <- D_full[tr_idx, ]$y
    pred_naive <- seasonal_naive(train_y, H)
    pred_naive <- post_process(pred_naive, dates_test)
    
    # ---- Métricas ----
    nmae_mlpe[b]  <- mmetric(real, pred,       metric = "NMAE", val = YR)
    rrse_mlpe[b]  <- mmetric(real, pred,       metric = "RRSE")
    r2_mlpe[b]    <- mmetric(real, pred,       metric = "R22")
    nmae_naive[b] <- mmetric(real, pred_naive, metric = "NMAE", val = YR)
    rrse_naive[b] <- mmetric(real, pred_naive, metric = "RRSE")
    r2_naive[b]   <- mmetric(real, pred_naive, metric = "R22")
    
    all_pred  <- c(all_pred, pred)
    all_naive <- c(all_naive, pred_naive)
    all_y     <- c(all_y, real)
    
    cat(sprintf("  iter %2d | treino: %3d | NMAE: mlpe=%.2f%% naive=%.2f%%\n",
                b, length(tr_idx), nmae_mlpe[b], nmae_naive[b]))
  }
  
  # --- Resumo ---
  cat("\n  --- Medianas", store_name, "---\n")
  cat(sprintf("  mlpe+exo : NMAE=%.2f%% | RRSE=%.3f | R2=%.3f\n",
              median(nmae_mlpe), median(rrse_mlpe), median(r2_mlpe)))
  cat(sprintf("  S.Naive  : NMAE=%.2f%% | RRSE=%.3f | R2=%.3f\n",
              median(nmae_naive), median(rrse_naive), median(r2_naive)))
  
  # --- Gráfico ---
  main_title <- sprintf("Cen.1 mlpe+exo — %s | Med.NMAE=%.1f%%",
                        store_name, median(nmae_mlpe))
  mgraph(all_y, all_pred, graph = "REG", main = main_title, Grid = 10,
         col = c("black", "blue"),
         leg = list(pos = "topleft", leg = c("target", "mlpe+exo")))
  
  return(data.frame(
    Loja     = store_name,
    Cenario  = "C1_exo",
    Iteracao = 1:RUNS,
    NMAE_mlpe = nmae_mlpe, RRSE_mlpe = rrse_mlpe, R2_mlpe = r2_mlpe,
    NMAE_naive = nmae_naive, RRSE_naive = rrse_naive, R2_naive = r2_naive
  ))
}

# =============================================================================
# CENÁRIO 2 — MLPE + Exógenas + Sales (endógena)
# Usa mfit/lforecastm do professor (multi-utils.R):
#   - Variáveis endógenas: Num_Customers, Sales (2 séries em mtr)
#   - Variáveis exógenas:  TouristDummy, Num_Employees, Pct_On_Sale
#   - VINP define os lags por variável endógena
#   - lforecastm trata o recursive forecasting de Sales automaticamente
# =============================================================================

run_cenario2 <- function(store_name, d) {
  cat("\n============================================================\n")
  cat(" CENÁRIO 2 — mlpe + exógenas + Sales | Loja:", store_name, "\n")
  cat("============================================================\n")
  
  d$Date <- as.Date(d$Date)
  L   <- nrow(d)
  YR  <- diff(range(d$Num_Customers))
  
  # Preparar variáveis
  d$TouristDummy <- ifelse(d$TouristEvent == "Yes", 1, 0)
  d$Pct_On_Sale[is.na(d$Pct_On_Sale)] <- median(d$Pct_On_Sale, na.rm = TRUE)
  
  # Endógenas: Num_Customers (col 1) e Sales (col 2)
  endo <- cbind(Num_Customers = d$Num_Customers, Sales = d$Sales)
  
  # Exógenas: TouristDummy, Num_Employees, Pct_On_Sale
  exo  <- cbind(TouristDummy  = d$TouristDummy,
                Num_Employees = d$Num_Employees,
                Pct_On_Sale   = d$Pct_On_Sale)
  
  # VINP: define lags para cada modelo endógeno
  VINP <- vector("list", length = 2)
  VINP[[1]] <- list(LAGS_NC, LAGS_SALES)        # prever Num_Customers
  VINP[[2]] <- list(c(1, 7), c(1, 2, 3, 7))     # prever Sales (auxiliar)
  
  # lagmax necessário para mfit
  lagmax <- max(unlist(VINP))  # = 28
  
  dates_aligned <- d$Date[(lagmax + 1):L]
  
  # Growing window
  n_eff  <- L - lagmax
  W_init <- n_eff - H * RUNS
  
  cat("  Dados:", L, "| lagmax:", lagmax, "| Linhas efectivas:", n_eff,
      "| Janela inicial:", W_init, "\n")
  
  # Vectores para métricas (só Num_Customers)
  nmae_mlpe  <- numeric(RUNS); nmae_naive  <- numeric(RUNS)
  rrse_mlpe  <- numeric(RUNS); rrse_naive  <- numeric(RUNS)
  r2_mlpe    <- numeric(RUNS); r2_naive    <- numeric(RUNS)
  all_pred   <- c(); all_naive <- c(); all_y <- c()
  
  for (b in 1:RUNS) {
    tr_end_orig   <- lagmax + W_init + (b - 1) * H
    ts_start_orig <- tr_end_orig + 1
    ts_end_orig   <- tr_end_orig + H
    
    tr_idx_orig <- 1:tr_end_orig
    ts_idx_orig <- ts_start_orig:ts_end_orig
    
    # Treino
    mtr_b    <- endo[tr_idx_orig, ]
    exo_tr_b <- exo[tr_idx_orig, ]
    
    # Teste
    real_nc    <- d$Num_Customers[ts_idx_orig]
    dates_test <- d$Date[ts_idx_orig]
    exo_ts_b   <- exo[ts_idx_orig, ]
    
    # ---- mfit: treinar modelos mlpe multivariados ----
    MNN <- mfit(mtr_b, "mlpe", VINP, exogen = exo_tr_b)
    
    # ---- lforecastm: previsões multi-step (recursive para Sales) ----
    Pred    <- lforecastm(MNN, h = H, exogen = exo_ts_b)
    pred_nc <- Pred[[1]]
    pred_nc <- post_process(pred_nc, dates_test)
    
    # ---- Seasonal Naive ----
    train_nc   <- d$Num_Customers[tr_idx_orig]
    pred_naive <- seasonal_naive(train_nc, H)
    pred_naive <- post_process(pred_naive, dates_test)
    
    # ---- Métricas ----
    nmae_mlpe[b]  <- mmetric(real_nc, pred_nc,    metric = "NMAE", val = YR)
    rrse_mlpe[b]  <- mmetric(real_nc, pred_nc,    metric = "RRSE")
    r2_mlpe[b]    <- mmetric(real_nc, pred_nc,    metric = "R22")
    nmae_naive[b] <- mmetric(real_nc, pred_naive, metric = "NMAE", val = YR)
    rrse_naive[b] <- mmetric(real_nc, pred_naive, metric = "RRSE")
    r2_naive[b]   <- mmetric(real_nc, pred_naive, metric = "R22")
    
    all_pred  <- c(all_pred, pred_nc)
    all_naive <- c(all_naive, pred_naive)
    all_y     <- c(all_y, real_nc)
    
    cat(sprintf("  iter %2d | treino: %3d | NMAE: mlpe=%.2f%% naive=%.2f%%\n",
                b, length(tr_idx_orig), nmae_mlpe[b], nmae_naive[b]))
  }
  
  # --- Resumo ---
  cat("\n  --- Medianas", store_name, "---\n")
  cat(sprintf("  mlpe+exo+Sales : NMAE=%.2f%% | RRSE=%.3f | R2=%.3f\n",
              median(nmae_mlpe), median(rrse_mlpe), median(r2_mlpe)))
  cat(sprintf("  S.Naive        : NMAE=%.2f%% | RRSE=%.3f | R2=%.3f\n",
              median(nmae_naive), median(rrse_naive), median(r2_naive)))
  
  # --- Gráfico ---
  main_title <- sprintf("Cen.2 mlpe+exo+Sales — %s | Med.NMAE=%.1f%%",
                        store_name, median(nmae_mlpe))
  mgraph(all_y, all_pred, graph = "REG", main = main_title, Grid = 10,
         col = c("black", "purple"),
         leg = list(pos = "topleft", leg = c("target", "mlpe+exo+Sales")))
  
  return(data.frame(
    Loja     = store_name,
    Cenario  = "C2_exo_sales",
    Iteracao = 1:RUNS,
    NMAE_mlpe = nmae_mlpe, RRSE_mlpe = rrse_mlpe, R2_mlpe = r2_mlpe,
    NMAE_naive = nmae_naive, RRSE_naive = rrse_naive, R2_naive = r2_naive
  ))
}

# =============================================================================
# EXECUÇÃO PRINCIPAL
# =============================================================================
cat("\n")
cat("=============================================================\n")
cat("  MLPE MULTIVARIADO — Cenário 1 e Cenário 2                 \n")
cat("=============================================================\n")
cat("  Target : Num_Customers | H = 7 dias | Growing window\n")
cat("  Lags NC:", paste(LAGS_NC, collapse = ","),
    "| Lags Sales:", paste(LAGS_SALES, collapse = ","), "\n")
cat("  Exógenas: TouristDummy, Num_Employees, Pct_On_Sale\n")
cat("=============================================================\n")

# Abrir PDF para gráficos
pdf("joao_mlpe_multi_plots.pdf", width = 10, height = 6)

# --- Cenário 1: todas as lojas ---
res_c1 <- list()
for (nm in names(stores)) {
  res_c1[[nm]] <- run_cenario1(nm, stores[[nm]])
}

# --- Cenário 2: todas as lojas ---
res_c2 <- list()
for (nm in names(stores)) {
  res_c2[[nm]] <- run_cenario2(nm, stores[[nm]])
}

dev.off()
cat("\nGráficos guardados em: joao_mlpe_multi_plots.pdf\n")

# =============================================================================
# GUARDAR RESULTADOS EM CSV
# =============================================================================
df_all <- rbind(do.call(rbind, res_c1), do.call(rbind, res_c2))
write.csv(df_all, "resultados_joao_multi.csv", row.names = FALSE)
cat("Resultados guardados em: resultados_joao_multi.csv\n")

# =============================================================================
# TABELA COMPARATIVA FINAL (Medianas por loja e cenário)
# =============================================================================
cat("\n")
cat("=============================================================================\n")
cat("  TABELA COMPARATIVA — Medianas por loja (mlpe)\n")
cat("=============================================================================\n")
cat(sprintf("%-15s %-18s %8s %8s %8s\n",
            "Loja", "Cenário", "NMAE(%)", "RRSE", "R²"))
cat(strrep("-", 60), "\n")

for (nm in names(stores)) {
  d1 <- res_c1[[nm]]
  cat(sprintf("%-15s %-18s %8.2f %8.3f %8.3f\n",
              nm, "C1 exo",
              median(d1$NMAE_mlpe), median(d1$RRSE_mlpe), median(d1$R2_mlpe)))
  
  d2 <- res_c2[[nm]]
  cat(sprintf("%-15s %-18s %8.2f %8.3f %8.3f\n",
              "", "C2 exo+Sales",
              median(d2$NMAE_mlpe), median(d2$RRSE_mlpe), median(d2$R2_mlpe)))
  
  cat(sprintf("%-15s %-18s %8.2f %8.3f %8.3f\n",
              "", "Seasonal Naive",
              median(d1$NMAE_naive), median(d1$RRSE_naive), median(d1$R2_naive)))
  cat(strrep("-", 60), "\n")
}

cat("\n")
cat("=============================================================================\n")
cat("  Para comparação completa com o univariado, juntar com joao_mlpe.R\n")
cat("  Critério lexicográfico: 1) NMAE  2) RRSE  3) R²  (tolerância 5%%)\n")
cat("=============================================================================\n")