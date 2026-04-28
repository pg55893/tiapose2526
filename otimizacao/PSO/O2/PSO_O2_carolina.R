# Autora: Carolina
# Previsao: ARIMAX Multivariado Cenario 2
# Otimizacao: PSO - O2 (maximizar lucro com restricao <= 10.000 unidades)
# Metodos: Death Penalty vs Repair
# Multi-run: 20 runs, mediana do profit
# Adaptado de: P. Cortez, Modern Optimization with R, 2021, Springer.

# you need to install these packages:
# install.packages("pso")
# install.packages("forecast")
# install.packages("rminer")
library(pso)
library(forecast)
library(rminer)

# -----------------------------------------------------------------------------
# carregar dados e config
# -----------------------------------------------------------------------------
setwd("~/TIAPOSE2526/data")
source("~/TIAPOSE2526/utils/tratamentoDeDados.R")
source("~/TIAPOSE2526/utils/config_otimizacao.R")

output_dir <- "~/TIAPOSE2526/otimizacao/PSO/O2"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# funcoes auxiliares do ARIMAX
# -----------------------------------------------------------------------------
to_numeric_safe <- function(x) {
  if (is.numeric(x))  return(x)
  if (is.logical(x))  return(as.numeric(x))
  if (is.factor(x))   return(as.numeric(as.character(x)))
  return(as.numeric(x))
}

build_df <- function(dados) {
  d <- dados[, c("Date", "Num_Customers", "Sales", "TouristEvent",
                 "Num_Employees", "Pct_On_Sale")]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  rownames(d) <- NULL
  if (is.character(d$TouristEvent) || is.factor(d$TouristEvent))
    d$TouristEvent <- as.numeric(d$TouristEvent == "Yes")
  else
    d$TouristEvent <- to_numeric_safe(d$TouristEvent)
  d$Num_Customers <- to_numeric_safe(d$Num_Customers)
  d$Sales         <- to_numeric_safe(d$Sales)
  d$Num_Employees <- to_numeric_safe(d$Num_Employees)
  d$Pct_On_Sale   <- to_numeric_safe(d$Pct_On_Sale)
  return(d)
}

arimax_prev <- function(dados, H = 7, lags_sales = c(1, 7)) {
  d  <- build_df(dados)
  N  <- nrow(d)
  tr <- 1:N
  te <- (N - H + 1):N

  # modelo Sales
  fit_s  <- auto.arima(ts(d$Sales[tr], frequency = 7),
                       xreg = as.matrix(d[tr, c("TouristEvent", "Num_Employees", "Pct_On_Sale")]))
  pred_s <- pmax(as.numeric(forecast(fit_s, h = H,
                                     xreg = as.matrix(d[te, c("TouristEvent", "Num_Employees", "Pct_On_Sale")]))$mean), 0)

  # modelo Customers com lags de Sales
  vi  <- tr[tr > max(lags_sales)]
  xtr <- data.frame(TouristEvent  = d$TouristEvent[vi],
                    Num_Employees = d$Num_Employees[vi],
                    Pct_On_Sale   = d$Pct_On_Sale[vi])
  for (lag in lags_sales) xtr[[paste0("Sales_lag", lag)]] <- d$Sales[vi - lag]

  fit_c <- auto.arima(ts(d$Num_Customers[vi], frequency = 7), xreg = as.matrix(xtr))

  sales_ext <- c(d$Sales, pred_s)
  cnames    <- c("TouristEvent", "Num_Employees", "Pct_On_Sale", paste0("Sales_lag", lags_sales))
  xte       <- matrix(NA, nrow = H, ncol = length(cnames), dimnames = list(NULL, cnames))
  for (step in 1:H) {
    i <- te[step]
    xte[step, "TouristEvent"]  <- d$TouristEvent[i]
    xte[step, "Num_Employees"] <- d$Num_Employees[i]
    xte[step, "Pct_On_Sale"]   <- d$Pct_On_Sale[i]
    for (lag in lags_sales) xte[step, paste0("Sales_lag", lag)] <- sales_ext[(N + step) - lag]
  }
  pred_c <- pmax(as.numeric(forecast(fit_c, h = H, xreg = xte)$mean), 0)
  return(pred_c)
}

# -----------------------------------------------------------------------------
# extrair PREV do ARIMAX para as 4 lojas
# -----------------------------------------------------------------------------
H          <- 7
LAGS_SALES <- c(1, 7)
stores     <- list(Baltimore    = baltimore,
                   Lancaster    = lancaster,
                   Philadelphia = philadelphia,
                   Richmond     = richmond)

cat("=== a extrair PREV do ARIMAX ===\n")
PREV_carolina <- c()
for (nome in names(stores)) {
  cat(" -", nome, "...\n")
  PREV_carolina <- c(PREV_carolina, arimax_prev(stores[[nome]], H = H, lags_sales = LAGS_SALES))
}

PREV  <- round(PREV_carolina)
upper <- calc_upper(PREV)

cat("\n=== PREV ARIMAX ===\n")
cat("Baltimore   :", PREV[1:7],   "\n")
cat("Lancaster   :", PREV[8:14],  "\n")
cat("Philadelphia:", PREV[15:21], "\n")
cat("Richmond    :", PREV[22:28], "\n\n")

# -----------------------------------------------------------------------------
# configuracao do PSO
# adaptado de: opt-3-rastrigin-2.R (P. Cortez)
# -----------------------------------------------------------------------------
D           <- length(lower)
popSize     <- max(20, 10 + round(2 * sqrt(D)))
maxit       <- 500
report      <- 50
RUNS        <- 20
LIMIT_UNITS <- 10000

idx_PR <- seq(1, D, by = 3)
idx_J  <- seq(2, D, by = 3)
idx_X  <- seq(3, D, by = 3)

# funcao auxiliar de convergencia
# adaptado de: opt-4-convergence-2demos.R (P. Cortez)
g_best <- function(val1, val2, type = "max") {
  if (type == "max") return(max(c(val1, val2)))
  else               return(min(c(val1, val2)))
}

normalize <- function(S) {
  S[idx_J] <- round(S[idx_J])
  S[idx_X] <- round(S[idx_X])
  return(pmin(pmax(S, lower), upper))
}

# -----------------------------------------------------------------------------
# repair: binary search sobre o factor de escala de J e X
# Objectivo: encontrar o MAIOR scale em [0,1] tal que total_units <= LIMIT_UNITS
# Logica: lo = maior scale FEASIVEL conhecido; hi = menor scale INFEASIVEL
# Converge em 20 iteracoes. Adaptado de: P. Cortez, Modern Optimization with R.
# -----------------------------------------------------------------------------
repair <- function(S) {
  S <- normalize(S)
  u <- total_units(S)
  if (is.na(u) || u <= LIMIT_UNITS) return(S)

  # binary search: lo sobe (feasivel), hi desce (infeasivel)
  S_orig <- S
  lo     <- 0   # scale=0 e sempre feasivel (J=X=0 -> units=0)
  hi     <- 1   # scale=1 e infeasivel (chamamos repair so quando u > LIMIT)
  for (it in 1:25) {
    mid   <- (lo + hi) / 2
    S_try <- S_orig
    S_try[idx_J] <- round(S_orig[idx_J] * mid)
    S_try[idx_X] <- round(S_orig[idx_X] * mid)
    S_try <- normalize(S_try)
    u_try <- total_units(S_try)
    if (is.na(u_try) || u_try <= LIMIT_UNITS) lo <- mid   # feasivel: tentar maior
    else                                       hi <- mid   # infeasivel: reduzir
  }
  # usar lo (maior scale feasivel encontrado)
  S_out        <- S_orig
  S_out[idx_J] <- round(S_orig[idx_J] * lo)
  S_out[idx_X] <- round(S_orig[idx_X] * lo)
  S_out        <- normalize(S_out)

  # verificacao de seguranca
  u_out <- total_units(S_out)
  if (!is.na(u_out) && u_out > LIMIT_UNITS) {
    S_out[idx_J] <- 0
    S_out[idx_X] <- 0
    S_out <- normalize(S_out)
  }
  return(S_out)
}

# -----------------------------------------------------------------------------
# funcoes de avaliacao monitorizadas
# adaptado de: opt-4-convergence-2demos.R (P. Cortez)
# global variables: EV, BEST, F, TYPE, MAXIT
# -----------------------------------------------------------------------------

# Death Penalty com penalidade proporcional a violacao
# Penalidade cresce com o excesso de unidades — da ao PSO gradiente para
# navegar em direcao a regiao feasivel (variante estatica da Death Penalty)
# Adaptado de: P. Cortez, Modern Optimization with R, 2021, Springer.
eval_death <- function(S) {
  S  <- normalize(S)
  EV <<- EV + 1
  u  <- total_units(S)
  if (!is.na(u) && u > LIMIT_UNITS) {
    # penalidade proporcional ao excesso (nao binaria — permite navegacao)
    excess <- (u - LIMIT_UNITS) / LIMIT_UNITS
    res    <- profit(S) - 1e5 * excess
    # BEST so conta solucoes feasiveis — FHIST guarda ultimo BEST feasivel
    if (EV <= MAXIT) FHIST[EV] <<- ifelse(is.finite(BEST), BEST, NA)
    return(-res)
  }
  res  <- profit(S)
  BEST <<- g_best(BEST, res, TYPE)
  if (EV <= MAXIT) FHIST[EV] <<- BEST
  return(-res)
}

# Repair: repara a solucao antes de avaliar — garante sempre solucao valida
eval_repair <- function(S) {
  S    <- repair(S)    # S reparado localmente
  EV   <<- EV + 1
  res  <- profit(S)
  BEST <<- g_best(BEST, res, TYPE)
  if (EV <= MAXIT) FHIST[EV] <<- BEST
  return(-res)
}

# -----------------------------------------------------------------------------
# funcao que corre N runs do PSO e devolve lucros + matriz de convergencia
# adaptado de: opt-3-rastrigin-2.R + opt-4-convergence-2demos.R (P. Cortez)
# -----------------------------------------------------------------------------
correr_runs <- function(fn_eval, usar_repair = FALSE, nome = "") {
  cat("\n=== PSO O2 -", nome, "(", RUNS, "runs) ===\n")
  cat("D=", D, "| popSize=", popSize, "| maxit=", maxit, "| runs=", RUNS, "\n")

  MAXIT_local      <- maxit * popSize
  lucros           <- numeric(RUNS)
  F_mat            <- matrix(NA, nrow = RUNS, ncol = MAXIT_local)
  S_best_global    <- NULL
  lucro_best_global <- -Inf

  for (run in 1:RUNS) {
    cat("run", run, "/", RUNS, "...\n")

    # variaveis globais para a funcao monitorizada
    TYPE  <<- "max"
    EV    <<- 0
    BEST  <<- -Inf
    MAXIT <<- MAXIT_local
    FHIST <<- rep(NA, MAXIT_local)

    set.seed(run)
    s0 <- lower + (upper - lower) * runif(D)

    ps <- psoptim(par     = s0,
                  fn      = fn_eval,
                  lower   = lower,
                  upper   = upper,
                  control = list(trace = 0, REPORT = report, maxit = maxit,
                                 s = popSize, w = 0.729, c.p = 1.494, c.g = 1.494,
                                 vectorize = FALSE))

    if (usar_repair)
      S_run <- repair(ps$par)
    else
      S_run <- normalize(ps$par)

    lucro_run        <- profit(S_run)
    lucros[run]      <- lucro_run
    F_mat[run, ]     <- FHIST

    if (lucro_run > lucro_best_global) {
      lucro_best_global <- lucro_run
      S_best_global     <- S_run
    }

    cat("  lucro run", run, ":", round(lucro_run), "$",
        "| unidades:", round(total_units(S_run)), "\n")
  }

  return(list(lucros    = lucros,
              F_mat     = F_mat,
              S_best    = S_best_global,
              lucro_best = lucro_best_global))
}

# -----------------------------------------------------------------------------
# correr Death Penalty e Repair
# -----------------------------------------------------------------------------
t0        <- proc.time()
res_death <- correr_runs(eval_death,  usar_repair = FALSE, nome = "DeathPenalty")
res_repair <- correr_runs(eval_repair, usar_repair = TRUE,  nome = "Repair")
tempo_total <- (proc.time() - t0)["elapsed"]

# --- metricas ---
med_death  <- median(res_death$lucros)
med_repair <- median(res_repair$lucros)

cat("\n=== RESULTADOS O2 ===\n")
cat(sprintf("%-20s %10s %10s %10s\n", "Metodo", "Mediana", "Max", "Min"))
cat(sprintf("%-20s %10.0f %10.0f %10.0f\n", "PSO DeathPenalty",
            med_death,  max(res_death$lucros),  min(res_death$lucros)))
cat(sprintf("%-20s %10.0f %10.0f %10.0f\n", "PSO Repair",
            med_repair, max(res_repair$lucros), min(res_repair$lucros)))
cat("Tempo total:", round(tempo_total, 1), "s\n")

# -----------------------------------------------------------------------------
# graficos — curvas de convergencia (mediana entre runs)
# eixo X = numero de avaliacoes da funcao profit()
# adaptado de: opt-4-convergence-2demos.R (P. Cortez)
# -----------------------------------------------------------------------------
plot_convergencia <- function(F_mat, lucro_mediana, nome, cor = "blue") {
  F_med   <- apply(F_mat, 2, function(col) median(col, na.rm = TRUE))
  F_clean <- F_med[!is.na(F_med) & is.finite(F_med)]
  if (length(F_clean) == 0) F_clean <- 0
  av <- 1:length(F_clean)

  pdf(file.path(output_dir, paste0("convergencia_PSO_O2_", nome, ".pdf")),
      width = 10, height = 6)
  plot(av, F_clean, type = "l", lwd = 2, col = cor,
       main = paste("Convergencia PSO O2 -", nome, "(D=", D, ", runs=", RUNS, ")"),
       xlab = "Numero de avaliacoes da funcao profit()",
       ylab = "Mediana do melhor lucro acumulado ($)")
  abline(h = lucro_mediana, col = "red", lty = 2)
  legend("bottomright", bty = "n",
         legend = c("Mediana entre runs",
                    paste0("Mediana final: $", round(lucro_mediana))),
         col = c(cor, "red"), lty = c(1, 2), lwd = 2)
  dev.off()
  cat("PDF guardado: convergencia_PSO_O2_", nome, ".pdf\n")

  # CSV convergencia
  write.csv(data.frame(Avaliacao     = av,
                       Lucro_Mediana = round(F_clean, 2)),
            file      = file.path(output_dir, paste0("convergencia_PSO_O2_", nome, ".csv")),
            row.names = FALSE)
}

plot_convergencia(res_death$F_mat,  med_death,  "DeathPenalty", cor = "blue")
plot_convergencia(res_repair$F_mat, med_repair, "Repair",       cor = "darkgreen")

# grafico comparativo Death Penalty vs Repair
F_med_death  <- apply(res_death$F_mat,  2, function(col) median(col, na.rm = TRUE))
F_med_repair <- apply(res_repair$F_mat, 2, function(col) median(col, na.rm = TRUE))
F_d  <- F_med_death[!is.na(F_med_death)  & is.finite(F_med_death)]
F_r  <- F_med_repair[!is.na(F_med_repair) & is.finite(F_med_repair)]

if (length(F_d) > 0 && length(F_r) > 0) {
  n    <- min(length(F_d), length(F_r))  # alinhar comprimentos
  ymin <- min(c(F_d[1:n], F_r[1:n]), na.rm = TRUE)
  ymax <- max(c(F_d[1:n], F_r[1:n]), na.rm = TRUE)

  pdf(file.path(output_dir, "comparacao_PSO_O2.pdf"), width = 10, height = 6)
  plot(1:n, F_d[1:n], type = "l", lwd = 2, col = "blue",
       ylim = c(ymin, ymax),
       main = paste("PSO O2 - DeathPenalty vs Repair (runs=", RUNS, ")"),
       xlab = "Numero de avaliacoes da funcao profit()",
       ylab = "Mediana do melhor lucro acumulado ($)")
  lines(1:n, F_r[1:n], lwd = 2, col = "darkgreen")
  abline(h = med_death,  col = "blue",      lty = 2)
  abline(h = med_repair, col = "darkgreen", lty = 2)
  legend("bottomright", bty = "n",
         legend = c(paste0("DeathPenalty (med=$", round(med_death), ")"),
                    paste0("Repair       (med=$", round(med_repair), ")")),
         col = c("blue", "darkgreen"), lty = 1, lwd = 2)
  dev.off()
  cat("PDF guardado: comparacao_PSO_O2.pdf\n")
} else {
  cat("AVISO: dados insuficientes para grafico comparativo\n")
}

# boxplot comparativo
pdf(file.path(output_dir, "boxplot_PSO_O2.pdf"), width = 8, height = 6)
boxplot(list(DeathPenalty = res_death$lucros, Repair = res_repair$lucros),
        col  = c("steelblue", "darkgreen"),
        main = paste("PSO O2 - Distribuicao lucros (", RUNS, "runs)"),
        ylab = "Lucro ($)")
abline(h = med_death,  col = "steelblue",  lty = 2, lwd = 1.5)
abline(h = med_repair, col = "darkgreen",  lty = 2, lwd = 1.5)
dev.off()
cat("PDF guardado: boxplot_PSO_O2.pdf\n")

# -----------------------------------------------------------------------------
# tabela resumo - CSV
# -----------------------------------------------------------------------------
tabela <- data.frame(
  Objetivo      = c("O2", "O2"),
  Metodo        = c("PSO_DeathPenalty", "PSO_Repair"),
  Previsao      = c("ARIMAX", "ARIMAX"),
  Runs          = c(RUNS, RUNS),
  Lucro_Mediana = round(c(med_death, med_repair), 2),
  Lucro_Max     = round(c(max(res_death$lucros),  max(res_repair$lucros)),  2),
  Lucro_Min     = round(c(min(res_death$lucros),  min(res_repair$lucros)),  2),
  Unidades_Best = round(c(total_units(res_death$S_best), total_units(res_repair$S_best)), 2),
  Tempo_Total_s = round(tempo_total, 2)
)
write.csv(tabela,
          file      = file.path(output_dir, "tabela_resumo_PSO_O2.csv"),
          row.names = FALSE)
cat("CSV guardado: tabela_resumo_PSO_O2.csv\n")

# lucros por run - CSV
write.csv(data.frame(Run          = 1:RUNS,
                     DeathPenalty = round(res_death$lucros,  2),
                     Repair       = round(res_repair$lucros, 2)),
          file      = file.path(output_dir, "lucros_runs_PSO_O2.csv"),
          row.names = FALSE)
cat("CSV guardado: lucros_runs_PSO_O2.csv\n")

cat("\n=== CONCLUIDO ===\n")
cat("Ficheiros em:", output_dir, "\n")
cat(" - convergencia_PSO_O2_DeathPenalty.pdf\n")
cat(" - convergencia_PSO_O2_Repair.pdf\n")
cat(" - comparacao_PSO_O2.pdf\n")
cat(" - boxplot_PSO_O2.pdf\n")
cat(" - tabela_resumo_PSO_O2.csv\n")
cat(" - lucros_runs_PSO_O2.csv\n")
cat(" - convergencia_PSO_O2_DeathPenalty.csv\n")
cat(" - convergencia_PSO_O2_Repair.csv\n")
