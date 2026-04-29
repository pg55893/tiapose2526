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
setwd("~/TIAPOSE_projeto/tiapose2526/data")
source("~/TIAPOSE_projeto/tiapose2526/utils/tratamentoDeDados.R")
source("~/TIAPOSE_projeto/tiapose2526/utils/config_otimizacao.R")

output_dir <- "~/TIAPOSE_projeto/tiapose2526/otimizacao/PSO/O2"
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


eval_death <- function(S) {
  S  <- normalize(S)
  EV <<- EV + 1
  u  <- total_units(S)
  
  if (!is.na(u) && u > LIMIT_UNITS) {
    excess <- (u - LIMIT_UNITS) / LIMIT_UNITS
    res    <- profit(S) - 1e5 * excess
    
    if (EV <= MAXIT) {
      FHIST[EV] <<- ifelse(is.finite(BEST), BEST, res)
    }
    
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

    if (usar_repair) {
      S_run <- repair(ps$par)
    } else {
      S_run <- normalize(ps$par)
      
      # garantir que Death Penalty só aceita solução válida
      if (total_units(S_run) > LIMIT_UNITS) {
        S_run <- repair(S_run)
      }
    }
    
    lucro_run   <- profit(S_run)
    lucros[run] <- lucro_run
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

build_mat <- function(F_mat) {
  hists <- list()
  
  for (i in 1:nrow(F_mat)) {
    h <- F_mat[i, ]
    h <- h[is.finite(h)]
    
    if (length(h) > 0) {
      hists[[length(hists) + 1]] <- h
    }
  }
  
  if (length(hists) == 0) return(NULL)
  
  max_len <- max(sapply(hists, length))
  mat <- sapply(hists, function(h) c(h, rep(h[length(h)], max_len - length(h))))
  
  return(mat)
}

plot_runs <- function(F_mat, titulo, pdf_out, csv_out, cor_med = "steelblue") {
  mat <- build_mat(F_mat)
  
  if (is.null(mat)) {
    cat("Sem dados para grafico:", titulo, "\n")
    return(NULL)
  }
  
  med_curve <- apply(mat, 1, median, na.rm = TRUE)
  x_fes <- seq_len(nrow(mat))
  y_rng <- range(mat[is.finite(mat)], na.rm = TRUE)
  
  pdf(pdf_out, width = 9, height = 6)
  
  for (j in seq_len(ncol(mat))) {
    if (j == 1) {
      plot(x_fes, mat[, j],
           type = "l",
           col = "grey80",
           lwd = 1,
           ylim = y_rng,
           xlab = "Numero de Avaliacoes (FES)",
           ylab = "Melhor Lucro",
           main = titulo)
    } else {
      lines(x_fes, mat[, j], col = "grey80", lwd = 1)
    }
  }
  
  lines(x_fes, med_curve, col = cor_med, lwd = 2.5)
  
  legend("bottomright",
         legend = c("Runs individuais", "Mediana"),
         col = c("grey70", cor_med),
         lwd = c(1, 2.5),
         bty = "n")
  
  dev.off()
  
  write.csv(data.frame(Avaliacao = x_fes,
                       Lucro_Mediana = round(med_curve, 2)),
            file = csv_out,
            row.names = FALSE)
  
  cat("Grafico guardado:", pdf_out, "\n")
  
  invisible(med_curve)
}

plot_dp_rep <- function(F_death, F_repair, med_death, med_repair, pdf_out) {
  mat_dp <- build_mat(F_death)
  mat_rep <- build_mat(F_repair)
  
  y_all <- c(
    if (!is.null(mat_dp)) mat_dp[is.finite(mat_dp)],
    if (!is.null(mat_rep)) mat_rep[is.finite(mat_rep)]
  )
  
  if (length(y_all) == 0) {
    cat("Sem dados para grafico comparativo.\n")
    return(NULL)
  }
  
  y_finite <- y_all[is.finite(y_all)]
  q_low    <- quantile(y_finite, 0.05, na.rm = TRUE)  # corta 5% mais baixo
  y_rng    <- c(max(q_low, -5000), max(y_finite, na.rm = TRUE) * 1.05)
  
  pdf(pdf_out, width = 10, height = 6)
  
  if (!is.null(mat_dp)) {
    curve_dp <- apply(mat_dp, 1, median, na.rm = TRUE)
    
    for (j in seq_len(ncol(mat_dp))) {
      if (j == 1) {
        plot(seq_len(nrow(mat_dp)), mat_dp[, j],
             type = "l",
             col = "#FFAAAA",
             lwd = 1,
             ylim = y_rng,
             xlab = "Numero de Avaliacoes (FES)",
             ylab = "Melhor Lucro",
             main = "PSO O2: Death Penalty vs Repair (runs + mediana)")
      } else {
        lines(seq_len(nrow(mat_dp)), mat_dp[, j], col = "#FFAAAA", lwd = 1)
      }
    }
    
    lines(seq_along(curve_dp), curve_dp, col = "firebrick", lwd = 2.5)
  } else {
    plot.new()
  }
  
  if (!is.null(mat_rep)) {
    curve_rep <- apply(mat_rep, 1, median, na.rm = TRUE)
    
    if (is.null(mat_dp)) {
      plot(seq_len(nrow(mat_rep)), mat_rep[, 1],
           type = "l",
           col = "#AADDAA",
           lwd = 1,
           ylim = y_rng,
           xlab = "Numero de Avaliacoes (FES)",
           ylab = "Melhor Lucro",
           main = "PSO O2: Death Penalty vs Repair (runs + mediana)")
    }
    
    for (j in seq_len(ncol(mat_rep))) {
      lines(seq_len(nrow(mat_rep)), mat_rep[, j], col = "#AADDAA", lwd = 1)
    }
    
    lines(seq_along(curve_rep), curve_rep, col = "forestgreen", lwd = 2.5)
  }
  
  legend("bottomright",
         legend = c(sprintf("DP runs (med=%.0f)", med_death),
                    sprintf("Repair runs (med=%.0f)", med_repair),
                    "Mediana DP",
                    "Mediana Repair"),
         col = c("#FFAAAA", "#AADDAA", "firebrick", "forestgreen"),
         lwd = c(1, 1, 2.5, 2.5),
         bty = "n")
  
  dev.off()
  cat("Grafico guardado:", pdf_out, "\n")
}

plot_runs(
  res_death$F_mat,
  titulo = paste0("PSO O2 - Death Penalty - ", RUNS, " runs"),
  pdf_out = file.path(output_dir, "convergencia_PSO_O2_DeathPenalty.pdf"),
  csv_out = file.path(output_dir, "convergencia_PSO_O2_DeathPenalty.csv"),
  cor_med = "steelblue"
)

plot_runs(
  res_repair$F_mat,
  titulo = paste0("PSO O2 - Repair - ", RUNS, " runs"),
  pdf_out = file.path(output_dir, "convergencia_PSO_O2_Repair.pdf"),
  csv_out = file.path(output_dir, "convergencia_PSO_O2_Repair.csv"),
  cor_med = "darkgreen"
)

plot_dp_rep(
  res_death$F_mat,
  res_repair$F_mat,
  med_death,
  med_repair,
  pdf_out = file.path(output_dir, "comparacao_PSO_O2.pdf")
)

pdf(file.path(output_dir, "boxplot_PSO_O2.pdf"), width = 8, height = 6)
boxplot(list(DeathPenalty = res_death$lucros, Repair = res_repair$lucros),
        col  = c("steelblue", "darkgreen"),
        main = paste("PSO O2 - Distribuicao lucros (", RUNS, "runs)"),
        ylab = "Lucro ($)")
abline(h = med_death,  col = "steelblue", lty = 2, lwd = 1.5)
abline(h = med_repair, col = "darkgreen", lty = 2, lwd = 1.5)
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
  Lucro_Mediana = as.numeric(round(c(med_death, med_repair), 2)),
  Lucro_Max     = as.numeric(round(c(max(res_death$lucros), max(res_repair$lucros)), 2)),
  Lucro_Min     = as.numeric(round(c(min(res_death$lucros), min(res_repair$lucros)), 2)),
  Unidades_Best = as.numeric(round(c(total_units(res_death$S_best), total_units(res_repair$S_best)), 2)),
  Tempo_Total_s = rep(as.numeric(round(tempo_total, 2)), 2)
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
