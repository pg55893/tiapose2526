# Autora: Carolina
# Previsao: ARIMAX Multivariado Cenario 2
# Otimizacao: PSO - O2 (maximizar lucro com restricao <= 10.000 unidades)
# Metodos: Death Penalty vs Repair
# Multi-run: 5 runs, mediana do profit
# Adaptado de: P. Cortez, Modern Optimization with R, 2021, Springer.

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
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.numeric(x))
  if (is.factor(x)) return(as.numeric(as.character(x)))
  return(as.numeric(x))
}

build_df <- function(dados) {
  d <- dados[, c("Date", "Num_Customers", "Sales", "TouristEvent",
                 "Num_Employees", "Pct_On_Sale")]
  
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  rownames(d) <- NULL
  
  if (is.character(d$TouristEvent) || is.factor(d$TouristEvent)) {
    d$TouristEvent <- as.numeric(d$TouristEvent == "Yes")
  } else {
    d$TouristEvent <- to_numeric_safe(d$TouristEvent)
  }
  
  d$Num_Customers <- to_numeric_safe(d$Num_Customers)
  d$Sales <- to_numeric_safe(d$Sales)
  d$Num_Employees <- to_numeric_safe(d$Num_Employees)
  d$Pct_On_Sale <- to_numeric_safe(d$Pct_On_Sale)
  
  return(d)
}

arimax_prev <- function(dados, H = 7, lags_sales = c(1, 7)) {
  d <- build_df(dados)
  N <- nrow(d)
  tr <- 1:N
  te <- (N - H + 1):N
  
  fit_s <- auto.arima(
    ts(d$Sales[tr], frequency = 7),
    xreg = as.matrix(d[tr, c("TouristEvent", "Num_Employees", "Pct_On_Sale")])
  )
  
  pred_s <- pmax(
    as.numeric(
      forecast(
        fit_s,
        h = H,
        xreg = as.matrix(d[te, c("TouristEvent", "Num_Employees", "Pct_On_Sale")])
      )$mean
    ),
    0
  )
  
  vi <- tr[tr > max(lags_sales)]
  
  xtr <- data.frame(
    TouristEvent = d$TouristEvent[vi],
    Num_Employees = d$Num_Employees[vi],
    Pct_On_Sale = d$Pct_On_Sale[vi]
  )
  
  for (lag in lags_sales) {
    xtr[[paste0("Sales_lag", lag)]] <- d$Sales[vi - lag]
  }
  
  fit_c <- auto.arima(
    ts(d$Num_Customers[vi], frequency = 7),
    xreg = as.matrix(xtr)
  )
  
  sales_ext <- c(d$Sales, pred_s)
  cnames <- c(
    "TouristEvent",
    "Num_Employees",
    "Pct_On_Sale",
    paste0("Sales_lag", lags_sales)
  )
  
  xte <- matrix(
    NA,
    nrow = H,
    ncol = length(cnames),
    dimnames = list(NULL, cnames)
  )
  
  for (step in 1:H) {
    i <- te[step]
    
    xte[step, "TouristEvent"] <- d$TouristEvent[i]
    xte[step, "Num_Employees"] <- d$Num_Employees[i]
    xte[step, "Pct_On_Sale"] <- d$Pct_On_Sale[i]
    
    for (lag in lags_sales) {
      xte[step, paste0("Sales_lag", lag)] <- sales_ext[(N + step) - lag]
    }
  }
  
  pred_c <- pmax(
    as.numeric(forecast(fit_c, h = H, xreg = xte)$mean),
    0
  )
  
  return(pred_c)
}

# -----------------------------------------------------------------------------
# extrair PREV do ARIMAX para as 4 lojas
# -----------------------------------------------------------------------------
H <- 7
LAGS_SALES <- c(1, 7)

stores <- list(
  Baltimore = baltimore,
  Lancaster = lancaster,
  Philadelphia = philadelphia,
  Richmond = richmond
)

cat("=== a extrair PREV do ARIMAX ===\n")

PREV_carolina <- c()

for (nome in names(stores)) {
  cat(" -", nome, "...\n")
  PREV_carolina <- c(
    PREV_carolina,
    arimax_prev(stores[[nome]], H = H, lags_sales = LAGS_SALES)
  )
}

PREV <- round(PREV_carolina)
upper <- calc_upper(PREV)

cat("\n=== PREV ARIMAX ===\n")
cat("Baltimore   :", PREV[1:7], "\n")
cat("Lancaster   :", PREV[8:14], "\n")
cat("Philadelphia:", PREV[15:21], "\n")
cat("Richmond    :", PREV[22:28], "\n\n")

# -----------------------------------------------------------------------------
# configuracao do PSO
# -----------------------------------------------------------------------------
D <- length(lower)
popSize <- max(20, 10 + round(2 * sqrt(D)))
maxit <- 500
report <- 50
RUNS <- 5
LIMIT_UNITS <- 10000

idx_PR <- seq(1, D, by = 3)
idx_J  <- seq(2, D, by = 3)
idx_X  <- seq(3, D, by = 3)

# -----------------------------------------------------------------------------
# cores pedidas pelo professor
# -----------------------------------------------------------------------------
cor_dp     <- "#ee8a37"
sombra_dp  <- "#fed0a5"

cor_rep    <- "#049e86"
sombra_rep <- "#9ceadd"

# -----------------------------------------------------------------------------
# funcoes auxiliares
# -----------------------------------------------------------------------------
g_best <- function(val1, val2, type = "max") {
  if (type == "max") return(max(c(val1, val2)))
  else return(min(c(val1, val2)))
}

normalize <- function(S) {
  S[idx_J] <- round(S[idx_J])
  S[idx_X] <- round(S[idx_X])
  return(pmin(pmax(S, lower), upper))
}

# -----------------------------------------------------------------------------
# repair
# -----------------------------------------------------------------------------
repair <- function(S) {
  S <- normalize(S)
  u <- total_units(S)
  
  if (is.na(u) || u <= LIMIT_UNITS) return(S)
  
  S_orig <- S
  lo <- 0
  hi <- 1
  
  for (it in 1:25) {
    mid <- (lo + hi) / 2
    S_try <- S_orig
    
    S_try[idx_J] <- round(S_orig[idx_J] * mid)
    S_try[idx_X] <- round(S_orig[idx_X] * mid)
    S_try <- normalize(S_try)
    
    u_try <- total_units(S_try)
    
    if (is.na(u_try) || u_try <= LIMIT_UNITS) {
      lo <- mid
    } else {
      hi <- mid
    }
  }
  
  S_out <- S_orig
  S_out[idx_J] <- round(S_orig[idx_J] * lo)
  S_out[idx_X] <- round(S_orig[idx_X] * lo)
  S_out <- normalize(S_out)
  
  u_out <- total_units(S_out)
  
  if (!is.na(u_out) && u_out > LIMIT_UNITS) {
    S_out[idx_J] <- 0
    S_out[idx_X] <- 0
    S_out <- normalize(S_out)
  }
  
  return(S_out)
}

# -----------------------------------------------------------------------------
# funcoes de avaliacao
# -----------------------------------------------------------------------------
eval_death <- function(S) {
  S <- normalize(S)
  EV <<- EV + 1
  
  u <- total_units(S)
  
  if (!is.na(u) && u > LIMIT_UNITS) {
    excess <- (u - LIMIT_UNITS) / LIMIT_UNITS
    res <- profit(S) - 1e5 * excess
    
    if (EV <= MAXIT) {
      FHIST[EV] <<- ifelse(is.finite(BEST), BEST, res)
    }
    
    return(-res)
  }
  
  res <- profit(S)
  BEST <<- g_best(BEST, res, TYPE)
  
  if (EV <= MAXIT) FHIST[EV] <<- BEST
  
  return(-res)
}

eval_repair <- function(S) {
  S <- repair(S)
  EV <<- EV + 1
  
  res <- profit(S)
  BEST <<- g_best(BEST, res, TYPE)
  
  if (EV <= MAXIT) FHIST[EV] <<- BEST
  
  return(-res)
}

# -----------------------------------------------------------------------------
# correr runs
# -----------------------------------------------------------------------------
correr_runs <- function(fn_eval, usar_repair = FALSE, nome = "") {
  cat("\n=== PSO O2 -", nome, "(", RUNS, "runs) ===\n")
  cat("D=", D, "| popSize=", popSize, "| maxit=", maxit, "| runs=", RUNS, "\n")
  
  MAXIT_local <- maxit * popSize
  lucros <- numeric(RUNS)
  F_mat <- matrix(NA, nrow = RUNS, ncol = MAXIT_local)
  
  S_best_global <- NULL
  lucro_best_global <- -Inf
  
  for (run in 1:RUNS) {
    cat("run", run, "/", RUNS, "...\n")
    
    TYPE <<- "max"
    EV <<- 0
    BEST <<- -Inf
    MAXIT <<- MAXIT_local
    FHIST <<- rep(NA, MAXIT_local)
    
    set.seed(run)
    
    s0 <- lower + (upper - lower) * runif(D)
    
    ps <- psoptim(
      par = s0,
      fn = fn_eval,
      lower = lower,
      upper = upper,
      control = list(
        trace = 0,
        REPORT = report,
        maxit = maxit,
        s = popSize,
        w = 0.729,
        c.p = 1.494,
        c.g = 1.494,
        vectorize = FALSE
      )
    )
    
    if (usar_repair) {
      S_run <- repair(ps$par)
    } else {
      S_run <- normalize(ps$par)
      
      if (total_units(S_run) > LIMIT_UNITS) {
        S_run <- repair(S_run)
      }
    }
    
    lucro_run <- profit(S_run)
    lucros[run] <- lucro_run
    F_mat[run, ] <- FHIST
    
    if (lucro_run > lucro_best_global) {
      lucro_best_global <- lucro_run
      S_best_global <- S_run
    }
    
    cat(
      "  lucro run", run, ":", round(lucro_run), "$",
      "| unidades:", round(total_units(S_run)), "\n"
    )
  }
  
  return(list(
    lucros = lucros,
    F_mat = F_mat,
    S_best = S_best_global,
    lucro_best = lucro_best_global
  ))
}

# -----------------------------------------------------------------------------
# correr Death Penalty e Repair
# -----------------------------------------------------------------------------
t0 <- proc.time()

res_death <- correr_runs(
  eval_death,
  usar_repair = FALSE,
  nome = "DeathPenalty"
)

res_repair <- correr_runs(
  eval_repair,
  usar_repair = TRUE,
  nome = "Repair"
)

tempo_total <- (proc.time() - t0)["elapsed"]

# -----------------------------------------------------------------------------
# metricas
# -----------------------------------------------------------------------------
med_death <- median(res_death$lucros, na.rm = TRUE)
med_repair <- median(res_repair$lucros, na.rm = TRUE)

cat("\n=== RESULTADOS O2 ===\n")
cat(sprintf("%-20s %10s %10s %10s\n", "Metodo", "Mediana", "Max", "Min"))

cat(sprintf(
  "%-20s %10.0f %10.0f %10.0f\n",
  "PSO DeathPenalty",
  med_death,
  max(res_death$lucros, na.rm = TRUE),
  min(res_death$lucros, na.rm = TRUE)
))

cat(sprintf(
  "%-20s %10.0f %10.0f %10.0f\n",
  "PSO Repair",
  med_repair,
  max(res_repair$lucros, na.rm = TRUE),
  min(res_repair$lucros, na.rm = TRUE)
))

cat("Tempo total:", round(tempo_total, 1), "s\n")

# -----------------------------------------------------------------------------
# construir matriz de convergencia sem lucros negativos
# -----------------------------------------------------------------------------
build_mat <- function(F_mat) {
  hists <- list()
  
  for (i in 1:nrow(F_mat)) {
    h <- F_mat[i, ]
    h <- h[is.finite(h)]
    h[h < 0] <- NA
    
    if (sum(is.finite(h)) > 0) {
      h <- h[is.finite(h)]
      hists[[length(hists) + 1]] <- h
    }
  }
  
  if (length(hists) == 0) return(NULL)
  
  max_len <- max(sapply(hists, length))
  
  mat <- sapply(
    hists,
    function(h) c(h, rep(h[length(h)], max_len - length(h)))
  )
  
  return(mat)
}

# -----------------------------------------------------------------------------
# grafico individual com banda min-max
# -----------------------------------------------------------------------------
plot_runs <- function(F_mat, titulo, pdf_out, csv_out,
                      cor_med, cor_sombra) {
  mat <- build_mat(F_mat)
  
  if (is.null(mat)) {
    cat("Sem dados para grafico:", titulo, "\n")
    return(NULL)
  }
  
  med_curve <- apply(mat, 1, median, na.rm = TRUE)
  vmin <- apply(mat, 1, min, na.rm = TRUE)
  vmax <- apply(mat, 1, max, na.rm = TRUE)
  x_fes <- seq_len(nrow(mat))
  
  y_all <- c(med_curve, vmin, vmax)
  y_all <- y_all[is.finite(y_all) & y_all >= 0]
  
  pdf(pdf_out, width = 9, height = 6)
  
  plot(
    NA,
    xlim = range(x_fes),
    ylim = range(y_all, na.rm = TRUE),
    xlab = "Numero de Avaliacoes (FES)",
    ylab = "Melhor Lucro",
    main = titulo
  )
  
  grid()
  
  ok <- is.finite(vmin) & is.finite(vmax) & vmin >= 0 & vmax >= 0
  
  if (any(ok)) {
    polygon(
      c(x_fes[ok], rev(x_fes[ok])),
      c(vmin[ok], rev(vmax[ok])),
      col = cor_sombra,
      border = NA
    )
  }
  
  lines(x_fes, med_curve, col = cor_med, lwd = 3)
  
  abline(
    h = median(med_curve, na.rm = TRUE),
    col = "grey40",
    lty = 2,
    lwd = 1
  )
  
  legend(
    "bottomright",
    legend = c("Mediana", "Banda min-max", "Mediana final"),
    col = c(cor_med, cor_sombra, "grey40"),
    lwd = c(3, NA, 1),
    pch = c(NA, 15, NA),
    lty = c(1, NA, 2),
    bty = "n"
  )
  
  dev.off()
  
  write.csv(
    data.frame(
      Avaliacao = x_fes,
      Lucro_Mediana = round(med_curve, 2),
      Lucro_Min = round(vmin, 2),
      Lucro_Max = round(vmax, 2)
    ),
    file = csv_out,
    row.names = FALSE
  )
  
  cat("Grafico guardado:", pdf_out, "\n")
  invisible(med_curve)
}

# -----------------------------------------------------------------------------
# grafico comparativo Death Penalty vs Repair com bandas
# -----------------------------------------------------------------------------
plot_dp_rep <- function(F_death, F_repair, med_death, med_repair, pdf_out) {
  mat_dp <- build_mat(F_death)
  mat_rep <- build_mat(F_repair)
  
  if (is.null(mat_dp) && is.null(mat_rep)) {
    cat("Sem dados para grafico comparativo.\n")
    return(NULL)
  }
  
  curve_dp <- if (!is.null(mat_dp)) apply(mat_dp, 1, median, na.rm = TRUE) else NULL
  curve_rep <- if (!is.null(mat_rep)) apply(mat_rep, 1, median, na.rm = TRUE) else NULL
  
  vmin_dp <- if (!is.null(mat_dp)) apply(mat_dp, 1, min, na.rm = TRUE) else NULL
  vmax_dp <- if (!is.null(mat_dp)) apply(mat_dp, 1, max, na.rm = TRUE) else NULL
  
  vmin_rep <- if (!is.null(mat_rep)) apply(mat_rep, 1, min, na.rm = TRUE) else NULL
  vmax_rep <- if (!is.null(mat_rep)) apply(mat_rep, 1, max, na.rm = TRUE) else NULL
  
  y_all <- c(curve_dp, curve_rep, vmin_dp, vmax_dp, vmin_rep, vmax_rep)
  y_all <- y_all[is.finite(y_all) & y_all >= 0]
  
  x_max <- max(
    if (!is.null(mat_dp)) nrow(mat_dp) else 1,
    if (!is.null(mat_rep)) nrow(mat_rep) else 1
  )
  
  pdf(pdf_out, width = 10, height = 6)
  
  plot(
    NA,
    xlim = c(1, x_max),
    ylim = range(y_all, na.rm = TRUE),
    xlab = "Numero de Avaliacoes (FES)",
    ylab = "Melhor Lucro",
    main = paste0("PSO O2: Death Penalty vs Repair - ", RUNS, " runs")
  )
  
  grid()
  
  if (!is.null(mat_dp)) {
    x_dp <- seq_len(nrow(mat_dp))
    ok_dp <- is.finite(vmin_dp) & is.finite(vmax_dp) & vmin_dp >= 0 & vmax_dp >= 0
    
    if (any(ok_dp)) {
      polygon(
        c(x_dp[ok_dp], rev(x_dp[ok_dp])),
        c(vmin_dp[ok_dp], rev(vmax_dp[ok_dp])),
        col = sombra_dp,
        border = NA
      )
    }
  }
  
  if (!is.null(mat_rep)) {
    x_rep <- seq_len(nrow(mat_rep))
    ok_rep <- is.finite(vmin_rep) & is.finite(vmax_rep) & vmin_rep >= 0 & vmax_rep >= 0
    
    if (any(ok_rep)) {
      polygon(
        c(x_rep[ok_rep], rev(x_rep[ok_rep])),
        c(vmin_rep[ok_rep], rev(vmax_rep[ok_rep])),
        col = sombra_rep,
        border = NA
      )
    }
  }
  
  if (!is.null(curve_dp)) {
    lines(seq_along(curve_dp), curve_dp, col = cor_dp, lwd = 3)
  }
  
  if (!is.null(curve_rep)) {
    lines(seq_along(curve_rep), curve_rep, col = cor_rep, lwd = 3)
  }
  
  legend(
    "bottomright",
    legend = c(
      sprintf("Death Penalty mediana (med=%.0f)", med_death),
      sprintf("Repair mediana (med=%.0f)", med_repair),
      "Banda Death Penalty",
      "Banda Repair"
    ),
    col = c(cor_dp, cor_rep, sombra_dp, sombra_rep),
    lwd = c(3, 3, NA, NA),
    pch = c(NA, NA, 15, 15),
    lty = c(1, 1, NA, NA),
    bty = "n"
  )
  
  dev.off()
  cat("Grafico guardado:", pdf_out, "\n")
}

# -----------------------------------------------------------------------------
# gerar graficos de convergencia
# -----------------------------------------------------------------------------
plot_runs(
  res_death$F_mat,
  titulo = paste0("PSO O2 - Death Penalty - ", RUNS, " runs"),
  pdf_out = file.path(output_dir, "convergencia_PSO_O2_DeathPenalty.pdf"),
  csv_out = file.path(output_dir, "convergencia_PSO_O2_DeathPenalty.csv"),
  cor_med = cor_dp,
  cor_sombra = sombra_dp
)

plot_runs(
  res_repair$F_mat,
  titulo = paste0("PSO O2 - Repair - ", RUNS, " runs"),
  pdf_out = file.path(output_dir, "convergencia_PSO_O2_Repair.pdf"),
  csv_out = file.path(output_dir, "convergencia_PSO_O2_Repair.csv"),
  cor_med = cor_rep,
  cor_sombra = sombra_rep
)

plot_dp_rep(
  res_death$F_mat,
  res_repair$F_mat,
  med_death,
  med_repair,
  pdf_out = file.path(output_dir, "comparacao_PSO_O2.pdf")
)

# -----------------------------------------------------------------------------
# boxplot sem lucros negativos
# -----------------------------------------------------------------------------
lucros_death_pos <- res_death$lucros[res_death$lucros >= 0]
lucros_repair_pos <- res_repair$lucros[res_repair$lucros >= 0]

pdf(file.path(output_dir, "boxplot_PSO_O2.pdf"), width = 8, height = 6)

boxplot(
  list(
    DeathPenalty = lucros_death_pos,
    Repair = lucros_repair_pos
  ),
  col = c(cor_dp, cor_rep),
  main = paste("PSO O2 - Distribuicao lucros positivos (", RUNS, "runs)"),
  ylab = "Lucro ($)"
)

abline(h = median(lucros_death_pos, na.rm = TRUE), col = cor_dp, lty = 2, lwd = 1.5)
abline(h = median(lucros_repair_pos, na.rm = TRUE), col = cor_rep, lty = 2, lwd = 1.5)

dev.off()

cat("PDF guardado: boxplot_PSO_O2.pdf\n")

# -----------------------------------------------------------------------------
# tabela resumo - CSV
# -----------------------------------------------------------------------------
tabela <- data.frame(
  Objetivo = c("O2", "O2"),
  Metodo = c("PSO_DeathPenalty", "PSO_Repair"),
  Previsao = c("ARIMAX", "ARIMAX"),
  Runs = c(RUNS, RUNS),
  Lucro_Mediana = as.numeric(round(c(med_death, med_repair), 2)),
  Lucro_Max = as.numeric(round(c(max(res_death$lucros, na.rm = TRUE),
                                 max(res_repair$lucros, na.rm = TRUE)), 2)),
  Lucro_Min = as.numeric(round(c(min(res_death$lucros, na.rm = TRUE),
                                 min(res_repair$lucros, na.rm = TRUE)), 2)),
  Unidades_Best = as.numeric(round(c(
    total_units(res_death$S_best),
    total_units(res_repair$S_best)
  ), 2)),
  Tempo_Total_s = rep(as.numeric(round(tempo_total, 2)), 2)
)

write.csv(
  tabela,
  file = file.path(output_dir, "tabela_resumo_PSO_O2.csv"),
  row.names = FALSE
)

cat("CSV guardado: tabela_resumo_PSO_O2.csv\n")

# -----------------------------------------------------------------------------
# lucros por run - CSV
# -----------------------------------------------------------------------------
write.csv(
  data.frame(
    Run = 1:RUNS,
    DeathPenalty = round(res_death$lucros, 2),
    Repair = round(res_repair$lucros, 2)
  ),
  file = file.path(output_dir, "lucros_runs_PSO_O2.csv"),
  row.names = FALSE
)

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