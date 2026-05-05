# Autora: Carolina
# Previsao: ARIMAX Multivariado Cenario 2
# Otimizacao: Differential Evolution - O1 e O2
# Multi-run: 5 runs, mediana do profit
# Adaptado de: P. Cortez, Modern Optimization with R, 2021, Springer.

library(DEoptim)
library(forecast)
library(rminer)

# -----------------------------------------------------------------------------
# carregar dados e config
# -----------------------------------------------------------------------------
setwd("~/TIAPOSE_projeto/tiapose2526/data")
source("~/TIAPOSE_projeto/tiapose2526/utils/tratamentoDeDados.R")
source("~/TIAPOSE_projeto/tiapose2526/utils/config_otimizacao.R")

output_dir_O1 <- "~/TIAPOSE_projeto/tiapose2526/otimizacao/DE/O1"
output_dir_O2 <- "~/TIAPOSE_projeto/tiapose2526/otimizacao/DE/O2"

dir.create(output_dir_O1, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_O2, showWarnings = FALSE, recursive = TRUE)

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
# extrair PREV do ARIMAX
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
# configuracao do DE
# -----------------------------------------------------------------------------
D <- length(lower)
popSize <- max(20, 10 + round(2 * sqrt(D)))
maxit <- 500
RUNS <- 5
LIMIT_UNITS <- 10000

idx_J <- seq(2, D, by = 3)
idx_X <- seq(3, D, by = 3)
idx_PR <- seq(1, D, by = 3)

# -----------------------------------------------------------------------------
# cores
# -----------------------------------------------------------------------------
cor_O1 <- "steelblue"
sombra_O1 <- "grey80"

cor_dp <- "#ee8a37"
sombra_dp <- "#fed0a5"

cor_rep <- "#049e86"
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
# graficos com banda min-max
# -----------------------------------------------------------------------------
build_mat <- function(hists, filtrar_negativos = FALSE) {
  valid <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  
  if (length(valid) == 0) return(NULL)
  
  if (filtrar_negativos) {
    valid <- lapply(valid, function(h) {
      h <- h[is.finite(h)]
      h[h < 0] <- NA
      h <- h[is.finite(h)]
      if (length(h) == 0) return(NULL)
      return(h)
    })
    
    valid <- Filter(function(h) !is.null(h) && length(h) > 0, valid)
  }
  
  if (length(valid) == 0) return(NULL)
  
  max_len <- max(sapply(valid, length))
  
  mat <- sapply(
    valid,
    function(h) c(h, rep(h[length(h)], max_len - length(h)))
  )
  
  return(mat)
}

plot_runs <- function(hists, titulo, pdf_out, csv_out,
                      cor_med = "steelblue",
                      cor_sombra = "grey80",
                      med_final = NULL,
                      filtrar_negativos = FALSE) {
  
  mat <- build_mat(hists, filtrar_negativos = filtrar_negativos)
  
  if (is.null(mat)) {
    cat("Sem dados para grafico:", titulo, "\n")
    return(NULL)
  }
  
  med_curve <- apply(mat, 1, median, na.rm = TRUE)
  vmin <- apply(mat, 1, min, na.rm = TRUE)
  vmax <- apply(mat, 1, max, na.rm = TRUE)
  
  x_fes <- seq_len(nrow(mat))
  
  y_all <- c(med_curve, vmin, vmax, med_final)
  y_all <- y_all[is.finite(y_all)]
  
  if (filtrar_negativos) {
    y_all <- y_all[y_all >= 0]
  }
  
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
  
  ok <- is.finite(vmin) & is.finite(vmax)
  
  if (filtrar_negativos) {
    ok <- ok & vmin >= 0 & vmax >= 0
  }
  
  if (any(ok)) {
    polygon(
      c(x_fes[ok], rev(x_fes[ok])),
      c(vmin[ok], rev(vmax[ok])),
      col = adjustcolor(cor_sombra, alpha.f = 0.7),
      border = NA
    )
  }
  
  lines(x_fes, med_curve, col = cor_med, lwd = 3)
  
  if (!is.null(med_final) && is.finite(med_final)) {
    abline(h = med_final, col = "red", lty = 2, lwd = 2)
    
    legend(
      "bottomright",
      legend = c("Mediana", "Banda min-max", paste0("Mediana final: $", round(med_final))),
      col = c(cor_med, adjustcolor(cor_sombra, alpha.f = 0.7), "red"),
      lwd = c(3, NA, 2),
      pch = c(NA, 15, NA),
      lty = c(1, NA, 2),
      bty = "n"
    )
  } else {
    legend(
      "bottomright",
      legend = c("Mediana", "Banda min-max"),
      col = c(cor_med, adjustcolor(cor_sombra, alpha.f = 0.7)),
      lwd = c(3, NA),
      pch = c(NA, 15),
      lty = c(1, NA),
      bty = "n"
    )
  }
  
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
  
  cat("PDF guardado:", pdf_out, "\n")
  
  invisible(med_curve)
}

plot_dp_rep <- function(hists_dp, hists_rep, med_death, med_repair, pdf_out) {
  mat_dp <- build_mat(hists_dp, filtrar_negativos = TRUE)
  mat_rep <- build_mat(hists_rep, filtrar_negativos = TRUE)
  
  if (is.null(mat_dp) && is.null(mat_rep)) {
    cat("Sem dados comparativo.\n")
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
    main = paste0("DE O2: Death Penalty vs Repair - ", RUNS, " runs")
  )
  
  grid()
  
  if (!is.null(mat_dp)) {
    x_dp <- seq_len(nrow(mat_dp))
    ok_dp <- is.finite(vmin_dp) & is.finite(vmax_dp) & vmin_dp >= 0 & vmax_dp >= 0
    
    if (any(ok_dp)) {
      polygon(
        c(x_dp[ok_dp], rev(x_dp[ok_dp])),
        c(vmin_dp[ok_dp], rev(vmax_dp[ok_dp])),
        col = adjustcolor(sombra_dp, alpha.f = 0.7),
        border = NA
      )
    }
    
    lines(seq_along(curve_dp), curve_dp, col = cor_dp, lwd = 3)
  }
  
  if (!is.null(mat_rep)) {
    x_rep <- seq_len(nrow(mat_rep))
    ok_rep <- is.finite(vmin_rep) & is.finite(vmax_rep) & vmin_rep >= 0 & vmax_rep >= 0
    
    if (any(ok_rep)) {
      polygon(
        c(x_rep[ok_rep], rev(x_rep[ok_rep])),
        c(vmin_rep[ok_rep], rev(vmax_rep[ok_rep])),
        col = adjustcolor(sombra_rep, alpha.f = 0.7),
        border = NA
      )
    }
    
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
  
  cat("PDF guardado:", pdf_out, "\n")
}

# =============================================================================
# DE - O1
# =============================================================================
cat("=== DE - O1: maximizar lucro (", RUNS, "runs) ===\n")

MAXIT_O1 <- maxit * popSize

lucros_O1 <- numeric(RUNS)
hists_O1 <- vector("list", RUNS)
S_best_O1 <- NULL
lucro_best_O1 <- -Inf

t0_O1 <- proc.time()

for (run in 1:RUNS) {
  cat("run", run, "/", RUNS, "...\n")
  
  TYPE <- "max"
  EV <- 0
  BEST <- 0
  FHIST <- rep(NA, MAXIT_O1)
  
  m_eval_O1 <- function(S) {
    S <- normalize(S)
    res <- profit(S)
    EV <<- EV + 1
    BEST <<- g_best(BEST, res, TYPE)
    if (EV <= MAXIT_O1) FHIST[EV] <<- BEST
    return(-res)
  }
  
  set.seed(run)
  
  de <- DEoptim(
    fn = m_eval_O1,
    lower = lower,
    upper = upper,
    DEoptim.control(NP = popSize, itermax = maxit, trace = FALSE)
  )
  
  S_run <- normalize(de$optim$bestmem)
  lucro_run <- profit(S_run)
  lucros_O1[run] <- lucro_run
  
  h <- FHIST[is.finite(FHIST)]
  hists_O1[[run]] <- if (length(h) > 0) h else NULL
  
  if (lucro_run > lucro_best_O1) {
    lucro_best_O1 <- lucro_run
    S_best_O1 <- S_run
  }
  
  cat("  lucro run", run, ":", round(lucro_run), "$\n")
}

tempo_O1 <- (proc.time() - t0_O1)["elapsed"]

med_O1 <- median(lucros_O1, na.rm = TRUE)
media_O1 <- mean(lucros_O1, na.rm = TRUE)

cat("\n=== RESULTADO DE O1 ===\n")
cat("Mediana do lucro :", round(med_O1), "$\n")
cat("Media do lucro   :", round(media_O1), "$\n")
cat("Melhor lucro     :", round(lucro_best_O1), "$\n")
cat("Tempo total      :", round(tempo_O1, 1), "s\n")

plot_runs(
  hists_O1,
  titulo = paste0("DE O1 - ", RUNS, " runs"),
  pdf_out = file.path(output_dir_O1, "convergencia_DE_O1.pdf"),
  csv_out = file.path(output_dir_O1, "convergencia_DE_O1.csv"),
  cor_med = cor_O1,
  cor_sombra = sombra_O1,
  med_final = med_O1,
  filtrar_negativos = FALSE
)

pdf(file.path(output_dir_O1, "boxplot_DE_O1.pdf"), width = 7, height = 6)
boxplot(lucros_O1, col = cor_O1,
        main = paste("DE O1 - Distribuicao lucros (", RUNS, "runs)"),
        ylab = "Lucro ($)")
abline(h = med_O1, col = "red", lty = 2, lwd = 2)
dev.off()

write.csv(
  data.frame(
    Objetivo = "O1",
    Metodo = "DE",
    Previsao = "ARIMAX",
    Runs = RUNS,
    Lucro_Mediana = round(med_O1, 2),
    Lucro_Media = round(media_O1, 2),
    Lucro_Max = round(max(lucros_O1, na.rm = TRUE), 2),
    Lucro_Min = round(min(lucros_O1, na.rm = TRUE), 2),
    Unidades_Best = round(total_units(S_best_O1), 2),
    Total_HR_Best = sum(S_best_O1[idx_J]) + sum(S_best_O1[idx_X]),
    Tempo_Total_s = round(tempo_O1, 2)
  ),
  file = file.path(output_dir_O1, "tabela_resumo_DE_O1.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(Run = 1:RUNS, Lucro = round(lucros_O1, 2)),
  file = file.path(output_dir_O1, "lucros_runs_DE_O1.csv"),
  row.names = FALSE
)

# =============================================================================
# DE - O2
# =============================================================================
cat("\n=== DE - O2: Death Penalty e Repair (", RUNS, "runs cada) ===\n")

MAXIT_O2 <- maxit * popSize

# Death Penalty
lucros_dp <- numeric(RUNS)
hists_dp <- vector("list", RUNS)
S_best_dp <- NULL
lucro_best_dp <- -Inf

t0_O2 <- proc.time()

for (run in 1:RUNS) {
  cat("Death Penalty run", run, "/", RUNS, "...\n")
  
  TYPE <- "max"
  EV <- 0
  BEST <- -Inf
  FHIST <- rep(NA, MAXIT_O2)
  
  eval_death <- function(S) {
    S <- normalize(S)
    EV <<- EV + 1
    
    u <- total_units(S)
    
    if (!is.na(u) && u > LIMIT_UNITS) {
      excess <- (u - LIMIT_UNITS) / LIMIT_UNITS
      res <- profit(S) - 1e5 * excess
      
      if (EV <= MAXIT_O2) FHIST[EV] <<- ifelse(is.finite(BEST), BEST, res)
      return(-res)
    }
    
    res <- profit(S)
    BEST <<- g_best(BEST, res, TYPE)
    
    if (EV <= MAXIT_O2) FHIST[EV] <<- BEST
    return(-res)
  }
  
  set.seed(run)
  
  de <- DEoptim(
    fn = eval_death,
    lower = lower,
    upper = upper,
    DEoptim.control(NP = popSize, itermax = maxit, trace = FALSE)
  )
  
  S_run <- normalize(de$optim$bestmem)
  
  if (total_units(S_run) > LIMIT_UNITS) {
    S_run <- repair(S_run)
  }
  
  lucro_run <- profit(S_run)
  lucros_dp[run] <- lucro_run
  
  h <- FHIST[is.finite(FHIST)]
  hists_dp[[run]] <- if (length(h) > 0) h else NULL
  
  if (lucro_run > lucro_best_dp) {
    lucro_best_dp <- lucro_run
    S_best_dp <- S_run
  }
}

# Repair
lucros_rep <- numeric(RUNS)
hists_rep <- vector("list", RUNS)
S_best_rep <- NULL
lucro_best_rep <- -Inf

for (run in 1:RUNS) {
  cat("Repair run", run, "/", RUNS, "...\n")
  
  TYPE <- "max"
  EV <- 0
  BEST <- -Inf
  FHIST <- rep(NA, MAXIT_O2)
  
  eval_repair <- function(S) {
    S <- repair(S)
    EV <<- EV + 1
    
    res <- profit(S)
    BEST <<- g_best(BEST, res, TYPE)
    
    if (EV <= MAXIT_O2) FHIST[EV] <<- BEST
    return(-res)
  }
  
  set.seed(run)
  
  de <- DEoptim(
    fn = eval_repair,
    lower = lower,
    upper = upper,
    DEoptim.control(NP = popSize, itermax = maxit, trace = FALSE)
  )
  
  S_run <- repair(de$optim$bestmem)
  lucro_run <- profit(S_run)
  lucros_rep[run] <- lucro_run
  
  h <- FHIST[is.finite(FHIST)]
  hists_rep[[run]] <- if (length(h) > 0) h else NULL
  
  if (lucro_run > lucro_best_rep) {
    lucro_best_rep <- lucro_run
    S_best_rep <- S_run
  }
}

tempo_O2 <- (proc.time() - t0_O2)["elapsed"]

med_dp <- median(lucros_dp, na.rm = TRUE)
med_rep <- median(lucros_rep, na.rm = TRUE)

cat("\n=== RESULTADOS DE O2 ===\n")
cat("Death Penalty mediana:", round(med_dp), "\n")
cat("Repair mediana       :", round(med_rep), "\n")

plot_runs(
  hists_dp,
  titulo = paste0("DE O2 - Death Penalty - ", RUNS, " runs"),
  pdf_out = file.path(output_dir_O2, "convergencia_DE_O2_DeathPenalty.pdf"),
  csv_out = file.path(output_dir_O2, "convergencia_DE_O2_DeathPenalty.csv"),
  cor_med = cor_dp,
  cor_sombra = sombra_dp,
  med_final = NULL,
  filtrar_negativos = TRUE
)

plot_runs(
  hists_rep,
  titulo = paste0("DE O2 - Repair - ", RUNS, " runs"),
  pdf_out = file.path(output_dir_O2, "convergencia_DE_O2_Repair.pdf"),
  csv_out = file.path(output_dir_O2, "convergencia_DE_O2_Repair.csv"),
  cor_med = cor_rep,
  cor_sombra = sombra_rep,
  med_final = NULL,
  filtrar_negativos = TRUE
)

plot_dp_rep(
  hists_dp,
  hists_rep,
  med_dp,
  med_rep,
  pdf_out = file.path(output_dir_O2, "comparacao_DE_O2.pdf")
)

pdf(file.path(output_dir_O2, "boxplot_DE_O2.pdf"), width = 8, height = 6)
boxplot(
  list(
    DeathPenalty = lucros_dp[lucros_dp >= 0],
    Repair = lucros_rep[lucros_rep >= 0]
  ),
  col = c(cor_dp, cor_rep),
  main = paste("DE O2 - Distribuicao lucros positivos (", RUNS, "runs)"),
  ylab = "Lucro ($)"
)
dev.off()

write.csv(
  data.frame(
    Objetivo = c("O2", "O2"),
    Metodo = c("DE_DeathPenalty", "DE_Repair"),
    Previsao = c("ARIMAX", "ARIMAX"),
    Runs = c(RUNS, RUNS),
    Lucro_Mediana = round(c(med_dp, med_rep), 2),
    Lucro_Max = round(c(max(lucros_dp, na.rm = TRUE), max(lucros_rep, na.rm = TRUE)), 2),
    Lucro_Min = round(c(min(lucros_dp, na.rm = TRUE), min(lucros_rep, na.rm = TRUE)), 2),
    Unidades_Best = round(c(total_units(S_best_dp), total_units(S_best_rep)), 2),
    Tempo_Total_s = rep(round(tempo_O2, 2), 2)
  ),
  file = file.path(output_dir_O2, "tabela_resumo_DE_O2.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    Run = 1:RUNS,
    DeathPenalty = round(lucros_dp, 2),
    Repair = round(lucros_rep, 2)
  ),
  file = file.path(output_dir_O2, "lucros_runs_DE_O2.csv"),
  row.names = FALSE
)

cat("\n=== CONCLUIDO ===\n")
cat("Ficheiros O1 em:", output_dir_O1, "\n")
cat("Ficheiros O2 em:", output_dir_O2, "\n")