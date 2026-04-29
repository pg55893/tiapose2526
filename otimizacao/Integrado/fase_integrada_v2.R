# =============================================================
# fase_integrada_v2.R — Eduardo
# Integração Forecasting → Otimização
#
# Para cada uma das 12 semanas do backtesting (growing window,
# H=7, S=7, RUNS=12), gera as previsões de Num_Customers com
# os modelos escolhidos na Fase I (ETS para Baltimore, RF C2
# para Lancaster / Philadelphia / Richmond), usa esses valores
# como PREV e corre todos os algoritmos de otimização.
#
# Resultado: 12 profits medianos por algoritmo/objetivo.
# Métrica final: mediana dos 12 lucros medianos.
#
# Algoritmos: MC (O1/O2), HC (O1/O2), SANN (O1/O2),
#             GA - rbga (O1/O2), NSGA-II (O3)
# =============================================================

pkgs <- c("rminer", "forecast", "genalg", "mco")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org", type = "binary")
  }
  library(p, character.only = TRUE)
}

# --- Paths ---
BASE_DIR  <- "/Users/edias/TIAPOSE2526"
OUT_DIR   <- file.path(BASE_DIR, "otimizacao", "Integrado")
LOG_FILE  <- file.path(OUT_DIR, "log_fase_integrada_v2.txt")
sink(LOG_FILE, append = FALSE, split = TRUE)
cat("Log iniciado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# --- Config otimizacao (lojas, hr_cost, profit, eval, etc.) ---
source(file.path(BASE_DIR, "utils", "config_otimizacao.R"))

# --- Utilitarios multivariate (mfit, lforecastm, etc.) ---
source(file.path(BASE_DIR, "utils", "multi-utils.R"))

# --- Carregar dados ---
setwd(file.path(BASE_DIR, "data"))
source(file.path(BASE_DIR, "utils", "tratamentoDeDados.R"))

# --- Indices fixos (nao dependem de PREV) ---
idx_JX <- as.vector(outer(0:27, c(2, 3), function(b, o) b * 3 + o))
idx_PR <- seq(1, 84, by = 3)

# =============================================================
# PARÂMETROS GERAIS
# =============================================================
H     <- 7     # horizonte
S     <- 7     # salto entre iteracoes
RUNS  <- 6     # iteracoes de backtesting (prova de conceito; mudar para 12 na versao final)
W     <- 672   # janela inicial para 6 semanas: 714 - 7 - 5*7 = 672

# Parâmetros dos algoritmos (reduzidos para 12 semanas)
NRUNS_BT    <- 5     # runs por semana (prova de conceito; 10 na versao final)
N_ITER_MC   <- 5000  # MC: avaliacoes por run
MAX_ITER_HC <- 5000  # HC: iteracoes por run
MAXIT_SANN  <- 1500  # SANN: iteracoes (prova de conceito; 3000 na versao final)
T_SANN      <- 1000  # SANN: temperatura (pre-calibrada no v2)
POPSIZE_GA  <- 50    # GA: populacao
ITERS_GA    <- 100   # GA: geracoes
ELIT_GA     <- ceiling(POPSIZE_GA * 0.20)
POPSIZE_NS  <- 32    # NSGA-II: populacao (multiplo de 4 — requisito do mco::nsga2)
NGENS_NS    <- 100   # NSGA-II: geracoes (prova de conceito; 200 na versao final)
NRUNS_NS    <- 3     # NSGA-II: runs por semana (prova de conceito; 5 na versao final)

cat("=== Parametros ===\n")
cat("NRUNS_BT    =", NRUNS_BT, "\n")
cat("N_ITER_MC   =", N_ITER_MC, "\n")
cat("MAX_ITER_HC =", MAX_ITER_HC, "\n")
cat("MAXIT_SANN  =", MAXIT_SANN, "| T_SANN =", T_SANN, "\n")
cat("GA: popSize =", POPSIZE_GA, "| iters =", ITERS_GA, "| elit =", ELIT_GA, "\n")
cat("NSGA2: pop =", POPSIZE_NS, "| gens =", NGENS_NS, "| runs =", NRUNS_NS, "\n\n")

# =============================================================
# SECÇÃO 1 — FUNÇÕES DE PREVISÃO (modelos escolhidos Fase I)
# =============================================================

# --- ETS/tslm para Baltimore ---
forecast_baltimore_ets <- function(b) {
  d   <- baltimore[order(baltimore$Date), ]
  d1  <- d$Num_Customers
  tour <- as.numeric(d$TouristEvent == "Yes")
  emp  <- d$Num_Employees

  H_idx   <- holdout(d1, ratio = H, mode = "incremental",
                     iter = b, window = W, increment = S)
  Y_tr    <- d1[H_idx$tr]
  tour_tr <- tour[H_idx$tr]
  emp_tr  <- emp[H_idx$tr]
  tour_ts <- tour[H_idx$ts]
  emp_ts  <- emp[H_idx$ts]

  ts_tr   <- ts(Y_tr, frequency = H)
  xreg_tr <- data.frame(TouristEvent = tour_tr, Num_Employees = emp_tr)
  fit_lm  <- tslm(ts_tr ~ TouristEvent + Num_Employees, data = xreg_tr)
  fit_ets <- suppressWarnings(ets(residuals(fit_lm)))

  xreg_ts <- data.frame(TouristEvent = tour_ts, Num_Employees = emp_ts)
  pred_lm  <- as.numeric(predict(fit_lm,  newdata = xreg_ts))
  pred_ets <- as.numeric(forecast(fit_ets, h = H)$mean)
  Pred     <- round(pmax(pred_lm + pred_ets, 0))

  datas_fecho <- as.Date(c("2012-12-25", "2013-12-25", "2013-03-31", "2014-04-20"))
  datas_ts    <- d$Date[H_idx$ts]
  Pred[datas_ts %in% datas_fecho] <- 0

  return(as.integer(Pred))
}

# --- RF Multivariado C2 para Lancaster, Philadelphia, Richmond ---
VINP_RF <- list(
  list(c(1:3, 7, 28), c(1, 7)),        # modelo NC:    lags NC + lags Sales
  list(c(1, 7),       c(1:3, 7, 28))   # modelo Sales: lags NC + lags Sales
)
MAX_LAG_RF <- 28

forecast_rf_c2 <- function(dados, b) {
  d    <- dados[order(dados$Date), ]
  nc   <- d$Num_Customers
  sa   <- d$Sales
  L    <- length(nc)
  ND   <- L - MAX_LAG_RF
  W_rf <- (ND - H) - (RUNS - 1) * S  # = 602 (mesmo que W=630 em termos orig.)

  end_tr   <- W_rf + (b - 1) * S
  start_ts <- end_tr + 1
  end_ts   <- start_ts + H - 1
  tr_orig  <- 1:(end_tr + MAX_LAG_RF)
  ts_orig  <- (start_ts + MAX_LAG_RF):(end_ts + MAX_LAG_RF)

  exogen_full <- cbind(
    TouristEvent  = as.numeric(d$TouristEvent == "Yes"),
    Num_Employees = d$Num_Employees,
    Pct_On_Sale   = d$Pct_On_Sale
  )

  mtr_train <- cbind(nc[tr_orig], sa[tr_orig])
  exo_train <- exogen_full[tr_orig, ]
  exo_test  <- exogen_full[ts_orig, ]

  MRF   <- mfit(mtr_train, "randomForest", VINP_RF, exogen = exo_train)
  Pred  <- round(pmax(lforecastm(MRF, h = H, exogen = exo_test)[[1]], 0))

  datas_fecho <- as.Date(c("2012-12-25", "2013-12-25", "2013-03-31", "2014-04-20"))
  datas_ts    <- d$Date[ts_orig]
  Pred[datas_ts %in% datas_fecho] <- 0

  return(as.integer(Pred))
}

# --- Gera PREV completo para a semana b (28 valores) ---
gerar_prev_semana <- function(b) {
  cat(sprintf("  [Forecasting] Semana %d: Baltimore(ETS)...", b))
  prev_bal <- forecast_baltimore_ets(b)
  cat(" OK | Lancaster(RF)...")
  prev_lan <- forecast_rf_c2(lancaster, b)
  cat(" OK | Philadelphia(RF)...")
  prev_phi <- forecast_rf_c2(philadelphia, b)
  cat(" OK | Richmond(RF)...")
  prev_ric <- forecast_rf_c2(richmond, b)
  cat(" OK\n")
  cat(sprintf("    Baltimore: %s\n", paste(prev_bal, collapse=" ")))
  cat(sprintf("    Lancaster: %s\n", paste(prev_lan, collapse=" ")))
  cat(sprintf("    Philadelphia: %s\n", paste(prev_phi, collapse=" ")))
  cat(sprintf("    Richmond: %s\n", paste(prev_ric, collapse=" ")))
  c(prev_bal, prev_lan, prev_phi, prev_ric)
}

# =============================================================
# SECÇÃO 2 — FUNÇÕES DOS ALGORITMOS
# Cada função recebe objetivo ("O1","O2") e devolve mediana de
# NRUNS_BT runs. Usa PREV/upper do ambiente global.
# =============================================================

# Repair rapido (30 x 0.90, identico ao v2)
repair_fast <- function(S) {
  S <- pmin(pmax(S, lower), upper)
  for (k in 1:30) {
    u <- total_units(S)
    if (!is.na(u) && u <= 10000) return(S)
    S[idx_PR] <- pmax(S[idx_PR] * 0.90, 0)
  }
  if (!is.na(total_units(S)) && total_units(S) <= 10000) return(S)
  return(NULL)
}

# ---- MONTE CARLO ----
run_MC <- function(objetivo) {
  lucros <- numeric(NRUNS_BT)

  upper_mc <- upper
  if (objetivo == "O2") {
    upper_mc[seq(2, 84, by=3)] <- ceiling(upper[seq(2, 84, by=3)] / 6)
    upper_mc[seq(3, 84, by=3)] <- ceiling(upper[seq(3, 84, by=3)] / 6)
  }

  for (r in 1:NRUNS_BT) {
    best_L <- -Inf
    best_S <- NULL
    for (k in 1:N_ITER_MC) {
      S <- pmin(pmax(runif(84, lower, upper_mc), lower), upper_mc)
      if (objetivo == "O1") {
        p <- profit(S)
        if (p > best_L) { best_L <- p; best_S <- S }
      } else {
        u <- total_units(S)
        if (!is.na(u) && u <= 10000) {
          p <- profit(S)
          if (p > best_L) { best_L <- p; best_S <- S }
        }
      }
    }
    if (!is.null(best_S)) {
      best_S[idx_JX] <- round(best_S[idx_JX])
      u <- total_units(best_S)
      if (objetivo == "O2" && (is.na(u) || u > 10000)) {
        lucros[r] <- NA
      } else {
        lucros[r] <- profit(best_S)
      }
    } else {
      lucros[r] <- NA
    }
  }
  median(lucros, na.rm = TRUE)
}

# ---- HILL CLIMBING ----
vizinhanca_mult <- function(S, sigma = 0.15) {
  S_new <- S * runif(84, 1 - sigma, 1 + sigma)
  S_new[idx_JX] <- round(S_new[idx_JX])
  pmin(pmax(S_new, lower), upper)
}

run_HC <- function(objetivo) {
  lucros <- numeric(NRUNS_BT)
  sigma  <- if (objetivo == "O2") 0.10 else 0.15

  for (r in 1:NRUNS_BT) {
    if (objetivo == "O1") {
      s0 <- runif(84) * (upper - lower) + lower
    } else {
      s0 <- lower
      s0[idx_PR] <- 0.05
      s0[seq(2, 84, by=3)] <- 1
      s0[seq(3, 84, by=3)] <- 1
      s0 <- pmin(pmax(s0 + runif(84) * 0.2, lower), upper)
    }
    S_curr <- pmin(pmax(s0, lower), upper)
    S_curr[idx_JX] <- round(S_curr[idx_JX])

    if (objetivo == "O1") {
      f_curr <- -profit(S_curr)
      for (iter in 1:MAX_ITER_HC) {
        S_new <- vizinhanca_mult(S_curr, sigma)
        f_new <- -profit(S_new)
        if (is.finite(f_new) && f_new < f_curr) {
          S_curr <- S_new; f_curr <- f_new
        }
      }
      S_curr[idx_JX] <- round(S_curr[idx_JX])
      lucros[r] <- profit(S_curr)
    } else {
      # O2 com repair embutido
      ev <- local({
        bv <- -Inf; bs <- NULL
        list(
          fn = function(S) {
            S   <- pmin(pmax(S, lower), upper)
            S_r <- repair_fast(S)
            if (is.null(S_r)) return(Inf)
            p <- profit(S_r)
            if (p > bv) { bv <<- p; bs <<- S_r }
            -p
          },
          get_lucro = function() bv,
          get_best  = function() bs
        )
      })
      f_curr <- ev$fn(S_curr)
      for (iter in 1:MAX_ITER_HC) {
        S_new <- vizinhanca_mult(S_curr, sigma)
        f_new <- ev$fn(S_new)
        if (is.finite(f_new) && f_new < f_curr) {
          S_curr <- S_new; f_curr <- f_new
        }
      }
      L_run <- ev$get_lucro()
      S_run <- ev$get_best()
      if (!is.null(S_run) && L_run > -Inf) {
        u <- total_units(S_run)
        lucros[r] <- if (!is.na(u) && u <= 10000) L_run else NA
      } else {
        lucros[r] <- NA
      }
    }
  }
  median(lucros, na.rm = TRUE)
}

# ---- SIMULATED ANNEALING ----
run_SANN <- function(objetivo) {
  lucros <- numeric(NRUNS_BT)

  for (r in 1:NRUNS_BT) {
    if (objetivo == "O1") {
      s0 <- runif(84) * (upper - lower) + lower
      sa <- optim(par = s0,
                  fn  = function(S) { S <- pmin(pmax(S, lower), upper); -profit(S) },
                  method  = "SANN",
                  control = list(maxit = MAXIT_SANN, temp = T_SANN, trace = FALSE))
      S_tmp <- pmin(pmax(sa$par, lower), upper)
      S_tmp[idx_JX] <- round(S_tmp[idx_JX])
      lucros[r] <- profit(S_tmp)
    } else {
      # O2 com repair
      ev <- local({
        bv <- -Inf; bs <- NULL
        list(
          fn = function(S) {
            S   <- pmin(pmax(S, lower), upper)
            S_r <- repair_fast(S)
            if (is.null(S_r)) return(Inf)
            p <- profit(S_r)
            if (p > bv) { bv <<- p; bs <<- S_r }
            -p
          },
          get_lucro = function() bv,
          get_best  = function() bs
        )
      })
      s0         <- lower
      s0[idx_PR] <- 0.05
      s0[seq(2, 84, by=3)] <- 1
      s0[seq(3, 84, by=3)] <- 1
      s0 <- pmin(pmax(s0 + runif(84) * 0.2, lower), upper)

      optim(par = s0, fn = ev$fn, method = "SANN",
            control = list(maxit = MAXIT_SANN * 2, temp = T_SANN, trace = FALSE))

      L_run <- ev$get_lucro()
      S_run <- ev$get_best()
      if (!is.null(S_run) && L_run > -Inf) {
        u <- total_units(S_run)
        lucros[r] <- if (!is.na(u) && u <= 10000) L_run else NA
      } else {
        lucros[r] <- NA
      }
    }
  }
  median(lucros, na.rm = TRUE)
}

# ---- GA (rbga) ----
# Monitorizacao FES (padrao OTIM_GA_v2.R)
g_best_ga <- function(v1, v2) min(c(v1, v2))
m_eval_ga <- function(S) {
  res  <- EVAL_FUN_GA(S)
  FES_GA      <<- FES_GA + 1L
  BEST_GA     <<- g_best_ga(BEST_GA, res)
  if (FES_GA <= MAXIT_GA_EST) FES_HIST_GA[FES_GA] <<- BEST_GA
  return(res)
}
MAXIT_GA_EST <- POPSIZE_GA + (ITERS_GA - 1) * (POPSIZE_GA - ELIT_GA)

# Sugestoes validas para O2 warm-start
gera_sugestoes_validas_ga <- function(N = 10) {
  D      <- 84
  idx_J2 <- seq(2, D, by = 3)
  idx_X2 <- seq(3, D, by = 3)
  mat    <- matrix(NA_real_, nrow = N, ncol = D)
  for (n in 1:N) {
    cand <- numeric(D)
    cand[idx_PR] <- runif(length(idx_PR), 0, 0.30)
    for (k in 1:200) {
      pos <- sample(28, 1)
      var <- sample(c("J","X"), 1)
      idx_c <- if (var == "J") idx_J2[pos] else idx_X2[pos]
      ub    <- upper[idx_c]
      if (cand[idx_c] >= ub) next
      cand[idx_c] <- cand[idx_c] + 1
      if (total_units(cand) > 10000) { cand[idx_c] <- cand[idx_c] - 1; break }
    }
    mat[n, ] <- pmin(pmax(cand, lower), upper)
  }
  mat
}

run_GA <- function(objetivo) {
  lucros <- numeric(NRUNS_BT)

  eval_ga_O1 <- function(S) { S <- pmin(pmax(S, lower), upper); -profit(S) }
  eval_ga_O2 <- function(S) {
    S <- pmin(pmax(S, lower), upper)
    u <- total_units(S)
    if (is.na(u) || u > 10000) return(Inf)
    -profit(S)
  }
  EVAL_FUN_GA <<- if (objetivo == "O1") eval_ga_O1 else eval_ga_O2

  for (r in 1:NRUNS_BT) {
    set.seed(2000 + r)
    FES_GA      <<- 0L
    BEST_GA     <<- Inf
    FES_HIST_GA <<- rep(NA_real_, MAXIT_GA_EST)

    args_rbga <- list(
      stringMin      = lower,
      stringMax      = upper,
      popSize        = POPSIZE_GA,
      iters          = ITERS_GA,
      mutationChance = 0.10,
      elitism        = ELIT_GA,
      evalFunc       = m_eval_ga
    )
    if (objetivo == "O2") {
      sugest <- gera_sugestoes_validas_ga(N = ceiling(POPSIZE_GA * 0.20))
      if (!is.null(sugest) && nrow(sugest) > 0) args_rbga$suggestions <- sugest
    }

    GA <- do.call(rbga, args_rbga)

    PMIN   <- which.min(GA$evaluations)
    S_best <- GA$population[PMIN, ]
    idx_J2 <- seq(2, 84, by = 3); idx_X2 <- seq(3, 84, by = 3)
    S_best[idx_J2] <- round(S_best[idx_J2])
    S_best[idx_X2] <- round(S_best[idx_X2])

    if (objetivo == "O2") {
      u <- total_units(S_best)
      lucros[r] <- if (!is.na(u) && u <= 10000) profit(S_best) else NA
    } else {
      lucros[r] <- profit(S_best)
    }
  }
  median(lucros, na.rm = TRUE)
}

# ---- NSGA-II (O3: max lucro + min HR, com restricao <= 10000 unidades) ----
total_HR_fn <- function(S) sum(round(S[idx_JX]))

obj_O3_fn <- function(S) {
  if (any(S[idx_PR] >= 1, na.rm = TRUE)) return(c(Inf, Inf))
  u <- total_units(S)
  if (is.na(u) || u > 10000) {
    excesso <- ifelse(is.na(u), 1e6, u - 10000)
    return(c(excesso * 10, 1000))
  }
  c(-profit(S), total_HR_fn(S))
}

run_NSGA2 <- function() {
  hvs    <- numeric(NRUNS_NS)
  lucros <- numeric(NRUNS_NS)

  for (r in 1:NRUNS_NS) {
    set.seed(3000 + r)
    res <- nsga2(
      fn           = obj_O3_fn,
      idim         = 84,
      odim         = 2,
      lower.bounds = lower,
      upper.bounds = upper,
      popsize      = POPSIZE_NS,
      generations  = NGENS_NS,
      cprob        = 0.7, cdist = 5,
      mprob        = 0.1, mdist = 10
    )
    vals   <- res$value
    validas <- which(is.finite(vals[,1]) & is.finite(vals[,2]) & vals[,1] < 1e5)
    if (length(validas) == 0) {
      hvs[r] <- 0; lucros[r] <- NA; next
    }
    # Hipervolume 2D (ponto de referencia nadir + 10%)
    vv  <- vals[validas, , drop = FALSE]
    ref <- c(max(vv[,1]) * 1.1, max(vv[,2]) * 1.1)
    ord <- order(vv[,1])
    vv_s <- vv[ord, ]
    hv <- 0; cur_y <- ref[2]
    for (i in seq_len(nrow(vv_s))) {
      if (vv_s[i,2] < cur_y) {
        w <- if (i < nrow(vv_s)) vv_s[i+1,1] - vv_s[i,1] else ref[1] - vv_s[i,1]
        hv <- hv + w * (cur_y - vv_s[i,2])
        cur_y <- vv_s[i,2]
      }
    }
    hvs[r] <- hv

    # Escalarizacao W=0.75: seleciona solucao de compromisso
    lucros_p <- -vals[validas, 1]
    hr_p     <-  vals[validas, 2]
    l_norm   <- (lucros_p - min(lucros_p)) / (max(lucros_p) - min(lucros_p) + 1e-9)
    h_norm   <- (hr_p     - min(hr_p))     / (max(hr_p)     - min(hr_p)     + 1e-9)
    score    <- 0.75 * l_norm - 0.25 * h_norm
    i_best   <- validas[which.max(score)]
    S_w      <- pmin(pmax(res$par[i_best, ], lower), upper)
    S_w[idx_JX] <- round(S_w[idx_JX])
    lucros[r] <- profit(S_w)
  }
  # Metrica: mediana dos lucros das solucoes de compromisso
  list(
    mediana_lucro = median(lucros, na.rm = TRUE),
    mediana_hv    = median(hvs)
  )
}

# =============================================================
# SECÇÃO 3 — LOOP PRINCIPAL: 12 SEMANAS
# =============================================================
cat("\n=== FASE INTEGRADA — 12 SEMANAS ===\n")
cat(sprintf("%-20s : %s\n", "Algoritmos", "MC, HC, SANN, GA, NSGA2"))
cat(sprintf("%-20s : %s\n", "Objetivos",  "O1 (max profit), O2 (<=10k unidades), O3 (bi-obj)"))
cat(sprintf("%-20s : %d runs/semana\n", "NRUNS_BT", NRUNS_BT))
cat(sprintf("%-20s : %d runs/semana\n", "NRUNS_NS (NSGA2)", NRUNS_NS))
cat("\n")

# Matrizes de resultados: RUNS linhas × colunas por algoritmo/objetivo
alg_cols <- c("MC_O1","MC_O2","HC_O1","HC_O2",
               "SANN_O1","SANN_O2","GA_O1","GA_O2","NSGA2_O3")
mat_profits <- matrix(NA_real_, nrow = RUNS, ncol = length(alg_cols))
colnames(mat_profits) <- alg_cols

mat_hv_nsga2 <- numeric(RUNS)
prev_matrix  <- matrix(NA_integer_, nrow = RUNS, ncol = 28)

t_total_start <- proc.time()

for (b in 1:RUNS) {
  cat(sprintf("\n========================================\n"))
  cat(sprintf("SEMANA %d / %d\n", b, RUNS))
  cat(sprintf("========================================\n"))
  t_sem <- proc.time()

  # --- 1. Gerar previsoes ---
  PREV_b <- gerar_prev_semana(b)
  prev_matrix[b, ] <- PREV_b

  # --- 2. Actualizar globals (PREV e upper) ---
  # profit() e total_units() usam PREV do ambiente global
  PREV  <<- PREV_b
  upper <<- calc_upper(PREV)
  cat(sprintf("  PREV actualizado | upper J max=%d X max=%d\n",
              max(upper[seq(2,84,by=3)]), max(upper[seq(3,84,by=3)])))

  # Solucao de teste rapida
  cat(sprintf("  profit(S_test) = %.2f | units = %d\n",
              profit(rep(c(0.10, 5, 3), 28)),
              total_units(rep(c(0.10, 5, 3), 28))))

  # --- 3. Monte Carlo ---
  cat("  [MC O1] ... ")
  set.seed(100 + b)
  mat_profits[b, "MC_O1"] <- run_MC("O1")
  cat(sprintf("med=%.2f\n", mat_profits[b, "MC_O1"]))

  cat("  [MC O2] ... ")
  set.seed(200 + b)
  mat_profits[b, "MC_O2"] <- run_MC("O2")
  cat(sprintf("med=%.2f\n", mat_profits[b, "MC_O2"]))

  # --- 4. Hill Climbing ---
  cat("  [HC O1] ... ")
  set.seed(300 + b)
  mat_profits[b, "HC_O1"] <- run_HC("O1")
  cat(sprintf("med=%.2f\n", mat_profits[b, "HC_O1"]))

  cat("  [HC O2] ... ")
  set.seed(400 + b)
  mat_profits[b, "HC_O2"] <- run_HC("O2")
  cat(sprintf("med=%.2f\n", mat_profits[b, "HC_O2"]))

  # --- 5. SANN ---
  cat("  [SANN O1] ... ")
  set.seed(500 + b)
  mat_profits[b, "SANN_O1"] <- run_SANN("O1")
  cat(sprintf("med=%.2f\n", mat_profits[b, "SANN_O1"]))

  cat("  [SANN O2] ... ")
  set.seed(600 + b)
  mat_profits[b, "SANN_O2"] <- run_SANN("O2")
  cat(sprintf("med=%.2f\n", mat_profits[b, "SANN_O2"]))

  # --- 6. GA ---
  cat("  [GA O1]  ... ")
  set.seed(700 + b)
  mat_profits[b, "GA_O1"] <- run_GA("O1")
  cat(sprintf("med=%.2f\n", mat_profits[b, "GA_O1"]))

  cat("  [GA O2]  ... ")
  set.seed(800 + b)
  mat_profits[b, "GA_O2"] <- run_GA("O2")
  cat(sprintf("med=%.2f\n", mat_profits[b, "GA_O2"]))

  # --- 7. NSGA-II (O3) ---
  cat("  [NSGA2 O3] ... ")
  set.seed(900 + b)
  res_ns <- run_NSGA2()
  mat_profits[b, "NSGA2_O3"] <- res_ns$mediana_lucro
  mat_hv_nsga2[b]             <- res_ns$mediana_hv
  cat(sprintf("med_lucro=%.2f | med_HV=%.3g\n",
              mat_profits[b, "NSGA2_O3"], mat_hv_nsga2[b]))

  t_sem_elapsed <- (proc.time() - t_sem)["elapsed"]
  cat(sprintf("  Semana %d concluida em %.1fs\n", b, t_sem_elapsed))
}

t_total_elapsed <- (proc.time() - t_total_start)["elapsed"]
cat(sprintf("\n=== Loop completo em %.1fs (%.1f min) ===\n",
            t_total_elapsed, t_total_elapsed / 60))

# =============================================================
# SECÇÃO 4 — RESULTADOS FINAIS
# =============================================================
cat("\n=== TABELA COMPARATIVA FINAL ===\n")
cat("Metrica: mediana dos lucros medianos das 12 semanas\n\n")

# Tabela principal
mediana_final <- apply(mat_profits, 2, median, na.rm = TRUE)
media_final   <- apply(mat_profits, 2, mean,   na.rm = TRUE)
min_final     <- apply(mat_profits, 2, min,    na.rm = TRUE)
max_final     <- apply(mat_profits, 2, max,    na.rm = TRUE)
sd_final      <- apply(mat_profits, 2, sd,     na.rm = TRUE)

tabela_final <- data.frame(
  Algoritmo = c("MC","MC","HC","HC","SANN","SANN","GA (rbga)","GA (rbga)","NSGA-II"),
  Objetivo  = c("O1","O2","O1","O2","O1","O2","O1","O2","O3"),
  Mediana   = round(mediana_final, 2),
  Media     = round(media_final,   2),
  Min       = round(min_final,     2),
  Max       = round(max_final,     2),
  SD        = round(sd_final,      2),
  row.names = NULL
)
print(tabela_final, row.names = FALSE)

# =============================================================
# SECÇÃO 5 — EXPORTAR
# =============================================================

# 5a) Tabela final
write.csv(tabela_final,
          file.path(OUT_DIR, "tabela_comparativa_final.csv"),
          row.names = FALSE)

# 5b) Profits por semana (matriz)
df_mat <- as.data.frame(mat_profits)
df_mat$semana <- 1:RUNS
df_mat <- df_mat[, c("semana", alg_cols)]
write.csv(df_mat,
          file.path(OUT_DIR, "profits_por_semana.csv"),
          row.names = FALSE)

# 5c) PREV das 12 semanas
df_prev <- as.data.frame(prev_matrix)
colnames(df_prev) <- c(
  paste0("Bal_d", 1:7),
  paste0("Lan_d", 1:7),
  paste0("Phi_d", 1:7),
  paste0("Ric_d", 1:7)
)
df_prev$semana <- 1:RUNS
write.csv(df_prev[, c("semana", colnames(df_prev)[-29])],
          file.path(OUT_DIR, "prev_12_semanas.csv"),
          row.names = FALSE)

# Paleta e nomes comuns a todos os graficos
cores_alg <- c(MC="lightblue", HC="lightgreen", SANN="salmon", GA="plum")
ALG_NOMES <- c("MC","HC","SANN","GA")

# ---- GRAFICO 1: Boxplot O1 e O2 lado a lado ----
pdf(file.path(OUT_DIR, "g1_boxplot_O1_O2.pdf"), width = 12, height = 6)
par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 1))

dados_O1 <- mat_profits[, c("MC_O1","HC_O1","SANN_O1","GA_O1")]
colnames(dados_O1) <- ALG_NOMES
boxplot(dados_O1, main = "O1 — Max Profit (sem restricao)",
        ylab = "Lucro ($)", col = cores_alg, las = 2, frame = FALSE)
abline(h = 0, lty = 2, col = "grey50")
for (j in seq_along(ALG_NOMES))
  text(j, mediana_final[paste0(ALG_NOMES[j],"_O1")],
       labels = sprintf("%.0f", mediana_final[paste0(ALG_NOMES[j],"_O1")]),
       pos = 3, cex = 0.75, col = "grey20", font = 2)

dados_O2 <- mat_profits[, c("MC_O2","HC_O2","SANN_O2","GA_O2")]
colnames(dados_O2) <- ALG_NOMES
boxplot(dados_O2, main = "O2 — Max Profit (<=10k unidades)",
        ylab = "Lucro ($)", col = cores_alg, las = 2, frame = FALSE)
abline(h = 0, lty = 2, col = "grey50")
for (j in seq_along(ALG_NOMES))
  text(j, mediana_final[paste0(ALG_NOMES[j],"_O2")],
       labels = sprintf("%.0f", mediana_final[paste0(ALG_NOMES[j],"_O2")]),
       pos = 3, cex = 0.75, col = "grey20", font = 2)
par(mfrow = c(1, 1))
dev.off()
cat("G1 boxplot O1/O2 guardado.\n")

# ---- GRAFICO 2: Evolucao do lucro por semana (linha temporal) ----
pdf(file.path(OUT_DIR, "g2_lucro_por_semana_O1.pdf"), width = 11, height = 5)
cores_linha <- c(MC="steelblue", HC="forestgreen", SANN="firebrick", GA="darkorchid")
y_rng <- range(mat_profits[, paste0(ALG_NOMES,"_O1")], na.rm = TRUE)
plot(1:RUNS, mat_profits[,"MC_O1"], type = "b", pch = 16, col = cores_linha["MC"],
     lwd = 2, ylim = y_rng, xlab = "Semana de Backtesting", ylab = "Lucro ($)",
     main = "O1 — Lucro por Semana (cada algoritmo)", xaxt = "n", frame = FALSE)
axis(1, at = 1:RUNS, labels = paste0("S", 1:RUNS))
grid(col = "grey90", lty = 1)
for (alg in ALG_NOMES[-1])
  lines(1:RUNS, mat_profits[, paste0(alg,"_O1")],
        type = "b", pch = 16, col = cores_linha[alg], lwd = 2)
legend("bottomright",
       legend = paste0(ALG_NOMES, " (med=",
                       round(mediana_final[paste0(ALG_NOMES,"_O1")], 0), ")"),
       col = cores_linha, lwd = 2, pch = 16, bty = "n", cex = 0.85)
dev.off()
cat("G2 lucro por semana O1 guardado.\n")

pdf(file.path(OUT_DIR, "g3_lucro_por_semana_O2.pdf"), width = 11, height = 5)
y_rng2 <- range(mat_profits[, paste0(ALG_NOMES,"_O2")], na.rm = TRUE)
plot(1:RUNS, mat_profits[,"MC_O2"], type = "b", pch = 16, col = cores_linha["MC"],
     lwd = 2, ylim = y_rng2, xlab = "Semana de Backtesting", ylab = "Lucro ($)",
     main = "O2 — Lucro por Semana (cada algoritmo)", xaxt = "n", frame = FALSE)
axis(1, at = 1:RUNS, labels = paste0("S", 1:RUNS))
grid(col = "grey90", lty = 1)
abline(h = 0, lty = 2, col = "grey60")
for (alg in ALG_NOMES[-1])
  lines(1:RUNS, mat_profits[, paste0(alg,"_O2")],
        type = "b", pch = 16, col = cores_linha[alg], lwd = 2)
legend("bottomright",
       legend = paste0(ALG_NOMES, " (med=",
                       round(mediana_final[paste0(ALG_NOMES,"_O2")], 0), ")"),
       col = cores_linha, lwd = 2, pch = 16, bty = "n", cex = 0.85)
dev.off()
cat("G3 lucro por semana O2 guardado.\n")

# ---- GRAFICO 4: Barplot medianas — O1 vs O2 vs O3 ----
pdf(file.path(OUT_DIR, "g4_barplot_medianas.pdf"), width = 11, height = 6)
par(mar = c(5, 5, 3, 1))
cols_O1_v <- mediana_final[paste0(ALG_NOMES,"_O1")]
cols_O2_v <- mediana_final[paste0(ALG_NOMES,"_O2")]
med_O3    <- mediana_final["NSGA2_O3"]
mat_bar   <- rbind(O1 = cols_O1_v, O2 = cols_O2_v)
colnames(mat_bar) <- ALG_NOMES
ymax <- max(c(mat_bar, med_O3), na.rm = TRUE) * 1.15
ymin <- min(c(mat_bar, 0),      na.rm = TRUE) * 1.10
bp <- barplot(mat_bar, beside = TRUE,
              col    = c("steelblue","tomato"),
              border = "white",
              ylim   = c(ymin, ymax),
              ylab   = "Mediana Lucro ($)",
              main   = sprintf("Comparacao Final — Mediana de %d Semanas", RUNS),
              names.arg = ALG_NOMES, las = 1)
# valores em cima de cada barra
for (i in 1:2)
  for (j in 1:length(ALG_NOMES)) {
    v <- mat_bar[i, j]
    if (!is.na(v))
      text(bp[i,j], v + ymax*0.01,
           labels = sprintf("%.0f", v), cex = 0.7, font = 2,
           col = ifelse(v >= 0, "grey20", "firebrick"))
  }
# linha NSGA-II O3
abline(h = med_O3, lty = 2, col = "darkgreen", lwd = 2)
text(max(bp) * 0.85, med_O3 + ymax*0.02,
     sprintf("NSGA-II O3 = %.0f", med_O3), col = "darkgreen", cex = 0.85, font = 2)
abline(h = 0, lty = 1, col = "grey40")
legend("topright", legend = c("O1","O2","NSGA-II O3"),
       fill = c("steelblue","tomato",NA), border = c("white","white",NA),
       lty = c(NA, NA, 2), col = c(NA, NA, "darkgreen"),
       lwd = c(NA, NA, 2), bty = "n", cex = 0.9,
       x.intersp = c(1,1,0.5))
dev.off()
cat("G4 barplot medianas guardado.\n")

# ---- GRAFICO 5: NSGA-II O3 por semana ----
pdf(file.path(OUT_DIR, "g5_nsga2_O3_por_semana.pdf"), width = 9, height = 5)
par(mar = c(4, 4.5, 3, 1))
plot(1:RUNS, mat_profits[,"NSGA2_O3"], type = "b", pch = 19, col = "darkgreen",
     lwd = 2, xlab = "Semana de Backtesting", ylab = "Lucro Compromisso ($)",
     main = sprintf("NSGA-II O3 — Lucro por Semana (W=0.75, mediana=%.0f)",
                    mediana_final["NSGA2_O3"]),
     xaxt = "n", frame = FALSE)
axis(1, at = 1:RUNS, labels = paste0("S", 1:RUNS))
grid(col = "grey90", lty = 1)
abline(h = mediana_final["NSGA2_O3"], lty = 2, col = "firebrick", lwd = 1.5)
text(RUNS * 0.1, mediana_final["NSGA2_O3"] * 1.05,
     sprintf("Mediana = %.0f", mediana_final["NSGA2_O3"]),
     col = "firebrick", cex = 0.85, font = 2)
points(which.min(mat_profits[,"NSGA2_O3"]),
       min(mat_profits[,"NSGA2_O3"], na.rm=TRUE),
       pch = 4, col = "firebrick", cex = 1.5, lwd = 2)
dev.off()
cat("G5 NSGA2 O3 por semana guardado.\n")

# ---- GRAFICO 6: Heatmap — profit por algoritmo e semana ----
pdf(file.path(OUT_DIR, "g6_heatmap_profits.pdf"), width = 10, height = 6)
par(mar = c(5, 6, 3, 5))
alg_todas <- c("MC_O1","HC_O1","SANN_O1","GA_O1",
                "MC_O2","HC_O2","SANN_O2","GA_O2","NSGA2_O3")
mat_heat  <- t(mat_profits[, alg_todas])
zlim      <- range(mat_heat, na.rm = TRUE)
image(1:RUNS, seq_along(alg_todas), t(mat_heat),
      col  = colorRampPalette(c("firebrick","white","steelblue"))(50),
      zlim = zlim,
      xlab = "Semana", ylab = "",
      main = "Heatmap — Lucro por Algoritmo e Semana",
      xaxt = "n", yaxt = "n")
axis(1, at = 1:RUNS, labels = paste0("S", 1:RUNS))
axis(2, at = seq_along(alg_todas),
     labels = gsub("_", " ", alg_todas), las = 2, cex.axis = 0.85)
# valores nas celulas
for (i in 1:RUNS)
  for (j in seq_along(alg_todas)) {
    v <- mat_heat[j, i]
    if (!is.na(v))
      text(i, j, sprintf("%.0f", v), cex = 0.6, font = 2,
           col = ifelse(abs(v) > diff(zlim)*0.4, "white", "grey20"))
  }
dev.off()
cat("G6 heatmap profits guardado.\n")

# ---- GRAFICO 7: Previsoes PREV por semana (media das 4 lojas) ----
pdf(file.path(OUT_DIR, "g7_prev_por_semana.pdf"), width = 10, height = 5)
par(mar = c(4, 4.5, 3, 1))
med_prev_bal <- rowMeans(prev_matrix[, 1:7])
med_prev_lan <- rowMeans(prev_matrix[, 8:14])
med_prev_phi <- rowMeans(prev_matrix[, 15:21])
med_prev_ric <- rowMeans(prev_matrix[, 22:28])
y_rng_p <- range(med_prev_bal, med_prev_lan, med_prev_phi, med_prev_ric)
plot(1:RUNS, med_prev_bal, type = "b", pch = 16, col = "steelblue", lwd = 2,
     ylim = y_rng_p, xlab = "Semana", ylab = "Media Num_Customers previsto",
     main = "Previsoes PREV — Media Diaria por Loja e Semana",
     xaxt = "n", frame = FALSE)
axis(1, at = 1:RUNS, labels = paste0("S", 1:RUNS))
grid(col = "grey90", lty = 1)
lines(1:RUNS, med_prev_lan, type="b", pch=16, col="tomato",     lwd=2)
lines(1:RUNS, med_prev_phi, type="b", pch=16, col="forestgreen",lwd=2)
lines(1:RUNS, med_prev_ric, type="b", pch=16, col="darkorange", lwd=2)
legend("topright", legend = c("Baltimore","Lancaster","Philadelphia","Richmond"),
       col = c("steelblue","tomato","forestgreen","darkorange"),
       lwd = 2, pch = 16, bty = "n", cex = 0.85)
dev.off()
cat("G7 PREV por semana guardado.\n")

# 5g) RDS com tudo
saveRDS(
  list(
    mat_profits    = mat_profits,
    mat_hv_nsga2   = mat_hv_nsga2,
    mediana_final  = mediana_final,
    tabela_final   = tabela_final,
    prev_matrix    = prev_matrix,
    params = list(
      NRUNS_BT=NRUNS_BT, N_ITER_MC=N_ITER_MC,
      MAX_ITER_HC=MAX_ITER_HC, MAXIT_SANN=MAXIT_SANN, T_SANN=T_SANN,
      POPSIZE_GA=POPSIZE_GA, ITERS_GA=ITERS_GA,
      POPSIZE_NS=POPSIZE_NS, NGENS_NS=NGENS_NS, NRUNS_NS=NRUNS_NS
    )
  ),
  file.path(OUT_DIR, "resultado_fase_integrada.rds")
)
cat("RDS completo guardado.\n")

# =============================================================
# SUMÁRIO FINAL
# =============================================================
cat("\n=== SUMÁRIO FASE INTEGRADA ===\n")
cat(sprintf("%-10s %-6s %12s %12s %12s\n",
            "Algoritmo","Obj","Mediana","Min","Max"))
cat(strrep("-", 55), "\n")
for (i in seq_len(nrow(tabela_final))) {
  cat(sprintf("%-10s %-6s %12.2f %12.2f %12.2f\n",
              tabela_final$Algoritmo[i],
              tabela_final$Objetivo[i],
              tabela_final$Mediana[i],
              tabela_final$Min[i],
              tabela_final$Max[i]))
}
cat(strrep("-", 55), "\n")
cat(sprintf("\nTempo total: %.1f min\n", t_total_elapsed / 60))
cat("\n=== FIM fase_integrada_v2.R ===\n")
cat("Log guardado em:", LOG_FILE, "\n")
sink()
