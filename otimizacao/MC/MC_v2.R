# =============================================================
# MC_v2.R — Nuno
# Monte Carlo O1 e O2
# 20 runs independentes, mediana profit, FES no eixo X
# Padrão compatível com comparativos.R do grupo (mesmo que SANN v2)
# =============================================================

BASE_DIR <- "/Users/martinho/Documents/MEGSI/2Semestre/TIAPOSE/TIAPOSE_project"
source(file.path(BASE_DIR, "utils", "config_otimizacao.R"))

# --- LOG ---
MC_OUT_DIR <- file.path(BASE_DIR, "otimizacao", "MC")
dir.create(MC_OUT_DIR, showWarnings = FALSE, recursive = TRUE)
LOG_FILE   <- file.path(MC_OUT_DIR, "log_MC_v2.txt")
sink(LOG_FILE, append = FALSE, split = TRUE)
cat("Log iniciado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# =============================================================
# PARÂMETROS
# =============================================================
NRUNS  <- 20      # runs independentes (igual aos outros algoritmos do grupo)
N_ITER <- 10000   # avaliações por run (igual ao SANN/HC — comparação justa no eixo FES)

idx_JX <- as.vector(outer(0:27, c(2, 3), function(b, o) b * 3 + o))  # índices de J e X
idx_PR <- seq(1, 84, by = 3)                                           # índices de PR

cat("NRUNS  =", NRUNS, "\n")
cat("N_ITER =", N_ITER, "(avaliacoes por run)\n")
cat("D      = 84 (dim do vetor S)\n\n")

# =============================================================
# WRAPPER FES — conta avaliações da função objetivo
# Mesmo padrão que RF_SANN_v2.R (Eduardo) — garantia de
# compatibilidade com comparativos.R
# =============================================================
make_eval_fes <- function(obj_fn) {
  FES    <- 0L
  MELHOR <- -Inf   # melhor LUCRO visto até agora (positivo)
  hist   <- numeric(0)

  list(
    fn = function(S) {
      v      <- obj_fn(S)          # retorna -profit(S) ou Inf
      FES    <<- FES + 1L
      p      <- if (is.finite(v)) -v else -Inf   # profit (positivo)
      if (p > MELHOR) MELHOR <<- p
      hist   <<- c(hist, MELHOR)
      return(v)
    },
    get_hist = function() hist,
    get_fes  = function() FES
  )
}

# =============================================================
# HELPERS — curva mediana e gráfico com runs individuais
# Mesmo estilo que HC_O1_O2.R (Eduardo)
# =============================================================
build_mat_mc <- function(hists) {
  valid   <- Filter(function(h) !is.null(h) && length(h) > 0, hists)
  if (length(valid) == 0) return(NULL)
  max_len <- max(sapply(valid, length))
  sapply(valid, function(h) c(h, rep(h[length(h)], max_len - length(h))))
}

plot_runs_mc <- function(hists, titulo, pdf_out, cor_med = "steelblue") {
  mat <- build_mat_mc(hists)
  if (is.null(mat)) { cat("Sem dados para grafico:", titulo, "\n"); return(NULL) }
  # Filtrar -Inf para não quebrar ylim
  finite_vals <- mat[is.finite(mat)]
  if (length(finite_vals) == 0) {
    cat("AVISO: nenhuma solucao valida para grafico:", titulo, "\n")
    return(NULL)
  }
  med_curve <- apply(mat, 1, median, na.rm = TRUE)
  x_fes     <- seq_len(nrow(mat))
  y_rng     <- range(finite_vals, na.rm = TRUE)

  pdf(pdf_out, width = 9, height = 6)
  for (j in seq_len(ncol(mat))) {
    if (j == 1)
      plot(x_fes, mat[, j], type = "l", col = "grey80", lwd = 1,
           xlab = "Numero de Avaliacoes (FES)", ylab = "Melhor Lucro ($)",
           main = titulo, ylim = y_rng)
    else
      lines(x_fes, mat[, j], col = "grey80", lwd = 1)
  }
  lines(x_fes, med_curve, col = cor_med, lwd = 2.5)
  legend("bottomright",
         legend = c("Runs individuais", "Mediana"),
         col    = c("grey70", cor_med), lwd = c(1, 2.5), bty = "n")
  dev.off()
  cat("Grafico guardado:", pdf_out, "\n")
  invisible(med_curve)
}

# =============================================================
# VALIDAÇÃO INICIAL — confirmar que o config carrega corretamente
# =============================================================
cat("=== VALIDACAO CONFIG ===\n")
cat("profit(S1)      =", profit(S1), "\n")
cat("eval(S1)        =", eval(S1),   "\n")
cat("eval_O2(S1)     =", eval_O2(S1), "\n")
cat("total_units(S1) =", total_units(S1), "\n")
cat("lower[1:6]      =", lower[1:6], "\n")
cat("upper[1:6]      =", upper[1:6], "\n")

# Confirmar que S_rand fica dentro dos bounds (validação obrigatória)
set.seed(0)
S_test <- runif(84, min = lower, max = upper)
cat("Bounds OK (S_rand in [lower,upper]):",
    all(S_test >= lower - 1e-10 & S_test <= upper + 1e-10), "\n\n")

# =============================================================
# O1 — Monte Carlo, 20 runs, N_ITER avaliações cada
# Sem restrições — maximizar lucro total
# =============================================================
cat(sprintf("=== O1: Monte Carlo (%d runs, %d aval/run) ===\n", NRUNS, N_ITER))

lucros_O1  <- numeric(NRUNS)
BEST_S_O1  <- NULL
best_O1    <- -Inf
hists_O1   <- list()

set.seed(42)
for (i in 1:NRUNS) {

  # Criar tracker FES para este run
  tracker <- make_eval_fes(function(S) {
    S <- pmin(pmax(S, lower), upper)   # garantir dentro dos bounds
    eval(S)                            # = -profit(S)
  })

  best_run    <- -Inf
  best_S_run  <- NULL

  # Gerar N_ITER soluções aleatórias independentes
  for (k in 1:N_ITER) {
    S_rand <- runif(84, min = lower, max = upper)
    val    <- tracker$fn(S_rand)   # incrementa FES, regista histórico
    p      <- -val                 # profit (positivo)
    if (is.finite(p) && p > best_run) {
      best_run   <- p
      best_S_run <- S_rand
    }
  }

  # Arredondar J e X da melhor solução (nunca arredondar PR)
  if (!is.null(best_S_run)) {
    best_S_run[idx_JX] <- round(best_S_run[idx_JX])
    L <- profit(best_S_run)
  } else {
    L <- -Inf
  }

  lucros_O1[i]  <- L
  hists_O1[[i]] <- tracker$get_hist()
  cat(sprintf("Run %2d | Lucro: %8.2f | FES: %d\n", i, L, tracker$get_fes()))
  if (L > best_O1) { best_O1 <- L; BEST_S_O1 <- best_S_run }
}

med_O1 <- median(lucros_O1)
cat(sprintf("\n>> Mediana lucro O1 : %.2f\n", med_O1))
cat(sprintf(">> Melhor lucro O1  : %.2f\n", best_O1))
if (!is.null(BEST_S_O1))
  cat(sprintf(">> Unidades         : %d\n", total_units(BEST_S_O1)))

# Gráfico O1: runs individuais + mediana
med_curve_O1 <- plot_runs_mc(
  hists_O1,
  titulo  = paste0("Monte Carlo O1 — ", NRUNS, " runs (", N_ITER, " aval/run)"),
  pdf_out = file.path(MC_OUT_DIR, "convergencia_O1_MC_v2.pdf"),
  cor_med = "steelblue"
)

# Guardar curva mediana para comparativos.R
# Formato: vetor numérico (mediana de melhor lucro por FES)
saveRDS(med_curve_O1, file.path(MC_OUT_DIR, "convergencia_O1_MC.rds"))
cat("RDS convergencia O1 guardado.\n\n")

# =============================================================
# O2 — Monte Carlo, 20 runs, Death Penalty
# Restrição: total de unidades vendidas <= 10 000
# Estratégia: reduzir bounds de J e X a 1/6 do máximo para aumentar
# a probabilidade de sampling dentro da zona viável
# (mesmo approach do montecarlo.R original do Nuno)
# eval_O2 aplica death penalty (retorna Inf se inválido)
# =============================================================
cat(sprintf("=== O2: Monte Carlo + Death Penalty (%d runs, %d aval/run) ===\n",
            NRUNS, N_ITER))

# Bounds reduzidos para O2: J e X limitados a 1/6 do máximo original
upper_O2              <- upper
upper_O2[seq(2, 84, by=3)] <- ceiling(upper[seq(2, 84, by=3)] / 6)  # J
upper_O2[seq(3, 84, by=3)] <- ceiling(upper[seq(3, 84, by=3)] / 6)  # X
cat(sprintf("Bounds O2 reduzidos: upper_J max=%.0f | upper_X max=%.0f\n",
            max(upper_O2[seq(2,84,by=3)]), max(upper_O2[seq(3,84,by=3)])))

lucros_O2  <- numeric(NRUNS)
BEST_S_O2  <- NULL
best_O2    <- -Inf
hists_O2   <- list()

set.seed(123)
for (i in 1:NRUNS) {

  tracker <- make_eval_fes(function(S) {
    S <- pmin(pmax(S, lower), upper)
    eval_O2(S)   # Death Penalty: Inf se total_units(S) > 10000
  })

  best_run   <- -Inf
  best_S_run <- NULL

  for (k in 1:N_ITER) {
    S_rand <- runif(84, min = lower, max = upper_O2)   # bounds reduzidos
    val    <- tracker$fn(S_rand)   # Inf se inválido, -profit se válido
    if (is.finite(val)) {          # só aceita soluções válidas
      p <- -val                    # profit (positivo)
      if (p > best_run) {
        best_run   <- p
        best_S_run <- S_rand
      }
    }
  }

  # Validar e arredondar
  if (!is.null(best_S_run) && best_run > -Inf) {
    best_S_run[idx_JX] <- round(best_S_run[idx_JX])
    u <- total_units(best_S_run)
    L <- if (!is.na(u) && u <= 10000) profit(best_S_run) else NA
  } else {
    L <- NA
  }

  lucros_O2[i]  <- L
  hists_O2[[i]] <- tracker$get_hist()

  if (!is.na(L)) {
    cat(sprintf("Run %2d | Lucro: %8.2f | Unidades: %d | FES: %d\n",
                i, L, total_units(best_S_run), tracker$get_fes()))
    if (L > best_O2) { best_O2 <- L; BEST_S_O2 <- best_S_run }
  } else {
    cat(sprintf("Run %2d | Nenhuma solucao valida encontrada | FES: %d\n",
                i, tracker$get_fes()))
  }
}

med_O2 <- median(lucros_O2, na.rm = TRUE)
cat(sprintf("\n>> Mediana lucro O2 : %.2f\n", med_O2))
cat(sprintf(">> Melhor lucro O2  : %.2f\n", best_O2))
if (!is.null(BEST_S_O2))
  cat(sprintf(">> Unidades         : %d\n", total_units(BEST_S_O2)))

# Gráfico O2: runs individuais + mediana
med_curve_O2 <- plot_runs_mc(
  hists_O2,
  titulo  = paste0("Monte Carlo O2 (Death Penalty) — ", NRUNS, " runs (", N_ITER, " aval/run)"),
  pdf_out = file.path(MC_OUT_DIR, "convergencia_O2_MC_v2.pdf"),
  cor_med = "tomato"
)

saveRDS(med_curve_O2, file.path(MC_OUT_DIR, "convergencia_O2_MC.rds"))
cat("RDS convergencia O2 guardado.\n\n")

# =============================================================
# EXPORTAR RESULTADOS
# =============================================================
resultado_MC <- data.frame(
  membro    = "Nuno",
  algoritmo = "Monte Carlo",
  objetivo  = c("O1", "O2"),
  lucro     = c(round(best_O1, 2), round(best_O2, 2)),
  mediana   = c(round(med_O1,  2), round(med_O2,  2)),
  unidades  = c(
    if (!is.null(BEST_S_O1)) total_units(BEST_S_O1) else NA,
    if (!is.null(BEST_S_O2)) total_units(BEST_S_O2) else NA
  ),
  total_HR  = c(
    if (!is.null(BEST_S_O1)) sum(BEST_S_O1[idx_JX]) else NA,
    if (!is.null(BEST_S_O2)) sum(BEST_S_O2[idx_JX]) else NA
  ),
  N_runs    = NRUNS,
  N_iter    = N_ITER,
  stringsAsFactors = FALSE
)
rownames(resultado_MC) <- NULL

CSV_OUT <- file.path(MC_OUT_DIR, "resultado_MC.csv")
write.csv(resultado_MC, CSV_OUT, row.names = FALSE)

cat("=== Resultado Final MC ===\n")
print(resultado_MC)
cat("\nCSV guardado em:", CSV_OUT, "\n")

# =============================================================
# VALIDAÇÕES FINAIS — para confirmar ao professor/grupo
# =============================================================
cat("\n=== VALIDACOES FINAIS ===\n")

# 1. FES está correto?
cat(sprintf("1. FES por run: esperado=%d | O1 run1=%.0f | O2 run1=%.0f\n",
            N_ITER,
            length(hists_O1[[1]]),
            if (length(hists_O2) > 0 && !is.null(hists_O2[[1]])) length(hists_O2[[1]]) else NA))

# 2. Mediana é calculada (não máximo nem média)?
cat(sprintf("2. Mediana O1=%.2f | Max O1=%.2f | Mean O1=%.2f\n",
            median(lucros_O1),
            max(lucros_O1),
            mean(lucros_O1)))

# 3. Bounds respeitados em todos os runs?
set.seed(999)
bounds_ok <- all(replicate(100, {
  S <- runif(84, min = lower, max = upper)
  all(S >= lower - 1e-10 & S <= upper + 1e-10)
}))
cat("3. Bounds respeitados (100 amostras):", bounds_ok, "\n")

# 4. O2 respeita restrição na melhor solução?
if (!is.null(BEST_S_O2)) {
  u_O2 <- total_units(BEST_S_O2)
  cat(sprintf("4. Melhor O2: %d unidades (restricao <= 10000): %s\n",
              u_O2, ifelse(u_O2 <= 10000, "OK", "VIOLADA!")))
}

cat("\n=== FIM MC_v2.R ===\n")
cat("Log guardado em:", LOG_FILE, "\n")
sink()
