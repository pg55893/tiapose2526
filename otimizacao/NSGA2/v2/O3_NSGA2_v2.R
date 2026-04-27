# =============================================================
# O3_NSGA2_v2.R — Eduardo
# Melhorias:
#   - Múltiplas runs (20) com mediana dos hipervolumes
#   - Escalarização normalizada com diferentes valores de W
#   - Comparação melhor ponto Pareto vs melhor O2
#   - FES no eixo X nas convergências
# =============================================================

library(mco)
source("/Users/edias/TIAPOSE2526/utils/config_otimizacao.R")

idx_JX <- as.vector(outer(0:27, c(2, 3), function(b, o) b * 3 + o))
idx_PR <- seq(1, 84, by = 3)

NRUNS   <- 20     # runs para mediana hipervolumes
POPSIZE <- 52
NGENS   <- 500

# =============================================================
# FUNÇÕES AUXILIARES
# =============================================================
total_HR <- function(S) sum(round(S[idx_JX]))

obj_O3 <- function(S) {
  if (any(S[idx_PR] >= 1, na.rm = TRUE)) return(c(Inf, Inf))
  u <- total_units(S)
  if (is.na(u) || u > 10000) {
    excesso <- ifelse(is.na(u), 1e6, u - 10000)
    return(c(excesso * 10, 1000))
  }
  c(-profit(S), total_HR(S))
}

# Hipervolume 2D com ponto de referência (nadir + margem)
hypervolume_2d <- function(vals, ref) {
  # vals: matriz Nx2 (minimização); ref: ponto de referência
  valid <- vals[is.finite(vals[, 1]) & is.finite(vals[, 2]) &
                  vals[, 1] < 1e5, , drop = FALSE]
  if (nrow(valid) == 0) return(0)
  ord <- order(valid[, 1])
  vals_s <- valid[ord, ]
  hv <- 0
  prev_x <- ref[1]
  for (i in nrow(vals_s):1) {
    if (vals_s[i, 2] < ref[2]) {
      width  <- ref[1] - vals_s[i, 1]
      height <- ref[2] - vals_s[i, 2]
      if (width > 0 && height > 0) hv <- hv + width * height
    }
  }
  # Simpler approach: sweep
  hv2 <- 0
  cur_y_max <- ref[2]
  for (i in seq_len(nrow(vals_s))) {
    if (vals_s[i, 2] < cur_y_max) {
      if (i < nrow(vals_s)) {
        width <- vals_s[i + 1, 1] - vals_s[i, 1]
      } else {
        width <- ref[1] - vals_s[i, 1]
      }
      hv2 <- hv2 + width * (cur_y_max - vals_s[i, 2])
      cur_y_max <- vals_s[i, 2]
    }
  }
  return(max(hv, hv2))
}

# =============================================================
# SECÇÃO 1 — 20 RUNS NSGA-II: mediana dos hipervolumes
# =============================================================
cat("=== O3: NSGA-II — 20 runs para mediana hipervolumes ===\n\n")

hvs      <- numeric(NRUNS)
pareto_vals_runs <- list()
pareto_pops_runs <- list()

set.seed(42)
for (i in 1:NRUNS) {
  res <- nsga2(
    fn           = obj_O3,
    idim         = 84,
    odim         = 2,
    lower.bounds = lower,
    upper.bounds = upper,
    popsize      = POPSIZE,
    generations  = NGENS,
    cprob        = 0.7,
    cdist        = 5,
    mprob        = 0.1,
    mdist        = 10
  )
  pareto_vals_runs[[i]] <- res$value
  pareto_pops_runs[[i]] <- res$par

  # ponto de referência: nadir + 10%
  valid_vals <- res$value[is.finite(res$value[, 1]) & res$value[, 1] < 1e5, ]
  if (nrow(valid_vals) > 0) {
    ref <- c(max(valid_vals[, 1]) * 1.1, max(valid_vals[, 2]) * 1.1)
    hvs[i] <- hypervolume_2d(valid_vals, ref)
  } else {
    hvs[i] <- 0
  }
  cat(sprintf("Run %2d | Pareto validas: %2d | HV: %.4g\n",
              i,
              sum(is.finite(res$value[, 1]) & res$value[, 1] < 1e5),
              hvs[i]))
}

med_hv <- median(hvs)
cat(sprintf("\n>> Mediana Hipervolumes (20 runs) : %.4g\n", med_hv))
cat(sprintf(">> Min HV : %.4g | Max HV : %.4g\n", min(hvs), max(hvs)))

# Usa run com hipervolume mais próximo da mediana para análise
idx_med_run <- which.min(abs(hvs - med_hv))
cat(sprintf(">> Run representativa (HV=%.4g): Run %d\n", hvs[idx_med_run], idx_med_run))

pareto_vals <- pareto_vals_runs[[idx_med_run]]
pareto_pops <- pareto_pops_runs[[idx_med_run]]

validas <- which(is.finite(pareto_vals[, 1]) & is.finite(pareto_vals[, 2]) &
                   pareto_vals[, 1] < 1e5)

if (length(validas) == 0) {
  stop("Nenhuma solucao valida na run representativa. Aumentar NGENS ou POPSIZE.")
}

lucros_pareto <- -pareto_vals[validas, 1]
hr_pareto     <-  pareto_vals[validas, 2]

cat(sprintf("\n>> Fronteira Pareto (run %d): %d solucoes\n", idx_med_run, length(validas)))
cat(sprintf(">> Lucro: [%.2f, %.2f]\n", min(lucros_pareto), max(lucros_pareto)))
cat(sprintf(">> HR:    [%d, %d]\n", min(hr_pareto), max(hr_pareto)))

# =============================================================
# SECÇÃO 2 — ESCALARIZAÇÃO COM DIFERENTES VALORES DE W
# W ∈ [0,1]: score = W * lucro_norm + (1-W) * (-HR_norm)
# =============================================================
cat("\n=== Escalarizacao normalizada com W ===\n")

lucros_norm <- (lucros_pareto - min(lucros_pareto)) /
  (max(lucros_pareto) - min(lucros_pareto) + 1e-9)
hr_norm     <- (hr_pareto - min(hr_pareto)) /
  (max(hr_pareto) - min(hr_pareto) + 1e-9)

W_vals <- c(0.25, 0.50, 0.75, 0.90)

resultados_W <- lapply(W_vals, function(W) {
  score    <- W * lucros_norm - (1 - W) * hr_norm
  idx_best <- which.max(score)
  idx_sol  <- validas[idx_best]
  S_w      <- pareto_pops[idx_sol, ]
  S_w      <- pmin(pmax(S_w, lower), upper)
  S_w[idx_JX] <- round(S_w[idx_JX])
  list(
    W        = W,
    lucro    = round(profit(S_w), 2),
    hr       = total_HR(S_w),
    unidades = total_units(S_w),
    S        = S_w
  )
})

cat(sprintf("%-6s | %8s | %6s | %8s\n", "W", "Lucro", "HR", "Unidades"))
cat(strrep("-", 35), "\n")
for (r in resultados_W) {
  cat(sprintf("%-6.2f | %8.2f | %6d | %8d\n", r$W, r$lucro, r$hr, r$unidades))
}

# Solução de compromisso: W=0.75 (privilegia lucro)
sol_compromisso <- resultados_W[[which(W_vals == 0.75)]]
S_best_O3 <- sol_compromisso$S
cat(sprintf("\n>> Compromisso (W=0.75): Lucro=%.2f | HR=%d | Unidades=%d\n",
            sol_compromisso$lucro, sol_compromisso$hr, sol_compromisso$unidades))

# =============================================================
# SECÇÃO 3 — COMPARAÇÃO MELHOR PONTO PARETO vs MELHOR O2
# =============================================================
cat("\n=== Comparacao: melhor ponto Pareto vs melhor O2 (SANN) ===\n")

resultado_SANN_file <- "/Users/edias/TIAPOSE2526/otimizacao/SANN/v2/resultado_SANN.csv"
if (file.exists(resultado_SANN_file)) {
  res_sann <- read.csv(resultado_SANN_file, stringsAsFactors = FALSE)
  res_O2   <- res_sann[toupper(res_sann$objetivo) == "O2", ]
  if (nrow(res_O2) > 0) {
    lucro_O2_sann   <- as.numeric(res_O2$lucro[1])
    unidades_O2_sann <- as.numeric(res_O2$unidades[1])
    hr_O2_sann       <- as.numeric(res_O2$total_HR[1])
    cat(sprintf("SANN O2 : Lucro=%.2f | HR=%d | Unidades=%d\n",
                lucro_O2_sann, hr_O2_sann, unidades_O2_sann))
  } else {
    lucro_O2_sann <- NA; hr_O2_sann <- NA; unidades_O2_sann <- NA
  }
} else {
  lucro_O2_sann <- NA; hr_O2_sann <- NA; unidades_O2_sann <- NA
  cat("AVISO: resultado_SANN.csv nao encontrado - comparacao sem SANN O2.\n")
}

cat(sprintf("NSGA-II O3 (W=0.75): Lucro=%.2f | HR=%d | Unidades=%d\n",
            sol_compromisso$lucro, sol_compromisso$hr, sol_compromisso$unidades))

# =============================================================
# SECÇÃO 4 — GRÁFICOS
# =============================================================

# 4a) Fronteira de Pareto com pontos de W e SANN O2
pdf("/Users/edias/TIAPOSE2526/otimizacao/NSGA2/pareto_O3_NSGA2_v2.pdf", width = 10, height = 7)
ord <- order(hr_pareto)
plot(hr_pareto[ord], lucros_pareto[ord],
     xlab = "Total HR (minimizar)", ylab = "Lucro (maximizar)",
     main = sprintf("Fronteira Pareto O3 - NSGA-II (HV mediana=%.3g, %d runs)",
                    med_hv, NRUNS),
     pch = 19, col = "steelblue", cex = 0.8, type = "b", lwd = 1.2)

# Pontos para cada W
cores_W  <- c("orange", "purple", "firebrick", "darkgreen")
pchs_W   <- c(17, 15, 18, 16)
for (j in seq_along(resultados_W)) {
  r <- resultados_W[[j]]
  points(r$hr, r$lucro, pch = pchs_W[j], col = cores_W[j], cex = 2.0)
  text(r$hr, r$lucro,
       labels = sprintf("W=%.2f\nL=%.0f", r$W, r$lucro),
       pos = 4, col = cores_W[j], cex = 0.75)
}

# Ponto SANN O2 (se existir)
if (!is.na(lucro_O2_sann)) {
  points(hr_O2_sann, lucro_O2_sann, pch = 8, col = "black", cex = 2.2, lwd = 2)
  text(hr_O2_sann, lucro_O2_sann,
       labels = sprintf("SANN O2\nL=%.0f", lucro_O2_sann),
       pos = 2, col = "black", cex = 0.75)
}

leg_lbl <- c("Fronteira Pareto",
             paste0("W=", W_vals),
             "SANN O2")
leg_col <- c("steelblue", cores_W, "black")
leg_pch <- c(19, pchs_W, 8)
legend("bottomright", legend = leg_lbl, col = leg_col, pch = leg_pch,
       pt.cex = 1.5, bty = "n", cex = 0.85)
dev.off()
cat("Grafico Pareto v2 guardado.\n")

# 4b) Boxplot hipervolumes entre runs
pdf("/Users/edias/TIAPOSE2526/otimizacao/NSGA2/boxplot_hv_O3.pdf", width = 7, height = 5)
boxplot(hvs,
        ylab = "Hipervolume", main = sprintf("Hipervolumes NSGA-II O3 (%d runs)", NRUNS),
        col = "steelblue", border = "navy")
abline(h = med_hv, lty = 2, col = "firebrick", lwd = 2)
text(1.35, med_hv, sprintf("Mediana=%.3g", med_hv), col = "firebrick", cex = 0.85)
dev.off()
cat("Boxplot HV guardado.\n")

# 4c) Escalarização: Lucro vs HR por valor de W
pdf("/Users/edias/TIAPOSE2526/otimizacao/NSGA2/escalarizacao_W_O3.pdf", width = 8, height = 5)
lucros_W <- sapply(resultados_W, `[[`, "lucro")
hr_W     <- sapply(resultados_W, `[[`, "hr")
plot(W_vals, lucros_W, type = "b", pch = 19, col = "steelblue", lwd = 2,
     xlab = "Peso W (lucro)", ylab = "Lucro da solucao selecionada",
     main = "Escalarizacao NSGA-II O3: impacto de W no lucro",
     ylim = range(lucros_W) * c(0.95, 1.05))
par(new = TRUE)
plot(W_vals, hr_W, type = "b", pch = 17, col = "firebrick", lwd = 2,
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4, col = "firebrick", col.axis = "firebrick")
mtext("Total HR", side = 4, line = 2.5, col = "firebrick")
legend("right", legend = c("Lucro (eixo esq.)", "HR (eixo dir.)"),
       col = c("steelblue", "firebrick"), pch = c(19, 17), lwd = 2, bty = "n")
dev.off()
cat("Grafico escalarizacao W guardado.\n")

# =============================================================
# SECÇÃO 5 — EXPORTAR RESULTADOS
# =============================================================
resultado_O3 <- data.frame(
  membro        = "Eduardo",
  algoritmo     = "NSGA2",
  objetivo      = "O3",
  lucro         = sol_compromisso$lucro,
  mediana_HV    = round(med_hv, 4),
  unidades      = sol_compromisso$unidades,
  total_HR      = sol_compromisso$hr,
  W_compromisso = 0.75,
  n_runs        = NRUNS,
  stringsAsFactors = FALSE
)
write.csv(resultado_O3,
          "/Users/edias/TIAPOSE2526/otimizacao/NSGA2/resultado_Eduardo_NSGA2.csv",
          row.names = FALSE)

pareto_df <- data.frame(lucro = lucros_pareto, total_HR = hr_pareto)
write.csv(pareto_df,
          "/Users/edias/TIAPOSE2526/otimizacao/NSGA2/pareto_O3_fronteira.csv",
          row.names = FALSE)

# Hipervolumes por run
hv_df <- data.frame(run = 1:NRUNS, hypervolume = hvs)
write.csv(hv_df,
          "/Users/edias/TIAPOSE2526/otimizacao/NSGA2/hipervolumes_O3_runs.csv",
          row.names = FALSE)

cat("\n=== Resultado O3 ===\n")
print(resultado_O3)
cat("\n=== FIM O3_NSGA2_v2.R ===\n")
