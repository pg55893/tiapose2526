# =============================================================
# rbga_bin_O1_O2.R — Nuno (Bonus Tarefa 5)
# Algoritmo Genetico binario (rbga.bin) para O1 e O2
# Mesma codificacao binaria do tabu_O1_O2.R:
#   476 bits (28 posicoes x 17 bits: PR=5, J=6, X=6)
# 20 runs, mediana profit, FES no eixo X
# Padrao: identical ao OTIM_GA_v2.R (Joao) mas com bits
# =============================================================

library(genalg)

BASE_DIR <- "/Users/martinho/Documents/MEGSI/2Semestre/TIAPOSE/TIAPOSE_project"
source(file.path(BASE_DIR, "utils", "config_otimizacao.R"))

GABIN_DIR <- file.path(BASE_DIR, "otimizacao", "GA_bin")
dir.create(GABIN_DIR, showWarnings = FALSE)
LOG_FILE  <- file.path(GABIN_DIR, "log_gabin.txt")
sink(LOG_FILE, append = FALSE, split = TRUE)
cat("Log iniciado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# =============================================================
# CODIFICACAO BINARIA (identica ao tabu_O1_O2.R)
# =============================================================
N_BITS_PR  <- 5L
N_BITS_J   <- 6L
N_BITS_X   <- 6L
N_BITS_POS <- N_BITS_PR + N_BITS_J + N_BITS_X   # 17
N_BITS     <- 28L * N_BITS_POS                   # 476

decode_S <- function(bits) {
  S <- numeric(84)
  for (pos in 1:28) {
    st  <- (pos - 1L) * N_BITS_POS + 1L
    idx <- (pos - 1L) * 3L + 1L

    pr_int    <- sum(bits[st:(st+N_BITS_PR-1L)] * 2^((N_BITS_PR-1L):0L))
    S[idx]    <- pr_int / (2^N_BITS_PR - 1) * 0.30

    j_int     <- sum(bits[(st+N_BITS_PR):(st+N_BITS_PR+N_BITS_J-1L)] * 2^((N_BITS_J-1L):0L))
    S[idx+1L] <- min(j_int, upper[idx+1L])

    x_int     <- sum(bits[(st+N_BITS_PR+N_BITS_J):(st+N_BITS_POS-1L)] * 2^((N_BITS_X-1L):0L))
    S[idx+2L] <- min(x_int, upper[idx+2L])
  }
  return(S)
}

# =============================================================
# VALIDACAO
# =============================================================
cat("=== VALIDACAO CODIFICACAO ===\n")
cat("N_BITS =", N_BITS, "\n")
set.seed(0)
bits_test <- sample(0:1, N_BITS, replace = TRUE)
S_test    <- decode_S(bits_test)
cat("length(S)      =", length(S_test), "\n")
cat("PR em [0,0.30]:", all(S_test[idx_PR] >= 0 & S_test[idx_PR] <= 0.30), "\n")
cat("profit(S_test) =", profit(S_test), "\n\n")

# =============================================================
# PARAMETROS GA BINARIO
# =============================================================
NRUNS   <- 20
POPSIZE <- 50
ITERS   <- 200
MUTCH   <- 0.01    # mutacao por bit (baixa — string longa)
ELIT    <- ceiling(POPSIZE * 0.10)

# FES estimado: POPSIZE + (ITERS-1)*(POPSIZE-ELIT)
MAXIT <- POPSIZE + (ITERS - 1L) * (POPSIZE - ELIT)

cat(sprintf("NRUNS=%d | POPSIZE=%d | ITERS=%d | MUTCH=%.3f | FES/run~%d\n\n",
            NRUNS, POPSIZE, ITERS, MUTCH, MAXIT))

# =============================================================
# MONITOR FES — mesmo padrao que OTIM_GA_v2.R (Joao)
# rbga.bin MINIMIZA evalFunc
# =============================================================
g_best <- function(v1, v2, type="min") {
  if (type=="min") min(c(v1,v2)) else max(c(v1,v2))
}

m_eval <- function(bits) {
  res  <- EVAL_FUN(bits)
  FES      <<- FES + 1L
  BEST     <<- g_best(BEST, res, TYPE)
  if (FES <= MAXIT) FES_HIST[FES] <<- BEST
  return(res)
}

# =============================================================
# FUNCAO PRINCIPAL — corre NRUNS do rbga.bin
# =============================================================
run_gabin <- function(eval_func, label, seed_base, cor_med="steelblue",
                      pdf_name, rds_name) {

  cat(sprintf("\n=== rbga.bin %s (%d runs) ===\n", label, NRUNS))
  EVAL_FUN <<- eval_func

  lucros    <- numeric(NRUNS)
  hist_runs <- matrix(NA_real_, nrow=NRUNS, ncol=MAXIT)
  BEST_S    <- NULL
  best      <- -Inf

  for (r in 1:NRUNS) {
    set.seed(seed_base + r)
    TYPE     <<- "min"
    FES      <<- 0L
    BEST     <<- Inf
    FES_HIST <<- rep(NA_real_, MAXIT)

    GA <- rbga.bin(
      size           = N_BITS,
      popSize        = POPSIZE,
      iters          = ITERS,
      mutationChance = MUTCH,
      elitism        = ELIT,
      evalFunc       = m_eval,
      verbose        = FALSE
    )

    # Melhor solucao desta run
    best_idx  <- which.min(GA$evaluations)
    bits_best <- GA$population[best_idx, ]
    S_best    <- decode_S(bits_best)
    L         <- profit(S_best)

    # Para O2: verificar restricao
    if (grepl("O2", label)) {
      u <- total_units(S_best)
      if (is.na(u) || u > 10000) L <- NA
    }

    lucros[r]       <- ifelse(is.na(L), NA, L)
    # converter FES_HIST (minimizacao) para lucro
    f_run <- -FES_HIST
    f_run[is.infinite(f_run) | f_run < -1e8] <- NA
    hist_runs[r, ] <- f_run

    cat(sprintf("Run %2d | Lucro: %8.2f | FES: %d\n",
                r, ifelse(is.na(L), -Inf, L), FES))
    if (!is.na(L) && L > best) { best <- L; BEST_S <- S_best }
  }

  med <- median(lucros, na.rm = TRUE)
  cat(sprintf("\n>> Mediana %s : %.2f\n", label, med))
  cat(sprintf(">> Melhor  %s : %.2f\n", label, best))
  if (!is.null(BEST_S))
    cat(sprintf(">> Unidades   : %d\n", total_units(BEST_S)))

  # Grafico: mediana + min-max por FES
  med_curve <- apply(hist_runs, 2, median, na.rm=TRUE)
  vmin      <- apply(hist_runs, 2, min,    na.rm=TRUE)
  vmax      <- apply(hist_runs, 2, max,    na.rm=TRUE)
  finitos   <- c(hist_runs)[is.finite(c(hist_runs))]

  if (length(finitos) > 0) {
    pdf(file.path(GABIN_DIR, pdf_name), width=9, height=6)
    ylim <- range(finitos)
    plot(NA, xlim=c(1,MAXIT), ylim=ylim,
         xlab="Numero de Avaliacoes (FES)", ylab="Lucro ($)",
         main=paste0("rbga.bin ", label, " — ", NRUNS, " runs"))
    grid()
    for (r2 in 1:NRUNS)
      if (any(is.finite(hist_runs[r2,])))
        lines(hist_runs[r2,], col="grey80", lwd=0.7)
    ok <- is.finite(vmin) & is.finite(vmax)
    if (any(ok))
      polygon(c(which(ok), rev(which(ok))),
              c(vmin[ok], rev(vmax[ok])),
              col=adjustcolor(cor_med, 0.15), border=NA)
    lines(med_curve, col=cor_med, lwd=2.5)
    abline(h=med, lty=2, col="grey40")
    legend("bottomright", bty="n", cex=0.85,
           legend=c("Mediana","Banda min-max","Run individual","Mediana final"),
           col=c(cor_med,cor_med,"grey80","grey40"),
           lwd=c(2.5,NA,0.7,1), pch=c(NA,15,NA,NA), lty=c(1,NA,1,2))
    dev.off()
    cat("Grafico guardado:", file.path(GABIN_DIR, pdf_name), "\n")
  }

  saveRDS(med_curve, file.path(GABIN_DIR, rds_name))

  list(lucros=lucros, mediana=med, melhor=best,
       BEST_S=BEST_S, hist_runs=hist_runs, med_curve=med_curve)
}

# =============================================================
# O1 — rbga.bin (minimiza -profit)
# =============================================================
eval_bin_O1 <- function(bits) {
  S <- decode_S(bits)
  -profit(S)
}

res_O1 <- run_gabin(
  eval_func = eval_bin_O1,
  label     = "O1",
  seed_base = 42,
  cor_med   = "steelblue",
  pdf_name  = "convergencia_O1_gabin.pdf",
  rds_name  = "convergencia_O1_gabin.rds"
)

# =============================================================
# O2 — rbga.bin + Death Penalty
# =============================================================
eval_bin_O2 <- function(bits) {
  S <- decode_S(bits)
  u <- total_units(S)
  if (is.na(u) || u > 10000) return(1e9)   # death penalty
  -profit(S)
}

res_O2 <- run_gabin(
  eval_func = eval_bin_O2,
  label     = "O2",
  seed_base = 123,
  cor_med   = "tomato",
  pdf_name  = "convergencia_O2_gabin.pdf",
  rds_name  = "convergencia_O2_gabin.rds"
)

# =============================================================
# EXPORTAR RESULTADOS
# =============================================================
resultado_gabin <- data.frame(
  membro    = "Nuno",
  algoritmo = "GA-bin (rbga.bin)",
  objetivo  = c("O1", "O2"),
  lucro     = c(round(res_O1$melhor, 2), round(res_O2$melhor, 2)),
  mediana   = c(round(res_O1$mediana, 2), round(res_O2$mediana, 2)),
  unidades  = c(
    if (!is.null(res_O1$BEST_S)) total_units(res_O1$BEST_S) else NA,
    if (!is.null(res_O2$BEST_S)) total_units(res_O2$BEST_S) else NA
  ),
  N_bits    = N_BITS,
  N_runs    = NRUNS,
  popSize   = POPSIZE,
  iters     = ITERS,
  stringsAsFactors = FALSE
)
CSV_OUT <- file.path(GABIN_DIR, "resultado_gabin.csv")
write.csv(resultado_gabin, CSV_OUT, row.names = FALSE)

cat("\n=== Resultado Final rbga.bin ===\n")
print(resultado_gabin)

cat("\n=== VALIDACOES FINAIS ===\n")
cat("1. N_BITS =", N_BITS, "(esperado 476):", N_BITS == 476, "\n")
cat("2. Mediana O1:", res_O1$mediana, "vs Melhor:", res_O1$melhor, "\n")
cat("3. Codificacao OK:", all(decode_S(sample(0:1,N_BITS,TRUE))[idx_PR] <= 0.30), "\n")
if (!is.null(res_O2$BEST_S)) {
  u_O2 <- total_units(res_O2$BEST_S)
  cat(sprintf("4. Melhor O2: %d unidades (<= 10000): %s\n",
              u_O2, ifelse(u_O2 <= 10000, "OK", "VIOLADA!")))
}

cat("\n=== FIM rbga_bin_O1_O2.R ===\n")
sink()
