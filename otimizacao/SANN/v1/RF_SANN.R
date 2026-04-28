# =============================================================
# otimizacao_eduardo_RF_SANN.R
# =============================================================

source("/Users/edias/TIAPOSE2526/Files/otimizacao/config_otimizacao.R")

idx_JX <- as.vector(outer(0:27, c(2, 3), function(b, o) b * 3 + o))

# -------------------------------------------------------------
# O1 — Simulated Annealing — maximizar lucro
# -------------------------------------------------------------
cat("=== O1: Simulated Annealing ===\n")

set.seed(42)
Runs           <- 5
best_O1        <- -Inf
BEST_S_O1      <- NULL
profit_history_O1 <- c()   # historico do melhor run (nao concatenado)

for (i in 1:Runs) {
  hist_run <- c()   # historico deste run — reinicia em cada run

  eval_trace_O1 <- function(S) {
    S <- pmin(pmax(S, lower), upper)
    p <- profit(S)
    hist_run <<- c(hist_run, p)
    return(-p)
  }

  s0 <- runif(84) * (upper - lower) + lower
  sa <- optim(
    par     = s0,
    fn      = eval_trace_O1,
    method  = "SANN",
    control = list(maxit = 10000, temp = 2000, trace = FALSE)
  )
  S_tmp <- pmin(pmax(sa$par, lower), upper)
  S_tmp[idx_JX] <- round(S_tmp[idx_JX])
  L <- profit(S_tmp)
  cat("Run", i, "| Lucro:", round(L, 2), "\n")
  if (L > best_O1) {
    best_O1           <- L
    BEST_S_O1         <- S_tmp
    profit_history_O1 <- hist_run   # guarda historico do melhor run
  }
}

BEST_S_O1 <- pmin(pmax(BEST_S_O1, lower), upper)
BEST_S_O1[idx_JX] <- round(BEST_S_O1[idx_JX])

cat("\n>> Melhor lucro O1:", round(best_O1, 2), "\n")
cat(">> Unidades:", total_units(BEST_S_O1), "\n")

if (length(profit_history_O1) > 0) {
  pdf("/Users/edias/TIAPOSE2526/Files/otimizacao/RF/convergencia_O1_SANN.pdf")
  plot(profit_history_O1, type = "l", col = "blue", lwd = 2,
       xlab = "Avaliacoes", ylab = "Lucro", main = "Convergencia SANN - O1")
  dev.off()
  cat("Grafico O1 guardado.\n")
}

# -------------------------------------------------------------
# O2 — death penalty (≤ 10.000 unidades TOTAL 4 lojas)
# -------------------------------------------------------------
cat("\n=== O2: Simulated Annealing + death penalty ===\n")
cat(">> Restricao: total unidades vendidas (4 lojas x 7 dias) <= 10.000\n\n")

best_O2            <- -Inf
BEST_S_O2          <- NULL
profit_history_O2  <- c()   # historico do melhor run (nao concatenado)

for (i in 1:Runs) {
  hist_run_O2 <- c()   # historico deste run — reinicia em cada run

  eval_trace_O2 <- function(S) {
    S <- pmin(pmax(S, lower), upper)
    idx_PR <- seq(1, 84, by = 3)
    if (any(S[idx_PR] >= 1, na.rm = TRUE)) return(Inf)
    u <- total_units(S)
    if (is.na(u) || u > 10000) return(Inf)
    p <- profit(S)
    hist_run_O2 <<- c(hist_run_O2, p)
    return(-p)
  }

  s0 <- lower
  s0[seq(1, 84, by = 3)] <- 0.10
  s0[seq(2, 84, by = 3)] <- 1
  s0[seq(3, 84, by = 3)] <- 1
  s0 <- s0 + runif(84) * 0.5
  s0 <- pmin(pmax(s0, lower), upper)

  sa <- optim(
    par     = s0,
    fn      = eval_trace_O2,
    method  = "SANN",
    control = list(maxit = 20000, temp = 500, trace = FALSE)
  )
  S_tmp <- pmin(pmax(sa$par, lower), upper)
  S_tmp[idx_JX] <- round(S_tmp[idx_JX])
  u <- total_units(S_tmp)
  if (!is.na(u) && u <= 10000) {
    L <- profit(S_tmp)
    cat("Run", i, "| Lucro:", round(L, 2), "| Unidades:", u, "\n")
    if (L > best_O2) {
      best_O2           <- L
      BEST_S_O2         <- S_tmp
      profit_history_O2 <- hist_run_O2   # guarda historico do melhor run
    }
  } else {
    cat("Run", i, "| invalida | unidades:", ifelse(is.na(u), "NA", u), "\n")
  }
}

if (!is.null(BEST_S_O2)) {
  BEST_S_O2 <- pmin(pmax(BEST_S_O2, lower), upper)
  BEST_S_O2[idx_JX] <- round(BEST_S_O2[idx_JX])
  cat("\n>> Melhor lucro O2:", round(best_O2, 2), "\n")
  cat(">> Unidades:", total_units(BEST_S_O2), "\n")
  if (length(profit_history_O2) > 0) {
    pdf("/Users/edias/TIAPOSE2526/Files/otimizacao/RF/convergencia_O2_SANN.pdf")
    plot(profit_history_O2, type = "l", col = "red", lwd = 2,
         xlab = "Avaliacoes validas", ylab = "Lucro", main = "Convergencia SANN - O2")
    dev.off()
    cat("Grafico O2 guardado.\n")
  }
} else {
  cat("\n>> O2: nenhuma solucao valida encontrada (<= 10.000 unidades).\n")
  cat(">> Documentar no relatorio como restricao infeasible com estes PREV.\n")
}

# -------------------------------------------------------------
# EXPORTAR RESULTADOS
# -------------------------------------------------------------
resultado <- data.frame(
  membro    = "Eduardo",
  algoritmo = "SANN",
  objetivo  = c("O1", "O2"),
  lucro     = c(round(best_O1, 2),
                ifelse(is.null(BEST_S_O2), NA, round(best_O2, 2))),
  unidades  = c(total_units(BEST_S_O1),
                ifelse(is.null(BEST_S_O2), NA, total_units(BEST_S_O2))),
  total_HR  = c(sum(BEST_S_O1[idx_JX]),
                ifelse(is.null(BEST_S_O2), NA, sum(BEST_S_O2[idx_JX])))
)
rownames(resultado) <- NULL
write.csv(resultado,
          "/Users/edias/TIAPOSE2526/Files/otimizacao/RF/resultado_SANN.csv",
          row.names = FALSE)
saveRDS(profit_history_O1,
        "/Users/edias/TIAPOSE2526/Files/otimizacao/RF/convergencia_O1_SANN.rds")
saveRDS(profit_history_O2,
        "/Users/edias/TIAPOSE2526/Files/otimizacao/RF/convergencia_O2_SANN.rds")

cat("\n=== Resultados ===\n")
print(resultado)

# -------------------------------------------------------------
# PLANO FINAL RECOMENDADO (O1)
# -------------------------------------------------------------
nomes_lojas <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
nomes_dias  <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")

cat("\n=== PLANO SEMANAL RECOMENDADO (O1) ===\n")
for (s in 1:4) {
  cat("\n--", nomes_lojas[s], "--\n")
  cat(sprintf("%-5s | %5s | %5s | %5s\n", "Dia", "PR", "J", "X"))
  for (d in 1:7) {
    idx <- (s - 1) * 21 + (d - 1) * 3 + 1
    cat(sprintf("%-5s | %5.2f | %5d | %5d\n",
                nomes_dias[d],
                BEST_S_O1[idx],
                round(BEST_S_O1[idx + 1]),
                round(BEST_S_O1[idx + 2])))
  }
}

# Plano O2 (se existir)
if (!is.null(BEST_S_O2)) {
  cat("\n=== PLANO SEMANAL RECOMENDADO (O2) ===\n")
  for (s in 1:4) {
    cat("\n--", nomes_lojas[s], "--\n")
    cat(sprintf("%-5s | %5s | %5s | %5s\n", "Dia", "PR", "J", "X"))
    for (d in 1:7) {
      idx <- (s - 1) * 21 + (d - 1) * 3 + 1
      cat(sprintf("%-5s | %5.2f | %5d | %5d\n",
                  nomes_dias[d],
                  BEST_S_O2[idx],
                  round(BEST_S_O2[idx + 1]),
                  round(BEST_S_O2[idx + 2])))
    }
  }
}