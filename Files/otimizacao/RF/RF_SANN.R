# =============================================================
# otimizacao_eduardo_RF_SANN.R
# =============================================================

source("/Users/edias/TIAPOSE2526/Files/otimizacao/config_otimizacao.R")

# PREV, lower, upper, profit, eval, eval_O2, total_units, S1
# já estão todos carregados pelo config

# -------------------------------------------------------------
# ÍNDICES DE J E X (para arredondar a solução final)
# -------------------------------------------------------------
idx_JX <- as.vector(outer(0:27, c(2, 3), function(b, o) b * 3 + o))

# -------------------------------------------------------------
# O1 — Simulated Annealing — maximizar lucro
# -------------------------------------------------------------
cat("=== O1: Simulated Annealing ===\n")

profit_history_O1 <- c()

eval_trace_O1 <- function(S) {
  p <- profit(S)
  profit_history_O1 <<- c(profit_history_O1, p)
  return(-p)
}

set.seed(42)
Runs  <- 5
best_O1  <- -Inf
BEST_S_O1 <- NULL

for (i in 1:Runs) {
  s0 <- runif(84) * (upper - lower) + lower
  sa <- optim(
    par     = s0,
    fn      = eval_trace_O1,
    method  = "SANN",
    control = list(maxit = 10000, temp = 2000, trace = FALSE)
  )
  S_tmp <- sa$par
  S_tmp[idx_JX] <- round(S_tmp[idx_JX])
  L <- profit(S_tmp)
  cat("Run", i, "| Lucro:", round(L, 2), "\n")
  if (L > best_O1) { best_O1 <- L; BEST_S_O1 <- S_tmp }
}

cat("\n>> Melhor lucro O1:", round(best_O1, 2), "\n")
cat(">> Unidades:", total_units(BEST_S_O1), "\n")

pdf("/Users/edias/TIAPOSE2526/Files/otimizacao/convergencia_O1_SANN.pdf")
plot(profit_history_O1, type = "l", col = "blue", lwd = 2,
     xlab = "Avaliações", ylab = "Lucro", main = "Convergência SANN — O1")
dev.off()

# -------------------------------------------------------------
# O2 — death penalty (≤ 10.000 unidades)
# -------------------------------------------------------------
cat("\n=== O2: Simulated Annealing + death penalty ===\n")

profit_history_O2 <- c()

eval_trace_O2 <- function(S) {
  if (total_units(S) > 10000) {
    profit_history_O2 <<- c(profit_history_O2, NA)
    return(Inf)
  }
  p <- profit(S)
  profit_history_O2 <<- c(profit_history_O2, p)
  return(-p)
}

best_O2   <- -Inf
BEST_S_O2 <- NULL

for (i in 1:Runs) {
  s0 <- runif(84) * (upper - lower) + lower
  sa <- optim(
    par     = s0,
    fn      = eval_trace_O2,
    method  = "SANN",
    control = list(maxit = 10000, temp = 2000, trace = FALSE)
  )
  S_tmp <- sa$par
  S_tmp[idx_JX] <- round(S_tmp[idx_JX])
  if (total_units(S_tmp) <= 10000) {
    L <- profit(S_tmp)
    cat("Run", i, "| Lucro:", round(L, 2), "| Unidades:", total_units(S_tmp), "\n")
    if (L > best_O2) { best_O2 <- L; BEST_S_O2 <- S_tmp }
  } else {
    cat("Run", i, "| inválida (>10000 unidades)\n")
  }
}

cat("\n>> Melhor lucro O2:", round(best_O2, 2), "\n")
cat(">> Unidades:", total_units(BEST_S_O2), "\n")

pdf("/Users/edias/TIAPOSE2526/Files/otimizacao/convergencia_O2_SANN.pdf")
plot(profit_history_O2, type = "l", col = "red", lwd = 2,
     xlab = "Avaliações", ylab = "Lucro", main = "Convergência SANN — O2",
     na.action = na.omit)
dev.off()

# -------------------------------------------------------------
# EXPORTAR RESULTADOS
# -------------------------------------------------------------
resultado <- data.frame(
  membro    = "Eduardo",
  algoritmo = "SANN",
  objetivo  = c("O1", "O2"),
  lucro     = c(round(best_O1, 2), round(best_O2, 2)),
  unidades  = c(total_units(BEST_S_O1), total_units(BEST_S_O2)),
  total_HR  = c(sum(BEST_S_O1[idx_JX]), sum(BEST_S_O2[idx_JX]))
)
write.csv(resultado,
          "/Users/edias/TIAPOSE2526/Files/otimizacao/resultado_Eduardo_SANN.csv",
          row.names = FALSE)

saveRDS(profit_history_O1,
        "/Users/edias/TIAPOSE2526/Files/otimizacao/convergencia_O1_SANN.rds")
saveRDS(profit_history_O2,
        "/Users/edias/TIAPOSE2526/Files/otimizacao/convergencia_O2_SANN.rds")

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