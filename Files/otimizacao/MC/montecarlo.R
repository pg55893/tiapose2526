# ============================================================
# montecarlo_nuno.R — Monte Carlo O1 e O2
# Nuno | depende de config_otimizacao.R
# ============================================================

source("Files/otimizacao/config_otimizacao.R")  # carrega PREV, profit, eval, eval_O2, bounds

set.seed(42)
N_iter <- 10000

# --- O1 ---
profit_history <- c()  # histórico para gráfico

eval_trace <- function(S) {       # eval com registo de histórico
  p <- profit(S)
  profit_history <<- c(profit_history, p)
  return(-p)
}

best_S_O1      <- NULL
best_profit_O1 <- -Inf

cat("=== Monte Carlo O1 ===\n")
for (i in 1:N_iter) {
  S_rand <- runif(84, min = lower, max = upper)
  p      <- profit(S_rand)
  if (p > best_profit_O1) {
    best_profit_O1 <- p
    best_S_O1      <- S_rand
  }
  profit_history <<- c(profit_history, p)
}

# Arredondar J e X
S_O1 <- best_S_O1
S_O1[seq(2, 84, by = 3)] <- round(S_O1[seq(2, 84, by = 3)])
S_O1[seq(3, 84, by = 3)] <- round(S_O1[seq(3, 84, by = 3)])

cat(sprintf("Lucro O1: %.2f | Unidades: %d | HR total: %d\n",
            profit(S_O1),
            total_units(S_O1),
            sum(round(S_O1[seq(2,84,by=3)]) + round(S_O1[seq(3,84,by=3)]))))

plot(cummax(profit_history), type = "l",
     main = "Monte Carlo — Melhor Lucro Acumulado O1 (Nuno)",
     xlab = "Iteração", ylab = "Melhor Lucro ($)", col = "steelblue")

# --- O2 — bounds reduzidos para respeitar ≤ 10000 unidades ---
upper_O2 <- upper
upper_O2[seq(2, 84, by = 3)] <- ceiling(upper[seq(2, 84, by = 3)] / 6)  # J
upper_O2[seq(3, 84, by = 3)] <- ceiling(upper[seq(3, 84, by = 3)] / 6)  # X

profit_history_O2 <- c()
best_S_O2         <- NULL
best_profit_O2    <- -Inf

cat("\n=== Monte Carlo O2 ===\n")
for (i in 1:N_iter) {
  S_rand <- runif(84, min = lower, max = upper_O2)
  u      <- total_units(S_rand)
  if (!is.na(u) && u <= 10000) {
    p <- profit(S_rand)
    profit_history_O2 <<- c(profit_history_O2, p)
    if (p > best_profit_O2) {
      best_profit_O2 <- p
      best_S_O2      <- S_rand
    }
  }
}

if (is.null(best_S_O2)) {
  cat("Nenhuma solução válida encontrada.\n")
} else {
  S_O2 <- best_S_O2
  S_O2[seq(2, 84, by = 3)] <- round(S_O2[seq(2, 84, by = 3)])
  S_O2[seq(3, 84, by = 3)] <- round(S_O2[seq(3, 84, by = 3)])
  
  cat(sprintf("Lucro O2: %.2f | Unidades: %d | HR total: %d\n",
              profit(S_O2),
              total_units(S_O2),
              sum(round(S_O2[seq(2,84,by=3)]) + round(S_O2[seq(3,84,by=3)]))))
  
  plot(cummax(profit_history_O2), type = "l",
       main = "Monte Carlo — Melhor Lucro Acumulado O2 (Nuno)",
       xlab = "Iteração", ylab = "Melhor Lucro ($)", col = "tomato")
}