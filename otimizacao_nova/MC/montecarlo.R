# ============================================================
# montecarlo_nuno.R — Monte Carlo O1, O2, O3
# Nuno | depende de config_otimizacao.R
# ============================================================

source("Files/otimizacao/config_otimizacao.R")  # carrega PREV, profit, eval, eval_O2, bounds

set.seed(42)
N_iter     <- 10000
output_dir <- "Files/otimizacao"
dir.create(output_dir, showWarnings = FALSE)

# --- Redirecionar output ---
sink(file.path(output_dir, "output_montecarlo_nuno.txt"), split = TRUE)

# ============================================================
# O1 — Maximizar lucro (sem restrições)
# ============================================================
profit_history <- c()

best_S_O1      <- NULL
best_profit_O1 <- -Inf

cat("=== Monte Carlo O1 ===\n")
t_O1 <- system.time({
  for (i in 1:N_iter) {
    S_rand <- runif(84, min = lower, max = upper)  # solução aleatória dentro dos bounds
    p      <- profit(S_rand)
    profit_history <<- c(profit_history, p)        # regista histórico
    if (p > best_profit_O1) {
      best_profit_O1 <- p
      best_S_O1      <- S_rand
    }
  }
})

# Arredondar J e X da melhor solução
S_O1 <- best_S_O1
S_O1[seq(2, 84, by = 3)] <- round(S_O1[seq(2, 84, by = 3)])  # J
S_O1[seq(3, 84, by = 3)] <- round(S_O1[seq(3, 84, by = 3)])  # X

lucro_O1   <- profit(S_O1)
units_O1   <- total_units(S_O1)
hr_O1      <- sum(S_O1[seq(2,84,by=3)] + S_O1[seq(3,84,by=3)])
tempo_O1   <- round(t_O1["elapsed"], 2)

cat(sprintf("Lucro O1: %.2f | Unidades: %d | HR: %d | Tempo: %.2fs\n",
            lucro_O1, units_O1, hr_O1, tempo_O1))

# Gráfico convergência O1
pdf(file.path(output_dir, "convergencia_montecarlo_O1_nuno.pdf"), width = 7, height = 5)
plot(cummax(profit_history), type = "l",
     main = "Monte Carlo — Melhor Lucro Acumulado O1 (Nuno)",
     xlab = "Iteração", ylab = "Melhor Lucro ($)", col = "steelblue")
dev.off()

# ============================================================
# O2 — Maximizar lucro com restrição ≤ 10000 unidades
# ============================================================

# Bounds reduzidos para aumentar probabilidade de soluções válidas
upper_O2 <- upper
upper_O2[seq(2, 84, by = 3)] <- ceiling(upper[seq(2, 84, by = 3)] / 6)  # J
upper_O2[seq(3, 84, by = 3)] <- ceiling(upper[seq(3, 84, by = 3)] / 6)  # X

profit_history_O2 <- c()
best_S_O2         <- NULL
best_profit_O2    <- -Inf

cat("\n=== Monte Carlo O2 ===\n")
t_O2 <- system.time({
  for (i in 1:N_iter) {
    S_rand <- runif(84, min = lower, max = upper_O2)
    u      <- total_units(S_rand)
    if (!is.na(u) && u <= 10000) {           # só aceita soluções válidas
      p <- profit(S_rand)
      profit_history_O2 <<- c(profit_history_O2, p)
      if (p > best_profit_O2) {
        best_profit_O2 <- p
        best_S_O2      <- S_rand
      }
    }
  }
})

if (is.null(best_S_O2)) {
  cat("Nenhuma solução válida encontrada para O2.\n")
  lucro_O2 <- NA; units_O2 <- NA; hr_O2 <- NA
} else {
  S_O2 <- best_S_O2
  S_O2[seq(2, 84, by = 3)] <- round(S_O2[seq(2, 84, by = 3)])
  S_O2[seq(3, 84, by = 3)] <- round(S_O2[seq(3, 84, by = 3)])
  
  lucro_O2 <- profit(S_O2)
  units_O2 <- total_units(S_O2)
  hr_O2    <- sum(S_O2[seq(2,84,by=3)] + S_O2[seq(3,84,by=3)])
  tempo_O2 <- round(t_O2["elapsed"], 2)
  
  cat(sprintf("Lucro O2: %.2f | Unidades: %d | HR: %d | Tempo: %.2fs\n",
              lucro_O2, units_O2, hr_O2, tempo_O2))
  
  pdf(file.path(output_dir, "convergencia_montecarlo_O2_nuno.pdf"), width = 7, height = 5)
  plot(cummax(profit_history_O2), type = "l",
       main = "Monte Carlo — Melhor Lucro Acumulado O2 (Nuno)",
       xlab = "Iteração", ylab = "Melhor Lucro ($)", col = "tomato")
  dev.off()
}

# ============================================================
# O3 — Maximizar lucro + minimizar HR (função ponderada)
# ============================================================

# Função auxiliar — total HR
total_HR <- function(S) {
  sum(round(S[seq(2, 84, by = 3)]) + round(S[seq(3, 84, by = 3)]))
}

# Referências para normalização (baseadas em O1)
profit_ref <- lucro_O1
hr_ref     <- hr_O1
w          <- 0.7   # peso do lucro (0.3 para HR)

# Eval O3: death penalty se unidades > 10000, caso contrário função ponderada
eval_O3 <- function(S) {
  u <- total_units(S)
  if (is.na(u) || u > 10000) return(Inf)
  p  <- profit(S)
  hr <- total_HR(S)
  return(-w * (p / profit_ref) + (1 - w) * (hr / hr_ref))  # minimização
}

best_S_O3    <- NULL
best_eval_O3 <- Inf
pareto_p     <- c()   # lucros das soluções válidas (para gráfico)
pareto_hr    <- c()   # HR das soluções válidas

cat("\n=== Monte Carlo O3 ===\n")
t_O3 <- system.time({
  for (i in 1:N_iter) {
    S_rand <- runif(84, min = lower, max = upper_O2)
    val    <- eval_O3(S_rand)
    if (is.finite(val)) {
      pareto_p  <- c(pareto_p,  profit(S_rand))
      pareto_hr <- c(pareto_hr, total_HR(S_rand))
      if (val < best_eval_O3) {
        best_eval_O3 <- val
        best_S_O3    <- S_rand
      }
    }
  }
})

if (is.null(best_S_O3)) {
  cat("Nenhuma solução válida encontrada para O3.\n")
  lucro_O3 <- NA; units_O3 <- NA; hr_O3 <- NA
} else {
  S_O3 <- best_S_O3
  S_O3[seq(2, 84, by = 3)] <- round(S_O3[seq(2, 84, by = 3)])
  S_O3[seq(3, 84, by = 3)] <- round(S_O3[seq(3, 84, by = 3)])
  
  lucro_O3 <- profit(S_O3)
  units_O3 <- total_units(S_O3)
  hr_O3    <- total_HR(S_O3)
  tempo_O3 <- round(t_O3["elapsed"], 2)
  
  cat(sprintf("Melhor O3: Lucro=%.2f | Unidades=%d | HR=%d | Tempo=%.2fs\n",
              lucro_O3, units_O3, hr_O3, tempo_O3))
  
  pdf(file.path(output_dir, "tradeoff_montecarlo_O3_nuno.pdf"), width = 7, height = 5)
  plot(pareto_hr, pareto_p,
       pch = 16, cex = 0.4, col = "seagreen",
       main = "Monte Carlo — Trade-off Lucro vs HR (O3, Nuno)",
       xlab = "Total HR", ylab = "Lucro ($)")
  dev.off()
}

# ============================================================
# CSV — tabela comparativa dos 3 objetivos
# ============================================================
resultados <- data.frame(
  Algoritmo  = "Monte Carlo",
  Membro     = "Nuno",
  Objetivo   = c("O1", "O2", "O3"),
  Lucro      = c(lucro_O1, lucro_O2, lucro_O3),
  Unidades   = c(units_O1, units_O2, units_O3),
  Total_HR   = c(hr_O1,    hr_O2,    hr_O3),
  Iteracoes  = N_iter,
  Tempo_s    = c(tempo_O1, tempo_O2, tempo_O3)
)

write.csv(resultados,
          file.path(output_dir, "resultados_montecarlo_nuno.csv"),
          row.names = FALSE)

cat("\nCSV guardado em:", file.path(output_dir, "resultados_montecarlo_nuno.csv"), "\n")

sink()