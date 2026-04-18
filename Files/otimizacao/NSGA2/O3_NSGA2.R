# =============================================================
# otimizacao_eduardo_O3_NSGA2.R
# O3 — Bi-objetivo: maximizar lucro + minimizar total HR
# Restricao: total unidades <= 10.000 (death penalty proporcional)
# =============================================================

library(mco)
source("/Users/edias/TIAPOSE2526/Files/otimizacao/config_otimizacao.R")

sink("/Users/edias/TIAPOSE2526/Files/otimizacao/output_O3_NSGA2.txt", split = TRUE)

idx_JX <- as.vector(outer(0:27, c(2, 3), function(b, o) b * 3 + o))

total_HR <- function(S) {
  sum(round(S[idx_JX]))
}

obj_O3 <- function(S) {
  idx_PR <- seq(1, 84, by = 3)
  if (any(S[idx_PR] >= 1, na.rm = TRUE)) return(c(Inf, Inf))
  u <- total_units(S)
  if (is.na(u) || u > 10000) {
    excesso <- ifelse(is.na(u), 1e6, u - 10000)
    return(c(excesso * 10, 1000))
  }
  o1 <- -profit(S)
  o2 <- total_HR(S)
  return(c(o1, o2))
}

cat("=== O3: NSGA-II (bi-objetivo) ===\n")
cat(">> Obj1: maximizar lucro | Obj2: minimizar total HR\n")
cat(">> Restricao: unidades <= 10.000\n\n")

set.seed(42)

resultado_nsga <- nsga2(
  fn           = obj_O3,
  idim         = 84,
  odim         = 2,
  lower.bounds = lower,
  upper.bounds = upper,
  popsize      = 52,
  generations  = 500,
  cprob        = 0.7,
  cdist        = 5,
  mprob        = 0.1,
  mdist        = 10
)

pareto_vals <- resultado_nsga$value
pareto_pops <- resultado_nsga$par

validas <- which(is.finite(pareto_vals[, 1]) & is.finite(pareto_vals[, 2]) &
                   pareto_vals[, 1] < 1e5)

if (length(validas) == 0) {
  cat(">> O3: nenhuma solucao valida encontrada.\n")
  cat(">> Aumentar generations ou relaxar a restricao.\n")
} else {
  lucros_pareto <- -pareto_vals[validas, 1]
  hr_pareto     <-  pareto_vals[validas, 2]
  
  cat(">> Solucoes na fronteira de Pareto:", length(validas), "\n")
  cat(">> Lucro max:", round(max(lucros_pareto), 2), "\n")
  cat(">> Lucro min:", round(min(lucros_pareto), 2), "\n")
  cat(">> HR min:", min(hr_pareto), "\n")
  cat(">> HR max:", max(hr_pareto), "\n")
  
  pdf("/Users/edias/TIAPOSE2526/Files/otimizacao/pareto_O3_NSGA2.pdf")
  plot(hr_pareto, lucros_pareto,
       xlab = "Total HR (minimizar)",
       ylab = "Lucro (maximizar)",
       main = "Fronteira de Pareto - O3 NSGA-II",
       pch = 19, col = "darkgreen")
  dev.off()
  cat("Grafico Pareto guardado.\n")
  
  lucros_norm <- (lucros_pareto - min(lucros_pareto)) /
    (max(lucros_pareto) - min(lucros_pareto) + 1e-9)
  hr_norm     <- (hr_pareto - min(hr_pareto)) /
    (max(hr_pareto) - min(hr_pareto) + 1e-9)
  
  # peso duplo no lucro para privilegiar maximização de lucro
  score    <- lucros_norm * 2 - hr_norm
  idx_best <- validas[which.max(score)]
  
  S_best_O3 <- pareto_pops[idx_best, ]
  S_best_O3 <- pmin(pmax(S_best_O3, lower), upper)
  S_best_O3[idx_JX] <- round(S_best_O3[idx_JX])
  
  cat("\n>> Solucao de compromisso selecionada:\n")
  cat("   Lucro   :", round(profit(S_best_O3), 2), "\n")
  cat("   HR      :", total_HR(S_best_O3), "\n")
  cat("   Unidades:", total_units(S_best_O3), "\n")
  
  resultado_O3 <- data.frame(
    membro    = "Eduardo",
    algoritmo = "NSGA2",
    objetivo  = "O3",
    lucro     = round(profit(S_best_O3), 2),
    unidades  = total_units(S_best_O3),
    total_HR  = total_HR(S_best_O3)
  )
  rownames(resultado_O3) <- NULL
  write.csv(resultado_O3,
            "/Users/edias/TIAPOSE2526/Files/otimizacao/resultado_Eduardo_NSGA2.csv",
            row.names = FALSE)
  
  pareto_df <- data.frame(
    lucro    = lucros_pareto,
    total_HR = hr_pareto
  )
  write.csv(pareto_df,
            "/Users/edias/TIAPOSE2526/Files/otimizacao/pareto_O3_fronteira.csv",
            row.names = FALSE)
  
  cat("\nFicheiros exportados.\n")
  cat("\n=== Resultado O3 ===\n")
  print(resultado_O3)
  
  nomes_lojas <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
  nomes_dias  <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")
  
  cat("\n=== PLANO SEMANAL RECOMENDADO (O3 - compromisso) ===\n")
  for (s in 1:4) {
    cat("\n--", nomes_lojas[s], "--\n")
    cat(sprintf("%-5s | %5s | %5s | %5s\n", "Dia", "PR", "J", "X"))
    for (d in 1:7) {
      idx <- (s - 1) * 21 + (d - 1) * 3 + 1
      cat(sprintf("%-5s | %5.2f | %5d | %5d\n",
                  nomes_dias[d],
                  S_best_O3[idx],
                  round(S_best_O3[idx + 1]),
                  round(S_best_O3[idx + 2])))
    }
  }
}

sink()