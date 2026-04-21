# =============================================================
# OTIM_GA.R
# João — MLPE → Algoritmo Genético (rbga, pacote genalg)
# Objetivo O1: maximizar o lucro sem restrições
# =============================================================

# --- dependências ---------------------------------------------
library(genalg)

# carrega PREV, lojas, hr_cost, lower, upper, profit, eval, S1, total_units
source("../config_otimizacao.R", chdir = TRUE)

# -------------------------------------------------------------
# 1. VALIDAÇÃO INICIAL — confirma que o config carrega bem
# -------------------------------------------------------------
cat("\n=== VALIDAÇÃO INICIAL (JOÃO — GA) ===\n")
cat("profit(S1) =", profit(S1), "\n")
cat("eval(S1)   =", eval(S1),   "\n")
cat("unidades   =", total_units(S1), "\n")
cat("D (dim)    =", length(lower), "\n")

# -------------------------------------------------------------
# 2. FUNÇÃO DE AVALIAÇÃO MONITORIZADA
# mesmo padrão do professor (opt-4-convergence-2demos.R):
# variáveis globais EV, BEST, F para registar a curva de convergência
# -------------------------------------------------------------
g_best <- function(val1, val2, type = "min") {
  if (type == "min") return(min(c(val1, val2)))
  else return(max(c(val1, val2)))
}

m_eval <- function(S) {
  res <- eval(S)                          # minimização: res = -profit
  EV   <<- EV + 1
  BEST <<- g_best(BEST, res, TYPE)        # melhor valor de eval (o mais baixo)
  if (EV <= MAXIT) F[EV] <<- BEST
  return(res)
}

# -------------------------------------------------------------
# 3. PARÂMETROS DO GA
# -------------------------------------------------------------
D        <- length(lower)    # 84
POPSIZE  <- 100
ITERS    <- 200
MUTCH    <- 0.10
ELIT     <- ceiling(POPSIZE * 0.20)
MAXIT    <- POPSIZE * ITERS  # nº total de avaliações (rbga avalia toda a pop em cada geração)

cat("\n=== PARÂMETROS DO GA ===\n")
cat("popSize         =", POPSIZE, "\n")
cat("iter            =", ITERS, "\n")
cat("mutationChance  =", MUTCH, "\n")
cat("elitism         =", ELIT, "\n")
cat("avaliações máx. =", MAXIT, "\n")

# -------------------------------------------------------------
# 4. EXECUÇÃO DO GA
# -------------------------------------------------------------
set.seed(12345)

# variáveis globais para a curva de convergência
TYPE <- "min"
EV   <- 0
BEST <- Inf
F    <- rep(NA, MAXIT)

cat("\n=== A correr rbga... ===\n")
t0 <- proc.time()

GA <- rbga(
  stringMin      = lower,
  stringMax      = upper,
  popSize        = POPSIZE,
  iters          = ITERS,
  mutationChance = MUTCH,
  elitism        = ELIT,
  evalFunc       = m_eval
)

elapsed <- (proc.time() - t0)["elapsed"]

# -------------------------------------------------------------
# 5. MELHOR SOLUÇÃO
# -------------------------------------------------------------
PMIN     <- which.min(GA$evaluations)
S_best   <- GA$population[PMIN, ]

# arredondar J e X da solução final antes de apresentar (PR fica contínuo)
idx_PR <- seq(1, D, by = 3)
idx_J  <- seq(2, D, by = 3)
idx_X  <- seq(3, D, by = 3)

S_final <- S_best
S_final[idx_J] <- round(S_final[idx_J])
S_final[idx_X] <- round(S_final[idx_X])

lucro_final  <- profit(S_final)
unid_final   <- total_units(S_final)

# total de horas/recursos humanos (J + X em todas as lojas e dias)
total_HR <- sum(S_final[idx_J]) + sum(S_final[idx_X])

cat("\n=== RESULTADO FINAL — GA (João) ===\n")
cat("lucro total   =", lucro_final, "\n")
cat("unidades      =", unid_final, "\n")
cat("total HR      =", total_HR, "\n")
cat("tempo (s)     =", round(elapsed, 2), "\n")

# -------------------------------------------------------------
# 6. SOLUÇÃO POR LOJA (para relatório/apresentação)
# -------------------------------------------------------------
nomes_lojas <- names(lojas)
cat("\n=== SOLUÇÃO POR LOJA (PR, J, X) ===\n")
for (s in 1:4) {
  cat("\n--", nomes_lojas[s], "--\n")
  cat(sprintf("%4s %6s %4s %4s\n", "dia", "PR", "J", "X"))
  for (d in 1:7) {
    idx <- (s - 1) * 21 + (d - 1) * 3 + 1
    cat(sprintf("%4d %6.3f %4d %4d\n",
                d, S_final[idx], S_final[idx + 1], S_final[idx + 2]))
  }
}

# -------------------------------------------------------------
# 7. CURVA DE CONVERGÊNCIA
# F guarda -profit (eval = minimização); o grupo quer ver LUCRO
# então invertemos o sinal para o profit_history
# -------------------------------------------------------------
profit_history <- -F[!is.na(F)]

# PDF da curva de convergência
pdf("OTIM_GA_convergencia.pdf", width = 8, height = 5)
par(mar = c(4.2, 4.2, 2.5, 1))
plot(profit_history,
     type = "l", col = "steelblue", lwd = 2,
     xlab = "Avaliações", ylab = "Lucro (€)",
     main = paste0("Convergência GA (rbga) — popSize=", POPSIZE,
                   ", iter=", ITERS))
grid()
abline(h = max(profit_history), lty = 2, col = "gray40")
dev.off()

cat("\nPDF da convergência gravado em OTIM_GA_convergencia.pdf\n")

# -------------------------------------------------------------
# 8. EXPORTAR RESULTADOS PARA COMPARAÇÃO FINAL DE GRUPO
# -------------------------------------------------------------
resultado_GA <- list(
  algoritmo       = "GA (rbga)",
  lucro           = lucro_final,
  unidades        = unid_final,
  total_HR        = total_HR,
  tempo_s         = as.numeric(elapsed),
  solucao         = S_final,
  profit_history  = profit_history,
  popSize         = POPSIZE,
  iter            = ITERS,
  mutationChance  = MUTCH,
  elitism         = ELIT
)

saveRDS(resultado_GA, "resultado_GA.rds")
cat("Resultados gravados em resultado_GA.rds (para a tabela comparativa)\n")

# -------------------------------------------------------------
# 9. EXPORTAR RESULTADOS PARA CSV
# -------------------------------------------------------------
nomes_lojas <- names(lojas)
dias <- c("Sab", "Dom", "Seg", "Ter", "Qua", "Qui", "Sex")  # ajusta se a ordem for outra

df_solucao <- data.frame(
  loja     = character(0),
  dia      = integer(0),
  dia_sem  = character(0),
  PREV     = integer(0),
  PR       = numeric(0),
  J        = integer(0),
  X        = integer(0)
)

for (s in 1:4) {
  for (d in 1:7) {
    idx <- (s - 1) * 21 + (d - 1) * 3 + 1
    df_solucao <- rbind(df_solucao, data.frame(
      loja    = nomes_lojas[s],
      dia     = d,
      dia_sem = dias[d],
      PREV    = PREV[(s - 1) * 7 + d],
      PR      = round(S_final[idx], 4),
      J       = S_final[idx + 1],
      X       = S_final[idx + 2]
    ))
  }
}

write.csv(df_solucao, "OTIM_GA_solucao.csv", row.names = FALSE)

# resumo numa linha só (para a tabela comparativa do grupo)
df_resumo <- data.frame(
  algoritmo = "GA (rbga)",
  lucro     = lucro_final,
  unidades  = unid_final,
  total_HR  = total_HR,
  tempo_s   = round(as.numeric(elapsed), 2),
  popSize   = POPSIZE,
  iter      = ITERS
)
write.csv(df_resumo, "OTIM_GA_resumo.csv", row.names = FALSE)

# curva de convergência (útil para o gráfico conjunto do grupo)
write.csv(
  data.frame(avaliacao = seq_along(profit_history), lucro = profit_history),
  "OTIM_GA_convergencia.csv", row.names = FALSE
)

cat("\nCSVs gravados:\n")
cat(" - OTIM_GA_solucao.csv       (28 linhas: PR, J, X por loja/dia)\n")
cat(" - OTIM_GA_resumo.csv        (1 linha: para a tabela comparativa)\n")
cat(" - OTIM_GA_convergencia.csv  (curva de convergência)\n")