# =============================================================
# OTIM_GA.R
# João — MLPE → Algoritmo Genético (rbga, pacote genalg)
# Objetivos:
#   O1: maximizar o lucro (sem restrições)
#   O2: maximizar o lucro com restrição (total de unidades <= 10000) via Death Penalty
#
# Correções aplicadas em relação à versão anterior:
#   1) 20 runs por objetivo, agrega resultados pela MEDIANA (não pelo máximo).
#   2) Curva de convergência com eixo X = nº de AVALIAÇÕES da função objetivo,
#      uniforme entre algoritmos do grupo. Reporta-se a curva da mediana
#      (e o intervalo min–max) ao longo das avaliações.
#
# Baseado nos scripts do professor:
#   - opt-2-rastrigin-1-rbga.R  (uso de rbga do pacote genalg)
#   - opt-4-convergence-2demos.R (g_best + m_eval, eixo X em avaliações)
# =============================================================

# --- dependências ---------------------------------------------
library(genalg)

# carrega PREV, lojas, hr_cost, lower, upper, profit, eval, eval_O2,
# S1 e total_units do ficheiro partilhado do grupo
source("~/Desktop/Mestrado/TIAPOSE/project/utils/config_otimizacao.R", chdir = TRUE)

# -------------------------------------------------------------
# 1. VALIDAÇÃO INICIAL — confirma que o config carrega bem
# -------------------------------------------------------------
cat("\n=== VALIDAÇÃO INICIAL (JOÃO — GA) ===\n")
cat("profit(S1)   =", profit(S1),   "\n")
cat("eval(S1)     =", eval(S1),     "\n")
cat("eval_O2(S1)  =", eval_O2(S1),  "\n")
cat("unidades(S1) =", total_units(S1), "\n")
cat("D (dim)      =", length(lower), "\n")

# -------------------------------------------------------------
# 2. FUNÇÃO MONITORIZADA + AUXILIAR g_best
# Mesmo padrão do professor (opt-4-convergence-2demos.R):
# variáveis globais FES, BEST, FES_HIST para registar a curva
# de convergência em função do nº de avaliações.
# -------------------------------------------------------------
g_best <- function(val1, val2, type = "min") {
  if (type == "min") return(min(c(val1, val2)))
  else               return(max(c(val1, val2)))
}

# wrapper monitorizado: incrementa FES, atualiza BEST e regista FES_HIST[FES]
# EVAL_FUN é a função objetivo "real" (eval para O1, eval_O2 para O2)
# NOTA: usa-se FES (em vez de EV) e FES_HIST (em vez de F) porque F está
# protegida em R (F = FALSE) — não pode ser sobrescrita com <<-.
m_eval <- function(S) {
  res <- EVAL_FUN(S)
  FES      <<- FES + 1
  BEST     <<- g_best(BEST, res, TYPE)
  if (FES <= MAXIT) FES_HIST[FES] <<- BEST
  return(res)
}

# -------------------------------------------------------------
# 3. PARÂMETROS DO GA
# -------------------------------------------------------------
D       <- length(lower)      # 84
POPSIZE <- 100
ITERS   <- 200
MUTCH   <- 0.10
ELIT    <- ceiling(POPSIZE * 0.20)
NRUNS   <- 20                 # nº de runs (correção pedida)

# nº total de avaliações por run.
# rbga: avalia toda a população na 1ª geração (POPSIZE) e depois
# avalia ITERS-1 gerações de POPSIZE-ELIT novos indivíduos
# (os ELIT melhores transitam sem reavaliação).
# => MAXIT = POPSIZE + (ITERS-1) * (POPSIZE - ELIT)
MAXIT <- POPSIZE + (ITERS - 1) * (POPSIZE - ELIT)

cat("\n=== PARÂMETROS DO GA ===\n")
cat("popSize         =", POPSIZE, "\n")
cat("iter            =", ITERS,   "\n")
cat("mutationChance  =", MUTCH,   "\n")
cat("elitism         =", ELIT,    "\n")
cat("nº runs         =", NRUNS,   "\n")
cat("avaliações/run  =", MAXIT,   "(estimativa)\n")

# -------------------------------------------------------------
# 4. FUNÇÃO AUXILIAR — gera N sugestões válidas para warm-start do O2
# Quando se usa Death Penalty, se a população inicial não tem
# nenhuma solução válida (total_units <= 10000) o GA fica preso a
# devolver Inf em todas as gerações.
#
# Estratégia: constrói cada candidato de FORMA DETERMINÍSTICA
# garantida-viável e só depois adiciona ruído controlado:
#   1) começa com J=X=0 em todas as posições + PR aleatório alto
#      (esta solução tem 0 unidades vendidas → SEMPRE viável)
#   2) tenta incrementar J/X aleatoriamente, mas só aceita o
#      incremento se mantiver total_units <= 10000
# Assim cada candidato é sempre viável por construção.
# -------------------------------------------------------------
gera_sugestoes_validas <- function(N, max_incr = 200) {
  idx_PR <- seq(1, D, by = 3)
  idx_J  <- seq(2, D, by = 3)
  idx_X  <- seq(3, D, by = 3)
  ups_J  <- upper[idx_J]
  ups_X  <- upper[idx_X]
  
  encontradas <- matrix(NA_real_, nrow = N, ncol = D)
  
  for (n in 1:N) {
    cand <- numeric(D)
    # PR aleatório uniforme em [0, 0.30] (dentro dos bounds)
    cand[idx_PR] <- runif(length(idx_PR), 0, 0.30)
    # arranca com J=X=0 → 0 unidades → viável de certeza
    cand[idx_J]  <- 0
    cand[idx_X]  <- 0
    # tenta incrementar J/X pseudo-aleatoriamente, parando quando
    # qualquer incremento adicional violar a restrição
    for (k in 1:max_incr) {
      pos <- sample(28, 1)            # escolhe um (loja, dia)
      var <- sample(c("J", "X"), 1)   # escolhe J ou X
      idx <- if (var == "J") idx_J[pos] else idx_X[pos]
      ub  <- if (var == "J") ups_J[pos] else ups_X[pos]
      if (cand[idx] >= ub) next       # já está no máximo, salta
      cand[idx] <- cand[idx] + 1
      if (total_units(cand) > 10000) {
        cand[idx] <- cand[idx] - 1    # desfaz o incremento
        break                          # cheio: nenhum próximo passo cabe
      }
    }
    cand <- pmin(pmax(cand, lower), upper)
    encontradas[n, ] <- cand
  }
  
  # validação final
  unids <- apply(encontradas, 1, total_units)
  cat(sprintf("    [warm-start] geradas %d sugestões; unidades min=%d, mediana=%d, max=%d\n",
              N, min(unids, na.rm = TRUE),
              as.integer(median(unids, na.rm = TRUE)),
              max(unids, na.rm = TRUE)))
  return(encontradas)
}

# -------------------------------------------------------------
# 5. FUNÇÃO QUE CORRE NRUNS RUNS DE rbga PARA UM DADO OBJETIVO
# Devolve:
#   $profits  : vetor de lucros finais (1 por run)
#   $solucoes : matriz NRUNS x D com a melhor solução de cada run
#   $F_runs   : matriz NRUNS x MAXIT com a curva de convergência
#               (em LUCRO, já com sinal invertido) por run
#   $tempos   : vetor de tempos (segundos) por run
# -------------------------------------------------------------
run_GA <- function(eval_func, label) {
  
  cat("\n>>> A correr GA para ", label, " (", NRUNS, " runs)...\n", sep = "")
  
  # estruturas para guardar resultados de TODAS as runs
  profits   <- numeric(NRUNS)
  solucoes  <- matrix(NA_real_, nrow = NRUNS, ncol = D)
  hist_runs <- matrix(NA_real_, nrow = NRUNS, ncol = MAXIT)  # convergência por avaliações
  tempos    <- numeric(NRUNS)
  
  EVAL_FUN <<- eval_func   # m_eval usa EVAL_FUN como função objetivo "real"
  
  for (r in 1:NRUNS) {
    
    # seed diferente em cada run mas reproduzível em conjunto
    set.seed(1000 + r)
    
    # variáveis globais para monitorizar a convergência (resetadas por run)
    TYPE     <<- "min"
    FES      <<- 0
    BEST     <<- Inf
    FES_HIST <<- rep(NA_real_, MAXIT)
    
    t0 <- proc.time()
    
    # Para O2 (Death Penalty), semear ~20% da população com soluções
    # válidas para evitar que todas as gerações sejam Inf.
    # Para O1, deixa o rbga gerar a população aleatoriamente como sempre.
    sugest <- NULL
    if (identical(eval_func, eval_O2)) {
      sugest <- gera_sugestoes_validas(N = ceiling(POPSIZE * 0.20))
      # se por alguma razão não saíram sugestões, não passar matriz vazia ao rbga
      if (is.null(sugest) || nrow(sugest) == 0) sugest <- NULL
    }
    
    # rbga não aceita suggestions = NULL como argumento explícito em
    # algumas versões — construir a chamada condicionalmente
    args_rbga <- list(
      stringMin      = lower,
      stringMax      = upper,
      popSize        = POPSIZE,
      iters          = ITERS,
      mutationChance = MUTCH,
      elitism        = ELIT,
      evalFunc       = m_eval
    )
    if (!is.null(sugest)) args_rbga$suggestions <- sugest
    
    GA <- do.call(rbga, args_rbga)
    
    tempos[r] <- (proc.time() - t0)["elapsed"]
    
    # melhor solução desta run
    PMIN   <- which.min(GA$evaluations)
    S_best <- GA$population[PMIN, ]
    
    # arredondar J e X (PR fica contínuo) — só para o resultado final
    idx_J <- seq(2, D, by = 3)
    idx_X <- seq(3, D, by = 3)
    S_final <- S_best
    S_final[idx_J] <- round(S_final[idx_J])
    S_final[idx_X] <- round(S_final[idx_X])
    
    # para O2: se a solução final ficar inviável devido ao arredondamento,
    # mantém-se mesmo assim no histórico mas o profit é registado tal qual.
    # (Para apresentação, usamos a solução não arredondada se for melhor.)
    p_round <- profit(S_final)
    p_raw   <- profit(S_best)
    if (identical(eval_func, eval_O2)) {
      u_round <- total_units(S_final)
      u_raw   <- total_units(S_best)
      ok_round <- !is.na(u_round) && u_round <= 10000
      ok_raw   <- !is.na(u_raw)   && u_raw   <= 10000
      if (ok_round && ok_raw) {
        if (p_round >= p_raw) { profits[r] <- p_round; solucoes[r, ] <- S_final }
        else                  { profits[r] <- p_raw;   solucoes[r, ] <- S_best  }
      } else if (ok_round)    { profits[r] <- p_round; solucoes[r, ] <- S_final
      } else if (ok_raw)      { profits[r] <- p_raw;   solucoes[r, ] <- S_best
      } else                  { profits[r] <- NA;      solucoes[r, ] <- S_best }
    } else {
      # O1 não tem restrição
      profits[r] <- p_round
      solucoes[r, ] <- S_final
    }
    
    # converter FES_HIST (que está em "minimização", ou seja -profit)
    # para LUCRO, invertendo o sinal. Mantém Inf como NA na curva.
    f_run <- FES_HIST
    f_run[is.infinite(f_run)] <- NA
    hist_runs[r, ] <- -f_run
    
    cat(sprintf("  run %2d/%d   lucro = %10.2f   tempo = %5.2fs   FES = %d\n",
                r, NRUNS, profits[r], tempos[r], FES))
  }
  
  list(
    profits   = profits,
    solucoes  = solucoes,
    F_runs    = hist_runs,   # mantém o nome F_runs no objeto devolvido
    # para não partir nada do código a jusante
    tempos    = tempos
  )
}

# -------------------------------------------------------------
# 5. EXECUÇÃO — O1 e O2
# -------------------------------------------------------------
res_O1 <- run_GA(eval,    "O1 (sem restrição)")
res_O2 <- run_GA(eval_O2, "O2 (Death Penalty, <= 10000 unidades)")

# -------------------------------------------------------------
# 6. RESUMO COM MEDIANA (e min/max para contextualizar)
# -------------------------------------------------------------
sumarizar <- function(res, label) {
  prof <- res$profits
  ok   <- !is.na(prof)
  cat("\n=== ", label, " — resumo de ", NRUNS, " runs ===\n", sep = "")
  cat(sprintf("runs válidas : %d / %d\n", sum(ok), NRUNS))
  cat(sprintf("mediana lucro: %.2f\n",  median(prof, na.rm = TRUE)))
  cat(sprintf("média lucro  : %.2f\n",  mean(prof,   na.rm = TRUE)))
  cat(sprintf("min lucro    : %.2f\n",  min(prof,    na.rm = TRUE)))
  cat(sprintf("max lucro    : %.2f\n",  max(prof,    na.rm = TRUE)))
  cat(sprintf("sd  lucro    : %.2f\n",  sd(prof,     na.rm = TRUE)))
  cat(sprintf("tempo médio  : %.2fs\n", mean(res$tempos)))
}

sumarizar(res_O1, "O1")
sumarizar(res_O2, "O2")

# -------------------------------------------------------------
# 7. CURVA DE CONVERGÊNCIA — eixo X em AVALIAÇÕES
# Uma linha cinza por run + linha colorida com a MEDIANA por
# nº de avaliações. Esta é a métrica pedida para comparação
# justa entre algoritmos do grupo.
# -------------------------------------------------------------
plot_convergencia <- function(res, label, cor_mediana) {
  
  F_runs <- res$F_runs
  med    <- apply(F_runs, 2, median, na.rm = TRUE)
  vmin   <- apply(F_runs, 2, min,    na.rm = TRUE)
  vmax   <- apply(F_runs, 2, max,    na.rm = TRUE)
  
  # tudo NA / Inf? então não há viáveis para mostrar
  todos_finitos <- c(med, vmin, vmax)[is.finite(c(med, vmin, vmax))]
  if (length(todos_finitos) == 0) {
    plot.new()
    title(main = paste0("Convergência GA (rbga) — ", label,
                        " — ", NRUNS, " runs"))
    text(0.5, 0.5, paste("Nenhuma solução viável encontrada\n",
                         "em ", NRUNS, " runs.\n",
                         "(Death Penalty devolveu Inf em todas as avaliações)",
                         sep = ""),
         cex = 1.1, col = "red")
    return(invisible(NULL))
  }
  ylim <- range(todos_finitos, na.rm = TRUE)
  
  par(mar = c(4.2, 4.4, 2.5, 1))
  plot(NA, xlim = c(1, MAXIT), ylim = ylim,
       xlab = "Avaliações da função objetivo",
       ylab = "Lucro (€)",
       main = paste0("Convergência GA (rbga) — ", label,
                     " — ", NRUNS, " runs"))
  grid()
  
  # 1) linhas finas cinzas: cada run individualmente
  for (r in 1:nrow(F_runs)) {
    if (any(is.finite(F_runs[r, ]))) {
      lines(F_runs[r, ], col = "gray80", lwd = 0.7)
    }
  }
  
  # 2) banda min–max das runs
  x  <- 1:MAXIT
  ok <- is.finite(vmin) & is.finite(vmax)
  if (any(ok)) {
    polygon(c(x[ok], rev(x[ok])),
            c(vmin[ok], rev(vmax[ok])),
            col = adjustcolor(cor_mediana, alpha.f = 0.15),
            border = NA)
  }
  
  # 3) linha grossa: mediana ao longo das avaliações
  lines(med, col = cor_mediana, lwd = 2)
  
  # 4) linha horizontal: lucro mediano final
  med_final <- median(res$profits, na.rm = TRUE)
  if (is.finite(med_final)) {
    abline(h = med_final, lty = 2, col = "gray40")
  }
  
  legend("bottomright",
         legend = c("Mediana de 20 runs", "Min–max das runs",
                    "Run individual", "Lucro mediano final"),
         col    = c(cor_mediana, cor_mediana, "gray80", "gray40"),
         lwd    = c(2, NA, 0.7, 1),
         pch    = c(NA, 15, NA, NA),
         lty    = c(1, NA, 1, 2),
         bty    = "n", cex = 0.8)
}

pdf("OTIM_GA_convergencia.pdf", width = 9, height = 6)
plot_convergencia(res_O1, "O1 (sem restrição)",       "steelblue")
plot_convergencia(res_O2, "O2 (Death Penalty)",       "seagreen")
dev.off()

cat("\nPDF da convergência gravado em OTIM_GA_convergencia.pdf\n")

# -------------------------------------------------------------
# 8. EXTRAIR MELHOR SOLUÇÃO REPRESENTATIVA (run da mediana)
# Para a tabela do grupo apresentamos a solução da run cujo
# lucro está mais próximo da MEDIANA — solução típica, não a
# melhor de todas (que daria uma imagem otimista enviesada).
# -------------------------------------------------------------
solucao_mediana <- function(res) {
  med <- median(res$profits, na.rm = TRUE)
  ix  <- which.min(abs(res$profits - med))
  list(idx = ix, S = res$solucoes[ix, ], lucro = res$profits[ix])
}

mO1 <- solucao_mediana(res_O1)
mO2 <- solucao_mediana(res_O2)

cat(sprintf("\nO1 — run mediana: #%d   lucro = %.2f\n", mO1$idx, mO1$lucro))
cat(sprintf("O2 — run mediana: #%d   lucro = %.2f\n", mO2$idx, mO2$lucro))

# -------------------------------------------------------------
# 9. EXPORTAR RESULTADOS PARA COMPARAÇÃO FINAL DE GRUPO
# -------------------------------------------------------------
nomes_lojas <- names(lojas)
dias <- c("Sab", "Dom", "Seg", "Ter", "Qua", "Qui", "Sex")  # confirmar ordem com o grupo

# 9.1 — RDS com tudo (para o gráfico/tabela conjuntos)
resultado_GA <- list(
  algoritmo       = "GA (rbga)",
  N_runs          = NRUNS,
  popSize         = POPSIZE,
  iter            = ITERS,
  mutationChance  = MUTCH,
  elitism         = ELIT,
  MAXIT           = MAXIT,
  # O1
  O1_profits      = res_O1$profits,
  O1_mediana      = median(res_O1$profits, na.rm = TRUE),
  O1_F_runs       = res_O1$F_runs,
  O1_solucao_med  = mO1$S,
  O1_tempos       = res_O1$tempos,
  # O2
  O2_profits      = res_O2$profits,
  O2_mediana      = median(res_O2$profits, na.rm = TRUE),
  O2_F_runs       = res_O2$F_runs,
  O2_solucao_med  = mO2$S,
  O2_tempos       = res_O2$tempos
)
saveRDS(resultado_GA, "resultado_GA.rds")

# 9.2 — CSV resumo (1 linha por objetivo) para a tabela comparativa final
df_resumo <- data.frame(
  algoritmo = "GA (rbga)",
  objetivo  = c("O1", "O2"),
  N_runs    = NRUNS,
  mediana   = c(median(res_O1$profits, na.rm = TRUE),
                median(res_O2$profits, na.rm = TRUE)),
  media     = c(mean(res_O1$profits,   na.rm = TRUE),
                mean(res_O2$profits,   na.rm = TRUE)),
  min       = c(min(res_O1$profits,    na.rm = TRUE),
                min(res_O2$profits,    na.rm = TRUE)),
  max       = c(max(res_O1$profits,    na.rm = TRUE),
                max(res_O2$profits,    na.rm = TRUE)),
  sd        = c(sd(res_O1$profits,     na.rm = TRUE),
                sd(res_O2$profits,     na.rm = TRUE)),
  tempo_med = c(mean(res_O1$tempos), mean(res_O2$tempos)),
  popSize   = POPSIZE,
  iter      = ITERS
)
write.csv(df_resumo, "OTIM_GA_resumo.csv", row.names = FALSE)

# 9.3 — CSV com TODOS os profits das 20 runs (para boxplots do grupo)
df_runs <- data.frame(
  algoritmo = "GA (rbga)",
  run       = rep(1:NRUNS, 2),
  objetivo  = rep(c("O1", "O2"), each = NRUNS),
  lucro     = c(res_O1$profits, res_O2$profits),
  tempo_s   = c(res_O1$tempos,  res_O2$tempos)
)
write.csv(df_runs, "OTIM_GA_runs.csv", row.names = FALSE)

# 9.4 — CSV com a curva da MEDIANA por avaliação (para o gráfico conjunto)
df_conv <- data.frame(
  avaliacao = 1:MAXIT,
  O1_mediana = apply(res_O1$F_runs, 2, median, na.rm = TRUE),
  O1_min     = apply(res_O1$F_runs, 2, min,    na.rm = TRUE),
  O1_max     = apply(res_O1$F_runs, 2, max,    na.rm = TRUE),
  O2_mediana = apply(res_O2$F_runs, 2, median, na.rm = TRUE),
  O2_min     = apply(res_O2$F_runs, 2, min,    na.rm = TRUE),
  O2_max     = apply(res_O2$F_runs, 2, max,    na.rm = TRUE)
)
write.csv(df_conv, "OTIM_GA_convergencia.csv", row.names = FALSE)

# 9.5 — CSV com a solução da run mediana (uma para O1 e outra para O2)
df_solucao <- data.frame()
for (obj in c("O1", "O2")) {
  S <- if (obj == "O1") mO1$S else mO2$S
  for (s in 1:4) {
    for (d in 1:7) {
      idx <- (s - 1) * 21 + (d - 1) * 3 + 1
      df_solucao <- rbind(df_solucao, data.frame(
        objetivo = obj,
        loja     = nomes_lojas[s],
        dia      = d,
        dia_sem  = dias[d],
        PREV     = PREV[(s - 1) * 7 + d],
        PR       = round(S[idx], 4),
        J        = S[idx + 1],
        X        = S[idx + 2]
      ))
    }
  }
}
write.csv(df_solucao, "OTIM_GA_solucao.csv", row.names = FALSE)

cat("\nFicheiros gravados:\n")
cat(" - resultado_GA.rds            (objeto completo para o grupo)\n")
cat(" - OTIM_GA_resumo.csv          (1 linha por objetivo: mediana é a métrica principal)\n")
cat(" - OTIM_GA_runs.csv            (40 linhas: 20 runs × O1+O2, para boxplots)\n")
cat(" - OTIM_GA_convergencia.csv    (curva da mediana por avaliação)\n")
cat(" - OTIM_GA_solucao.csv         (PR/J/X da run mediana, O1 e O2)\n")
cat(" - OTIM_GA_convergencia.pdf    (gráfico de convergência, eixo X = avaliações)\n")

# =============================================================
# 10. FASE INTEGRADA — 12 SEMANAS DO BACKTESTING (esqueleto)
# =============================================================
# Esta secção fica COMENTADA porque o config_otimizacao.R atual
# tem PREV de UMA semana só. Quando a equipa preparar uma matriz
# PREV_12W (12 linhas × 28 valores) com as previsões do backtesting,
# basta descomentar e correr. A lógica é a mesma do bloco anterior:
# 12 PREVs → 12 medianas (uma por semana, com NRUNS_BT runs cada)
# → mediana final dos 12 lucros medianos.
# =============================================================

# NRUNS_BT <- 20  # também 20 runs por semana, conforme exigido
#
# # PREV_12W: matriz 12 x 28 — cada linha = previsão de uma semana
# # (Pedir à equipa de forecasting / extrair do MLPE_multi.R)
# # PREV_12W <- readRDS("../forecasting/PREV_12W.rds")
#
# profits_semanais <- numeric(12)
# F_semanais_O1    <- vector("list", 12)
#
# for (w in 1:12) {
#
#   # injecta PREV da semana w no ambiente global
#   PREV   <<- PREV_12W[w, ]
#   upper  <<- calc_upper(PREV)   # bounds dependem de PREV
#
#   cat(sprintf("\n--- Semana %d/12 (backtesting) ---\n", w))
#
#   # corre NRUNS_BT runs do GA com a previsão desta semana
#   NRUNS  <<- NRUNS_BT
#   res_w  <- run_GA(eval, sprintf("Semana %d", w))
#
#   profits_semanais[w] <- median(res_w$profits, na.rm = TRUE)
#   F_semanais_O1[[w]]  <- res_w$F_runs
# }
#
# cat("\n=== FASE INTEGRADA — GA ===\n")
# cat("Lucros medianos por semana (12):\n")
# print(round(profits_semanais, 2))
# cat(sprintf("MEDIANA dos 12 lucros medianos = %.2f\n",
#             median(profits_semanais)))
#
# saveRDS(list(profits_semanais = profits_semanais,
#              F_semanais_O1    = F_semanais_O1),
#         "resultado_GA_backtesting.rds")