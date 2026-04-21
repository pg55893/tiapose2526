# ============================================================
# Otimização — profit.R
# Nuno | Monte Carlo | O1, O2, O3
# PREV: Baltimore (ETS C1); restantes a preencher pelo grupo
# ============================================================

# --- PREV: clientes previstos (4 lojas × 7 dias) ---
prev_baltimore    <- c(114, 114, 108, 110, 85, 97, 164)
prev_lancaster    <- c(NA, NA, NA, NA, NA, NA, NA)  # Eduardo preenche
prev_philadelphia <- c(NA, NA, NA, NA, NA, NA, NA)  # Eduardo preenche
prev_richmond     <- c(NA, NA, NA, NA, NA, NA, NA)  # Eduardo preenche

#PREV <- c(prev_baltimore, prev_lancaster, prev_philadelphia, prev_richmond)
PREV <- rep(c(114, 114, 108, 110, 85, 97, 164), 4)  # temporário para validação
# --- Parâmetros por loja (índices 1=Baltimore, 2=Lancaster, 3=Philadelphia, 4=Richmond) ---
FJ <- c(1.00, 1.05, 1.10, 1.15)   # fator Junior por loja
FX <- c(1.15, 1.20, 1.15, 1.25)   # fator eXpert por loja
WS <- c(700,  730,  760,  800)     # custo fixo semanal por loja

# --- Dias da semana dos próximos 7 dias (a partir de 2015-01-01) ---
# 1=Qui, 2=Sex, 3=Sáb(WE), 4=Dom(WE), 5=Seg, 6=Ter, 7=Qua
is_weekend <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)

# Custo diário Junior e eXpert (weekday/weekend)
hr_cost_J <- ifelse(is_weekend, 70, 60)  # vetor de 7 valores
hr_cost_X <- ifelse(is_weekend, 95, 80)  # vetor de 7 valores

# --- Função profit(S) ---
# S: vetor de 84 parâmetros
# Ordem: por loja (4 blocos de 21), dentro de cada loja: PR_d1, J_d1, X_d1, ..., PR_d7, J_d7, X_d7
profit <- function(S) {
  
  lucro_total   <- 0   # lucro total acumulado das 4 lojas
  total_units   <- 0   # total de unidades vendidas (para O2)
  total_HR      <- 0   # total de recursos humanos (para O3)
  
  for (s in 1:4) {  # iterar sobre as 4 lojas
    
    base <- (s - 1) * 21  # índice base do bloco desta loja em S
    fj   <- FJ[s]         # fator Junior desta loja
    fx   <- FX[s]         # fator eXpert desta loja
    ws   <- WS[s]         # custo fixo semanal desta loja
    
    lucro_semanal <- 0  # acumula lucro diário desta loja
    
    for (d in 1:7) {  # iterar sobre os 7 dias
      
      idx <- base + (d - 1) * 3  # posição do trio (PR, J, X) deste dia
      
      PR <- S[idx + 1]           # taxa de promoção (real, 0–0.30)
      J  <- round(S[idx + 2])    # Juniors (inteiro)
      X  <- round(S[idx + 3])    # eXperts (inteiro)
      
      C  <- PREV[(s - 1) * 7 + d]  # clientes previstos para esta loja e dia
      
      # Clientes assistidos: X são usados primeiro, depois J
      A <- min(7 * X + 6 * J, C)
      
      # Clientes assistidos por X (até ao máximo que X consegue)
      A_X <- min(7 * X, A)
      # Clientes assistidos por J (o restante)
      A_J <- A - A_X
      
      # Unidades vendidas e lucro por cliente X
      U_X <- round(fx * 10 / log(2 - PR))   # unidades por cliente X
      P_X <- round(U_X * (1 - PR) * 1.07)   # lucro por cliente X
      
      # Unidades vendidas e lucro por cliente J
      U_J <- round(fj * 10 / log(2 - PR))   # unidades por cliente J
      P_J <- round(U_J * (1 - PR) * 1.07)   # lucro por cliente J
      
      # Receita diária = lucro de todos os clientes - custos HR
      R_d <- (A_X * P_X + A_J * P_J) - (J * hr_cost_J[d]) - (X * hr_cost_X[d])
      
      lucro_semanal <- lucro_semanal + R_d  # acumula lucro diário
      total_units   <- total_units + (A_X * U_X + A_J * U_J)  # acumula unidades
      total_HR      <- total_HR + J + X                         # acumula HR
    }
    
    # Lucro semanal desta loja após custo fixo
    lucro_total <- lucro_total + lucro_semanal - ws
  }
  
  return(list(profit = lucro_total, units = total_units, HR = total_HR))
}

# --- Função eval(S) para O1 (minimização = -profit) ---
eval_O1 <- function(S) {
  res <- profit(S)
  profit_history <<- c(profit_history, res$profit)  # regista histórico
  return(-res$profit)  # negado porque Monte Carlo vai minimizar
}

# --- Função eval(S) para O2 (death penalty se unidades > 10000) ---
eval_O2 <- function(S) {
  res <- profit(S)
  if (res$units > 10000) return(Inf)  # solução inválida
  profit_history <<- c(profit_history, res$profit)
  return(-res$profit)
}

# --- Bounds ---
calc_upper <- function(PREV) {
  upper <- numeric(84)
  for (s in 1:4) {
    for (d in 1:7) {
      C   <- PREV[(s - 1) * 7 + d]   # clientes previstos
      idx <- (s - 1) * 21 + (d - 1) * 3
      upper[idx + 1] <- 0.30          # PR máximo = 30%
      upper[idx + 2] <- ceiling(C / 6) # J máximo
      upper[idx + 3] <- ceiling(C / 7) # X máximo
    }
  }
  return(upper)
}

lower <- rep(0, 84)
upper <- calc_upper(PREV)

# --- Validação com S1 ---
S1 <- rep(c(0.10, 5, 3), 28)  # PR=0.10, J=5, X=3 para todos os dias/lojas
cat("=== Validação com S1 ===\n")
res_S1 <- profit(S1)
cat(sprintf("Lucro: %.2f | Unidades: %d | HR: %d\n", res_S1$profit, res_S1$units, res_S1$HR))

# ============================================================
# Monte Carlo — O1 (maximizar lucro)
# ============================================================

set.seed(42)                    # reprodutibilidade
N_iter        <- 10000          # número de soluções aleatórias a testar
profit_history <<- numeric(0)   # histórico de lucros (global para eval_O1)

best_S      <- NULL   # melhor solução encontrada
best_profit <- -Inf   # melhor lucro encontrado

cat("\n=== Monte Carlo O1 ===\n")

for (i in 1:N_iter) {
  
  # Gerar solução aleatória dentro dos bounds
  S_rand <- runif(84, min = lower, max = upper)
  
  # Avaliar (eval_O1 nega o lucro e regista histórico)
  val <- eval_O1(S_rand)
  
  # Guardar se for melhor
  if (-val > best_profit) {
    best_profit <- -val
    best_S      <- S_rand
  }
}

# --- Arredondar J e X da melhor solução ---
best_S_rounded <- best_S
for (s in 1:4) {
  for (d in 1:7) {
    idx <- (s - 1) * 21 + (d - 1) * 3
    best_S_rounded[idx + 2] <- round(best_S[idx + 2])  # J
    best_S_rounded[idx + 3] <- round(best_S[idx + 3])  # X
  }
}

# --- Resultado final ---
res_final <- profit(best_S_rounded)
cat(sprintf("Melhor lucro O1: %.2f | Unidades: %d | HR: %d\n",
            res_final$profit, res_final$units, res_final$HR))

# Gráfico de convergência — melhor acumulado
plot(cummax(profit_history),
     type = "l",
     main = "Monte Carlo — Melhor Lucro Acumulado O1 (Nuno)",
     xlab = "Iteração",
     ylab = "Melhor Lucro ($)",
     col  = "steelblue")

# ============================================================
# Monte Carlo — O2 (restrição ≤ 10000 unidades)
# ============================================================

# Bounds O2: reduzir J e X a ~1/3 (mais soluções válidas que /5)
upper_O2 <- upper
upper_O2[seq(2, 84, by = 3)] <- ceiling(upper[seq(2, 84, by = 3)] / 3)  # J
upper_O2[seq(3, 84, by = 3)] <- ceiling(upper[seq(3, 84, by = 3)] / 3)  # X

set.seed(42)

best_S_O2      <- NULL
best_profit_O2 <- -Inf
best_per_iter  <- numeric(N_iter)  # melhor válido acumulado por iteração

cat("\n=== Monte Carlo O2 ===\n")

for (i in 1:N_iter) {
  
  S_rand <- runif(84, min = lower, max = upper_O2)
  res    <- profit(S_rand)
  
  # Só aceita se respeitar a restrição
  if (res$units <= 10000 && res$profit > best_profit_O2) {
    best_profit_O2 <- res$profit
    best_S_O2      <- S_rand
  }
  
  # Regista o melhor acumulado até esta iteração (para o gráfico)
  best_per_iter[i] <- best_profit_O2
}

if (is.null(best_S_O2)) {
  cat("Nenhuma solução válida encontrada para O2.\n")
} else {
  best_S_O2_rounded <- best_S_O2
  for (s in 1:4) {
    for (d in 1:7) {
      idx <- (s - 1) * 21 + (d - 1) * 3
      best_S_O2_rounded[idx + 2] <- round(best_S_O2[idx + 2])
      best_S_O2_rounded[idx + 3] <- round(best_S_O2[idx + 3])
    }
  }
  
  res_O2 <- profit(best_S_O2_rounded)
  cat(sprintf("Melhor lucro O2: %.2f | Unidades: %d | HR: %d\n",
              res_O2$profit, res_O2$units, res_O2$HR))
  
  # Gráfico — ignora iterações iniciais com -Inf
  valid_start <- which(best_per_iter > -Inf)[1]
  plot(valid_start:N_iter, best_per_iter[valid_start:N_iter],
       type = "l",
       main = "Monte Carlo — Melhor Lucro Acumulado O2 (Nuno)",
       xlab = "Iteração",
       ylab = "Melhor Lucro ($)",
       col  = "tomato")
}