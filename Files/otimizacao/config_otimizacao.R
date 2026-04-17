# =============================================================
# config_otimizacao.R
# Ficheiro partilhado por todo o grupo — NÃO EDITAR sem avisar
# =============================================================

# -------------------------------------------------------------
# 1. PARÂMETROS DAS LOJAS
# -------------------------------------------------------------
lojas <- list(
  baltimore    = list(Fj = 1.00, Fx = 1.15, Ws = 700),
  lancaster    = list(Fj = 1.05, Fx = 1.20, Ws = 730),
  philadelphia = list(Fj = 1.10, Fx = 1.15, Ws = 760),
  richmond     = list(Fj = 1.15, Fx = 1.25, Ws = 800)
)

# Custos HR por tipo de dia
hr_cost <- list(
  J = c(weekday = 60, weekend = 70),
  X = c(weekday = 80, weekend = 95)
)

# -------------------------------------------------------------
# 2. PREV — clientes previstos (4 lojas × 7 dias = 28 valores)
# Ordem: Baltimore d1..d7, Lancaster d1..d7, Philadelphia d1..d7, Richmond d1..d7
# Substituir pelos valores reais dos modelos (HW → Baltimore, RF → restantes)
# -------------------------------------------------------------
PREV <- c(
  # Baltimore (HW/ETS — Nuno)
  NA, NA, NA, NA, NA, NA, NA,
  # Lancaster (RF — Eduardo)
  NA, NA, NA, NA, NA, NA, NA,
  # Philadelphia (RF — Eduardo)
  NA, NA, NA, NA, NA, NA, NA,
  # Richmond (RF — Eduardo)
  NA, NA, NA, NA, NA, NA, NA
)

# Dias da semana para a semana a otimizar (TRUE = weekday, FALSE = weekend)
# Ajustar conforme a semana real (última semana dos dados = 2014-10-24 a 2014-10-30)
IS_WEEKDAY <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)

# -------------------------------------------------------------
# 3. BOUNDS
# -------------------------------------------------------------
calc_upper <- function(PREV) {
  upper <- c()
  for (i in 1:4) {
    for (d in 1:7) {
      C <- max(PREV[(i - 1) * 7 + d], 1)  # evitar divisão por 0
      upper <- c(upper,
                 0.30,             # PR_max
                 ceiling(C / 6),   # J_max (cada J assiste 6 clientes)
                 ceiling(C / 7))   # X_max (cada X assiste 7 clientes)
    }
  }
  return(upper)
}

lower <- rep(0, 84)
# upper <- calc_upper(PREV)  # descomentar quando PREV estiver preenchido

# -------------------------------------------------------------
# 4. FUNÇÃO PROFIT
# -------------------------------------------------------------
profit <- function(S) {
  total <- 0
  
  for (s in 1:4) {
    loja  <- lojas[[s]]
    Rs    <- 0
    
    for (d in 1:7) {
      idx <- (s - 1) * 21 + (d - 1) * 3 + 1
      PR  <- S[idx]
      J   <- round(S[idx + 1])   # round() APENAS em J e X
      X   <- round(S[idx + 2])
      
      C   <- PREV[(s - 1) * 7 + d]
      As  <- min(7 * X + 6 * J, C)
      
      # Primeiro usam-se todos os X, depois os J restantes
      n_X <- min(7 * X, As)
      n_J <- As - n_X
      
      soma_P <- 0
      
      if (n_X > 0) {
        U_X <- round(loja$Fx * 10 / log(2 - PR))
        P_X <- round(U_X * (1 - PR) * 1.07)
        soma_P <- soma_P + n_X * P_X
      }
      
      if (n_J > 0) {
        U_J <- round(loja$Fj * 10 / log(2 - PR))
        P_J <- round(U_J * (1 - PR) * 1.07)
        soma_P <- soma_P + n_J * P_J
      }
      
      tipo_dia <- ifelse(IS_WEEKDAY[d], "weekday", "weekend")
      custo_HR <- J * hr_cost$J[tipo_dia] + X * hr_cost$X[tipo_dia]
      
      Rs <- Rs + soma_P - custo_HR
    }
    
    total <- total + Rs - loja$Ws
  }
  
  return(total)
}

# -------------------------------------------------------------
# 5. FUNÇÃO EVAL (negada para minimização)
# -------------------------------------------------------------
eval <- function(S) {
  return(-profit(S))
}

# -------------------------------------------------------------
# 6. SOLUÇÃO DE TESTE
# S1: PR=0.10, J=5, X=3 para todos os dias/lojas
# PR válido (0.10 ≤ 0.30), J e X inteiros positivos
# -------------------------------------------------------------
S1 <- rep(c(0.10, 5, 3), times = 28)

# Para validar: descomentar depois de preencher PREV
# cat("profit(S1) =", profit(S1), "\n")
# cat("eval(S1)   =", eval(S1), "\n")   # deve ser -profit(S1)

# -------------------------------------------------------------
# 7. FUNÇÃO AUXILIAR — total de unidades vendidas (para O2/O3)
# -------------------------------------------------------------
total_units <- function(S) {
  units <- 0
  for (s in 1:4) {
    loja <- lojas[[s]]
    for (d in 1:7) {
      idx <- (s - 1) * 21 + (d - 1) * 3 + 1
      PR  <- S[idx]
      J   <- round(S[idx + 1])
      X   <- round(S[idx + 2])
      C   <- PREV[(s - 1) * 7 + d]
      As  <- min(7 * X + 6 * J, C)
      n_X <- min(7 * X, As)
      n_J <- As - n_X
      if (n_X > 0) units <- units + n_X * round(loja$Fx * 10 / log(2 - PR))
      if (n_J > 0) units <- units + n_J * round(loja$Fj * 10 / log(2 - PR))
    }
  }
  return(units)
}

# -------------------------------------------------------------
# 8. EVAL COM DEATH PENALTY — para O2 (≤ 10.000 unidades)
# -------------------------------------------------------------
eval_O2 <- function(S) {
  if (total_units(S) > 10000) return(Inf)  # death penalty
  return(-profit(S))
}

# -------------------------------------------------------------
# 9. TEMPLATE CURVA DE CONVERGÊNCIA
# -------------------------------------------------------------
# Usar dentro de cada script individual:
#
# profit_history <- c()
# eval_trace <- function(S) {
#   p <- profit(S)
#   profit_history <<- c(profit_history, p)
#   return(-p)
# }
# Depois: plot(profit_history, type="l", xlab="Iteração", ylab="Lucro")