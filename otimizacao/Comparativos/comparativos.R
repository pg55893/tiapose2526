# =============================================================
# comparativos.R  (v2 — todos os algoritmos)
# GA (Joao) | MC (Nuno) | NSGA-II (Eduardo) | PSO (Carolina) | SANN (Eduardo)
# Pacotes: base R apenas
# =============================================================

BASE_DIR <- "/Users/edias/TIAPOSE2526/Files/otimizacao"

# =============================================================
# HELPERS
# =============================================================

normalise_resultado <- function(df, membro_default = NA,
                                 algoritmo_default = NA,
                                 objetivo_default  = NA) {
  n           <- nrow(df)
  names_lower <- tolower(names(df))

  get_col <- function(candidates) {
    idx <- which(names_lower %in% tolower(candidates))
    if (length(idx) > 0) df[[idx[1]]] else rep(NA, n)
  }

  result <- data.frame(
    membro    = as.character(get_col(c("membro",    "Membro"))),
    algoritmo = as.character(get_col(c("algoritmo", "Algoritmo", "Metodo", "metodo"))),
    objetivo  = as.character(get_col(c("objetivo",  "Objetivo"))),
    lucro     = suppressWarnings(as.numeric(get_col(c(
                  "lucro", "Lucro", "Lucro_Total", "lucro_total")))),
    unidades  = suppressWarnings(as.numeric(get_col(c(
                  "unidades", "Unidades", "Unidades_Vendidas", "unidades_vendidas")))),
    total_HR  = suppressWarnings(as.numeric(get_col(c(
                  "total_HR", "Total_HR", "totalHR", "total_hr", "HR")))),
    stringsAsFactors = FALSE
  )

  if (all(is.na(result$membro)    | result$membro    == "NA") && !is.na(membro_default))
    result$membro    <- membro_default
  if (all(is.na(result$algoritmo) | result$algoritmo == "NA") && !is.na(algoritmo_default))
    result$algoritmo <- algoritmo_default
  if (all(is.na(result$objetivo)  | result$objetivo  == "NA") && !is.na(objetivo_default))
    result$objetivo  <- objetivo_default

  result
}

# Carrega serie de convergencia de ficheiro RDS
# Suporta vector numerico ou lista com campo profit_history
load_rds_conv <- function(filepath, label) {
  if (!file.exists(filepath)) return(NULL)
  obj <- tryCatch(readRDS(filepath), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  v <- if (is.list(obj) && !is.null(obj$profit_history)) obj$profit_history
       else if (is.numeric(obj)) obj
       else NULL
  if (is.null(v) || length(v) == 0) return(NULL)
  list(algo = label, data = v)
}

# Carrega serie de convergencia de CSV (2 colunas: index, lucro)
load_csv_conv <- function(filepath, label) {
  if (!file.exists(filepath)) return(NULL)
  df <- tryCatch(read.csv(filepath, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || ncol(df) < 2) return(NULL)
  names_lwr <- tolower(names(df))
  col_idx <- which(names_lwr %in% c("lucro", "lucro_total"))[1]
  if (is.na(col_idx)) col_idx <- 2
  v <- suppressWarnings(as.numeric(df[[col_idx]]))
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NULL)
  list(algo = label, data = v)
}

# Plota comparacao de convergencia e guarda PDF
plot_convergence <- function(series_list, title, pdf_out) {
  valid <- Filter(Negate(is.null), series_list)
  if (length(valid) == 0) {
    cat("AVISO: sem dados de convergencia para:", title, "\n")
    return()
  }

  cores <- c("steelblue", "firebrick", "forestgreen", "darkorange", "purple", "brown")

  best_series <- lapply(valid, function(s) {
    v <- s$data
    v[is.nan(v) | is.infinite(v)] <- NA
    cummax(ifelse(is.na(v), -Inf, v))
  })

  y_vals  <- unlist(lapply(best_series, function(b) b[is.finite(b)]))
  y_range <- range(y_vals, na.rm = TRUE)

  pdf(pdf_out, width = 9, height = 6)
  for (i in seq_along(valid)) {
    best <- best_series[[i]]
    if (i == 1) {
      plot(best, type = "l", col = cores[i], lwd = 2,
           xlab = "Avaliacao", ylab = "Melhor Lucro",
           main = title, ylim = y_range)
    } else {
      lines(best, col = cores[i], lwd = 2)
    }
  }
  legend("bottomright",
         legend = sapply(valid, function(s) s$algo),
         col    = cores[seq_along(valid)],
         lwd    = 2, bty = "n")
  dev.off()
  cat("Guardado:", pdf_out, "\n")
}

# =============================================================
# TAREFA 1 — Tabela comparativa (todos os algoritmos)
# =============================================================
cat("\n=== TAREFA 1: Tabela comparativa ===\n")

sources <- list(
  list(
    file      = file.path(BASE_DIR, "RF",   "resultado_SANN.csv"),
    membro    = "Eduardo", algoritmo = NA,           objetivo = NA
  ),
  list(
    file      = file.path(BASE_DIR, "NSGA2","resultado_Eduardo_NSGA2.csv"),
    membro    = "Eduardo", algoritmo = NA,           objetivo = NA
  ),
  list(
    file      = file.path(BASE_DIR, "GA",   "OTIM_GA_resumo.csv"),
    membro    = "Joao",    algoritmo = "GA",         objetivo = "O1"
  ),
  list(
    file      = file.path(BASE_DIR, "PSO",  "resultados_O1", "tabela_resumo_PSO_O1.csv"),
    membro    = "Carolina",algoritmo = "PSO",        objetivo = NA
  ),
  list(
    file      = file.path(BASE_DIR, "MC",   "resultados_montecarlo_nuno.csv"),
    membro    = "Nuno",    algoritmo = "Monte Carlo",objetivo = NA
  )
)

tabelas <- lapply(sources, function(src) {
  if (!file.exists(src$file)) {
    cat("AVISO: nao encontrado:", src$file, "\n")
    return(NULL)
  }
  df <- tryCatch(read.csv(src$file, stringsAsFactors = FALSE), error = function(e) {
    cat("ERRO ao ler:", basename(src$file), "->", conditionMessage(e), "\n")
    NULL
  })
  if (is.null(df)) return(NULL)
  normalise_resultado(df, src$membro, src$algoritmo, src$objetivo)
})

tabela_final <- do.call(rbind, Filter(Negate(is.null), tabelas))
rownames(tabela_final) <- NULL

out_csv <- file.path(BASE_DIR, "tabela_comparativa_final.csv")
write.csv(tabela_final, out_csv, row.names = FALSE)
cat("Guardado:", out_csv, "\n\n")
print(tabela_final)

# =============================================================
# TAREFA 2 — Convergencia O1
# =============================================================
cat("\n=== TAREFA 2: Convergencia O1 ===\n")

conv_O1 <- list(
  load_rds_conv(file.path(BASE_DIR, "RF",  "convergencia_O1_SANN.rds"),      "SANN"),
  load_rds_conv(file.path(BASE_DIR, "GA",  "resultado_GA.rds"),              "GA"),
  load_csv_conv(file.path(BASE_DIR, "PSO", "resultados_O1",
                           "convergencia_PSO_O1.csv"),                        "PSO"),
  load_rds_conv(file.path(BASE_DIR, "MC",  "convergencia_O1_MC_nuno.rds"),   "MC")
)

plot_convergence(conv_O1,
                 "Convergencia O1 - Comparacao de Algoritmos",
                 file.path(BASE_DIR, "convergencia_O1_comparacao.pdf"))

# =============================================================
# TAREFA 3 — Convergencia O2
# =============================================================
cat("\n=== TAREFA 3: Convergencia O2 ===\n")

conv_O2 <- list(
  load_rds_conv(file.path(BASE_DIR, "RF", "convergencia_O2_SANN.rds"), "SANN"),
  load_rds_conv(file.path(BASE_DIR, "MC", "convergencia_O2_MC_nuno.rds"), "MC")
  # GA/PSO nao produziram historico O2
)

plot_convergence(conv_O2,
                 "Convergencia O2 - Comparacao de Algoritmos",
                 file.path(BASE_DIR, "convergencia_O2_comparacao.pdf"))

# =============================================================
# TAREFA 4 — Fronteira de Pareto O3
# =============================================================
cat("\n=== TAREFA 4: Fronteira de Pareto O3 ===\n")

pareto_file <- file.path(BASE_DIR, "NSGA2", "pareto_O3_fronteira.csv")

if (!file.exists(pareto_file)) {
  cat("AVISO: pareto_O3_fronteira.csv nao encontrado.\n")
} else {
  pareto       <- read.csv(pareto_file, stringsAsFactors = FALSE)
  names(pareto) <- tolower(names(pareto))
  if (!"lucro"    %in% names(pareto)) names(pareto)[1] <- "lucro"
  if (!"total_hr" %in% names(pareto)) names(pareto)[2] <- "total_hr"

  # Recolhe pontos individuais O3 (MC, outros sem fronteira completa)
  pontos_O3 <- list()
  mc_file <- file.path(BASE_DIR, "MC", "resultados_montecarlo_nuno.csv")
  if (file.exists(mc_file)) {
    mc_df <- read.csv(mc_file, stringsAsFactors = FALSE)
    names(mc_df) <- tolower(names(mc_df))
    col_obj <- grep("obj", names(mc_df))[1]
    if (!is.na(col_obj)) {
      row_O3 <- mc_df[tolower(as.character(mc_df[[col_obj]])) == "o3", ]
      if (nrow(row_O3) > 0) {
        col_l  <- grep("lucro", names(mc_df))[1]
        col_hr <- grep("total_hr|hr", names(mc_df))[1]
        pontos_O3[["Monte Carlo"]] <- list(
          lucro = as.numeric(row_O3[[col_l]]),
          hr    = as.numeric(row_O3[[col_hr]]),
          cor   = "forestgreen", pch = 17
        )
      }
    }
  }

  # Calcula range incluindo pontos extras
  todos_hr    <- c(pareto$total_hr,    unlist(lapply(pontos_O3, `[[`, "hr")))
  todos_lucro <- c(pareto$lucro, unlist(lapply(pontos_O3, `[[`, "lucro")))
  xr <- range(todos_hr,    na.rm = TRUE)
  yr <- range(todos_lucro, na.rm = TRUE)
  xr[2] <- xr[2] + diff(xr) * 0.20
  yr[1] <- yr[1] - diff(yr) * 0.05

  pdf_pareto <- file.path(BASE_DIR, "pareto_O3_final.pdf")
  pdf(pdf_pareto, width = 9, height = 6)

  plot(pareto$total_hr, pareto$lucro,
       xlab = "Total HR (horas)", ylab = "Lucro",
       main = "Fronteira de Pareto O3",
       pch = 19, col = "steelblue", cex = 1.0,
       xlim = xr, ylim = yr)
  ord <- order(pareto$total_hr)
  lines(pareto$total_hr[ord], pareto$lucro[ord], col = "steelblue", lwd = 1.5)

  # Destaque — solucao de compromisso NSGA-II (lucro=1241, HR=80)
  points(80, 1241, pch = 19, col = "red", cex = 2.5)
  text(80, 1241, labels = "NSGA-II\n(HR=80, L=1241)", pos = 4, col = "red", cex = 0.80)

  # Pontos individuais de outros algoritmos
  leg_lbl <- c("Fronteira Pareto (NSGA-II)", "Compromisso NSGA-II")
  leg_col <- c("steelblue", "red")
  leg_pch <- c(19, 19)
  for (nm in names(pontos_O3)) {
    p <- pontos_O3[[nm]]
    points(p$hr, p$lucro, pch = p$pch, col = p$cor, cex = 2.0)
    text(p$hr, p$lucro,
         labels = sprintf("%s\n(HR=%d, L=%d)", nm, p$hr, p$lucro),
         pos = 4, col = p$cor, cex = 0.80)
    leg_lbl <- c(leg_lbl, nm)
    leg_col <- c(leg_col, p$cor)
    leg_pch <- c(leg_pch, p$pch)
  }

  legend("topright", legend = leg_lbl, col = leg_col, pch = leg_pch, bty = "n")
  dev.off()
  cat("Guardado:", pdf_pareto, "\n")
}

# =============================================================
# TAREFA 5 — Resumo final: melhor algoritmo por objetivo
# =============================================================
cat("\n=== RESUMO FINAL: Melhor algoritmo por objetivo ===\n")

for (obj in c("O1", "O2", "O3")) {
  sub_df <- tabela_final[!is.na(tabela_final$objetivo) &
                           toupper(tabela_final$objetivo) == obj, ]
  sub_df <- sub_df[!is.na(sub_df$lucro), ]
  if (nrow(sub_df) == 0) { cat(obj, ": sem dados\n"); next }
  melhor <- sub_df[which.max(sub_df$lucro), ]
  cat(sprintf("%-3s -> %-14s (%-8s) | Lucro: %7g | Unidades: %6g | HR: %g\n",
              obj,
              melhor$algoritmo, melhor$membro,
              melhor$lucro, melhor$unidades, melhor$total_HR))
}

# =============================================================
# TAREFA 6 — Diagnostico de convergencia
# Para cada serie: plateau, ganho marginal, estabilidade
# =============================================================
cat("\n=== DIAGNOSTICO DE CONVERGENCIA ===\n")

conv_diag <- function(serie) {
  v <- serie$data
  v[is.nan(v) | is.infinite(v)] <- NA
  best <- cummax(ifelse(is.na(v), -Inf, v))
  best <- best[is.finite(best)]
  n    <- length(best)
  if (n < 10) return(NULL)

  melhor_final <- best[n]
  melhor_ini   <- best[1]

  # Ganho total
  ganho_total  <- melhor_final - melhor_ini

  # Ganho na 1a metade vs 2a metade
  meio         <- floor(n / 2)
  ganho_1a     <- best[meio]   - melhor_ini
  ganho_2a     <- melhor_final - best[meio]

  # Plateau: ultima avaliacao com melhoria > 1% do ganho total
  limiar       <- abs(ganho_total) * 0.01
  melhorias    <- which(diff(best) > limiar)
  plateau_em   <- if (length(melhorias) > 0) melhorias[length(melhorias)] else 1
  pct_plateau  <- round(100 * plateau_em / n, 1)

  # % do melhor final alcancado a 50% e 80% das avaliacoes
  pct50 <- round(100 * best[floor(n * 0.50)] / melhor_final, 1)
  pct80 <- round(100 * best[floor(n * 0.80)] / melhor_final, 1)

  # Estabilidade final: desvio do ultimo 10% de avaliacoes (se best ainda oscila)
  janela_fin   <- best[floor(n * 0.90):n]
  estavel      <- (max(janela_fin) - min(janela_fin)) < limiar

  list(
    algo         = serie$algo,
    n_aval       = n,
    melhor       = melhor_final,
    ganho_total  = ganho_total,
    ganho_1a_pct = round(100 * ganho_1a / max(abs(ganho_total), 1), 1),
    ganho_2a_pct = round(100 * ganho_2a / max(abs(ganho_total), 1), 1),
    plateau_aval = plateau_em,
    plateau_pct  = pct_plateau,
    pct50        = pct50,
    pct80        = pct80,
    estavel      = estavel
  )
}

imprimir_diag <- function(d) {
  if (is.null(d)) return()
  cat(sprintf(
    "  %-6s | %6d aval | melhor=%7g | ganho total=%7g\n",
    d$algo, d$n_aval, d$melhor, d$ganho_total))
  cat(sprintf(
    "         1a metade: %5.1f%% do ganho | 2a metade: %5.1f%% do ganho\n",
    d$ganho_1a_pct, d$ganho_2a_pct))
  cat(sprintf(
    "         Plateau apos aval %d (%s%% do total)\n",
    d$plateau_aval, d$plateau_pct))
  cat(sprintf(
    "         A 50%% das aval ja tinha %s%% do melhor | a 80%% tinha %s%%\n",
    d$pct50, d$pct80))
  cat(sprintf(
    "         Ultimo 10%% estavel: %s\n\n",
    ifelse(d$estavel, "SIM (convergiu)", "NAO (ainda melhorava)")))
}

cat("\n--- O1 ---\n")
for (s in Filter(Negate(is.null), conv_O1)) imprimir_diag(conv_diag(s))

cat("--- O2 ---\n")
for (s in Filter(Negate(is.null), conv_O2)) imprimir_diag(conv_diag(s))

# =============================================================
# TAREFA 7 — PDF: grafico de diagnostico (% do melhor vs % aval)
# Mostra quao rapido cada algoritmo chegou a 90%, 95%, 100%
# =============================================================
cat("=== TAREFA 7: Grafico de maturidade de convergencia ===\n")

plot_maturidade <- function(series_list, title, pdf_out) {
  valid <- Filter(Negate(is.null), series_list)
  if (length(valid) == 0) return()
  cores <- c("steelblue", "firebrick", "forestgreen", "darkorange", "purple")

  pdf(pdf_out, width = 9, height = 6)
  primeiro <- TRUE
  for (i in seq_along(valid)) {
    s    <- valid[[i]]
    v    <- s$data
    v[is.nan(v) | is.infinite(v)] <- NA
    best <- cummax(ifelse(is.na(v), -Inf, v))
    best <- best[is.finite(best)]
    n    <- length(best)
    if (n < 2) next
    pct_aval  <- seq(0, 100, length.out = n)
    pct_melhor <- 100 * best / best[n]

    if (primeiro) {
      plot(pct_aval, pct_melhor, type = "l",
           col = cores[i], lwd = 2,
           xlab = "% das avaliacoes usadas",
           ylab = "% do melhor resultado final",
           main = title,
           ylim = c(min(pct_melhor, 0), 102))
      abline(h = c(90, 95, 99), lty = 2, col = "grey70")
      primeiro <- FALSE
    } else {
      lines(pct_aval, pct_melhor, col = cores[i], lwd = 2)
    }
  }
  legend("bottomright",
         legend = sapply(valid, function(s) s$algo),
         col    = cores[seq_along(valid)],
         lwd = 2, bty = "n")
  dev.off()
  cat("Guardado:", pdf_out, "\n")
}

plot_maturidade(conv_O1,
                "Maturidade de Convergencia O1 (% do melhor vs % aval)",
                file.path(BASE_DIR, "maturidade_O1_comparacao.pdf"))

plot_maturidade(conv_O2,
                "Maturidade de Convergencia O2 (% do melhor vs % aval)",
                file.path(BASE_DIR, "maturidade_O2_comparacao.pdf"))

cat("\n=== FIM comparativos.R ===\n")
