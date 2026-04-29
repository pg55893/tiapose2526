# =============================================================
# comparativos_final.R
# Comparação final: GA, MC, HC, SANN, PSO, DE, NSGA-II
# Métrica principal: mediana do lucro entre runs
# Eixo X das curvas: FES / número de avaliações
# =============================================================

BASE_DIR <- "~/TIAPOSE_projeto/tiapose2526/otimizacao"
OUT_DIR  <- file.path(BASE_DIR, "teste_final")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# =============================================================
# HELPERS
# =============================================================

normalise_resultado <- function(df, membro_default = NA,
                                algoritmo_default = NA,
                                objetivo_default = NA) {
  n <- nrow(df)
  names_lower <- tolower(names(df))
  
  get_col <- function(candidates) {
    idx <- which(names_lower %in% tolower(candidates))
    if (length(idx) > 0) df[[idx[1]]] else rep(NA, n)
  }
  
  result <- data.frame(
    membro = rep(membro_default, n),
    algoritmo = as.character(get_col(c("algoritmo", "metodo", "Metodo"))),
    objetivo = as.character(get_col(c("objetivo", "Objetivo"))),
    mediana = suppressWarnings(as.numeric(get_col(c("mediana", "Lucro_Mediana", "lucro_mediana")))),
    media = suppressWarnings(as.numeric(get_col(c("media", "Lucro_Media", "lucro_media")))),
    lucro_max = suppressWarnings(as.numeric(get_col(c("Lucro_Max", "lucro_max", "max")))),
    lucro_min = suppressWarnings(as.numeric(get_col(c("Lucro_Min", "lucro_min", "min")))),
    unidades = suppressWarnings(as.numeric(get_col(c("Unidades_Best", "unidades", "Unidades")))),
    total_HR = suppressWarnings(as.numeric(get_col(c("Total_HR_Best", "total_HR", "HR")))),
    tempo_s = suppressWarnings(as.numeric(get_col(c("Tempo_Total_s", "tempo_s", "tempo")))),
    stringsAsFactors = FALSE
  )
  
  if (all(is.na(result$algoritmo))) result$algoritmo <- algoritmo_default
  if (all(is.na(result$objetivo))) result$objetivo <- objetivo_default
  
  result
}

ler_resultado <- function(file, membro, algoritmo, objetivo = NA) {
  if (!file.exists(file)) {
    cat("AVISO: ficheiro não encontrado:", file, "\n")
    return(NULL)
  }
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  normalise_resultado(df, membro, algoritmo, objetivo)
}

# =============================================================
# TABELA COMPARATIVA FINAL
# =============================================================

sources <- list(
  # Carolina — PSO
  list(file = file.path(BASE_DIR, "PSO", "O1", "tabela_resumo_PSO_O1.csv"),
       membro = "Carolina", algoritmo = "PSO", objetivo = "O1"),
  
  list(file = file.path(BASE_DIR, "PSO", "O2", "tabela_resumo_PSO_O2.csv"),
       membro = "Carolina", algoritmo = "PSO", objetivo = "O2"),
  
  # Carolina — DE
  list(file = file.path(BASE_DIR, "DE", "O1", "tabela_resumo_DE_O1.csv"),
       membro = "Carolina", algoritmo = "DE", objetivo = "O1"),
  
  list(file = file.path(BASE_DIR, "DE", "O2", "tabela_resumo_DE_O2.csv"),
       membro = "Carolina", algoritmo = "DE", objetivo = "O2"),
  
  # Eduardo / Nuno / João
  list(file = file.path(BASE_DIR, "SANN", "v2", "resultado_SANN.csv"),
       membro = "Eduardo", algoritmo = "SANN", objetivo = NA),
  
  list(file = file.path(BASE_DIR, "HC", "resultado_HC.csv"),
       membro = "Eduardo", algoritmo = "HC", objetivo = NA),
  
  list(file = file.path(BASE_DIR, "NSGA2", "resultado_Eduardo_NSGA2.csv"),
       membro = "Eduardo", algoritmo = "NSGA-II", objetivo = "O3"),
  
  list(file = file.path(BASE_DIR, "MC", "resultado_MC.csv"),
       membro = "Nuno", algoritmo = "Monte Carlo", objetivo = NA),
  
  list(file = file.path(BASE_DIR, "GA", "OTIM_GA_resumo.csv"),
       membro = "João", algoritmo = "GA", objetivo = NA)
)

tabelas <- lapply(sources, function(s) {
  ler_resultado(s$file, s$membro, s$algoritmo, s$objetivo)
})

tabela_final <- do.call(rbind, Filter(Negate(is.null), tabelas))
rownames(tabela_final) <- NULL

# limpar nomes de métodos
tabela_final$algoritmo <- gsub("_", " ", tabela_final$algoritmo)

# ordenar por objetivo e mediana
tabela_final <- tabela_final[order(tabela_final$objetivo,
                                   -tabela_final$mediana,
                                   na.last = TRUE), ]

out_csv <- file.path(OUT_DIR, "tabela_comparativa_final.csv")
write.csv(tabela_final, out_csv, row.names = FALSE)

cat("\nTabela comparativa guardada em:\n", out_csv, "\n")
print(tabela_final)

# =============================================================
# RANKING POR OBJETIVO
# =============================================================

for (obj in c("O1", "O2", "O3")) {
  sub <- tabela_final[toupper(tabela_final$objetivo) == obj, ]
  sub <- sub[!is.na(sub$mediana), ]
  
  if (nrow(sub) == 0) next
  
  sub <- sub[order(-sub$mediana), ]
  sub$posicao <- paste0(seq_len(nrow(sub)), "º")
  
  out_rank <- file.path(OUT_DIR, paste0("ranking_", obj, ".csv"))
  write.csv(sub, out_rank, row.names = FALSE)
  
  cat("\nRanking", obj, "guardado em:", out_rank, "\n")
  print(sub[, c("posicao", "membro", "algoritmo", "mediana", "lucro_max", "lucro_min", "unidades", "tempo_s")])
}