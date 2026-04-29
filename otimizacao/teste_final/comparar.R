# =============================================================
# graficos_todos_algoritmos_CORRIGIDO.R
# Boxplot O1 + Boxplot O2 + Barras finais
# Usa os nomes reais dos ficheiros das tuas pastas
# =============================================================

BASE_DIR <- "~/TIAPOSE_projeto/tiapose2526/otimizacao"
OUT_DIR  <- file.path(BASE_DIR, "teste_final")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

add <- function(lst, x) {
  if (!is.null(x)) c(lst, list(x)) else lst
}

ler_col <- function(file, algoritmo, objetivo, coluna) {
  if (!file.exists(file)) {
    cat("AVISO: não encontrei:", file, "\n")
    return(NULL)
  }
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  if (!coluna %in% names(df)) {
    cat("AVISO: coluna", coluna, "não existe em:", file, "\n")
    cat("Colunas disponíveis:", paste(names(df), collapse = ", "), "\n")
    return(NULL)
  }
  
  data.frame(
    Algoritmo = algoritmo,
    Objetivo  = objetivo,
    Lucro     = as.numeric(df[[coluna]])
  )
}

ler_resultado_resumo <- function(file, algoritmo) {
  if (!file.exists(file)) {
    cat("AVISO: não encontrei:", file, "\n")
    return(NULL)
  }
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  nms <- tolower(names(df))
  
  col_obj <- which(nms %in% c("objetivo"))[1]
  col_med <- which(nms %in% c("mediana", "lucro_mediana", "median"))[1]
  col_luc <- which(nms %in% c("lucro", "lucro_max", "melhor", "max"))[1]
  
  if (is.na(col_obj)) {
    cat("AVISO: sem coluna objetivo em:", file, "\n")
    return(NULL)
  }
  
  col_use <- if (!is.na(col_med)) col_med else col_luc
  
  if (is.na(col_use)) {
    cat("AVISO: sem coluna de lucro/mediana em:", file, "\n")
    cat("Colunas disponíveis:", paste(names(df), collapse = ", "), "\n")
    return(NULL)
  }
  
  data.frame(
    Algoritmo = algoritmo,
    Objetivo  = toupper(as.character(df[[col_obj]])),
    Lucro     = as.numeric(df[[col_use]])
  )
}

dados <- list()

# =============================================================
# PSO
# =============================================================

dados <- add(dados, ler_col(
  file.path(BASE_DIR, "PSO", "O1", "lucros_runs_PSO_O1.csv"),
  "PSO", "O1", "Lucro"
))

dados <- add(dados, ler_col(
  file.path(BASE_DIR, "PSO", "O2", "lucros_runs_PSO_O2.csv"),
  "PSO", "O2", "Repair"
))

# =============================================================
# DE
# =============================================================

dados <- add(dados, ler_col(
  file.path(BASE_DIR, "DE", "O1", "lucros_runs_DE_O1.csv"),
  "DE", "O1", "Lucro"
))

dados <- add(dados, ler_col(
  file.path(BASE_DIR, "DE", "O2", "lucros_runs_DE_O2.csv"),
  "DE", "O2", "Repair"
))

# =============================================================
# GA-bin
# Só existe resultado_gabin.csv, não há runs individuais.
# Vai aparecer como 1 ponto no boxplot.
# =============================================================

dados <- add(dados, ler_resultado_resumo(
  file.path(BASE_DIR, "GA_bin", "resultado_gabin.csv"),
  "GA-bin"
))

# =============================================================
# HC
# Só existe resultado_HC.csv, não há lucros por run.
# Vai aparecer como 1 ponto no boxplot.
# =============================================================

dados <- add(dados, ler_resultado_resumo(
  file.path(BASE_DIR, "HC", "resultado_HC.csv"),
  "HC"
))

# =============================================================
# MC
# Usa resultado_MC.csv, se não houver ficheiro de runs individuais.
# =============================================================

dados <- add(dados, ler_resultado_resumo(
  file.path(BASE_DIR, "MC", "resultado_MC.csv"),
  "MC"
))

dados <- add(dados, ler_resultado_resumo(
  file.path(BASE_DIR, "MC", "resultados_montecarlo_nuno.csv"),
  "MC"
))

# =============================================================
# SANN
# =============================================================

dados <- add(dados, ler_resultado_resumo(
  file.path(BASE_DIR, "SANN", "v2", "resultado_SANN.csv"),
  "SANN"
))

# =============================================================
# Tabu
# =============================================================

dados <- add(dados, ler_resultado_resumo(
  file.path(BASE_DIR, "Tabu", "resultado_tabu.csv"),
  "Tabu"
))

# =============================================================
# JUNTAR DADOS
# =============================================================

dados <- do.call(rbind, dados)
dados <- dados[!is.na(dados$Lucro), ]
dados$Objetivo <- toupper(dados$Objetivo)

write.csv(dados, file.path(OUT_DIR, "dados_boxplots_final.csv"), row.names = FALSE)

cat("\nDados carregados:\n")
print(table(dados$Algoritmo, dados$Objetivo))

# =============================================================
# BOXPLOT O1
# =============================================================

dados_O1 <- dados[dados$Objetivo == "O1", ]

pdf(file.path(OUT_DIR, "boxplot_O1_todos_algoritmos.pdf"), width = 12, height = 6)

boxplot(Lucro ~ Algoritmo,
        data = dados_O1,
        col = "lightblue",
        main = "O1 - Max Profit (sem restrição)",
        xlab = "",
        ylab = "Lucro ($)",
        las = 2)

abline(h = 0, lty = 2, col = "gray70")

med_O1 <- tapply(dados_O1$Lucro, dados_O1$Algoritmo, median, na.rm = TRUE)
text(seq_along(med_O1),
     med_O1,
     labels = round(med_O1, 0),
     pos = 3,
     cex = 0.8,
     font = 2)

dev.off()

# =============================================================
# BOXPLOT O2
# =============================================================

dados_O2 <- dados[dados$Objetivo == "O2", ]

pdf(file.path(OUT_DIR, "boxplot_O2_todos_algoritmos.pdf"), width = 12, height = 6)

boxplot(Lucro ~ Algoritmo,
        data = dados_O2,
        col = "salmon",
        main = "O2 - Max Profit (<=10k unidades)",
        xlab = "",
        ylab = "Lucro ($)",
        las = 2)

abline(h = 0, lty = 2, col = "gray70")

med_O2 <- tapply(dados_O2$Lucro, dados_O2$Algoritmo, median, na.rm = TRUE)
text(seq_along(med_O2),
     med_O2,
     labels = round(med_O2, 0),
     pos = ifelse(med_O2 >= 0, 3, 1),
     cex = 0.8,
     font = 2)

dev.off()

# =============================================================
# GRÁFICO DE BARRAS FINAL
# =============================================================

medianas <- aggregate(Lucro ~ Algoritmo + Objetivo, dados, median, na.rm = TRUE)
write.csv(medianas, file.path(OUT_DIR, "medianas_comparacao_final.csv"), row.names = FALSE)

tab <- reshape(medianas,
               idvar = "Algoritmo",
               timevar = "Objetivo",
               direction = "wide")

names(tab) <- gsub("Lucro.", "", names(tab), fixed = TRUE)

if (!"O1" %in% names(tab)) tab$O1 <- NA
if (!"O2" %in% names(tab)) tab$O2 <- NA

tab <- tab[order(-tab$O1, na.last = TRUE), ]

mat <- t(as.matrix(tab[, c("O1", "O2")]))
colnames(mat) <- tab$Algoritmo

pdf(file.path(OUT_DIR, "comparacao_final_barras_O1_O2.pdf"), width = 12, height = 6)

bp <- barplot(mat,
              beside = TRUE,
              col = c("steelblue", "tomato"),
              main = "Comparação Final - Mediana do Lucro",
              ylab = "Mediana Lucro ($)",
              las = 2,
              ylim = c(min(0, min(mat, na.rm = TRUE) * 1.2),
                       max(mat, na.rm = TRUE) * 1.15))

abline(h = 0, lty = 2, col = "gray60")

legend("topright",
       legend = c("O1", "O2"),
       fill = c("steelblue", "tomato"),
       bty = "n")

text(bp,
     mat,
     labels = round(mat, 0),
     pos = ifelse(mat >= 0, 3, 1),
     cex = 0.75)

dev.off()

cat("\nGRÁFICOS GUARDADOS EM:\n")
cat(OUT_DIR, "\n")
cat(" - boxplot_O1_todos_algoritmos.pdf\n")
cat(" - boxplot_O2_todos_algoritmos.pdf\n")
cat(" - comparacao_final_barras_O1_O2.pdf\n")
cat(" - dados_boxplots_final.csv\n")
cat(" - medianas_comparacao_final.csv\n")