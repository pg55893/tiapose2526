# =============================================================
# visualizacao_utils.R
# Funções utilitárias para gráficos (Sombreado/Ribbons, Paletas)
# =============================================================

library(ggplot2)

# --- Paleta sugerida pelo professor ---
PALETA_DSS <- list(
  principal = "#4682B4", # SteelBlue (Cinzento-Azul)
  suave     = "#B0C4DE", # LightSteelBlue
  contraste = "#2E8B57", # SeaGreen (Cinzento-Verde)
  invalido  = "#CD5C5C"  # IndianRed
)

# --- Função para plotar convergência com sombreado (Task 1.2) ---
plot_shaded_convergence <- function(hists, title, xlab = "Avaliações (FES)", ylab = "Melhor Lucro") {
  # hists: lista de vectores numéricos (cada um é um run)
  
  # Alinhar tamanhos
  max_len <- max(sapply(hists, length))
  mat <- sapply(hists, function(h) {
    c(h, rep(h[length(h)], max_len - length(h)))
  })
  
  # Estatísticas
  df <- data.frame(
    fes    = 1:max_len,
    mediana = apply(mat, 1, median, na.rm = TRUE),
    q25     = apply(mat, 1, quantile, probs = 0.25, na.rm = TRUE),
    q75     = apply(mat, 1, quantile, probs = 0.75, na.rm = TRUE),
    min     = apply(mat, 1, min, na.rm = TRUE),
    max     = apply(mat, 1, max, na.rm = TRUE)
  )
  
  ggplot(df, aes(x = fes)) +
    # Área sombreada (Min/Max ou Q25/Q75)
    geom_ribbon(aes(ymin = min, ymax = max), fill = PALETA_DSS$suave, alpha = 0.3) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = PALETA_DSS$principal, alpha = 0.4) +
    # Linha da mediana por cima
    geom_line(aes(y = mediana), color = PALETA_DSS$principal, linewidth = 1.2) +
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}
