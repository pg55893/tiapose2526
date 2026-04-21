# =============================================================================
# VERIFICAR CORRELAÇÃO ENTRE Num_Customers E Sales
# =============================================================================

check_sales_correlation <- function(dados, store_name = "Store", max_lag = 14) {
  d <- dados[, c("Date", "Num_Customers", "Sales")]
  d$Date <- as.Date(d$Date)
  d <- d[order(d$Date), ]
  
  cat("\n============================================================\n")
  cat("CORRELACAO -", toupper(store_name), "\n")
  cat("============================================================\n")
  
  cor_now <- cor(d$Num_Customers, d$Sales, use = "complete.obs")
  cat("Correlacao contemporanea Num_Customers vs Sales:", round(cor_now, 4), "\n")
  
  ccf_res <- ccf(d$Sales, d$Num_Customers, lag.max = max_lag, plot = TRUE,
                 main = paste("CCF -", store_name, "(Sales vs Num_Customers)"))
  
  ccf_df <- data.frame(
    lag = as.numeric(ccf_res$lag),
    corr = as.numeric(ccf_res$acf)
  )
  
  ccf_df <- ccf_df[order(-abs(ccf_df$corr)), ]
  
  cat("Lag com maior correlacao absoluta:", ccf_df$lag[1], "\n")
  cat("Valor dessa correlacao:", round(ccf_df$corr[1], 4), "\n")
  
  return(list(
    correlation = cor_now,
    ccf_table = ccf_df
  ))
}

# =============================================================================
# CORRER PARA TODAS AS LOJAS
# =============================================================================

par(mfrow = c(2, 2))

res_cor <- list(
  Baltimore    = check_sales_correlation(baltimore, "Baltimore"),
  Lancaster    = check_sales_correlation(lancaster, "Lancaster"),
  Philadelphia = check_sales_correlation(philadelphia, "Philadelphia"),
  Richmond     = check_sales_correlation(richmond, "Richmond")
)

par(mfrow = c(1, 1))