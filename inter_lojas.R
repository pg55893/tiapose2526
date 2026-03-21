# ============================================================
# CORRELACAO INTER-LOJAS
# ============================================================

library(corrplot)

cat("============================================================\n")
cat("ANALISE DE CORRELACAO INTER-LOJAS\n")
cat("============================================================\n\n")

# ============================================================
# 1. CARREGAR DADOS DAS 4 LOJAS
# ============================================================

cat("========================================\n")
cat("1. CARREGAR DADOS DAS 4 LOJAS\n")
cat("========================================\n")

baltimore    <- read.csv("Files/csv/baltimore.csv")
lancaster    <- read.csv("Files/csv/lancaster.csv")
philadelphia <- read.csv("Files/csv/philadelphia.csv")
richmond     <- read.csv("Files/csv/richmond.csv")

# Converter datas
baltimore$Date    <- as.Date(baltimore$Date)
lancaster$Date    <- as.Date(lancaster$Date)
philadelphia$Date <- as.Date(philadelphia$Date)
richmond$Date     <- as.Date(richmond$Date)

# Converter TouristEvent para 0/1
baltimore$TouristEvent    <- ifelse(baltimore$TouristEvent == "Yes", 1, 0)
lancaster$TouristEvent    <- ifelse(lancaster$TouristEvent == "Yes", 1, 0)
philadelphia$TouristEvent <- ifelse(philadelphia$TouristEvent == "Yes", 1, 0)
richmond$TouristEvent     <- ifelse(richmond$TouristEvent == "Yes", 1, 0)

cat("TouristEvent convertido: No = 0 | Yes = 1\n\n")

cat("Baltimore:    ", nrow(baltimore), "registos\n")
cat("Lancaster:    ", nrow(lancaster), "registos\n")
cat("Philadelphia: ", nrow(philadelphia), "registos\n")
cat("Richmond:     ", nrow(richmond), "registos\n\n")

# ============================================================
# FUNCAO AUXILIAR PARA MOSTRAR HEATMAPS UM A UM
# ============================================================

mostrar_corrplot <- function(matriz, titulo) {
  par(mfrow = c(1,1))
  corrplot(
    matriz,
    method = "color",
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 1.1,
    col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
    addCoef.col = "black",
    number.cex = 0.9,
    mar = c(0, 0, 2, 0),
    title = titulo
  )
}

# ============================================================
# 2. CORRELACAO INTER-LOJAS - Num_Customers
# ============================================================

cat("========================================\n")
cat("2. CORRELACAO INTER-LOJAS - Num_Customers\n")
cat("========================================\n")

clientes_lojas <- data.frame(
  Baltimore    = baltimore$Num_Customers,
  Lancaster    = lancaster$Num_Customers,
  Philadelphia = philadelphia$Num_Customers,
  Richmond     = richmond$Num_Customers
)

cor_inter_clientes <- round(cor(clientes_lojas, use = "complete.obs"), 3)

cat("Matriz de correlacao inter-lojas (Num_Customers):\n")
print(cor_inter_clientes)
cat("\n")

mostrar_corrplot(cor_inter_clientes, "Correlacao Inter-Lojas - Num_Customers")

# ============================================================
# 3. CORRELACAO INTER-LOJAS - Num_Employees
# ============================================================

cat("========================================\n")
cat("3. CORRELACAO INTER-LOJAS - Num_Employees\n")
cat("========================================\n")

employees_lojas <- data.frame(
  Baltimore    = baltimore$Num_Employees,
  Lancaster    = lancaster$Num_Employees,
  Philadelphia = philadelphia$Num_Employees,
  Richmond     = richmond$Num_Employees
)

cor_inter_emp <- round(cor(employees_lojas, use = "complete.obs"), 3)

cat("Matriz de correlacao inter-lojas (Num_Employees):\n")
print(cor_inter_emp)
cat("\n")

mostrar_corrplot(cor_inter_emp, "Correlacao Inter-Lojas - Num_Employees")

# ============================================================
# 4. CORRELACAO INTER-LOJAS - Sales
# ============================================================

cat("========================================\n")
cat("4. CORRELACAO INTER-LOJAS - Sales\n")
cat("========================================\n")

sales_lojas <- data.frame(
  Baltimore    = baltimore$Sales,
  Lancaster    = lancaster$Sales,
  Philadelphia = philadelphia$Sales,
  Richmond     = richmond$Sales
)

cor_inter_sales <- round(cor(sales_lojas, use = "complete.obs"), 3)

cat("Matriz de correlacao inter-lojas (Sales):\n")
print(cor_inter_sales)
cat("\n")

mostrar_corrplot(cor_inter_sales, "Correlacao Inter-Lojas - Sales")

# ============================================================
# 5. CORRELACAO INTER-LOJAS - Pct_On_Sale
# ============================================================

cat("========================================\n")
cat("5. CORRELACAO INTER-LOJAS - Pct_On_Sale\n")
cat("========================================\n")

promo_lojas <- data.frame(
  Baltimore    = baltimore$Pct_On_Sale,
  Lancaster    = lancaster$Pct_On_Sale,
  Philadelphia = philadelphia$Pct_On_Sale,
  Richmond     = richmond$Pct_On_Sale
)

cor_inter_promo <- round(cor(promo_lojas, use = "complete.obs"), 3)

cat("Matriz de correlacao inter-lojas (Pct_On_Sale):\n")
print(cor_inter_promo)
cat("\n")

mostrar_corrplot(cor_inter_promo, "Correlacao Inter-Lojas - Pct_On_Sale")

# ============================================================
# 6. CORRELACAO INTER-LOJAS - TouristEvent
# ============================================================

cat("========================================\n")
cat("6. CORRELACAO INTER-LOJAS - TouristEvent\n")
cat("========================================\n")

event_lojas <- data.frame(
  Baltimore    = baltimore$TouristEvent,
  Lancaster    = lancaster$TouristEvent,
  Philadelphia = philadelphia$TouristEvent,
  Richmond     = richmond$TouristEvent
)

cor_inter_event <- round(cor(event_lojas, use = "complete.obs"), 3)

cat("Matriz de correlacao inter-lojas (TouristEvent):\n")
print(cor_inter_event)
cat("\n")

mostrar_corrplot(cor_inter_event, "Correlacao Inter-Lojas - TouristEvent")

# ============================================================
# 7. SCATTERPLOT MATRIX - Num_Customers ENTRE LOJAS
# ============================================================

cat("========================================\n")
cat("7. SCATTERPLOT MATRIX - Num_Customers ENTRE LOJAS\n")
cat("========================================\n\n")

par(mfrow = c(1,1))
pairs(
  clientes_lojas,
  main = "Scatterplot Matrix - Num_Customers entre Lojas"
)

# ============================================================
# 8. RESUMO FINAL
# ============================================================

cat("========================================\n")
cat("8. RESUMO FINAL\n")
cat("========================================\n\n")

cat("Num_Customers:\n")
print(cor_inter_clientes)
cat("\n")

cat("Num_Employees:\n")
print(cor_inter_emp)
cat("\n")

cat("Sales:\n")
print(cor_inter_sales)
cat("\n")

cat("Pct_On_Sale:\n")
print(cor_inter_promo)
cat("\n")

cat("TouristEvent:\n")
print(cor_inter_event)
cat("\n")

cat("============================================================\n")
cat("ANALISE INTER-LOJAS CONCLUIDA\n")
cat("============================================================\n")