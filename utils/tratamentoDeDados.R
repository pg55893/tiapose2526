preparar_dados <- function(df) {
  df$Date <- as.Date(df$Date)
  
  # --- Garantir ordenação cronológica antes de qualquer operação ---
  df <- df[order(df$Date), ]
  
  # --- Natal: substituir todos os campos pela mediana do mesmo dia da semana, usando apenas dados anteriores ---
  natais <- as.Date(c("2012-12-25", "2013-12-25"))
  
  for (data_natal in natais) {
    data_natal <- as.Date(data_natal, origin = "1970-01-01")
    idx   <- which(df$Date == data_natal)
    wday  <- weekdays(data_natal)
    antes <- df[df$Date < data_natal, ]
    mesmo <- antes[weekdays(antes$Date) == wday, ]
    
    df$Sales[idx]         <- median(mesmo$Sales)
    df$Num_Customers[idx] <- median(mesmo$Num_Customers)
    df$Num_Employees[idx] <- median(mesmo$Num_Employees)
    df$Pct_On_Sale[idx]   <- median(mesmo$Pct_On_Sale)
  }
  
  # --- 2014-04-20 (Domingo de Páscoa): Pct_On_Sale é NA e Num_Customers é 0 (loja fechada) ---
  # --- Imputar com mediana do mesmo dia da semana, usando apenas dados anteriores ---
  idx_na   <- which(df$Date == as.Date("2014-04-20"))
  wday_na  <- weekdays(as.Date("2014-04-20"))
  antes_na <- df[df$Date < as.Date("2014-04-20"), ]
  mesmo_na <- antes_na[weekdays(antes_na$Date) == wday_na, ]
  
  df$Pct_On_Sale[idx_na]   <- median(mesmo_na$Pct_On_Sale, na.rm = TRUE)
  df$Num_Customers[idx_na] <- median(mesmo_na$Num_Customers)
  
  # --- Black Friday: marcar como TouristEvent = "Yes" ---
  black_fridays <- as.Date(c("2012-11-23", "2013-11-29"))
  df$TouristEvent[df$Date %in% black_fridays] <- "Yes"
  
  return(df)
}

# --- Aplicar a cada loja ---
baltimore    <- preparar_dados(read.csv("baltimore.csv"))
lancaster    <- preparar_dados(read.csv("lancaster.csv"))
richmond     <- preparar_dados(read.csv("richmond.csv"))
philadelphia <- preparar_dados(read.csv("philadelphia.csv"))

# --- Verificação 1: registos alterados ---
datas_verificar <- as.Date(c("2012-12-25", "2013-12-25", "2014-04-20", "2012-11-23", "2013-11-29"))

cat("=== Baltimore ===\n")
print(baltimore[baltimore$Date %in% datas_verificar, ])
cat("=== Lancaster ===\n")
print(lancaster[lancaster$Date %in% datas_verificar, ])
cat("=== Richmond ===\n")
print(richmond[richmond$Date %in% datas_verificar, ])
cat("=== Philadelphia ===\n")
print(philadelphia[philadelphia$Date %in% datas_verificar, ])

# --- Verificação 2: datas em falta em cada loja ---
for (nome in c("baltimore", "lancaster", "richmond", "philadelphia")) {
  df <- get(nome)
  datas_esperadas <- seq(min(df$Date), max(df$Date), by = "day")
  datas_em_falta  <- datas_esperadas[!datas_esperadas %in% df$Date]
  cat(nome, "- datas em falta:", length(datas_em_falta), "\n")
  if (length(datas_em_falta) > 0) print(datas_em_falta)
}

# --- Verificação 3: Sales <= 0 fora do Natal ---
natais <- as.Date(c("2012-12-25", "2013-12-25"))
for (nome in c("baltimore", "lancaster", "richmond", "philadelphia")) {
  df       <- get(nome)
  anomalos <- df[df$Sales <= 0 & !df$Date %in% natais, ]
  cat(nome, "- registos com Sales <= 0 fora do Natal:", nrow(anomalos), "\n")
  if (nrow(anomalos) > 0) print(anomalos)
}