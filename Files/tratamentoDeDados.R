preparar_dados <- function(df) {
  df$Date <- as.Date(df$Date)
  
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
  
  # --- 2014-04-20: substituir Pct_On_Sale (NA) e Num_Customers (0 suspeito) pela mediana, usando apenas dados anteriores ---
  idx_na  <- which(df$Date == as.Date("2014-04-20"))
  wday_na <- weekdays(as.Date("2014-04-20"))
  antes_na <- df[df$Date < as.Date("2014-04-20"), ]
  mesmo_na <- antes_na[weekdays(antes_na$Date) == wday_na, ]
  
  df$Pct_On_Sale[idx_na]   <- median(antes_na$Pct_On_Sale, na.rm = TRUE)
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