setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data_dir    <- "../data"
out_dir     <- "../data/tratados"
dir.create(out_dir, showWarnings = FALSE)

preparar_dados <- function(df) {
  df$Date <- as.Date(df$Date)
  df <- df[order(df$Date), ]

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

  idx_na   <- which(df$Date == as.Date("2014-04-20"))
  wday_na  <- weekdays(as.Date("2014-04-20"))
  antes_na <- df[df$Date < as.Date("2014-04-20"), ]
  mesmo_na <- antes_na[weekdays(antes_na$Date) == wday_na, ]
  df$Pct_On_Sale[idx_na]   <- median(mesmo_na$Pct_On_Sale, na.rm = TRUE)
  df$Num_Customers[idx_na] <- median(mesmo_na$Num_Customers)

  black_fridays <- as.Date(c("2012-11-23", "2013-11-29"))
  df$TouristEvent[df$Date %in% black_fridays] <- "Yes"

  return(df)
}

lojas <- c("baltimore", "lancaster", "richmond", "philadelphia")

for (loja in lojas) {
  raw <- read.csv(file.path(data_dir, paste0(loja, ".csv")))
  tratado <- preparar_dados(raw)
  write.csv(tratado, file.path(out_dir, paste0(loja, "_tratado.csv")), row.names = FALSE)
  cat("Exportado:", loja, "_tratado.csv\n")
}

cat("\nDone. Ficheiros em", out_dir, "\n")
