# ============================================================
# TADA - Projeto: Intelligent Decision Support System
# Fase: Data Understanding (CRISP-DM)
# Loja: Baltimore
# ============================================================

library(tidyverse)
library(dplyr)
library(rminer)

# --- import do csv baltimore ---
baltimore <- read.csv("baltimore.csv", stringsAsFactors = FALSE)

# --- summary ---
summary(baltimore)

# --- correlações ---
cor(baltimore[, c("Sales","Num_Employees","Num_Customers","Pct_On_Sale")], use="complete.obs")

# --- padrão semanal ---
baltimore$Date <- as.Date(baltimore$Date)
baltimore$Weekday <- weekdays(baltimore$Date)
baltimore$Weekday <- factor(baltimore$Weekday,
                            levels=c("Monday","Tuesday","Wednesday",
                                     "Thursday","Friday","Saturday","Sunday"))
aggregate(Sales ~ Weekday, data=baltimore, FUN=mean)

ggplot(baltimore, aes(x=Weekday, y=Sales)) +
  geom_boxplot(fill="#2c7bb6") +
  labs(title="Sales por Dia da Semana", x="Dia", y="Sales") +
  theme_minimal()

# --- TouristEvent ---
aggregate(Sales ~ TouristEvent, data=baltimore, FUN=mean)
table(baltimore$TouristEvent)

ggplot(baltimore, aes(x=TouristEvent, y=Sales, fill=TouristEvent)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Sales: Tourist Event vs Sem Evento", x="TouristEvent", y="Sales") +
  theme_minimal()

# --- outliers ---
Q1 <- quantile(baltimore$Sales, 0.25)
Q3 <- quantile(baltimore$Sales, 0.75)
IQR_val <- Q3 - Q1
outliers <- baltimore$Sales[baltimore$Sales < (Q1 - 1.5*IQR_val) | baltimore$Sales > (Q3 + 1.5*IQR_val)]
cat("Nº de outliers:", length(outliers), "\n")
sort(outliers)

ggplot(baltimore, aes(y=Sales)) +
  geom_boxplot(fill="#d7191c") +
  labs(title="Boxplot Sales (outliers)", y="Sales") +
  theme_minimal()

# --- dias com sales = 0 ---
baltimore[baltimore$Sales == 0, ]

# --- evolução anual ---
aggregate(Sales ~ format(Date, "%Y"), data=baltimore, FUN=mean)

ggplot(baltimore, aes(x=Date, y=Sales)) +
  geom_line(color="#2c7bb6", linewidth=0.5) +
  labs(title="Evolução Temporal das Sales", x="Data", y="Sales") +
  theme_minimal()

# --- verificar se o NA em Pct_On_Sale é isolado ou se está agrupado ---
which(is.na(baltimore$Pct_On_Sale))
baltimore[is.na(baltimore$Pct_On_Sale), ]

# --- analisar a distribuição de Num_Customers e Num_Employees para perceber se os máximos extremos --- 
baltimore[baltimore$Num_Customers > 500, ]
baltimore[baltimore$Num_Employees > 50, ]

