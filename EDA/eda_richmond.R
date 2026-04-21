# ============================================================
# EDA para richmond.csv — Projeto DSS USA Stores
# Target: Num_Customers | Problema: Time Series Forecasting
# Dados: 714 registos diários (2012-07-01 a 2014-06-14)
# Variáveis exógenas: Num_Employees, Pct_On_Sale, TouristEvent
# Variável endógena: Sales
# ============================================================
# OUTPUTS GERADOS:
#   - EDA_richmond_log.txt  → todos os resultados em texto
#   - EDA_richmond_plots.pdf → todos os gráficos num único PDF
# ============================================================

# --- INICIAR REGISTO ---

log_file = "EDA_richmond_log.txt"                       # ficheiro onde guardar todos os outputs de texto
sink(log_file, split=TRUE)                              # redirige output para ficheiro E consola (split=TRUE)

pdf("EDA_richmond_plots.pdf", width=10, height=7)      # abre PDF para guardar todos os gráficos

cat("============================================================\n")
cat("EDA richmond.csv — Executado em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n\n")

# --- 1. CARREGAR E PREPARAR DADOS ---

cat("========================================\n")
cat("1. CARREGAR E PREPARAR DADOS\n")
cat("========================================\n")
cat("A ler o ficheiro richmond.csv...\n")
cat("Este dataset contém dados diários da loja de Richmond, VA.\n")
cat("O objectivo do projecto é prever Num_Customers (nº diário de clientes).\n\n")

d = read.table("richmond.csv", sep=",", header=TRUE)   # lê o CSV com separador vírgula e cabeçalho
d$Date = as.Date(d$Date)                                # converte texto para formato data
d$TouristEvent = as.factor(d$TouristEvent)              # converte para factor (Yes/No como níveis)

# --- 2. ESTRUTURA E RESUMO GERAL ---

cat("========================================\n")
cat("2. ESTRUTURA E RESUMO GERAL\n")
cat("========================================\n")
cat("str() mostra o tipo de cada variável (int, num, factor, Date).\n")
cat("summary() dá min, 1ºQ, mediana, média, 3ºQ e max de cada coluna.\n")
cat("Isto permite ter uma primeira noção das escalas e distribuições.\n\n")

str(d)                                                  # mostra tipo de cada variável
cat("\n")
print(summary(d))                                       # estatísticas descritivas por coluna
cat("\nDimensão:", nrow(d), "linhas x", ncol(d), "colunas\n")           # nº de observações e atributos
cat("Período:", as.character(min(d$Date)), "a", as.character(max(d$Date)), "\n")
cat("Total de dias:", as.numeric(max(d$Date) - min(d$Date)), "\n\n")    # duração total em dias

# --- 3. VALORES EM FALTA (NAs) ---

cat("========================================\n")
cat("3. VALORES EM FALTA (NAs)\n")
cat("========================================\n")
cat("Verificar se existem NAs é essencial antes de modelar.\n")
cat("Se houver NAs, será necessário decidir como tratá-los\n")
cat("(remover, imputar com média/mediana, interpolar, etc.).\n\n")

na_counts = colSums(is.na(d))                           # conta NAs por coluna
print(na_counts)                                        # mostra contagem (0 = sem valores em falta)
cat("\nTotal de NAs no dataset:", sum(na_counts), "\n\n") # total global de NAs

# --- 4. ESTATÍSTICAS DETALHADAS DO TARGET ---

cat("========================================\n")
cat("4. ESTATÍSTICAS DETALHADAS DO TARGET (Num_Customers)\n")
cat("========================================\n")
cat("Estatísticas adicionais do target para referência futura.\n")
cat("O desvio padrão e coeficiente de variação indicam a dispersão dos dados.\n\n")

target = d$Num_Customers                                # guarda o target numa variável
cat("Média:", round(mean(target, na.rm=TRUE), 2), "\n")            # média
cat("Mediana:", median(target, na.rm=TRUE), "\n")                  # mediana
cat("Desvio Padrão:", round(sd(target, na.rm=TRUE), 2), "\n")     # desvio padrão
cat("Mínimo:", min(target, na.rm=TRUE), "\n")                      # valor mínimo
cat("Máximo:", max(target, na.rm=TRUE), "\n")                      # valor máximo
cat("Coef. Variação:", round(sd(target, na.rm=TRUE)/mean(target, na.rm=TRUE)*100, 2), "%\n") # CV em %
cat("Nº Outliers (IQR):",                                         # conta outliers pelo método IQR
    sum(target < quantile(target, 0.25, na.rm=TRUE) - 1.5*IQR(target, na.rm=TRUE) |
          target > quantile(target, 0.75, na.rm=TRUE) + 1.5*IQR(target, na.rm=TRUE),
        na.rm=TRUE), "\n\n")

# --- 5. HISTOGRAMAS DAS VARIÁVEIS NUMÉRICAS ---

cat("========================================\n")
cat("5. HISTOGRAMAS\n")
cat("========================================\n")
cat("Os histogramas mostram a distribuição de frequências de cada variável.\n")
cat("Permitem ver se os dados são simétricos, assimétricos, bimodais, etc.\n")
cat("Num_Customers (TARGET) é o mais importante a analisar.\n")
cat("Sales é endógena — depende dos clientes, não é input do modelo.\n\n")

par(mfrow=c(2,2))                                       # divide janela em 2x2 (4 gráficos)
hist(d$Num_Customers, col="steelblue",
     main="Num_Customers (TARGET)", xlab="Clientes")    # distribuição do target
hist(d$Num_Employees, col="gray",
     main="Num_Employees", xlab="Empregados")            # distribuição nº empregados
hist(d$Pct_On_Sale, col="gray",
     main="Pct_On_Sale", xlab="% Promoção")              # distribuição % produtos em promoção
hist(d$Sales, col="gray",
     main="Sales (endógena)", xlab="Vendas")              # distribuição vendas diárias

# --- 6. BOXPLOTS (OUTLIERS) ---

cat("========================================\n")
cat("6. BOXPLOTS (DETECÇÃO DE OUTLIERS)\n")
cat("========================================\n")
cat("Os boxplots mostram mediana, quartis e outliers (pontos fora dos bigodes).\n")
cat("Outliers extremos podem distorcer modelos de previsão.\n")
cat("Se existirem, pode ser necessário investigar se são erros ou valores reais.\n\n")

par(mfrow=c(2,2))                                       # grelha 2x2
boxplot(d$Num_Customers, main="Num_Customers (TARGET)")  # mediana, quartis e outliers do target
boxplot(d$Num_Employees, main="Num_Employees")            # idem para empregados
boxplot(d$Pct_On_Sale, main="Pct_On_Sale")                # idem para % promoção
boxplot(d$Sales, main="Sales")                              # idem para vendas

# --- 7. DISTRIBUIÇÃO DA VARIÁVEL CATEGÓRICA ---

cat("========================================\n")
cat("7. DISTRIBUIÇÃO DE TouristEvent\n")
cat("========================================\n")
cat("TouristEvent indica se houve evento turístico nesse dia (Yes/No).\n")
cat("O barplot mostra a proporção entre dias com e sem evento.\n")
cat("Se uma classe for muito rara, pode ter pouco impacto estatístico.\n\n")

par(mfrow=c(1,1))                                       # volta a 1 gráfico por janela
barplot(table(d$TouristEvent),
        main="TouristEvent (Yes vs No)",
        col=c("gray","steelblue"))                       # contagem de dias Yes vs No

te_table = table(d$TouristEvent)                         # tabela de frequências
print(te_table)                                          # imprime contagens
cat("Proporção Yes:", round(te_table["Yes"]/sum(te_table)*100, 1), "%\n\n") # % de dias com evento

# --- 8. CORRELAÇÕES NUMÉRICAS ---

cat("========================================\n")
cat("8. MATRIZ DE CORRELAÇÃO\n")
cat("========================================\n")
cat("A correlação de Pearson mede a relação linear entre pares de variáveis.\n")
cat("Valores perto de 1 ou -1 indicam forte relação linear.\n")
cat("Valores perto de 0 indicam pouca relação linear.\n")
cat("Atenção: Sales é endógena, logo a correlação com Num_Customers é esperada.\n\n")

nums = d[, c("Num_Customers","Num_Employees","Pct_On_Sale","Sales")] # selecciona só numéricas
cor_matrix = round(cor(nums, use="complete.obs"), 3)     # calcula e arredonda a 3 decimais
print(cor_matrix)                                        # imprime a matriz
cat("\n")

# --- 9. SCATTERPLOT MATRIX ---

cat("========================================\n")
cat("9. SCATTERPLOT MATRIX\n")
cat("========================================\n")
cat("Mostra gráficos de dispersão entre todos os pares de variáveis numéricas.\n")
cat("Permite visualizar rapidamente relações lineares ou não-lineares.\n\n")

par(mfrow=c(1,1))                                       # 1 gráfico
plot(nums, main="Scatterplot Matrix")                    # dispersão entre todos os pares

# --- 10. RELAÇÃO INPUTS vs TARGET ---

cat("========================================\n")
cat("10. INPUTS vs TARGET (Num_Customers)\n")
cat("========================================\n")
cat("Analisa a relação individual de cada variável exógena com o target.\n")
cat("Os scatterplots mostram se há relação entre input e Num_Customers.\n")
cat("O boxplot mostra se eventos turísticos influenciam o nº de clientes.\n\n")

par(mfrow=c(2,2))                                       # grelha 2x2
plot(d$Num_Employees, d$Num_Customers,
     xlab="Num_Employees", ylab="Num_Customers",
     main="Empregados vs Clientes")                      # dispersão empregados vs target
plot(d$Pct_On_Sale, d$Num_Customers,
     xlab="Pct_On_Sale", ylab="Num_Customers",
     main="% Promoção vs Clientes")                      # dispersão promoção vs target
plot(d$Sales, d$Num_Customers,
     xlab="Sales", ylab="Num_Customers",
     main="Vendas vs Clientes")                           # dispersão vendas vs target
boxplot(Num_Customers ~ TouristEvent, data=d,
        main="Clientes vs TouristEvent",
        col=c("gray","steelblue"))                       # distribuição do target por evento turístico

# Estatísticas por grupo TouristEvent
cat("Média de clientes SEM evento:", round(mean(d$Num_Customers[d$TouristEvent=="No"], na.rm=TRUE), 2), "\n")
cat("Média de clientes COM evento:", round(mean(d$Num_Customers[d$TouristEvent=="Yes"], na.rm=TRUE), 2), "\n\n")

# ============================================================
# ANÁLISE TEMPORAL (essencial para forecasting)
# ============================================================

# --- 11. SÉRIE TEMPORAL DO TARGET ---

cat("========================================\n")
cat("11. SÉRIE TEMPORAL DO TARGET\n")
cat("========================================\n")
cat("Gráfico da evolução diária de Num_Customers ao longo de ~2 anos.\n")
cat("Permite identificar visualmente tendência (crescente/decrescente),\n")
cat("sazonalidade (padrões repetitivos) e possíveis anomalias.\n\n")

par(mfrow=c(1,1))                                       # 1 gráfico
plot(d$Date, d$Num_Customers, type="l",
     xlab="Data", ylab="Num_Customers",
     main="Série Temporal: Num_Customers (TARGET)",
     col="steelblue")                                    # linha temporal do target

# --- 12. SÉRIES TEMPORAIS DAS OUTRAS VARIÁVEIS ---

cat("========================================\n")
cat("12. SÉRIES TEMPORAIS DAS VARIÁVEIS EXÓGENAS\n")
cat("========================================\n")
cat("Visualiza a evolução temporal das variáveis que podem ser inputs\n")
cat("em modelos multivariados (VAR, ARIMAX, ML multivariado).\n")
cat("Ajuda a perceber se estas variáveis têm padrões semelhantes ao target.\n\n")

par(mfrow=c(2,2))                                       # grelha 2x2
plot(d$Date, d$Num_Employees, type="l",
     main="Num_Employees ao longo do tempo",
     xlab="Data", ylab="Empregados")                     # série temporal empregados
plot(d$Date, d$Pct_On_Sale, type="l",
     main="Pct_On_Sale ao longo do tempo",
     xlab="Data", ylab="% Promoção")                     # série temporal promoção
plot(d$Date, d$Sales, type="l",
     main="Sales ao longo do tempo",
     xlab="Data", ylab="Vendas")                          # série temporal vendas

# --- 13. SAZONALIDADE SEMANAL ---

cat("========================================\n")
cat("13. SAZONALIDADE SEMANAL\n")
cat("========================================\n")
cat("Boxplot do nº de clientes agrupado por dia da semana.\n")
cat("Se houver diferenças claras entre dias (ex: fim-de-semana vs semana),\n")
cat("isso confirma sazonalidade semanal — muito relevante para o forecasting.\n")
cat("O projecto pede previsões com horizonte de 7 dias (H=7).\n\n")

d$DayOfWeek = weekdays(d$Date)                           # extrai o nome do dia da semana
d$DayOfWeek = factor(d$DayOfWeek,                        # ordena os dias logicamente
                     levels=c("Monday","Tuesday","Wednesday",
                              "Thursday","Friday","Saturday","Sunday"))

par(mfrow=c(1,1))                                       # 1 gráfico
boxplot(Num_Customers ~ DayOfWeek, data=d,
        main="Clientes por Dia da Semana",
        col="steelblue", las=2)                          # las=2 roda os labels do eixo X

# Médias por dia da semana
cat("Média de clientes por dia da semana:\n")
print(round(tapply(d$Num_Customers, d$DayOfWeek, mean, na.rm=TRUE), 1)) # média por dia
cat("\n")

# --- 14. SAZONALIDADE MENSAL ---

cat("========================================\n")
cat("14. SAZONALIDADE MENSAL\n")
cat("========================================\n")
cat("Boxplot do nº de clientes agrupado por mês.\n")
cat("Permite detectar padrões anuais (ex: mais clientes no verão).\n")
cat("Útil para perceber se modelos precisam de capturar tendência anual.\n\n")

d$Month = months(d$Date)                                 # extrai o nome do mês
d$Month = factor(d$Month,                                # ordena os meses logicamente
                 levels=c("January","February","March","April",
                          "May","June","July","August",
                          "September","October","November","December"))

par(mfrow=c(1,1))                                       # 1 gráfico
boxplot(Num_Customers ~ Month, data=d,
        main="Clientes por Mês",
        col="lightgreen", las=2)                         # boxplot por mês

# Médias por mês
cat("Média de clientes por mês:\n")
print(round(tapply(d$Num_Customers, d$Month, mean, na.rm=TRUE), 1)) # média por mês
cat("\n")

# --- 15. AUTOCORRELAÇÃO (ACF e PACF) ---

cat("========================================\n")
cat("15. AUTOCORRELAÇÃO (ACF e PACF)\n")
cat("========================================\n")
cat("A ACF (Autocorrelation Function) mostra a correlação entre o valor\n")
cat("actual e valores passados (lags). Picos regulares a cada 7 lags\n")
cat("confirmam sazonalidade semanal.\n")
cat("A PACF (Partial ACF) mostra a correlação directa com cada lag,\n")
cat("removendo o efeito dos lags intermédios. Ajuda a identificar\n")
cat("a ordem dos modelos AR/ARIMA.\n\n")

ts_customers = ts(d$Num_Customers, frequency=7)          # cria objecto ts com frequência semanal
par(mfrow=c(1,2))                                        # 2 gráficos lado a lado
acf(ts_customers, lag.max=60,
    main="ACF Num_Customers")                             # autocorrelação até 60 lags
pacf(ts_customers, lag.max=60,
     main="PACF Num_Customers")                           # autocorrelação parcial até 60 lags

# --- 16. DECOMPOSIÇÃO DA SÉRIE TEMPORAL ---

cat("========================================\n")
cat("16. DECOMPOSIÇÃO DA SÉRIE TEMPORAL\n")
cat("========================================\n")
cat("Decompõe a série em 3 componentes aditivas:\n")
cat("  - Tendência (trend): direcção geral a longo prazo\n")
cat("  - Sazonalidade (seasonal): padrão repetitivo semanal\n")
cat("  - Resíduo (remainder): variação aleatória não explicada\n")
cat("Se a componente sazonal for forte, modelos como ARIMA sazonal\n")
cat("ou Holt-Winters devem funcionar bem.\n\n")

par(mfrow=c(1,1))                                        # 1 gráfico (decompose gera 4 painéis)
dec = decompose(ts_customers)                             # decompõe em tendência + sazonalidade + resíduo
plot(dec)                                                 # visualiza as 3 componentes

# Regista amplitude da sazonalidade
cat("Amplitude sazonal (max - min da componente seasonal):",
    round(max(dec$seasonal, na.rm=TRUE) - min(dec$seasonal, na.rm=TRUE), 2), "\n\n")

# --- 17. RESUMO FINAL ---

cat("============================================================\n")
cat("17. RESUMO DA EDA\n")
cat("============================================================\n")
cat("Dataset: richmond.csv |", nrow(d), "registos |", ncol(d), "variáveis originais\n")
cat("Período:", as.character(min(d$Date)), "a", as.character(max(d$Date)), "\n")
cat("Target: Num_Customers (média:", round(mean(target, na.rm=TRUE), 1),
    "| sd:", round(sd(target, na.rm=TRUE), 1), ")\n")
cat("NAs totais:", sum(na_counts), "\n")
cat("Correlação Num_Customers ~ Sales:", cor_matrix["Num_Customers","Sales"], "\n")
cat("Correlação Num_Customers ~ Num_Employees:", cor_matrix["Num_Customers","Num_Employees"], "\n")
cat("Correlação Num_Customers ~ Pct_On_Sale:", cor_matrix["Num_Customers","Pct_On_Sale"], "\n")
cat("Proporção TouristEvent=Yes:", round(te_table["Yes"]/sum(te_table)*100, 1), "%\n")
cat("Amplitude sazonal semanal:",
    round(max(dec$seasonal, na.rm=TRUE) - min(dec$seasonal, na.rm=TRUE), 2), "\n")
cat("============================================================\n\n")

cat("EDA CONCLUÍDA.\n\n")
cat("Próximos passos (segundo o guia do projecto):\n")
cat("  1. Forecasting univariado (ex: ARIMA, Holt-Winters, ETS)\n")
cat("  2. Forecasting multivariado (ex: VAR, ARIMAX, ML)\n")
cat("  3. Backtesting com growing/rolling window\n")
cat("  4. Optimização (plano semanal de recursos)\n")
cat("  5. DSS (sistema de apoio à decisão)\n\n")

cat("Ficheiros gerados:\n")
cat("  - EDA_richmond_log.txt   → este log com todos os resultados\n")
cat("  - EDA_richmond_plots.pdf → todos os gráficos\n")

# --- FECHAR REGISTO ---

dev.off()                                                 # fecha o PDF de gráficos
sink()                                                    # fecha o registo de texto

# Mensagem final na consola (já fora do sink)
cat("\n>> EDA concluída com sucesso!\n")
cat(">> Log guardado em: EDA_richmond_log.txt\n")
cat(">> Gráficos guardados em: EDA_richmond_plots.pdf\n")

