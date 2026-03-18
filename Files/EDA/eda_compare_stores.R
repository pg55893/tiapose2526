# ============================================================
# EDA COMPARATIVA - 4 Lojas USA Stores (TADA)
# Prof. Paulo Cortez - Universidade do Minho
#
# Compara Baltimore, Lancaster, Philadelphia e Richmond
# Outputs:
#   EDA_compare_log.txt   -> resultados em texto
#   EDA_compare_plots.pdf -> todos os graficos comparativos
# ============================================================

log_file = "EDA_compare_log.txt"
pdf_file = "EDA_compare_plots.pdf"

sink(log_file, split=TRUE)
pdf(pdf_file, width=12, height=7)

tryCatch({
  
  # ---- CORES E CONFIGURACOES POR LOJA ----
  stores  = c("baltimore","lancaster","philadelphia","richmond")
  cores   = c("steelblue","tomato","seagreen","darkorange")
  configs = list(
    baltimore    = list(Fj=1.00, Fx=1.15, Ws=700),
    lancaster    = list(Fj=1.05, Fx=1.20, Ws=730),
    philadelphia = list(Fj=1.10, Fx=1.15, Ws=760),
    richmond     = list(Fj=1.15, Fx=1.25, Ws=800)
  )
  
  cat("============================================================\n")
  cat("EDA COMPARATIVA - 4 Lojas | Executado em:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("============================================================\n\n")
  
  # ============================================================
  # 1. CARREGAR TODOS OS DATASETS
  # ============================================================
  
  cat("========================================\n")
  cat("1. CARREGAR DADOS\n")
  cat("========================================\n")
  
  data_list = list()
  for (s in stores) {
    d = read.table(paste0(s, ".csv"), sep=",", header=TRUE)
    d$Date         = as.Date(d$Date)
    d$TouristEvent = as.factor(d$TouristEvent)
    d$DayOfWeek    = factor(weekdays(d$Date),
                            levels=c("Monday","Tuesday","Wednesday",
                                     "Thursday","Friday","Saturday","Sunday"))
    d$Month        = factor(months(d$Date),
                            levels=c("January","February","March","April",
                                     "May","June","July","August",
                                     "September","October","November","December"))
    d$Store        = s
    data_list[[s]] = d
    cat(s, "| Periodo:", as.character(min(d$Date)), "a", as.character(max(d$Date)),
        "| Registos:", nrow(d), "\n")
  }
  
  # dataset combinado
  all_data = do.call(rbind, data_list)
  cat("\nTotal combinado:", nrow(all_data), "registos\n\n")
  
  # ============================================================
  # 2. RESUMO ESTATISTICO COMPARATIVO
  # ============================================================
  
  cat("========================================\n")
  cat("2. RESUMO ESTATISTICO - Num_Customers (TARGET)\n")
  cat("========================================\n")
  
  for (s in stores) {
    d = data_list[[s]]
    x = d$Num_Customers
    cat(sprintf("%-15s | Media: %6.1f | Mediana: %6.1f | Sd: %6.1f | Min: %4d | Max: %5d | CV: %5.1f%%\n",
                toupper(s),
                mean(x, na.rm=TRUE), median(x, na.rm=TRUE), sd(x, na.rm=TRUE),
                min(x, na.rm=TRUE), max(x, na.rm=TRUE),
                sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100))
  }
  cat("\n")
  
  cat("========================================\n")
  cat("RESUMO ESTATISTICO - Sales\n")
  cat("========================================\n")
  
  for (s in stores) {
    d = data_list[[s]]
    x = d$Sales
    cat(sprintf("%-15s | Media: %7.0f | Mediana: %7.0f | Sd: %7.0f | Min: %6d | Max: %7d\n",
                toupper(s),
                mean(x, na.rm=TRUE), median(x, na.rm=TRUE), sd(x, na.rm=TRUE),
                min(x, na.rm=TRUE), max(x, na.rm=TRUE)))
  }
  cat("\n")
  
  # ============================================================
  # 3. QUALIDADE DOS DADOS
  # ============================================================
  
  cat("========================================\n")
  cat("3. QUALIDADE DOS DADOS\n")
  cat("========================================\n")
  
  for (s in stores) {
    d = data_list[[s]]
    cat(toupper(s), "\n")
    cat("  NAs:          ", sum(colSums(is.na(d))), "\n")
    cat("  Zeros Cust:   ", sum(d$Num_Customers == 0, na.rm=TRUE), "\n")
    cat("  Zeros Sales:  ", sum(d$Sales == 0, na.rm=TRUE), "\n")
    cat("  Dup. Datas:   ", sum(duplicated(d$Date)), "\n")
  }
  cat("\n")
  
  # ============================================================
  # 4. SERIE TEMPORAL - Num_Customers (4 lojas sobrepostas)
  # ============================================================
  
  cat("========================================\n")
  cat("4. SERIE TEMPORAL - Num_Customers\n")
  cat("========================================\n\n")
  
  par(mfrow=c(1,1))
  ylim_range = range(sapply(data_list, function(d) range(d$Num_Customers, na.rm=TRUE)))
  plot(data_list[[1]]$Date, data_list[[1]]$Num_Customers,
       type="l", col=cores[1], ylim=ylim_range,
       xlab="Data", ylab="Num_Customers",
       main="Serie Temporal - Num_Customers (4 lojas)")
  for (i in 2:4) {
    lines(data_list[[stores[i]]]$Date, data_list[[stores[i]]]$Num_Customers,
          col=cores[i])
  }
  legend("topleft", legend=toupper(stores), col=cores, lty=1, lwd=2, cex=0.8)
  
  # ============================================================
  # 5. SERIE TEMPORAL - Sales (4 lojas sobrepostas)
  # ============================================================
  
  par(mfrow=c(1,1))
  ylim_range = range(sapply(data_list, function(d) range(d$Sales, na.rm=TRUE)))
  plot(data_list[[1]]$Date, data_list[[1]]$Sales,
       type="l", col=cores[1], ylim=ylim_range,
       xlab="Data", ylab="Sales",
       main="Serie Temporal - Sales (4 lojas)")
  for (i in 2:4) {
    lines(data_list[[stores[i]]]$Date, data_list[[stores[i]]]$Sales,
          col=cores[i])
  }
  legend("topleft", legend=toupper(stores), col=cores, lty=1, lwd=2, cex=0.8)
  
  # ============================================================
  # 6. SERIES TEMPORAIS INDIVIDUAIS - painel 2x2
  # ============================================================
  
  par(mfrow=c(2,2))
  for (i in 1:4) {
    s = stores[i]
    d = data_list[[s]]
    plot(d$Date, d$Num_Customers, type="l", col=cores[i],
         xlab="Data", ylab="Num_Customers",
         main=paste(toupper(s), "- Num_Customers"))
  }
  
  par(mfrow=c(2,2))
  for (i in 1:4) {
    s = stores[i]
    d = data_list[[s]]
    plot(d$Date, d$Sales, type="l", col=cores[i],
         xlab="Data", ylab="Sales",
         main=paste(toupper(s), "- Sales"))
  }
  
  # ============================================================
  # 7. BOXPLOT COMPARATIVO - Num_Customers
  # ============================================================
  
  cat("========================================\n")
  cat("7. BOXPLOT COMPARATIVO\n")
  cat("========================================\n\n")
  
  par(mfrow=c(1,2))
  boxplot(Num_Customers ~ Store, data=all_data,
          main="Num_Customers por Loja",
          col=cores, las=2, xlab="")
  boxplot(Sales ~ Store, data=all_data,
          main="Sales por Loja",
          col=cores, las=2, xlab="")
  
  # ============================================================
  # 8. DIA DA SEMANA COMPARATIVO
  # ============================================================
  
  cat("========================================\n")
  cat("8. DIA DA SEMANA - Num_Customers\n")
  cat("========================================\n")
  
  day_means = sapply(data_list, function(d)
    tapply(d$Num_Customers, d$DayOfWeek, mean, na.rm=TRUE))
  print(round(day_means, 1))
  cat("\n")
  
  par(mfrow=c(2,2))
  for (i in 1:4) {
    s = stores[i]
    d = data_list[[s]]
    boxplot(Num_Customers ~ DayOfWeek, data=d,
            main=paste(toupper(s), "- Clientes/Dia"),
            col=cores[i], las=2, xlab="")
  }
  
  # ============================================================
  # 9. MES COMPARATIVO
  # ============================================================
  
  cat("========================================\n")
  cat("9. MES - Num_Customers\n")
  cat("========================================\n")
  
  month_means = sapply(data_list, function(d)
    tapply(d$Num_Customers, d$Month, mean, na.rm=TRUE))
  print(round(month_means, 1))
  cat("\n")
  
  par(mfrow=c(2,2))
  for (i in 1:4) {
    s = stores[i]
    d = data_list[[s]]
    boxplot(Num_Customers ~ Month, data=d,
            main=paste(toupper(s), "- Clientes/Mes"),
            col=cores[i], las=2, xlab="")
  }
  
  # ============================================================
  # 10. HISTOGRAMAS COMPARATIVOS
  # ============================================================
  
  par(mfrow=c(2,2))
  for (i in 1:4) {
    s = stores[i]
    d = data_list[[s]]
    hist(d$Num_Customers, col=cores[i], breaks=30,
         main=paste(toupper(s), "- Num_Customers"),
         xlab="Clientes")
  }
  
  par(mfrow=c(2,2))
  for (i in 1:4) {
    s = stores[i]
    d = data_list[[s]]
    hist(d$Sales, col=cores[i], breaks=30,
         main=paste(toupper(s), "- Sales"),
         xlab="Vendas")
  }
  
  # ============================================================
  # 11. CORRELACOES COMPARATIVAS
  # ============================================================
  
  cat("========================================\n")
  cat("11. CORRELACOES - Num_Customers vs outros\n")
  cat("========================================\n")
  
  for (s in stores) {
    d   = data_list[[s]]
    nums = d[, c("Num_Customers","Num_Employees","Pct_On_Sale","Sales")]
    cor_m = round(cor(nums, use="complete.obs"), 3)
    cat(toupper(s), "\n")
    cat("  ~ Sales:        ", cor_m["Num_Customers","Sales"], "\n")
    cat("  ~ Num_Employees:", cor_m["Num_Customers","Num_Employees"], "\n")
    cat("  ~ Pct_On_Sale:  ", cor_m["Num_Customers","Pct_On_Sale"], "\n\n")
  }
  
  # ============================================================
  # 12. TOURIST EVENT COMPARATIVO
  # ============================================================
  
  cat("========================================\n")
  cat("12. TOURIST EVENT - Impacto em Num_Customers\n")
  cat("========================================\n")
  
  for (s in stores) {
    d = data_list[[s]]
    te = table(d$TouristEvent)
    media_no  = mean(d$Num_Customers[d$TouristEvent=="No"],  na.rm=TRUE)
    media_yes = mean(d$Num_Customers[d$TouristEvent=="Yes"], na.rm=TRUE)
    cat(sprintf("%-15s | Yes: %3d dias (%.1f%%) | Media sem evento: %6.1f | Media com evento: %6.1f\n",
                toupper(s),
                te["Yes"], te["Yes"]/sum(te)*100,
                media_no, media_yes))
  }
  cat("\n")
  
  par(mfrow=c(2,2))
  for (i in 1:4) {
    s = stores[i]
    d = data_list[[s]]
    boxplot(Num_Customers ~ TouristEvent, data=d,
            main=paste(toupper(s), "- TouristEvent"),
            col=c("gray70", cores[i]))
  }
  
  # ============================================================
  # 13. ACF COMPARATIVA - Num_Customers, Sales, Pct_On_Sale, Num_Employees
  # ============================================================
  
  cat("========================================\n")
  cat("13. ACF - todas as variaveis numericas (4 lojas)\n")
  cat("========================================\n\n")
  
  num_vars_acf = c("Num_Customers", "Sales", "Pct_On_Sale", "Num_Employees")
  
  for (vname in num_vars_acf) {
    par(mfrow=c(2,2))
    for (i in 1:4) {
      s = stores[i]
      d = data_list[[s]]
      if (vname %in% colnames(d)) {
        ts_v = ts(d[[vname]], frequency=7)
        acf(ts_v, lag.max=30,
            main=paste(toupper(s), "-", vname),
            col=cores[i], na.action=na.pass)
      }
    }
  }
  
  # ============================================================
  # 14. VARIAVEIS EXOGENAS - Pct_On_Sale comparativo
  # ============================================================
  
  cat("========================================\n")
  cat("14. Pct_On_Sale COMPARATIVO\n")
  cat("========================================\n")
  
  for (s in stores) {
    d = data_list[[s]]
    cat(sprintf("%-15s | Media: %.2f | Sd: %.2f | Min: %.2f | Max: %.2f | NAs: %d\n",
                toupper(s),
                mean(d$Pct_On_Sale, na.rm=TRUE), sd(d$Pct_On_Sale, na.rm=TRUE),
                min(d$Pct_On_Sale, na.rm=TRUE),  max(d$Pct_On_Sale, na.rm=TRUE),
                sum(is.na(d$Pct_On_Sale))))
  }
  cat("\n")
  
  par(mfrow=c(1,1))
  ylim_pct = range(sapply(data_list, function(d) range(d$Pct_On_Sale, na.rm=TRUE)))
  plot(data_list[[1]]$Date, data_list[[1]]$Pct_On_Sale,
       type="l", col=cores[1], ylim=ylim_pct,
       xlab="Data", ylab="Pct_On_Sale",
       main="Serie Temporal - Pct_On_Sale (4 lojas)")
  for (i in 2:4) {
    lines(data_list[[stores[i]]]$Date, data_list[[stores[i]]]$Pct_On_Sale,
          col=cores[i])
  }
  legend("topleft", legend=toupper(stores), col=cores, lty=1, lwd=2, cex=0.8)
  
  # ============================================================
  # 15. HEATMAP DE CORRELACAO - por loja (todas as variaveis)
  # ============================================================
  
  cat("========================================\n")
  cat("15. HEATMAP DE CORRELACAO - por loja\n")
  cat("========================================\n")
  
  num_vars = c("Num_Customers","Sales","Num_Employees","Pct_On_Sale")
  cor_pal  = colorRampPalette(c("tomato","white","steelblue"))(50)
  
  par(mfrow=c(2,2))
  for (i in 1:4) {
    s     = stores[i]
    d     = data_list[[s]]
    cor_m = cor(d[, num_vars], use="complete.obs")
    nv    = length(num_vars)
    
    cat(toupper(s), "\n")
    print(round(cor_m, 3))
    cat("\n")
    
    image(1:nv, 1:nv,
          t(cor_m[nrow(cor_m):1, ]),
          col=cor_pal, zlim=c(-1,1),
          axes=FALSE,
          main=paste(toupper(s), "- Correlacoes"),
          xlab="", ylab="")
    axis(1, at=1:nv, labels=num_vars, las=2, cex.axis=0.75)
    axis(2, at=1:nv, labels=rev(num_vars), las=2, cex.axis=0.75)
    for (r in 1:nv)
      for (cc in 1:nv)
        text(cc, nv+1-r, round(cor_m[r,cc],2),
             cex=0.85, font=ifelse(abs(cor_m[r,cc])>0.5,2,1))
  }
  
  # ============================================================
  # 16. DISTANCIA COMPORTAMENTAL ENTRE LOJAS
  #     Baseada em: medias mensais + dia-da-semana de Num_Customers
  #     (perfil temporal normalizado -> distancia euclidiana)
  # ============================================================
  
  cat("========================================\n")
  cat("16. DISTANCIA COMPORTAMENTAL ENTRE LOJAS\n")
  cat("========================================\n")
  
  # -- periodo comum para comparacao justa
  dates_common = Reduce(intersect, lapply(data_list, function(d) as.character(d$Date)))
  cat("Periodo comum:", length(dates_common), "dias\n\n")
  
  aligned = lapply(data_list, function(d) {
    d[as.character(d$Date) %in% dates_common, ]
  })
  
  # perfil = vetor com medias por dia-da-semana (7) + medias por mes (12)
  # normalizado com scale() para remover diferenca de escala entre lojas
  get_profile = function(d) {
    dow = tapply(d$Num_Customers, d$DayOfWeek, mean, na.rm=TRUE)
    mon = tapply(d$Num_Customers, d$Month,     mean, na.rm=TRUE)
    v   = c(dow, mon)
    as.numeric(scale(v))  # z-score para remover efeito de escala absoluta
  }
  
  profiles = sapply(aligned, get_profile)   # matriz: (19 features) x (4 lojas)
  
  # --- matriz de distancia euclidiana (comportamento temporal)
  dist_mat = as.matrix(dist(t(profiles), method="euclidean"))
  rownames(dist_mat) = colnames(dist_mat) = toupper(stores)
  
  cat("Distancia euclidiana (perfil semanal + mensal normalizado):\n")
  print(round(dist_mat, 3))
  cat("\n")
  
  # --- matriz de correlacao entre series (periodo comum)
  aligned_nc = sapply(aligned, function(d) d$Num_Customers)
  cor_stores  = cor(aligned_nc, use="complete.obs")
  colnames(cor_stores) = rownames(cor_stores) = toupper(stores)
  
  cat("Correlacao entre series Num_Customers (periodo comum):\n")
  print(round(cor_stores, 3))
  cat("\n")
  
  # --- heatmap: distancia comportamental
  par(mfrow=c(1,2))
  
  dist_norm = dist_mat / max(dist_mat)  # normaliza 0-1 para visualizacao
  image(1:4, 1:4,
        t(dist_norm[4:1, ]),
        col=colorRampPalette(c("steelblue","white","tomato"))(50),
        zlim=c(0,1), axes=FALSE,
        main="Distancia Comportamental\n(perfil dia+mes normalizado)",
        xlab="", ylab="")
  axis(1, at=1:4, labels=toupper(stores), las=2, cex.axis=0.8)
  axis(2, at=1:4, labels=rev(toupper(stores)), las=2, cex.axis=0.8)
  for (r in 1:4)
    for (cc in 1:4)
      text(cc, 5-r, round(dist_mat[r,cc],2), cex=0.85,
           font=ifelse(dist_mat[r,cc] < quantile(dist_mat[dist_mat>0],0.33),2,1))
  
  # --- heatmap: correlacao entre series
  image(1:4, 1:4,
        t(cor_stores[4:1, ]),
        col=colorRampPalette(c("tomato","white","steelblue"))(50),
        zlim=c(-1,1), axes=FALSE,
        main="Correlacao entre Series\nNum_Customers (periodo comum)",
        xlab="", ylab="")
  axis(1, at=1:4, labels=toupper(stores), las=2, cex.axis=0.8)
  axis(2, at=1:4, labels=rev(toupper(stores)), las=2, cex.axis=0.8)
  for (r in 1:4)
    for (cc in 1:4)
      text(cc, 5-r, round(cor_stores[r,cc],2), cex=0.85,
           font=ifelse(abs(cor_stores[r,cc])>0.7,2,1))
  
  # ============================================================
  # 17. CCF CRUZADA ENTRE LOJAS - Num_Customers
  # ============================================================
  
  cat("========================================\n")
  cat("17. CCF CRUZADA - Num_Customers entre lojas\n")
  cat("========================================\n\n")
  
  pares = list(
    c("baltimore",    "philadelphia"),
    c("baltimore",    "lancaster"),
    c("philadelphia", "lancaster"),
    c("richmond",     "philadelphia")
  )
  
  par(mfrow=c(2,2))
  for (p in pares) {
    s1 = p[1]; s2 = p[2]
    x1 = aligned[[s1]]$Num_Customers
    x2 = aligned[[s2]]$Num_Customers
    ccf_res = ccf(x1, x2, lag.max=21, plot=FALSE)
    
    lag0_idx = which(ccf_res$lag == 0)
    cor0     = round(ccf_res$acf[lag0_idx], 3)
    cat(sprintf("%-13s vs %-13s | r(lag=0): %+.3f\n",
                toupper(s1), toupper(s2), cor0))
    
    plot(ccf_res,
         main=paste(toupper(s1), "vs", toupper(s2)),
         col=ifelse(abs(cor0) > 0.5, "seagreen","tomato"),
         ylim=c(-1,1))
    abline(h=0, lty=2, col="gray50")
  }
  cat("\n")
  
  # ============================================================
  # 18. RESUMO FINAL
  # ============================================================
  
  cat("============================================================\n")
  cat("18. RESUMO FINAL COMPARATIVO\n")
  cat("============================================================\n")
  cat(sprintf("%-15s | %-8s | %-8s | %-8s | %-8s | %-6s\n",
              "LOJA", "Med.Cust", "Sd.Cust", "Med.Sales", "Sd.Sales", "NA.tot"))
  for (s in stores) {
    d = data_list[[s]]
    cat(sprintf("%-15s | %8.1f | %8.1f | %8.0f | %8.0f | %6d\n",
                toupper(s),
                mean(d$Num_Customers, na.rm=TRUE), sd(d$Num_Customers, na.rm=TRUE),
                mean(d$Sales, na.rm=TRUE),         sd(d$Sales, na.rm=TRUE),
                sum(colSums(is.na(d)))))
  }
  cat("============================================================\n\n")
  
}, error = function(e) {
  cat("\n>> ERRO:", conditionMessage(e), "\n")
}, finally = {
  dev.off()
  sink()
})

cat("\n>> EDA comparativa concluida!\n")
cat(">> Log:     ", log_file, "\n")
cat(">> Graficos:", pdf_file, "\n")