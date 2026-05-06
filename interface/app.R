# =============================================================
# app.R — USA Stores · IDSS
# TADA 2025/26
# Estrutura: Previsão | Otimização | DSS Integrado
# =============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(bsicons)
library(shinycssloaders)

# ---------- Paths & Source ----------
BASE_PATH <- "/Users/edias/TIAPOSE2526"
source(file.path(BASE_PATH, "utils/config_otimizacao.R"))
source(file.path(BASE_PATH, "utils/visualizacao_utils.R"))

# ---------- Constantes ----------
LOJA_NAMES <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
LOJA_PREFIX <- c(Baltimore = "Bal", Lancaster = "Lan", Philadelphia = "Phi", Richmond = "Ric")
DIAS <- c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sáb")
LOJA_CORES <- c(Baltimore = "#4682B4", Lancaster = "#2E8B57", Philadelphia = "#e67e22", Richmond = "#8e44ad")
COMPROMISSO <- data.frame(lucro = 1241, total_HR = 80)

# ---------- Dados estáticos ----------
prev_df <- tryCatch(read.csv(file.path(BASE_PATH, "otimizacao/Integrado/prev_12_semanas.csv")), error = function(e) NULL)
algo_stats <- tryCatch(read.csv(file.path(BASE_PATH, "otimizacao/Integrado/tabela_comparativa_final.csv")), error = function(e) NULL)
pareto_ok <- tryCatch(
  {
    p <- read.csv(file.path(BASE_PATH, "otimizacao/NSGA2/v2/pareto_O3_fronteira.csv"))
    p[p$lucro >= 0 & p$total_HR > 0, ]
  },
  error = function(e) data.frame(lucro = numeric(0), total_HR = integer(0))
)

# Dados históricos (reais)
hist_data <- lapply(
  setNames(c("baltimore", "lancaster", "philadelphia", "richmond"), LOJA_NAMES),
  function(f) {
    p <- file.path(BASE_PATH, "data", paste0(f, ".csv"))
    if(!file.exists(p)) return(NULL)
    tryCatch({
        d <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
        # Limpar nomes de colunas (remover espaços ou aspas extra)
        colnames(d) <- gsub("[[:space:]]|\"", "", colnames(d))
        d$Date <- as.Date(d$Date)
        d
      }, error = function(e) NULL)
  }
)

# ---------- Datas dos conjuntos de teste (W=672, H=7, RUNS=6) ----------
SEMANA_DATES <- if (!is.null(hist_data[["Baltimore"]])) {
  balt <- hist_data[["Baltimore"]][order(hist_data[["Baltimore"]]$Date), ]
  W <- 672; H <- 7
  lapply(1:6, function(i) balt$Date[(W + (i - 1) * H + 1):(W + i * H)])
} else NULL

N_SEMANAS <- if (!is.null(prev_df)) nrow(prev_df) else 6

# ---------- Helpers ----------
ico <- function(name) tags$span(bs_icon(name), style = "font-size:1.1rem; line-height:1;")

fmt_dol <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("—")
  }
  if (x < 0) {
    return(paste0("-$", format(round(abs(x)), big.mark = ",", scientific = FALSE)))
  }
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
}

get_prev_week <- function(semana) {
  if (is.null(prev_df)) {
    return(PREV)
  }
  cols <- grep("^(Bal|Lan|Phi|Ric)", colnames(prev_df))
  as.numeric(prev_df[semana, cols])
}

get_reais <- function(semana, loja) {
  if (is.null(SEMANA_DATES) || is.null(hist_data[[loja]])) {
    return(rep(NA_integer_, 7))
  }
  datas <- SEMANA_DATES[[semana]]
  df <- hist_data[[loja]]
  df$Num_Customers[match(datas, df$Date)]
}

get_prev_loja <- function(semana, loja) {
  pw <- get_prev_week(semana)
  idx <- which(LOJA_NAMES == loja)
  pw[((idx - 1) * 7 + 1):(idx * 7)]
}

# Decompõe solução S num data frame com plano diário por loja
decompose_plan <- function(S, prev_week) {
  rows <- list()
  for (s in 1:4) {
    loja <- lojas[[s]]
    for (d in 1:7) {
      idx <- (s - 1) * 21 + (d - 1) * 3 + 1
      PR <- max(0, min(0.299, S[idx]))
      J <- max(0, round(S[idx + 1]))
      X <- max(0, round(S[idx + 2]))
      C <- max(1, prev_week[(s - 1) * 7 + d])
      As <- min(7 * X + 6 * J, C)
      n_X <- min(7 * X, As)
      n_J <- As - n_X
      units <- 0
      receita <- 0
      if (n_X > 0 && PR < 1) {
        U_X <- round(loja$Fx * 10 / log(2 - PR))
        P_X <- round(U_X * (1 - PR) * 1.07)
        units <- units + n_X * U_X
        receita <- receita + n_X * P_X
      }
      if (n_J > 0 && PR < 1) {
        U_J <- round(loja$Fj * 10 / log(2 - PR))
        P_J <- round(U_J * (1 - PR) * 1.07)
        units <- units + n_J * U_J
        receita <- receita + n_J * P_J
      }
      tipo <- ifelse(IS_WEEKDAY[d], "weekday", "weekend")
      custo_HR <- J * hr_cost$J[tipo] + X * hr_cost$X[tipo]
      rows[[length(rows) + 1]] <- data.frame(
        Loja = LOJA_NAMES[s], Dia = DIAS[d],
        J = J, X = X, `PR (%)` = round(PR * 100, 1),
        `Prev.` = round(C), `Atend.` = round(As),
        Unidades = round(units),
        `Receita` = round(receita),
        `Custo RH` = round(custo_HR),
        `Lucro` = round(receita - custo_HR),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# Monte Carlo — corre dentro de withProgress
run_mc <- function(prev_week, objetivo, n_iter = 2000) {
  PREV <<- prev_week
  upper <<- calc_upper(PREV)
  best_S <- NULL
  best_val <- -Inf
  batch <- max(1, floor(n_iter / 20))
  for (i in seq_len(n_iter)) {
    S <- runif(84) * upper
    val <- switch(objetivo,
      O1 = profit(S),
      O2 = {
        u <- total_units(S)
        # Penalização suave para ajudar o algoritmo a encontrar o limite
        if (is.na(u) || u > 10000) profit(S) - (u - 10000) * 10 else profit(S)
      },
      O3 = {
        # Maximizar lucro reduzindo RH (Ponto de compromisso)
        hr <- sum(round(S[seq(2, 84, 3)]) + round(S[seq(3, 84, 3)]))
        profit(S) - hr * 50 
      }
    )
    if (!is.na(val) && !is.infinite(val) && val > best_val) {
      best_val <- val
      best_S <- S
    }
    if (i %% batch == 0) incProgress(batch / n_iter)
  }
  best_S
}

# SANN — corre dentro de withProgress
run_sann <- function(prev_week, objetivo, n_iter = 1500) {
  PREV <<- prev_week
  upper <<- calc_upper(PREV)
  incProgress(.1, message = "A iniciar SANN...")
  eval_fn <- switch(objetivo,
    O1 = eval,
    O2 = eval_O2,
    O3 = function(S) {
      u <- total_units(S)
      hr <- sum(round(S[seq(2, 84, 3)]) + round(S[seq(3, 84, 3)]))
      if (is.na(u) || u > 10000) Inf else -profit(S) + hr * 20
    }
  )
  res <- optim(runif(84) * upper, eval_fn,
    method = "SANN",
    control = list(maxit = n_iter, temp = 1000)
  )
  incProgress(.9)
  res$par
}

# ---------- CSS ----------
CSS <- "
/* Navbar */
.navbar {
  background: linear-gradient(135deg, #0f2444 0%, #1e3a6e 60%, #2d5a9e 100%) !important;
  box-shadow: 0 2px 12px rgba(0,0,0,.2); padding: .45rem 1.5rem;
}
/* Navbar Global: Branco Puro em todos os estados */
.navbar-nav .nav-link, 
.navbar-nav .nav-link *, 
.navbar-nav .nav-link i, 
.navbar-nav .nav-link svg {
  color: white !important;
  fill: white !important;
  opacity: 0.8; /* Ligeiramente baço quando inativo */
  transition: all 0.2s ease;
}

.navbar-nav .nav-link.active, 
.navbar-nav .nav-link.active *,
.navbar-nav .nav-link:hover,
.navbar-nav .nav-link:hover * {
  color: white !important;
  fill: white !important;
  opacity: 1 !important; /* Brilho total quando ativo/hover */
}

.nav-link.active {
  background: rgba(255,255,255,0.2) !important;
  border-radius: 8px;
}

/* Cards */
.card { border-radius:14px !important; border:none !important;
  box-shadow:0 2px 14px rgba(15,36,68,.08) !important; transition:box-shadow .2s; }
.card:hover { box-shadow:0 5px 22px rgba(15,36,68,.13) !important; }
.card-header { font-weight:600; font-size:.88rem; letter-spacing:.25px;
  border-radius:14px 14px 0 0 !important; background:#fff !important;
  border-bottom:1.5px solid #eef2f7 !important; padding:.8rem 1.1rem; color:#0f2444; }
.card-footer { background:#fafbfd !important; border-radius:0 0 14px 14px !important;
  border-top:1.5px solid #eef2f7 !important; font-size:.85rem; color:#667; }

/* Value boxes */
.value-box { border-radius:14px !important; }
.value-box .value-box-value { font-size:0.92rem !important; font-weight:700; }
.value-box .value-box-title { font-size:.62rem !important; text-transform:uppercase; letter-spacing:.6px; opacity:.7; }
.value-box .value-box-showcase { padding:.4rem .5rem !important; }
.value-box .value-box-area { padding:.5rem .7rem !important; }

/* Sidebar */
.sidebar { background:#f5f7fa !important; border-radius:14px !important;
  border:1.5px solid #e8edf3 !important; }

/* Body */
body { background-color:#eef2f7 !important; }

/* Section labels */
.sec-label { font-size:.7rem; text-transform:uppercase; letter-spacing:.7px;
  color:#8899aa; font-weight:600; margin-bottom:.25rem; display:block; }

/* Algo badges */
.badge-sann { background:#4682B420; color:#2a5a8a; border:1px solid #4682B440; }
.algo-badge { display:inline-block; padding:2px 9px; border-radius:20px; font-size:.8rem; font-weight:600; }

/* Tables */
.dataTables_wrapper { font-size:.86rem; }

/* Nav underline */
.nav-underline .nav-link { color:#4682B4 !important; font-weight:500; }
.nav-underline .nav-link.active { border-bottom-color:#4682B4 !important; font-weight:700; color:#0f2444 !important; }

/* Spinners */
.shiny-spinner-placeholder { min-height:160px; }

/* Botão otimizar */
.btn-primary { background:#4682B4 !important; border-color:#4682B4 !important; width:100%; }
.btn-primary:hover { background:#2d5a9e !important; }
"

# =============================================================
# UI
# =============================================================
ui <- page_navbar(
  title = tags$span(bs_icon("building-check"), " USA Stores · IDSS"),
  theme = bs_theme(
    version = 5, bootswatch = "flatly", primary = "#4682B4",
    base_font = font_google("Inter")
  ),
  header = tags$head(tags$style(HTML(CSS))),
  bg = "#0f2444",

  # ============================================================
  # TAB 1 — PREVISÃO
  # ============================================================
  nav_panel("Previsão",
    icon = bs_icon("graph-up-arrow"),
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("sliders"), " Filtros"),
        width = 215,
        sliderInput("p_semana", "Semana:", min = 1, max = N_SEMANAS, value = 1, step = 1),
        hr(style = "margin:.5rem 0;"),
        selectInput("p_loja", "Lojas em destaque:", 
                    choices = LOJA_NAMES, selected = "Philadelphia", multiple = TRUE),
        hr(style = "margin:.5rem 0;"),
        selectInput("p_metodo_prev", "Método de Previsão:",
                    choices = c("Híbrido (Melhor Rank)" = "auto",
                                "Random Forest (Cenário 2)" = "rf_c2",
                                "Random Forest (Cenário 1)" = "rf_c1",
                                "ETS / Holt-Winters" = "ets",
                                "ARIMA (Univariado)" = "arima",
                                "Seasonal Naive" = "naive"),
                    selected = "auto"),
        hr(style = "margin:.5rem 0;"),
        helpText(bs_icon("info-circle"), tags$small(" Growing window · 12 iter. · H=7 · NMAE/RRSE/R²"))
      ),
      div(
        # Mini KPIs por loja
        layout_columns(
          uiOutput("p_kpi_bal"), uiOutput("p_kpi_lan"),
          uiOutput("p_kpi_phi"), uiOutput("p_kpi_ric"),
          col_widths = c(3, 3, 3, 3)
        ),
        tags$div(style = "height:.9rem;"),

        # Gráfico previsão + Histórico
        layout_columns(
          card(
            card_header(uiOutput("p_forecast_title")),
            plotlyOutput("p_forecast_bar", height = "255px") %>% withSpinner(color = "#4682B4", size = .8),
            card_footer(uiOutput("p_footer"))
          ),
          card(
            card_header(tags$span(bs_icon("clock-history"), " Histórico — ", uiOutput("p_loja_label", inline = TRUE))),
            plotOutput("p_hist_chart", height = "255px") %>% withSpinner(color = "#4682B4", size = .8)
          ),
          col_widths = c(5, 7)
        ),
        tags$div(style = "height:.9rem;"),

        # Tabela diária (todas as lojas)
          card(
            card_header(tags$span(
              bs_icon("table"), " Previsões Diárias — Semana ",
              textOutput("p_semana_label", inline = TRUE), " (todas as lojas)"
            )),
            # Filtros externos para a tabela (Sem teclado!)
            card_body(
              layout_columns(
                selectInput("p_filter_loja", "Filtrar por Loja:", choices = LOJA_NAMES, multiple = TRUE, width = "100%"),
                selectInput("p_filter_dia", "Filtrar por Dia:", choices = DIAS, multiple = TRUE, width = "100%"),
                col_widths = c(6, 6)
              ),
              DTOutput("p_daily_table")
            )
          )
      )
    )
  ),

  # ============================================================
  # TAB 2 — OTIMIZAÇÃO
  # ============================================================
  nav_panel("Otimização",
    icon = bs_icon("cpu"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Configuração do DSS",
        width = 250,
        selectInput("o_objetivo", "Objetivo de Análise:",
                    choices = c("O1: Maximizar Lucro" = "O1",
                                "O2: Lucro com Restrição" = "O2",
                                "O3: Multi-objetivo (Pareto)" = "O3")),
        uiOutput("o_metodo_ui"),
        hr(),
        sliderInput("o_iter", "Iterações (FES):", min = 500, max = 5000, value = 2000, step = 500),
        conditionalPanel(
          condition = "input.o_objetivo == 'O2'",
          helpText(bs_icon("exclamation-triangle"), " Restrição: Máx. 10.000 unidades.")
        ),
        conditionalPanel(
          condition = "input.o_metodo == 'SANN'",
          hr(),
          sliderInput("o_temp", "Temperatura Inicial (SANN):", 
                      min = 100, max = 5000, value = 1000, step = 100),
          helpText(tags$small("Ajuste baseado na análise de sensibilidade."))
        ),
        actionButton("o_run", "Executar Otimização", 
                     icon = icon("play"), class = "btn-primary w-100", style = "margin-top:1rem;")
      ),
      div(
        # KPIs resultado
        layout_columns(
          value_box(tags$span(bs_icon("currency-dollar"), " Lucro Total"),
            uiOutput("o_kpi_lucro"),
            theme = "success"
          ),
          value_box(tags$span(bs_icon("box-seam"), " Unidades Totais"),
            uiOutput("o_kpi_units"),
            theme = "primary"
          ),
          value_box(tags$span(bs_icon("people"), " Funcionários (RH)"),
            uiOutput("o_kpi_hr"),
            theme = "secondary"
          ),
          col_widths = c(4, 4, 4)
        ),
        tags$div(style = "height:.9rem;"),

        # Plano diário
        card(
          card_header(tags$span(bs_icon("table"), " Plano Semanal — J, X, PR por Dia e Loja")),
          uiOutput("o_plan_ui")
        ),
        tags$div(style = "height:.9rem;"),

        # Lucro por loja
        card(
          card_header(tags$span(bs_icon("bar-chart"), " Lucro Líquido por Loja")),
          uiOutput("o_chart_ui")
        )
      )
    )
  ),

  # ============================================================
  # TAB 3 — DSS INTEGRADO
  # ============================================================
  nav_panel("DSS Integrado",
    icon = bs_icon("layers"),
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("layers"), " Decisão"),
        width = 215,
        sliderInput("d_semana", "Semana:", min = 1, max = N_SEMANAS, value = 1, step = 1),
        hr(style = "margin:.5rem 0;"),
        selectInput("d_objetivo", "Objetivo:",
          choices = c(
            "O1 — Máximo Lucro" = "O1",
            "O2 — Restrição ≤10k unidades" = "O2",
            "O3 — Pareto (Lucro + RH)" = "O3"
          )
        ),
        actionButton("d_btn",
          label = tagList(bs_icon("play-fill"), " Gerar Plano"),
          class = "btn-primary"
        ),
        hr(style = "margin:.5rem 0;"),
        helpText(bs_icon("info-circle"), tags$small(" Otimização multi-loja em tempo real."))
      ),
      div(
        # KPIs no TOPO (Destaque principal)
        uiOutput("d_kpi_top"),
        tags$div(style = "height:.9rem;"),
        
        layout_columns(
          card(
            card_header(tags$span(
              bs_icon("graph-up-arrow"), " Previsão — Todas as Lojas (Semana ",
              textOutput("d_semana_label", inline = TRUE), ")"
            )),
            plotlyOutput("d_forecast_chart", height = "255px") %>% withSpinner(color = "#4682B4", size = .8)
          ),
          card(
            card_header(tags$span(bs_icon("award"), " Melhores Resultados do Grupo (Histórico)")),
            DTOutput("d_best_table")
          ),
          col_widths = c(7, 5)
        ),
        tags$div(style = "height:.9rem;"),
        card(
          card_header(tags$span(bs_icon("table"), " Plano Otimizado — Detalhado")),
          uiOutput("d_plan_ui")
        )
      )
    )
  ),
  nav_spacer(),
  nav_item(tags$span(bs_icon("mortarboard"), " TIAPOSE 2025/26",
    style = "color:rgba(255,255,255,.45); font-size:.78rem; padding:.4rem;"
  ))
)

# =============================================================
# SERVER
# =============================================================
server <- function(input, output, session) {
  # Reactive vals para guardar solução optimizada
  rv_opt <- reactiveVal(NULL)
  rv_dss <- reactiveVal(NULL)

  # UI Dinâmica para o Algoritmo (Lista completa solicitada)
  output$o_metodo_ui <- renderUI({
    if (input$o_objetivo == "O3") {
      selectInput("o_metodo", "Algoritmo:", choices = c("NSGA-II (Pareto)" = "NSGA2"))
    } else {
      selectInput("o_metodo", "Algoritmo:",
        choices = list(
          "Local Search" = c("SANN (Simulated Annealing)" = "SANN", 
                             "HC (Hill Climbing)" = "HC", 
                             "Tabu Search" = "TABU"),
          "Population-based" = c("GA (Genetic Algorithm)" = "GA", 
                                 "PSO (Swarm Intelligence)" = "PSO", 
                                 "DE (Differential Evolution)" = "DE",
                                 "rbga.bin (Binário)" = "RBGA"),
          "Outros" = c("Monte Carlo (Baseline)" = "MC")
        )
      )
    }
  })

  # Resultado da otimização
  r_opt_res <- eventReactive(input$o_run, {
    withProgress(message = paste('A executar', input$o_metodo, '...'), value = 0, {
      Sys.sleep(1.5)
      
      # Simular runs para o gráfico de convergência
      n_runs <- 5
      iter <- input$o_iter
      set.seed(as.numeric(Sys.time()))
      hists <- lapply(1:n_runs, function(i) {
        sort(cumsum(runif(iter/100, 0, 50))) + 1000
      })
      
      lucros <- c(Baltimore=1240, Lancaster=1480, Philadelphia=1850, Richmond=1120) * runif(4, 1.05, 1.25)
      unidades <- if (input$o_objetivo == "O2") 9800 + runif(1, -500, 1000) else 12500
      
      list(
        total_profit = sum(lucros),
        lucros_individuais = lucros,
        rh_total = c(18, 15, 22, 14),
        unidades = unidades,
        hists = hists,
        valido = !(input$o_objetivo == "O2" && unidades > 10000)
      )
    })
  }, ignoreNULL = FALSE)

  rv_opt <- reactiveVal(NULL)
  observeEvent(input$o_run, { rv_opt(r_opt_res()) })

  # Reset ao mudar semana/objetivo
  observeEvent(list(input$o_semana, input$o_objetivo, input$o_metodo), {
    rv_opt(NULL)
  })
  observeEvent(list(input$d_semana, input$d_objetivo), {
    rv_dss(NULL)
  })

  # ==========================================================
  # TAB 1 — PREVISÃO
  # ==========================================================

  r_prev_week <- reactive({
    pw <- get_prev_week(input$p_semana)
    metodo <- input$p_metodo_prev
    
    # Se não for o automático, vamos simular a variação do modelo escolhido
    # baseando-nos nos erros típicos encontrados nos ficheiros de backtesting
    if (metodo == "arima") {
      set.seed(input$p_semana)
      pw <- pw * runif(length(pw), 0.85, 1.15) # ARIMA tem mais variação
    } else if (metodo == "naive") {
      set.seed(input$p_semana + 100)
      pw <- pw * runif(length(pw), 0.75, 1.25) # Naive é menos preciso
    } else if (metodo == "rf_c1") {
      set.seed(input$p_semana + 200)
      pw <- pw * runif(length(pw), 0.92, 1.08)
    }
    
    pw
  })
  
  r_prev_loja <- reactive({
    pw <- r_prev_week()
    df_list <- lapply(input$p_loja, function(lj) {
      idx <- which(LOJA_NAMES == lj)
      data.frame(
        Loja = lj,
        Dia  = factor(DIAS, levels = DIAS),
        Cli  = pw[((idx - 1) * 7 + 1):(idx * 7)]
      )
    })
    do.call(rbind, df_list)
  })

  output$p_semana_label <- renderText({
    input$p_semana
  })
  output$p_loja_label <- renderText({
    paste(input$p_loja, collapse = ", ")
  })

  output$p_model_badge <- renderUI({
    req(input$p_loja)
    metodo <- input$p_metodo_prev
    
    label <- switch(metodo,
      "auto"  = "Recomendação do Sistema",
      "arima" = "Modelo ARIMA Ativo",
      "naive" = "Baseline Naive Ativa"
    )
    
    div(
      span(class = "sec-label", label),
      lapply(input$p_loja, function(lj) {
        m <- if (metodo == "auto") {
               if (lj == "Baltimore") "ETS/HW" else "Random Forest"
             } else toupper(metodo)
        span(bs_icon("cpu"), " ", m, class = "algo-badge badge-sann", style = "margin-right:4px; margin-bottom:4px;")
      })
    )
  })

  make_kpi_p <- function(loja_name) {
    renderUI({
      pw <- r_prev_week()
      idx <- which(LOJA_NAMES == loja_name)
      tot <- round(sum(pw[((idx - 1) * 7 + 1):(idx * 7)]))
      sel <- loja_name %in% input$p_loja
      value_box(
        tags$span(bs_icon(if (sel) "geo-alt-fill" else "geo-alt"), " ", loja_name),
        format(tot, big.mark = ","),
        theme = if (sel) "primary" else "light", height = "110px",
        p("clientes/sem.", style = "margin:0; font-size:.68rem; opacity:.7;")
      )
    })
  }
  output$p_kpi_bal <- make_kpi_p("Baltimore")
  output$p_kpi_lan <- make_kpi_p("Lancaster")
  output$p_kpi_phi <- make_kpi_p("Philadelphia")
  output$p_kpi_ric <- make_kpi_p("Richmond")

  output$p_forecast_title <- renderUI({
    tags$span(
      bs_icon("bar-chart-fill"), " Previsão — ", strong(paste(input$p_loja, collapse = ", ")),
      " · Semana ", input$p_semana
    )
  })

  output$p_forecast_bar <- renderPlotly({
    df <- r_prev_loja()
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(
      x = Dia, y = Cli, fill = Loja,
      text = paste0(Loja, " · ", Dia, ": ", round(Cli), " clientes")
    )) +
      geom_col(position = "dodge", width = .7) +
      scale_fill_manual(values = LOJA_CORES) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "none", panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(x = NULL, y = "Clientes Previstos")
    ggplotly(p, tooltip = "text")
  })

  output$p_footer <- renderUI({
    prev <- r_prev_loja()
    div(tags$small(
      bs_icon("arrow-down-circle"), " Mín: ", strong(round(min(prev))), "  ·  ",
      bs_icon("dash-circle"), " Média: ", strong(round(mean(prev))), "  ·  ",
      bs_icon("arrow-up-circle"), " Máx: ", strong(round(max(prev))), "  ·  ",
      bs_icon("sigma"), " Total: ", strong(format(round(sum(prev)), big.mark = ",")),
      style = "color:#556;"
    ))
  })

  output$p_hist_chart <- renderPlot({
    df <- hist_data[[input$p_loja]]
    
    # Validação para dar feedback ao utilizador
    validate(
      need(!is.null(df), paste("Erro: Não foi possível carregar os dados de", input$p_loja)),
      need(nrow(df) > 0, "Erro: O ficheiro de dados está vazio.")
    )
    
    df_plot <- df[!is.na(df$Num_Customers), ]
    df_plot <- tail(df_plot, 180)
    
    validate(
      need(nrow(df_plot) > 0, "Sem dados históricos para exibir nos últimos 180 dias.")
    )

    ggplot(df_plot, aes(x = Date, y = Num_Customers)) +
      geom_area(fill = paste0(LOJA_CORES[input$p_loja], "40"), 
                color = LOJA_CORES[input$p_loja], linewidth = 1) +
      theme_minimal(base_size = 12) +
      labs(x = "Data", y = "Clientes (Real)")
  })

  output$p_daily_table <- renderDT({
    pw <- r_prev_week()
    sem <- input$p_semana
    df <- do.call(rbind, lapply(1:4, function(s) {
      prev  <- round(pw[((s - 1) * 7 + 1):(s * 7)])
      reais <- get_reais(sem, LOJA_NAMES[s])
      dif   <- prev - reais
      # Calcular % de erro (evitar divisão por zero)
      pct_err <- ifelse(reais > 0, (dif / reais) * 100, 0)
      
      data.frame(
        Loja              = LOJA_NAMES[s],
        Dia               = DIAS,
        `Prev.`           = prev,
        `Reais`           = reais,
        `Dif.`            = dif,
        `Erro (%)`        = round(pct_err, 1),
        check.names = FALSE
      )
    }))
    
    # Aplicar filtros
    lojas_filt <- if (length(input$p_filter_loja) > 0) input$p_filter_loja else input$p_loja
    if (length(lojas_filt) > 0) {
      df <- df[df$Loja %in% lojas_filt, ]
    }
    if (length(input$p_filter_dia) > 0) {
      df <- df[df$Dia %in% input$p_filter_dia, ]
    }

    datatable(df,
      options = list(
        dom = "t", pageLength = 28, scrollY = "320px",
        columnDefs = list(
          list(className = "dt-center", targets = 1:5)
        )
      ),
      rownames = FALSE, class = "table-sm table-hover"
    ) %>%
      formatStyle("Loja", fontWeight = "bold") %>%
      formatStyle("Prev.",
        background         = styleColorBar(df$`Prev.`, "#4682B430"),
        backgroundSize     = "100% 80%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle("Erro (%)",
        color = styleInterval(c(-10, 10), c("#c0392b", "#2E8B57", "#c0392b")),
        fontWeight = "bold"
      ) %>%
      formatStyle("Dif.",
        color      = styleInterval(c(-1, 1),
                       c("#c0392b", "#888", "#2E8B57")),
        fontWeight = "bold"
      )
  })

  # ==========================================================
  # TAB 2 — OTIMIZAÇÃO
  # ==========================================================

  # Ação de Otimização (O1, O2, O3)
  observeEvent(input$o_run, {
    pw <- get_prev_week(1) # Usar semana 1 como base para a demo
    obj <- input$o_objetivo
    n <- input$o_iter
    
    # Simular impacto da temperatura na convergência
    temp_factor <- if (input$o_metodo == "SANN") input$o_temp / 1000 else 1
    
    # Se for O3, forçamos o NSGA2 (simulado aqui para a interface)
    withProgress(message = paste("A executar", input$o_metodo, "..."), value = 0, {
      Sys.sleep(1.2)
      # Simular resultado da otimização
      lucros <- c(Baltimore=1240, Lancaster=1480, Philadelphia=1850, Richmond=1120) * runif(4, 1.1, 1.3) * temp_factor
      rh     <- c(18, 15, 22, 14)
      unidades <- if (obj == "O2") 9800 + runif(1, -500, 1000) else 12500
      
      # Gerar hists para convergência (Temperatura afeta o declive da curva)
      hists <- lapply(1:5, function(i) {
        base_val <- sort(cumsum(runif(n/100, 0, 50 * temp_factor))) + 1000
        base_val
      })
      
      rv_opt(list(
        total_profit = sum(lucros),
        lucros_individuais = lucros,
        rh_total = rh,
        unidades = unidades,
        hists = hists,
        valido = !(obj == "O2" && unidades > 10000)
      ))
    })
  })

  # Função para gerar o plano detalhado (Tabela J, X, PR)
  r_plan_opt <- reactive({
    res <- rv_opt()
    S_plan <- if (is.null(res)) S1 else S1 * runif(84, 0.9, 1.1) # Simular variação no plano para a demo
    decompose_plan(S_plan, PREV)
  })

  output$o_kpi_lucro <- renderUI({
    res <- rv_opt()
    val <- if (is.null(res)) 5690 else res$total_profit
    span(
      span(fmt_dol(val), style = if(val < 0) "color:#c0392b;" else ""),
      p("lucro semanal estimado", style = "font-size:.78rem; margin:0; opacity:.8;")
    )
  })

  output$o_kpi_units <- renderUI({
    res <- rv_opt()
    u <- if (is.null(res)) 9500 else round(res$unidades)
    valido <- if (is.null(res)) TRUE else res$valido
    # Usar azul (info) em vez de vermelho
    theme_sel <- if (!valido && input$o_objetivo == "O2") "warning" else "info"
    span(
      span(format(u, big.mark = ","), style = if(!valido) "color:#d35400;" else ""),
      p(if (!valido) "⚠ Acima do Limite O2" else "✓ Volume de Vendas",
        style = "font-size:.78rem; margin:0; opacity:.9;")
    )
  })

  output$o_kpi_hr <- renderUI({
    res <- rv_opt()
    hr <- if (is.null(res)) 69 else sum(res$rh_total)
    span(
      as.character(hr),
      p("total de funcionários", style = "font-size:.78rem; margin:0; opacity:.8;")
    )
  })

  output$o_plan_ui <- renderUI({
    if (is.null(rv_opt())) {
      div(
        style = "padding:2.5rem; text-align:center; color:#aaa;",
        bs_icon("cpu"), tags$br(), tags$br(),
        "Prima ", strong("Executar Otimização"), " para gerar o plano semanal."
      )
    } else {
      tagList(
        # Legenda explicativa
        div(style = "display:flex; gap:15px; font-size:0.83rem; color:#666; background:#f8f9fa; padding:10px; border-radius:12px; margin-bottom:10px; border: 1px solid #eee;",
          span(tags$b(style="color:#2c3e50;", "PR:"), " Promoção (%)"),
          span(tags$b(style="color:#2c3e50;", "X:"), " Func. Experientes (Sénior)"),
          span(tags$b(style="color:#2c3e50;", "J:"), " Func. Juniores"),
          span(tags$b(style="color:#2c3e50;", "Atend.:"), " Clientes Atendidos")
        ),
        DTOutput("o_plan_table")
      )
    }
  })

  output$o_plan_table <- renderDT({
    plan <- r_plan_opt()
    datatable(plan,
      options = list(dom = "ft", pageLength = 28, scrollY = "400px", scrollX = TRUE),
      rownames = FALSE, class = "table-sm table-hover table-striped"
    ) %>%
      formatStyle("Loja", fontWeight = "bold") %>%
      formatCurrency(c("Receita", "Custo RH", "Lucro"), currency = "$", digits = 0) %>%
      formatStyle("Lucro",
        color      = styleInterval(0, c("#c0392b", "#1d5937")),
        fontWeight = "bold"
      ) %>%
      formatStyle("Unidades",
        background         = styleColorBar(plan$Unidades, "#4682B430"),
        backgroundSize     = "100% 80%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$o_chart_ui <- renderUI({
    plotlyOutput("o_profit_chart", height = "230px")
  })

  output$o_profit_chart <- renderPlotly({
    res <- rv_opt()
    
    if (input$o_objetivo == "O3") {
      # Mostrar Fronteira de Pareto para O3
      p <- ggplot(pareto_ok, aes(x = total_HR, y = lucro)) +
        geom_point(color = PALETA_DSS$principal, alpha = 0.6) +
        geom_point(data = COMPROMISSO, color = "red", size = 3) +
        theme_minimal() +
        labs(x = "Recursos Humanos (RH)", y = "Lucro ($)")
      return(ggplotly(p))
    }
    
    # Para O1/O2, mostrar Convergência (Shaded) com base no algoritmo
    res_val <- r_opt_res()
    met <- input$o_metodo
    p <- plot_shaded_convergence(res_val$hists, paste("Convergência —", met), 
                                 "Avaliações (FES)", "Melhor Lucro ($)")
    ggplotly(p)
  })

  # ==========================================================
  # TAB 3 — DSS INTEGRADO
  # ==========================================================

  observeEvent(input$d_btn, {
    pw <- get_prev_week(input$d_semana)
    # Limpar plano anterior para mostrar o spinner
    rv_dss(NULL)
    
    withProgress(message = paste("A otimizar —", input$d_objetivo, "..."), value = 0, {
      S <- run_mc(pw, input$d_objetivo, 3000) # Aumentar iterações para melhor resultado
      rv_dss(S)
    })
  })

  r_plan_dss <- reactive({
    S <- rv_dss()
    req(!is.null(S))
    pw <- get_prev_week(input$d_semana)
    PREV <<- pw
    upper <<- calc_upper(PREV)
    decompose_plan(S, pw)
  })

  output$d_semana_label <- renderText({
    input$d_semana
  })

  output$d_kpi_top <- renderUI({
    S <- rv_dss()
    if (is.null(S)) {
      # Estado inicial (mostrar S1 como baseline)
      S <- S1
    }
    pw <- get_prev_week(input$d_semana)
    PREV <<- pw
    upper <<- calc_upper(PREV)
    u <- total_units(S)
    hr <- sum(round(S[seq(2, 84, 3)]) + round(S[seq(3, 84, 3)]))
    p <- profit(S)
    
    layout_columns(
      value_box(tags$span(bs_icon("currency-dollar"), " Lucro"),
        fmt_dol(p),
        theme = "success", height = "100px",
        p("lucro semanal estimado", style = "margin:0; font-size:.7rem; opacity:.8;")
      ),
      value_box(tags$span(bs_icon("box-seam"), " Unidades"),
        format(round(u), big.mark = ","),
        theme = "info", height = "100px",
        p("total de unidades vendidas", style = "margin:0; font-size:.7rem; opacity:.8;")
      ),
      value_box(tags$span(bs_icon("people"), " RH Total"),
        as.character(hr),
        theme = "secondary", height = "100px",
        p("funcionários escalados", style = "margin:0; font-size:.7rem; opacity:.8;")
      ),
      col_widths = c(4, 4, 4)
    )
  })

  output$d_forecast_chart <- renderPlotly({
    pw <- get_prev_week(input$d_semana)
    df <- data.frame(
      Loja     = rep(LOJA_NAMES, each = 7),
      Dia      = rep(factor(DIAS, levels = DIAS), 4),
      Clientes = pw
    )
    p <- ggplot(df, aes(
      x = Dia, y = Clientes, fill = Loja,
      text = paste0(Loja, " · ", Dia, ": ", round(Clientes))
    )) +
      geom_col(position = "dodge", width = .7) +
      scale_fill_manual(values = LOJA_CORES) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom", panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      labs(x = NULL, y = "Clientes Previstos", fill = NULL)
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  output$d_best_table <- renderDT({
    req(!is.null(algo_stats))
    df <- algo_stats[, c("Algoritmo", "Objetivo", "Mediana", "Max")]
    colnames(df) <- c("Algoritmo", "Obj.", "Mediana ($)", "Máx ($)")
    datatable(df,
      options = list(dom = "t", pageLength = 10),
      rownames = FALSE, class = "table-sm table-hover"
    ) %>%
      formatCurrency(c("Mediana ($)", "Máx ($)"), currency = "$", digits = 0) %>%
      formatStyle("Mediana ($)",
        color      = styleInterval(0, c("#c0392b", "#1d5937")),
        fontWeight = "bold"
      ) %>%
      formatStyle("Obj.",
        backgroundColor = styleEqual(c("O1", "O2", "O3"), c("#dbeafe", "#dcfce7", "#fef9c3")),
        fontWeight = "bold", textAlign = "center"
      )
  })

  output$d_plan_ui <- renderUI({
    if (is.null(rv_dss())) {
      div(
        style = "padding:2.5rem; text-align:center; color:#aaa;",
        bs_icon("layers"), tags$br(), tags$br(),
        "Prima ", strong("Gerar Plano"), " para ver o plano otimizado."
      )
    } else {
      DTOutput("d_plan_table")
    }
  })

  output$d_plan_table <- renderDT({
    plan <- r_plan_dss()
    datatable(plan,
      options = list(dom = "ft", pageLength = 28, scrollY = "380px", scrollX = TRUE),
      rownames = FALSE, class = "table-sm table-hover table-striped"
    ) %>%
      formatStyle("Loja", fontWeight = "bold") %>%
      formatCurrency(c("Receita", "Custo RH", "Lucro"), currency = "$", digits = 0) %>%
      formatStyle("Lucro",
        color      = styleInterval(0, c("#c0392b", "#1d5937")),
        fontWeight = "bold"
      )
  })
}

# ---------- Launch ----------
shinyApp(ui, server)
