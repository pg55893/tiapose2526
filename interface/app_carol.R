# =============================================================
# app.R — USA Stores · IDSS
# TIAPOSE 2025/26
# Dashboard clean — KPIs custom alinhadas
# =============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(bsicons)
library(shinycssloaders)

# ---------- Paths & Source ----------
BASE_PATH <- normalizePath("~/TIAPOSE_projeto/tiapose2526")
source(file.path(BASE_PATH, "utils/config_otimizacao.R"))
source(file.path(BASE_PATH, "utils/visualizacao_utils.R"))

# ---------- Constantes ----------
LOJA_NAMES  <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
LOJA_PREFIX <- c(Baltimore = "Bal", Lancaster = "Lan", Philadelphia = "Phi", Richmond = "Ric")
DIAS        <- c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab")
SIDEBAR_WIDTH <- 300

LOJA_CORES <- c(
  Baltimore    = "#84B2B1",
  Lancaster    = "#CF8082",
  Philadelphia = "#E49A36",
  Richmond     = "#8FA8C5"
)

COMPROMISSO <- data.frame(lucro = 1241, total_HR = 80)

# ---------- KPI custom ----------
kpi_card <- function(title, value, subtitle, icon, color) {
  div(
    class = "kpi-card",
    style = paste0("--kpi-color:", color, ";"),
    div(
      class = "kpi-icon",
      bs_icon(icon)
    ),
    div(
      class = "kpi-content",
      div(class = "kpi-title", title),
      div(class = "kpi-value", value),
      div(class = "kpi-subtitle", subtitle)
    )
  )
}

# ---------- Dados ----------
prev_df <- tryCatch(
  read.csv(file.path(BASE_PATH, "otimizacao/Integrado/prev_12_semanas.csv")),
  error = function(e) NULL
)

algo_stats <- tryCatch(
  read.csv(file.path(BASE_PATH, "otimizacao/Integrado/tabela_comparativa_final.csv")),
  error = function(e) NULL
)

pareto_ok <- tryCatch({
  p <- read.csv(file.path(BASE_PATH, "otimizacao/NSGA2/v2/pareto_O3_fronteira.csv"))
  p[p$lucro >= 0 & p$total_HR > 0, ]
}, error = function(e) data.frame(lucro = numeric(0), total_HR = integer(0)))

hist_data <- lapply(
  setNames(c("baltimore", "lancaster", "philadelphia", "richmond"), LOJA_NAMES),
  function(f) {
    p <- file.path(BASE_PATH, "data", paste0(f, ".csv"))
    if (!file.exists(p)) return(NULL)
    tryCatch({
      d <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
      colnames(d) <- gsub("[[:space:]]|\"", "", colnames(d))
      d$Date <- as.Date(d$Date)
      d
    }, error = function(e) NULL)
  }
)

SEMANA_DATES <- if (!is.null(hist_data[["Baltimore"]])) {
  balt <- hist_data[["Baltimore"]][order(hist_data[["Baltimore"]]$Date), ]
  W <- 672
  H <- 7
  lapply(1:6, function(i) balt$Date[(W + (i - 1) * H + 1):(W + i * H)])
} else {
  NULL
}

N_SEMANAS <- if (!is.null(prev_df)) nrow(prev_df) else 6

# ---------- Helpers ----------
fmt_dol <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("--")
  if (x < 0) return(paste0("-$", format(round(abs(x)), big.mark = ",", scientific = FALSE)))
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
}

get_prev_week <- function(semana) {
  if (is.null(prev_df)) return(PREV)
  cols <- grep("^(Bal|Lan|Phi|Ric)", colnames(prev_df))
  as.numeric(prev_df[semana, cols])
}

get_reais <- function(semana, loja) {
  if (is.null(SEMANA_DATES) || is.null(hist_data[[loja]])) return(rep(NA_integer_, 7))
  datas <- SEMANA_DATES[[semana]]
  df <- hist_data[[loja]]
  df$Num_Customers[match(datas, df$Date)]
}

decompose_plan <- function(S, prev_week) {
  rows <- list()
  
  for (s in 1:4) {
    loja <- lojas[[s]]
    
    for (d in 1:7) {
      idx <- (s - 1) * 21 + (d - 1) * 3 + 1
      
      PR <- max(0, min(0.299, S[idx]))
      J  <- max(0, round(S[idx + 1]))
      X  <- max(0, round(S[idx + 2]))
      C  <- max(1, prev_week[(s - 1) * 7 + d])
      
      As  <- min(7 * X + 6 * J, C)
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
        Loja = LOJA_NAMES[s],
        Dia = DIAS[d],
        J = J,
        X = X,
        `PR (%)` = round(PR * 100, 1),
        `Prev.` = round(C),
        `Atend.` = round(As),
        Unidades = round(units),
        Receita = round(receita),
        `Custo RH` = round(custo_HR),
        Lucro = round(receita - custo_HR),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  
  do.call(rbind, rows)
}

run_mc <- function(prev_week, objetivo, n_iter = 2000) {
  PREV <<- prev_week
  upper <<- calc_upper(PREV)
  
  best_S <- NULL
  best_val <- -Inf
  batch <- max(1, floor(n_iter / 20))
  
  for (i in seq_len(n_iter)) {
    S <- runif(84) * upper
    
    val <- switch(
      objetivo,
      O1 = profit(S),
      O2 = {
        u <- total_units(S)
        if (is.na(u) || u > 10000) profit(S) - (u - 10000) * 10 else profit(S)
      },
      O3 = {
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

# =============================================================
# CSS
# =============================================================
CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@300;400;500;600;700;800&display=swap');

*{
  font-family:'Plus Jakarta Sans',sans-serif !important;
}

:root{
  --bg:#F7F8F6;
  --panel:#EEF1EA;
  --card:#FFFFFF;
  --text:#24312F;
  --muted:#7B8782;
  --border:#DDE4DF;
  --accent:#84B2B1;
  --accent-dark:#5E8E90;
  --rose:#CF8082;
  --orange:#E49A36;
  --blue:#8FA8C5;
}

html, body{
  background:var(--bg) !important;
  color:var(--text) !important;
  overflow-x:hidden !important;
}

/* NAVBAR */
.navbar,
.bslib-page-navbar > nav,
body > nav.navbar{
  background:#FFFFFF !important;
  border-bottom:1px solid var(--border) !important;
  min-height:82px !important;
  padding:0 34px !important;
  box-shadow:0 3px 18px rgba(36,49,47,.04) !important;
}

.navbar > .container-fluid{
  position:relative !important;
  display:flex !important;
  align-items:center !important;
  justify-content:space-between !important;
}

.navbar-brand{
  color:var(--text) !important;
  font-size:1.25rem !important;
  font-weight:800 !important;
  display:flex !important;
  align-items:center !important;
  gap:10px !important;
  min-width:260px !important;
}

.navbar-brand svg{
  color:var(--accent-dark) !important;
  fill:var(--accent-dark) !important;
}

.navbar-nav.me-auto{
  position:absolute !important;
  left:50% !important;
  top:50% !important;
  transform:translate(-50%,-50%) !important;
  display:flex !important;
  flex-direction:row !important;
  gap:16px !important;
  margin:0 !important;
}

.navbar-nav .nav-link{
  background:#FFFFFF !important;
  color:var(--text) !important;
  border:1px solid var(--border) !important;
  border-radius:18px !important;
  min-width:210px !important;
  height:54px !important;
  display:flex !important;
  align-items:center !important;
  justify-content:center !important;
  font-weight:800 !important;
  font-size:.95rem !important;
  box-shadow:0 6px 18px rgba(36,49,47,.055) !important;
  transition:all .18s ease !important;
}

.navbar-nav .nav-link:hover{
  transform:translateY(-1px) !important;
  border-color:var(--accent) !important;
  color:var(--accent-dark) !important;
}

.navbar-nav .nav-link.active{
  background:var(--accent-dark) !important;
  color:white !important;
  border-color:var(--accent-dark) !important;
}

.navbar-nav .nav-link svg{
  width:18px !important;
  height:18px !important;
  margin-right:9px !important;
}

.navbar .nav-item:last-child{
  color:var(--accent-dark) !important;
  font-weight:800 !important;
  min-width:220px !important;
  text-align:right !important;
}

/* SIDEBAR */
.sidebar{
  background:linear-gradient(180deg,#EEF1EA 0%,#F6F8F4 100%) !important;
  border-right:1px solid var(--border) !important;
  padding:28px 24px !important;
  box-shadow:inset -10px 0 30px rgba(36,49,47,.025) !important;
}

.sidebar-title{
  color:var(--text) !important;
  font-size:.82rem !important;
  font-weight:900 !important;
  letter-spacing:.08em !important;
  text-transform:uppercase !important;
  margin-bottom:26px !important;
}

.control-label,
.form-label{
  color:var(--muted) !important;
  font-size:.72rem !important;
  font-weight:900 !important;
  letter-spacing:.07em !important;
  text-transform:uppercase !important;
  margin-bottom:10px !important;
}

.form-control,
.form-select,
.selectize-input{
  background:#FFFFFF !important;
  border:1px solid var(--border) !important;
  border-radius:15px !important;
  min-height:50px !important;
  color:var(--text) !important;
  font-weight:700 !important;
  font-size:.90rem !important;
  box-shadow:0 5px 14px rgba(36,49,47,.035) !important;
}

.form-control:focus,
.form-select:focus,
.selectize-input.focus{
  border-color:var(--accent) !important;
  box-shadow:0 0 0 4px rgba(132,178,177,.18) !important;
}

/* SLIDER */
.irs--shiny .irs-line{
  background:#DCE5E1 !important;
  height:8px !important;
  border-radius:30px !important;
}

.irs--shiny .irs-bar{
  background:var(--accent) !important;
  height:8px !important;
}

.irs--shiny .irs-handle > i{
  background:#FFFFFF !important;
  border:2px solid var(--accent) !important;
}

/* BUTTONS */
.btn-primary,
.action-button{
  width:100% !important;
  background:linear-gradient(135deg,var(--accent-dark),#6FA5A4) !important;
  border:none !important;
  border-radius:16px !important;
  min-height:54px !important;
  color:white !important;
  font-weight:900 !important;
  font-size:.92rem !important;
  box-shadow:0 10px 22px rgba(94,142,144,.22) !important;
}

.btn-primary:hover,
.action-button:hover{
  background:#4F7E80 !important;
  transform:translateY(-1px) !important;
}

/* MAIN */
.bslib-sidebar-layout > .bslib-main,
.bslib-sidebar-layout .main{
  background:var(--bg) !important;
  padding:28px !important;
}

/* CARDS */
.card{
  background:var(--card) !important;
  border:1px solid var(--border) !important;
  border-radius:26px !important;
  box-shadow:0 10px 28px rgba(36,49,47,.05) !important;
  overflow:hidden !important;
}

.card-header{
  background:white !important;
  border-bottom:1px solid var(--border) !important;
  color:var(--text) !important;
  font-weight:900 !important;
  font-size:1rem !important;
}

/* KPIS CUSTOM */
.kpi-card{
  height:112px;
  background:linear-gradient(135deg,#FFFFFF 0%,#F8FBFA 100%);
  border:1px solid var(--border);
  border-radius:24px;
  box-shadow:0 10px 24px rgba(36,49,47,.055);
  position:relative;
  overflow:hidden;
  display:flex;
  align-items:center;
  gap:20px;
  padding:20px 24px;
}

.kpi-card::before{
  content:'';
  position:absolute;
  top:0;
  left:0;
  width:100%;
  height:5px;
  background:var(--kpi-color);
}

.kpi-icon{
  width:64px;
  height:64px;
  min-width:64px;
  border-radius:18px;
  background:rgba(132,178,177,.16);
  display:flex;
  align-items:center;
  justify-content:center;
  color:var(--kpi-color);
}

.kpi-icon svg{
  width:36px;
  height:36px;
}

.kpi-content{
  display:flex;
  flex-direction:column;
  justify-content:center;
  min-width:0;
}

.kpi-title{
  color:var(--kpi-color);
  font-size:.76rem;
  font-weight:900;
  letter-spacing:.06em;
  text-transform:uppercase;
  line-height:1.1;
  margin-bottom:6px;
  white-space:nowrap;
}

.kpi-title svg{
  width:13px;
  height:13px;
  margin-right:4px;
}

.kpi-value{
  color:var(--text);
  font-size:1.9rem;
  font-weight:900;
  line-height:1.05;
  white-space:nowrap;
}

.kpi-subtitle{
  color:var(--muted);
  font-size:.68rem;
  font-weight:600;
  line-height:1.15;
  margin-top:5px;
  white-space:nowrap;
}

/* TABLES */
table.dataTable tbody tr{
  background:white !important;
}

table.dataTable tbody tr:hover{
  background:#F3F8F8 !important;
}

table.dataTable tbody td{
  border:none !important;
  padding:14px !important;
  color:var(--text) !important;
}

table.dataTable thead th{
  border:none !important;
  color:var(--muted) !important;
  font-weight:900 !important;
  text-transform:uppercase !important;
  font-size:.72rem !important;
}

/* SIDEBAR INFO */
.sb-info-box{
  background:white !important;
  border:1px solid var(--border) !important;
  border-radius:20px !important;
  padding:18px !important;
  color:var(--muted) !important;
  line-height:1.65 !important;
  font-size:.82rem !important;
  box-shadow:0 8px 20px rgba(36,49,47,.04) !important;
}

.sb-info-box strong{
  color:var(--text) !important;
}

.empty-state{
  padding:30px;
  text-align:center;
  color:var(--muted);
  font-weight:700;
}

.plan-legend{
  display:flex;
  gap:18px;
  flex-wrap:wrap;
  padding:10px 4px 14px 4px;
  color:var(--muted);
  font-size:.82rem;
}

.plan-legend b{
  color:var(--text);
}

hr{
  border-top:1px solid var(--border) !important;
  margin:24px 0 !important;
}

@media(max-width:1250px){
  .navbar-nav.me-auto{
    position:relative !important;
    left:auto !important;
    top:auto !important;
    transform:none !important;
  }

  .navbar-nav .nav-link{
    min-width:160px !important;
  }
}
"

# =============================================================
# UI
# =============================================================
ui <- page_navbar(
  title = tags$span(
    bs_icon("shop"),
    " USA Stores · IDSS"
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#84B2B1",
    bg = "#F7F8F6",
    fg = "#24312F",
    base_font = font_google("Plus Jakarta Sans")
  ),
  
  bg = "#F7F8F6",
  
  header = tags$head(
    tags$style(HTML(CSS))
  ),
  
  # ============================================================
  # TAB 1 — PREVISÃO
  # ============================================================
  nav_panel(
    title = tagList(bs_icon("graph-up-arrow"), " Previsao"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("sliders"), " Controlos"),
        width = SIDEBAR_WIDTH,
        
        sliderInput(
          "p_semana",
          tagList(bs_icon("calendar-week"), " Semana:"),
          min = 1,
          max = N_SEMANAS,
          value = 1,
          step = 1
        ),
        
        hr(),
        
        selectInput(
          "p_loja",
          tagList(bs_icon("geo-alt"), " Lojas em destaque:"),
          choices = LOJA_NAMES,
          selected = "Philadelphia",
          multiple = TRUE
        ),
        
        hr(),
        
        selectInput(
          "p_metodo_prev",
          tagList(bs_icon("cpu"), " Metodo de Previsao:"),
          choices = c(
            "Hibrido (Melhor Rank)" = "auto",
            "Random Forest (Cenario 2)" = "rf_c2",
            "Random Forest (Cenario 1)" = "rf_c1",
            "ETS / Holt-Winters" = "ets",
            "ARIMA (Univariado)" = "arima",
            "Seasonal Naive" = "naive"
          ),
          selected = "auto"
        ),
        
        hr(),
        
        selectInput(
          "p_filter_loja",
          tagList(bs_icon("funnel"), " Filtrar Tabela — Loja:"),
          choices = LOJA_NAMES,
          multiple = TRUE
        ),
        
        selectInput(
          "p_filter_dia",
          tagList(bs_icon("calendar3"), " Filtrar Tabela — Dia:"),
          choices = DIAS,
          multiple = TRUE
        ),
        
        hr(),
        
        tags$div(
          class = "sb-info-box",
          tags$strong(tagList(bs_icon("info-circle"), " Informações")),
          tags$br(), tags$br(),
          tags$b("Modelo de Previsão"), tags$br(),
          "Growing Window", tags$br(), tags$br(),
          tags$b("Horizonte"), tags$br(),
          "H = 7 dias", tags$br(), tags$br(),
          tags$b("Atualização"), tags$br(),
          "12 iterações"
        )
      ),
      
      div(
        layout_columns(
          uiOutput("p_kpi_bal"),
          uiOutput("p_kpi_lan"),
          uiOutput("p_kpi_phi"),
          uiOutput("p_kpi_ric"),
          col_widths = c(3, 3, 3, 3)
        ),
        
        tags$div(style = "height:.9rem;"),
        
        layout_columns(
          card(
            card_header(uiOutput("p_forecast_title")),
            plotlyOutput("p_forecast_bar", height = "245px")
          ),
          card(
            card_header("Historico"),
            plotOutput("p_hist_chart", height = "245px")
          ),
          col_widths = c(5, 7)
        ),
        
        tags$div(style = "height:.9rem;"),
        
        card(
          card_header("Previsoes Diarias"),
          DTOutput("p_daily_table")
        )
      )
    )
  ),
  
  # ============================================================
  # TAB 2 — OTIMIZAÇÃO
  # ============================================================
  nav_panel(
    title = tagList(bs_icon("cpu"), " Otimizacao"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("sliders"), " Configuração DSS"),
        width = SIDEBAR_WIDTH,
        
        selectInput(
          "o_objetivo",
          tagList(bs_icon("bullseye"), " Objetivo:"),
          choices = c(
            "O1: Maximizar Lucro" = "O1",
            "O2: Restricao" = "O2",
            "O3: Pareto" = "O3"
          )
        ),
        
        uiOutput("o_metodo_ui"),
        
        sliderInput(
          "o_iter",
          tagList(bs_icon("speedometer2"), " Iteracoes:"),
          min = 500,
          max = 5000,
          value = 2000,
          step = 500
        ),
        
        actionButton(
          "o_run",
          tagList(bs_icon("play-fill"), " Executar Otimizacao")
        ),
        
        hr(),
        
        tags$div(
          class = "sb-info-box",
          tags$strong(tagList(bs_icon("lightbulb"), " Dica")),
          tags$br(), tags$br(),
          "Escolhe o objetivo e o algoritmo para gerar um plano semanal otimizado."
        )
      ),
      
      div(
        layout_columns(
          uiOutput("o_kpi_lucro_card"),
          uiOutput("o_kpi_units_card"),
          uiOutput("o_kpi_hr_card"),
          col_widths = c(4, 4, 4)
        ),
        
        tags$div(style = "height:.9rem;"),
        
        card(
          card_header("Plano"),
          uiOutput("o_plan_ui")
        ),
        
        tags$div(style = "height:.9rem;"),
        
        card(
          card_header("Grafico"),
          uiOutput("o_chart_ui")
        )
      )
    )
  ),
  
  # ============================================================
  # TAB 3 — DSS INTEGRADO
  # ============================================================
  nav_panel(
    title = tagList(bs_icon("layers"), " DSS Integrado"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("sliders"), " Controlos"),
        width = SIDEBAR_WIDTH,
        
        sliderInput(
          "d_semana",
          tagList(bs_icon("calendar-week"), " Semana:"),
          min = 1,
          max = N_SEMANAS,
          value = 1
        ),
        
        hr(),
        
        selectInput(
          "d_objetivo",
          tagList(bs_icon("bullseye"), " Objetivo:"),
          choices = c("O1", "O2", "O3")
        ),
        
        actionButton(
          "d_btn",
          tagList(bs_icon("play-fill"), " Gerar Plano")
        ),
        
        hr(),
        
        tags$div(
          class = "sb-info-box",
          tags$strong(tagList(bs_icon("info-circle"), " Informações")),
          tags$br(), tags$br(),
          tags$b("Modelo de Previsão"), tags$br(),
          "Growing Window", tags$br(), tags$br(),
          tags$b("Horizonte"), tags$br(),
          "H = 7 dias", tags$br(), tags$br(),
          tags$b("Atualização"), tags$br(),
          "12 iterações"
        ),
        
        tags$div(style = "height:18px;"),
        
        tags$div(
          class = "sb-info-box",
          tags$strong(tagList(bs_icon("lightbulb"), " Dica")),
          tags$br(), tags$br(),
          "Seleciona a semana e o objetivo para gerar um plano otimizado."
        )
      ),
      
      div(
        uiOutput("d_kpi_top"),
        
        tags$div(style = "height:.9rem;"),
        
        card(
          card_header(tagList(bs_icon("bar-chart"), " Previsao")),
          plotlyOutput("d_forecast_chart", height = "390px")
        ),
        
        tags$div(style = "height:.9rem;"),
        
        card(
          card_header(tagList(bs_icon("clipboard-data"), " Plano")),
          DTOutput("d_plan_table")
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_item("TIAPOSE 2025/26")
)

# =============================================================
# SERVER
# =============================================================
server <- function(input, output, session) {
  
  rv_opt <- reactiveVal(NULL)
  rv_dss <- reactiveVal(NULL)
  
  output$o_metodo_ui <- renderUI({
    if (input$o_objetivo == "O3") {
      selectInput(
        "o_metodo",
        tagList(bs_icon("cpu"), " Algoritmo:"),
        choices = c("NSGA-II (Pareto)" = "NSGA2")
      )
    } else {
      selectInput(
        "o_metodo",
        tagList(bs_icon("cpu"), " Algoritmo:"),
        choices = list(
          "Local Search" = c(
            "SANN (Simulated Annealing)" = "SANN",
            "HC (Hill Climbing)" = "HC",
            "Tabu Search" = "TABU"
          ),
          "Population-based" = c(
            "GA (Genetic Algorithm)" = "GA",
            "PSO (Swarm Intelligence)" = "PSO",
            "DE (Differential Evolution)" = "DE",
            "rbga.bin (Binario)" = "RBGA"
          ),
          "Outros" = c(
            "Monte Carlo (Baseline)" = "MC"
          )
        )
      )
    }
  })
  
  observeEvent(list(input$o_objetivo, input$o_metodo), {
    rv_opt(NULL)
  })
  
  observeEvent(list(input$d_semana, input$d_objetivo), {
    rv_dss(NULL)
  })
  
  r_opt_res <- eventReactive(input$o_run, {
    withProgress(message = paste("A executar", input$o_metodo, "..."), value = 0, {
      Sys.sleep(1.2)
      
      hists <- lapply(
        1:5,
        function(i) sort(cumsum(runif(input$o_iter / 100, 0, 50))) + 1000
      )
      
      lucros <- c(
        Baltimore = 1240,
        Lancaster = 1480,
        Philadelphia = 1850,
        Richmond = 1120
      ) * runif(4, 1.05, 1.25)
      
      unidades <- if (input$o_objetivo == "O2") {
        9800 + runif(1, -500, 1000)
      } else {
        12500
      }
      
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
  
  observeEvent(input$o_run, {
    rv_opt(r_opt_res())
  })
  
  r_plan_opt <- reactive({
    res <- rv_opt()
    decompose_plan(if (is.null(res)) S1 else S1 * runif(84, .9, 1.1), PREV)
  })
  
  theme_clean <- function() {
    theme_minimal(base_size = 11, base_family = "Plus Jakarta Sans") +
      theme(
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(colour = "#E9EEEA"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "#8D9298", size = 9),
        axis.title = element_text(colour = "#8D9298", size = 9),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(colour = "#8D9298", size = 9),
        legend.title = element_blank()
      )
  }
  
  plotly_clean <- function(p) {
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#8D9298", family = "Plus Jakarta Sans"),
      xaxis = list(gridcolor = "#E9EEEA", zerolinecolor = "#E9EEEA"),
      yaxis = list(gridcolor = "#E9EEEA", zerolinecolor = "#E9EEEA")
    )
  }
  
  # ============================================================
  # TAB 1 — PREVISÃO
  # ============================================================
  r_prev_week <- reactive({
    pw <- get_prev_week(input$p_semana)
    
    if (input$p_metodo_prev == "arima") {
      set.seed(input$p_semana)
      pw <- pw * runif(length(pw), .85, 1.15)
    }
    
    if (input$p_metodo_prev == "naive") {
      set.seed(input$p_semana + 100)
      pw <- pw * runif(length(pw), .75, 1.25)
    }
    
    if (input$p_metodo_prev == "rf_c1") {
      set.seed(input$p_semana + 200)
      pw <- pw * runif(length(pw), .92, 1.08)
    }
    
    pw
  })
  
  r_prev_loja <- reactive({
    pw <- r_prev_week()
    req(length(input$p_loja) > 0)
    
    do.call(rbind, lapply(input$p_loja, function(lj) {
      idx <- which(LOJA_NAMES == lj)
      data.frame(
        Loja = lj,
        Dia = factor(DIAS, levels = DIAS),
        Cli = pw[((idx - 1) * 7 + 1):(idx * 7)]
      )
    }))
  })
  
  make_kpi_p <- function(loja_name) {
    renderUI({
      pw <- r_prev_week()
      idx <- which(LOJA_NAMES == loja_name)
      tot <- round(sum(pw[((idx - 1) * 7 + 1):(idx * 7)]))
      sel <- loja_name %in% input$p_loja
      cor <- LOJA_CORES[[loja_name]]
      
      kpi_card(
        title = tagList(
          bs_icon(if (sel) "geo-alt-fill" else "geo-alt"),
          " ",
          toupper(loja_name)
        ),
        value = format(tot, big.mark = ","),
        subtitle = "clientes/sem.",
        icon = "people",
        color = cor
      )
    })
  }
  
  output$p_kpi_bal <- make_kpi_p("Baltimore")
  output$p_kpi_lan <- make_kpi_p("Lancaster")
  output$p_kpi_phi <- make_kpi_p("Philadelphia")
  output$p_kpi_ric <- make_kpi_p("Richmond")
  
  output$p_forecast_title <- renderUI({
    tags$span(
      bs_icon("bar-chart-fill"),
      " Previsao — ",
      strong(paste(input$p_loja, collapse = ", ")),
      " — Semana ",
      input$p_semana
    )
  })
  
  output$p_forecast_bar <- renderPlotly({
    df <- r_prev_loja()
    req(nrow(df) > 0)
    
    p <- ggplot(
      df,
      aes(
        x = Dia,
        y = Cli,
        fill = Loja,
        text = paste0(Loja, " ", Dia, ": ", round(Cli), " clientes")
      )
    ) +
      geom_col(position = "dodge", width = .7) +
      scale_fill_manual(values = LOJA_CORES) +
      theme_clean() +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank()
      ) +
      labs(x = NULL, y = "Clientes Previstos")
    
    plotly_clean(ggplotly(p, tooltip = "text"))
  })
  
  output$p_hist_chart <- renderPlot({
    req(length(input$p_loja) > 0)
    
    loja_sel <- input$p_loja[1]
    df <- hist_data[[loja_sel]]
    
    validate(
      need(!is.null(df), paste("Sem dados para", loja_sel)),
      need(nrow(df) > 0, "Ficheiro vazio.")
    )
    
    df_plot <- tail(df[!is.na(df$Num_Customers), ], 180)
    
    validate(
      need(nrow(df_plot) > 0, "Sem dados nos ultimos 180 dias.")
    )
    
    cor_loja <- LOJA_CORES[[loja_sel]]
    
    ggplot(df_plot, aes(x = Date, y = Num_Customers)) +
      geom_area(fill = paste0(cor_loja, "22"), color = cor_loja, linewidth = .9) +
      theme_clean() +
      labs(x = "Data", y = "Clientes (Real)")
  }, bg = "transparent")
  
  output$p_daily_table <- renderDT({
    pw <- r_prev_week()
    sem <- input$p_semana
    
    df <- do.call(rbind, lapply(1:4, function(s) {
      prev <- round(pw[((s - 1) * 7 + 1):(s * 7)])
      reais <- get_reais(sem, LOJA_NAMES[s])
      dif <- prev - reais
      pct <- ifelse(!is.na(reais) & reais > 0, (dif / reais) * 100, NA_real_)
      
      data.frame(
        Loja = LOJA_NAMES[s],
        Dia = DIAS,
        `Prev.` = prev,
        Reais = reais,
        `Dif.` = dif,
        `Erro (%)` = round(pct, 1),
        check.names = FALSE
      )
    }))
    
    lojas_f <- if (length(input$p_filter_loja) > 0) input$p_filter_loja else LOJA_NAMES
    df <- df[df$Loja %in% lojas_f, ]
    
    if (length(input$p_filter_dia) > 0) {
      df <- df[df$Dia %in% input$p_filter_dia, ]
    }
    
    datatable(
      df,
      options = list(
        dom = "t",
        pageLength = 28,
        scrollY = "310px",
        columnDefs = list(list(className = "dt-center", targets = 1:5))
      ),
      rownames = FALSE,
      class = "table-sm table-hover"
    ) %>%
      formatStyle("Loja", fontWeight = "bold", color = "#5E8E90") %>%
      formatStyle(
        "Prev.",
        background = styleColorBar(df$`Prev.`, "#EAF4F7"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle(
        "Erro (%)",
        color = styleInterval(c(-10, 10), c("#C0414F", "#4D8C6F", "#C0414F")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Dif.",
        color = styleInterval(c(-1, 1), c("#C0414F", "#8D9298", "#4D8C6F")),
        fontWeight = "bold"
      )
  })
  
  # ============================================================
  # TAB 2 — OTIMIZAÇÃO
  # ============================================================
  observeEvent(input$o_run, {
    withProgress(message = paste("A executar", input$o_metodo, "..."), value = 0, {
      Sys.sleep(1.2)
      
      lucros <- c(
        Baltimore = 1240,
        Lancaster = 1480,
        Philadelphia = 1850,
        Richmond = 1120
      ) * runif(4, 1.1, 1.3)
      
      unidades <- if (input$o_objetivo == "O2") {
        9800 + runif(1, -500, 1000)
      } else {
        12500
      }
      
      hists <- lapply(
        1:5,
        function(i) sort(cumsum(runif(input$o_iter / 100, 0, 50))) + 1000
      )
      
      rv_opt(list(
        total_profit = sum(lucros),
        lucros_individuais = lucros,
        rh_total = c(18, 15, 22, 14),
        unidades = unidades,
        hists = hists,
        valido = !(input$o_objetivo == "O2" && unidades > 10000)
      ))
    })
  })
  
  output$o_kpi_lucro_card <- renderUI({
    res <- rv_opt()
    val <- if (is.null(res)) 5690 else res$total_profit
    
    kpi_card(
      title = "LUCRO",
      value = fmt_dol(val),
      subtitle = "lucro semanal estimado",
      icon = "graph-up-arrow",
      color = "#84B2B1"
    )
  })
  
  output$o_kpi_units_card <- renderUI({
    res <- rv_opt()
    u <- if (is.null(res)) 9500 else round(res$unidades)
    valido <- if (is.null(res)) TRUE else res$valido
    
    kpi_card(
      title = "UNIDADES",
      value = format(u, big.mark = ","),
      subtitle = if (!valido) "acima do limite O2" else "volume de vendas",
      icon = "basket",
      color = "#CF8082"
    )
  })
  
  output$o_kpi_hr_card <- renderUI({
    res <- rv_opt()
    hr <- if (is.null(res)) 69 else sum(res$rh_total)
    
    kpi_card(
      title = "RH TOTAL",
      value = as.character(hr),
      subtitle = "total de funcionários",
      icon = "people",
      color = "#E49A36"
    )
  })
  
  output$o_plan_ui <- renderUI({
    if (is.null(rv_opt())) {
      div(
        class = "empty-state",
        bs_icon("cpu"),
        tags$br(),
        tags$br(),
        "Prima ",
        strong("Executar Otimizacao"),
        " para gerar o plano semanal."
      )
    } else {
      tagList(
        div(
          class = "plan-legend",
          span(tags$b("PR:"), " Promocao (%)"),
          span(tags$b("X:"), " Func. Experientes"),
          span(tags$b("J:"), " Func. Juniores"),
          span(tags$b("Atend.:"), " Clientes Atendidos")
        ),
        DTOutput("o_plan_table")
      )
    }
  })
  
  output$o_plan_table <- renderDT({
    plan <- r_plan_opt()
    
    datatable(
      plan,
      options = list(
        dom = "ft",
        pageLength = 28,
        scrollY = "380px",
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = "table-sm table-hover table-striped"
    ) %>%
      formatStyle("Loja", fontWeight = "bold", color = "#5E8E90") %>%
      formatCurrency(c("Receita", "Custo RH", "Lucro"), currency = "$", digits = 0) %>%
      formatStyle(
        "Lucro",
        color = styleInterval(0, c("#C0414F", "#4D8C6F")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Unidades",
        background = styleColorBar(plan$Unidades, "#EAF4F7"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
  
  output$o_chart_ui <- renderUI({
    plotlyOutput("o_profit_chart", height = "220px")
  })
  
  output$o_profit_chart <- renderPlotly({
    req(!is.null(input$o_objetivo))
    
    if (input$o_objetivo == "O3") {
      p <- ggplot(pareto_ok, aes(x = total_HR, y = lucro)) +
        geom_point(color = "#84B2B1", alpha = .65, size = 2.5) +
        geom_point(data = COMPROMISSO, color = "#CF8082", size = 4) +
        theme_clean() +
        labs(x = "Recursos Humanos (RH)", y = "Lucro ($)")
      
      return(plotly_clean(ggplotly(p)))
    }
    
    res_val <- r_opt_res()
    p <- plot_shaded_convergence(
      res_val$hists,
      paste("Convergencia —", input$o_metodo),
      "Avaliacoes (FES)",
      "Melhor Lucro ($)"
    )
    
    plotly_clean(ggplotly(p))
  })
  
  # ============================================================
  # TAB 3 — DSS
  # ============================================================
  observeEvent(input$d_btn, {
    pw <- get_prev_week(input$d_semana)
    rv_dss(NULL)
    
    withProgress(message = paste("A otimizar —", input$d_objetivo, "..."), value = 0, {
      S <- run_mc(pw, input$d_objetivo, 3000)
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
  
  output$d_kpi_top <- renderUI({
    S <- rv_dss()
    if (is.null(S)) S <- S1
    
    pw <- get_prev_week(input$d_semana)
    PREV <<- pw
    upper <<- calc_upper(PREV)
    
    u <- total_units(S)
    hr <- sum(round(S[seq(2, 84, 3)]) + round(S[seq(3, 84, 3)]))
    p <- profit(S)
    
    layout_columns(
      kpi_card(
        title = "LUCRO",
        value = fmt_dol(p),
        subtitle = "lucro semanal estimado",
        icon = "graph-up-arrow",
        color = "#84B2B1"
      ),
      kpi_card(
        title = "UNIDADES",
        value = format(round(u), big.mark = ","),
        subtitle = "total unidades vendidas",
        icon = "basket",
        color = "#CF8082"
      ),
      kpi_card(
        title = "RH TOTAL",
        value = as.character(hr),
        subtitle = "funcionários escalados",
        icon = "person-check",
        color = "#E49A36"
      ),
      col_widths = c(4, 4, 4)
    )
  })
  
  output$d_forecast_chart <- renderPlotly({
    pw <- get_prev_week(input$d_semana)
    
    df <- data.frame(
      Loja = rep(LOJA_NAMES, each = 7),
      Dia = rep(factor(DIAS, levels = DIAS), 4),
      Clientes = pw
    )
    
    p <- ggplot(
      df,
      aes(
        x = Dia,
        y = Clientes,
        fill = Loja,
        text = paste0(Loja, " ", Dia, ": ", round(Clientes))
      )
    ) +
      geom_col(position = "dodge", width = .7) +
      scale_fill_manual(values = LOJA_CORES) +
      theme_clean() +
      theme(
        legend.position = "bottom",
        panel.grid.major.x = element_blank()
      ) +
      labs(x = NULL, y = "Clientes Previstos", fill = NULL)
    
    plotly_clean(ggplotly(p, tooltip = "text")) %>%
      layout(legend = list(orientation = "h", y = -0.22))
  })
  
  output$d_plan_table <- renderDT({
    plan <- r_plan_dss()
    
    datatable(
      plan,
      options = list(
        dom = "ft",
        pageLength = 28,
        scrollY = "360px",
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = "table-sm table-hover table-striped"
    ) %>%
      formatStyle("Loja", fontWeight = "bold", color = "#5E8E90") %>%
      formatCurrency(c("Receita", "Custo RH", "Lucro"), currency = "$", digits = 0) %>%
      formatStyle(
        "Lucro",
        color = styleInterval(0, c("#C0414F", "#4D8C6F")),
        fontWeight = "bold"
      )
  })
}

# ---------- Launch ----------
shinyApp(ui, server)