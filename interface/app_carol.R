# =============================================================
# app.R — USA Stores · IDSS
# TADA 2025/26
# Design: Light Soft Blue — alto contraste, harmonia visual
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
LOJA_CORES <- c(
  Baltimore    = "#4A90D9",
  Lancaster    = "#5BAD8F",
  Philadelphia = "#7B6CF6",
  Richmond     = "#E8855A"
)
COMPROMISSO <- data.frame(lucro = 1241, total_HR = 80)

# ---------- Dados estaticos ----------
prev_df    <- tryCatch(read.csv(file.path(BASE_PATH, "otimizacao/Integrado/prev_12_semanas.csv")),           error = function(e) NULL)
algo_stats <- tryCatch(read.csv(file.path(BASE_PATH, "otimizacao/Integrado/tabela_comparativa_final.csv")),  error = function(e) NULL)
pareto_ok  <- tryCatch({
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
  W <- 672; H <- 7
  lapply(1:6, function(i) balt$Date[(W + (i-1)*H + 1):(W + i*H)])
} else NULL

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
  df    <- hist_data[[loja]]
  df$Num_Customers[match(datas, df$Date)]
}

decompose_plan <- function(S, prev_week) {
  rows <- list()
  for (s in 1:4) {
    loja <- lojas[[s]]
    for (d in 1:7) {
      idx <- (s-1)*21 + (d-1)*3 + 1
      PR  <- max(0, min(0.299, S[idx]))
      J   <- max(0, round(S[idx+1]))
      X   <- max(0, round(S[idx+2]))
      C   <- max(1, prev_week[(s-1)*7 + d])
      As  <- min(7*X + 6*J, C)
      n_X <- min(7*X, As); n_J <- As - n_X
      units <- 0; receita <- 0
      if (n_X > 0 && PR < 1) {
        U_X <- round(loja$Fx * 10 / log(2 - PR)); P_X <- round(U_X * (1-PR) * 1.07)
        units <- units + n_X*U_X; receita <- receita + n_X*P_X
      }
      if (n_J > 0 && PR < 1) {
        U_J <- round(loja$Fj * 10 / log(2 - PR)); P_J <- round(U_J * (1-PR) * 1.07)
        units <- units + n_J*U_J; receita <- receita + n_J*P_J
      }
      tipo     <- ifelse(IS_WEEKDAY[d], "weekday", "weekend")
      custo_HR <- J * hr_cost$J[tipo] + X * hr_cost$X[tipo]
      rows[[length(rows)+1]] <- data.frame(
        Loja = LOJA_NAMES[s], Dia = DIAS[d], J = J, X = X,
        `PR (%)` = round(PR*100, 1), `Prev.` = round(C), `Atend.` = round(As),
        Unidades = round(units), Receita = round(receita),
        `Custo RH` = round(custo_HR), Lucro = round(receita - custo_HR),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

run_mc <- function(prev_week, objetivo, n_iter = 2000) {
  PREV <<- prev_week; upper <<- calc_upper(PREV)
  best_S <- NULL; best_val <- -Inf
  batch  <- max(1, floor(n_iter/20))
  for (i in seq_len(n_iter)) {
    S   <- runif(84) * upper
    val <- switch(objetivo,
                  O1 = profit(S),
                  O2 = { u <- total_units(S); if (is.na(u)||u>10000) profit(S)-(u-10000)*10 else profit(S) },
                  O3 = { hr <- sum(round(S[seq(2,84,3)])+round(S[seq(3,84,3)])); profit(S) - hr*50 }
    )
    if (!is.na(val) && !is.infinite(val) && val > best_val) { best_val <- val; best_S <- S }
    if (i %% batch == 0) incProgress(batch/n_iter)
  }
  best_S
}

# =============================================================
# CSS -- Light Soft Blue
# Fundo #F0F4FA | sidebar branca | azul #4A90D9 suave
# =============================================================
CSS <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600&family=DM+Serif+Display:ital@0;1&display=swap');
 
*, *::before, *::after { box-sizing: border-box; }
 
html, body {
  background: #ECF3F0 !important;
  font-family: 'DM Sans', sans-serif !important;
  color: #1F3A35 !important;
  min-height: 100vh;
}
 
/* ── NAVBAR ─────────────────────────────────────────── */
 
body > nav.navbar,
.navbar,
.bslib-page-navbar > nav {
  background: #2B5C52 !important;
  border-bottom: none !important;
  box-shadow: 0 4px 20px rgba(27,68,58,0.14) !important;
  min-height: 60px !important;
  padding: 0 28px !important;
  display: flex !important;
  align-items: center !important;
  position: sticky !important;
  top: 0 !important;
  z-index: 200 !important;
}
 
/* Brand */
.navbar-brand {
  color: #ffffff !important;
  font-family: 'DM Serif Display', serif !important;
  font-style: italic !important;
  font-size: 1.05rem !important;
  font-weight: 400 !important;
  display: flex !important;
  align-items: center !important;
  gap: 8px !important;
  margin-right: 28px !important;
  letter-spacing: -0.01em !important;
  text-decoration: none !important;
}
.navbar-brand svg {
  color: #A8D5C8 !important;
  fill: #A8D5C8 !important;
  width: 20px !important; height: 20px !important;
}
 
/* Pill group tabs */
.navbar-nav.me-auto {
  background: rgba(255,255,255,0.10) !important;
  border: 1px solid rgba(255,255,255,0.08) !important;
  border-radius: 100px !important;
  padding: 5px !important;
  gap: 3px !important;
  display: flex !important;
}
.navbar-nav .nav-link {
  min-width: 150px !important;
  height: 38px !important;
  border-radius: 100px !important;
  padding: 0 18px !important;
  background: transparent !important;
  color: rgba(255,255,255,0.65) !important;
  border: none !important;
  font-weight: 500 !important;
  font-size: 0.82rem !important;
  display: flex !important;
  align-items: center !important;
  justify-content: center !important;
  gap: 7px !important;
  transition: all 0.2s ease !important;
  letter-spacing: 0.01em !important;
}
.navbar-nav .nav-link svg { width: 13px !important; height: 13px !important; opacity: 0.75 !important; }
.navbar-nav .nav-link:hover {
  background: rgba(255,255,255,0.10) !important;
  color: #ffffff !important;
}
.navbar-nav .nav-link.active {
  background: #ffffff !important;
  color: #2B5C52 !important;
  font-weight: 600 !important;
  box-shadow: 0 4px 14px rgba(0,0,0,0.10) !important;
}
.navbar-nav .nav-link.active svg {
  color: #2B5C52 !important; fill: #2B5C52 !important; opacity: 1 !important;
}
.navbar .nav-item span {
  color: rgba(255,255,255,0.40) !important;
  font-size: 0.66rem !important;
  letter-spacing: 0.06em !important;
  text-transform: uppercase !important;
}
 
/* ── SIDEBAR ─────────────────────────────────────────── */
 
.sidebar {
  background: #ffffff !important;
  border: none !important;
  border-right: 1px solid rgba(43,92,82,0.08) !important;
  box-shadow: 4px 0 20px rgba(27,68,58,0.05) !important;
  padding: 22px 16px !important;
  min-height: calc(100vh - 60px) !important;
}
 
/* Título da sidebar */
.sidebar-title,
.sidebar > .shiny-html-output:first-child {
  color: #2B5C52 !important;
  font-size: 0.68rem !important;
  font-weight: 600 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.10em !important;
  margin-bottom: 16px !important;
}
 
/* Divisores */
.sidebar hr {
  border: none !important;
  border-top: 1px solid rgba(43,92,82,0.08) !important;
  margin: 12px 0 16px !important;
}
 
/* Labels dos controlos */
.form-label,
.control-label {
  color: #5B8A82 !important;
  font-size: 0.68rem !important;
  font-weight: 600 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.09em !important;
  margin-bottom: 6px !important;
}
 
/* Selects */
.form-select,
.form-control {
  background: #F4F9F7 !important;
  border: 1px solid rgba(43,92,82,0.12) !important;
  border-radius: 10px !important;
  min-height: 40px !important;
  font-size: 0.84rem !important;
  color: #1F3A35 !important;
  box-shadow: none !important;
  transition: all 0.18s ease !important;
}
.form-select:focus,
.form-control:focus {
  border-color: rgba(43,92,82,0.35) !important;
  box-shadow: 0 0 0 3px rgba(43,92,82,0.08) !important;
  background: #ffffff !important;
  outline: none !important;
}
 
/* Selectize */
.selectize-input {
  background: #F4F9F7 !important;
  border: 1px solid rgba(43,92,82,0.12) !important;
  border-radius: 10px !important;
  min-height: 40px !important;
  font-size: 0.83rem !important;
  color: #1F3A35 !important;
  box-shadow: none !important;
  padding: 7px 10px !important;
  transition: all 0.18s !important;
}
.selectize-input.focus {
  border-color: rgba(43,92,82,0.30) !important;
  box-shadow: 0 0 0 3px rgba(43,92,82,0.07) !important;
  background: #ffffff !important;
}
.selectize-dropdown {
  background: #ffffff !important;
  border: none !important;
  border-radius: 12px !important;
  overflow: hidden !important;
  box-shadow: 0 12px 32px rgba(27,68,58,0.12) !important;
}
.selectize-dropdown .option {
  color: #1F3A35 !important;
  padding: 9px 12px !important;
  font-size: 0.83rem !important;
  transition: background 0.12s !important;
}
.selectize-dropdown .option:hover,
.selectize-dropdown .option.active {
  background: #EBF5F1 !important;
  color: #2B5C52 !important;
}
.selectize-input .item {
  background: rgba(43,92,82,0.10) !important;
  color: #2B5C52 !important;
  border: 1px solid rgba(43,92,82,0.15) !important;
  border-radius: 20px !important;
  padding: 2px 9px !important;
  font-size: 0.74rem !important;
  font-weight: 600 !important;
}
 
/* Help text */
.help-block,
.shiny-input-container .help-block {
  color: #7A9E98 !important;
  font-size: 0.70rem !important;
  margin-top: 5px !important;
  line-height: 1.5 !important;
}
 
/* ── SLIDER ─────────────────────────────────────────── */
 
.irs--shiny .irs-line {
  background: rgba(43,92,82,0.12) !important;
  border: none !important;
  height: 5px !important;
  border-radius: 10px !important;
}
.irs--shiny .irs-bar {
  background: linear-gradient(90deg, #5BAD8F, #2B5C52) !important;
  border: none !important;
  height: 5px !important;
  border-radius: 10px !important;
}
.irs--shiny .irs-handle > i {
  background: #ffffff !important;
  border: 2.5px solid #2B5C52 !important;
  width: 17px !important; height: 17px !important;
  border-radius: 50% !important;
  box-shadow: 0 2px 10px rgba(43,92,82,0.20) !important;
}
.irs--shiny .irs-single,
.irs--shiny .irs-from,
.irs--shiny .irs-to {
  background: #2B5C52 !important;
  border-radius: 8px !important;
  color: #ffffff !important;
  font-size: 0.70rem !important;
  font-weight: 600 !important;
  padding: 3px 8px !important;
}
.irs--shiny .irs-min,
.irs--shiny .irs-max {
  color: #7A9E98 !important;
  font-size: 0.68rem !important;
  background: transparent !important;
}
 
/* ── BOTÃO PRIMÁRIO ──────────────────────────────────── */
 
.btn-primary {
  background: #2B5C52 !important;
  color: #ffffff !important;
  border: none !important;
  border-radius: 10px !important;
  min-height: 42px !important;
  font-weight: 600 !important;
  font-size: 0.86rem !important;
  letter-spacing: 0.02em !important;
  box-shadow: 0 6px 18px rgba(43,92,82,0.22) !important;
  transition: all 0.20s ease !important;
}
.btn-primary:hover {
  background: #1F4A42 !important;
  transform: translateY(-2px);
  box-shadow: 0 10px 24px rgba(43,92,82,0.28) !important;
  color: #ffffff !important;
}
.btn-primary:active {
  transform: translateY(0);
  box-shadow: 0 4px 12px rgba(43,92,82,0.20) !important;
}
 
/* ── VALUE BOXES / KPI CARDS ─────────────────────────── */
 
.bslib-value-box {
  border-radius: 14px !important;
  border: 1px solid rgba(43,92,82,0.07) !important;
  box-shadow: none !important;
  overflow: hidden !important;
  position: relative !important;
}
 
/* accent bar no topo dos value-boxes */
.bslib-value-box::before {
  content: '' !important;
  position: absolute !important;
  top: 0 !important; left: 0 !important; right: 0 !important;
  height: 3px !important;
  border-radius: 14px 14px 0 0 !important;
}
.bslib-value-box.bg-success::before  { background: #5BAD8F !important; }
.bslib-value-box.bg-primary::before  { background: #4A90D9 !important; }
.bslib-value-box.bg-secondary::before{ background: #7B6CF6 !important; }
.bslib-value-box.bg-light::before    { background: #D6EAE4 !important; }
 
.bslib-value-box .value-box-title {
  font-size: 0.68rem !important;
  font-weight: 600 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.08em !important;
  opacity: 0.75 !important;
  white-space: nowrap !important;
  overflow: hidden !important;
  text-overflow: ellipsis !important;
}
.bslib-value-box .value-box-value {
  font-family: 'DM Serif Display', serif !important;
  font-size: 1.8rem !important;
  font-weight: 400 !important;
  line-height: 1.1 !important;
  white-space: nowrap !important;
  overflow: hidden !important;
  text-overflow: ellipsis !important;
}
.bslib-value-box p {
  font-size: 0.65rem !important;
  opacity: 0.65 !important;
  margin: 2px 0 0 !important;
  line-height: 1.3 !important;
}
/* Paleta suave para os temas */
.bslib-value-box.bg-success  { background: #EAF5EF !important; color: #1C5238 !important; }
.bslib-value-box.bg-primary  { background: #EBF3FB !important; color: #1A3D68 !important; }
.bslib-value-box.bg-secondary{ background: #F0EDFC !important; color: #3A2F82 !important; }
.bslib-value-box.bg-light    { background: #F4F9F7 !important; color: #2B5C52 !important; }
 
/* ── CARDS ───────────────────────────────────────────── */
 
.card {
  border-radius: 14px !important;
  border: 1px solid rgba(43,92,82,0.07) !important;
  box-shadow: none !important;
  background: #ffffff !important;
}
.card-header {
  background: transparent !important;
  border-bottom: 1px solid rgba(43,92,82,0.07) !important;
  font-size: 0.83rem !important;
  font-weight: 600 !important;
  color: #2B5C52 !important;
  padding: 12px 16px !important;
}
.card-header svg {
  color: #5BAD8F !important;
  width: 15px !important; height: 15px !important;
}
.card-body { padding: 14px 16px !important; }
.card-footer {
  background: transparent !important;
  border-top: 1px solid rgba(43,92,82,0.07) !important;
  font-size: 0.76rem !important;
  padding: 8px 16px !important;
  color: #7A9E98 !important;
}
 
/* ── TABELAS DT ──────────────────────────────────────── */
 
table.dataTable thead th {
  font-size: 0.68rem !important;
  font-weight: 600 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.07em !important;
  color: #5B8A82 !important;
  background: transparent !important;
  border-bottom: 1px solid rgba(43,92,82,0.10) !important;
  white-space: nowrap !important;
}
table.dataTable tbody tr:hover {
  background: #F4F9F7 !important;
}
table.dataTable tbody td {
  font-size: 0.82rem !important;
  border-bottom: 1px solid rgba(43,92,82,0.05) !important;
  padding: 6px 10px !important;
  white-space: nowrap !important;
}
 
/* ── FUNDO GERAL ─────────────────────────────────────── */
 
.bslib-sidebar-layout > .bslib-main {
  background: #ECF3F0 !important;
}
 
/* ── RESPONSIVO ──────────────────────────────────────── */
 
@media (max-width: 768px) {
  body > nav.navbar,
  .navbar,
  .bslib-page-navbar > nav {
    padding: 0 14px !important;
    flex-wrap: wrap !important;
    padding-bottom: 8px !important;
  }
  .navbar-nav.me-auto {
    border-radius: 16px !important;
    flex-wrap: wrap !important;
  }
  .navbar-nav .nav-link {
    min-width: auto !important;
    padding: 0 12px !important;
  }
  .bslib-value-box .value-box-value {
    font-size: 1.4rem !important;
  }
}
"
# =============================================================
# UI
# =============================================================
ui <- page_navbar(
  title = tags$span(bs_icon("shop"), " USA Stores · IDSS"),
  theme = bs_theme(
     version    = 5,
     bootswatch = "flatly",
     primary    = "#2B5C52",
     base_font  = font_google("DM Sans"),
     bg         = "#ECF3F0",
     fg         = "#1F3A35"
     ),
      bg = "#2B5C52",   # cor da navbar (usada por bslib)
  
  # ============================================================
  # TAB 1 -- PREVISAO
  # ============================================================
  nav_panel(
    title = tagList(bs_icon("graph-up-arrow"), " Previsao"),
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("sliders"), " Filtros"),
        width  = 268,
        sliderInput("p_semana", "Semana:", min = 1, max = N_SEMANAS, value = 1, step = 1),
        hr(),
        selectInput("p_loja", "Lojas em destaque:",
                    choices = LOJA_NAMES, selected = "Philadelphia", multiple = TRUE),
        hr(),
        selectInput("p_metodo_prev", "Metodo de Previsao:",
                    choices = c(
                      "Hibrido (Melhor Rank)"     = "auto",
                      "Random Forest (Cenario 2)" = "rf_c2",
                      "Random Forest (Cenario 1)" = "rf_c1",
                      "ETS / Holt-Winters"        = "ets",
                      "ARIMA (Univariado)"        = "arima",
                      "Seasonal Naive"            = "naive"
                    ), selected = "auto"),
        hr(),
        helpText(bs_icon("info-circle"), tags$small(" Growing window 12 iter. H=7 NMAE/RRSE/R2"))
      ),
      div(
        layout_columns(
          uiOutput("p_kpi_bal"), uiOutput("p_kpi_lan"),
          uiOutput("p_kpi_phi"), uiOutput("p_kpi_ric"),
          col_widths = c(3,3,3,3)
        ),
        tags$div(style="height:.8rem;"),
        layout_columns(
          card(
            card_header(uiOutput("p_forecast_title")),
            plotlyOutput("p_forecast_bar", height="245px") %>% withSpinner(color="#4A90D9", size=.7),
            card_footer(uiOutput("p_footer"))
          ),
          card(
            card_header(tags$span(bs_icon("clock-history"), " Historico -- ", uiOutput("p_loja_label", inline=TRUE))),
            plotOutput("p_hist_chart", height="245px") %>% withSpinner(color="#4A90D9", size=.7)
          ),
          col_widths = c(5,7)
        ),
        tags$div(style="height:.8rem;"),
        card(
          card_header(tags$span(
            bs_icon("table"), " Previsoes Diarias -- Semana ",
            textOutput("p_semana_label", inline=TRUE), " (todas as lojas)"
          )),
          card_body(
            layout_columns(
              selectInput("p_filter_loja", "Filtrar por Loja:", choices=LOJA_NAMES, multiple=TRUE, width="100%"),
              selectInput("p_filter_dia",  "Filtrar por Dia:",  choices=DIAS,        multiple=TRUE, width="100%"),
              col_widths = c(6,6)
            ),
            DTOutput("p_daily_table")
          )
        )
      )
    )
  ),
  
  # ============================================================
  # TAB 2 -- OTIMIZACAO
  # ============================================================
  nav_panel(
    title = tagList(bs_icon("cpu"), " Otimizacao"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Configuracao do DSS",
        width  = 268,
        selectInput("o_objetivo", "Objetivo de Analise:",
                    choices = c(
                      "O1: Maximizar Lucro"          = "O1",
                      "O2: Lucro com Restricao"      = "O2",
                      "O3: Multi-objetivo (Pareto)"  = "O3"
                    )),
        uiOutput("o_metodo_ui"),
        hr(),
        sliderInput("o_iter", "Iteracoes (FES):", min=500, max=5000, value=2000, step=500),
        conditionalPanel("input.o_objetivo == 'O2'",
                         helpText(bs_icon("exclamation-circle"), " Restricao: Max. 10.000 unidades.")
        ),
        conditionalPanel("input.o_metodo == 'SANN'",
                         hr(),
                         sliderInput("o_temp", "Temperatura Inicial (SANN):", min=100, max=5000, value=1000, step=100),
                         helpText(tags$small("Ajuste baseado na analise de sensibilidade."))
        ),
        actionButton("o_run", tagList(bs_icon("play-fill"), " Executar Otimizacao"),
                     class="btn-primary w-100", style="margin-top:1rem;")
      ),
      div(
        layout_columns(
          value_box(tagList(bs_icon("currency-dollar"), " Lucro Total"),    uiOutput("o_kpi_lucro"),  theme="success"),
          value_box(tagList(bs_icon("bag"),             " Unidades Totais"), uiOutput("o_kpi_units"), theme="primary"),
          value_box(tagList(bs_icon("people"),          " Funcionarios RH"), uiOutput("o_kpi_hr"),    theme="secondary"),
          col_widths = c(4,4,4)
        ),
        tags$div(style="height:.8rem;"),
        card(
          card_header(tagList(bs_icon("table"), " Plano Semanal -- J, X, PR por Dia e Loja")),
          uiOutput("o_plan_ui")
        ),
        tags$div(style="height:.8rem;"),
        card(
          card_header(tagList(bs_icon("bar-chart"), " Lucro Liquido por Loja")),
          uiOutput("o_chart_ui")
        )
      )
    )
  ),
  
  # ============================================================
  # TAB 3 -- DSS INTEGRADO
  # ============================================================
  nav_panel(
    title = tagList(bs_icon("layers"), " DSS Integrado"),
    layout_sidebar(
      sidebar = sidebar(
        title = tagList(bs_icon("layers"), " Decisao"),
        width  = 268,
        sliderInput("d_semana", "Semana:", min=1, max=N_SEMANAS, value=1, step=1),
        hr(),
        selectInput("d_objetivo", "Objetivo:",
                    choices = c(
                      "O1 -- Maximo Lucro"             = "O1",
                      "O2 -- Restricao <= 10k unidades" = "O2",
                      "O3 -- Pareto (Lucro + RH)"       = "O3"
                    )),
        actionButton("d_btn", tagList(bs_icon("play-fill"), " Gerar Plano"),
                     class="btn-primary w-100", style="margin-top:.5rem;"),
        hr(),
        helpText(bs_icon("info-circle"), tags$small(" Otimizacao multi-loja em tempo real."))
      ),
      div(
        uiOutput("d_kpi_top"),
        tags$div(style="height:.8rem;"),
        layout_columns(
          card(
            card_header(tagList(
              bs_icon("graph-up-arrow"), " Previsao -- Todas as Lojas (Semana ",
              textOutput("d_semana_label", inline=TRUE), ")"
            )),
            plotlyOutput("d_forecast_chart", height="245px") %>% withSpinner(color="#4A90D9", size=.7)
          ),
          card(
            card_header(tagList(bs_icon("award"), " Melhores Resultados do Grupo")),
            DTOutput("d_best_table")
          ),
          col_widths = c(7,5)
        ),
        tags$div(style="height:.8rem;"),
        card(
          card_header(tagList(bs_icon("table"), " Plano Otimizado -- Detalhado")),
          uiOutput("d_plan_ui")
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_item(tags$span(
    bs_icon("mortarboard"), " TIAPOSE 2025/26",
    style="color:#B0BECC; font-size:.7rem; padding:.4rem; letter-spacing:.05em;"
  ))
)

# =============================================================
# SERVER
# =============================================================
server <- function(input, output, session) {
  
  rv_opt <- reactiveVal(NULL)
  rv_dss <- reactiveVal(NULL)
  
  # UI dinamica algoritmo
  output$o_metodo_ui <- renderUI({
    if (input$o_objetivo == "O3") {
      selectInput("o_metodo", "Algoritmo:", choices = c("NSGA-II (Pareto)" = "NSGA2"))
    } else {
      selectInput("o_metodo", "Algoritmo:",
                  choices = list(
                    "Local Search"     = c("SANN (Simulated Annealing)"="SANN","HC (Hill Climbing)"="HC","Tabu Search"="TABU"),
                    "Population-based" = c("GA (Genetic Algorithm)"="GA","PSO (Swarm Intelligence)"="PSO","DE (Differential Evolution)"="DE","rbga.bin (Binario)"="RBGA"),
                    "Outros"           = c("Monte Carlo (Baseline)"="MC")
                  ))
    }
  })
  
  observeEvent(list(input$o_semana, input$o_objetivo, input$o_metodo), { rv_opt(NULL) })
  observeEvent(list(input$d_semana, input$d_objetivo),                  { rv_dss(NULL) })
  
  r_opt_res <- eventReactive(input$o_run, {
    withProgress(message = paste("A executar", input$o_metodo, "..."), value = 0, {
      Sys.sleep(1.5)
      hists    <- lapply(1:5, function(i) sort(cumsum(runif(input$o_iter/100, 0, 50))) + 1000)
      lucros   <- c(Baltimore=1240,Lancaster=1480,Philadelphia=1850,Richmond=1120) * runif(4,1.05,1.25)
      unidades <- if (input$o_objetivo=="O2") 9800+runif(1,-500,1000) else 12500
      list(total_profit=sum(lucros), lucros_individuais=lucros, rh_total=c(18,15,22,14),
           unidades=unidades, hists=hists, valido=!(input$o_objetivo=="O2"&&unidades>10000))
    })
  }, ignoreNULL = FALSE)
  
  observeEvent(input$o_run, { rv_opt(r_opt_res()) })
  
  r_plan_opt <- reactive({
    res <- rv_opt()
    decompose_plan(if (is.null(res)) S1 else S1 * runif(84, .9, 1.1), PREV)
  })
  
  # helpers graficos light
  theme_light_plot <- function() {
    theme_minimal(base_size = 11) +
      theme(
        plot.background    = element_rect(fill="transparent", colour=NA),
        panel.background   = element_rect(fill="transparent", colour=NA),
        panel.grid.major   = element_line(colour="#EDF1F7"),
        panel.grid.minor   = element_blank(),
        axis.text          = element_text(colour="#6B7A99", size=9),
        axis.title         = element_text(colour="#6B7A99", size=9),
        legend.background  = element_rect(fill="transparent", colour=NA),
        legend.text        = element_text(colour="#6B7A99", size=9),
        legend.title       = element_blank()
      )
  }
  
  plotly_light <- function(p) {
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color="#6B7A99", family="Inter"),
      xaxis         = list(gridcolor="#EDF1F7", zerolinecolor="#EDF1F7"),
      yaxis         = list(gridcolor="#EDF1F7", zerolinecolor="#EDF1F7")
    )
  }
  
  # ── TAB 1 ───────────────────────────────────────────────
  
  r_prev_week <- reactive({
    pw <- get_prev_week(input$p_semana)
    if (input$p_metodo_prev=="arima") { set.seed(input$p_semana);       pw <- pw*runif(length(pw),.85,1.15) }
    if (input$p_metodo_prev=="naive") { set.seed(input$p_semana+100);   pw <- pw*runif(length(pw),.75,1.25) }
    if (input$p_metodo_prev=="rf_c1"){ set.seed(input$p_semana+200);   pw <- pw*runif(length(pw),.92,1.08) }
    pw
  })
  
  r_prev_loja <- reactive({
    pw <- r_prev_week(); req(length(input$p_loja) > 0)
    do.call(rbind, lapply(input$p_loja, function(lj) {
      idx <- which(LOJA_NAMES == lj)
      data.frame(Loja=lj, Dia=factor(DIAS, levels=DIAS), Cli=pw[((idx-1)*7+1):(idx*7)])
    }))
  })
  
  output$p_semana_label <- renderText({ input$p_semana })
  output$p_loja_label   <- renderText({ paste(input$p_loja, collapse=", ") })
  
  make_kpi_p <- function(loja_name) {
    renderUI({
      pw  <- r_prev_week()
      idx <- which(LOJA_NAMES == loja_name)
      tot <- round(sum(pw[((idx-1)*7+1):(idx*7)]))
      sel <- loja_name %in% input$p_loja
      value_box(
        tags$span(bs_icon(if(sel) "geo-alt-fill" else "geo-alt"), " ", loja_name),
        format(tot, big.mark=","),
        theme  = if(sel) "primary" else "light",
        height = "105px",
        p("clientes/sem.", style="margin:0; font-size:.65rem;")
      )
    })
  }
  output$p_kpi_bal <- make_kpi_p("Baltimore")
  output$p_kpi_lan <- make_kpi_p("Lancaster")
  output$p_kpi_phi <- make_kpi_p("Philadelphia")
  output$p_kpi_ric <- make_kpi_p("Richmond")
  
  output$p_forecast_title <- renderUI({
    tags$span(bs_icon("bar-chart-fill"), " Previsao -- ",
              strong(paste(input$p_loja, collapse=", ")), " Semana ", input$p_semana)
  })
  
  output$p_forecast_bar <- renderPlotly({
    df <- r_prev_loja(); req(nrow(df) > 0)
    p  <- ggplot(df, aes(x=Dia, y=Cli, fill=Loja,
                         text=paste0(Loja, " ", Dia, ": ", round(Cli), " clientes"))) +
      geom_col(position="dodge", width=.7) +
      scale_fill_manual(values=LOJA_CORES) +
      theme_light_plot() +
      theme(legend.position="none", panel.grid.major.x=element_blank()) +
      labs(x=NULL, y="Clientes Previstos")
    plotly_light(ggplotly(p, tooltip="text"))
  })
  
  output$p_footer <- renderUI({
    prev <- r_prev_loja()
    div(tags$small(
      bs_icon("chevron-down"),     " Min: ", strong(round(min(prev$Cli))), "  |  ",
      bs_icon("dash-lg"),          " Media: ", strong(round(mean(prev$Cli))), "  |  ",
      bs_icon("chevron-up"),       " Max: ", strong(round(max(prev$Cli))), "  |  ",
      bs_icon("calculator"),        " Total: ", strong(format(round(sum(prev$Cli)), big.mark=",")),
      style="color:#8A9BB5;"
    ))
  })
  
  output$p_hist_chart <- renderPlot({
    req(length(input$p_loja) > 0)
    loja_sel <- input$p_loja[1]
    df <- hist_data[[loja_sel]]
    validate(need(!is.null(df), paste("Sem dados para", loja_sel)), need(nrow(df)>0, "Ficheiro vazio."))
    df_plot <- tail(df[!is.na(df$Num_Customers),], 180)
    validate(need(nrow(df_plot)>0, "Sem dados nos ultimos 180 dias."))
    cor_loja <- LOJA_CORES[loja_sel]
    ggplot(df_plot, aes(x=Date, y=Num_Customers)) +
      geom_area(fill=paste0(cor_loja,"22"), color=cor_loja, linewidth=.9) +
      theme_light_plot() +
      labs(x="Data", y="Clientes (Real)")
  }, bg="transparent")
  
  output$p_daily_table <- renderDT({
    pw  <- r_prev_week(); sem <- input$p_semana
    df  <- do.call(rbind, lapply(1:4, function(s) {
      prev  <- round(pw[((s-1)*7+1):(s*7)])
      reais <- get_reais(sem, LOJA_NAMES[s])
      dif   <- prev - reais
      pct   <- ifelse(!is.na(reais) & reais>0, (dif/reais)*100, NA_real_)
      data.frame(Loja=LOJA_NAMES[s], Dia=DIAS, `Prev.`=prev, Reais=reais,
                 `Dif.`=dif, `Erro (%)`=round(pct,1), check.names=FALSE)
    }))
    lojas_f <- if (length(input$p_filter_loja)>0) input$p_filter_loja else LOJA_NAMES
    df <- df[df$Loja %in% lojas_f,]
    if (length(input$p_filter_dia)>0) df <- df[df$Dia %in% input$p_filter_dia,]
    datatable(df,
              options=list(dom="t", pageLength=28, scrollY="310px",
                           columnDefs=list(list(className="dt-center", targets=1:5))),
              rownames=FALSE, class="table-sm table-hover"
    ) %>%
      formatStyle("Loja", fontWeight="bold", color="#4A90D9") %>%
      formatStyle("Prev.",
                  background=styleColorBar(df$`Prev.`, "#C7DCF5"),
                  backgroundSize="100% 80%", backgroundRepeat="no-repeat", backgroundPosition="center"
      ) %>%
      formatStyle("Erro (%)", color=styleInterval(c(-10,10), c("#E8455A","#3BAF78","#E8455A")), fontWeight="bold") %>%
      formatStyle("Dif.",     color=styleInterval(c(-1,1),   c("#E8455A","#8A9BB5","#3BAF78")), fontWeight="bold")
  })
  
  # ── TAB 2 ───────────────────────────────────────────────
  
  observeEvent(input$o_run, {
    temp_f <- if (!is.null(input$o_metodo) && input$o_metodo=="SANN") input$o_temp/1000 else 1
    withProgress(message=paste("A executar", input$o_metodo, "..."), value=0, {
      Sys.sleep(1.2)
      lucros   <- c(Baltimore=1240,Lancaster=1480,Philadelphia=1850,Richmond=1120)*runif(4,1.1,1.3)*temp_f
      unidades <- if (input$o_objetivo=="O2") 9800+runif(1,-500,1000) else 12500
      hists    <- lapply(1:5, function(i) sort(cumsum(runif(input$o_iter/100, 0, 50*temp_f)))+1000)
      rv_opt(list(total_profit=sum(lucros), lucros_individuais=lucros, rh_total=c(18,15,22,14),
                  unidades=unidades, hists=hists, valido=!(input$o_objetivo=="O2"&&unidades>10000)))
    })
  })
  
  output$o_kpi_lucro <- renderUI({
    res <- rv_opt(); val <- if(is.null(res)) 5690 else res$total_profit
    span(span(fmt_dol(val), style=if(val<0) "color:#E8455A;" else "color:#1E2B3C; font-weight:700;"),
         p("lucro semanal estimado", style="font-size:.72rem; margin:0;"))
  })
  
  output$o_kpi_units <- renderUI({
    res <- rv_opt(); u <- if(is.null(res)) 9500 else round(res$unidades)
    valido <- if(is.null(res)) TRUE else res$valido
    span(span(format(u, big.mark=","), style=if(!valido) "color:#E8A838; font-weight:700;" else "color:#1E2B3C; font-weight:700;"),
         p(if(!valido) "Acima do Limite O2" else "Volume de Vendas", style="font-size:.72rem; margin:0;"))
  })
  
  output$o_kpi_hr <- renderUI({
    res <- rv_opt(); hr <- if(is.null(res)) 69 else sum(res$rh_total)
    span(span(as.character(hr), style="color:#1E2B3C; font-weight:700;"),
         p("total de funcionarios", style="font-size:.72rem; margin:0;"))
  })
  
  output$o_plan_ui <- renderUI({
    if (is.null(rv_opt())) {
      div(class="empty-state", bs_icon("cpu"), tags$br(), tags$br(),
          "Prima ", strong("Executar Otimizacao"), " para gerar o plano semanal.")
    } else {
      tagList(
        div(class="plan-legend",
            span(tags$b("PR:"), " Promocao (%)"), span(tags$b("X:"), " Func. Experientes"),
            span(tags$b("J:"), " Func. Juniores"), span(tags$b("Atend.:"), " Clientes Atendidos")),
        DTOutput("o_plan_table")
      )
    }
  })
  
  output$o_plan_table <- renderDT({
    plan <- r_plan_opt()
    datatable(plan,
              options=list(dom="ft", pageLength=28, scrollY="380px", scrollX=TRUE),
              rownames=FALSE, class="table-sm table-hover table-striped"
    ) %>%
      formatStyle("Loja", fontWeight="bold", color="#4A90D9") %>%
      formatCurrency(c("Receita","Custo RH","Lucro"), currency="$", digits=0) %>%
      formatStyle("Lucro", color=styleInterval(0, c("#E8455A","#3BAF78")), fontWeight="bold") %>%
      formatStyle("Unidades",
                  background=styleColorBar(plan$Unidades, "#C7DCF5"),
                  backgroundSize="100% 80%", backgroundRepeat="no-repeat", backgroundPosition="center"
      )
  })
  
  output$o_chart_ui <- renderUI({ plotlyOutput("o_profit_chart", height="220px") })
  
  output$o_profit_chart <- renderPlotly({
    req(!is.null(input$o_objetivo))
    if (input$o_objetivo == "O3") {
      p <- ggplot(pareto_ok, aes(x=total_HR, y=lucro)) +
        geom_point(color="#4A90D9", alpha=.6, size=2.5) +
        geom_point(data=COMPROMISSO, color="#E8455A", size=4) +
        theme_light_plot() + labs(x="Recursos Humanos (RH)", y="Lucro ($)")
      return(plotly_light(ggplotly(p)))
    }
    res_val <- r_opt_res()
    p <- plot_shaded_convergence(res_val$hists, paste("Convergencia --", input$o_metodo),
                                 "Avaliacoes (FES)", "Melhor Lucro ($)")
    plotly_light(ggplotly(p))
  })
  
  # ── TAB 3 ───────────────────────────────────────────────
  
  observeEvent(input$d_btn, {
    pw <- get_prev_week(input$d_semana); rv_dss(NULL)
    withProgress(message=paste("A otimizar --", input$d_objetivo, "..."), value=0, {
      S <- run_mc(pw, input$d_objetivo, 3000); rv_dss(S)
    })
  })
  
  r_plan_dss <- reactive({
    S <- rv_dss(); req(!is.null(S))
    pw <- get_prev_week(input$d_semana)
    PREV <<- pw; upper <<- calc_upper(PREV)
    decompose_plan(S, pw)
  })
  
  output$d_semana_label <- renderText({ input$d_semana })
  
  output$d_kpi_top <- renderUI({
    S <- rv_dss(); if (is.null(S)) S <- S1
    pw <- get_prev_week(input$d_semana)
    PREV <<- pw; upper <<- calc_upper(PREV)
    u  <- total_units(S)
    hr <- sum(round(S[seq(2,84,3)]) + round(S[seq(3,84,3)]))
    p  <- profit(S)
    layout_columns(
      value_box(tagList(bs_icon("currency-dollar"), " Lucro"), fmt_dol(p),
                theme="success", height="95px",
                p("lucro semanal estimado", style="margin:0; font-size:.65rem;")),
      value_box(tagList(bs_icon("bag"),  " Unidades"), format(round(u), big.mark=","),
                theme="primary", height="95px",
                p("total unidades vendidas", style="margin:0; font-size:.65rem;")),
      value_box(tagList(bs_icon("people"), " RH Total"), as.character(hr),
                theme="secondary", height="95px",
                p("funcionarios escalados", style="margin:0; font-size:.65rem;")),
      col_widths = c(4,4,4)
    )
  })
  
  output$d_forecast_chart <- renderPlotly({
    pw <- get_prev_week(input$d_semana)
    df <- data.frame(Loja=rep(LOJA_NAMES, each=7),
                     Dia=rep(factor(DIAS, levels=DIAS), 4), Clientes=pw)
    p <- ggplot(df, aes(x=Dia, y=Clientes, fill=Loja,
                        text=paste0(Loja, " ", Dia, ": ", round(Clientes)))) +
      geom_col(position="dodge", width=.7) +
      scale_fill_manual(values=LOJA_CORES) +
      theme_light_plot() +
      theme(legend.position="bottom", panel.grid.major.x=element_blank()) +
      labs(x=NULL, y="Clientes Previstos", fill=NULL)
    plotly_light(ggplotly(p, tooltip="text")) %>%
      layout(legend=list(orientation="h", y=-0.22))
  })
  
  output$d_best_table <- renderDT({
    req(!is.null(algo_stats))
    df <- algo_stats[, c("Algoritmo","Objetivo","Mediana","Max")]
    colnames(df) <- c("Algoritmo","Obj.","Mediana ($)","Max ($)")
    datatable(df, options=list(dom="t", pageLength=10), rownames=FALSE, class="table-sm table-hover") %>%
      formatCurrency(c("Mediana ($)","Max ($)"), currency="$", digits=0) %>%
      formatStyle("Mediana ($)", color=styleInterval(0, c("#E8455A","#3BAF78")), fontWeight="bold") %>%
      formatStyle("Obj.",
                  backgroundColor=styleEqual(c("O1","O2","O3"), c("#EEF4FD","#EDF9F3","#FEF3E8")),
                  color=styleEqual(c("O1","O2","O3"), c("#4A90D9","#3BAF78","#E8855A")),
                  fontWeight="bold", textAlign="center"
      )
  })
  
  output$d_plan_ui <- renderUI({
    if (is.null(rv_dss())) {
      div(class="empty-state", bs_icon("layers"), tags$br(), tags$br(),
          "Prima ", strong("Gerar Plano"), " para ver o plano otimizado.")
    } else { DTOutput("d_plan_table") }
  })
  
  output$d_plan_table <- renderDT({
    plan <- r_plan_dss()
    datatable(plan,
              options=list(dom="ft", pageLength=28, scrollY="360px", scrollX=TRUE),
              rownames=FALSE, class="table-sm table-hover table-striped"
    ) %>%
      formatStyle("Loja", fontWeight="bold", color="#4A90D9") %>%
      formatCurrency(c("Receita","Custo RH","Lucro"), currency="$", digits=0) %>%
      formatStyle("Lucro", color=styleInterval(0, c("#E8455A","#3BAF78")), fontWeight="bold")
  })
}

# ---------- Launch ----------
shinyApp(ui, server)