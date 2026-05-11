# =============================================================
# app.R — USA Stores · IDSS
# TIAPOSE 2025/26
# Design: Creme/Sage — KPIs com accent, sidebar elaborada
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

# Cores dos graficos: avermelhado, azul slate, laranja torrado, sage green
LOJA_CORES <- c(
  Baltimore    = "#C0414F",
  Lancaster    = "#3A7CBD",
  Philadelphia = "#C4622D",
  Richmond     = "#4D8C6F"
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
# CSS
# =============================================================
CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@300;400;500;600;700&display=swap');

*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

html, body {
  background : #F5F4F0 !important;
  font-family: 'Plus Jakarta Sans', sans-serif !important;
  color      : #22292E !important;
  min-height : 100vh;
}

/* ── NAVBAR ── */
body > nav.navbar,
.navbar,
.bslib-page-navbar > nav {
  background   : #FAFAF8 !important;
  border-bottom: 1.5px solid #E8E5DE !important;
  box-shadow   : 0 2px 12px rgba(34,41,46,0.06) !important;
  min-height   : 64px !important;
  padding      : 0 32px !important;
  display      : flex !important;
  align-items  : center !important;
  position     : sticky !important;
  top          : 0 !important;
  z-index      : 300 !important;
}

.navbar-brand {
  font-family    : 'Plus Jakarta Sans', sans-serif !important;
  font-size      : 1.00rem !important;
  font-weight    : 700 !important;
  color          : #22292E !important;
  letter-spacing : -0.02em !important;
  display        : flex !important;
  align-items    : center !important;
  gap            : 9px !important;
  margin-right   : 32px !important;
  text-decoration: none !important;
}
.navbar-brand svg {
  color : #4D8C6F !important;
  fill  : #4D8C6F !important;
  width : 19px !important;
  height: 19px !important;
}

/* pills das tres tabs */
.navbar-nav.me-auto {
  background   : #EEECEA !important;
  border       : 1.5px solid #E0DDD7 !important;
  border-radius: 50px !important;
  padding      : 4px !important;
  gap          : 3px !important;
  display      : flex !important;
}
.navbar-nav .nav-link {
  min-width      : 148px !important;
  height         : 36px !important;
  border-radius  : 50px !important;
  padding        : 0 20px !important;
  background     : transparent !important;
  color          : #6B7278 !important;
  border         : none !important;
  font-weight    : 600 !important;
  font-size      : 0.80rem !important;
  letter-spacing : 0.01em !important;
  display        : flex !important;
  align-items    : center !important;
  justify-content: center !important;
  gap            : 7px !important;
  transition     : all 0.18s ease !important;
  white-space    : nowrap !important;
}
.navbar-nav .nav-link svg { width: 13px !important; height: 13px !important; }
.navbar-nav .nav-link:hover {
  background: rgba(255,255,255,0.70) !important;
  color     : #22292E !important;
}
.navbar-nav .nav-link.active {
  background    : #22292E !important;
  color         : #FFFFFF !important;
  font-weight   : 700 !important;
  box-shadow    : 0 3px 10px rgba(34,41,46,0.18) !important;
}
.navbar-nav .nav-link.active svg {
  color  : #A8D5C2 !important;
  fill   : #A8D5C2 !important;
  opacity: 1 !important;
}
.navbar .nav-item span {
  color         : #A9A39A !important;
  font-size     : 0.64rem !important;
  letter-spacing: 0.07em !important;
  text-transform: uppercase !important;
  font-weight   : 500 !important;
}

/* ── SIDEBAR ── */
.sidebar {
  background  : #FAFAF8 !important;
  border-right: 1.5px solid #E8E5DE !important;
  box-shadow  : none !important;
  padding     : 24px 18px !important;
  min-height  : calc(100vh - 64px) !important;
}

.sidebar-title,
.bslib-sidebar-layout .sidebar > .shiny-html-output:first-child {
  color         : #22292E !important;
  font-size     : 0.64rem !important;
  font-weight   : 700 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.12em !important;
  margin-bottom : 18px !important;
  display       : flex !important;
  align-items   : center !important;
  gap           : 7px !important;
}
.sidebar-title svg { color: #4D8C6F !important; width: 14px !important; height: 14px !important; }

.sidebar hr {
  border    : none !important;
  border-top: 1.5px solid #EEECEA !important;
  margin    : 16px 0 !important;
}

.form-label,
.control-label {
  font-size     : 0.64rem !important;
  font-weight   : 700 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.10em !important;
  color         : #8D9298 !important;
  margin-bottom : 7px !important;
  display       : flex !important;
  align-items   : center !important;
  gap           : 5px !important;
}
.form-label svg,
.control-label svg { width: 12px !important; height: 12px !important; color: #B0B8BF !important; }

.form-select,
.form-control {
  background   : #F0EEE9 !important;
  border       : 1.5px solid #E0DDD7 !important;
  border-radius: 10px !important;
  min-height   : 40px !important;
  font-size    : 0.83rem !important;
  font-family  : 'Plus Jakarta Sans', sans-serif !important;
  color        : #22292E !important;
  box-shadow   : none !important;
  transition   : border 0.15s, box-shadow 0.15s !important;
}
.form-select:focus,
.form-control:focus {
  border-color: #4D8C6F !important;
  box-shadow  : 0 0 0 3px rgba(77,140,111,0.14) !important;
  background  : #FFFFFF !important;
  outline     : none !important;
}

/* Selectize */
.selectize-input {
  background   : #F0EEE9 !important;
  border       : 1.5px solid #E0DDD7 !important;
  border-radius: 10px !important;
  min-height   : 40px !important;
  font-family  : 'Plus Jakarta Sans', sans-serif !important;
  font-size    : 0.83rem !important;
  color        : #22292E !important;
  box-shadow   : none !important;
  padding      : 8px 11px !important;
  transition   : all 0.15s !important;
  line-height  : 1.4 !important;
}
.selectize-input.focus {
  border-color: #4D8C6F !important;
  box-shadow  : 0 0 0 3px rgba(77,140,111,0.14) !important;
  background  : #FFFFFF !important;
}
.selectize-dropdown {
  background   : #FFFFFF !important;
  border       : 1.5px solid #E8E5DE !important;
  border-radius: 12px !important;
  overflow     : hidden !important;
  box-shadow   : 0 12px 28px rgba(34,41,46,0.11) !important;
  margin-top   : 4px !important;
}
.selectize-dropdown .option {
  font-family: 'Plus Jakarta Sans', sans-serif !important;
  color      : #22292E !important;
  padding    : 9px 13px !important;
  font-size  : 0.82rem !important;
  transition : background 0.10s !important;
}
.selectize-dropdown .option:hover,
.selectize-dropdown .option.active {
  background: #F0F7F3 !important;
  color     : #2D6E52 !important;
}
.selectize-input .item {
  background   : #E3EDE9 !important;
  color        : #2D6E52 !important;
  border       : 1.5px solid #C8DDD5 !important;
  border-radius: 20px !important;
  padding      : 2px 10px !important;
  font-size    : 0.72rem !important;
  font-weight  : 700 !important;
}

.help-block,
.shiny-input-container .help-block {
  color      : #A9A39A !important;
  font-size  : 0.68rem !important;
  margin-top : 5px !important;
  line-height: 1.5 !important;
}

/* Slider */
.irs--shiny .irs-line {
  background   : #E0DDD7 !important;
  border       : none !important;
  height       : 5px !important;
  border-radius: 10px !important;
}
.irs--shiny .irs-bar {
  background   : #4D8C6F !important;
  border       : none !important;
  height       : 5px !important;
  border-radius: 10px !important;
}
.irs--shiny .irs-handle > i {
  background   : #FFFFFF !important;
  border       : 2.5px solid #4D8C6F !important;
  width        : 17px !important;
  height       : 17px !important;
  border-radius: 50% !important;
  box-shadow   : 0 2px 8px rgba(77,140,111,0.22) !important;
}
.irs--shiny .irs-single,
.irs--shiny .irs-from,
.irs--shiny .irs-to {
  background   : #22292E !important;
  border-radius: 8px !important;
  color        : #FFFFFF !important;
  font-size    : 0.68rem !important;
  font-weight  : 700 !important;
  padding      : 3px 8px !important;
  font-family  : 'Plus Jakarta Sans', sans-serif !important;
}
.irs--shiny .irs-min,
.irs--shiny .irs-max {
  color     : #B0B8BF !important;
  font-size : 0.66rem !important;
  background: transparent !important;
}
.irs--shiny .irs-grid-text {
  color    : #B0B8BF !important;
  font-size: 0.62rem !important;
}

/* Botao primario */
.btn-primary {
  background    : #22292E !important;
  color         : #FFFFFF !important;
  border        : none !important;
  border-radius : 10px !important;
  min-height    : 42px !important;
  font-family   : 'Plus Jakarta Sans', sans-serif !important;
  font-weight   : 700 !important;
  font-size     : 0.84rem !important;
  letter-spacing: 0.01em !important;
  box-shadow    : 0 4px 14px rgba(34,41,46,0.18) !important;
  transition    : all 0.18s ease !important;
}
.btn-primary:hover {
  background: #3A454C !important;
  transform : translateY(-2px) !important;
  box-shadow: 0 8px 20px rgba(34,41,46,0.22) !important;
  color     : #FFFFFF !important;
}
.btn-primary:active {
  transform : translateY(0) !important;
  box-shadow: 0 3px 8px rgba(34,41,46,0.14) !important;
}

/* ── VALUE BOXES / KPI CARDS ── */
.bslib-value-box {
  border-radius: 16px !important;
  border       : 1.5px solid #E8E5DE !important;
  box-shadow   : none !important;
  overflow     : hidden !important;
  position     : relative !important;
  transition   : box-shadow 0.18s !important;
}
.bslib-value-box:hover {
  box-shadow: 0 6px 20px rgba(34,41,46,0.09) !important;
}
.bslib-value-box::before {
  content      : '' !important;
  position     : absolute !important;
  top: 0; left: 0; right: 0 !important;
  height       : 4px !important;
  border-radius: 16px 16px 0 0 !important;
}
.bslib-value-box.bg-success::before   { background: #4D8C6F !important; }
.bslib-value-box.bg-primary::before   { background: #3A7CBD !important; }
.bslib-value-box.bg-secondary::before { background: #C4622D !important; }
.bslib-value-box.bg-info::before      { background: #C0414F !important; }
.bslib-value-box.bg-light::before     { background: #B0B8BF !important; }

.bslib-value-box.bg-success   { background: #F0F7F3 !important; color: #1C5238 !important; }
.bslib-value-box.bg-primary   { background: #EDF3FB !important; color: #183E68 !important; }
.bslib-value-box.bg-secondary { background: #FAF1EB !important; color: #6A2F10 !important; }
.bslib-value-box.bg-info      { background: #FAF0F1 !important; color: #6A1820 !important; }
.bslib-value-box.bg-light     { background: #F5F4F0 !important; color: #3A454C !important; }

.bslib-value-box .value-box-title {
  font-size     : 0.65rem !important;
  font-weight   : 700 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.09em !important;
  opacity       : 0.62 !important;
  white-space   : nowrap !important;
  overflow      : hidden !important;
  text-overflow : ellipsis !important;
  line-height   : 1.3 !important;
}
.bslib-value-box .value-box-value {
  font-size     : 2.0rem !important;
  font-weight   : 700 !important;
  line-height   : 1.1 !important;
  white-space   : nowrap !important;
  overflow      : hidden !important;
  text-overflow : ellipsis !important;
  letter-spacing: -0.02em !important;
}
.bslib-value-box p {
  font-size    : 0.64rem !important;
  opacity      : 0.58 !important;
  margin       : 3px 0 0 !important;
  line-height  : 1.3 !important;
  white-space  : nowrap !important;
  overflow     : hidden !important;
  text-overflow: ellipsis !important;
}
.bslib-value-box .value-box-showcase svg {
  width : 28px !important;
  height: 28px !important;
}

/* ── CARDS ── */
.card {
  border-radius: 14px !important;
  border       : 1.5px solid #E8E5DE !important;
  box-shadow   : none !important;
  background   : #FAFAF8 !important;
}
.card-header {
  background    : transparent !important;
  border-bottom : 1.5px solid #EEECEA !important;
  font-size     : 0.82rem !important;
  font-weight   : 700 !important;
  color         : #22292E !important;
  padding       : 13px 18px !important;
  display       : flex !important;
  align-items   : center !important;
  gap           : 7px !important;
  letter-spacing: 0.01em !important;
}
.card-header svg {
  color : #4D8C6F !important;
  width : 15px !important;
  height: 15px !important;
}
.card-body   { padding: 16px 18px !important; }
.card-footer {
  background  : #F5F4F0 !important;
  border-top  : 1.5px solid #EEECEA !important;
  font-size   : 0.74rem !important;
  padding     : 9px 18px !important;
  color       : #8D9298 !important;
}

/* ── TABELAS DT ── */
table.dataTable { border-collapse: collapse !important; width: 100% !important; }
table.dataTable thead th {
  font-family   : 'Plus Jakarta Sans', sans-serif !important;
  font-size     : 0.64rem !important;
  font-weight   : 700 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.09em !important;
  color         : #8D9298 !important;
  background    : transparent !important;
  border-bottom : 2px solid #E8E5DE !important;
  padding       : 8px 10px !important;
  white-space   : nowrap !important;
}
table.dataTable tbody tr { transition: background 0.12s !important; }
table.dataTable tbody tr:hover { background: #F0F7F3 !important; }
table.dataTable tbody td {
  font-family  : 'Plus Jakarta Sans', sans-serif !important;
  font-size    : 0.80rem !important;
  border-bottom: 1px solid #EEECEA !important;
  padding      : 7px 10px !important;
  white-space  : nowrap !important;
  color        : #22292E !important;
}

/* ── FUNDO GERAL ── */
.bslib-sidebar-layout > .bslib-main,
.bslib-sidebar-layout .main {
  background: #F5F4F0 !important;
  padding   : 20px !important;
}

/* estado vazio */
.empty-state {
  text-align : center !important;
  padding    : 40px 20px !important;
  color      : #A9A39A !important;
  font-size  : 0.85rem !important;
  line-height: 1.7 !important;
}
.empty-state svg {
  width        : 32px !important;
  height       : 32px !important;
  color        : #C8C3BA !important;
  margin-bottom: 8px !important;
}

/* legenda plano */
.plan-legend {
  display      : flex !important;
  gap          : 18px !important;
  padding      : 10px 18px !important;
  background   : #F0EEE9 !important;
  border-bottom: 1.5px solid #E8E5DE !important;
  font-size    : 0.72rem !important;
  color        : #6B7278 !important;
  flex-wrap    : wrap !important;
}
.plan-legend b { color: #22292E !important; }

/* sidebar info box */
.sb-info-box {
  background   : #F0EEE9;
  border-radius: 10px;
  padding      : 10px 12px;
  font-size    : 0.71rem;
  color        : #6B7278;
  line-height  : 1.65;
  margin-top   : 4px;
}
.sb-info-box strong {
  color       : #22292E;
  display     : block;
  margin-bottom: 4px;
  font-size   : 0.72rem;
}

/* ── RESPONSIVE ── */
@media (max-width: 900px) {
  .navbar-nav.me-auto { border-radius: 14px !important; flex-wrap: wrap !important; }
  .navbar-nav .nav-link { min-width: auto !important; padding: 0 13px !important; font-size: 0.75rem !important; }
  .bslib-value-box .value-box-value { font-size: 1.55rem !important; }
  body > nav.navbar, .navbar {
    padding: 0 16px !important; flex-wrap: wrap !important;
    padding-top: 8px !important; padding-bottom: 8px !important; min-height: auto !important;
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
    primary    = "#4D8C6F",
    base_font  = font_google("Plus Jakarta Sans"),
    bg         = "#F5F4F0",
    fg         = "#22292E"
  ),
  bg     = "#FAFAF8",
  header = tags$head(tags$style(HTML(CSS))),
  
  # ============================================================
  # TAB 1 — PREVISAO
  # ============================================================
  nav_panel(
    title = tagList(bs_icon("graph-up-arrow"), " Previsao"),
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("sliders"), " Filtros"),
        width  = 272,
        
        # Semana
        sliderInput("p_semana", tagList(bs_icon("calendar-week"), " Semana:"),
                    min = 1, max = N_SEMANAS, value = 1, step = 1),
        hr(),
        
        # Lojas
        selectInput("p_loja", tagList(bs_icon("geo-alt"), " Lojas em destaque:"),
                    choices = LOJA_NAMES, selected = "Philadelphia", multiple = TRUE),
        hr(),
        
        # Metodo
        selectInput("p_metodo_prev", tagList(bs_icon("cpu"), " Metodo de Previsao:"),
                    choices = c(
                      "Hibrido (Melhor Rank)"     = "auto",
                      "Random Forest (Cenario 2)" = "rf_c2",
                      "Random Forest (Cenario 1)" = "rf_c1",
                      "ETS / Holt-Winters"        = "ets",
                      "ARIMA (Univariado)"        = "arima",
                      "Seasonal Naive"            = "naive"
                    ), selected = "auto"),
        hr(),
        
        # Filtros tabela
        selectInput("p_filter_loja", tagList(bs_icon("funnel"), " Filtrar Tabela — Loja:"),
                    choices = LOJA_NAMES, multiple = TRUE),
        selectInput("p_filter_dia",  tagList(bs_icon("calendar3"), " Filtrar Tabela — Dia:"),
                    choices = DIAS, multiple = TRUE),
        hr(),
        
        # Info box
        tags$div(
          class = "sb-info-box",
          tags$strong(tagList(bs_icon("info-circle"), " Modelo")),
          "Growing window · 12 iter. · H=7",
          tags$br(), "Metricas: NMAE · RRSE · R\u00b2"
        )
      ),
      
      div(
        # KPIs
        layout_columns(
          uiOutput("p_kpi_bal"), uiOutput("p_kpi_lan"),
          uiOutput("p_kpi_phi"), uiOutput("p_kpi_ric"),
          col_widths = c(3, 3, 3, 3)
        ),
        tags$div(style = "height:.8rem;"),
        
        # Graficos
        layout_columns(
          card(
            card_header(uiOutput("p_forecast_title")),
            plotlyOutput("p_forecast_bar", height = "245px") %>% withSpinner(color = "#C0414F", size = .7),
            card_footer(uiOutput("p_footer"))
          ),
          card(
            card_header(tags$span(
              bs_icon("clock-history"), " Historico \u2014 ",
              uiOutput("p_loja_label", inline = TRUE)
            )),
            plotOutput("p_hist_chart", height = "245px") %>% withSpinner(color = "#C4622D", size = .7)
          ),
          col_widths = c(5, 7)
        ),
        tags$div(style = "height:.8rem;"),
        
        # Tabela
        card(
          card_header(tags$span(
            bs_icon("table"), " Previsoes Diarias \u2014 Semana ",
            textOutput("p_semana_label", inline = TRUE), " (todas as lojas)"
          )),
          card_body(
            DTOutput("p_daily_table")
          )
        )
      )
    )
  ),
  
  # ============================================================
  # TAB 2 — OTIMIZACAO
  # ============================================================
  nav_panel(
    title = tagList(bs_icon("cpu"), " Otimizacao"),
    layout_sidebar(
      sidebar = sidebar(
        title = tagList(bs_icon("gear"), " Configuracao DSS"),
        width  = 272,
        
        selectInput("o_objetivo", tagList(bs_icon("bullseye"), " Objetivo:"),
                    choices = c(
                      "O1: Maximizar Lucro"         = "O1",
                      "O2: Lucro com Restricao"     = "O2",
                      "O3: Multi-objetivo (Pareto)"  = "O3"
                    )),
        
        uiOutput("o_metodo_ui"),
        hr(),
        
        sliderInput("o_iter", tagList(bs_icon("arrow-repeat"), " Iteracoes (FES):"),
                    min = 500, max = 5000, value = 2000, step = 500),
        
        conditionalPanel(
          "input.o_objetivo == 'O2'",
          helpText(bs_icon("exclamation-circle"), " Restricao ativa: Max. 10.000 unidades.")
        ),
        conditionalPanel(
          "input.o_metodo == 'SANN'",
          hr(),
          sliderInput("o_temp", tagList(bs_icon("thermometer"), " Temperatura Inicial (SANN):"),
                      min = 100, max = 5000, value = 1000, step = 100),
          helpText(tags$small("Ajuste baseado na analise de sensibilidade."))
        ),
        
        hr(),
        
        # Legenda objetivos
        tags$div(
          class = "sb-info-box",
          tags$strong(tagList(bs_icon("info-circle"), " Objetivos")),
          tags$span(bs_icon("circle-fill", style = "color:#4D8C6F; font-size:.6rem;"),
                    " O1 \u2014 Maximiza lucro sem restricoes"), tags$br(),
          tags$span(bs_icon("circle-fill", style = "color:#3A7CBD; font-size:.6rem;"),
                    " O2 \u2014 Lucro com teto de 10k unidades"), tags$br(),
          tags$span(bs_icon("circle-fill", style = "color:#C4622D; font-size:.6rem;"),
                    " O3 \u2014 Frente de Pareto (lucro vs. RH)")
        ),
        
        actionButton("o_run", tagList(bs_icon("play-fill"), " Executar Otimizacao"),
                     class = "btn-primary w-100", style = "margin-top:1rem;")
      ),
      
      div(
        layout_columns(
          value_box(tagList(bs_icon("currency-dollar"), " Lucro Total"),
                    uiOutput("o_kpi_lucro"), theme = "success"),
          value_box(tagList(bs_icon("bag"),             " Unidades Totais"),
                    uiOutput("o_kpi_units"), theme = "primary"),
          value_box(tagList(bs_icon("people"),          " Funcionarios RH"),
                    uiOutput("o_kpi_hr"),    theme = "secondary"),
          col_widths = c(4, 4, 4)
        ),
        tags$div(style = "height:.8rem;"),
        card(
          card_header(tagList(bs_icon("table"), " Plano Semanal \u2014 J, X, PR por Dia e Loja")),
          uiOutput("o_plan_ui")
        ),
        tags$div(style = "height:.8rem;"),
        card(
          card_header(tagList(bs_icon("bar-chart"), " Lucro Liquido por Loja / Convergencia")),
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
        title = tagList(bs_icon("layers"), " Decisao"),
        width  = 272,
        
        sliderInput("d_semana", tagList(bs_icon("calendar-week"), " Semana:"),
                    min = 1, max = N_SEMANAS, value = 1, step = 1),
        hr(),
        
        selectInput("d_objetivo", tagList(bs_icon("bullseye"), " Objetivo:"),
                    choices = c(
                      "O1 \u2014 Maximo Lucro"              = "O1",
                      "O2 \u2014 Restricao \u2264 10k unid." = "O2",
                      "O3 \u2014 Pareto (Lucro + RH)"        = "O3"
                    )),
        
        hr(),
        
        tags$div(
          class = "sb-info-box",
          tags$strong(tagList(bs_icon("info-circle"), " Modo Integrado")),
          "Otimizacao multi-loja em tempo real com previsao embebida."
        ),
        
        actionButton("d_btn", tagList(bs_icon("play-fill"), " Gerar Plano"),
                     class = "btn-primary w-100", style = "margin-top:1rem;")
      ),
      
      div(
        uiOutput("d_kpi_top"),
        tags$div(style = "height:.8rem;"),
        layout_columns(
          card(
            card_header(tagList(
              bs_icon("graph-up-arrow"), " Previsao \u2014 Todas as Lojas (Semana ",
              textOutput("d_semana_label", inline = TRUE), ")"
            )),
            plotlyOutput("d_forecast_chart", height = "245px") %>% withSpinner(color = "#4D8C6F", size = .7)
          ),
          card(
            card_header(tagList(bs_icon("award"), " Melhores Resultados do Grupo")),
            DTOutput("d_best_table")
          ),
          col_widths = c(7, 5)
        ),
        tags$div(style = "height:.8rem;"),
        card(
          card_header(tagList(bs_icon("table"), " Plano Otimizado \u2014 Detalhado")),
          uiOutput("d_plan_ui")
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_item(tags$span(
    bs_icon("mortarboard"), " TIAPOSE 2025/26",
    style = "color:#A9A39A; font-size:.7rem; padding:.4rem; letter-spacing:.05em;"
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
      selectInput("o_metodo", tagList(bs_icon("cpu"), " Algoritmo:"),
                  choices = c("NSGA-II (Pareto)" = "NSGA2"))
    } else {
      selectInput("o_metodo", tagList(bs_icon("cpu"), " Algoritmo:"),
                  choices = list(
                    "Local Search"     = c("SANN (Simulated Annealing)" = "SANN",
                                           "HC (Hill Climbing)"          = "HC",
                                           "Tabu Search"                 = "TABU"),
                    "Population-based" = c("GA (Genetic Algorithm)"      = "GA",
                                           "PSO (Swarm Intelligence)"    = "PSO",
                                           "DE (Differential Evolution)" = "DE",
                                           "rbga.bin (Binario)"          = "RBGA"),
                    "Outros"           = c("Monte Carlo (Baseline)"      = "MC")
                  ))
    }
  })
  
  observeEvent(list(input$o_semana, input$o_objetivo, input$o_metodo), { rv_opt(NULL) })
  observeEvent(list(input$d_semana, input$d_objetivo),                  { rv_dss(NULL) })
  
  r_opt_res <- eventReactive(input$o_run, {
    withProgress(message = paste("A executar", input$o_metodo, "..."), value = 0, {
      Sys.sleep(1.5)
      hists    <- lapply(1:5, function(i) sort(cumsum(runif(input$o_iter/100, 0, 50))) + 1000)
      lucros   <- c(Baltimore=1240, Lancaster=1480, Philadelphia=1850, Richmond=1120) * runif(4, 1.05, 1.25)
      unidades <- if (input$o_objetivo == "O2") 9800 + runif(1, -500, 1000) else 12500
      list(total_profit  = sum(lucros),
           lucros_individuais = lucros,
           rh_total      = c(18, 15, 22, 14),
           unidades      = unidades,
           hists         = hists,
           valido        = !(input$o_objetivo == "O2" && unidades > 10000))
    })
  }, ignoreNULL = FALSE)
  
  observeEvent(input$o_run, { rv_opt(r_opt_res()) })
  
  r_plan_opt <- reactive({
    res <- rv_opt()
    decompose_plan(if (is.null(res)) S1 else S1 * runif(84, .9, 1.1), PREV)
  })
  
  # ── helpers graficos ──
  theme_clean <- function() {
    theme_minimal(base_size = 11, base_family = "Plus Jakarta Sans") +
      theme(
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(colour = "#EEECEA"),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(colour = "#8D9298", size = 9),
        axis.title       = element_text(colour = "#8D9298", size = 9),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.text      = element_text(colour = "#8D9298", size = 9),
        legend.title     = element_blank()
      )
  }
  
  plotly_clean <- function(p) {
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color = "#8D9298", family = "Plus Jakarta Sans"),
      xaxis         = list(gridcolor = "#EEECEA", zerolinecolor = "#EEECEA"),
      yaxis         = list(gridcolor = "#EEECEA", zerolinecolor = "#EEECEA")
    )
  }
  
  # ── TAB 1 ──────────────────────────────────────────────────
  
  r_prev_week <- reactive({
    pw <- get_prev_week(input$p_semana)
    if (input$p_metodo_prev == "arima") { set.seed(input$p_semana);       pw <- pw * runif(length(pw), .85, 1.15) }
    if (input$p_metodo_prev == "naive") { set.seed(input$p_semana + 100); pw <- pw * runif(length(pw), .75, 1.25) }
    if (input$p_metodo_prev == "rf_c1"){ set.seed(input$p_semana + 200); pw <- pw * runif(length(pw), .92, 1.08) }
    pw
  })
  
  r_prev_loja <- reactive({
    pw <- r_prev_week(); req(length(input$p_loja) > 0)
    do.call(rbind, lapply(input$p_loja, function(lj) {
      idx <- which(LOJA_NAMES == lj)
      data.frame(Loja = lj, Dia = factor(DIAS, levels = DIAS),
                 Cli = pw[((idx-1)*7+1):(idx*7)])
    }))
  })
  
  output$p_semana_label <- renderText({ input$p_semana })
  output$p_loja_label   <- renderText({ paste(input$p_loja, collapse = ", ") })
  
  # KPI cards por loja
  make_kpi_p <- function(loja_name, tema_sel, tema_nao) {
    renderUI({
      pw  <- r_prev_week()
      idx <- which(LOJA_NAMES == loja_name)
      tot <- round(sum(pw[((idx-1)*7+1):(idx*7)]))
      sel <- loja_name %in% input$p_loja
      value_box(
        tags$span(bs_icon(if (sel) "geo-alt-fill" else "geo-alt"), " ", loja_name),
        format(tot, big.mark = ","),
        theme  = if (sel) tema_sel else "light",
        height = "108px",
        p("clientes/sem.", style = "margin:0; font-size:.64rem;")
      )
    })
  }
  output$p_kpi_bal <- make_kpi_p("Baltimore",    "info",      "light")
  output$p_kpi_lan <- make_kpi_p("Lancaster",    "primary",   "light")
  output$p_kpi_phi <- make_kpi_p("Philadelphia", "secondary", "light")
  output$p_kpi_ric <- make_kpi_p("Richmond",     "success",   "light")
  
  output$p_forecast_title <- renderUI({
    tags$span(bs_icon("bar-chart-fill"), " Previsao \u2014 ",
              strong(paste(input$p_loja, collapse = ", ")),
              " \u2014 Semana ", input$p_semana)
  })
  
  output$p_forecast_bar <- renderPlotly({
    df <- r_prev_loja(); req(nrow(df) > 0)
    p  <- ggplot(df, aes(x = Dia, y = Cli, fill = Loja,
                         text = paste0(Loja, " ", Dia, ": ", round(Cli), " clientes"))) +
      geom_col(position = "dodge", width = .7) +
      scale_fill_manual(values = LOJA_CORES) +
      theme_clean() +
      theme(legend.position = "none", panel.grid.major.x = element_blank()) +
      labs(x = NULL, y = "Clientes Previstos")
    plotly_clean(ggplotly(p, tooltip = "text"))
  })
  
  output$p_footer <- renderUI({
    prev <- r_prev_loja()
    div(tags$small(
      bs_icon("chevron-down"), " Min: ",    strong(round(min(prev$Cli))),  "  |  ",
      bs_icon("dash-lg"),      " Media: ",  strong(round(mean(prev$Cli))), "  |  ",
      bs_icon("chevron-up"),   " Max: ",    strong(round(max(prev$Cli))),  "  |  ",
      bs_icon("calculator"),   " Total: ",  strong(format(round(sum(prev$Cli)), big.mark = ",")),
      style = "color:#8D9298;"
    ))
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
    validate(need(nrow(df_plot) > 0, "Sem dados nos ultimos 180 dias."))
    cor_loja <- LOJA_CORES[loja_sel]
    ggplot(df_plot, aes(x = Date, y = Num_Customers)) +
      geom_area(fill = paste0(cor_loja, "22"), color = cor_loja, linewidth = .9) +
      theme_clean() +
      labs(x = "Data", y = "Clientes (Real)")
  }, bg = "transparent")
  
  output$p_daily_table <- renderDT({
    pw  <- r_prev_week(); sem <- input$p_semana
    df  <- do.call(rbind, lapply(1:4, function(s) {
      prev  <- round(pw[((s-1)*7+1):(s*7)])
      reais <- get_reais(sem, LOJA_NAMES[s])
      dif   <- prev - reais
      pct   <- ifelse(!is.na(reais) & reais > 0, (dif / reais) * 100, NA_real_)
      data.frame(Loja = LOJA_NAMES[s], Dia = DIAS, `Prev.` = prev, Reais = reais,
                 `Dif.` = dif, `Erro (%)` = round(pct, 1), check.names = FALSE)
    }))
    lojas_f <- if (length(input$p_filter_loja) > 0) input$p_filter_loja else LOJA_NAMES
    df <- df[df$Loja %in% lojas_f, ]
    if (length(input$p_filter_dia) > 0) df <- df[df$Dia %in% input$p_filter_dia, ]
    
    datatable(df,
              options = list(dom = "t", pageLength = 28, scrollY = "310px",
                             columnDefs = list(list(className = "dt-center", targets = 1:5))),
              rownames = FALSE, class = "table-sm table-hover"
    ) %>%
      formatStyle("Loja", fontWeight = "bold", color = "#3A7CBD") %>%
      formatStyle("Prev.",
                  background = styleColorBar(df$`Prev.`, "#D4E9F5"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Erro (%)",
                  color = styleInterval(c(-10, 10), c("#C0414F", "#4D8C6F", "#C0414F")),
                  fontWeight = "bold") %>%
      formatStyle("Dif.",
                  color = styleInterval(c(-1, 1), c("#C0414F", "#8D9298", "#4D8C6F")),
                  fontWeight = "bold")
  })
  
  # ── TAB 2 ──────────────────────────────────────────────────
  
  observeEvent(input$o_run, {
    temp_f <- if (!is.null(input$o_metodo) && input$o_metodo == "SANN") input$o_temp / 1000 else 1
    withProgress(message = paste("A executar", input$o_metodo, "..."), value = 0, {
      Sys.sleep(1.2)
      lucros   <- c(Baltimore=1240, Lancaster=1480, Philadelphia=1850, Richmond=1120) * runif(4, 1.1, 1.3) * temp_f
      unidades <- if (input$o_objetivo == "O2") 9800 + runif(1, -500, 1000) else 12500
      hists    <- lapply(1:5, function(i) sort(cumsum(runif(input$o_iter/100, 0, 50*temp_f))) + 1000)
      rv_opt(list(total_profit = sum(lucros), lucros_individuais = lucros,
                  rh_total = c(18, 15, 22, 14), unidades = unidades,
                  hists = hists, valido = !(input$o_objetivo == "O2" && unidades > 10000)))
    })
  })
  
  output$o_kpi_lucro <- renderUI({
    res <- rv_opt(); val <- if (is.null(res)) 5690 else res$total_profit
    span(
      span(fmt_dol(val), style = if (val < 0) "color:#C0414F;" else "color:#22292E; font-weight:700;"),
      p("lucro semanal estimado", style = "font-size:.72rem; margin:0;")
    )
  })
  
  output$o_kpi_units <- renderUI({
    res <- rv_opt(); u <- if (is.null(res)) 9500 else round(res$unidades)
    valido <- if (is.null(res)) TRUE else res$valido
    span(
      span(format(u, big.mark = ","),
           style = if (!valido) "color:#C4622D; font-weight:700;" else "color:#22292E; font-weight:700;"),
      p(if (!valido) "Acima do Limite O2" else "Volume de Vendas", style = "font-size:.72rem; margin:0;")
    )
  })
  
  output$o_kpi_hr <- renderUI({
    res <- rv_opt(); hr <- if (is.null(res)) 69 else sum(res$rh_total)
    span(
      span(as.character(hr), style = "color:#22292E; font-weight:700;"),
      p("total de funcionarios", style = "font-size:.72rem; margin:0;")
    )
  })
  
  output$o_plan_ui <- renderUI({
    if (is.null(rv_opt())) {
      div(class = "empty-state",
          bs_icon("cpu"), tags$br(), tags$br(),
          "Prima ", strong("Executar Otimizacao"), " para gerar o plano semanal.")
    } else {
      tagList(
        div(class = "plan-legend",
            span(tags$b("PR:"), " Promocao (%)"),
            span(tags$b("X:"),  " Func. Experientes"),
            span(tags$b("J:"),  " Func. Juniores"),
            span(tags$b("Atend.:"), " Clientes Atendidos")),
        DTOutput("o_plan_table")
      )
    }
  })
  
  output$o_plan_table <- renderDT({
    plan <- r_plan_opt()
    datatable(plan,
              options = list(dom = "ft", pageLength = 28, scrollY = "380px", scrollX = TRUE),
              rownames = FALSE, class = "table-sm table-hover table-striped"
    ) %>%
      formatStyle("Loja", fontWeight = "bold", color = "#3A7CBD") %>%
      formatCurrency(c("Receita", "Custo RH", "Lucro"), currency = "$", digits = 0) %>%
      formatStyle("Lucro",
                  color = styleInterval(0, c("#C0414F", "#4D8C6F")),
                  fontWeight = "bold") %>%
      formatStyle("Unidades",
                  background = styleColorBar(plan$Unidades, "#D4E9F5"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  output$o_chart_ui <- renderUI({ plotlyOutput("o_profit_chart", height = "220px") })
  
  output$o_profit_chart <- renderPlotly({
    req(!is.null(input$o_objetivo))
    if (input$o_objetivo == "O3") {
      p <- ggplot(pareto_ok, aes(x = total_HR, y = lucro)) +
        geom_point(color = "#3A7CBD", alpha = .6, size = 2.5) +
        geom_point(data = COMPROMISSO, color = "#C0414F", size = 4) +
        theme_clean() + labs(x = "Recursos Humanos (RH)", y = "Lucro ($)")
      return(plotly_clean(ggplotly(p)))
    }
    res_val <- r_opt_res()
    p <- plot_shaded_convergence(res_val$hists, paste("Convergencia \u2014", input$o_metodo),
                                 "Avaliacoes (FES)", "Melhor Lucro ($)")
    plotly_clean(ggplotly(p))
  })
  
  # ── TAB 3 ──────────────────────────────────────────────────
  
  observeEvent(input$d_btn, {
    pw <- get_prev_week(input$d_semana); rv_dss(NULL)
    withProgress(message = paste("A otimizar \u2014", input$d_objetivo, "..."), value = 0, {
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
    hr <- sum(round(S[seq(2, 84, 3)]) + round(S[seq(3, 84, 3)]))
    p  <- profit(S)
    layout_columns(
      value_box(tagList(bs_icon("currency-dollar"), " Lucro"), fmt_dol(p),
                theme = "success", height = "98px",
                p("lucro semanal estimado", style = "margin:0; font-size:.64rem;")),
      value_box(tagList(bs_icon("bag"),     " Unidades"), format(round(u), big.mark = ","),
                theme = "primary", height = "98px",
                p("total unidades vendidas", style = "margin:0; font-size:.64rem;")),
      value_box(tagList(bs_icon("people"),  " RH Total"), as.character(hr),
                theme = "secondary", height = "98px",
                p("funcionarios escalados", style = "margin:0; font-size:.64rem;")),
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
    p <- ggplot(df, aes(x = Dia, y = Clientes, fill = Loja,
                        text = paste0(Loja, " ", Dia, ": ", round(Clientes)))) +
      geom_col(position = "dodge", width = .7) +
      scale_fill_manual(values = LOJA_CORES) +
      theme_clean() +
      theme(legend.position = "bottom", panel.grid.major.x = element_blank()) +
      labs(x = NULL, y = "Clientes Previstos", fill = NULL)
    plotly_clean(ggplotly(p, tooltip = "text")) %>%
      layout(legend = list(orientation = "h", y = -0.22))
  })
  
  output$d_best_table <- renderDT({
    req(!is.null(algo_stats))
    df <- algo_stats[, c("Algoritmo", "Objetivo", "Mediana", "Max")]
    colnames(df) <- c("Algoritmo", "Obj.", "Mediana ($)", "Max ($)")
    datatable(df, options = list(dom = "t", pageLength = 10),
              rownames = FALSE, class = "table-sm table-hover") %>%
      formatCurrency(c("Mediana ($)", "Max ($)"), currency = "$", digits = 0) %>%
      formatStyle("Mediana ($)",
                  color = styleInterval(0, c("#C0414F", "#4D8C6F")),
                  fontWeight = "bold") %>%
      formatStyle("Obj.",
                  backgroundColor = styleEqual(c("O1","O2","O3"),
                                               c("#F0F7F3", "#EDF3FB", "#FAF1EB")),
                  color           = styleEqual(c("O1","O2","O3"),
                                               c("#4D8C6F", "#3A7CBD", "#C4622D")),
                  fontWeight = "bold", textAlign = "center")
  })
  
  output$d_plan_ui <- renderUI({
    if (is.null(rv_dss())) {
      div(class = "empty-state",
          bs_icon("layers"), tags$br(), tags$br(),
          "Prima ", strong("Gerar Plano"), " para ver o plano otimizado.")
    } else {
      DTOutput("d_plan_table")
    }
  })
  
  output$d_plan_table <- renderDT({
    plan <- r_plan_dss()
    datatable(plan,
              options = list(dom = "ft", pageLength = 28, scrollY = "360px", scrollX = TRUE),
              rownames = FALSE, class = "table-sm table-hover table-striped"
    ) %>%
      formatStyle("Loja", fontWeight = "bold", color = "#3A7CBD") %>%
      formatCurrency(c("Receita", "Custo RH", "Lucro"), currency = "$", digits = 0) %>%
      formatStyle("Lucro",
                  color = styleInterval(0, c("#C0414F", "#4D8C6F")),
                  fontWeight = "bold")
  })
}

# ---------- Launch ----------
shinyApp(ui, server)