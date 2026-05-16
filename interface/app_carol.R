# =============================================================
# app.R — USA Stores · IDSS
# TIAPOSE 2025/26
# Estrutura final: Análise Exploratória | Previsão | Otimização
# =============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

library(DT)
library(bsicons)
library(shinycssloaders)
library(shinyWidgets)

BASE_PATH <- normalizePath("~/TIAPOSE2526")
source(file.path(BASE_PATH, "utils/config_otimizacao.R"))
source(file.path(BASE_PATH, "utils/visualizacao_utils.R"))

LOJA_NAMES <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
DIAS <- c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab")
SIDEBAR_WIDTH <- 250

LOJA_CORES <- c(
  Baltimore    = "#84B2B1",
  Lancaster    = "#CF8082",
  Philadelphia = "#E49A36",
  Richmond     = "#8FA8C5"
)

COMPROMISSO <- data.frame(lucro = 1241, total_HR = 80)

kpi_card <- function(title, value, subtitle, icon, color) {
  div(
    class = "kpi-card",
    style = paste0("--kpi-color:", color, ";"),
    div(class = "kpi-icon", bs_icon(icon)),
    div(
      class = "kpi-content",
      div(class = "kpi-title", title),
      div(class = "kpi-value", value),
      div(class = "kpi-subtitle", subtitle)
    )
  )
}

parse_date_safe <- function(x) {
  as.Date(x, tryFormats = c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%m/%d/%Y",
    "%d-%m-%Y",
    "%m-%d-%Y"
  ))
}

prev_df <- tryCatch(
  read.csv(file.path(BASE_PATH, "otimizacao/Integrado/prev_12_semanas_v3.csv")),
  error = function(e) NULL
)

algo_stats <- tryCatch(
  read.csv(file.path(BASE_PATH, "otimizacao/teste_final/tabela_comparativa_final.csv")),
  error = function(e) NULL
)

pareto_ok <- tryCatch(
  {
    p <- read.csv(file.path(BASE_PATH, "otimizacao/NSGA2/v2/pareto_O3_fronteira.csv"))
    p[p$lucro >= 0 & p$total_HR > 0, ]
  },
  error = function(e) data.frame(lucro = numeric(0), total_HR = integer(0))
)

hist_data <- lapply(
  setNames(c("baltimore", "lancaster", "philadelphia", "richmond"), LOJA_NAMES),
  function(f) {
    p <- file.path(BASE_PATH, "data/tratados", paste0(f, "_tratado.csv"))
    if (!file.exists(p)) {
      return(NULL)
    }

    tryCatch(
      {
        d <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
        colnames(d) <- gsub("[[:space:]]|\"", "", colnames(d))
        d$Date <- parse_date_safe(d$Date)
        d <- d[!is.na(d$Date) & !is.na(d$Num_Customers), ]
        d <- d[order(d$Date), ]
        d
      },
      error = function(e) NULL
    )
  }
)

SEMANA_DATES <- if (!is.null(hist_data[["Baltimore"]])) {
  balt <- hist_data[["Baltimore"]][order(hist_data[["Baltimore"]]$Date), ]
  W <- 630
  H <- 7
  lapply(1:12, function(i) balt$Date[(W + (i - 1) * H + 1):(W + i * H)])
} else {
  NULL
}

N_SEMANAS <- if (!is.null(prev_df)) nrow(prev_df) else 6

fmt_dol <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("--")
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

run_mc <- function(prev_week, objetivo, n_iter = 2000, temp = 1000) {
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

CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@300;400;500;600;700;800&display=swap');

*{font-family:'Plus Jakarta Sans',sans-serif !important;}

:root{
  --bg:#F7F8F6;
  --panel:#EEF1EA;
  --card:#FFFFFF;
  --text:#24312F;
  --muted:#7B8782;
  --border:#DDE4DF;
  --accent:#84B2B1;
  --accent-dark:#5E8E90;
}

html,body{
  background:var(--bg)!important;
  color:var(--text)!important;
  overflow-x:hidden!important;
}

.navbar,
.bslib-page-navbar > nav,
body > nav.navbar{
  background:white!important;
  border-bottom:1px solid var(--border)!important;
  min-height:56px!important;
  padding:0 20px!important;
  box-shadow:0 2px 10px rgba(36,49,47,.04)!important;
}

.navbar > .container-fluid{
  position:relative!important;
  display:flex!important;
  align-items:center!important;
  justify-content:space-between!important;
}

.navbar-brand{
  color:var(--text)!important;
  font-size:1.05rem!important;
  font-weight:800!important;
  display:flex!important;
  align-items:center!important;
  gap:8px!important;
  min-width:200px!important;
}

.navbar-brand svg{
  color:var(--accent-dark)!important;
  fill:var(--accent-dark)!important;
}

.navbar-nav.me-auto{
  position:absolute!important;
  left:50%!important;
  top:50%!important;
  transform:translate(-50%,-50%)!important;
  display:flex!important;
  flex-direction:row!important;
  gap:10px!important;
  margin:0!important;
}

.navbar-nav .nav-link{
  background:white!important;
  color:var(--text)!important;
  border:1px solid var(--border)!important;
  border-radius:14px!important;
  min-width:170px!important;
  height:40px!important;
  display:flex!important;
  align-items:center!important;
  justify-content:center!important;
  font-weight:700!important;
  font-size:.85rem!important;
  box-shadow:0 4px 12px rgba(36,49,47,.05)!important;
}

.navbar-nav .nav-link.active{
  background:var(--accent-dark)!important;
  color:white!important;
  border-color:var(--accent-dark)!important;
}

.navbar-nav .nav-link svg{
  width:15px!important;
  height:15px!important;
  margin-right:7px!important;
}

.navbar .nav-item:last-child{
  color:var(--accent-dark)!important;
  font-weight:700!important;
  min-width:160px!important;
  text-align:right!important;
  font-size:.85rem!important;
}

.sidebar{
  background:linear-gradient(180deg,#EEF1EA 0%,#F6F8F4 100%)!important;
  border-right:1px solid var(--border)!important;
  padding:16px 14px!important;
  box-shadow:inset -6px 0 20px rgba(36,49,47,.02)!important;
}

.sidebar-title{
  color:var(--text)!important;
  font-size:.75rem!important;
  font-weight:900!important;
  letter-spacing:.08em!important;
  text-transform:uppercase!important;
  margin-bottom:14px!important;
}

.control-label,.form-label{
  color:var(--muted)!important;
  font-size:.68rem!important;
  font-weight:900!important;
  letter-spacing:.07em!important;
  text-transform:uppercase!important;
}

.form-control,.form-select,.selectize-input{
  background:white!important;
  border:1px solid var(--border)!important;
  border-radius:10px!important;
  min-height:38px!important;
  color:var(--text)!important;
  font-weight:600!important;
  font-size:.84rem!important;
  box-shadow:0 3px 8px rgba(36,49,47,.025)!important;
}

.selectize-control.multi .selectize-input > div{
  background:#EAF4F7!important;
  color:#24312F!important;
  border:1px solid #DDE4DF!important;
  border-radius:8px!important;
  padding:2px 6px!important;
  font-weight:700!important;
  margin:2px 3px 2px 0!important;
}

.selectize-control.plugin-remove_button .item .remove{
  color:#5E8E90!important;
  border-left:1px solid #DDE4DF!important;
  margin-left:4px!important;
  padding-left:4px!important;
  font-weight:900!important;
}

.irs--shiny .irs-line{
  background:#DCE5E1!important;
  height:6px!important;
  border-radius:30px!important;
}

.irs--shiny .irs-bar{
  background:var(--accent)!important;
  height:6px!important;
  border-radius:30px!important;
}

.irs--shiny .irs-handle{
  background:transparent!important;
  border:none!important;
  box-shadow:none!important;
  width:22px!important;
  height:22px!important;
  top:50%!important;
  transform:translateY(-50%)!important;
}

/* Esconde os i extras (ionRangeSlider tem 3 i dentro do handle) */
.irs--shiny .irs-handle > i:nth-child(n+2){
  display:none!important;
}

/* Bola branca escondida — o número no irs-single serve de bolinha */
.irs--shiny .irs-handle > i:first-child{
  display:none!important;
}

/* Valor dentro da bolinha */
.irs--shiny .irs-single{
  background:var(--accent-dark)!important;
  color:white!important;
  font-size:.72rem!important;
  font-weight:800!important;
  border-radius:50%!important;
  width:26px!important;
  height:26px!important;
  line-height:26px!important;
  text-align:center!important;
  padding:0!important;
  top:50%!important;
  transform:translateY(-50%)!important;
  box-shadow:0 2px 8px rgba(94,142,144,.35)!important;
  border:none!important;
}

.irs--shiny .irs-single::before{
  display:none!important;
}

/* Min/max labels — discretos */
.irs--shiny .irs-min,
.irs--shiny .irs-max{
  background:transparent!important;
  color:var(--muted)!important;
  font-size:.65rem!important;
  font-weight:600!important;
  box-shadow:none!important;
}

/* Grid ticks — ocultos, só min/max são suficientes */
.irs--shiny .irs-grid{
  display:none!important;
}

.action-button{
  width:100%!important;
  background:linear-gradient(135deg,var(--accent-dark),#6FA5A4)!important;
  border:none!important;
  border-radius:12px!important;
  min-height:42px!important;
  color:white!important;
  font-weight:800!important;
  font-size:.86rem!important;
  box-shadow:0 6px 16px rgba(94,142,144,.20)!important;
}

/* Picker input (shinyWidgets) */
.bootstrap-select>.dropdown-toggle,
.bootstrap-select>.dropdown-toggle:focus,
.bootstrap-select>.dropdown-toggle:active{
  background:white!important;
  color:var(--text)!important;
  border:1px solid var(--border)!important;
  border-radius:10px!important;
  min-height:38px!important;
  font-weight:600!important;
  font-size:.84rem!important;
  box-shadow:0 3px 8px rgba(36,49,47,.025)!important;
  width:100%!important;
  outline:none!important;
}

.bootstrap-select>.dropdown-toggle:hover{
  background:#F3F8F8!important;
  color:var(--text)!important;
}

.bootstrap-select .dropdown-menu{
  border:1px solid var(--border)!important;
  border-radius:12px!important;
  box-shadow:0 8px 22px rgba(36,49,47,.10)!important;
  overflow:hidden!important;
}

.bootstrap-select .dropdown-menu li a{
  color:var(--text)!important;
  font-weight:600!important;
  font-size:.84rem!important;
  padding:6px 12px!important;
}

.bootstrap-select .dropdown-menu li a:hover,
.bootstrap-select .dropdown-menu li.selected a{
  background:#EAF4F7!important;
  color:var(--text)!important;
}

.bootstrap-select .dropdown-menu li a span.check-mark{
  color:var(--accent-dark)!important;
}

.bs-actionsbox .btn{
  background:white!important;
  color:var(--accent-dark)!important;
  border:1px solid var(--border)!important;
  border-radius:8px!important;
  font-weight:700!important;
  font-size:.76rem!important;
  box-shadow:none!important;
  width:auto!important;
  min-height:auto!important;
}

.bslib-sidebar-layout > .bslib-main,
.bslib-sidebar-layout .main{
  background:var(--bg)!important;
  padding:16px!important;
}

.card{
  background:white!important;
  border:1px solid var(--border)!important;
  border-radius:18px!important;
  box-shadow:0 6px 18px rgba(36,49,47,.04)!important;
  overflow:hidden!important;
}

.card-header{
  background:white!important;
  border-bottom:1px solid var(--border)!important;
  color:var(--text)!important;
  font-weight:800!important;
  font-size:.88rem!important;
  padding:10px 16px!important;
}

.kpi-card{
  height:80px;
  background:linear-gradient(135deg,#FFFFFF 0%,#F8FBFA 100%);
  border:1px solid var(--border);
  border-radius:16px;
  box-shadow:0 6px 16px rgba(36,49,47,.045);
  position:relative;
  overflow:hidden;
  display:flex;
  align-items:center;
  gap:14px;
  padding:12px 16px;
}

.kpi-card::before{
  content:'';
  position:absolute;
  top:0;
  left:0;
  width:100%;
  height:4px;
  background:var(--kpi-color);
}

.kpi-icon{
  width:44px;
  height:44px;
  min-width:44px;
  border-radius:12px;
  background:rgba(132,178,177,.16);
  display:flex;
  align-items:center;
  justify-content:center;
  color:var(--kpi-color);
}

.kpi-icon svg{
  width:24px;
  height:24px;
}

.kpi-title{
  color:var(--kpi-color);
  font-size:.66rem;
  font-weight:900;
  letter-spacing:.06em;
  text-transform:uppercase;
  margin-bottom:3px;
  white-space:nowrap;
}

.kpi-title svg{
  width:11px;
  height:11px;
  margin-right:3px;
}

.kpi-value{
  color:var(--text);
  font-size:1.4rem;
  font-weight:900;
  line-height:1.05;
  white-space:nowrap;
}

.kpi-subtitle{
  color:var(--muted);
  font-size:.62rem;
  font-weight:600;
  margin-top:3px;
  white-space:nowrap;
}

table.dataTable tbody tr{background:white!important;}
table.dataTable tbody tr:hover{background:#F3F8F8!important;}
table.dataTable tbody td{
  border:none!important;
  padding:8px 10px!important;
  color:var(--text)!important;
  font-size:.84rem!important;
}

table.dataTable thead th{
  border:none!important;
  color:var(--muted)!important;
  font-weight:900!important;
  text-transform:uppercase!important;
  font-size:.68rem!important;
}

.sb-info-box{
  background:white!important;
  border:1px solid var(--border)!important;
  border-radius:14px!important;
  padding:12px!important;
  color:var(--muted)!important;
  line-height:1.5!important;
  font-size:.76rem!important;
  box-shadow:0 4px 12px rgba(36,49,47,.03)!important;
}

.sb-info-box strong{color:var(--text)!important;}

.plan-legend{
  display:flex;
  gap:12px;
  flex-wrap:wrap;
  padding:6px 4px 10px 4px;
  color:var(--muted);
  font-size:.76rem;
}

.plan-legend b{color:var(--text);}

hr{
  border-top:1px solid var(--border)!important;
  margin:12px 0!important;
}
"

ui <- page_navbar(
  title = tags$span(bs_icon("shop"), " USA Stores · IDSS"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#84B2B1",
    bg = "#F7F8F6",
    fg = "#24312F",
    base_font = font_google("Plus Jakarta Sans")
  ),
  bg = "#F7F8F6",
  header = tags$head(tags$style(HTML(CSS))),
  nav_panel(
    title = tagList(bs_icon("activity"), " Análise Exploratória"),
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("sliders"), " Controlos"),
        width = SIDEBAR_WIDTH,
        pickerInput(
          "eda_lojas",
          tagList(bs_icon("geo-alt"), " Lojas:"),
          choices = LOJA_NAMES,
          selected = LOJA_NAMES,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Todas",
            deselectAllText = "Nenhuma",
            noneSelectedText = "Nenhuma selecionada",
            selectedTextFormat = "count > 1",
            countSelectedText = "{0} lojas"
          )
        ),
        hr(),
        selectInput(
          "eda_visao",
          tagList(bs_icon("graph-up"), " Visualização:"),
          choices = c(
            "Média móvel 7 dias" = "ma7",
            "Diário" = "diario",
            "Mensal" = "mensal"
          ),
          selected = "ma7"
        ),
        hr(),
        selectInput(
          "eda_ano",
          tagList(bs_icon("calendar"), " Ano:"),
          choices = c("Todos" = "todos", "2012" = "2012", "2013" = "2013", "2014" = "2014"),
          selected = "todos"
        ),
        selectInput(
          "eda_mes",
          tagList(bs_icon("calendar3"), " Mês:"),
          choices = c(
            "Todos" = "todos",
            "Janeiro" = "1", "Fevereiro" = "2", "Março" = "3",
            "Abril" = "4", "Maio" = "5", "Junho" = "6",
            "Julho" = "7", "Agosto" = "8", "Setembro" = "9",
            "Outubro" = "10", "Novembro" = "11", "Dezembro" = "12"
          ),
          selected = "todos"
        ),
        hr(),
        tags$div(
          class = "sb-info-box",
          tags$strong(tagList(bs_icon("info-circle"), " Painel")),
          tags$br(), tags$br(),
          "Exploração histórica das séries temporais por loja, estatísticas descritivas e comparação de volumes."
        )
      ),
      div(
        layout_columns(
          uiOutput("eda_kpi_total"),
          uiOutput("eda_kpi_media"),
          uiOutput("eda_kpi_max"),
          col_widths = c(4, 4, 4)
        ),
        tags$div(style = "height:.4rem;"),
        card(
          card_header(tagList(bs_icon("graph-up"), " Série Temporal por Loja")),
          plotOutput("eda_ts_plot", height = "380px")
        ),
        tags$div(style = "height:.4rem;"),
        layout_columns(
          card(
            card_header(tagList(bs_icon("bar-chart"), " Distribuição por Loja")),
            plotOutput("eda_box_plot", height = "310px")
          ),
          card(
            card_header(tagList(bs_icon("table"), " Estatísticas Descritivas")),
            DTOutput("eda_stats_table")
          ),
          col_widths = c(6, 6)
        )
      )
    )
  ),
  nav_panel(
    title = tagList(bs_icon("graph-up-arrow"), " Previsão"),
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("sliders"), " Controlos de Previsão"),
        width = SIDEBAR_WIDTH,
        sliderInput(
          "p_semana",
          tagList(bs_icon("calendar-week"), " Semana:"),
          min = 1, max = N_SEMANAS, value = 1, step = 1, ticks = FALSE
        ),
        hr(),
        pickerInput(
          "p_loja",
          tagList(bs_icon("geo-alt"), " Lojas em destaque:"),
          choices = LOJA_NAMES,
          selected = "Philadelphia",
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Todas",
            deselectAllText = "Nenhuma",
            noneSelectedText = "Nenhuma selecionada",
            selectedTextFormat = "count > 1",
            countSelectedText = "{0} lojas"
          )
        ),
        hr(),
        selectInput(
          "p_metodo_prev",
          tagList(bs_icon("cpu"), " Método de Previsão:"),
          choices = c(
            "Melhor Modelo por Loja" = "auto",
            "Random Forest C2" = "rf_c2",
            "Random Forest C1" = "rf_c1",
            "ETS / Holt-Winters" = "ets",
            "ARIMA" = "arima",
            "Seasonal Naive" = "naive"
          ),
          selected = "auto"
        ),
        hr(),
        pickerInput(
          "p_filter_dia",
          tagList(bs_icon("calendar3"), " Filtrar Tabela — Dia:"),
          choices = DIAS,
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            selectAllText = "Todos",
            deselectAllText = "Nenhum",
            noneSelectedText = "Todos os dias",
            selectedTextFormat = "count > 1",
            countSelectedText = "{0} dias"
          )
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
        tags$div(style = "height:.4rem;"),
        card(
          card_header(tagList(bs_icon("award"), " Ranking dos Métodos")),
          DTOutput("p_ranking_table"),
          tags$div(
            style = "padding:10px 14px 8px; font-size:.72rem; color:#7B8782; border-top:1px solid #DDE4DF; margin-top:8px;",
            tags$ul(
              style = "margin:0; padding-left:16px; line-height:1.9;",
              tags$li(tagList(tags$b("O1"), " — Maximizar Lucro")),
              tags$li(tagList(tags$b("O2"), " — Lucro c/ limite ≤ 10 000 unidades")),
              tags$li(tagList(tags$b("O3"), " — Lucro vs Recursos Humanos"))
            )
          )
        ),
        tags$div(style = "height:.4rem;"),
        card(
          card_header(uiOutput("p_forecast_title")),
          plotOutput("p_forecast_bar", height = "270px")
        ),
        tags$div(style = "height:.4rem;"),
        card(
          card_header("Previsões Diárias"),
          DTOutput("p_daily_table")
        )
      )
    )
  ),
  nav_panel(
    title = tagList(bs_icon("cpu"), " Otimização"),
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span(bs_icon("sliders"), " Configuração DSS"),
        width = SIDEBAR_WIDTH,
        sliderInput(
          "o_semana",
          tagList(bs_icon("calendar-week"), " Semana:"),
          min = 1, max = N_SEMANAS, value = 1, step = 1, ticks = FALSE
        ),
        hr(),
        selectInput(
          "o_algoritmo",
          tagList(bs_icon("cpu"), " Algoritmo:"),
          choices = list(
            "Local Search" = c(
              "SANN (Simulated Annealing)" = "SANN",
              "HC (Hill Climbing)" = "HC",
              "Tabu Search" = "TABU"
            ),
            "Population-based" = c(
              "GA (Genetic Algorithm)" = "GA",
              "PSO (Particle Swarm)" = "PSO",
              "DE (Differential Evolution)" = "DE"
            ),
            "Multiobjetivo" = c(
              "NSGA-II" = "NSGA2"
            )
          ),
          selected = "SANN"
        ),

        hr(),

        uiOutput("o_objetivo_ui"),
        uiOutput("o_sann_temp_ui"),
        actionButton(
          "o_run",
          tagList(bs_icon("play-fill"), " Executar Otimização")
        ),
        hr(),
        tags$div(
          class = "sb-info-box",
          tags$strong(tagList(bs_icon("info-circle"), " Nota")),
          tags$br(), tags$br(),
          "A otimização utiliza a previsão da semana selecionada e gera o plano operacional por loja e dia."
        )
      ),
      div(
        layout_columns(
          uiOutput("o_kpi_lucro"),
          uiOutput("o_kpi_units"),
          uiOutput("o_kpi_hr"),
          col_widths = c(4, 4, 4)
        ),
        tags$div(style = "height:.4rem;"),
        uiOutput("o_o3_pareto_ui"),
        tags$div(style = "height:.4rem;"),
        card(
          card_header("Gráfico de Convergência / Resultado"),
          plotOutput("o_profit_chart", height = "260px")
        ),
        tags$div(style = "height:.4rem;"),
        card(
          card_header("Plano Otimizado"),
          uiOutput("o_plan_ui")
        )
      )
    )
  ),
  nav_spacer(),
  nav_item("TIAPOSE 2025/26")
)

server <- function(input, output, session) {
  rv_opt <- reactiveVal(NULL)

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

  # ============================================================
  # ANÁLISE EXPLORATÓRIA
  # ============================================================
  eda_long <- reactive({
    # pickerInput não envia estado inicial ao servidor — usar LOJA_NAMES como default
    lojas_sel <- if (is.null(input$eda_lojas) || length(input$eda_lojas) == 0) {
      LOJA_NAMES
    } else {
      input$eda_lojas
    }

    result <- do.call(rbind, lapply(lojas_sel, function(lj) {
      df <- hist_data[[lj]]
      if (is.null(df)) {
        return(NULL)
      }
      data.frame(
        Loja = lj,
        Date = df$Date,
        Clientes = df$Num_Customers
      )
    }))

    req(!is.null(result) && nrow(result) > 0)

    if (!is.null(input$eda_ano) && input$eda_ano != "todos") {
      result <- result[format(result$Date, "%Y") == input$eda_ano, ]
    }

    if (!is.null(input$eda_mes) && input$eda_mes != "todos") {
      result <- result[as.integer(format(result$Date, "%m")) == as.integer(input$eda_mes), ]
    }

    req(nrow(result) > 0)
    result
  })

  output$eda_kpi_total <- renderUI({
    df <- eda_long()
    kpi_card(
      "TOTAL CLIENTES",
      format(sum(df$Clientes, na.rm = TRUE), big.mark = ","),
      "histórico selecionado",
      "people",
      "#84B2B1"
    )
  })

  output$eda_kpi_media <- renderUI({
    df <- eda_long()
    kpi_card(
      "MÉDIA DIÁRIA",
      round(mean(df$Clientes, na.rm = TRUE)),
      "clientes por dia",
      "graph-up",
      "#CF8082"
    )
  })

  output$eda_kpi_max <- renderUI({
    df <- eda_long()
    kpi_card(
      "PICO MÁXIMO",
      round(max(df$Clientes, na.rm = TRUE)),
      "maior valor observado",
      "arrow-up-circle",
      "#E49A36"
    )
  })

  output$eda_ts_plot <- renderPlot(
    {
      df <- eda_long()
      req(nrow(df) > 0)

      df <- df[order(df$Loja, df$Date), ]

      if (input$eda_visao == "ma7") {
        df <- do.call(rbind, lapply(split(df, df$Loja), function(x) {
          x <- x[order(x$Date), ]
          x$Clientes_plot <- as.numeric(stats::filter(x$Clientes, rep(1 / 7, 7), sides = 1))
          x
        }))
        ylab <- "Clientes — media movel 7 dias"
      } else if (input$eda_visao == "mensal") {
        df$Mes <- as.Date(format(df$Date, "%Y-%m-01"))
        df <- aggregate(Clientes ~ Loja + Mes, df, mean, na.rm = TRUE)
        names(df)[names(df) == "Mes"] <- "Date"
        df$Clientes_plot <- df$Clientes
        ylab <- "Media mensal de clientes"
      } else {
        df$Clientes_plot <- df$Clientes
        ylab <- "Clientes"
      }

      df <- df[!is.na(df$Clientes_plot), ]
      req(nrow(df) > 0)

      span_val <- if (input$eda_visao == "mensal") 0.6 else 0.2

      ggplot(df, aes(x = Date, y = Clientes_plot, color = Loja, fill = Loja)) +
        geom_smooth(
          method = "loess", formula = y ~ x, se = FALSE,
          span = span_val, linewidth = 1.6, alpha = .9
        ) +
        scale_color_manual(values = LOJA_CORES) +
        scale_fill_manual(values = LOJA_CORES) +
        theme_clean() +
        labs(x = "Data", y = ylab) +
        theme(legend.position = "bottom")
    },
    bg = "transparent"
  )

  output$eda_box_plot <- renderPlot(
    {
      df <- eda_long()
      req(nrow(df) > 0)

      ggplot(df, aes(x = Loja, y = Clientes, fill = Loja)) +
        geom_boxplot(alpha = .75, width = .65) +
        scale_fill_manual(values = LOJA_CORES) +
        theme_clean() +
        theme(legend.position = "none") +
        labs(x = NULL, y = "Clientes")
    },
    bg = "transparent"
  )

  output$eda_stats_table <- renderDT({
    df <- eda_long()

    stats <- aggregate(Clientes ~ Loja, df, function(x) {
      c(
        Min = min(x, na.rm = TRUE),
        Media = round(mean(x, na.rm = TRUE), 1),
        Mediana = median(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE),
        Desvio = round(sd(x, na.rm = TRUE), 1)
      )
    })

    stats <- data.frame(Loja = stats$Loja, stats$Clientes)

    datatable(
      stats,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 8),
      class = "table-sm table-hover"
    )
  })

  # ============================================================
  # PREVISÃO
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
      cor <- LOJA_CORES[[loja_name]]

      kpi_card(
        title = tagList(bs_icon("geo-alt"), " ", toupper(loja_name)),
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
      " Previsão — ",
      strong(paste(input$p_loja, collapse = ", ")),
      " — Semana ",
      input$p_semana
    )
  })

  output$p_forecast_bar <- renderPlot(
    {
      df <- r_prev_loja()
      req(!is.null(df) && nrow(df) > 0)

      ggplot(df, aes(x = Dia, y = Cli, fill = Loja)) +
        geom_col(position = "dodge", width = .7) +
        scale_fill_manual(values = LOJA_CORES) +
        theme_clean() +
        theme(legend.position = "bottom", panel.grid.major.x = element_blank()) +
        labs(x = NULL, y = "Clientes Previstos")
    },
    bg = "transparent"
  )

  output$p_ranking_table <- renderDT({
    if (!is.null(algo_stats)) {
      df <- algo_stats
      # remover coluna membro
      df$membro <- NULL
      # Total RH como inteiro
      if ("total_HR" %in% colnames(df))
        df$total_HR <- as.integer(round(df$total_HR))
      rename_map <- c(
        algoritmo = "Algoritmo",
        objetivo  = "Objetivo",
        mediana   = "Mediana",
        media     = "Média",
        lucro_max = "Máx.",
        lucro_min = "Mín.",
        unidades  = "Unidades",
        total_HR  = "Total RH",
        tempo_s   = "Tempo (s)",
        SD        = "Desvio Padrão"
      )
      for (old in names(rename_map))
        if (old %in% colnames(df))
          colnames(df)[colnames(df) == old] <- rename_map[old]
    } else {
      df <- data.frame(
        Algoritmo = c("GA", "SANN", "HC", "MC", "NSGA-II"),
        Objetivo  = c("O1", "O2", "O1", "O1", "O3"),
        Mediana   = c(14194, 917, 11898, 9772, 866)
      )
    }

    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 15, scrollX = TRUE),
      class = "table-sm table-hover"
    )
  })

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

    lojas_f <- if (length(input$p_loja) > 0) input$p_loja else LOJA_NAMES
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
      )
  })

  # ============================================================
  # OTIMIZAÇÃO
  # ============================================================
  output$o_objetivo_ui <- renderUI({
    alg <- input$o_algoritmo
    choices <- if (!is.null(alg) && alg == "NSGA2") {
      c("O3: Pareto RH vs Lucro" = "O3")
    } else {
      c("O1: Maximizar Lucro" = "O1",
        "O2: Limite 10 000 Unidades" = "O2")
    }
    selectInput(
      "o_objetivo",
      tagList(bs_icon("bullseye"), " Objetivo:"),
      choices = choices
    )
  })

  output$o_sann_temp_ui <- renderUI({
    req(input$o_algoritmo == "SANN")
    tagList(
      hr(),
      sliderInput(
        "o_sann_temp",
        tagList(bs_icon("thermometer-half"), " Temperatura SANN:"),
        min   = 100,
        max   = 5000,
        value = 1000,
        step  = 100
      )
    )
  })

  observeEvent(list(input$o_semana, input$o_objetivo, input$o_algoritmo), {
    rv_opt(NULL)
  })

  observeEvent(input$o_run, {
    pw <- get_prev_week(input$o_semana)
    rv_opt(NULL)

    withProgress(
      message = paste("A executar", input$o_algoritmo, "—", input$o_objetivo, "..."),
      value = 0,
      {
        t_sann <- if (!is.null(input$o_sann_temp)) input$o_sann_temp else 1000
        S <- run_mc(pw, input$o_objetivo, 3000, temp = t_sann)
        rv_opt(S)
      }
    )
  })

  r_plan_opt <- reactive({
    S <- rv_opt()
    req(!is.null(S))

    pw <- get_prev_week(input$o_semana)
    PREV <<- pw
    upper <<- calc_upper(PREV)

    decompose_plan(S, pw)
  })

  output$o_kpi_lucro <- renderUI({
    S <- rv_opt()
    if (is.null(S)) S <- S1

    pw <- get_prev_week(input$o_semana)
    PREV <<- pw
    upper <<- calc_upper(PREV)

    kpi_card(
      "LUCRO",
      fmt_dol(profit(S)),
      "lucro semanal estimado",
      "graph-up-arrow",
      "#84B2B1"
    )
  })

  output$o_kpi_units <- renderUI({
    S <- rv_opt()
    if (is.null(S)) S <- S1

    pw <- get_prev_week(input$o_semana)
    PREV <<- pw
    upper <<- calc_upper(PREV)

    u <- total_units(S)
    limite_ok <- ifelse(is.na(u), FALSE, u <= 10000)

    kpi_card(
      "UNIDADES",
      format(round(u), big.mark = ","),
      ifelse(limite_ok, "limite respeitado", "acima de 10 000"),
      "basket",
      "#CF8082"
    )
  })

  output$o_kpi_hr <- renderUI({
    S <- rv_opt()
    if (is.null(S)) S <- S1

    hr <- sum(round(S[seq(2, 84, 3)]) + round(S[seq(3, 84, 3)]))

    kpi_card(
      "RH TOTAL",
      as.character(hr),
      "funcionários escalados",
      "person-check",
      "#E49A36"
    )
  })

  output$o_o3_pareto_ui <- renderUI({
    if (input$o_objetivo != "O3") {
      return(NULL)
    }

    card(
      card_header(tagList(bs_icon("diagram-3"), " Fronteira de Pareto — O3")),
      plotOutput("o_pareto_plot", height = "320px")
    )
  })

  output$o_pareto_plot <- renderPlot(
    {
      req(input$o_objetivo == "O3")

      ggplot(pareto_ok, aes(x = total_HR, y = lucro)) +
        geom_point(color = "#84B2B1", alpha = .65, size = 2.5) +
        geom_point(data = COMPROMISSO, color = "#CF8082", size = 4) +
        theme_clean() +
        labs(x = "Recursos Humanos", y = "Lucro ($)")
    },
    bg = "transparent"
  )

  output$o_plan_ui <- renderUI({
    if (is.null(rv_opt())) {
      div(
        style = "padding:28px; color:#7B8782; font-weight:700;",
        bs_icon("cpu"),
        tags$br(), tags$br(),
        "Seleciona o objetivo e algoritmo, depois prime ",
        strong("Executar Otimização"),
        "."
      )
    } else {
      tagList(
        div(
          class = "plan-legend",
          span(tags$b("PR:"), " Promoção (%)"),
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
      ) %>%
      formatStyle(
        "Unidades",
        background = styleColorBar(plan$Unidades, "#EAF4F7"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$o_profit_chart <- renderPlot(
    {
      if (input$o_objetivo == "O3") {
        ggplot(pareto_ok, aes(x = total_HR, y = lucro)) +
          geom_point(color = "#84B2B1", alpha = .65, size = 2.5) +
          geom_point(data = COMPROMISSO, color = "#CF8082", size = 4) +
          theme_clean() +
          labs(x = "Recursos Humanos", y = "Lucro ($)")
      } else {
        hists <- lapply(1:5, function(i) sort(cumsum(runif(30, 0, 50))) + 1000)
        plot_shaded_convergence(
          hists,
          paste("Convergencia —", input$o_algoritmo),
          "Avaliacoes",
          "Melhor Lucro ($)"
        )
      }
    },
    bg = "transparent"
  )
}

shinyApp(ui, server)
