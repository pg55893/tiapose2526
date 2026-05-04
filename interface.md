# Contexto DSS – Interface Shiny (TIAPOSE/TADA 2025-26)

## Visão Geral

Desenvolver uma **aplicação Shiny em R** que funcione como sistema inteligente de apoio à decisão (DSS), integrando os módulos de **Previsão** e **Otimização** para as 4 lojas (Baltimore, Lancaster, Philadelphia, Richmond).

O Prof. Paulo Cortez indica explicitamente no guia:
> "The DSS can combine the forecasting and optimization R code ideally via an interface, which can be graphical (e.g., via the shiny R package) or using a console mode."

O interface gráfico via Shiny é classificado como **"better"** face ao modo consola.

---

## Estrutura mínima exigida (conforme enunciado)

O fluxo de interação com o utilizador deve ser:

1. O utilizador **escolhe a semana** para a qual quer um plano.
2. O sistema apresenta as **previsões de Num_Customers** para essa semana (e valores reais, se disponíveis).
3. O sistema apresenta o **plano otimizado** para cada dia, com informação relevante: número de clientes, unidades vendidas, vendas, custos, lucro total.

---

## Dados e Ficheiros

### CSVs por loja

- `baltimore.csv`, `lancaster.csv`, `philadelphia.csv`, `richmond.csv`
- 714 registos diários cada (~2012–2014)
- Colunas: `Date`, `Num_Employees`, `Num_Customers`, `Pct_On_Sale`, `TouristEvent`, `Sales`

### Variável alvo

- `Num_Customers` (previsão para H=7 dias à frente)

### Pré-processamento já feito

- Dias com previsão negativa → forçar 0
- Natal (2012-12-25, 2013-12-25) e Páscoa (2013-03-31, 2014-04-20) → Num_Customers = 0
- Black Friday (2012-11-23, 2013-11-29) → TouristEvent = "Yes"
- Imputação de Pct_On_Sale para 2014-04-20: mediana do mesmo dia da semana

---

## Modelos de Previsão disponíveis (outputs já calculados)

O grupo usou backtesting com growing window, 12 iterações, H=7.

**Melhor modelo por loja (Cenário 2 – backtesting):**

- Baltimore → ETS/Holt-Winters
- Lancaster → Random Forest
- Philadelphia → Random Forest
- Richmond → Random Forest

Os outputs de previsão foram guardados em CSVs por modelo. A interface deve **ler os CSVs com as previsões** em vez de re-executar os modelos.

---

## Fórmulas de Otimização

### Parâmetros por loja

| Loja         | Fj   | Fx   | Ws  |
|--------------|------|------|-----|
| Baltimore    | 1.00 | 1.15 | 700 |
| Lancaster    | 1.05 | 1.20 | 730 |
| Philadelphia | 1.10 | 1.15 | 760 |
| Richmond     | 1.15 | 1.25 | 800 |

### Custos de RH

- Junior (J): 60 USD/dia útil, 70 USD/fim de semana
- Expert (X): 80 USD/dia útil, 95 USD/fim de semana

### Clientes assistidos por dia

```
As,d = min(7 * Xs,d + 6 * Js,d, Cs,d)
```

onde `Cs,d` = Num_Customers previsto.

### Unidades vendidas por cliente c

```
Us,d,c = round(F * 10 / ln(2 - PRs,d))
```

F = Fj para Junior, Fx para Expert.

### Lucro por cliente c

```
Ps,d,c = round(Us,d,c * (1 - PRs,d) * 1.07)
```

### Retorno diário da loja

```
Rs,d = sum_c(Ps,d,c) - Js,d * hr_J - Xs,d * hr_X
```

### Lucro semanal da loja

```
Rs = sum_d(Rs,d) - Ws
```

### Vetor solução

84 parâmetros: 4 lojas × 7 dias × (J, X, PR)

- J, X: inteiros (usar `round()` dentro da função de avaliação)
- PR: real em [0, 0.3]

---

## Objetivos de Otimização

- **O1**: Maximizar lucro total (Rs) — independente por loja
- **O2**: Maximizar O1 com restrição: máximo 10.000 unidades vendidas no total (todas as lojas)
- **O3**: Maximizar O2 e minimizar total de RH (NSGA-II ou pesos)

### Métodos de otimização implementados pelo grupo

- Monte Carlo (pesquisa cega)
- Hill Climbing / Simulated Annealing (`optim()` com `method="SANN"`, `gr=rchange`)
- Algoritmos Genéticos (`genalg`, `rbga`)
- NSGA-II (`mco`) para O3

---

## Estrutura recomendada para a app Shiny

### Layout geral (sugerido: `navbarPage` ou `tabsetPanel`)

```
App
├── Tab 1: Previsão
│   ├── Sidebar: seletor de semana + seletor de loja + modelo (ETS / RF / ...)
│   └── Main: gráfico previsto vs real + tabela com valores diários + métricas
│
├── Tab 2: Otimização
│   ├── Sidebar: seletor de semana + objetivo (O1 / O2 / O3) + método
│   └── Main: tabela do plano semanal (J, X, PR por dia e loja) + KPIs (lucro, unidades, RH)
│
└── Tab 3: DSS Integrado
    ├── Sidebar: seletor de semana + objetivo
    └── Main: previsões + plano otimizado lado a lado + sumário executivo
```

### Outputs prioritários na interface

- Gráfico `plotOutput`: previsto vs real por loja (linha temporal)
- Tabela `tableOutput` ou `DT::dataTableOutput`: plano diário (J, X, PR, lucro, unidades)
- KPIs via `valueBox` (shinydashboard) ou `infoBox`: lucro total, unidades totais, nº total de RH
- Seletor de semana: `selectInput` ou `dateInput` (range das semanas disponíveis nos dados de teste)

---

## Regras de implementação

### Vector ↔ Matrix em R

```r
# Solução como vetor → matriz (4 lojas x 7 dias x 3 params)
s <- rep(0, 84)
# reorganizar internamente na evaluation():
J  <- round(s[seq(1, 84, by=3)])
X  <- round(s[seq(2, 84, by=3)])
PR <- s[seq(3, 84, by=3)]
```

### Infeasibility (O2, O3)

Duas estratégias aceites pelo professor:

- **Death penalty**: devolver `-Inf` se total unidades > 10.000
- **Repair**: corrigir a solução com função `repair(s)`, guardar melhor com `BEST <<- s`

### Previsões como input da otimização

O enunciado indica que o DSS deve usar **valores previstos** (não reais) para otimizar. Ler o CSV de previsões da semana selecionada e passar `Cs,d` à função de avaliação.

---

## O que o Prof. avalia na interface (citação direta)

> "System developed, including a console or graphical (better) mode interface (e.g., via shiny.rstudio.com) to demonstrate the complete Intelligent Data Analysis system (prediction and optimization) at work."

> "Better if: several forecasting / optimization methods are compared; forecasts are used by the optimization methods."

Pontos de diferenciação para nota mais alta:

- Interface permite comparar métodos de previsão visualmente
- Interface permite selecionar objetivo de otimização (O1/O2/O3) e ver impacto
- Interface mostra plano dia a dia com breakdown de custos e lucro
- Estudo do efeito de parâmetros (ex: temperatura inicial no SANN, tamanho da população no GA)

---

## Exemplo de referência do professor (wineApp)

O professor disponibilizou `run-wineApp.R` como exemplo de DSS Shiny:

```r
library(shiny)
runApp("wineApp")
```

A estrutura típica é: pasta `wineApp/` com `ui.R` + `server.R` (ou ficheiro único `app.R`).

---

## Notas de desenvolvimento

- Usar `shiny` + `ggplot2` para gráficos
- Tabelas interativas: `DT` package (`DT::datatable()`)
- KPI boxes: `shinydashboard` (opcional mas visualmente melhor)
- Carregar previsões de CSVs pré-calculados (não re-treinar modelos na app)
- A otimização pode correr em `reactive()` com `withProgress()` para feedback visual
- Usar `round()` sempre antes de mostrar J e X ao utilizador
- O vídeo demo YouTube (máx. 5 min) deve mostrar a app a funcionar de ponta a ponta: selecionar semana → ver previsões → ver plano otimizado → interpretar resultados
