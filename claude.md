# CLAUDE.md — Projeto TIAPOSE/TADA 2025/26

## Contexto Geral

Projeto de grupo da UC TADA (Prof. Paulo Cortez, Universidade do Minho).
Objetivo: construir um **Intelligent Decision Support System (IDSS)** para gerir 4 lojas de retalho nos EUA.
Entrega final: **2026-05-27** | Vale 75% da nota.
Resumos semanais obrigatórios a partir de 2026-03-16.
Demo em vídeo YouTube (máx. 5 min) obrigatório.

**Grupo:** Eduardo (RF + HW multivariate), João (MLPE/MLP), Carolina/Chefa (ARIMA/ARIMAX), Nuno/Edu (RF univariate + HW univariate)

---

## Dataset

4 lojas × 714 registos (~2012–2014). Ficheiros: `baltimore.csv`, `lancaster.csv`, `philadelphia.csv`, `richmond.csv`

Variáveis: `Date`, `Num_Employees`, `Num_Customers`, `Pct_On_Sale`, `TouristEvent`, `Sales`

**Target da previsão: `Num_Customers`** (não `Sales` — mudança feita no início do projeto)

---

## Ambiente de Trabalho

- R no macOS Apple Silicon M4 (username: `edias`)
- Base path: `~/TIAPOSE2526/`
- CSVs em: `data/`
- Outputs de modelos em: `multivariado/[NomeModelo]/` ou `univariado/[NomeModelo]/`
- Sempre fazer `setwd("data/")` antes de `source("../utils/tratamentoDeDados.R")`
- GitHub: `pg55893/tiapose2526` | username pessoal: `Eduardomfdias` / `pg55893`
- Sempre `git pull` antes de começar a trabalhar e antes de fazer `push`

Packages principais: `rminer`, `genalg`, `mco`, `optim` (SANN), `forecast`, `corrplot`, `rstudioapi`

Fix para macOS Apple Silicon com rminer: `install.packages("rminer", type="binary")`
Não usar `dev.new()` — deixar plots renderizar no painel Plots do RStudio.

---

## Estrutura de Scripts

- `tratamentoDeDados.R` — pré-processamento partilhado
- `funcoes.R` — funções da Fase I
- `backtesting.R` — Fase II (backtesting)
- `preparar_dados()` faz source de `tratamentoDeDados.R` internamente (não duplicar)
- `utils/multi-utils.R` — funções: `mfit()`, `lforecastm()`

---

## Fase I — Previsão (concluída)

### Backtesting
- **Growing window**, 12 iterações, **H=7**
- Métricas: **NMAE** (primária), **RRSE** (secundária), **R²** (terciária)
- Agregação por **média** (não mediana — confirmado pelo professor)
- Seleção lexicográfica com tolerância de 5% no NMAE

### Pós-processamento
- Previsões negativas → 0
- Natal e Páscoa forçados a 0:
  - Natal: 2012-12-25, 2013-12-25, 2014-12-25
  - Páscoa: 2013-03-31, 2014-04-20

### Cenários Multivariados
- C1 e C2 apenas para modelos ML (RF, MLPE)
- C1 removido da comparação final (C2 sempre superior)
- ARIMA, ETS/HW, VAR usam mecanismos próprios para exógenas

### Modelos e Seleção Final por Loja
- **Baltimore** → ETS/HW
- **Lancaster** → Random Forest
- **Philadelphia** → Random Forest
- **Richmond** → Random Forest

RF globalmente mais robusto. VAR incluído por projeto. ETS é a evolução natural do HW com suporte a variáveis exógenas.

---

## Fase II — Otimização (Resultados Atuais)

### Melhores Resultados por Objetivo

| Objetivo | Algoritmo | Autor | Profit ($) | Unidades | HR |
|---|---|---|---|---|---|
| **O1** (Max Profit) | **GA (rbga)** | João | **17.330** | 58.582 | 529 |
| **O2** (Constraint 10k) | **SANN** | Eduardo | **1.145** | 9.999 | 101 |
| **O3** (Bi-objetivo) | **NSGA2** | Eduardo | **1.241** | 9.911 | 80* |

*\*Nota: O resulto de O3 apresentado é o ponto de compromisso selecionado da fronteira de Pareto (L=1241, HR=80).*

### Comparação Geral
- **GA (João):** Melhor performance em O1 (pesquisa global mais eficaz).
- **NSGA2 (Eduardo):** Encontrou soluções O3 superiores às de O2 (curiosamente com menos funcionários e mais lucro), sendo o algoritmo escolhido para a aplicação final.
- **SANN:** Robusto para O2, mas superado pelo NSGA2 na exploração do trade-off HR/Lucro.
- **Monte Carlo (Nuno):** Serve como baseline, apresentando lucros significativamente inferiores.

---

## Estrutura do Vetor Solução (84 parâmetros)

Para cada (loja `s`, dia `d`):
- `J` (juniors): inteiro
- `X` (experts): inteiro  
- `PR` (promotion rate): contínuo ∈ [0, 0.3]

Ordem no vetor: `c(PR, J, X)` repetido 28 vezes (4 lojas × 7 dias).

**Rounding seletivo:** `round()` aplica-se APENAS aos índices de J e X. NUNCA ao vetor completo (corromperia PR).

### Parâmetros por Loja

| Loja | Fj | Fx | Ws |
|---|---|---|---|
| Baltimore | 1.00 | 1.15 | 700 |
| Lancaster | 1.05 | 1.20 | 730 |
| Philadelphia | 1.10 | 1.15 | 760 |
| Richmond | 1.15 | 1.25 | 800 |

Custos HR:
- Junior: 60/dia semana, 70/fim de semana
- Expert: 80/dia semana, 95/fim de semana

### Fórmulas de Otimização

```
As,d = min(7*X + 6*J, Cs,d)               # clientes atendidos (Cs,d = previsão)
Us,d,j = round(Fj * 10 / log(2 - PR))      # unidades/cliente Junior
Us,d,x = round(Fx * 10 / log(2 - PR))      # unidades/cliente Expert
Ps,d,j = round(Us,d,j * (1 - PR) * 1.07)   # preço/unidade Junior (com IVA)
Ps,d,x = round(Us,d,x * (1 - PR) * 1.07)   # preço/unidade Expert (com IVA)
Rs,d = (A_j*Ps,d,j + A_x*Ps,d,x) - J*hr_J - X*hr_X   # revenue diário (A_j/A_x = clientes atendidos por tipo)
Rs = sum(Rs,d) - Ws                        # profit semanal (deduz custos fixos)
```

### Convenção eval()
```r
eval <- function(S) -profit(S)   # negado para algoritmos de minimização (SANN, etc.)
```

### Bounds do Vetor
Upper bounds derivados via `calc_upper(PREV)` com base nas previsões:
- J pode atender 6 clientes, X pode atender 7

Solução teste válida: `S1 = rep(c(0.10, 5, 3), 28)` — PR ∈ [0, 0.3], J e X positivos inteiros.

### Objetivos

- **O1:** Maximizar profit total (cada loja independente, `optim` SANN)
- **O2:** O1 com constraint hard — total de unidades vendidas ≤ 10.000 (todas as lojas em simultâneo); death penalty para violações
- **O3:** Bi-objetivo — maximizar O2 + minimizar total de funcionários (HR count); usa NSGA-II via package `mco`

O2 e O3 envolvem todas as lojas simultaneamente.

### Algoritmos a Implementar (pipeline completo)

1. Monte Carlo (pesquisa cega)
2. Hill Climbing (pesquisa local)
3. Simulated Annealing — `optim(..., method="SANN")`
4. Algoritmos Genéticos — package `genalg`
5. NSGA-II — package `mco` (para O3 bi-objetivo)

Implementar curvas de convergência com wrapper de função traced.

---

## O que Falta Fazer

- [x] Implementação dos algoritmos (MC, HC, SANN, GA, NSGA-II)
- [x] Scripts de comparação e figuras de convergência (`comparativos.R`)
- [ ] Integração da solução NSGA-II na **App Shiny DSS** (Foco atual)
- [ ] Relatório final (20–40 páginas)
- [ ] Vídeo demo YouTube (máx. 5 min)
- [ ] Resumos semanais (ongoing)

---

## Estrutura do Relatório

Secções: Introdução, Execução do Projeto (auto-avaliação grupo A 0–20 + individual), Previsão, Otimização, Demo (link YouTube), Conclusões, Bibliografia, Apêndices.

O grupo propõe nota A (0–20); professor atribui nota final P.

---

## Erros Comuns a Evitar

- `round()` no vetor inteiro → corrompe PR. Só arredondar índices J e X.
- `eval()` retornar profit positivo em vez de `-profit()` → algoritmos de minimização ficam errados.
- Agregação por mediana em vez de **média** no backtesting → erro crítico nas métricas.
- Rolling window em vez de **growing window** → João teve de corrigir.
- Menos de 12 iterações no backtesting → Nuno tinha 10, teve de corrigir.
- C1/C2 aplicados a ARIMA/ETS → cenários multivariados só para ML.
- `dev.new()` em macOS → plots não aparecem no RStudio.

---

## Metodologia

O projeto segue **CRISP-DM**: Business Understanding → Data Understanding → Data Preparation → Modeling → Evaluation → Deployment (Shiny app).

---

## Referências Bibliográficas Base

- Michalewicz et al. (2007) *Adaptive Business Intelligence*
- Cortez (2021) *Modern Optimization with R*
- Witten et al. (2017) *Data Mining*
- Hyndman & Athanasopoulos (2021) *Forecasting: Principles and Practice*