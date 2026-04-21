# 🗂️ Repositório Organizad - TIAPOSE2526

Este repositório foi reorganizado para facilitar a navegação e o desenvolvimento do projeto. Abaixo está a descrição da nova estrutura.

## 🏗️ Nova Estrutura

```
TIAPOSE2526/
├── data/                  # Dados originais em CSV (Baltimore, Lancaster, Philadelphia, Richmond)
├── docs/                  # Documentação, enunciados e guiões do projeto
├── EDA/                   # Análise Exploratória de Dados
│   ├── output/            # Logs e PDFs gerados pela análise
│   └── *.R                # Scripts de análise por loja e comparativos
├── univariado/            # Modelos de Previsão Univariada (Fase 1)
│   ├── ARIMA/             # Scripts e resultados ARIMA
│   ├── HoltWinters/       # Scripts e resultados Holt-Winters
│   ├── MLPE/              # Scripts e resultados MLPE
│   └── RF/                # Scripts e resultados Random Forest
├── multivariado/          # Modelos de Previsão Multivariada
│   ├── ARIMAX/            # Scripts e resultados ARIMAX (Cenários 1 e 2)
│   ├── HoltWinters/       # Scripts e resultados ETS e VAR
│   ├── MLPE/              # Scripts e resultados MLPE Multi
│   ├── RF/                # Scripts e resultados Random Forest Multi
│   └── CCF/               # Análise de Correlação Cruzada
├── otimizacao/            # Algoritmos de Otimização (Fase 2)
│   ├── GA/                # Algoritmo Genético
│   ├── MC/                # Monte Carlo
│   ├── NSGA2/             # NSGA-II (Otimização Multiobjectivo)
│   ├── PSO/               # Particle Swarm Optimization
│   ├── SANN/              # Simulated Annealing
│   └── comparativos.R     # Script de comparação de resultados
├── utils/                 # Funções auxiliares e configurações partilhadas
│   ├── config_otimizacao.R
│   ├── multi-utils.R
│   └── tratamentoDeDados.R
└── .gitignore             # Ficheiros ignorados pelo Git (RData, logs temporários, etc.)
```

## 🚀 Como utilizar

1.  **Dados**: Os ficheiros CSV originais estão agora na pasta `data/`. Se algum script der erro ao ler os ficheiros, verifique o caminho (path) no código.
2.  **Scripts**: Todos os scripts R foram movidos para as pastas correspondentes ao seu propósito (EDA, univariado, multivariado ou otimização).
3.  **Configurações**: O ficheiro `config_otimizacao.R` e outras funções úteis estão na pasta `utils/`.

## 🧹 Limpeza efetuada

- Removidos ficheiros duplicados (scripts e PDFs repetidos).
- Removidos ficheiros temporários do R (`.RData`, `.Rhistory`) da raiz.
- Escondido o token do GitHub que estava exposto.
- Criado um `.gitignore` robusto para manter o repositório limpo.

---
*Organizado automaticamente pelo Antigravity.*
