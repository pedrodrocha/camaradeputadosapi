
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `cameradeputadosapi`: A R wrapper for the Brazilian House of Representatives open data API

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/pedrodrocha/camaradeputadosapi/workflows/R-CMD-check/badge.svg)](https://github.com/pedrodrocha/camaradeputadosapi/actions)
<!-- badges: end -->

The goal of `camaradeputadosapi` is to ease access and provide a clean
interface in R for querying the Brazilian House of Representatives open
data [RESTful API](https://dadosabertos.camara.leg.br/swagger/api.html).

The package is under active development, but major functionalities are
already on. Note that in this state of the project there isn’t proper
documentation and breaking changes are expected to occur. In any case,
user feedback is welcome and valued.

## Instalation

You can install the latest development version from Github with:

``` r
install.packages("devtools")
devtools::install_github("pedrodrocha/camaradeputadosapi")
```

## Usage

In its final stage the package is expected to cover eight families of
functions for querying each one a type of data provided by the API:

  - `deputados()` & `deputados_*()`: For querying data on
    representatives that are or once were in office.  
  - `eventos()` & `eventos_*()`: For querying data on events that
    occurred or are scheduled.  
  - `frentes()` & `frentes_*()`: For querying data on parliamentary
    fronts.  
  - `legislaturas()` & `legislaturas_*()`: For querying data on
    legislative periods.  
  - `partidos()` & `partidos_*()`: For querying data on political
    parties.  
  - `proposicoes()` & `proposicoes_*()`: For querying data on
    propositions.  
  - `votacoes()` & `votacoes_*()`: For querying data on voting
    processes.
  - `orgaos()` & `orgaos_*()`: For querying data on commissions and
    other legislative bodies.  
  - `referencias_*()`: For querying data on available variables and its
    values (**NOT AVAILABLE YET**).

**OBS:** In general the API returns by default only 15 results per
request and the max limit is 100 results, but for querying voting data
the default value and the max limit is 200. When appropriate make sure
to add `itens= N` as a parameter for greater control.
