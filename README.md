
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `cameradeputadosapi`: A R wrapper for the Brazilian House of Representatives open data API

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/pedrodrocha/camaradeputadosapi/workflows/R-CMD-check/badge.svg)](https://github.com/pedrodrocha/camaradeputadosapi/actions)
<!-- badges: end -->

The goal of `camaradeputadosapi` is to ease access and provide a clean
interface in R for querying the Brazilian House of Representatives open
data [RESTful API](https://dadosabertos.camara.leg.br/swagger/api.html).

The package is under active development, but some functionalities are
already on. Note that in this state of the project there isn’t proper
documentation and breaking changes are expected. In any case, user
feedback is welcome and valued.

## Instalation

You can install the latest development version from Github with:

``` r
install.packages("devtools")
devtools::install_github("pedrodrocha/camaradeputadosapi")
```

## Usage

In its final stage the package is expected to cover nine families of
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
  - `votacoes()` & `votacoes_*()`: For querying data on voting processes
    (**NOT AVAILABLE YET**).
  - `orgaos()` & `orgaos_*()`: For querying data on commissions and
    other legislative bodies (**NOT AVAILABLE YET**).  
  - `referencias_*()`: For querying data on available variables and its
    values (**NOT AVAILABLE YET**).

**OBS:** By default the API returns only 15 results per request and the
limit is 100 results. When appropriate make sure to add `itens= N` as a
parameter for greater control.
