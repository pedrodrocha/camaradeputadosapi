
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `cameradeputadosapi`: A R wrapper for the Brazilian House of Representatives open data API

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/pedrodrocha/camaradeputadosapi/workflows/R-CMD-check/badge.svg)](https://github.com/pedrodrocha/camaradeputadosapi/actions)
<!-- badges: end -->

**camaradeputadosapi** provides a clean interface in R for querying the
Brazilian House of Representatives open data [RESTful
API](https://dadosabertos.camara.leg.br/swagger/api.html).

## Instalation

You can install the latest development version from Github with:

``` r
install.packages("devtools")
devtools::install_github("pedrodrocha/camaradeputadosapi")
```

## Usage

**camaradeputadosapi** functions are divided in eight categories, one
for each data type provided by the Brazilian House of Representatives
API:

#### ‘deputados’

Use functions on this category for querying data on representatives that
are or were in office in any moment of time at the Brazilian House of
Representatives.

The basic function for this category is `deputados()` which by default
returns all representatives that are currently in office.

``` r
# Return all representatives that are currently in office at the Brazilian House of Representatives API
deputados()
```

To have more control over the results you can use further parameters
from the API, such as ‘idLegislatura’ which matches a legislative period
by its Id and return all representatives that were part of that specific
legislative period. To access legislative periods ids you can use
`legislaturas_id()`.

``` r
# Get the id for a legislative period active in '1900-01-01'
leg <- legislaturas_id(date = "1900-01-01")
# Get a list of representatives for that legislative period
rep <- deputados(idLegislatura = leg)
```

There are six more functions on this category for querying data on a
specific representative: `deputados_despesas()`,`deputados_discursos()`,
`deputados_eventos()`, `deputados_frentes()`, `deputados_info()` and
`deputados_orgaos()`. Each one receives a representative unique
identifier that can be obtained running `deputados_id()`.

``` r
# Get Rodrigo Maia's id
id <- deputados_id(name = "Rodrigo Maia")
# Get his expenditures in 2020
deputados_despesas(id = id, year = 2020)
```

Reference and metadata for this category can be accessed with
`deputados_referencias()`.

``` r
deputados_referencias()
```

#### ‘eventos’

Use functions on this category for querying data on events that occurred
or are on schedule at the Brazilian House of representatives.

The basic function for this category is `eventos()` and by default it
returns events from 5 days before and after the query, but you can use
further parameters from the official API for controlling the time
interval.

``` r
# Get events ocurred in '2020-12-01'
eventos(dataInicio = "2020-12-01", dataFim = "2020-12-01")
```

There are five more functions on this category for querying data on a
specific event: `eventos_deputados()`, `eventos_orgaos()`,
`eventos_pauta()`, `eventos_votacoes()` and `eventos_info()`. Each one
receives an event unique identifier.

``` r
# Get a list of representatives that attended a specific event
eventos_deputados(id = 60027)
```

Reference and metadata for this category can be accessed with
`eventos_referencias()`.

``` r
eventos_referencias()
```

#### ‘frentes’

Use functions on this category for querying data on parliamentary fronts
created after 2003 at the Brazilian House of Representatives.

The basic function for this category is `eventos()` and it can be used
to query data on parliamentary fronts active in on or more legislative
periods. For getting the id from a legislative period you can run
`legislaturas_id()`.

``` r
# Get id for active legislative period in '2005-01-01'
id <- legislaturas_id(date = '2005-01-01')
frentes(idLegislatura = id)
```

There are two more functions on this category for querying data on a
specific parliamentary front: `frentes_info()`and `frentes_membros()`.
Each receives a parliamentary front unique identifier that can be
obtained running `frentes_id()`.

``` r
# Get a parliamentary front id
id <- frentes_id(nome = "fortalecimento do sus", idLegislatura = 56)
# Ger information on members of a specific parliamentary front
frentes_membros(id = id)
```

#### ‘legislaturas’

Use functions on this category for querying data on legislative periods
for the Brazilian House of Representatives.

The basic function for this category is `legislaturas()` which retunrs
by default a complete list of legislative periods.

``` r
legislaturas()
```

This category has one more function for querying data on speakers of the
House for a particular legislative period.

``` r
# Get id for active legislative period in '2020-01-01'
id <- legislaturas_id(date = '2020-01-01')
# Get info on speakers of the House.
legislaturas_mesa(id = id)
```

#### ‘partidos’

Use functions on this category for querying data on Brazilian political
parties.

The basic function for this category is `partidos()` which by default
return representatives in office and its political parties in the
currently legislative period.

``` r
partidos()
```

There are four more functions on this category for querying data on a
specific political party: `partidos_blocos()`, `partidos_id()`,
`partidos_info()` and `partidos_membros()`. Each one receives a
political party unique identifier.

``` r
# Get a political party unique identifier
id <- partidos_id(abbr = 'PT')
# Get info on a political party
partidos_info(id = id)
```

#### ‘proposicoes’

Use functions on this category for querying data on propositions
presented at the Brazilian House of Representatives.

The basic function for this category is `proposicoes()` which by default
return a list of propositions active 30 days prior to the query.

``` r
proposicoes()
```

There are six more functions on this category for querying data on a
specific proposition: `proposicoes_autores()`,
`proposicoes_historico()`, `proposicoes_info()`,
`proposicoes_relacionadas()`, `proposicoes_temas()`,
`proposicoes_votacoes()`. Each one receives a proposition unique
identifier.

``` r
# Get a proposition id
id <- proposicoes_id(type = "PL",number = 1665, year = 2020)
# Get historical record for a proposition
proposicoes_historico(id)
```

Reference and metadata for this category can be accessed with
`proposicoes_referencias()`.

``` r
proposicoes_referencias()
```

#### ‘votacoes’

Use functions on this category for querying data on voting processes at
several bodies and commissions from the Brazilian House of
Representatives.

The basic function for this category is `votacoes()` which returns by
default a list of voting procedures from the last 30 days.

``` r
votacoes()
```

There are three more functions on this category for querying data on a
specific voting processes: `votacoes_info()`, `votacoes_orientacoes()`,
`votacoes_votos()`. Each one receives a voting unique identifier.

``` r
# Voting records at voting process "2265603-43" 
votacoes_votos(id = "2265603-43")
```

#### ‘orgaos’

Use functions on this category for querying data on bodies and
commissions from the Brazilian House of Representatives.

The basic function for this category is `orgaos()` which returns a list
of bodies and commissions.

``` r
orgaos()
```

There are three more functions on this category for querying data on a
specific voting processes: `orgaos_eventos()`, `orgaos_info()`,
`orgaos_membros()`, `orgaos_votacoes()`. Each one receives a voting
unique identifier.

``` r
# Get a body or commission unique identifier
id <- orgaos_id(abbr = 'PLEN')
# Get general info on a specific body or commission
orgaos_info(id = id)
```

Reference and metadata for this category can be accessed with
`orgaos_referencias()`

``` r
orgaos_referencias()
```

## Further reference

Check the Brazilian House of Representatives [RESTful
API](https://dadosabertos.camara.leg.br/swagger/api.html) official
documentation for further reference.

## Changelog

Click
[here](https://github.com/pedrodrocha/camaradeputadosapi/blob/master/NEWS.md)
for accessing **camaradeputadosapi** changelog.
