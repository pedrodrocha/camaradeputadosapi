
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
#> # A tibble: 513 x 7
#>        id nome    siglaPartido siglaUf idLegislatura urlFoto           email    
#>     <int> <chr>   <chr>        <chr>           <int> <chr>             <chr>    
#>  1 204554 Abílio~ PL           BA                 56 https://www.cama~ dep.abil~
#>  2 204521 Abou A~ PSL          SP                 56 https://www.cama~ dep.abou~
#>  3 204379 Acácio~ PROS         AP                 56 https://www.cama~ dep.acac~
#>  4 204560 Adolfo~ PSDB         BA                 56 https://www.cama~ dep.adol~
#>  5 204528 Adrian~ NOVO         SP                 56 https://www.cama~ dep.adri~
#>  6 121948 Adrian~ PP           GO                 56 https://www.cama~ dep.adri~
#>  7  74646 Aécio ~ PSDB         MG                 56 https://www.cama~ dep.aeci~
#>  8 160508 Afonso~ PT           BA                 56 https://www.cama~ dep.afon~
#>  9 136811 Afonso~ PP           RS                 56 https://www.cama~ dep.afon~
#> 10 178835 Afonso~ PDT          RS                 56 https://www.cama~ dep.afon~
#> # ... with 503 more rows
```

To have more control over the results you can use further parameters
from the API, such as ‘idLegislatura’ which matches a legislative period
by its Id and return all representatives that were part of that specific
legislative period. To access legislative periods ids you can use
`legislaturas_id()`

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
#> # A tibble: 15 x 17
#>      ano   mes tipoDespesa codDocumento tipoDocumento codTipoDocumento
#>    <int> <int> <chr>              <int> <chr>                    <int>
#>  1  2020     1 MANUTENÇÃO~      7015132 Nota Fiscal                  0
#>  2  2020     2 MANUTENÇÃO~      7031074 Nota Fiscal                  0
#>  3  2020     4 MANUTENÇÃO~      7051884 Nota Fiscal                  0
#>  4  2020     5 MANUTENÇÃO~      7062129 Nota Fiscal                  0
#>  5  2020     6 MANUTENÇÃO~      7070567 Nota Fiscal                  0
#>  6  2020     7 MANUTENÇÃO~      7079007 Nota Fiscal                  0
#>  7  2020     8 MANUTENÇÃO~      7092074 Nota Fiscal                  0
#>  8  2020    11 MANUTENÇÃO~      7126253 Nota Fiscal                  0
#>  9  2020     9 MANUTENÇÃO~      7105458 Nota Fiscal                  0
#> 10  2020    10 MANUTENÇÃO~      7116674 Nota Fiscal                  0
#> 11  2020     3 MANUTENÇÃO~      7030519 Nota Fiscal ~                4
#> 12  2020    11 COMBUSTÍVE~      7119592 Nota Fiscal ~                4
#> 13  2020     1 COMBUSTÍVE~      6997029 Nota Fiscal ~                4
#> 14  2020     2 COMBUSTÍVE~      7010048 Nota Fiscal ~                4
#> 15  2020     2 COMBUSTÍVE~      7012760 Nota Fiscal ~                4
#> # ... with 11 more variables: dataDocumento <chr>, numDocumento <chr>,
#> #   valorDocumento <dbl>, urlDocumento <chr>, nomeFornecedor <chr>,
#> #   cnpjCpfFornecedor <chr>, valorLiquido <dbl>, valorGlosa <dbl>,
#> #   numRessarcimento <chr>, codLote <int>, parcela <int>
```

Reference and metadata for this category can be accessed with
`deputados_referencias()`.

``` r
deputados_referencias()
#> $siglaUF
#> # A tibble: 27 x 4
#>    cod   sigla nome             descricao
#>    <chr> <chr> <chr>            <chr>    
#>  1 6     AC    ACRE             ""       
#>  2 14    AL    ALAGOAS          ""       
#>  3 4     AM    AMAZONAS         ""       
#>  4 2     AP    AMAPÁ            ""       
#>  5 16    BA    BAHIA            ""       
#>  6 9     CE    CEARÁ            ""       
#>  7 22    DF    DISTRITO FEDERAL ""       
#>  8 18    ES    ESPÍRITO SANTO   ""       
#>  9 23    GO    GOIÁS            ""       
#> 10 8     MA    MARANHÃO         ""       
#> # ... with 17 more rows
#> 
#> $siglaSituacao
#> # A tibble: 8 x 4
#>   cod   sigla nome           descricao
#>   <chr> <chr> <chr>          <chr>    
#> 1 ""    A     Afastado       ""       
#> 2 ""    C     Convocado      ""       
#> 3 ""    E     Exercício      ""       
#> 4 ""    F     Fim de Mandato ""       
#> 5 ""    L     Licença        ""       
#> 6 ""    S     Suplência      ""       
#> 7 ""    U     Suspenso       ""       
#> 8 ""    V     Vacância       ""
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
#> # A tibble: 6 x 10
#>      id dataHoraInicio dataHoraFim situacao descricaoTipo descricao localExterno
#>   <int> <chr>          <chr>       <chr>    <chr>         <chr>     <chr>       
#> 1 60070 2020-12-01T09~ 2020-12-01~ Encerra~ Evento Técni~ "Preserv~ <NA>        
#> 2 60121 2020-12-01T09~ 2020-12-01~ Encerra~ Visita Técni~ "Audiênc~ Porto Alegr~
#> 3 60124 2020-12-01T09~ <NA>        Cancela~ Visita Técni~ "1º dia ~ Venezuela   
#> 4 60133 2020-12-01T14~ 2020-12-01~ Encerra~ Evento Técni~ "Preserv~ <NA>        
#> 5 60134 2020-12-01T11~ 2020-12-01~ Encerra~ Visita Técni~ "Visita ~ Sala Virtua~
#> 6 60141 2020-12-01T11~ 2020-12-01~ Encerra~ Debate        "Evento ~ <NA>        
#> # ... with 6 more variables: orgaos <list>, localCamara$nome <chr>,
#> #   $predio <lgl>, $sala <lgl>, $andar <lgl>, urlRegistro <chr>
```

There are five more functions on this category for querying data on a
specific event: `eventos_deputados()`, `eventos_orgaos()`,
`eventos_pauta()`, `eventos_votacoes()` and `eventos_info()`. Each one
receives an event unique identifier.

``` r
# Get a list of representatives that attended a specific event
eventos_deputados(id = 60027)
#> # A tibble: 403 x 7
#>       id nome     siglaPartido siglaUf idLegislatura urlFoto          email     
#>    <int> <chr>    <chr>        <chr>           <int> <chr>            <chr>     
#>  1 66179 Norma A~ DEM          ES                 56 https://www.cam~ dep.norma~
#>  2 66828 Fausto ~ PP           SP                 56 https://www.cam~ dep.faust~
#>  3 67138 Iracema~ PP           PI                 56 https://www.cam~ dep.irace~
#>  4 68720 Fábio H~ PDT          SE                 56 https://www.cam~ dep.fabio~
#>  5 69871 Bacelar  PODE         BA                 56 https://www.cam~ dep.bacel~
#>  6 73441 Celso R~ REPUBLICANOS SP                 56 https://www.cam~ dep.celso~
#>  7 73460 Gustavo~ PDT          PR                 56 https://www.cam~ dep.gusta~
#>  8 73466 Rubens ~ CIDADANIA    PR                 56 https://www.cam~ dep.ruben~
#>  9 73531 Ivan Va~ PSOL         SP                 56 https://www.cam~ dep.ivanv~
#> 10 73586 Júlio D~ PSB          MG                 56 https://www.cam~ dep.julio~
#> # ... with 393 more rows
```

Reference and metadata for this category can be accessed with
`eventos_referencias()`.

``` r
eventos_referencias()
#> $codTipoEvento
#> # A tibble: 36 x 4
#>    cod   sigla nome                               descricao
#>    <chr> <chr> <chr>                              <chr>    
#>  1 110   ""    Sessão Deliberativa                ""       
#>  2 112   ""    Reunião Deliberativa               ""       
#>  3 115   ""    Sessão Não Deliberativa Solene     ""       
#>  4 118   ""    Trabalho de Comissões              ""       
#>  5 120   ""    Audiência Pública                  ""       
#>  6 122   ""    Reunião para Tomada de Depoimento  ""       
#>  7 125   ""    Audiência Pública e Deliberação    ""       
#>  8 130   ""    Seminário                          ""       
#>  9 140   ""    Mesa Redonda                       ""       
#> 10 150   ""    Sessão Não Deliberativa de Debates ""       
#> # ... with 26 more rows
#> 
#> $codSituacao
#> # A tibble: 9 x 4
#>   cod   sigla nome                        descricao
#>   <chr> <chr> <chr>                       <chr>    
#> 1 1     ""    "Não Confirmada           " ""       
#> 2 2     ""    "Convocada                " ""       
#> 3 3     ""    "Em Andamento             " ""       
#> 4 4     ""    "Encerrada                " ""       
#> 5 5     ""    "Cancelada                " ""       
#> 6 6     ""    "Suspensa                 " ""       
#> 7 7     ""    "Encerrada (Termo)        " ""       
#> 8 8     ""    "Encerrada (Final)        " ""       
#> 9 9     ""    "Encerrada(Comunicado)    " ""
```

#### ‘frentes’

Use functions on this category for querying data on parliamentary fronts
created after 2003 at the Brazilian House of representatives.

The basic function for this category is `eventos()` and it can be used
to query data on parliamentary fronts active in on or more legislative
periods. For getting the id from a legislative period you can run
`legislaturas_id()`.

``` r
# Get id for active legislative period in '2005-01-01'
id <- legislaturas_id(date = '2005-01-01')
frentes(idLegislatura = id)
#> # A tibble: 113 x 3
#>       id titulo                                                    idLegislatura
#>    <int> <chr>                                                             <int>
#>  1 52171 FRENTE PLURISSETORIAL EM DEFESA DAS FORÇAS ARMADAS                   52
#>  2 52162 DEFESA DA CAPOEIRA                                                   52
#>  3 52022 APOIO À CULTURA POPULAR BRASILEIRA                                   52
#>  4 52169 DEFESA DOS DIREITOS DA PESSOA COM DEFICIÊNCIA                        52
#>  5 52159 FRUTICULTURA                                                         52
#>  6 52166 PRÓ-BIOCOMBUSTÍVEIS                                                  52
#>  7 52164 AVICULTURA                                                           52
#>  8 52157 DIREITOS DOS ANISTIADOS POLÍTICOS                                    52
#>  9 52154 DEFESA DA ENERGIA DE FONTES RENOVÁVEIS                               52
#> 10 52153 DEFESA DA REPRESENTAÇÃO PROPORCIONAL DOS MUNICÍPIOS BRAS~            52
#> # ... with 103 more rows
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
#> # A tibble: 187 x 11
#>       id nome  siglaPartido siglaUf idLegislatura urlFoto email titulo codTitulo
#>    <int> <chr> <chr>        <chr>           <int> <chr>   <lgl> <chr>      <int>
#>  1 81055 Márc~ PCdoB        MA                 56 https:~ NA    Coord~        14
#>  2 68720 Fábi~ PDT          SE                 56 https:~ NA    Membro      1004
#>  3 73433 Arli~ PT           SP                 56 https:~ NA    Membro      1004
#>  4 73460 Gust~ PDT          PR                 56 https:~ NA    Membro      1004
#>  5 73466 Rube~ CIDADANIA    PR                 56 https:~ NA    Membro      1004
#>  6 73482 Henr~ PT           RS                 56 https:~ NA    Membro      1004
#>  7 73486 Pomp~ PDT          RS                 56 https:~ NA    Membro      1004
#>  8 73531 Ivan~ PSOL         SP                 56 https:~ NA    Membro      1004
#>  9 73586 Júli~ PSB          MG                 56 https:~ NA    Membro      1004
#> 10 73604 Rui ~ PT           SP                 56 https:~ NA    Membro      1004
#> # ... with 177 more rows, and 2 more variables: dataInicio <lgl>, dataFim <lgl>
```

#### ‘legislaturas’

Use functions on this category for querying data on legislative periods
for the Brazilian House of representatives.

The basic function for this category is `legislaturas()` which retunrs
by default a complete list of legislative periods.

``` r
legislaturas()
#> # A tibble: 56 x 3
#>       id dataInicio dataFim   
#>    <int> <chr>      <chr>     
#>  1    56 2019-02-01 2023-01-31
#>  2    55 2015-02-01 2019-01-31
#>  3    54 2011-02-01 2015-01-31
#>  4    53 2007-02-01 2011-01-31
#>  5    52 2003-02-01 2007-01-31
#>  6    51 1999-02-01 2003-01-31
#>  7    50 1995-02-01 1999-01-31
#>  8    49 1991-02-01 1995-01-31
#>  9    48 1987-02-01 1991-01-31
#> 10    47 1983-02-01 1987-01-31
#> # ... with 46 more rows
```

This category has one more function for querying data on speakers of the
House for a particular legislative period.

``` r
# Get id for active legislative period in '2020-01-01'
id <- legislaturas_id(date = '2020-01-01')
# Get info on speakers of the House.
legislaturas_mesa(id = id)
#> # A tibble: 13 x 11
#>        id nome  siglaPartido siglaUf idLegislatura urlFoto email dataInicio
#>     <int> <chr> <chr>        <chr>           <int> <chr>   <lgl> <chr>     
#>  1  74693 "Rod~ DEM          RJ                 56 https:~ NA    2019-02-01
#>  2  74158 "Már~ PDT          MG                 56 https:~ NA    2019-02-01
#>  3 178951 "Raf~ PSB          RN                 56 https:~ NA    2019-02-01
#>  4 141428 "Fáb~ PSD          RN                 56 https:~ NA    2019-02-01
#>  5 178953 "Exp~ PSD          RO                 56 https:~ NA    2020-07-08
#>  6 178966 "Geo~ PSDB         SC                 56 https:~ NA    2019-02-01
#>  7  74478 "Luc~ PSL          PE                 56 https:~ NA    2019-02-01
#>  8 171617 "Pau~ PT           AL                 56 https:~ NA    2020-07-08
#>  9 159237 "Ass~ PT           PI                 56 https:~ NA    2019-02-01
#> 10 204436 "Isn~ MDB          AL                 56 https:~ NA    2019-02-01
#> 11 178882 "And~ PP           MA                 56 https:~ NA    2019-02-01
#> 12 178946 "Sor~ PR           RJ                 56 https:~ NA    2019-02-01
#> 13 204506 "Mar~ PRB          SP                 56 https:~ NA    2019-02-01
#> # ... with 3 more variables: dataFim <chr>, titulo <chr>, codTitulo <chr>
```

#### ‘partidos’

Use functions on this category for querying data on Brazilian political
parties.

The basic function for this category is `partidos()` which by default
return representatives in office for the currently legislative period.

``` r
partidos()
#> # A tibble: 40 x 3
#>       id sigla     nome                                 
#>    <int> <chr>     <chr>                                
#>  1 36898 AVANTE    Avante                               
#>  2 37905 CIDADANIA Cidadania                            
#>  3 37902 DC        Democracia Cristã                    
#>  4 36769 DEM       Democratas                           
#>  5 36899 MDB       Movimento Democrático Brasileiro     
#>  6 37901 NOVO      Partido Novo                         
#>  7 37900 PATRI     Patriota                             
#>  8 37907 PATRIOTA  Patriota                             
#>  9 36863 PCB       Partido Constitucionalista Brasileiro
#> 10 36779 PCdoB     Partido Comunista do Brasil          
#> # ... with 30 more rows
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
#> # A tibble: 1 x 16
#>      id sigla nome  numeroEleitoral urlLogo urlWebSite urlFacebook data 
#>   <int> <chr> <chr> <lgl>           <chr>   <lgl>      <lgl>       <chr>
#> 1 36844 PT    Part~ NA              http:/~ NA         NA          2020~
#> # ... with 8 more variables: idLegislatura <chr>, situacao <chr>,
#> #   totalPosse <chr>, totalMembros <chr>, lider_nome <chr>, lider_uf <chr>,
#> #   lider_idLegislatura <int>, lider_urlFoto <chr>
```

#### ‘proposicoes’

Use functions on this category for querying data on propositions
presented at the Brazilian House of Representatives.

The basic function for this category is `proposicoes()` which by default
return a list of propositions active 30 days prior to the query.

``` r
proposicoes()
#> # A tibble: 100 x 6
#>       id siglaTipo codTipo numero   ano ementa                                  
#>    <int> <chr>       <int>  <int> <int> <chr>                                   
#>  1 15990 PL            139    887  1991 "Cria salvaguardas para a tecnologia no~
#>  2 18963 PL            139   3011  2000 "Altera o art. 315 do Decreto-Lei nº 2.~
#>  3 19034 PL            139   3054  2000 "Dispõe sobre a revogação da Lei de Seg~
#>  4 19175 PL            139   3147  2000 "Dispõe sobre a reserva de vagas para t~
#>  5 19217 PL            139   3174  1997 "Altera a Lei nº 5.700, de 1º de setemb~
#>  6 19499 PL            139   3340  2000 "Determina que a criação de novos curso~
#>  7 20411 PL            139   3894  2000 "Disciplina a publicidade e propaganda ~
#>  8 20529 PL            139   3968  1997 "Isenta os órgãos públicos e as entidad~
#>  9 20552 PL            139   3980  2000 "Dispõe sobre a proibição da expressão ~
#> 10 21640 PRC           444     13  1995 "Dá nova redação aos arts. 46 e 65 do R~
#> # ... with 90 more rows
```

There are six more functions on this cateogory for querying data on a
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
#> # A tibble: 61 x 11
#>    dataHora sequencia siglaOrgao regime descricaoTramit~ codTipoTramitac~
#>    <chr>        <int> <chr>      <chr>  <chr>            <chr>           
#>  1 2020-04~         1 PLEN       Urgên~ Apresentação de~ 100             
#>  2 2020-04~         2 SEPRO      Urgên~ Notificação de ~ 910             
#>  3 2020-04~         3 CEDI       Urgên~ Notificação (Ce~ 1004            
#>  4 2020-04~         4 SEPRO      Urgên~ Encaminhamento   180             
#>  5 2020-04~         5 SECAP(SGM) Urgên~ Recebimento      500             
#>  6 2020-04~         7 PLEN       Urgên~ Apresentação de~ 194             
#>  7 2020-04~         6 PLEN       Urgên~ Apresentação de~ 194             
#>  8 2020-04~         8 MESA       Urgên~ Distribuição     110             
#>  9 2020-07~         9 PLEN       Urgên~ Apresentação de~ 194             
#> 10 2020-07~        10 PLEN       Urgên~ Apresentação de~ 194             
#> # ... with 51 more rows, and 5 more variables: descricaoSituacao <chr>,
#> #   codSituacao <int>, despacho <chr>, url <chr>, ambito <chr>
```

Reference and metadata for this category can be accessed with
`proposicoes_referencias()`.

``` r
proposicoes_referencias()
#> $siglaTipo
#> # A tibble: 472 x 4
#>    cod   sigla nome                   descricao                                 
#>    <chr> <chr> <chr>                  <chr>                                     
#>  1 129   CON   Consulta               Consulta                                  
#>  2 130   EMC   Emenda na Comissão     Emenda Apresentada na Comissão            
#>  3 131   EMP   Emenda de Plenário     Emenda de Plenário                        
#>  4 132   EMS   Emenda/Substitutivo d~ Emenda/Substitutivo do Senado             
#>  5 133   INC   Indicação              Indicação                                 
#>  6 134   MSC   Mensagem               Mensagem                                  
#>  7 135   PDC   Projeto de Decreto Le~ Projeto de Decreto Legislativo            
#>  8 136   PEC   Proposta de Emenda à ~ Proposta de Emenda à Constituição (Art. 6~
#>  9 137   PET   Petição                Petição                                   
#> 10 138   PFC   Proposta de Fiscaliza~ Proposta de Fiscalização e Controle       
#> # ... with 462 more rows
#> 
#> $codSituacao
#> # A tibble: 90 x 4
#>    cod   sigla nome                                                    descricao
#>    <chr> <chr> <chr>                                                   <chr>    
#>  1 900   ""    "Aguardando Autógrafos na Mesa"                         ""       
#>  2 901   ""    "Aguardando Constituição de Comissão Temporária "       ""       
#>  3 902   ""    "Aguardando Criação de Comissão Temporária"             ""       
#>  4 903   ""    "Aguardando Deliberação"                                ""       
#>  5 904   ""    "Aguardando Deliberação de Recurso"                     ""       
#>  6 905   ""    "Aguardando Despacho do Presidente da Câmara dos Deput~ ""       
#>  7 906   ""    "Aguardando Distribuição"                               ""       
#>  8 907   ""    "Aguardando Designação de Relator"                      ""       
#>  9 910   ""    "Aguardando Encaminhamento"                             ""       
#> 10 911   ""    "Aguardando Instalação de Comissão Temporária"          ""       
#> # ... with 80 more rows
#> 
#> $tiposTramitacao
#> # A tibble: 221 x 4
#>    cod   sigla nome                         descricao
#>    <chr> <chr> <chr>                        <chr>    
#>  1 5     ""    "Não Informado"              ""       
#>  2 100   ""    "Apresentação de Proposição" ""       
#>  3 104   ""    "Desapensação"               ""       
#>  4 105   ""    "Leitura e publicação"       ""       
#>  5 106   ""    "Apensação"                  ""       
#>  6 107   ""    "Não Apensação"              ""       
#>  7 110   ""    "Distribuição "              ""       
#>  8 112   ""    "Redistribuição "            ""       
#>  9 113   ""    "Envio para a redação Final" ""       
#> 10 115   ""    "Declarada insubsistência"   ""       
#> # ... with 211 more rows
#> 
#> $codTema
#> # A tibble: 32 x 4
#>    cod   sigla nome                             descricao
#>    <chr> <chr> <chr>                            <chr>    
#>  1 34    ""    Administração Pública            ""       
#>  2 35    ""    Arte, Cultura e Religião         ""       
#>  3 37    ""    Comunicações                     ""       
#>  4 39    ""    Esporte e Lazer                  ""       
#>  5 40    ""    Economia                         ""       
#>  6 41    ""    Cidades e Desenvolvimento Urbano ""       
#>  7 42    ""    Direito Civil e Processual Civil ""       
#>  8 43    ""    Direito Penal e Processual Penal ""       
#>  9 44    ""    Direitos Humanos e Minorias      ""       
#> 10 46    ""    Educação                         ""       
#> # ... with 22 more rows
#> 
#> $codTipoAutor
#> # A tibble: 45 x 4
#>    cod   sigla nome                              descricao
#>    <chr> <chr> <chr>                             <chr>    
#>  1 1     ""    COMISSÃO DIRETORA                 ""       
#>  2 2     ""    COMISSÃO PERMANENTE               ""       
#>  3 3     ""    COMISSÃO ESPECIAL                 ""       
#>  4 4     ""    COMISSÃO PARLAMENTAR DE INQUÉRITO ""       
#>  5 5     ""    COMISSÃO EXTERNA                  ""       
#>  6 6     ""    COMISSÃO MISTA PERMANENTE         ""       
#>  7 7     ""    COMISSÃO DE SINDICÂNCIA           ""       
#>  8 8     ""    COMISSÃO REPRESENTATIVA           ""       
#>  9 9     ""    COMISSÃO MEDIDA PROVISÓRIA        ""       
#> 10 10    ""    GRUPO DE TRABALHO                 ""       
#> # ... with 35 more rows
```

#### ‘votacoes’

Use functions on this category for querying data on voting processes at
several bodies and commissions from the Brazilian House of
Representatives.

The basic function for this category is `votacoes()` which returns by
default a list of voting procedures from the last 30 days.

``` r
votacoes()
#> # A tibble: 200 x 7
#>    id     data  dataHoraRegistro siglaOrgao proposicaoObjeto descricao aprovacao
#>    <chr>  <chr> <chr>            <chr>      <chr>            <chr>         <int>
#>  1 22656~ 2020~ 2020-12-22T23:3~ PLEN       <NA>             "Rejeita~         0
#>  2 21411~ 2020~ 2020-12-22T21:5~ PLEN       <NA>             "Aprovad~         1
#>  3 21411~ 2020~ 2020-12-22T21:5~ PLEN       <NA>             "Aprovad~         1
#>  4 22656~ 2020~ 2020-12-22T21:1~ MESA       <NA>             "Deferid~         1
#>  5 22676~ 2020~ 2020-12-22T20:1~ PLEN       <NA>             "Aprovad~         1
#>  6 22676~ 2020~ 2020-12-22T20:1~ PLEN       <NA>             "Alteraç~         1
#>  7 22676~ 2020~ 2020-12-22T20:1~ PLEN       REQ 2983/2020    "Aprovad~         1
#>  8 22655~ 2020~ 2020-12-22T19:5~ PLEN       <NA>             "Aprovad~         1
#>  9 22534~ 2020~ 2020-12-22T19:4~ PLEN       <NA>             "Aprovad~         1
#> 10 22534~ 2020~ 2020-12-22T19:4~ PLEN       <NA>             "Aprovad~         1
#> # ... with 190 more rows
```

There are three more functions on this category for querying data on a
specific voting processes:: `votacoes_info()`, `votacoes_orientacoes()`,
`votacoes_votos()`. Each one receives a voting unique identifier.

``` r
# Voting records at voting process "2265603-43" 
votacoes_votos(id = "2265603-43")
#> # A tibble: 268 x 9
#>    tipoVoto dataRegistroVoto    id nome  siglaPartido siglaUf idLegislatura
#>    <chr>    <chr>            <int> <chr> <chr>        <chr>           <int>
#>  1 Não      2020-12-22T23:3~ 66179 Norm~ DEM          ES                 56
#>  2 Não      2020-12-22T23:3~ 66828 Faus~ PP           SP                 56
#>  3 Não      2020-12-22T23:3~ 68720 Fábi~ PDT          SE                 56
#>  4 Não      2020-12-22T23:3~ 69871 Bace~ PODE         BA                 56
#>  5 Não      2020-12-22T23:3~ 72442 Feli~ PSB          PE                 56
#>  6 Não      2020-12-22T23:3~ 73433 Arli~ PT           SP                 56
#>  7 Não      2020-12-22T23:3~ 73466 Rube~ CIDADANIA    PR                 56
#>  8 Não      2020-12-22T23:3~ 73486 Pomp~ PDT          RS                 56
#>  9 Não      2020-12-22T23:3~ 73531 Ivan~ PSOL         SP                 56
#> 10 Não      2020-12-22T23:3~ 73586 Júli~ PSB          MG                 56
#> # ... with 258 more rows, and 2 more variables: urlFoto <chr>, email <chr>
```

#### ‘orgaos’

Use functions on this category for querying data on bodies and
commissions from the Brazilian House of Representatives.

The basic function for this category is `orgaos()` which returns a list
of bodies and commissions.

``` r
orgaos()
#> # A tibble: 1,000 x 7
#>       id sigla  nome         apelido    codTipoOrgao tipoOrgao   nomePublicacao 
#>    <int> <chr>  <chr>        <chr>             <int> <chr>       <chr>          
#>  1     4 MESA   Mesa Direto~ Mesa Dire~            1 Comissão D~ Mesa Diretora  
#>  2    51 Judic~ Judiciário   Judiciário        50000 Órgão do P~ Judiciário     
#>  3    57 MPU    MINISTÉRIO ~ MINISTÉRI~        81007 MPU - Mini~ MINISTÉRIO PÚB~
#>  4    60 PR     Presidência~ PRESIDÊNC~        30000 Órgão do P~ PRESIDÊNCIA DA~
#>  5    78 SF     Senado Fede~ Senado Fe~        40000 Órgão do P~ Senado Federal 
#>  6    79 SFCMRC Senado Fede~ Senado Fe~        22000 Órgão do S~ Senado Federal~
#>  7    80 STF    Supremo Tri~ Supremo T~        50000 Órgão do P~ Supremo Tribun~
#>  8    81 STJ    Superior Tr~ Superior ~        50000 Órgão do P~ Superior Tribu~
#>  9    82 TCU    Tribunal de~ Tribunal ~        40000 Órgão do P~ Tribunal de Co~
#> 10    93 GSF    Gráfica - S~ Gráfica -~        22000 Órgão do S~ Gráfica - Sena~
#> # ... with 990 more rows
```

There are three more functions on this category for querying data on a
specific voting processes:: `orgaos_eventos()`, `orgaos_info()`,
`orgaos_membros()`, `orgaos_votacoes()`. Each one receives a voting
unique identifier.

``` r
# Get a body or commission unique identifier
id <- orgaos_id(abbr = 'PLEN')
# Get general info on a specific body or commission
orgaos_info(id = id)
#> # A tibble: 1 x 14
#>      id sigla nome  apelido codTipoOrgao tipoOrgao nomePublicacao dataInicio
#>   <int> <chr> <chr> <chr>          <int> <chr>     <chr>          <chr>     
#> 1   180 PLEN  Plen~ Plenár~           26 Plenário~ Plenário       2020-03-2~
#> # ... with 6 more variables: dataInstalacao <chr>, dataFim <lgl>,
#> #   dataFimOriginal <lgl>, casa <chr>, sala <chr>, urlWebsite <lgl>
```

Reference and metadata for this category can be accessed with
`orgaos_referencias()`

``` r
orgaos_referencias()
#> $idTipoOrgao
#> # A tibble: 44 x 4
#>    cod   sigla nome                              descricao
#>    <chr> <chr> <chr>                             <chr>    
#>  1 1     ""    Comissão Diretora                 ""       
#>  2 2     ""    Comissão Permanente               ""       
#>  3 3     ""    Comissão Especial                 ""       
#>  4 4     ""    Comissão Parlamentar de Inquérito ""       
#>  5 5     ""    Comissão Externa                  ""       
#>  6 6     ""    Comissão Mista Permanente         ""       
#>  7 7     ""    Comissão de Sindicância           ""       
#>  8 8     ""    Comissão Representativa do CN     ""       
#>  9 9     ""    Comissão Medida Provisória        ""       
#> 10 10    ""    Grupo de Trabalho                 ""       
#> # ... with 34 more rows
#> 
#> $idSituacao
#> # A tibble: 20 x 4
#>    cod   sigla nome                                            descricao
#>    <chr> <chr> <chr>                                           <chr>    
#>  1 2     ""    Pronta para criação                             ""       
#>  2 3     ""    Aguardando indicações dos Líderes               ""       
#>  3 4     ""    Completa aguardando constituição                ""       
#>  4 5     ""    Aguardando definição de Presidente e/ou relator ""       
#>  5 6     ""    Pronta para instalação                          ""       
#>  6 7     ""    Em funcionamento                                ""       
#>  7 8     ""    Com parecer aprovado                            ""       
#>  8 9     ""    Em tramitação no Senado Federal                 ""       
#>  9 10    ""    Extinta                                         ""       
#> 10 15    ""    Aguardando admissibilidade na CCJC              ""       
#> 11 16    ""    Aguardando análise de apensação                 ""       
#> 12 17    ""    Aguardando decisão do Presidente                ""       
#> 13 18    ""    Aguardando despacho do Presidente               ""       
#> 14 19    ""    Apensada                                        ""       
#> 15 20    ""    Devolvida ao Autor                              ""       
#> 16 21    ""    Em tramitação nas Comissões                     ""       
#> 17 22    ""    Não criada                                      ""       
#> 18 23    ""    PRC pronto para a pauta                         ""       
#> 19 24    ""    Requerimento pronto para pauta                  ""       
#> 20 25    ""    Criada                                          ""
```

## Further reference

Check the Brazilian House of Representatives [RESTful
API](https://dadosabertos.camara.leg.br/swagger/api.html) official
documentation for further reference.

## Changelog

Click
[here](https://github.com/pedrodrocha/camaradeputadosapi/blob/master/NEWS.md)
for accessing **camaradeputadosapi** changelog.
