% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proposicoes.R
\name{proposicoes}
\alias{proposicoes}
\title{Get a list of propositions discussed in between a time interval}
\usage{
proposicoes(..., from, to)
}
\arguments{
\item{...}{query parameters for the House of Representatives API (See: https://dadosabertos.camara.leg.br/swagger/api.html)}

\item{from}{start date (YYYY-MM-DD) for the time interval in which a proposition was discussed}

\item{to}{end date (YYYY-MM) for the time interval in which a proposition was discussed}
}
\value{
A tibble of propositions discussed in between a time interval
}
\description{
Get a list of propositions discussed in between a time interval
}
\examples{
proposicoes(from = "2020-01-01", to = "2020-12-01", itens = 100)
}
\seealso{
Other proposicoes: 
\code{\link{proposicoes_autores}()},
\code{\link{proposicoes_historico}()},
\code{\link{proposicoes_info}()},
\code{\link{proposicoes_relacionadas}()},
\code{\link{proposicoes_temas}()},
\code{\link{proposicoes_votacoes}()}
}
\concept{proposicoes}