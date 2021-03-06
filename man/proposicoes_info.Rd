% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proposicoes.R
\name{proposicoes_info}
\alias{proposicoes_info}
\title{Get information about a proposition of Brazilian House of Representatives}
\usage{
proposicoes_info(id)
}
\arguments{
\item{id}{A proposition unique identifier}
}
\value{
A tibble with information about a proposition
}
\description{
Get detailed information about a proposition presented at the Brazilian House of representatives
}
\examples{
a <- proposicoes_info(id = 19175)
}
\seealso{
Other proposicoes: 
\code{\link{proposicoes_autores}()},
\code{\link{proposicoes_historico}()},
\code{\link{proposicoes_id}()},
\code{\link{proposicoes_referencias}()},
\code{\link{proposicoes_relacionadas}()},
\code{\link{proposicoes_temas}()},
\code{\link{proposicoes_votacoes}()},
\code{\link{proposicoes}()}
}
\concept{proposicoes}
