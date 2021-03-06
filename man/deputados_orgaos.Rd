% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deputados.R
\name{deputados_orgaos}
\alias{deputados_orgaos}
\title{Get a list of bodies and commissions a representative is member}
\usage{
deputados_orgaos(id, ...)
}
\arguments{
\item{id}{An unique identifier for a given representative}

\item{...}{Query parameters for the Brazilian House of Representatives API}
}
\value{
A tibble with bodies and commissions a representative is member
}
\description{
Get a list of bodies and commissions a representative is currently member or was a member in a given period o time.
If query parameters from the API such as 'dataInicio' or 'dataFim' are not used, it will return only bodies or commissions a representative is currently member.
For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
}
\examples{
a <- deputados_orgaos(160511)
}
\seealso{
Other deputados: 
\code{\link{deputados_despesas}()},
\code{\link{deputados_discursos}()},
\code{\link{deputados_eventos}()},
\code{\link{deputados_frentes}()},
\code{\link{deputados_id}()},
\code{\link{deputados_info}()},
\code{\link{deputados_referencias}()},
\code{\link{deputados}()}
}
\concept{deputados}
