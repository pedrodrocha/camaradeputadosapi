% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partidos.R
\name{partidos}
\alias{partidos}
\title{Get a list of political parties}
\usage{
partidos(...)
}
\arguments{
\item{...}{Further query parameters from the House of Representatives API}
}
\value{
A tibble with political parties
}
\description{
Get a list of political parties that had or currently have representatives at the Brazilian House of Representatives.
If query parameters from the API such as 'dataInicio', 'dataFim' and 'idLegislatura' are not used, it will return only parties that currently have a representative.
For further parameters please check the official API website (\href{https://dadosabertos.camara.leg.br/swagger/api.html}{Info}).
}
\examples{
a <- partidos(idLegislatura = "56")
}
\seealso{
Other partidos: 
\code{\link{partidos_blocos}()},
\code{\link{partidos_id}()},
\code{\link{partidos_info}()},
\code{\link{partidos_membros}()}
}
\concept{partidos}
