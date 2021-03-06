% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proposicoes.R
\name{proposicoes_referencias}
\alias{proposicoes_referencias}
\title{Get metadata information for data related to 'proposicoes'}
\usage{
proposicoes_referencias(
  meta = c("siglaTipo", "codSituacao", "tiposTramitacao", "codTema", "codTipoAutor")
)
}
\arguments{
\item{meta}{Types of metadata}
}
\value{
A list of tibbles with metadata information on data related to 'proposicoes'
}
\description{
Get metadata information for data related to 'proposicoes'.
Five types of metadata are available: a) 'siglaTipo', with types of propositions;
b) 'codSituacao', with types of status for a proposition;
c) 'tiposTramitacao', with types of procedures for a proposition;
d) 'codTema', with types of thematic areas a proposition can be classified;
e) 'codTipoAuto', with actors that can be authors of a proposition
}
\examples{
a <- proposicoes_referencias()
}
\seealso{
Other proposicoes: 
\code{\link{proposicoes_autores}()},
\code{\link{proposicoes_historico}()},
\code{\link{proposicoes_id}()},
\code{\link{proposicoes_info}()},
\code{\link{proposicoes_relacionadas}()},
\code{\link{proposicoes_temas}()},
\code{\link{proposicoes_votacoes}()},
\code{\link{proposicoes}()}
}
\concept{proposicoes}
