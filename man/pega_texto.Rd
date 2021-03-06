% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pega_texto.R
\name{pega_texto}
\alias{pega_texto}
\title{Conexão do R com a SIDRA - IBGE}
\usage{
pega_texto(pagina, css)
}
\arguments{
\item{pagina}{Página que retorna da requisição da API com a função descritores().}

\item{css}{Seletor de css em que as informações serão tiradas}
}
\description{
Esta função retorna o texto de determinado seletor css de uma página.
}
\examples{
clas_PAM <- SIDRA_classificacao(1612)
}
\keyword{IBGE}
\keyword{SIDRA}
\keyword{dados}

