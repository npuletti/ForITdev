\name{Q_INFCvpe}
\alias{Q_INFCvpe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volume and Phytomass Estimates
}
\description{
The function estimates tree volume and phytomass per species from stem diameter at 1.3 m height (DBH) and total height (HT)
}
\usage{
Q_INFCvpe(mydata)
}

\arguments{
  \item{mydata}{
%%     ~~Describe \code{TreeList} here~~
}
}
\details{

}
\value{
Returns a \code{list} with the following objects: \cr
\item{$mainData }{a \code{data.frame} with the following columns
\tabular{ll}{ \code{spg} \tab a string with the species group code \cr \code{d130} \tab a value indicating the stem diameter at 1.3 m height (DBH) [cm] \cr
\code{h_tot} \tab a value indicating the total stem height (HT) [m]\cr \code{freq}\tab the number of trees of the same \code{spg} with equal DBH and HT\cr \code{mod} \tab a character, the same as \code{mod} in arguments. \cr \code{T_0}\tab a value of the estimates for \code{mod} value.  \code{v} is expressed in \emph{dm^3} while \code{dw1}, \code{dw2}, \code{dw3}, \code{dw4} are expressed in
\emph{kg}; \cr \code{SEE}\tab a value of Standard Error of the Estimates \cr
\code{dof} \tab the degree of freedom \cr \code{in.range} \tab tree inside (\code{y}) or out of the range (\code{n}) of the sampled trees in Gasparini and
Tabacchi (2011)\cr } } \item{out.of.range}{a \code{data.frame} listing the
trees out of the range of application (domain)}
}
\references{
Gasparini, P., Tabacchi, G.(eds), 2011. \emph{L'Inventario
Nazionale delle Foreste e dei serbatoi forestali di Carbonio INFC 2005.
Secondo inventario forestale nazionale italiano. Metodi e risultati}.
Edagricole. 653 pp. [ITA, ita]
Tabacchi G., Di Cosmo L., Gasparini P., Morelli S., 2011a. \emph{Stima
del volume e della fitomassa delle principali specie forestali italiane.
Equazioni di previsione, tavole del volume e tavole della fitomassa arborea
epigea. Stima del volume e della fitomassa delle principali specie
forestali italiane. Equazioni di previsione, tavole del volume e tavole
della fitomassa arborea epigea}. 412 pp. [ITA, ita]
Tabacchi G., Di Cosmo L., Gasparini P., 2011b. \emph{Aboveground tree
volume and phytomass prediction equations for forest species in Italy}.
European Journal of Forest Research 130: 6 911-934 [ENG, eng]
}
\author{
Nicola Puletti
}

\examples{
## try something by yourself
}
