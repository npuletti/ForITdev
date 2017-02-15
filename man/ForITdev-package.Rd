\name{ForITdev-package}
\alias{ForITdev-package}
\alias{ForITdev}
\docType{package}
\title{
Volume and Biomass computations by Italian Forest Inventory
}
\description{
The ForIT package allows the computation of the biomass and volume functions carried out by Gasparini and Tabacchi (2011) and Tabacchi et al. (2011a) during the 2nd Italian National Forest Inventory. An English description of the methodology is provided by Tabacchi et al. (2011b). This package is intended as the translation in R of the literature cited above.
}
\details{
\tabular{ll}{Package: \tab ForIT\cr
             Type: \tab Package\cr
             Version: \tab 1.1 \cr
             Date: \tab 2017-02-28\cr
             License: \tab 3.0}
}
\author{
Nicola Puletti, Cristiano Castaldi, Maurizio Marchi, Roberto Scotti
Maintainer: Nicola Puletti <nicola.puletti@gmail.com>
}
\references{
Gasparini, P., Tabacchi, G.(eds), 2011. \emph{L'Inventario
Nazionale delle Foreste e dei serbatoi forestali di Carbonio INFC 2005.
Secondo inventario forestale nazionale italiano. Metodi e risultati}.
Edagricole. 653 pp. [ITA, ita]
Tabacchi G.; Di Cosmo L.; Gasparini P.; Morelli S., 2011a. \emph{Stima del
volume e della fitomassa delle principali specie forestali italiane.
Equazioni di previsione, tavole del volume e tavole della fitomassa arborea
epigea. Stima del volume e della fitomassa delle principali specie
forestali italiane. Equazioni di previsione, tavole del volume e tavole
della fitomassa arborea epigea}. 412 pp. [ITA, ita]
Tabacchi G., Di Cosmo L., Gasparini P., 2011b. \emph{Aboveground tree
volume and phytomass prediction equations for forest species in Italy}.
European Journal of Forest Research 130: 6 911-934 [ENG, eng]
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# Analyse your dataset
TL <- Q_INFCvpe(mydata)
}
