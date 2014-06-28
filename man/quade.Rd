\encoding{UTF-8}
\name{quade}
\alias{quade}
\title{Calculate Quade's statistic.}
\description{Quade.}
\usage{
quade(obj)
}
\arguments{
  \item{obj}{an object of class "\code{\link{matchedSets}}".
 }
}

\details{Function for running quade 
}
\value{
	\code{quade} returns an object of class \code{"quade"}, which consists of the following components: 
	
	\item{Qobs}{observed Q statistic.}
	\item{permutations}{a permutation matrix of possible Q statistics and associated probabilities.}
	\item{pval}{p-value.}
	} % end of \value
\references{

}
\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}

\seealso{
}
\examples{
data(GlynnIchino)
between.ranks <- c(2,3,1)
dat <- prepareData(data=GlynnIchino, set="set", treatment="treat", 
withinRank="rank", betweenRank=between.ranks)
qout <- quade(dat)
qout
head(qout$permutations)
}
\keyword{causal inference}
