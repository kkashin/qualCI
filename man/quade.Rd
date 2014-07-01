\encoding{UTF-8}
\name{quade}
\alias{quade}
\title{Calculate Quade's statistic.}
\description{Calculates signed rank statistic for matched pairs design and Quade's statistic for a general matched sets design, along with the corresponding p-value.}
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
	
	\item{Qobs}{observed test statistic.}
	\item{permutations}{a permutation matrix of possible test statistics and associated probabilities.}
	\item{pval}{p-value.}
	\item{sets}{the "\code{\link{matchedSets}}" object input into the function.}
	} % end of \value
\references{

}
\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}

\seealso{
}
\examples{
data(plurality)

between.ranks <- c(3,4,2,1)
dat <- prepareData(data=pluralityPairs, set="set", treatment="treat", 
withinRank="rank", betweenRank=between.ranks)
qout <- quade(dat)
qout

between.ranks <- c(2,3,1)
dat <- prepareData(data=pluralitySets, set="set", treatment="treat", 
withinRank="rank", betweenRank=between.ranks)
qout <- quade(dat)
qout
head(qout$permutations)
}
\keyword{causal inference}
