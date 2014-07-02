\encoding{UTF-8}
\name{quade}
\alias{quade}
\title{Calculate Quade's statistic and one-sided p-value.}
\description{Calculates Quade's statistic and one-sided p-value for a stratified completely randomized design with arbitrary probability of treatment assignment}
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
data(pluralityPairs)
between.ranks <- c(3,4,2,1)
dat <- prepareData(data=pluralityPairs, set="pair", treatment="plurality", 
withinRank="OppHarRankPair", betweenRank=between.ranks)
qout <- quade(dat)
qout

data(pluralitySets)
between.ranks <- c(2,3,1)
dat <- prepareData(data=pluralitySets, set="set", treatment="plurality", 
withinRank="OppHarRankSet", betweenRank=between.ranks)
qout <- quade(dat)
qout
head(qout$permutations)
}
\keyword{causal inference}
