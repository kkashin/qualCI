\encoding{UTF-8}
\name{quade}
\alias{quade}
\title{Calculate Quade's statistic and one-sided p-value.}
\description{Calculates Quade's statistic and one-sided p-value for a stratified completely randomized design with arbitrary probability of treatment assignment.}
\usage{
quade(obj)
}
\arguments{
  \item{obj}{an object of class "\code{matchedSets}".
 }
}


\value{
	\code{quade} returns an object of class "\code{quade}", which consists of the following components: 
	
	\item{Qobs}{observed test statistic.}
	\item{permutations}{a permutation matrix of possible test statistics and associated probabilities.}
	\item{pval}{p-value.}
	\item{sets}{the "\code{matchedSets}" object input into the function.}
	} % end of \value

\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}


\examples{
data(pluralityPairs)
between.ranks <- c(3,4,2,1)
dat <- quade.prep(data=pluralityPairs, set="pair", treatment="plurality", 
withinRank="OppHarRank", unit="country", betweenRank=between.ranks)
qout <- quade(dat)
qout

data(pluralitySets)
between.ranks <- c(2,3,1)
dat <- quade.prep(data=pluralitySets, set="set", treatment="plurality", 
withinRank="OppHarRank", unit="country", betweenRank=between.ranks)
qout <- quade(dat)
qout
head(qout$permutations)
}
\keyword{causal inference}
