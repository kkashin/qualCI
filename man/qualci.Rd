\encoding{UTF-8}
\name{qualCI}
\alias{qualCI}
\title{Calculate qualitative confidence intervals.}
\description{Calculates one-sided qualitative confidence intervals based on a sign test for a matched pairs design and a stratified rank sum test for a general matched sets design.}
\usage{
qualCI(obj)
}
\arguments{
  \item{obj}{an object of class "\code{\link{quade}}".
 }
}

\details{For a matched pairs design, the function identifies the difference between the units in the lowest ranked pair as the qualitative confidence interval, along with the confidence level from the sign test. For a general matched sets design, the function prompts the user to identify the hardest-to-rank pair from a pool of candidate pairs (all treatment-control pairs where the treated unit has a within-set rank that is immediately above the rank for the control unit). The function then returns the difference between the units in this pair as the qualitative confidence interval, along with the confidence level from the stratified rank sum test.
}
\value{
	\code{qualCI} returns an object of class "\code{qualCI}", which consists of the following components: 
	
	\item{qualci}{pair of units whose difference represents the qualitative confidence interval}
	\item{set}{set from which pair was drawn}
	\item{ci.level}{confidence interval level based on sign rank test for matched pairs and the stratified rank sum test for more general matched sets}
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
qualCI(qout)


data(pluralitySets)
between.ranks <- c(2,3,1)
dat <- quade.prep(data=pluralitySets, set="set", treatment="plurality", 
withinRank="OppHarRank", unit="country", betweenRank=between.ranks)
qout <- quade(dat)
# not run because requires user input
# qualCI(qout)

}
\keyword{causal inference}
