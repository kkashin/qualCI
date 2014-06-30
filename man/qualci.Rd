\encoding{UTF-8}
\name{qualci}
\alias{qualci}
\title{Calculate qualitative confidence intervals.}
\description{insert here.}
\usage{
qualci(obj)
}
\arguments{
  \item{obj}{an object of class "\code{\link{quade}}".
 }
}

\details{Function for calculating qualitative confidence intervals 
}
\value{
	\code{qualci} returns an object of class \code{"qualci"}, which consists of the following components: 
	
	\item{qualci}{pair of units whose difference represents the qualitative confidence interval.}
	\item{set}{set from which pair was drawn.}
	\item{ci.level}{confidence interval level based on sign rank test (for matched pairs) and the stratified rank sum test (for more general matched sets).}
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
qualci(qout)

between.ranks <- c(2,3,1)
dat <- prepareData(data=pluralitySets, set="set", treatment="treat", 
withinRank="rank", betweenRank=between.ranks)
qout <- quade(dat)
qualci(qout)

}
\keyword{causal inference}
