\encoding{UTF-8}
\name{prepareData}
\alias{prepareData}
\alias{print.matchedSets}
\title{Prepare data for qualCI package.}
\description{The function prepares data.}
\usage{
prepareData(data, set, treatment, withinRank, unit=NULL, betweenRank)
}
\arguments{
  \item{data}{an object of class "\code{\link{data.frame}}" containing set, treatment, and within-set rank variables.
 }
  \item{set}{the column name of \code{data} that contains the set identifier. It cannot contain any \code{NA}s.
}
  \item{treatment}{the column name of \code{data} that contains the treatment indicator. Treatment indicator must be binary and cannot contain any \code{NA}s.
}
  \item{withinRank}{the column name of \code{data} that contains the within-set ranks. It cannot contain any \code{NA}s and must be numeric.
}
  \item{unit}{the column name of \code{data} that contains the unit names. May be set to \code{NULL}, in which case the unit's name is set to its within-set position.
 }
  \item{betweenRank}{a (named) numeric vector of between-set ranks. Names on vector should correspond to set identifiers in the \code{data$set}. If \code{betweenRank} object has no \code{names} attribute, an element's name is set to its position in the vector.
}
}

\details{Function for preparing data for quade. 

}
\value{
	\code{prepareData} returns an object of class \code{"matchedSets"}, which is a named list of length equal to the number of sets. A given list element (corresponding to a set) consists of the following components: 
	
	\item{possibleTreat}{a matrix of possible treatment vectors.}
	\item{prob}{a vector of probabilities giving probability of receiving particular treatment vector.}
	\item{obsTreat}{observed treatment vector.}
	\item{withinRank}{within-set ranks of units in given set.}
	\item{rank}{between-set rank of given set.}
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
dat

between.ranks <- c(2,3,1)
dat <- prepareData(data=pluralitySets, set="set", treatment="treat", 
withinRank="rank", betweenRank=between.ranks)
dat
dat[[1]]
}
\keyword{causal inference}
