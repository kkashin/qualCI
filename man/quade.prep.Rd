\encoding{UTF-8}
\name{quade.prep}
\alias{quade.prep}
\alias{print.matchedSets}
\title{Prepare data for qualitative causal inference.}
\description{Function to prepare data for the \code{\link{quade}} function and hence the \code{\link{qualCI}} function. Processes a data frame with set identifiers, treatment indicators, and within-set ranks, along with vector of between-set ranks to create a "\code{matchedSets}" object.}
\usage{
quade.prep(data, set, treatment, withinRank, unit=NULL, betweenRank)
}
\arguments{
  \item{data}{an object of class "\code{data.frame}" containing set, treatment, and within-set rank variables.
 }
  \item{set}{the column name of \code{data} that contains the set identifier. It cannot contain any \code{NA}s.
}
  \item{treatment}{the column name of \code{data} that contains the treatment indicator. Treatment indicator must be binary and cannot contain any \code{NA}s.
}
  \item{withinRank}{the column name of \code{data} that contains the within-set ranks. It cannot contain any \code{NA}s and must be numeric. See below for details.
}
  \item{unit}{the column name of \code{data} that contains the unit names. May be set to \code{NULL}, in which case the unit's name is set to its within-set position.
 }
  \item{betweenRank}{a (named) numeric vector of between-set ranks. Names on vector should correspond to set identifiers. If \code{betweenRank} object has no \code{names} attribute, an element's name is set to its position in the vector.
}
}

\details{Function for preparing data for the \code{\link{quade}} function. Each set should contain at least one treated unit and at least one control unit.
	
Within-set ranks must consist of a sequence of consecutive integers starting with either 0 or 1. Ties are not permitted. Typical practice uses 0 and 1 to rank units in a matched pairs design and uses consecutive integers starting with 1 to rank units in a more general matched sets design. 

Between-set ranks should be consecutive integers starting with 1. The names on the vector should correspond to the set nomenclature in \code{data[,set]}.
}
\value{
	\code{quade.prep} returns an object of class "\code{matchedSets}", which is a named list of length equal to the number of sets. A given list element (corresponding to a set) consists of the following components: 
	
	\item{possibleTreat}{a matrix of possible treatment vectors, where rows correspond to different treatment permutations and columns correspond to the units.}
	\item{prob}{a vector of probabilities corresponding to the probability of receiving a particular treatment permutation from the matrix of possible treatments. Length is equal to the number of rows in \code{possibleTreat}.}
	\item{obsTreat}{a named vector denoting observed treatment status.}
	\item{withinRank}{a named vector denoting within-set ranks of units.}
	\item{rank}{numeric, denoting between-set rank of given set.}
	} % end of \value

\author{
Konstantin Kashin \email{kkashin@fas.harvard.edu}
}


\examples{
data(pluralityPairs)
between.ranks <- c(3,4,2,1)
dat <- quade.prep(data=pluralityPairs, set="pair", treatment="plurality", 
withinRank="OppHarRank", unit="country", betweenRank=between.ranks)
dat

data(pluralitySets)
between.ranks <- c(2,3,1)
dat <- quade.prep(data=pluralitySets, set="set", treatment="plurality", 
withinRank="OppHarRank", unit="country", betweenRank=between.ranks)
dat
dat[[1]]
}
\keyword{causal inference}
