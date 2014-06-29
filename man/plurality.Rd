\name{plurality}
\alias{plurality}
\docType{data}
\title{Glynn and Ichino (2014) Replication Data}

\description{Replication data for Glynn and Ichino (2014). Contains two dataframes, corresponding to a matched pairs design (\code{pluralityPairs}) and a full matching design (\code{pluralitySets}).}

\usage{pluralityPairs}
\usage{pluralitySets}

\format{The two dataframes both have the following variables: 
	\itemize{
		\item{\code{country}: country identifier (string).}
		\item{\code{set}: set identifier (numeric).}
		\item{\code{treat}: binary treatment indicator, where treatment denotes (insert here).}
		\item{\code{rank}: within-set unit rank.}
		}
}
		
	
\source{
cite paper here.
}

\seealso{\code{\link{quade}}. Run \code{demo(qualci)} for examples which use \code{plurality}.}
\keyword{causal inference}
\keyword{datasets}
