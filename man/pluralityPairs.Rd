\name{pluralityPairs}
\alias{pluralityPairs}
\docType{data}
\title{Glynn and Ichino (2014) Replication Data}

\description{Replication data for Glynn and Ichino (2014). Contains the dataframe \code{pluralityPairs}, corresponding to a matched pairs design.}

\usage{pluralityPairs
	   }

\format{The dataframe has the following variables for 8 countries in sub-Saharan Africa:
	\itemize{
		\item{\code{country}: Country name (string).}
		\item{\code{set}: set identifier (numeric).  The 8 countries are matched into 4 sets, each with 2 countries.}
		\item{\code{treat}: binary treatment indicator, where treatment denotes holding a transitional multi-party presidential elections in the 1990s using plurality rules with one round only.}
		\item{\code{rank}: within-set unit rank.}		

}
		
	
\source{
Supplementary Information (online appendix) for Glynn, Adam N., and Nahomi Ichino.  2014.  "Using Qualitative Information to Improve Causal Inference."  American Journal of Political Science.  Forthcoming.
}

\seealso{\code{\link{quade}}. Run \code{demo(qualci)} for examples which use \code{pluralityPairs}.}
\keyword{causal inference}
\keyword{datasets}
