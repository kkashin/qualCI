\name{pluralitySets}
\alias{pluralitySets}
\docType{data}
\title{Glynn and Ichino (2014) Replication Data for Full Matching Design}

\description{Replication data for Glynn and Ichino (2014). Contains the dataframe \code{pluralitySets}, corresponding to a full matching design.}

\usage{pluralitySets}

\format{The dataframe has the following variables for 9 countries in sub-Saharan Africa:
	\itemize{
		\item{\code{country}: Country name (string).}
		\item{\code{set}: set identifier (numeric).  The 9 countries are matched into 3 sets, with variable numbers of treated and control countries.}
		\item{\code{treat}: binary treatment indicator, where treatment denotes holding a transitional multi-party presidential elections in the 1990s using plurality rules with one round only.}
		\item{\code{rank}: within-set unit rank.}		

}
		
	
\source{
Supplementary Information (online appendix) for Glynn, Adam N., and Nahomi Ichino.  2014.  "Using Qualitative Information to Improve Causal Inference."  American Journal of Political Science.  Forthcoming.
}

\seealso{\code{\link{quade}}. Run \code{demo(qualci)} for examples which use \code{pluralitySets}.}
\keyword{causal inference}
\keyword{datasets}
