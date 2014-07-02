\name{pluralityPairs}
\alias{pluralityPairs}
\docType{data}
\title{Glynn and Ichino (2014) Replication Data for Paired Design}

\description{Replication data for Glynn and Ichino (2014). Contains the dataframe \code{pluralityPairs}, corresponding to a matched pairs design.}

\usage{pluralityPairs
	   }

\format{The dataframe has the following variables for 8 countries in sub-Saharan Africa in the 1990s:
	\itemize{
		\item{\code{country}: country name (string).}
		\item{\code{pair}: pair identifier (numeric).  The 8 countries are matched into 4 pairs.}
		\item{\code{treat}: binary treatment indicator (numeric), where treatment denotes holding a transitional multi-party presidential elections in the 1990s using plurality rules with one round only.}
		\item{\code{opphar_rankPair}: within-pair country rank for opposition harassment by the incumbent regime in the lead up to the transitional presidential election (numeric).  Within each pair, the country that had greater opposition harassment is coded 1; the other country is coded 0.  The rank is determined by qualitative reports of whether the incumbent regime targeted only opposition leaders or supporters or both; whether opposition members were detained and their treatment during dention, such as torture or death; whether violence was threatened or used; and the extent of any violence towards the opposition.  
}		

}
		
	
\source{
Supplementary Information (online appendix) for Glynn, Adam N., and Nahomi Ichino.  2014.  "Using Qualitative Information to Improve Causal Inference."  American Journal of Political Science.  Forthcoming.
}

\seealso{\code{\link{quade}}. Run \code{demo(qualci)} for examples which use \code{pluralityPairs}.}
\keyword{causal inference}
\keyword{datasets}
