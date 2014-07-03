\name{pluralitySets}
\alias{pluralitySets}
\docType{data}
\title{Glynn and Ichino (2014) Replication Data for Full Matching Design}

\description{Replication data for Glynn and Ichino (2014). Contains the dataframe \code{pluralitySets}, corresponding to a full matching design.}

\usage{pluralitySets}

\format{The dataframe has 4 variables for 9 countries in sub-Saharan Africa that held transitional multi-party presidential elections in the 1990s:
	\itemize{
		\item{\code{country}: country name (string).}
		\item{\code{set}: set identifier (numeric).  The 9 countries are matched into 3 sets with varying numbers of treated and control units.}
		\item{\code{plurality}: binary treatment indicator (numeric), where 1 denotes holding a transitional multi-party presidential elections in the 1990s using plurality rules with one round only. A country is coded 0 if it had a run-off provision to eliminate weaker candidates and hold a second round election if no candidate met a vote threshold in the first round.}
		\item{\code{OppHarRank}: within-set country rank for opposition harassment by the incumbent regime in the lead up to the transitional multi-party presidential election (numeric).  Within each set, the country that had the lowest level of opposition harassment is coded 1.  The country with the highest level of opposition harassment in the set is assigned the rank equal to the number of units in the set.  The rank is determined by qualitative reports of whether the incumbent regime targeted only opposition leaders or supporters or both; whether opposition members were detained and their treatment during dention, such as torture or death; whether violence was threatened or used; and the extent of any violence towards the opposition.  
		}		
	}
}

\source{	
Glynn and Ichino (2014a,b) contain fuller descriptions of opposition harassment that are the basis for the ranks and details on the sample and full matching.  
	}	


\references{
Glynn, Adam N., and Nahomi Ichino.  2014a.  "Using Qualitative Information to Improve Causal Inference."  American Journal of Political Science.  Forthcoming.)

Glynn, Adam N., and Nahomi Ichino.  2014b.  Supplementary Information (online appendix) for "Using Qualitative Information to Improve Causal Inference."  American Journal of Political Science.  (weblink))
	}

\seealso{\code{\link{quade}}.}
\keyword{causal inference}
\keyword{datasets}