\name{pluralityPairs}
\alias{pluralityPairs}
\docType{data}
\title{Glynn and Ichino (2014) Replication Data for Paired Design}

\description{Replication data for Glynn and Ichino (2014). Contains the dataframe \code{pluralityPairs}, corresponding to a matched pairs design.}

\usage{pluralityPairs}

\format{The dataframe has 4 variables for 8 countries in sub-Saharan Africa that held transitional multi-party presidential elections in the 1990s:
	\itemize{
		\item{\code{country}: country name (string).}
		\item{\code{pair}: pair identifier (numeric).  The 8 countries are matched into 4 pairs.}
		\item{\code{plurality}: binary treatment indicator (numeric), where 1 denotes holding a transitional multi-party presidential elections in the 1990s using plurality rules with one round only. A country is coded 0 if it had a run-off provision to eliminate weaker candidates and hold a second round election if no candidate met a vote threshold in the first round.}
		\item{\code{OppHarRank}: within-pair country rank for opposition harassment by the incumbent regime in the lead up to the transitional multi-party presidential election (numeric).  Within each pair, the country that had greater opposition harassment is coded 1; the other country is coded 0.  The rank is determined by qualitative reports of whether the incumbent regime targeted only opposition leaders or supporters or both; whether opposition members were detained and their treatment during dention, such as torture or death; whether violence was threatened or used; and the extent of any violence towards the opposition.  
		}		
	}
}
	
\source{	
Glynn and Ichino (2014a,b) contain fuller descriptions of opposition harassment that are the basis for the ranks and details on the sample and matching.  
	}

\references{
Glynn, Adam N., and Nahomi Ichino.  2014a.  "Using Qualitative Information to Improve Causal Inference."  American Journal of Political Science.  Forthcoming.)

Glynn, Adam N., and Nahomi Ichino.  2014b.  Supplementary Information (online appendix) for "Using Qualitative Information to Improve Causal Inference."  American Journal of Political Science.  (weblink))
	}

\seealso{\code{\link{quade}}.}
\keyword{causal inference}
\keyword{datasets}