\name{pluralitySets}
\alias{pluralitySets}
\docType{data}
\title{Glynn and Ichino (2014b) replication data for full matching design}

\description{Replication data for Glynn and Ichino (2014b). Contains the dataframe \code{pluralitySets}, corresponding to a full matching design.}

\usage{pluralitySets}

\format{The dataframe has 4 variables for 9 countries in sub-Saharan Africa that held transitional multi-party presidential elections in the 1990s:
	\itemize{
		\item{\code{country}: country name (string).}
		\item{\code{set}: set identifier (numeric).  The 9 countries are matched into 3 sets with varying numbers of treated and control units.}
		\item{\code{plurality}: binary treatment indicator (numeric), where a country is coded 1 if it used plurality rules with one round only for its transitional multi-party presidential elections in the 1990s. A country is coded 0 if the electoral rules for the transitional election had a run-off provision to eliminate weaker candidates and hold a second round election if no candidate met a vote threshold in the first round.}
		\item{\code{OppHarRankSet}: within-set rank for the country's level of opposition harassment by the incumbent regime in the lead up to the transitional multi-party presidential election (numeric).  Within each set, the country that had the lowest level of opposition harassment is coded 1.  The country with the highest level of opposition harassment in the set is assigned the rank equal to the number of units in the set.  The rank is determined by qualitative reports of whether the incumbent regime targeted only opposition leaders or supporters or both; whether opposition members were detained and their treatment during dention, such as torture or death; whether violence was threatened or used; and the extent of any violence towards the opposition.  
		}		
	}
}

\source{	
Glynn and Ichino (2014a,b) contain fuller descriptions of opposition harassment that are the basis for the ranks and details on the sample and full matching.  
	}	


\references{
Glynn, Adam N., and Nahomi Ichino.  2014a.  Supplementary Information (online appendix) for "Using Qualitative Information to Improve Causal Inference."  \emph{American Journal of Political Science}.  (weblink)

Glynn, Adam N., and Nahomi Ichino.  2014b.  "Using Qualitative Information to Improve Causal Inference."  \emph{American Journal of Political Science}.  Forthcoming.
	}

\seealso{\code{\link{quade}}.}
\keyword{causal inference}
\keyword{datasets}
