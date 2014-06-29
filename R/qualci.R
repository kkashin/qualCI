qualci <- function(obj){
	# if matched pairs
	if(attr(obj,"pairs")){
		lowest.rank.set <- which.min(sapply(obj$sets,function(x) x$rank))
		lowest.rank.set.name <- names(lowest.rank.set)
		lowest.rank.pair <- colnames(obj$sets[[lowest.rank.set]]$possibleTreat)
	}
	else{
		lowest.rank.set.name <- NA
		lowest.rank.pair <- NA
	}
	# ci level
	alpha <- within.statistic(obj$sets)
	ci.level <- 1-alpha
	out <- list(set=lowest.rank.set.name,qualci=lowest.rank.pair, ci.level=ci.level)
	attr(out,"pairs") <- attr(obj,"pairs")
	class(out) <- "qualci"
	return(out)
}

print.qualci <- function(obj){
	if(attr(obj,"pairs")){
		cat(paste("Qualitative confidence interval is difference between ",obj$qualci[1], " and ", obj$qualci[2], " in set ", obj$set, "\n", sep=""))
		cat(paste("Confidence level: ", round(obj$ci.level*100,2), "%", sep=""))
	} else{
		cat("Qualitative confidence interval not yet implemented for matched sets.")
		cat(paste("Confidence level: ", round(obj$ci.level*100,2), "%", sep=""))
	}
}
