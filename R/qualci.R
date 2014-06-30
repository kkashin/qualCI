qualci <- function(obj){
	# if matched pairs
	if(attr(obj,"pairs")){
		lowest.rank.set <- which.min(sapply(obj$sets,function(x) x$rank))
		lowest.rank.set.name <- names(lowest.rank.set)
		lowest.rank.pair <- colnames(obj$sets[[lowest.rank.set]]$possibleTreat)
	}
	else{
		candidatePairsBySet <- lapply(obj$sets, function(st) getCandidatePairs(st))
		candidatePairsBySet <- candidatePairsBySet[sapply(candidatePairsBySet,length)!=0]
		candidatePairsDF <- melt(candidatePairsBySet)
		colnames(candidatePairsDF) <- c("Pair","Set")		
		cat("All treatment / control pairs of adjacent rank:\n")
		print(candidatePairsDF)
		hardpair.idx <- readline("Please enter the number of the pair that was hardest to rank and press enter:")
		lowest.rank.set.name <- candidatePairsDF[hardpair.idx,"Set"]
		lowest.rank.pair <- strsplit(as.character(candidatePairsDF[hardpair.idx,"Pair"])," / ")[[1]]
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
		cat(paste("Qualitative confidence interval is difference between ",obj$qualci[1], " and ", obj$qualci[2], " in set ", obj$set, "\n", sep=""))
		cat(paste("Confidence level: ", round(obj$ci.level*100,2), "%", sep=""))
	}
}


getCandidatePairs <- function(st){
	rank.order <- order(st$withinRank, decreasing=T)
	treat.ordered <- st$obsTreat[rank.order]
	breaks <- (treat.ordered[-1L] != treat.ordered[-length(treat.ordered)]) & (treat.ordered[-length(treat.ordered)]==1)
	ind.breaks <- as.numeric(which(breaks))
	out <- sapply(ind.breaks, function(i) paste(names(treat.ordered[i]),"/",names(treat.ordered[i+1]),sep=" "))
	out
}

