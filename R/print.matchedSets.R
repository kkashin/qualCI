print.matchedSets <- function(x){
	nsets <- length(x)
	sets <- names(dat)
	if(attr(x,"unitNames")){
		tab <- t(sapply(dat,function(x) c(paste(names(which(x$obsTreat==1)),collapse=", "),paste(names(which(x$obsTreat==0)),collapse=", "), x$rank)))
		tab <- cbind(sets,tab)
		colnames(tab) <- c("Set", "Treated","Control","Rank")
	}
	else{
		tab <- t(sapply(dat,function(x) c(sum(x$obsTreat==1),sum(x$obsTreat==0), x$rank)))
		tab <- cbind(sets,tab)
		colnames(tab) <- c("Set", "Num. Treated","Num. Control","Rank")
	}
	if(attr(x,"pairs")){
		cat(paste("Design contains",nsets,"matched pairs:\n\n",sep=" "))
	}
	else{
		cat(paste("Design contains",nsets,"matched sets:\n\n",sep=" "))	
	}
	print(as.data.frame(tab),right=FALSE,quote=FALSE, row.names=FALSE)
}