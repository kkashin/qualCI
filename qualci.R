##### Qual Info Package ######
##### Konstantin Kashin ######
##### March 6, 2014     ######

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
	cat(paste("Design contains",nsets,"matched sets:\n\n",sep=" "))
	print(as.data.frame(tab),right=FALSE,quote=FALSE, row.names=FALSE)
}


prepareData <- function(data, set, treatment, withinRank, unit=NULL, betweenRank){
    # save call
    call <- match.call()
    
	# check for presence of needed objects
    ind <- match(c("data","set","treatment","withinRank","betweenRank"),names(call),nomatch=0)
    if(ind[1]==0){
    	stop("A 'data' argument is required.",call.=FALSE)
    }
    if(ind[2]==0){
    	stop("A 'set' argument is required.",call.=FALSE)
    }
    if(ind[3]==0){
    	stop("You must specify the treatment indicator using 'treatment'.",call.=FALSE)
    }
    if(ind[4]==0){
    	stop("You must specify the within-set rank using 'withinRank'.",call.=FALSE)
    }
    if(ind[5]==0){
    	stop("A 'betweenRank' argument is required.",call.=FALSE)
    }
    
    # check that data is dataframe
	if(class(data)!="data.frame"){
		stop("'data' object must be a data frame.")
	}
	
	# check that variables in dataframe
   	if (!(set %in% colnames(data))) {
        stop("Please make sure that 'set' is in the data object.",call.=FALSE)
    }
   	if (!(treatment %in% colnames(data))) {
        stop("Please make sure that 'treatment' is in the data object.",call.=FALSE)
    }
   	if (!(withinRank %in% colnames(data))) {
        stop("Please make sure that 'withinRank' is in the data object.",call.=FALSE)
    } 

	# check that no missing data
	dsub <- data[,c(set,treatment,withinRank)]
	if(any(is.na(dsub))){
		stop("Please make sure that there is no missing data in set identifiers, treatment indicators, or the within-set rank.")	
	}
	
	# check that treatment is binary
    if(any(data[,treatment] %in% c(0,1)==FALSE)){
    	stop("Treatment indicator must be binary.")
    }

    if(!is.numeric(data[,withinRank])){
    	stop("Within-set rank must be numeric / integer.")
    }
  
    # get all unique sets
    sets <- as.character(as.vector(unique(data[,set])))
	
	# turn variables into lists by set
	treatList <- by(data, data[,set],function(x) x[, treatment])
	withinRankList <- by(data, data[,set],function(x) x[, withinRank])
	if(!is.null(unit)){
		units <- by(data[,unit], data[,set],function(x) as.character(x))
	}
	
  	# check that ranks don't exceed number of units per set (by set)
    if(any(sapply(withinRankList, function(x) any(x %in% 1:length(x) ==FALSE)))){
    	stop("Within-set ranks may not exceed the number of units per set.")
    }
    
    # check that betweenRank is a vector
    if(!is.vector(betweenRank) | !is.numeric(betweenRank)){
    	stop('betweenRank should be a numeric vector of between-set ranks.')
    }
    
    # if no names for betweenRank, set to integers
    if(is.null(names(betweenRank))){
    	names(betweenRank) <- as.character(1:length(betweenRank))
    }
    
    # check that sets are contained in betweenRanks
 	if(any(sets %in% names(betweenRank)==FALSE)){
 		stop("'betweenRank' object is missing between-set rank for at least one set.")
 	}   
    
    # calculate possible treatments	
	out <- lapply(treatList,function(x) possible.treatments(x))
	names(out) <- as.character(1:length(out))
	for(s in 1:length(out)){
		out[[s]]$obsTreat <- treatList[[s]]	
		out[[s]]$withinRank <- withinRankList[[s]]
		bw <- betweenRank[names(out)[s]]
		names(bw) <- NULL
		out[[s]]$rank <- bw
		if(!is.null(unit)){
			set.labels <- units[[s]]
			colnames(out[[s]]$possibleTreat) <- set.labels
			names(out[[s]]$obsTreat) <- set.labels
			names(out[[s]]$withinRank) <- set.labels
			}
		}
	attr(out,"unitNames") <- ifelse(!is.null(unit), TRUE, FALSE)
	class(out) <- "matchedSets"
	return(out)
}

possible.treatments <- function(d) {
  dat <- factor(d)
  N <- length(dat) # total number of units in set
  n <- tabulate(dat) # count of ctrl and treated units
  ng <- length(n) # number of values (should always be 2)
  if(ng!=2) stop("Need 2 treatment levels per set.")
  foo2 <- combn(N,n[2]) # these are essentially indices for control
  #out <- matrix(NA, nrow=N, ncol=ncol(foo2))
  out <- sapply(1:ncol(foo2),function(c){
  	vec <- rep(0,N)
  	vec[foo2[,c]] <- 1
  	vec
  	})
  out <- list(possibleTreat = t(out), prob = rep(1/nrow(t(out)),nrow(t(out))))
}

quade <- function(dat){
	Qs <- lapply(dat,function(st) st$rank*as.vector(st$possibleTreat %*% st$withinRank))
	Qtab <- do.call(expand.grid,Qs)
	Q <- rowSums(Qtab)
	Qobs <- sum(sapply(dat,function(st) st$rank*as.vector(st$obsTreat %*% st$withinRank)))
	Qprobs <- apply(do.call(expand.grid,lapply(dat, function(st) st$prob)),1,prod)
	pval <- sum(Qprobs[which(Q>=Qobs)])
	perms <- data.frame(Q=Q,prob=Qprobs)
	out <- list(Qobs=Qobs,permutations=perms,pval=pval)
	class(out) <- "quade"
	return(out)
}

print.quade<- function(obj){
	cat(paste("Observed Quade's statistic:",obj$Qobs,"\n",sep=" "))
	cat(paste("Pr(>= Qobs):",round(obj$pval,5),"\n",sep=" "))
}