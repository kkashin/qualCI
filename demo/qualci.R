##### EXAMPLES OF USING QUAL INFO PACKAGE ######

# let's create the data as in the pair matching section in the paper:
# set 1: 1 0
# set 2: 1 0 
# set 3: 1 0
# set 4: 1 0

### PREPARE DATAFRAME VIA LISTS ###
# create list where element s is a vector of observed treatments for set s
# element j of vector s is the observed treatment indicator for unit j of set s
obs.treatments <- list(c(1,0),c(1,0),c(1,0),c(1,0))

# create a corresponding list of within-set ranks (should match structure of obs.treatments)
within.ranks <- list(c(2,1),c(2,1),c(2,1),c(2,1))

# create a corresponding list of labels (optional)
labels <- list(c("Cameroon","Gabon"),c("Kenya","Cote d'Ivoire"), c("Malawi","Zambia"), c("Tanzania", "Guinea-Bissau"))

# put together into dataframe
df <- data.frame(country=unlist(labels), set = rep(c(1,2,3,4), times=c(2,2,2,2)), treat=unlist(obs.treatments), rank = unlist(within.ranks))
df

# create vector of between ranks (could be named vector; if it isn't, assumes set names are just the vector positions)
between.ranks <- c(3,4,2,1)

### RUN DATA PREPARE FUNCTION ###
# pass dataframe to prepareData function, along with the variable labels that denote treatment, set membership, label, and within-set rank
# also pass list that gives betweenRank between sets
# function return a list for each set j, containing a matrix with possible treatment combinations for that set j (conditional on m_s = number of treated units in set s), vector of probabilities associated with each row in the matrix, and the set's rank
# by default, function outputs equal probabilities for each treatment vector 
dat <- prepareData(data=df,set="set", treatment="treat",withinRank="rank",betweenRank=between.ranks)

# print data object (output made pretty)
dat

# now look at a given set to see the insides of the dat object
dat[[1]]


### RUN THE FUNCTION TO CALCULATE P-VALUES for Quade's statistic ###
qout <- quade(dat)
qout 

# of course, you can look inside qout to get the permutation distribution of Q
qout$permutations



# let's create the data as in the full matching section in the paper:
# set 1: 1 0 0 0
# set 2: 1 1 0
# set 3: 1 0

### PREPARE DATAFRAME VIA LISTS ###
# create list where element s is a vector of observed treatments for set s
# element j of vector s is the observed treatment indicator for unit j of set s
obs.treatments <- list(c(1,0,0,0),c(1,1,0),c(1,0))

# create a corresponding list of within-set ranks (should match structure of obs.treatments)
within.ranks <- list(c(4,3,2,1),c(3,2,1),c(2,1))

# create a corresponding list of labels (optional)
labels <- list(c("Cameroon","Gabon","Cote d'Ivoire", "Madagascar"), c("Kenya","Malawi","Zambia"), c("Tanzania", "Guinea-Bissau"))

# put together into dataframe
df <- data.frame(country=unlist(labels), set = rep(c(1,2,3), times=c(4,3,2)), treat=unlist(obs.treatments), rank = unlist(within.ranks))
df

# create vector of between ranks (could be named vector; if it isn't, assumes set names are just the vector positions)
between.ranks <- c(2,3,1)

### RUN DATA PREPARE FUNCTION ###
# pass dataframe to prepareData function, along with the variable labels that denote treatment, set membership, label, and within-set rank
# also pass list that gives betweenRank between sets
# function return a list for each set j, containing a matrix with possible treatment combinations for that set j (conditional on m_s = number of treated units in set s), vector of probabilities associated with each row in the matrix, and the set's rank
# by default, function outputs equal probabilities for each treatment vector 
dat <- prepareData(data=df, set="set", treatment="treat", withinRank="rank", betweenRank=between.ranks)

# print data object (output made pretty)
dat

# now look at a given set to see the insides of the dat object
dat[[1]]


### RUN THE FUNCTION TO CALCULATE P-VALUES for Quade's statistic ###
qout <- quade(dat)
qout 

# of course, you can look inside qout to get the permutation distribution of Q
qout$permutations



### CHANGE PROBABILITIES OF TREATMENT VECTOR ###
# for example, if we believe that treatment is 2 times as likely for unit 1 than for unit 2 in set 3:
dat[[3]]$prob <- c(2/3,1/3)
quade(dat)


### Replicate Table 7 from paper ###
pi1 <- list(c(1/4,1/4,1/4,1/4),c(1.25/4.25,rep((1-1.25/4.25)/3,3)),c(1.5/4.5,rep((1-1.5/4.5)/3,3)), c(2/5,rep(1/5,3)), c(2.5/5.5,rep((1-2.5/5.5)/3,3)),c(3/6,rep(1/6,3)))
pi3 <- list(c(1/2,1/2),c(1.25/2.25,1-1.25/2.25), c(1.5/2.5, 1-1.5/2.5), c(2/3, 1/3), c(2.5/3.5, 1-2.5/3.5), c(3/4, 1/4))
table7 <- matrix(data=NA, nrow=length(pi1), ncol=length(pi3))

for(m in 1:nrow(table7)){
	for(n in 1:ncol(table7)){
		dat[[1]]$prob <- pi1[[m]]
		dat[[3]]$prob <- pi3[[n]]
		table7[m,n] <- quade(dat)$pval
	}
}

table7