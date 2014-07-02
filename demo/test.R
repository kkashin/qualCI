library(qualci)
data(pluralityPairs)
data(pluralitySets)


##### PAIRS #####
between.ranks <- c(3,4,2,1)
dat <- prepareData(data=pluralityPairs, set="pair", unit="country", treatment="plurality", withinRank="OppHarRank", betweenRank=between.ranks)
dat
qout <- quade(dat)
qout
head(qout$permutations)
qualci(qout)


##### SETS #####
between.ranks <- c(2,3,1)
dat <- prepareData(data=pluralitySets, set="set", unit="country", treatment="plurality", withinRank="OppHarRank", betweenRank=between.ranks)
qout <- quade(dat)
dat
qout
head(qout$permutations)
qualci(qout)