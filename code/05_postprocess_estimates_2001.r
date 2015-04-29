#----------------------------------------------------------------
# Determining the impact of strategic voting on election results
# Michael Herrmann, Simon Munzert, Peter Selb
# 2015
#----------------------------------------------------------------


## load packages and functions ----------------
source("packages.r")
source("functions.r")


### 2001: Load and prepare data
###-----------------------------------------------------------------------------------
load("../data/data_strategic_bayesprep01.RData")
load("../bugs_simulations/stratvotemodel01_14.RData") 
attach.bugs(model1.sim)


# first, second, third position
votes92 <- cbind(spdf.england$convt92,spdf.england$labvt92,spdf.england$ldvt92,spdf.england$snpvt92,spdf.england$pcvt92,spdf.england$refvt92,spdf.england$othvt92)
winner92 <- spdf.england$win92 # winner party 1992
second92 <- spdf.england$sec92 # second party 1992
third92 <- ifelse(winner92!=1&second92!=1,1, ifelse(winner92!=2 & second92!=2, 2, 3)) # third party 1992
votes92.first <- apply(votes92, 1, max, na.rm=T) # no. of votes for the winner 1992
votes92.second <- apply(votes92, 1, second) # no. of votes for the second 1992
votes92.third <- apply(votes92[,1:3], 1, min, na.rm=T) # no. of votes for the third 1992
winner92.margin <- votes92.first - votes92.second # margin of victory 1992
second92.margin <- votes92.second - votes92.third

votes97 <- cbind(spdf.england$convt97,spdf.england$labvt97,spdf.england$ldvt97,spdf.england$snpvt97,spdf.england$pcvt97,spdf.england$refvt97,spdf.england$othvt97)
winner97 <- spdf.england$win97 # winner party 1997
winner97[497] <- 2 # correct wrong West Bromwich West prediction, Speaker district!
second97 <- spdf.england$sec97 # second party 1997
third97 <- ifelse(winner97!=1&second97!=1,1, ifelse(winner97!=2 & second97!=2, 2, 3))  # third party 1997
votes97.first <- apply(votes97, 1, max, na.rm=T) # no. of votes for the winner 1997
votes97.second <- apply(votes97, 1, second) # no. of votes for the second 1997
votes97.third <- apply(votes97[,1:3], 1, min, na.rm=T) # no. of votes for the third 1997
second97.margin <- votes97.second - votes97.third
winner97.margin <- votes97.first - votes97.second # margin of victory 1997
voteshare97.conser <- spdf.england$con97/100
voteshare97.labour <- spdf.england$lab97/100
voteshare97.libdem <- spdf.england$ld97/100
voteshare97.other <- 1 - (spdf.england$con97/100 + spdf.england$lab97/100 + spdf.england$ld97/100)
conpos97 <- ifelse(voteshare97.conser > voteshare97.labour & voteshare97.conser > voteshare97.libdem, 1,
                   ifelse((voteshare97.conser > voteshare97.labour & voteshare97.conser < voteshare97.libdem) | (voteshare97.conser < voteshare97.labour & voteshare97.conser > voteshare97.libdem), 2, 3))
labpos97 <- ifelse(voteshare97.labour > voteshare97.conser & voteshare97.labour > voteshare97.libdem, 1,
                   ifelse((voteshare97.labour > voteshare97.conser & voteshare97.labour < voteshare97.libdem) | (voteshare97.labour < voteshare97.conser & voteshare97.labour > voteshare97.libdem), 2, 3)) 
ldpos97 <-  ifelse(voteshare97.libdem > voteshare97.conser & voteshare97.libdem > voteshare97.labour, 1,
                   ifelse((voteshare97.libdem > voteshare97.conser & voteshare97.libdem < voteshare97.labour) | (voteshare97.libdem < voteshare97.conser & voteshare97.libdem > voteshare97.labour), 2, 3))

votes01 <- cbind(spdf.england$convt01,spdf.england$labvt01,spdf.england$ldvt01,spdf.england$snpvt01,spdf.england$pcvt01,spdf.england$ukipvt01,spdf.england$grnvt01,spdf.england$bnpvt01,spdf.england$othvt01)
winner01 <- spdf.england$winner01 # winner party 2001
spdf.england$second01[spdf.england$shapefile_string=="Torridge and West Devon"] <- 1 # correct error in Pippa Norris' data set
second01 <- spdf.england$second01 # second party 2001
third01 <- ifelse(winner01!=1&second01!=1,1, ifelse(winner01!=2 & second01!=2, 2, 3))
votes01.first <- apply(votes01, 1, max, na.rm=T) # no. of votes for the winner 2001
votes01.second <- apply(votes01, 1, second) # no. of votes for the second 2001
winner01.margin <- votes01.first - votes01.second # margin of victory 2001


# actual vote shares
spdf.england$con92[is.na(spdf.england$con92)] <- 0
spdf.england$lab92[is.na(spdf.england$lab92)] <- 0
spdf.england$ld92[is.na(spdf.england$ld92)] <- 0
spdf.england$con97[is.na(spdf.england$con97)] <- 0
spdf.england$lab97[is.na(spdf.england$lab97)] <- 0
spdf.england$ld97[is.na(spdf.england$ld97)] <- 0
voteshare92.conser <- spdf.england$con92/100
voteshare92.labour <- spdf.england$lab92/100
voteshare92.libdem <- spdf.england$ld92/100
voteshare92.other <- 1 - (spdf.england$con92/100 + spdf.england$lab92/100 + spdf.england$ld92/100)
voteshare97.conser <- spdf.england$con97/100
voteshare97.labour <- spdf.england$lab97/100
voteshare97.libdem <- spdf.england$ld97/100
voteshare97.other <- 1 - (spdf.england$con97/100 + spdf.england$lab97/100 + spdf.england$ld97/100)
spdf.england$con01[is.na(spdf.england$con01)] <- 0
spdf.england$lab01[is.na(spdf.england$lab01)] <- 0
spdf.england$ld01[is.na(spdf.england$ld01)] <- 0
voteshare01.conser <- spdf.england$con01/100
voteshare01.labour <- spdf.england$lab01/100
voteshare01.libdem <- spdf.england$ld01/100
voteshare01.other <- 1 - (spdf.england$con01/100 + spdf.england$lab01/100 + spdf.england$ld01/100)

# party positions
conpos92 <- ifelse(voteshare92.conser > voteshare92.labour & voteshare92.conser > voteshare92.libdem, 1,
                   ifelse((voteshare92.conser > voteshare92.labour & voteshare92.conser < voteshare92.libdem) | (voteshare92.conser < voteshare92.labour & voteshare92.conser > voteshare92.libdem), 2, 3 ))
labpos92 <- ifelse(voteshare92.labour > voteshare92.conser & voteshare92.labour > voteshare92.libdem, 1,
                   ifelse((voteshare92.labour > voteshare92.conser & voteshare92.labour < voteshare92.libdem) | (voteshare92.labour < voteshare92.conser & voteshare92.labour > voteshare92.libdem), 2, 3)) 
ldpos92 <-  ifelse(voteshare92.libdem > voteshare92.conser & voteshare92.libdem > voteshare92.labour, 1,
                   ifelse((voteshare92.libdem > voteshare92.conser & voteshare92.libdem < voteshare92.labour) | (voteshare92.libdem < voteshare92.conser & voteshare92.libdem > voteshare92.labour), 2, 3)) 
conpos97 <- ifelse(voteshare97.conser > voteshare97.labour & voteshare97.conser > voteshare97.libdem, 1,
                   ifelse((voteshare97.conser > voteshare97.labour & voteshare97.conser < voteshare97.libdem) | (voteshare97.conser < voteshare97.labour & voteshare97.conser > voteshare97.libdem), 2, 3))
labpos97 <- ifelse(voteshare97.labour > voteshare97.conser & voteshare97.labour > voteshare97.libdem, 1,
                   ifelse((voteshare97.labour > voteshare97.conser & voteshare97.labour < voteshare97.libdem) | (voteshare97.labour < voteshare97.conser & voteshare97.labour > voteshare97.libdem), 2, 3)) 
ldpos97 <-  ifelse(voteshare97.libdem > voteshare97.conser & voteshare97.libdem > voteshare97.labour, 1,
                   ifelse((voteshare97.libdem > voteshare97.conser & voteshare97.libdem < voteshare97.labour) | (voteshare97.libdem < voteshare97.conser & voteshare97.libdem > voteshare97.labour), 2, 3))

# strategic shifts, party-specific
numsim <- n.sims
shift.conser <- matrix(NA,numsim,528)
shift.labour <- matrix(NA,numsim,528)
shift.libdem <- matrix(NA,numsim,528)
#b.incent.cola <- rep(0,numsim) # set artificially to 0
#b.incent.laco <- rep(0,numsim) # set artificially to 0
for (i in 1:N){
  shift.conser[,i] <- b.incent.cola[1:numsim]*-incent.cola[i] + b.incent.coli[1:numsim]*-incent.coli[i] + b.incent.laco[1:numsim]*incent.laco[i] + b.incent.lico[1:numsim]*incent.lico[i] 
  shift.labour[,i] <- b.incent.cola[1:numsim]*incent.cola[i] + b.incent.lali[1:numsim]*-incent.lali[i] + b.incent.laco[1:numsim]*-incent.laco[i] + b.incent.lila[1:numsim]*incent.lila[i]
  shift.libdem[,i] <- b.incent.coli[1:numsim]*incent.coli[i] + b.incent.lali[1:numsim]*incent.lali[i] + b.incent.lico[1:numsim]*-incent.lico[i] + b.incent.lila[1:numsim]*-incent.lila[i]
}

# strategic shifts, total votes
shift.cola.votes <- matrix(NA,numsim,528)
shift.coli.votes <- matrix(NA,numsim,528)
shift.lali.votes <- matrix(NA,numsim,528)
for (i in 1:N){
  shift.cola.votes[,i] <- b.incent.cola[1:numsim]*incent.cola[i]*spdf.england$totvt01[i] - b.incent.laco[1:numsim]*incent.laco[i]*spdf.england$totvt01[i]
  shift.coli.votes[,i] <- b.incent.coli[1:numsim]*incent.coli[i]*spdf.england$totvt01[i] - b.incent.lico[1:numsim]*incent.lico[i]*spdf.england$totvt01[i]
  shift.lali.votes[,i] <- b.incent.lali[1:numsim]*incent.lali[i]*spdf.england$totvt01[i] - b.incent.lila[1:numsim]*incent.lila[i]*spdf.england$totvt01[i]
}

shift.cola.votes.mean <- NULL
shift.cola.votes.95lo <- NULL
shift.cola.votes.95hi <- NULL
shift.coli.votes.mean <- NULL
shift.coli.votes.95lo <- NULL
shift.coli.votes.95hi <- NULL
shift.lali.votes.mean <- NULL
shift.lali.votes.95lo <- NULL
shift.lali.votes.95hi <- NULL
for (i in 1:N) {
  shift.cola.votes.mean[i] <- mean(shift.cola.votes[,i], na.rm=T)
  shift.cola.votes.95lo[i] <- quantile(shift.cola.votes[,i], na.rm=T, probs=c(.025))
  shift.cola.votes.95hi[i] <- quantile(shift.cola.votes[,i], na.rm=T, probs=c(.975))
  shift.coli.votes.mean[i] <- mean(shift.coli.votes[,i], na.rm=T)
  shift.coli.votes.95lo[i] <- quantile(shift.coli.votes[,i], na.rm=T, probs=c(.025))
  shift.coli.votes.95hi[i] <- quantile(shift.coli.votes[,i], na.rm=T, probs=c(.975))
  shift.lali.votes.mean[i] <- mean(shift.lali.votes[,i], na.rm=T)
  shift.lali.votes.95lo[i] <- quantile(shift.lali.votes[,i], na.rm=T, probs=c(.025))
  shift.lali.votes.95hi[i] <- quantile(shift.lali.votes[,i], na.rm=T, probs=c(.975))
}

# vote shares under non-strategic voting
voteshare01.conser.nostrategic <-  matrix(NA,numsim,528)
voteshare01.labour.nostrategic <-  matrix(NA,numsim,528)
voteshare01.libdem.nostrategic <-  matrix(NA,numsim,528)
voteshare01.other.nostrategic <-   matrix(NA,numsim,528)
for (i in 1:N){
  voteshare01.conser.nostrategic[,i] <- voteshare01.conser[i] - shift.conser[,i]
  voteshare01.labour.nostrategic[,i] <- voteshare01.labour[i] - shift.labour[,i]
  voteshare01.libdem.nostrategic[,i] <- voteshare01.libdem[i] - shift.libdem[,i]
  voteshare01.other.nostrategic[,i] <- 1 - (voteshare01.conser.nostrategic[,i] + voteshare01.labour.nostrategic[,i] + voteshare01.libdem.nostrategic[,i])
}

# winners under non-strategic voting
winner01.nostrategic <-   matrix(NA,numsim,528)
for (i in 1:N){
  winner01.nostrategic[,i] <-
    ifelse((voteshare01.conser.nostrategic[,i] > voteshare01.labour.nostrategic[,i]) & (voteshare01.conser.nostrategic[,i] > voteshare01.libdem.nostrategic[,i]) & (voteshare01.conser.nostrategic[,i] > voteshare01.other.nostrategic[,i]), 1,
           ifelse((voteshare01.labour.nostrategic[,i] > voteshare01.conser.nostrategic[,i]) & (voteshare01.labour.nostrategic[,i] > voteshare01.libdem.nostrategic[,i]) & (voteshare01.labour.nostrategic[,i] > voteshare01.other.nostrategic[,i]), 2, 
                  ifelse((voteshare01.libdem.nostrategic[,i] > voteshare01.conser.nostrategic[,i]) & (voteshare01.libdem.nostrategic[,i] > voteshare01.labour.nostrategic[,i]) & (voteshare01.libdem.nostrategic[,i] > voteshare01.other.nostrategic[,i]), 3, 6)))
}

# seconds under non-strategic voting
second01.nostrategic <-   matrix(NA,numsim,528)
for (i in 1:N){
  second01.nostrategic[,i] <-
    ifelse(
      ((voteshare01.conser.nostrategic[,i] < voteshare01.labour.nostrategic[,i]) & (voteshare01.conser.nostrategic[,i] > voteshare01.libdem.nostrategic[,i]) & (voteshare01.conser.nostrategic[,i] > voteshare01.other.nostrategic[,i])) |  # labour, conser, libdem, other or labour, conser, other, libdem
        ((voteshare01.conser.nostrategic[,i] > voteshare01.labour.nostrategic[,i]) & (voteshare01.conser.nostrategic[,i] < voteshare01.libdem.nostrategic[,i]) & (voteshare01.conser.nostrategic[,i] > voteshare01.other.nostrategic[,i])) |  # libdem, conser, labour, other or libdem, conser, other, labour
        ((voteshare01.conser.nostrategic[,i] > voteshare01.labour.nostrategic[,i]) & (voteshare01.conser.nostrategic[,i] > voteshare01.libdem.nostrategic[,i]) & (voteshare01.conser.nostrategic[,i] < voteshare01.other.nostrategic[,i])) , 1, # other, conser, labour, libdem or other, conser, libdem, labour
      ifelse(
        ((voteshare01.labour.nostrategic[,i] < voteshare01.conser.nostrategic[,i]) & (voteshare01.labour.nostrategic[,i] > voteshare01.libdem.nostrategic[,i]) & (voteshare01.labour.nostrategic[,i] > voteshare01.other.nostrategic[,i])) | # conser, labour, libdem, other or conser, labour, other, libdem
          ((voteshare01.labour.nostrategic[,i] > voteshare01.conser.nostrategic[,i]) & (voteshare01.labour.nostrategic[,i] < voteshare01.libdem.nostrategic[,i]) & (voteshare01.labour.nostrategic[,i] > voteshare01.other.nostrategic[,i])) | # libdem, labour, conser, other or libdem, labour, other, conser   
          ((voteshare01.labour.nostrategic[,i] > voteshare01.conser.nostrategic[,i]) & (voteshare01.labour.nostrategic[,i] > voteshare01.libdem.nostrategic[,i]) & (voteshare01.labour.nostrategic[,i] < voteshare01.other.nostrategic[,i]))	, 2,  # other, labour, conser, libdem or other, labour, libdem, conser 	
        ifelse(
          ((voteshare01.libdem.nostrategic[,i] < voteshare01.conser.nostrategic[,i]) & (voteshare01.libdem.nostrategic[,i] > voteshare01.labour.nostrategic[,i]) & (voteshare01.libdem.nostrategic[,i] > voteshare01.other.nostrategic[,i])) | # conser, libdem, labour, other or conser, libdem, other, labour
            ((voteshare01.libdem.nostrategic[,i] > voteshare01.conser.nostrategic[,i]) & (voteshare01.libdem.nostrategic[,i] < voteshare01.labour.nostrategic[,i]) & (voteshare01.libdem.nostrategic[,i] > voteshare01.other.nostrategic[,i])) | # labour, libdem, conser, other or labour, libdem, other, conser	
            ((voteshare01.libdem.nostrategic[,i] > voteshare01.conser.nostrategic[,i]) & (voteshare01.libdem.nostrategic[,i] > voteshare01.labour.nostrategic[,i]) & (voteshare01.libdem.nostrategic[,i] < voteshare01.other.nostrategic[,i])) # other, libdem, labour, conser or other, libdem, conser, labour	
          , 3, 6)))
}


# net margins under non-strategic voting
# vote share of actual winner under non-strategic voting  - vote share of best other party under non-strategic voting
net.margin.share <-   matrix(NA,numsim,528)
net.margin.votes <- matrix(NA,numsim,528)
for (i in 1:N){
  for (j in 1:numsim) {
    if (winner01[i]==1) { net.margin.share[j,i] <- voteshare01.conser.nostrategic[j,i] - max(voteshare01.labour.nostrategic[j,i], voteshare01.libdem.nostrategic[j,i], voteshare01.other.nostrategic[j,i]) }
    else if (winner01[i]==2) { net.margin.share[j,i] <- voteshare01.labour.nostrategic[j,i] - max(voteshare01.conser.nostrategic[j,i], voteshare01.libdem.nostrategic[j,i], voteshare01.other.nostrategic[j,i]) }
    else if (winner01[i]==3) { net.margin.share[j,i] <- voteshare01.libdem.nostrategic[j,i] - max(voteshare01.conser.nostrategic[j,i], voteshare01.labour.nostrategic[j,i], voteshare01.other.nostrategic[j,i]) }
    else net.margin.share[j,i] <- voteshare01.other.nostrategic[j,i] - max(voteshare01.conser.nostrategic[j,i], voteshare01.labour.nostrategic[j,i], voteshare01.libdem.nostrategic[j,i]) 
  }
  net.margin.votes[,i] <- net.margin.share[,i]*spdf.england$totvt01[i]
}

net.margin.mean <- NULL
net.margin.votes.mean <- NULL
net.margin.votes.95lo <- NULL
net.margin.votes.95hi <- NULL
net.margin.votes.80lo <- NULL
net.margin.votes.80hi <- NULL
for (i in 1:N) {
  net.margin.mean[i] <- mean(net.margin.share[,i], na.rm=T)
  net.margin.votes.mean[i] <- mean(net.margin.votes[,i], na.rm=T)
  net.margin.votes.95lo[i] <- quantile(net.margin.votes[,i], na.rm=T, probs=c(.025))
  net.margin.votes.95hi[i] <- quantile(net.margin.votes[,i], na.rm=T, probs=c(.975))
  net.margin.votes.80lo[i] <- quantile(net.margin.votes[,i], na.rm=T, probs=c(.1))
  net.margin.votes.80hi[i] <- quantile(net.margin.votes[,i], na.rm=T, probs=c(.9))
}


# table actual winners vs. predicted winners under non-strategic voting
winner01.nostrategic.modes  <- NULL
for (i in 1:N){
  winner01.nostrategic.modes[i] <- as.numeric(names(sort(-table(winner01.nostrategic[,i])))[1])
}
table01 <- table(winner01,winner01.nostrategic.modes)

