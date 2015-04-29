#----------------------------------------------------------------
# Determining the impact of strategic voting on election results
# Michael Herrmann, Simon Munzert, Peter Selb
# 2015
#----------------------------------------------------------------


## load packages and functions -------------------
source("packages.r")
source("functions.r")


## load workspace
load("../data/data_strategic_uk9701.RData")



### 1997

## list of neighbors for WinBUGS
nb.districts <- poly2nb(spdf.full.97)
nb <- unlist(nb.districts)
weight <- rep(1, times=length(nb))
num <- card(nb.districts)
spdata <- list("nb","weight", "num")

## dependent variables for preference model
y.conser <- ifelse(df.survey.97$partyid2==1,1,0)
y.labour <- ifelse(df.survey.97$partyid2==2,1,0)
y.libdem <- ifelse(df.survey.97$partyid2==3,1,0)
y.conser[df.survey.97$partyid3==12 | df.survey.97$partyid3==13] <- 1 # also set to 1 if equal preference applies
y.labour[df.survey.97$partyid3==12 | df.survey.97$partyid3==23] <- 1 
y.libdem[df.survey.97$partyid3==13 | df.survey.97$partyid3==23] <- 1 

## dependent variable for strategic voting model
y <- cbind(spdf.england$con97/100,spdf.england$lab97/100,spdf.england$ld97/100)

## independent variables
loginvdistsize <- log((spdf.england$area/1000)^-1) - (mean(log((spdf.england$area/1000)^-1))) # area size = log inverse district size
pension <- spdf.england$pension01 - mean(spdf.england$pension01) # Percentage of retired pop, 2001 census
migrant <- spdf.england$migrant01 - mean(spdf.england$migrant01) # Percentage of migrants, 2001 census 
working <- spdf.england$working01  - mean(spdf.england$working01) # Percentage of working age population employed, 2001 census 
region <- spdf.england$region # region variable
# Incumbent
df.constituency.data.england <- df.constituency.data[df.constituency.data$shapefile_id2<=528 & !is.na(df.constituency.data$shapefile_id2),]
incumb97.conser <- ifelse(df.constituency.data.england$coninc97==1,1,0)
incumb97.labour <- ifelse(df.constituency.data.england$labinc97==1,1,0)
incumb97.libdem <- ifelse(df.constituency.data.england$ldpinc97==1,1,0)
incumb97.conser[is.na(incumb97.conser)] <- 0
incumb97.labour[is.na(incumb97.labour)] <- 0
incumb97.libdem[is.na(incumb97.libdem)] <- 0
incumbent.conser <- incumb97.conser
incumbent.labour <- incumb97.labour
incumbent.libdem <- incumb97.libdem
turnout <- df.constituency.data.england$turn97 - mean(df.constituency.data.england$turn97) # Turnout
vip.conser <- ifelse(df.constituency.data.england$convip97==1,1,0) # Prominent candidates
vip.labour <- ifelse(df.constituency.data.england$labvip97==1,1,0)
vip.libdem <- ifelse(df.constituency.data.england$ldpvip97==1,1,0)


## index variables
N <- length(spdf.full.97) # No. of districts
ns <- length(spdf.full.97$nobs[!is.na(spdf.full.97$nobs)]) # No. of non-missing districts
s <- spdf.full.97$shapefile_id2[!is.na(spdf.full.97$nobs)]  # IDs of non-missing districts
n <- spdf.full.97$nobs[!is.na(spdf.full.97$nobs)] # No. of obs. per district (non-miss. districts only; note that districts with wk.right.n!=0 have to be excluded manually here)
temp.dist.obs <- spdf.full.97$nobs
temp.dist.obs <- temp.dist.obs[(is.na(temp.dist.obs))==F]
temp.cumn <-  cumsum(temp.dist.obs)
cumn <- c(0,temp.cumn[1:(length(temp.cumn)-1)]) # Cumulative no. of obs. per district (non-miss. districts only)
N.region <- 9 # No. of regions
P <- 3 # No. of parties
u.conser = ifelse(!is.na(spdf.full.97$nobs), NA, 0) # Idiosyncratic district effect
u.labour = ifelse(!is.na(spdf.full.97$nobs), NA, 0) 
u.libdem = ifelse(!is.na(spdf.full.97$nobs), NA, 0) 



## Strategic incentive (from previous vote shares)
attach(df.constituency.data.england)
prevvote97.conser <- con92/(con92 + lab92 + ld92)
prevvote97.labour <- lab92/(con92 + lab92 + ld92)
prevvote97.libdem <- ld92/(con92 + lab92 + ld92)
detach(df.constituency.data.england)

# 1997 pre-election polling information
# gathered from http://ukpollingreport.co.uk/historical-polls/voting-intention-1992-1997
library(XML)
polls97.table <- readHTMLTable("../data/pollstable97.htm", which = 1, colClasses=c("character","character", "numeric", "numeric", "numeric"))
names(polls97.table) <- c("institute", "date", "con", "lab", "lib", "conlead")
polls97.table <- polls97.table[-1,]
polls97.table.1week <- polls97.table[as.Date(polls97.table$date) >= "1997-04-24",]
polls97.con <- mean(polls97.table.1week$con)/100
polls97.lab <- mean(polls97.table.1week$lab)/100
polls97.lib <- mean(polls97.table.1week$lib)/100
(swing97.con <- mean(c(polls97.con, .419)) - .419)
(swing97.lab <- mean(c(polls97.lab, .344)) +  - .344)
(swing97.lib <- mean(c(polls97.lib, .178)) - .178)

# prior belief
polls97.conser <- prevvote97.conser + swing97.con
polls97.labour <- prevvote97.labour + swing97.lab
polls97.libdem <- prevvote97.libdem + swing97.lib 

## incentiveFun() to generate incentive variable
incentiveFun <- function(Num = 20, prior97.conser = polls97.conser, prior97.labour = polls97.labour, prior97.libdem = polls97.libdem) {
  lowlim <- ceiling(Num/3)
  uplim <- Num/2

  p.cola97.list <- list()
  p.coli97.list <- list()
  p.lali97.list <- list()
  for (i in lowlim:uplim){
  p.cola97.list[[i]] <- exp(lfactorial(Num)-2*lfactorial(i)-lfactorial(Num-2*i)) * prior97.conser^(i) * prior97.labour^(i) * prior97.libdem^(Num-2*i)
  p.coli97.list[[i]] <- exp(lfactorial(Num)-2*lfactorial(i)-lfactorial(Num-2*i)) * prior97.conser^(i) * prior97.labour^(Num-2*i) * prior97.libdem^(i)
  p.lali97.list[[i]] <- exp(lfactorial(Num)-2*lfactorial(i)-lfactorial(Num-2*i)) * prior97.conser^(Num-2*i) * prior97.labour^(i) * prior97.libdem^(i)
  }
  
  p.cola97.matrix <- matrix(NA, nrow=528, ncol = length(lowlim:uplim))
  p.coli97.matrix <- matrix(NA, nrow=528, ncol = length(lowlim:uplim))
  p.lali97.matrix <- matrix(NA, nrow=528, ncol = length(lowlim:uplim))
  for (i in 1:528) {
  for (j in lowlim:uplim) {
  p.cola97.matrix[i,j-(lowlim-1)] <- p.cola97.list[[j]][i]
  p.coli97.matrix[i,j-(lowlim-1)] <- p.coli97.list[[j]][i]
  p.lali97.matrix[i,j-(lowlim-1)] <- p.lali97.list[[j]][i]
  }
  } 
  p.cola97 <- rowSums(p.cola97.matrix)
  p.coli97 <- rowSums(p.coli97.matrix)
  p.lali97 <- rowSums(p.lali97.matrix)
  incentive.cola <- log((2*p.cola97+p.lali97)/(2*p.cola97+p.coli97))
  incentive.coli <- log((2*p.coli97+p.lali97)/(2*p.coli97+p.cola97))
  incentive.lali <- log((2*p.lali97+p.coli97)/(2*p.lali97+p.cola97))
  
  # Generate splitted incentive variable
  incent.cola <<- incentive.cola
  incent.cola[incent.cola<0] <<- 0
  incent.laco <<- - incentive.cola
  incent.laco[incent.laco<0] <<- 0
  incent.coli <<- incentive.coli
  incent.coli[incent.coli<0] <<- 0
  incent.lico <<- - incentive.coli
  incent.lico[incent.lico<0] <<- 0
  incent.lali <<- incentive.lali
  incent.lali[incent.lali<0] <<- 0
  incent.lila <<- - incentive.lali
  incent.lila[incent.lila<0] <<- 0
}

incentiveFun(Num = 20, prior97.conser = prevvote97.conser, prior97.labour = prevvote97.labour, prior97.libdem = prevvote97.libdem)      
incent.cola.prevvote <- incent.cola
incent.laco.prevvote <- incent.laco
incent.coli.prevvote <- incent.coli
incent.lico.prevvote <- incent.lico
incent.lali.prevvote <- incent.lali
incent.lila.prevvote <- incent.lila
#incentiveFun(Num = 20, prior97.conser = polls97.conser, prior97.labour = polls97.labour, prior97.libdem = polls97.libdem)
#incent.cola.polls <- incent.cola
#incent.laco.polls <- incent.laco
#incent.coli.polls <- incent.coli
#incent.lico.polls <- incent.lico
#incent.lali.polls <- incent.lali
#incent.lila.polls <- incent.lila


## save data image
save.image("../data/data_strategic_bayesprep97.RData")



