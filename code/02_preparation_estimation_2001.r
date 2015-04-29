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



### 2001

## list of neighbors for WinBUGS
nb.districts <- poly2nb(spdf.full.01)
nb <- unlist(nb.districts)
weight <- rep(1, times=length(nb))
num <- card(nb.districts)
spdata <- list("nb","weight", "num")

## dependent variables for preference mode
y.conser <- ifelse(df.survey.01$partyid2==1,1,0)
y.labour <- ifelse(df.survey.01$partyid2==2,1,0)
y.libdem <- ifelse(df.survey.01$partyid2==3,1,0)
y.conser[df.survey.01$partyid3==12 | df.survey.01$partyid3==13] <- 1 # also set to 1 if equal preference applies
y.labour[df.survey.01$partyid3==12 | df.survey.01$partyid3==23] <- 1 
y.libdem[df.survey.01$partyid3==13 | df.survey.01$partyid3==23] <- 1 

## dependent variable in strategic voting model
y <- cbind(spdf.england$con01/100,spdf.england$lab01/100,spdf.england$ld01/100)

## independent variables
loginvdistsize <- log((spdf.england$area/1000)^-1) - (mean(log((spdf.england$area/1000)^-1))) # area size = log inverse district size
pension <- spdf.england$pension01 - mean(spdf.england$pension01) # Percentage of retired pop, 2001 census
migrant <- spdf.england$migrant01 - mean(spdf.england$migrant01) # Percentage of migrants, 2001 census 
working <- spdf.england$working01  - mean(spdf.england$working01) # Percentage of working age population employed, 2001 census 
region <- spdf.england$region # region variable
# Incumbent
df.constituency.data.england <- df.constituency.data[df.constituency.data$shapefile_id2<=528 & !is.na(df.constituency.data$shapefile_id2),]
incumb01.conser <- ifelse(df.constituency.data.england$conmp01==1,1,0)
incumb01.labour <- ifelse(df.constituency.data.england$labmp01==1,1,0)
incumb01.libdem <- ifelse(df.constituency.data.england$ldmp01==1,1,0)
incumb01.conser[is.na(incumb01.conser)] <- 0
incumb01.labour[is.na(incumb01.labour)] <- 0
incumb01.libdem[is.na(incumb01.libdem)] <- 0
incumbent.conser <- incumb01.conser
incumbent.labour <- incumb01.labour
incumbent.libdem <- incumb01.libdem
turnout <- df.constituency.data.england$turn01 - mean(df.constituency.data.england$turn01) # Turnout
vip.conser <- ifelse(df.constituency.data.england$convip01==1,1,0) # Prominent candidates
vip.labour <- ifelse(df.constituency.data.england$labvip01==1,1,0)
vip.libdem <- ifelse(df.constituency.data.england$ldpvip01==1,1,0)

## index variables
N <- length(spdf.full.01) # No. of districts
ns <- length(spdf.full.01$nobs[!is.na(spdf.full.01$nobs)]) # No. of non-missing districts
s <- spdf.full.01$shapefile_id2[!is.na(spdf.full.01$nobs)]  # IDs of non-missing districts
n <- spdf.full.01$nobs[!is.na(spdf.full.01$nobs)] # No. of obs. per district (non-miss. districts only; note that districts with wk.right.n!=0 have to be excluded manually here)
temp.dist.obs <- spdf.full.01$nobs
temp.dist.obs <- temp.dist.obs[(is.na(temp.dist.obs))==F]
temp.cumn <-  cumsum(temp.dist.obs)
cumn <- c(0,temp.cumn[1:(length(temp.cumn)-1)]) # Cumulative no. of obs. per district (non-miss. districts only)
N.region <- 9 # No. of regions
P <- 3 # No. of parties
u.conser = ifelse(!is.na(spdf.full.01$nobs), NA, 0) # Idiosyncratic district effect
u.labour = ifelse(!is.na(spdf.full.01$nobs), NA, 0) 
u.libdem = ifelse(!is.na(spdf.full.01$nobs), NA, 0) 


## Strategic incentive (from previous vote shares)
attach(df.constituency.data.england)
con97 <- ifelse(is.na(con97),0.5,con97)
lab97 <- ifelse(is.na(lab97),0.5,lab97)
ld97 <- ifelse(is.na(ld97),0.5,ld97)
prevvote01.conser <- con97/(con97 + lab97 + ld97)
prevvote01.labour <- lab97/(con97 + lab97 + ld97)
prevvote01.libdem <- ld97/(con97 + lab97 + ld97)
detach(df.constituency.data.england)

# 2001 pre-election polling information
# gathered from http://ukpollingreport.co.uk/historical-polls/voting-intention-1997-2001
library(XML)
library(lubridate)
polls01.table <- readHTMLTable("../data/pollstable01.htm", which = 1, colClasses=c("character","character", "numeric", "numeric", "numeric"))
names(polls01.table) <- c("institute", "date", "con", "lab", "lib", "conlead")
polls01.table <- polls01.table[-1,]
polls01.table.1week <- polls01.table[dmy(polls01.table$date) >= "2001-05-31" & dmy(polls01.table$date) < "2002-01-01" & !is.na(dmy(polls01.table$date)),]
(polls01.con <- mean(polls01.table.1week$con)/100)
(polls01.lab <- mean(polls01.table.1week$lab)/100)
(polls01.lib <- mean(polls01.table.1week$lib)/100)
(swing01.con <- mean(c(polls01.con, .307)) - .307)
(swing01.lab <- mean(c(polls01.lab, .407)) +  - .407)
(swing01.lib <- mean(c(polls01.lib, .168)) - .168)

# prior belief
polls01.conser <- prevvote01.conser + swing01.con
polls01.labour <- prevvote01.labour + swing01.lab
polls01.libdem <- prevvote01.libdem + swing01.lib 

## incentiveFun() to generate incentive variable
incentiveFun <- function(Num = 20, prior01.conser = polls01.conser, prior01.labour = polls01.labour, prior01.libdem = polls01.libdem) {
  lowlim <- ceiling(Num/3)
  uplim <- Num/2
  
  p.cola01.list <- list()
  p.coli01.list <- list()
  p.lali01.list <- list()
  for (i in lowlim:uplim){
  p.cola01.list[[i]] <- exp(lfactorial(Num)-2*lfactorial(i)-lfactorial(Num-2*i)) * prior01.conser^(i) * prior01.labour^(i) * prior01.libdem^(Num-2*i)
  p.coli01.list[[i]] <- exp(lfactorial(Num)-2*lfactorial(i)-lfactorial(Num-2*i)) * prior01.conser^(i) * prior01.labour^(Num-2*i) * prior01.libdem^(i)
  p.lali01.list[[i]] <- exp(lfactorial(Num)-2*lfactorial(i)-lfactorial(Num-2*i)) * prior01.conser^(Num-2*i) * prior01.labour^(i) * prior01.libdem^(i)
  }
  
  p.cola01.matrix <- matrix(NA, nrow=528, ncol = length(lowlim:uplim))
  p.coli01.matrix <- matrix(NA, nrow=528, ncol = length(lowlim:uplim))
  p.lali01.matrix <- matrix(NA, nrow=528, ncol = length(lowlim:uplim))
  for (i in 1:528) {
  for (j in lowlim:uplim) {
  p.cola01.matrix[i,j-(lowlim-1)] <- p.cola01.list[[j]][i]
  p.coli01.matrix[i,j-(lowlim-1)] <- p.coli01.list[[j]][i]
  p.lali01.matrix[i,j-(lowlim-1)] <- p.lali01.list[[j]][i]
  }
  } 
  p.cola01 <- rowSums(p.cola01.matrix)
  p.coli01 <- rowSums(p.coli01.matrix)
  p.lali01 <- rowSums(p.lali01.matrix)
  incentive.cola <- log((2*p.cola01+p.lali01)/(2*p.cola01+p.coli01))
  incentive.coli <- log((2*p.coli01+p.lali01)/(2*p.coli01+p.cola01))
  incentive.lali <- log((2*p.lali01+p.coli01)/(2*p.lali01+p.cola01))
  
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

incentiveFun(Num = 20, prior01.conser = prevvote01.conser, prior01.labour = prevvote01.labour, prior01.libdem = prevvote01.libdem)      
incent.cola.prevvote <- incent.cola
incent.laco.prevvote <- incent.laco
incent.coli.prevvote <- incent.coli
incent.lico.prevvote <- incent.lico
incent.lali.prevvote <- incent.lali
incent.lila.prevvote <- incent.lila
# incentiveFun(Num = 20, prior01.conser = polls01.conser, prior01.labour = polls01.labour, prior01.libdem = polls01.libdem)
# incent.cola.polls <- incent.cola
# incent.laco.polls <- incent.laco
# incent.coli.polls <- incent.coli
# incent.lico.polls <- incent.lico
# incent.lali.polls <- incent.lali
# incent.lila.polls <- incent.lila


## save data image
save.image("../data/data_strategic_bayesprep01.RData")


