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

# Create list of neighbors for WinBUGS
nb.districts <- poly2nb(spdf.full.97)
nb <- unlist(nb.districts)
weight <- rep(1, times=length(nb))
num <- card(nb.districts)
spdata <- list("nb","weight", "num")
# Dependent variables
y.conser <- ifelse(df.survey.97$partyvote==1,1,0)
summary(y.conser)
y.labour <- ifelse(df.survey.97$partyvote==2,1,0)
summary(y.labour)
y.libdem <- ifelse(df.survey.97$partyvote==3,1,0)
summary(y.libdem)
# Independent variables
loginvdistsize <- log((spdf.england$area/1000)^-1) - (mean(log((spdf.england$area/1000)^-1))) # area size = log inverse district size
pension <- spdf.england$pension01 - mean(spdf.england$pension01) # Percentage of retired pop, 2001 census
migrant <- spdf.england$migrant01 - mean(spdf.england$migrant01) # Percentage of migrants, 2001 census 
working <- spdf.england$working01  - mean(spdf.england$working01) # Percentage of working age population employed, 2001 census 
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
# region variable
region <- spdf.england$region
# No. of districts
N <- length(spdf.full.97)
# No. of non-missing districts
ns <- length(spdf.full.97$nobs[!is.na(spdf.full.97$nobs)])
# IDs of non-missing districts
s <- spdf.full.97$shapefile_id2[!is.na(spdf.full.97$nobs)] 
# No. of obs. per district (non-miss. districts only; note that districts with wk.right.n!=0 have to be excluded manually here)
n <- spdf.full.97$nobs[!is.na(spdf.full.97$nobs)]
# Cumulative no. of obs. per district (non-miss. districts only)
temp.dist.obs <- spdf.full.97$nobs
temp.dist.obs <- temp.dist.obs[(is.na(temp.dist.obs))==F]
temp.cumn <-  cumsum(temp.dist.obs)
cumn <- c(0,temp.cumn[1:(length(temp.cumn)-1)])
# No. of regions
N.region <- 9
# Idiosyncratic district effect
u.conser = ifelse(!is.na(spdf.full.97$nobs), NA, 0)
u.labour = ifelse(!is.na(spdf.full.97$nobs), NA, 0) 
u.libdem = ifelse(!is.na(spdf.full.97$nobs), NA, 0) 

# Data and model
data1 <- list("N", "y.conser", "y.labour", "y.libdem", "ns", "s", "n", "cumn", "u.conser", "u.labour", "u.libdem", "loginvdistsize", "pension", "working", "region", "N.region",  "incumbent.conser", "incumbent.labour", "incumbent.libdem")
model.full <- paste(getwd(), "../bugs_simulations/model2b.txt", sep = "")

# Inits
inits1 <- source(paste(getwd(), "../bugs_simulations/model2b_97_inits.txt", sep = ""))$value

# Parameters to save
parameters.to.save <- source(paste(getwd(), "../bugs_simulations/model2b_params.txt", sep = ""))$value

# Run MCMC
model1.sim <- bugs(data=c(data1, spdata), model.file=model.full, inits=inits1, parameters.to.save=parameters.to.save, n.chains=3, n.iter=20000, n.burnin=10000, n.thin=5, bugs.directory="C:/WinBUGS14", debug=T) 

# Save model workspace
save(model1.sim, file = "../bugs_simulations/model2b_vote97.RData")




### 2001

# Create list of neighbors for WinBUGS
nb.districts <- poly2nb(spdf.full.01)
nb <- unlist(nb.districts)
weight <- rep(1, times=length(nb))
num <- card(nb.districts)
spdata <- list("nb","weight", "num")
# Dependent variables
y.conser <- ifelse(df.survey.01$partyvote==1,1,0)
summary(y.conser)
y.labour <- ifelse(df.survey.01$partyvote==2,1,0)
summary(y.labour)
y.libdem <- ifelse(df.survey.01$partyvote==3,1,0)
summary(y.libdem)
# Independent variables
loginvdistsize <- log((spdf.england$area/1000)^-1) - (mean(log((spdf.england$area/1000)^-1))) # area size = log inverse district size
pension <- spdf.england$pension01 - mean(spdf.england$pension01) # Percentage of retired pop, 2001 census
migrant <- spdf.england$migrant01 - mean(spdf.england$migrant01) # Percentage of migrants, 2001 census 
working <- spdf.england$working01  - mean(spdf.england$working01) # Percentage of working age population employed, 2001 census
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
# region variable
region <- spdf.england$region
# No. of districts
N <- length(spdf.full.01)
# No. of non-missing districts
ns <- length(spdf.full.01$nobs[!is.na(spdf.full.01$nobs)])
# IDs of non-missing districts
s <- spdf.full.01$shapefile_id2[!is.na(spdf.full.01$nobs)] 
# No. of obs. per district (non-miss. districts only; note that districts with wk.right.n!=0 have to be excluded manually here)
n <- spdf.full.01$nobs[!is.na(spdf.full.01$nobs)]
# Cumulative no. of obs. per district (non-miss. districts only)
temp.dist.obs <- spdf.full.01$nobs
temp.dist.obs <- temp.dist.obs[(is.na(temp.dist.obs))==F]
temp.cumn <-  cumsum(temp.dist.obs)
cumn <- c(0,temp.cumn[1:(length(temp.cumn)-1)])
# No. of regions
N.region <- 9
# Idiosyncratic district effect
u.conser = ifelse(!is.na(spdf.full.01$nobs), NA, 0)
u.labour = ifelse(!is.na(spdf.full.01$nobs), NA, 0) 
u.libdem = ifelse(!is.na(spdf.full.01$nobs), NA, 0) 

# Data and model
data1 <- list("N", "y.conser", "y.labour", "y.libdem", "ns", "s", "n", "cumn", "u.conser", "u.labour", "u.libdem", "loginvdistsize", "pension", "working", "region", "N.region",  "incumbent.conser", "incumbent.labour", "incumbent.libdem")
model.full <- paste(getwd(), "../bugs_simulations/model2b.txt", sep = "")

# Inits
inits1 <- source(paste(getwd(), "../bugs_simulations/model2b_01_inits.txt", sep = ""))$value

# Parameters to save
parameters.to.save <- source(paste(getwd(), "../bugs_simulations/model2b_params.txt", sep = ""))$value

# Run MCMC
model1.sim <- bugs(data=c(data1, spdata), model.file=model.full, inits=inits1, parameters.to.save=parameters.to.save, n.chains=3, n.iter=20000, n.burnin=10000, n.thin=5, bugs.directory="C:/WinBUGS14", debug=T) 


# Save model workspace
save(model1.sim, file = "../bugs_simulations/model2b_vote01.RData")






### analyze estimation results

## load data - load sequentially!
load("../data/data_strategic_uk9701.RData")
load("../bugs_simulations/model2b_vote97.RData")
load("../bugs_simulations/model2b_vote01.RData")

N <- 528
attach.bugs(model1.sim)
car.mu.conser <- 0
car.90lo.conser <- 0
car.90hi.conser <- 0
car.mu.labour <- 0
car.90lo.labour <- 0
car.90hi.labour <- 0
car.mu.libdem <- 0
car.90lo.libdem <- 0
car.90hi.libdem <- 0
for (i in 1:N){
car.mu.conser[i] <- median(mu.conser[,i])
car.90lo.conser[i] <- quantile(mu.conser[,i],probs=c(.05))
car.90hi.conser[i] <- quantile(mu.conser[,i],probs=c(.95))
car.mu.labour[i] <- median(mu.labour[,i])
car.90lo.labour[i] <- quantile(mu.labour[,i],probs=c(.05))
car.90hi.labour[i] <- quantile(mu.labour[,i],probs=c(.95)) 
car.mu.libdem[i] <- median(mu.libdem[,i])
car.90lo.libdem[i] <- quantile(mu.libdem[,i],probs=c(.05))
car.90hi.libdem[i] <- quantile(mu.libdem[,i],probs=c(.95)) 
}
detach.bugs()

spdf.full.97$vote.conser97.est <- car.mu.conser
spdf.full.97$vote.conser97.est.90lo <- car.90lo.conser
spdf.full.97$vote.conser97.est.90hi <- car.90hi.conser
spdf.full.97$vote.labour97.est <- car.mu.labour
spdf.full.97$vote.labour97.est.90lo <- car.90lo.labour
spdf.full.97$vote.labour97.est.90hi <- car.90hi.labour
spdf.full.97$vote.libdem97.est <- car.mu.libdem
spdf.full.97$vote.libdem97.est.90lo <- car.90lo.libdem
spdf.full.97$vote.libdem97.est.90hi <- car.90hi.libdem

spdf.full.01$vote.conser01.est <- car.mu.conser
spdf.full.01$vote.conser01.est.90lo <- car.90lo.conser
spdf.full.01$vote.conser01.est.90hi <- car.90hi.conser
spdf.full.01$vote.labour01.est <- car.mu.labour
spdf.full.01$vote.labour01.est.90lo <- car.90lo.labour
spdf.full.01$vote.labour01.est.90hi <- car.90hi.labour
spdf.full.01$vote.libdem01.est <- car.mu.libdem
spdf.full.01$vote.libdem01.est.90lo <- car.90lo.libdem
spdf.full.01$vote.libdem01.est.90hi <- car.90hi.libdem

spdf.england$con97 <- spdf.england$con97/100
spdf.england$lab97 <- spdf.england$lab97/100
spdf.england$ld97  <- spdf.england$ld97/100
spdf.england$con01 <- spdf.england$con01/100
spdf.england$lab01 <- spdf.england$lab01/100
spdf.england$ld01  <- spdf.england$ld01/100


## Moran's I Plot, election results
nb.districts <- poly2nb(spdf.full.97)
W.districts <- nb2listw(nb.districts, style="W")

pdf(file="../figures/figure_app_moran_plot_voteshares.pdf", height=9, width=12, family="URWTimes")
par(mar=c(3.5,3.5,3.5,1))
par(oma=c(2,3,3,1))
par(mfrow=c(2,3), pty = "s")

x <- spdf.england$con97
x[is.na(x)] <- 0
wx <- lag.listw(W.districts, x, zero.policy = T)
moran.estimate <- moran.test(x, nb2listw(nb.districts, style="W"))
morans.i <-moran.estimate$estimate[1]
plot(x, wx, ylim=c(0,.8), xlim=c(0,.8), axes=F,ylab="", xlab="")
	xwx.lm <- lm(wx ~ x)
	abline(xwx.lm)
	abline(h = mean(wx), lty = 2)
	abline(v = mean(x), lty = 2)
	title(main="Con 1997", cex.main=2.5, line=2)
	mtext("Spatial Lag", side=2, outer=F, line=4, cex=1.5)
	text(0, .75 , paste("Moran's I =", format(morans.i, digits=2)), pos=4, cex=1.5)
	axis(1, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	axis(2, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	box()
x <- spdf.england$lab97
x[is.na(x)] <- 0
wx <- lag.listw(W.districts, x, zero.policy = T)
moran.estimate <- moran.test(x, nb2listw(nb.districts, style="W"))
morans.i <-moran.estimate$estimate[1]
plot(x, wx, ylim=c(0,.8), xlim=c(0,.8), axes=F,ylab="", xlab="")
	xwx.lm <- lm(wx ~ x)
	abline(xwx.lm)
	abline(h = mean(wx), lty = 2)
	abline(v = mean(x), lty = 2)
	title(main="Lab 1997", cex.main=2.5, line=2)
	text(0, .75 , paste("Moran's I =", format(morans.i, digits=2)), pos=4, cex=1.5)
	axis(1, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	axis(2, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	box()
x <- spdf.england$ld97
x[is.na(x)] <- 0
wx <- lag.listw(W.districts, x, zero.policy = T)
moran.estimate <- moran.test(x, nb2listw(nb.districts, style="W"))
morans.i <-moran.estimate$estimate[1]
plot(x, wx, ylim=c(0,.8), xlim=c(0,.8), axes=F,ylab="", xlab="")
	xwx.lm <- lm(wx ~ x)
	abline(xwx.lm)
	abline(h = mean(wx), lty = 2)
	abline(v = mean(x), lty = 2)
	title(main="Lib 1997", cex.main=2.5, line=2)
	text(.8, .75 , paste("Moran's I =", format(morans.i, digits=2)), pos=2, cex=1.5)
	axis(1, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	axis(2, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	box()
x <- spdf.england$con01
x[is.na(x)] <- 0
wx <- lag.listw(W.districts, x, zero.policy = T)
moran.estimate <- moran.test(x, nb2listw(nb.districts, style="W"))
morans.i <-moran.estimate$estimate[1]
plot(x, wx, ylim=c(0,.8), xlim=c(0,.8), axes=F,ylab="", xlab="")
	xwx.lm <- lm(wx ~ x)
	abline(xwx.lm)
	abline(h = mean(wx), lty = 2)
	abline(v = mean(x), lty = 2)
	title(main="Con 2001", cex.main=2.5, line=2)
	mtext("Spatial Lag", side=2, outer=F, line=4, cex=1.5)
	text(0, .75 , paste("Moran's I =", format(morans.i, digits=2)), pos=4, cex=1.5)
	axis(1, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	axis(2, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	box()
x <- spdf.england$lab01
x[is.na(x)] <- 0
wx <- lag.listw(W.districts, x, zero.policy = T)
moran.estimate <- moran.test(x, nb2listw(nb.districts, style="W"))
morans.i <-moran.estimate$estimate[1]
plot(x, wx, ylim=c(0,.8), xlim=c(0,.8), axes=F,ylab="", xlab="")
	xwx.lm <- lm(wx ~ x)
	abline(xwx.lm)
	abline(h = mean(wx), lty = 2)
	abline(v = mean(x), lty = 2)
	title(main="Lab 2001", cex.main=2.5, line=2)
	mtext("District vote share", side=1, outer=F, line=4, cex=1.5)
	text(0, .75 , paste("Moran's I =", format(morans.i, digits=2)), pos=4, cex=1.5)
	axis(1, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	axis(2, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	box()
x <- spdf.england$ld01
x[is.na(x)] <- 0
wx <- lag.listw(W.districts, x, zero.policy = T)
moran.estimate <- moran.test(x, nb2listw(nb.districts, style="W"))
morans.i <-moran.estimate$estimate[1]
plot(x, wx, ylim=c(0,.8), xlim=c(0,.8), axes=F,ylab="", xlab="")
	xwx.lm <- lm(wx ~ x)
	abline(xwx.lm)
	abline(h = mean(wx), lty = 2)
	abline(v = mean(x), lty = 2)
	title(main="Lib 2001", cex.main=2.5, line=2)
	text(.8, .75 , paste("Moran's I =", format(morans.i, digits=2)), pos=2, cex=1.5)
	axis(1, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	axis(2, at=c(0,.2,.4,.6,.8), labels=T, cex.axis=1.8)
	box()
dev.off()



# Plot: Check the sum of estimated shares
sum.shares97.est <- spdf.full.97$vote.conser97.est + spdf.full.97$vote.labour97.est + spdf.full.97$vote.libdem97.est
shares97.est.over1 <- ifelse(sum.shares97.est>1,1,0)
shares97.est.over1 <- ifelse(sum.shares97.est>1,1,0)
table(shares97.est.over1)
mean97.over1 <- mean(shares97.est.over1, na.rm=T)

sum.shares97 <- spdf.england$con97 + spdf.england$lab97 + spdf.england$ld97
shares97.over1 <- ifelse(sum.shares97>1,1,0)
shares97.over1 <- ifelse(sum.shares97>1,1,0)
table(shares97.over1)

sum.shares01.est <- spdf.full.01$vote.conser01.est + spdf.full.01$vote.labour01.est + spdf.full.01$vote.libdem01.est
shares01.est.over1 <- ifelse(sum.shares01.est>1,1,0)
shares01.est.over1 <- ifelse(sum.shares01.est>1,1,0)
table(shares01.est.over1)
mean01.over1 <- mean(shares01.est.over1, na.rm=T)

sum.shares01 <- spdf.england$con01 + spdf.england$lab01 + spdf.england$ld01
shares01.over1 <- ifelse(sum.shares01>1,1,0)
shares01.over1 <- ifelse(sum.shares01>1,1,0)
table(shares01.over1)

pdf(file="../figures/figure_app_sum_voteshares.pdf", height=8, width=9, family="URWTimes")
par(mar=c(2,2.5,2,1))
par(oma=c(2,2,2,1))
par(mfrow=c(2,2), pty = "s")
plot(sum.shares97, ylim=c(.6, 1.05), xlab="", ylab="Sum of vote shares", xaxt="n", cex.lab=1.5)
	title(main="1997", line=1, cex.main=1.5)
	text(155, .6 , paste("Share of sums over 1 = 0"), pos=4, cex=1.2)
abline(h=1, lty=2, lwd=2, col="red")
plot(sum.shares97.est, ylim=c(.6, 1.05), xlab="", ylab="Sum of estimated vote shares", xaxt="n", cex.lab=1.5)
	title(main="1997", line=1, cex.main=1.5)
	text(155, .6 , paste("Share of sums over 1 =", format(mean97.over1, digits=2)), pos=4, cex=1.2)	

abline(h=1, lty=2, lwd=2, col="red")
plot(sum.shares01, ylim=c(.6, 1.05), xlab="", ylab="Sum of vote shares", xaxt="n", cex.lab=1.5)
	title(main="2001", line=1, cex.main=1.5)
	text(155, .6 , paste("Share of sums over 1 = 0"), pos=4, cex=1.2)	
abline(h=1, lty=2, lwd=2, col="red")
plot(sum.shares01.est, ylim=c(.6, 1.05), ylab="Sum of estimated vote shares", xaxt="n", cex.lab=1.5)
	title(main="2001", line=1, cex.main=1.5)
	text(155, .6 , paste("Share of sums over 1 =", format(mean01.over1, digits=2)), pos=4, cex=1.2)
abline(h=1, lty=2, lwd=2, col="red")
dev.off()

# Check quality of direct estimator
pdf(file="../figures/figure_app_pvt_direct_voteshares.pdf", height=6, width=8, family="URWTimes")
par(mar=c(2,3,2,1))
par(oma=c(2,3,2,1))
par(mfrow=c(2,2), pty = "s")
par(mfrow=c(2,3))
plot(spdf.england$con97,spdf.full.97$v_conser, xlim=c(0,1), ylim=c(0,1))
    abline(v=mean(spdf.england$con97, na.rm=T), lty=2)
    abline(h=mean(spdf.full.97$v_conser, na.rm=T), lty=2)
    axis(3, at=.5, labels="Con", tick=F, cex.axis=1.5)
    axis(2, at=.5, labels="1997", tick=F, cex.axis=1.5, line=3)
    axis(2, at=.5, labels="Predicted values", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(spdf.england$lab97,spdf.full.97$v_labour, xlim=c(0,1), ylim=c(0,1))
    abline(v=mean(spdf.england$lab97, na.rm=T), lty=2)
    abline(h=mean(spdf.full.97$v_labour, na.rm=T), lty=2)
    axis(3, at=.5, labels="Lab", tick=F, cex.axis=1.5)
abline(0,1)
plot(spdf.england$ld97,spdf.full.97$v_libdem, xlim=c(0,1), ylim=c(0,1))
    abline(v=mean(spdf.england$ld97, na.rm=T), lty=2)
    abline(h=mean(spdf.full.97$v_libdem, na.rm=T), lty=2)
    axis(3, at=.5, labels="Lib", tick=F, cex.axis=1.5)
abline(0,1)
plot(spdf.england$con01,spdf.full.01$v_conser, xlim=c(0,1), ylim=c(0,1))
    abline(v=mean(spdf.england$con01, na.rm=T), lty=2)
    abline(h=mean(spdf.full.01$v_conser, na.rm=T), lty=2)
    axis(2, at=.5, labels="2001", tick=F, cex.axis=1.5, line=3)
    axis(2, at=.5, labels="Predicted values", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(spdf.england$lab01,spdf.full.01$v_labour, xlim=c(0,1), ylim=c(0,1))
    abline(v=mean(spdf.england$lab01, na.rm=T), lty=2)
    abline(h=mean(spdf.full.01$v_labour, na.rm=T), lty=2)
    axis(1, at=.5, labels="True values", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(spdf.england$ld01,spdf.full.01$v_libdem, xlim=c(0,1), ylim=c(0,1))
    abline(v=mean(spdf.england$ld01, na.rm=T), lty=2)
    abline(h=mean(spdf.full.01$v_libdem, na.rm=T), lty=2)
abline(0,1)
dev.off()

# Validation Scatterplots: Predicted vs. true voteshare
pdf(file="../figures/figure_app_pvt_estimate_voteshares.pdf", height=6, width=8, family="URWTimes")
par(mar=c(2,3,2,1))
par(oma=c(2,3,2,1))
par(mfrow=c(2,2), pty = "s")
par(mfrow=c(2,3))
plot(spdf.england$con97,spdf.full.97$vote.conser97.est, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.england$con97, na.rm=T), lty=2)
	abline(h=mean(spdf.full.97$vote.conser97.est, na.rm=T), lty=2)
	axis(3, at=.5, labels="Con", tick=F, cex.axis=1.5)
	axis(2, at=.5, labels="1997", tick=F, cex.axis=1.5, line=3)
	axis(2, at=.5, labels="Predicted values", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(spdf.england$lab97,spdf.full.97$vote.labour97.est, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.england$lab97, na.rm=T), lty=2)
	abline(h=mean(spdf.full.97$vote.labour97.est, na.rm=T), lty=2)
	axis(3, at=.5, labels="Lab", tick=F, cex.axis=1.5)
abline(0,1)
plot(spdf.england$ld97,spdf.full.97$vote.libdem97.est, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.england$ld97, na.rm=T), lty=2)
	abline(h=mean(spdf.full.97$vote.libdem97.est, na.rm=T), lty=2)
	axis(3, at=.5, labels="Lib", tick=F, cex.axis=1.5)
abline(0,1)
plot(spdf.england$con01,spdf.full.01$vote.conser01.est, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.england$con01, na.rm=T), lty=2)
	abline(h=mean(spdf.full.01$vote.conser01.est, na.rm=T), lty=2)
	axis(2, at=.5, labels="2001", tick=F, cex.axis=1.5, line=3)
	axis(2, at=.5, labels="Predicted values", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(spdf.england$lab01,spdf.full.01$vote.labour01.est, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.england$lab01, na.rm=T), lty=2)
	abline(h=mean(spdf.full.01$vote.labour01.est, na.rm=T), lty=2)
	axis(1, at=.5, labels="True values", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(spdf.england$ld01,spdf.full.01$vote.libdem01.est, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.england$ld01, na.rm=T), lty=2)
	abline(h=mean(spdf.full.01$vote.libdem01.est, na.rm=T), lty=2)
abline(0,1)
dev.off()


# Validation Scatterplots: Predicted vs. true voteshare, panel
pdf(file="../figures/figure_app_pvt_97_01_voteshares.pdf", height=6, width=8, family="URWTimes")
par(mar=c(2,3,2,1))
par(oma=c(2,3,2,1))
par(mfrow=c(2,2), pty = "s")
par(mfrow=c(2,3))
plot(spdf.england$con97,spdf.england$con01, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.england$con97, na.rm=T), lty=2)
	abline(h=mean(spdf.england$con01, na.rm=T), lty=2)
	axis(3, at=.5, labels="Con", tick=F, cex.axis=1.5)
	axis(2, at=.5, labels="True values", tick=F, cex.axis=1.5, line=3)
	axis(2, at=.5, labels="2001", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(spdf.england$lab97,spdf.england$lab01, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.england$lab97, na.rm=T), lty=2)
	abline(h=mean(spdf.england$lab01, na.rm=T), lty=2)
	axis(3, at=.5, labels="Lab", tick=F, cex.axis=1.5)
abline(0,1)
plot(spdf.england$ld97,spdf.england$ld01, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.england$ld97, na.rm=T), lty=2)
	abline(h=mean(spdf.england$ld01, na.rm=T), lty=2)
	axis(3, at=.5, labels="Lib", tick=F, cex.axis=1.5)
abline(0,1)
plot(spdf.full.97$vote.conser97.est,spdf.full.01$vote.conser01.est, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.full.97$vote.conser97.est, na.rm=T), lty=2)
	abline(h=mean(spdf.full.01$vote.conser01.est, na.rm=T), lty=2)
	axis(2, at=.5, labels="Predicted values", tick=F, cex.axis=1.5, line=3)
	axis(2, at=.5, labels="2001", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(spdf.full.97$vote.labour97.est,spdf.full.01$vote.labour01.est, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.full.97$vote.labour97.est, na.rm=T), lty=2)
	abline(h=mean(spdf.full.01$vote.labour01.est, na.rm=T), lty=2)
	axis(1, at=.5, labels="1997", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(spdf.full.97$vote.libdem97.est,spdf.full.01$vote.libdem01.est, xlim=c(0,1), ylim=c(0,1))
	abline(v=mean(spdf.full.97$vote.libdem97.est, na.rm=T), lty=2)
	abline(h=mean(spdf.full.01$vote.libdem01.est, na.rm=T), lty=2)
abline(0,1)
dev.off()



### Summary table
sumtable <-  as.data.frame(matrix(NA,nrow=6,ncol=4))
colnames(sumtable) <- c("MAE Dir", "MAE Mod", "Mean width of 90% CI","Coverage probabilites")
rownames(sumtable) <- c("Con 97", "Lab 97", "Lib 97", "Con 01", "Lab 01", "Lib 01")

# MAE
sumtable[1,1] <- mean(abs(spdf.full.97$v_conser - spdf.england$con97), na.rm=T)
sumtable[2,1] <- mean(abs(spdf.full.97$v_labour - spdf.england$lab97), na.rm=T)
sumtable[3,1] <- mean(abs(spdf.full.97$v_libdem - spdf.england$ld97), na.rm=T)
sumtable[4,1] <- mean(abs(spdf.full.01$v_conser - spdf.england$con01), na.rm=T)
sumtable[5,1] <- mean(abs(spdf.full.01$v_labour - spdf.england$lab01), na.rm=T)
sumtable[6,1] <- mean(abs(spdf.full.01$v_libdem - spdf.england$ld01), na.rm=T)

sumtable[1,2] <- mean(abs(spdf.full.97$vote.conser97.est - spdf.england$con97), na.rm=T)
sumtable[2,2] <- mean(abs(spdf.full.97$vote.labour97.est - spdf.england$lab97), na.rm=T)
sumtable[3,2] <- mean(abs(spdf.full.97$vote.libdem97.est - spdf.england$ld97), na.rm=T)
sumtable[4,2] <- mean(abs(spdf.full.01$vote.conser01.est - spdf.england$con01), na.rm=T)
sumtable[5,2] <- mean(abs(spdf.full.01$vote.labour01.est - spdf.england$lab01), na.rm=T)
sumtable[6,2] <- mean(abs(spdf.full.01$vote.libdem01.est - spdf.england$ld01), na.rm=T)

# Diminuition of MAE model vs. direct estimates by factor X
sumtable[,1]/sumtable[,2]
mean(sumtable[1:3,1]/sumtable[1:3,2])
mean(sumtable[4:6,1]/sumtable[4:6,2])
mean(sumtable[,1]/sumtable[,2])

# Mean width of 90% CI
sumtable[1,3] <- mean(spdf.full.97$vote.conser97.est.90hi-spdf.full.97$vote.conser97.est.90lo)
sumtable[2,3] <- mean(spdf.full.97$vote.labour97.est.90hi-spdf.full.97$vote.labour97.est.90lo)
sumtable[3,3] <- mean(spdf.full.97$vote.libdem97.est.90hi-spdf.full.97$vote.libdem97.est.90lo)
sumtable[4,3] <- mean(spdf.full.01$vote.conser01.est.90hi-spdf.full.01$vote.conser01.est.90lo)
sumtable[5,3] <- mean(spdf.full.01$vote.labour01.est.90hi-spdf.full.01$vote.labour01.est.90lo)
sumtable[6,3] <- mean(spdf.full.01$vote.libdem01.est.90hi-spdf.full.01$vote.libdem01.est.90lo)

# Coverage probabilites
conser97.cov90 <- spdf.england$con97 <= spdf.full.97$vote.conser97.est.90hi & spdf.england$con97 >= spdf.full.97$vote.conser97.est.90lo
sumtable[1,4] <- conser97.prcov90 <- sum(conser97.cov90, na.rm=T)/N
labour97.cov90 <- spdf.england$lab97 <= spdf.full.97$vote.labour97.est.90hi & spdf.england$lab97 >= spdf.full.97$vote.labour97.est.90lo
sumtable[2,4] <- labour97.prcov90 <- sum(labour97.cov90, na.rm=T)/N
libdem97.cov90 <- spdf.england$ld97 <= spdf.full.97$vote.libdem97.est.90hi & spdf.england$ld97 >= spdf.full.97$vote.libdem97.est.90lo
sumtable[3,4] <- libdem97.prcov90 <- sum(libdem97.cov90, na.rm=T)/N
conser01.cov90 <- spdf.england$con01 <= spdf.full.01$vote.conser01.est.90hi & spdf.england$con01 >= spdf.full.01$vote.conser01.est.90lo
sumtable[4,4] <- conser01.prcov90 <- sum(conser01.cov90, na.rm=T)/N
labour01.cov90 <- spdf.england$lab01 <= spdf.full.01$vote.labour01.est.90hi & spdf.england$lab01 >= spdf.full.01$vote.labour01.est.90lo
sumtable[5,4] <- labour01.prcov90 <- sum(labour01.cov90, na.rm=T)/N
libdem01.cov90 <- spdf.england$ld01 <= spdf.full.01$vote.libdem01.est.90hi & spdf.england$ld01 >= spdf.full.01$vote.libdem01.est.90lo
sumtable[6,4] <- libdem01.prcov90 <- sum(libdem01.cov90, na.rm=T)/N

sumtable



### Check distribution of vote shares / preferences over surveys
pdf(file="../figures/figure_app_studies_voteshares.pdf", height=10, width=8, family="URWTimes")
par(mar=c(2,2,5,1))
par(oma=c(2,2,0,1))
par(mfrow=c(2,1))

df.unit <- rbind(df.survey.97, df.survey.01)
df.unit.sub <- df.unit[!is.na(df.unit$partyvote) & df.unit$partyvote!=9 & df.unit$year<1999,]
counts.tab <- table(as.factor(df.unit.sub$study), as.factor(df.unit.sub$partyvote))
num.studies <- length(unique(df.unit.sub$study))
multiplic <- seq(1,5,1)
highnum <- (num.studies+1)
lab.positions <- multiplic*highnum - (num.studies/2)
counts <- ctab(as.factor(df.unit.sub$study), as.factor(df.unit.sub$partyvote), type="row", percentages=F)
pcounts <- print(counts)
voteshare97.conser <- sum(spdf.england$convt97, na.rm=T)/sum(spdf.england$totvt97, na.rm=T)
voteshare97.labour <- sum(spdf.england$labvt97, na.rm=T)/sum(spdf.england$totvt97, na.rm=T)
voteshare97.libdem <- sum(spdf.england$ldvt97, na.rm=T)/sum(spdf.england$totvt97, na.rm=T)
voteshare97.other <-  sum(spdf.england$totvt97 - spdf.england$convt97 - spdf.england$labvt97 - spdf.england$ldvt97, na.rm=T)/sum(spdf.england$totvt97, na.rm=T)

barplot(pcounts, beside=TRUE, axes=F, ylim=c(0,.55), main="1997")
abline(h=c(0,.1,.2,.3,.4,.5), lty=2)
barplot(pcounts, beside=TRUE, axes=F, col=gray.colors(3), ylim=c(0,.55), add=T,  legend.text=c("BSA 1996", "GES 1997, Cross-Section", "GES 1992-1997 Panel", "True vote share"), 
args.legend = list(x = "topright", fill=c(gray.colors(3),NA), border=c(1,1,1,NA), bg="white", lty=c(NA, NA, NA, 1), pch=c(NA, NA, NA, NA), col=c(NULL, NULL, NULL, "red"), lwd=c(NA, NA, NA, 2))
)
axis(1, at=lab.positions, tick=F, labels=c("Con","Lab","Lib","Other","No vote"))
axis(2, at=c(0,.1,.2,.3,.4,.5), tick=T, labels=c(0,.1,.2,.3,.4,.5))
axis(2, at=.25, labels="Share", tick=F, cex.axis=1.3, line=1.7)
segments(1,voteshare97.conser,4, col="red", lwd=2)
segments(5,voteshare97.labour,8, col="red", lwd=2)
segments(9,voteshare97.libdem,12, col="red", lwd=2)
segments(13,voteshare97.other,16, col="red", lwd=2)

df.unit.sub <- df.unit[!is.na(df.unit$partyvote) & df.unit$partyvote!=9 & df.unit$year>1999,]
counts.tab <- table(as.factor(df.unit.sub$study), as.factor(df.unit.sub$partyvote))
num.studies <- length(unique(df.unit.sub$study))
multiplic <- seq(1,5,1)
highnum <- (num.studies+1)
lab.positions <- multiplic*highnum - (num.studies/2)
counts <- ctab(as.factor(df.unit.sub$study), as.factor(df.unit.sub$partyvote), type="row", percentages=F)
pcounts <- print(counts)
voteshare01.conser <- sum(spdf.england$convt01, na.rm=T)/sum(spdf.england$totvt01, na.rm=T)
voteshare01.labour <- sum(spdf.england$labvt01, na.rm=T)/sum(spdf.england$totvt01, na.rm=T)
voteshare01.libdem <- sum(spdf.england$ldvt01, na.rm=T)/sum(spdf.england$totvt01, na.rm=T)
voteshare01.other <-  sum(spdf.england$totvt01 - spdf.england$convt01 - spdf.england$labvt01 - spdf.england$ldvt01, na.rm=T)/sum(spdf.england$totvt01, na.rm=T)

barplot(pcounts, beside=TRUE, axes=F, ylim=c(0,.55), main="2001")
abline(h=c(0,.1,.2,.3,.4,.5), lty=2)
barplot(pcounts, beside=TRUE, axes=F, col=gray.colors(4), ylim=c(0,.55), add=T,  legend.text=c("BSA 2001", "GES 2001, Cross-Section", "GES 2001, Campaign Panel", "GES 1997-2001 Panel", "True vote share"), 
args.legend = list(x = "topright", fill=c(gray.colors(4),NA), border=c(1,1,1,1,NA), bg="white", lty=c(NA, NA, NA, NA, 1), pch=c(NA, NA, NA, NA, NA), col=c(NULL, NULL, NULL, NULL, "red"), lwd=c(NA, NA, NA, NA, 2))
)
axis(1, at=lab.positions, tick=F, labels=c("Cons","Lab","Lib","Other","No vote"))
axis(2, at=c(0,.1,.2,.3,.4,.5), tick=T, labels=c(0,.1,.2,.3,.4,.5))
axis(2, at=.25, labels="Share", tick=F, cex.axis=1.3, line=1.7)
segments(1,voteshare97.conser,5, col="red", lwd=2)
segments(6,voteshare97.labour,10, col="red", lwd=2)
segments(11,voteshare97.libdem,15, col="red", lwd=2)
segments(16,voteshare97.other,20, col="red", lwd=2)
dev.off()



pdf(file="../figures/figure_app_studies_preferences.pdf", height=10, width=8, family="URWTimes")
par(mar=c(2,2,5,1))
par(oma=c(2,2,2,1))
par(mfrow=c(2,1))
df.unit.sub <- df.unit[!is.na(df.unit$partyid2) & df.unit$vote==1 & !is.na(df.unit$vote) & df.unit$year<1999,]
counts.tab <- table(as.factor(df.unit.sub$study), as.factor(df.unit.sub$partyid2))
num.studies <- length(unique(df.unit.sub$study))
multiplic <- seq(1,5,1)
highnum <- (num.studies+1)
lab.positions <- multiplic*highnum - (num.studies/2)
counts <- ctab(as.factor(df.unit.sub$study), as.factor(df.unit.sub$partyid2), type="row", percentages=F)
pcounts <- print(counts)
barplot(pcounts, beside=TRUE, axes=F, ylim=c(0,.55), main="1997")
abline(h=c(0,.1,.2,.3,.4,.5), lty=2)
barplot(pcounts, legend = rownames(counts.tab), beside=TRUE, axes=F, ylim=c(0,.55), add=T, args.legend = list(x = "topright", bg="white"))
axis(1, at=lab.positions, tick=F, labels=c("Con","Lab","Lib","Other","No preference"))
axis(2, at=c(0,.1,.2,.3,.4,.5), tick=T, labels=c(0,.1,.2,.3,.4,.5))
axis(2, at=.25, labels="Share", tick=F, cex.axis=1.3, line=1.7)

df.unit.sub <- df.unit[!is.na(df.unit$partyid2) & df.unit$vote==1 & !is.na(df.unit$vote) & df.unit$year>1999,]
counts.tab <- table(as.factor(df.unit.sub$study), as.factor(df.unit.sub$partyid2))
num.studies <- length(unique(df.unit.sub$study))
multiplic <- seq(1,5,1)
highnum <- (num.studies+1)
lab.positions <- multiplic*highnum - (num.studies/2)
counts <- ctab(as.factor(df.unit.sub$study), as.factor(df.unit.sub$partyid2), type="row", percentages=F)
pcounts <- print(counts)
barplot(pcounts, beside=TRUE, axes=F, ylim=c(0,.55), main="2001")
abline(h=c(0,.1,.2,.3,.4,.5), lty=2)
barplot(pcounts, legend = rownames(counts.tab), beside=TRUE, axes=F, ylim=c(0,.55), add=T, args.legend = list(x = "topright", bg="white"))
axis(1, at=lab.positions, tick=F, labels=c("Cons","Lab","Lib","Other","No preference"))
axis(2, at=c(0,.1,.2,.3,.4,.5), tick=T, labels=c(0,.1,.2,.3,.4,.5))
axis(2, at=.25, labels="Share", tick=F, cex.axis=1.3, line=1.7)
dev.off()


