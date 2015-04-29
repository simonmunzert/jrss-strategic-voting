#----------------------------------------------------------------
# Determining the impact of strategic voting on election results
# Michael Herrmann, Simon Munzert, Peter Selb
# 2015
#----------------------------------------------------------------


## load packages and functions -------------------
source("packages.r")
source("functions.r")

## post-process 1997 estimates
source("05_postprocess_estimates_1997.r")



# uncertainty estimate of the number of seats affected: compare simulation results with actual results 

## con
mat.affected.con <- matrix(NA,numsim,528)
for (i in 1:numsim){
  mat.affected.con[i,] <- as.numeric((winner97!=winner97.nostrategic[i,]) & winner97 == 1)
}
num.affected.con <- apply(mat.affected.con, 1, sum)
summary(num.affected.con)
quantile(num.affected.con, probs=c(.025,.975))

## lab
mat.affected.lab <- matrix(NA,numsim,528)
for (i in 1:numsim){
  mat.affected.lab[i,] <- as.numeric((winner97!=winner97.nostrategic[i,]) & winner97 == 2)
}
num.affected.lab <- apply(mat.affected.lab, 1, sum)
summary(num.affected.lab)
quantile(num.affected.lab, probs=c(.025,.975))

## lib
mat.affected.lib <- matrix(NA,numsim,528)
for (i in 1:numsim){
  mat.affected.lib[i,] <- as.numeric((winner97!=winner97.nostrategic[i,]) & winner97 == 3)
}
num.affected.lib <- apply(mat.affected.lib, 1, sum)
summary(num.affected.lib)
quantile(num.affected.lib, probs=c(.025,.975))

## total
mat.affected <- matrix(NA,numsim,528)
for (i in 1:numsim){
  mat.affected[i,] <- as.numeric(winner97!=winner97.nostrategic[i,])
}
num.affected <- apply(mat.affected, 1, sum)
summary(num.affected)
quantile(num.affected, probs=c(.025,.975))



## plot histogram for the number of seats where strategic voting possibly made a difference
pdf(file="../figures/figure_app_hist_seats_affected_1997.pdf", height=8, width=8, family="URWTimes")
hist(num.affected, breaks=40, freq = FALSE, xlim = c(10, 60), xlab = "Number of affected seats", main = "Distribution of the number of affected seats 1997, based on 15,000 simulations")
dev.off()


## plot net winning margins with identification of predicted shifts due to strategic voting
pdf(file="../figures/figure_app_netmargins_1997.pdf", height=9, width=14, family="URWTimes")
par(mar=c(.1,4,.4,3))
par(oma=c(.1,1,.4,0.5))
par(mfrow=c(1,1))
col.con <- rgb(0,135,220, maxColorValue=255)
col.lab <- rgb(220,36,31, maxColorValue=255)
col.lib <- rgb(255,153,0, maxColorValue=255)
col.oth <- rgb(153,247,48, maxColorValue=255)
plotCI(net.margin.votes.mean, ui=net.margin.votes.95hi, li=net.margin.votes.95lo, err="y", gap=0, sfrac=0, ylab="Net margin of the true winner (total number of votes)", xlab="",  cex=.7, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, barcol="black", lwd=.5)
abline(h=0, lty=2)
axis(2, at=seq(-15000,30000, by=5000), labels=seq(-15000,30000, by=5000), tick=T)
axis(4, at=seq(-15000,30000, by=5000), labels=seq(-15000,30000, by=5000), tick=T)
plotCI(spdf.england$shapefile_id2[net.margin.votes.mean<0 & winner97==1], net.margin.votes.mean[net.margin.votes.mean<0 & winner97==1], ui=net.margin.votes.95hi[net.margin.votes.mean<0 & winner97==1], li=net.margin.votes.95lo[net.margin.votes.mean<0 & winner97==1], err="y", gap=0, sfrac=0, ylab="", xlab="",  cex=1, barcol=col.con, col=col.con, lwd=.5, pch=20, add=T)
plotCI(spdf.england$shapefile_id2[net.margin.votes.mean<0 & winner97==2], net.margin.votes.mean[net.margin.votes.mean<0 & winner97==2], ui=net.margin.votes.95hi[net.margin.votes.mean<0 & winner97==2], li=net.margin.votes.95lo[net.margin.votes.mean<0 & winner97==2], err="y", gap=0, sfrac=0, ylab="", xlab="",  cex=1, barcol=col.lab, col=col.lab, lwd=.5, pch=20, add=T)
plotCI(spdf.england$shapefile_id2[net.margin.votes.mean<0 & winner97==3], net.margin.votes.mean[net.margin.votes.mean<0 & winner97==3], ui=net.margin.votes.95hi[net.margin.votes.mean<0 & winner97==3], li=net.margin.votes.95lo[net.margin.votes.mean<0 & winner97==3], err="y", gap=0, sfrac=0, ylab="", xlab="",  cex=1, barcol=col.lib, col=col.lib, lwd=.5, pch=20, add=T)
text(spdf.england$shapefile_id2[net.margin.votes.mean<0 & winner97==1],net.margin.votes.mean[net.margin.votes.mean<0 & winner97==1], spdf.england$shapefile_string[net.margin.votes.mean<0 & winner97==1], cex=.85, col=col.con, font=2, pos=2, srt=40)
text(spdf.england$shapefile_id2[net.margin.votes.mean<0 & winner97==2],net.margin.votes.mean[net.margin.votes.mean<0 & winner97==2], spdf.england$shapefile_string[net.margin.votes.mean<0 & winner97==2], cex=.85, col=col.lab, font=2, pos=2, srt=40)
text(spdf.england$shapefile_id2[net.margin.votes.mean<0 & winner97==3],net.margin.votes.mean[net.margin.votes.mean<0 & winner97==3], spdf.england$shapefile_string[net.margin.votes.mean<0 & winner97==3], cex=.85, col=col.lib, font=2, pos=2, srt=40)
text(spdf.england$shapefile_id2[net.margin.votes.mean<0 & winner97==6],net.margin.votes.mean[net.margin.votes.mean<0 & winner97==6], spdf.england$shapefile_string[net.margin.votes.mean<0 & winner97==6], cex=.85, col=col.oth, font=2, pos=2, srt=40)
text(spdf.england$shapefile_id2[net.margin.votes.95lo<0 & net.margin.votes.mean>0],net.margin.votes.mean[net.margin.votes.95lo<0 & net.margin.votes.mean>0], spdf.england$shapefile_string[net.margin.votes.95lo<0 & net.margin.votes.mean>0], cex=.85, col="darkgrey", font=2, pos=4, srt=40)
dev.off()



## plot strategic incentives vs. margin of victory given the own party is ranked third in the previous election
pdf(file="../figures/figure_app_incentive_mov1997.pdf", height=5, width=7, family="URWTimes")
par(mar=c(4,4,2,1))
par(oma=c(1,1,1,1))
par(mfrow=c(2,2), pty = "s")
par(mfrow=c(2,3))
plot(winner92.margin[labpos92==3],incent.lali[labpos92==3], ylab="Incentive variable", xlab="", main=expression(paste("Lab " %->% " Lib")), xaxt="n", yaxt="n", ylim=c(0,8), xlim=c(0,30000))
abline(lm(incent.lali[labpos92==3] ~ winner92.margin[labpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,8,by=2), labels=seq(0,8,by=2), tick=T)
correlation <- cor(incent.lali[labpos92==3],winner92.margin[labpos92==3])
text(25000, 7.7 , paste("r =", format(correlation, digits=2)), cex=1)
plot(winner92.margin[conpos92==3],incent.coli[conpos92==3], ylab="", xlab="", main=expression(paste("Con " %->% " Lib")), xaxt="n", yaxt="n", ylim=c(0,3), xlim=c(0,30000))
abline(lm(incent.coli[conpos92==3] ~ winner92.margin[conpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,3,by=1), labels=seq(0,3,by=1), tick=T)
correlation <- cor(incent.coli[conpos92==3],winner92.margin[conpos92==3])
text(25000, 2.85 , paste("r =", format(correlation, digits=2)), cex=1)
plot(winner92.margin[conpos92==3],incent.cola[conpos92==3], ylab="", xlab="", main=expression(paste("Con " %->% " Lab")), xaxt="n", yaxt="n", ylim=c(0,3), xlim=c(0,30000))
abline(lm(incent.cola[conpos92==3] ~ winner92.margin[conpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)  
axis(2, at=seq(0,3,by=1), labels=seq(0,3,by=1), tick=T)
correlation <- cor(incent.cola[conpos92==3],winner92.margin[conpos92==3])
text(25000, 2.85 , paste("r =", format(correlation, digits=2)), cex=1)
plot(winner92.margin[ldpos92==3],incent.lila[ldpos92==3], ylab="Incentive variable", xlab="Margin of victory", main=expression(paste("Lib " %->% " Lab")), xaxt="n", yaxt="n", ylim=c(0,8), xlim=c(0,30000))
abline(lm(incent.lila[ldpos92==3] ~ winner92.margin[ldpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,8,by=2), labels=seq(0,8,by=2), tick=T)
correlation <- cor(incent.lila[ldpos92==3],winner92.margin[ldpos92==3])
text(25000, 7.7 , paste("r =", format(correlation, digits=2)), cex=1)
plot(winner92.margin[ldpos92==3],incent.lico[ldpos92==3], ylab="", xlab="Margin of victory", main=expression(paste("Lib " %->% " Con")), xaxt="n", yaxt="n", ylim=c(0,8), xlim=c(0,30000))
abline(lm(incent.lico[ldpos92==3] ~ winner92.margin[ldpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,8,by=2), labels=seq(0,8,by=2), tick=T)	
correlation <- cor(incent.lico[ldpos92==3], winner92.margin[ldpos92==3])
text(25000, 7.7 , paste("r =", format(correlation, digits=2)), cex=1)
plot(winner92.margin[labpos92==3],incent.laco[labpos92==3], ylab="", xlab="Margin of victory", main=expression(paste("Lab " %->% " Con")), xaxt="n", yaxt="n", ylim=c(0,8), xlim=c(0,30000))
abline(lm(incent.laco[labpos92==3] ~ winner92.margin[labpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,8,by=2), labels=seq(0,8,by=2), tick=T)
correlation <- cor(incent.laco[labpos92==3], winner92.margin[labpos92==3])
text(25000, 7.7 , paste("r =", format(correlation, digits=2)), cex=1)
dev.off()



## plot strategic incentives vs. distance from contention (margin between second and third party given the own party is ranked third in the previous election)
pdf(file="../figures/figure_app_incentive_dfc1997.pdf", height=5, width=7, family="URWTimes")
par(mar=c(4,4,2,1))
par(oma=c(1,1,1,1))
par(mfrow=c(2,2), pty = "s")
par(mfrow=c(2,3))
plot(second92.margin[labpos92==3],incent.lali[labpos92==3], ylab="Incentive variable", xlab="", main=expression(paste("Lab " %->% " Lib")), xaxt="n", yaxt="n", ylim=c(0,8), xlim=c(0,30000))
abline(lm(incent.lali[labpos92==3] ~ second92.margin[labpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,8,by=2), labels=seq(0,8,by=2), tick=T)
correlation <- cor(incent.lali[labpos92==3],second92.margin[labpos92==3])
text(25000, 0.5 , paste("r =", format(correlation, digits=2)), cex=1)
plot(second92.margin[conpos92==3],incent.coli[conpos92==3], ylab="", xlab="", main=expression(paste("Con " %->% " Lib")), xaxt="n", yaxt="n", ylim=c(0,3), xlim=c(0,30000))
abline(lm(incent.coli[conpos92==3] ~ second92.margin[conpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,3,by=1), labels=seq(0,3,by=1), tick=T)
correlation <- cor(incent.coli[conpos92==3],second92.margin[conpos92==3])
text(25000, 0.15 , paste("r =", format(correlation, digits=2)), cex=1)
plot(second92.margin[conpos92==3],incent.cola[conpos92==3], ylab="", xlab="", main=expression(paste("Con " %->% " Lab")), xaxt="n", yaxt="n", ylim=c(0,3), xlim=c(0,30000))
abline(lm(incent.cola[conpos92==3] ~ second92.margin[conpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)  
axis(2, at=seq(0,3,by=1), labels=seq(0,3,by=1), tick=T)
correlation <- cor(incent.cola[conpos92==3],second92.margin[conpos92==3])
text(25000, 0.15 , paste("r =", format(correlation, digits=2)), cex=1)
plot(second92.margin[ldpos92==3],incent.lila[ldpos92==3], ylab="Incentive variable", xlab="Distance from contention", main=expression(paste("Lib " %->% " Lab")), xaxt="n", yaxt="n", ylim=c(0,8), xlim=c(0,30000))
abline(lm(incent.lila[ldpos92==3] ~ second92.margin[ldpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,8,by=2), labels=seq(0,8,by=2), tick=T)
correlation <- cor(incent.lila[ldpos92==3],second92.margin[ldpos92==3])
text(25000, 0.5 , paste("r =", format(correlation, digits=2)), cex=1)
plot(second92.margin[ldpos92==3],incent.lico[ldpos92==3], ylab="", xlab="Distance from contention", main=expression(paste("Lib " %->% " Con")), xaxt="n", yaxt="n", ylim=c(0,8), xlim=c(0,30000))
abline(lm(incent.lico[ldpos92==3] ~ second92.margin[ldpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,8,by=2), labels=seq(0,8,by=2), tick=T)	
correlation <- cor(incent.lico[ldpos92==3], second92.margin[ldpos92==3])
text(25000, 0.5 , paste("r =", format(correlation, digits=2)), cex=1)
plot(second92.margin[labpos92==3],incent.laco[labpos92==3], ylab="", xlab="Distance from contention", main=expression(paste("Lab " %->% " Con")), xaxt="n", yaxt="n", ylim=c(0,8), xlim=c(0,30000))
abline(lm(incent.laco[labpos92==3] ~ second92.margin[labpos92==3]))
axis(1, at=seq(0,30000,by=5000), labels=c(0,"",10000,"",20000,"",30000), tick=T)
axis(2, at=seq(0,8,by=2), labels=seq(0,8,by=2), tick=T)
correlation <- cor(incent.laco[labpos92==3], second92.margin[labpos92==3])
text(25000, 0.5 , paste("r =", format(correlation, digits=2)), cex=1)
dev.off()




## plot strategic incentives vs. position in previous election
pdf(file="../figures/figure_app_incentive_pos1997.pdf", height=7, width=9, family="URWTimes")
par(mar=c(4,4,2,1))
par(oma=c(1,1,1,1))
par(mfrow=c(2,2), pty = "s")
par(mfrow=c(2,3))
boxplot(incent.lali~labpos92, yaxt="n", ylim=c(-8,8), ylab="Incentive variable", xlab="Position of Lab in district (1992 election)", main=expression(paste("Lab " %->% " Lib")))
abline(h=0, lty=2)
axis(2, at=seq(-8,8,by=2), labels=seq(-8,8,by=2), tick=T)
boxplot(incent.coli~conpos92, yaxt="n", ylim=c(-8,8), ylab="", xlab="Position of Con in district (1992 election)", main=expression(paste("Con " %->% " Lib")))
abline(h=0, lty=2)
axis(2, at=seq(-8,8,by=2), labels=seq(-8,8,by=2), tick=T)  
boxplot(incent.cola~conpos92, yaxt="n", ylim=c(-8,8), ylab="", xlab="Position of Con in district (1992 election)", main=expression(paste("Con " %->% " Lab")))
abline(h=0, lty=2)
axis(2, at=seq(-8,8,by=2), labels=seq(-8,8,by=2), tick=T)	
boxplot(incent.lila~ldpos92, yaxt="n", ylim=c(-8,8), ylab="Incentive variable", xlab="Position of Lib in district (1992 election)", main=expression(paste("Lib " %->% " Lab")))
abline(h=0, lty=2)
axis(2, at=seq(-8,8,by=2), labels=seq(-8,8,by=2), tick=T)	
boxplot(incent.lico~ldpos92, yaxt="n", ylim=c(-8,8), ylab="", xlab="Position of Lib in district (1992 election)", main=expression(paste("Lib " %->% " Con")))
abline(h=0, lty=2)
axis(2, at=seq(-8,8,by=2), labels=seq(-8,8,by=2), tick=T)	
boxplot(incent.laco~labpos92, yaxt="n", ylim=c(-8,8), ylab="", xlab="Position of Lab in district (1992 election)", main=expression(paste("Lab " %->% " Con")))
abline(h=0, lty=2)
axis(2, at=seq(-8,8,by=2), labels=seq(-8,8,by=2), tick=T)	
dev.off()




## plot reported strategic voting vs. predicted strategic shift
numsim <- length(b.incent.cola)
shift.conser <- NULL
shift.labour <- NULL
shift.libdem <- NULL
shift.total97 <- NULL
for (i in 1:N){
  shift.total97[i] <- abs(mean(b.incent.cola)*incent.cola[i]) + abs(mean(b.incent.laco)*incent.laco[i]) + abs(mean(b.incent.lali)*incent.lali[i]) + abs(mean(b.incent.lila)*incent.lila[i]) + abs(mean(b.incent.coli)*incent.coli[i]) + abs(mean(b.incent.lico)*incent.lico[i])
}

pdf(file="../figures/figure_app_report_vs_estimate_1997.pdf", height=3.5, width=10, family="URWTimes")
par(mar=c(4,4,2,1))
par(oma=c(1,1,1,1))
par(mfrow=c(1,3))
# create restrictive tactical vote variable
df.survey.97$tacticalvote3 <- ifelse(third92[df.survey.97$shapefile_id2] == df.survey.97$partyid2 | second92[df.survey.97$shapefile_id2] == df.survey.97$partyid2 | winner92[df.survey.97$shapefile_id2] == df.survey.97$partyid2, df.survey.97$tacticalvote, NA)
table(df.survey.97$tacticalvote3, useNA="always")

df.survey.97$index <- ifelse(!is.na(df.survey.97$tacticalvote3),1,0)
nobs.tactical <- aggregate(df.survey.97$index[df.survey.97$study=="gescs1997" | df.survey.97$study=="prefs_gespanel1997"], by=list(df.survey.97$shapefile_id2[df.survey.97$study=="gescs1997" | df.survey.97$study=="prefs_gespanel1997"]), na.rm=T, FUN="sum") # important condition: select only studies in which tacticalvote was asked
share.tactical.direct <- aggregate(df.survey.97$tacticalvote3[df.survey.97$study=="gescs1997" | df.survey.97$study=="prefs_gespanel1997"], by=list(df.survey.97$shapefile_id2[df.survey.97$study=="gescs1997" | df.survey.97$study=="prefs_gespanel1997"]), na.rm=T, FUN="mean")
colnames(share.tactical.direct) <- c("shapefile_id2", "share.tactical")
colnames(nobs.tactical) <- c("shapefile_id2", "nobs.tactical")
shapefile_id2 <- seq(1,528, by=1)
district.id <- as.data.frame(shapefile_id2)
share.tactical.direct  <- merge(share.tactical.direct, district.id, by="shapefile_id2", all=T) 
nobs.tactical <- merge(nobs.tactical, district.id, by="shapefile_id2", all=T) 
nobs.tactical[,2] <- ifelse(is.na(nobs.tactical[,2])==T,0,nobs.tactical[,2])

coi <- nobs.tactical[,2] >= 0 & !is.na(share.tactical.direct[,2]) & !is.nan(share.tactical.direct[,2])
plot(shift.total97[coi==T], share.tactical.direct[,2][coi==T], xlab="", ylab=c("1997", "Direct estimate"), xlim=c(0,.5), ylim=c(0,.5))
mae <- mean(abs(shift.total97[coi==T] - share.tactical.direct[,2][coi==T]))
nj <- length(coi[coi==T])
text(.52,.03,paste("N(districts) = ",format(nj)), pos=2)
text(.52,0,paste("MAE = ",format(mae, digits=2)), pos=2)
title(main=expression(paste("Districts with ",N[obs] > 0)), line=1.2)
abline(0,1)
abline(v=mean(shift.total97[coi==T], na.rm=T), lty=2)
abline(h=mean(share.tactical.direct[,2][coi==T], na.rm=T), lty=2)  

coi <- nobs.tactical[,2] >= 20
plot(shift.total97[coi==T], share.tactical.direct[,2][coi==T],xlab="", ylab="", xlim=c(0,.5), ylim=c(0,.5))
mae <- mean(abs(shift.total97[coi==T] - share.tactical.direct[,2][coi==T]))
nj <- length(coi[coi==T])
text(.52,.03,paste("N(districts) = ",format(nj)), pos=2)
text(.52,0,paste("MAE = ",format(mae, digits=2)), pos=2)
title(main=expression(paste("Districts with ",N[obs] >= 20)), line=1.2)
abline(0,1)
abline(v=mean(shift.total97[coi==T]), lty=2)
abline(h=mean(share.tactical.direct[,2][coi==T], na.rm=T), lty=2)	

coi <- nobs.tactical[,2] >= 30
plot(shift.total97[coi==T], share.tactical.direct[,2][coi==T],xlab="", ylab="", xlim=c(0,.5), ylim=c(0,.5))
mae <- mean(abs(shift.total97[coi==T] - share.tactical.direct[,2][coi==T]))
nj <- length(coi[coi==T])
text(.52,.03,paste("N(districts) = ",format(nj)), pos=2)
text(.52,0,paste("MAE = ",format(mae, digits=2)), pos=2)
title(main=expression(paste("Districts with ",N[obs] >= 30)), line=1.2)
abline(0,1)	
abline(v=mean(shift.total97[coi==T]), lty=2)
abline(h=mean(share.tactical.direct[,2][coi==T], na.rm=T), lty=2)	
dev.off()




### True vote shares vs. predicted vote shares under non-strategic voting
voteshare97.conser.nostrategic.means <- colMeans(voteshare97.conser.nostrategic)
voteshare97.labour.nostrategic.means <- colMeans(voteshare97.labour.nostrategic)
voteshare97.libdem.nostrategic.means <- colMeans(voteshare97.libdem.nostrategic)
pdf(file="../figures/figure_app_pvt_strategic_1997.pdf", height=4, width=11, family="URWTimes")
par(mar=c(2,3,2,1))
par(oma=c(2,3,2,1))
par(mfrow=c(1,3))
plot(voteshare97.conser,voteshare97.conser.nostrategic.means, xlim=c(0,1), ylim=c(0,1))
abline(v=mean(voteshare97.conser, na.rm=T), lty=2)
abline(h=mean(voteshare97.conser.nostrategic.means, na.rm=T), lty=2)
axis(3, at=.5, labels="Con", tick=F, cex.axis=1.5)
axis(2, at=.5, labels="1997", tick=F, cex.axis=1.5, line=3)
axis(2, at=.5, labels="Predicted values", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(voteshare97.labour,voteshare97.labour.nostrategic.means, xlim=c(0,1), ylim=c(0,1))
abline(v=mean(voteshare97.labour, na.rm=T), lty=2)
abline(h=mean(voteshare97.labour.nostrategic.means, na.rm=T), lty=2)
axis(3, at=.5, labels="Lab", tick=F, cex.axis=1.5)
arrows(.1,.1,.1,.55, length=.1, col="blue", lwd=2)  
arrows(.2,.2,.2,.55, length=.1, col="blue", lwd=2)
arrows(.3,.3,.3,.55, length=.1, col="blue", lwd=2)
text(.25, .6, "Vote loss due to strategic voting", col="blue")
arrows(.5,.5,.5,.25, length=.1, col="blue", lwd=2)
arrows(.6,.6,.6,.25, length=.1, col="blue", lwd=2)
arrows(.7,.7,.7,.25, length=.1, col="blue", lwd=2)
text(.7, .2, "Vote gain due to strategic voting", col="blue")
axis(1, at=.5, labels="True values", tick=F, cex.axis=1.3, line=1.7)
abline(0,1)
plot(voteshare97.libdem,voteshare97.libdem.nostrategic.means, xlim=c(0,1), ylim=c(0,1))
abline(v=mean(voteshare97.libdem, na.rm=T), lty=2)
abline(h=mean(voteshare97.libdem.nostrategic.means, na.rm=T), lty=2)
axis(3, at=.5, labels="Lib", tick=F, cex.axis=1.5)
abline(0,1)
dev.off()





