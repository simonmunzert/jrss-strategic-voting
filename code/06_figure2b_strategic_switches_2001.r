#----------------------------------------------------------------
# Determining the impact of strategic voting on election results
# Michael Herrmann, Simon Munzert, Peter Selb
# 2015
#----------------------------------------------------------------


## load packages and functions -------------------
source("packages.r")
source("functions.r")


## post-process 2001 estimates
source("05_postprocess_estimates_2001.r")



## Plot voter shifts
pdf(file="../figures/figure2b_strategic_switches_2001.pdf", height=8, width=7, family="URWTimes")
par(mar=c(2,1,6,1))
par(oma=c(1,1,1,0))
par(mfrow=c(3,1))
b.incent.cola.mean <- mean(b.incent.cola)
b.incent.cola.95lo <- quantile(b.incent.cola, probs=c(.025))
b.incent.cola.95hi <- quantile(b.incent.cola, probs=c(.975))
b.incent.coli.mean <- mean(b.incent.coli)
b.incent.coli.95lo <- quantile(b.incent.coli, probs=c(.025))
b.incent.coli.95hi <- quantile(b.incent.coli, probs=c(.975))
b.incent.lali.mean <- mean(b.incent.lali)
b.incent.lali.95lo <- quantile(b.incent.lali, probs=c(.025))
b.incent.lali.95hi <- quantile(b.incent.lali, probs=c(.975))
b.incent.laco.mean <- mean(b.incent.laco)
b.incent.laco.95lo <- quantile(b.incent.laco, probs=c(.025))
b.incent.laco.95hi <- quantile(b.incent.laco, probs=c(.975))
b.incent.lico.mean <- mean(b.incent.lico)
b.incent.lico.95lo <- quantile(b.incent.lico, probs=c(.025))
b.incent.lico.95hi <- quantile(b.incent.lico, probs=c(.975))
b.incent.lila.mean <- mean(b.incent.lila)
b.incent.lila.95lo <- quantile(b.incent.lila, probs=c(.025))
b.incent.lila.95hi <- quantile(b.incent.lila, probs=c(.975))
mean.cola <- b.incent.cola.mean*incent.cola
mean.laco <- -b.incent.laco.mean*incent.laco
ui.cola <- b.incent.cola.95hi*incent.cola
li.cola <- b.incent.cola.95lo*incent.cola
ui.laco <- -b.incent.laco.95lo*incent.laco
li.laco <- -b.incent.laco.95hi*incent.laco 
df.b.incent.cola <- cbind(mean.cola,b.incent.cola.mean,ui.cola,li.cola)
df.b.incent.cola <- subset(df.b.incent.cola, mean.cola!=0)
df.b.incent.cola <- df.b.incent.cola[order(df.b.incent.cola[,1], decreasing=T),]
df.b.incent.laco <- cbind(mean.laco,b.incent.laco.mean,ui.laco,li.laco)
df.b.incent.laco <- df.b.incent.laco[order(df.b.incent.laco[,1], decreasing=T),]
df.b.incent.laco <- subset(df.b.incent.laco, mean.laco!=0)
df.b.incent.colalaco <- rbind(df.b.incent.laco,df.b.incent.cola)
mean.coli <- b.incent.coli.mean*incent.coli - b.incent.lico.mean*incent.lico
ui.coli <- ifelse(incent.coli>0,b.incent.coli.95hi*incent.coli,-b.incent.lico.95lo*incent.lico)
li.coli <- ifelse(incent.coli>0,b.incent.coli.95lo*incent.coli,-b.incent.lico.95hi*incent.lico)
df.b.incent.coli <- cbind(mean.coli,b.incent.coli.mean,ui.coli,li.coli)
df.b.incent.coli <- df.b.incent.coli[order(df.b.incent.coli[,1], decreasing=F),]
mean.lali <- b.incent.lali.mean*incent.lali - b.incent.lila.mean*incent.lila
ui.lali <- ifelse(incent.lali>0,b.incent.lali.95hi*incent.lali,-b.incent.lila.95lo*incent.lila)
li.lali <- ifelse(incent.lali>0,b.incent.lali.95lo*incent.lali,-b.incent.lila.95hi*incent.lila)
df.b.incent.lali <- cbind(mean.lali,b.incent.lali.mean,ui.lali,li.lali)
df.b.incent.lali <- df.b.incent.lali[order(df.b.incent.lali[,1], decreasing=F),]
N <- length(incent.cola)
plotCI(df.b.incent.colalaco[,1],1:N, ui=df.b.incent.colalaco[,3], li=df.b.incent.colalaco[,4], err="x", gap=0, sfrac=0, ylab="", xlab="", xlim=c(-.27,.27), cex=.4, axes=F, barcol="darkgrey", lwd=.5)
abline(v=0, lty=2)
axis(3, at=c(-.3,-.25,-.2,-.15,-.1,-.05,0,.05,.1,.15,.2,.25,.3), labels=c(NA,-.25,-.2, -.15,-.1,-.05,0,.05,.1,.15,.2,.25,NA), tick=T, cex.axis=1.5)
axis(3, at=c(-.15,.15), labels=c("From Lab to Con", "From Con to Lab"), tick=F, line=1.7, cex.axis=1.5)
axis(1, at=c(-.3,-.25,-.2,-.15,-.1,-.05,0,.05,.1,.15,.2,.25,.3), labels=c(NA,-.25,-.2, -.15,-.1,-.05,0,.05,.1,.15,.2,.25,NA), tick=T, cex.axis=1.5)
title(main="2001", line=-1, cex.main=1.8, outer=T)  
plotCI(df.b.incent.coli[,1],1:N, ui=df.b.incent.coli[,3], li=df.b.incent.coli[,4], err="x", gap=0, sfrac=0, ylab="", xlab="", xlim=c(-.27,.27), cex=.4, axes=F, barcol="darkgrey", lwd=.5)
abline(v=0, lty=2)
axis(3, at=c(-.3,-.25,-.2,-.15,-.1,-.05,0,.05,.1,.15,.2,.25,.3), labels=c(NA,-.25,-.2, -.15,-.1,-.05,0,.05,.1,.15,.2,.25,NA), tick=T, cex.axis=1.5)
axis(3, at=c(-.15,.15), labels=c("From Lib to Con", "From Con to Lib"), tick=F, line=1.7, cex.axis=1.5)
axis(1, at=c(-.3,-.25,-.2,-.15,-.1,-.05,0,.05,.1,.15,.2,.25,.3), labels=c(NA,-.25,-.2, -.15,-.1,-.05,0,.05,.1,.15,.2,.25,NA), tick=T, cex.axis=1.5)
plotCI(df.b.incent.lali[,1],1:N, ui=df.b.incent.lali[,3], li=df.b.incent.lali[,4], err="x", gap=0, sfrac=0, ylab="", xlab="", xlim=c(-.27,.27), cex=.4, axes=F, barcol="darkgrey", lwd=.5)
abline(v=0, lty=2)
axis(3, at=c(-.3,-.25,-.2,-.15,-.1,-.05,0,.05,.1,.15,.2,.25,.3), labels=c(NA,-.25,-.2, -.15,-.1,-.05,0,.05,.1,.15,.2,.25,NA), tick=T, cex.axis=1.5)
axis(3, at=c(-.15,.15), labels=c("From Lib to Lab", "From Lab to Lib"), tick=F, line=1.7, cex.axis=1.5)
axis(1, at=c(-.3,-.25,-.2,-.15,-.1,-.05,0,.05,.1,.15,.2,.25,.3), labels=c(NA,-.25,-.2, -.15,-.1,-.05,0,.05,.1,.15,.2,.25,NA), tick=T, cex.axis=1.5)
dev.off()
