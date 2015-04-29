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


## Strategic shift and net margin table

# Prepare data
spdf.england$shapefile_string[spdf.england$shapefile_string=="North Southwark and Bermondsey"] <- "Southwark and Bermondsey"
row.names.district <- spdf.england$shapefile_string # row names: district
row.names.winner <- ifelse(winner97==1, "Con", ifelse(winner97==2, "Lab", ifelse(winner97==3, "Lib", "Other")))
row.names.second <- ifelse(second97==1, "Con", ifelse(second97==2, "Lab", ifelse(second97==3, "Lib", "Other")))
row.names.winnerpred <- ifelse(winner97.nostrategic.modes==1, "Con", ifelse(winner97.nostrategic.modes==2, "Lab", ifelse(winner97.nostrategic.modes==3, "Lib", "Other"))) 
row.names.margin.winner <- winner97.margin
# Extract those cases in which shifts due to strategic voting are predicted at least in some of the simulated cases
coi <- (net.margin.votes.95lo<0 & !is.na(net.margin.votes.mean)) # mark cases of interest
net.margin.votes.mean <- net.margin.votes.mean[coi==T]
net.margin.votes.95lo <- net.margin.votes.95lo[coi==T]
net.margin.votes.95hi <- net.margin.votes.95hi[coi==T]
shift.cola.votes.mean <- shift.cola.votes.mean[coi==T]
shift.cola.votes.95lo <- shift.cola.votes.95lo[coi==T]
shift.cola.votes.95hi <- shift.cola.votes.95hi[coi==T]
shift.coli.votes.mean <- shift.coli.votes.mean[coi==T]
shift.coli.votes.95lo <- shift.coli.votes.95lo[coi==T]
shift.coli.votes.95hi <- shift.coli.votes.95hi[coi==T]
shift.lali.votes.mean <- shift.lali.votes.mean[coi==T]
shift.lali.votes.95lo <- shift.lali.votes.95lo[coi==T]
shift.lali.votes.95hi <- shift.lali.votes.95hi[coi==T]
row.names.district <- row.names.district[coi==T]
row.names.winner <- row.names.winner[coi==T]
row.names.margin.winner <- row.names.margin.winner[coi==T]
row.names.second <- row.names.second[coi==T]
row.names.winnerpred <- row.names.winnerpred[coi==T]
# Sort vectors after net.margin.votes.mean
sort.order <- order(net.margin.votes.mean)
net.margin.votes.mean <- net.margin.votes.mean[sort.order]
net.margin.votes.95lo <- net.margin.votes.95lo[sort.order]
net.margin.votes.95hi <- net.margin.votes.95hi[sort.order]
shift.cola.votes.mean <- shift.cola.votes.mean[sort.order]
shift.cola.votes.95lo <- shift.cola.votes.95lo[sort.order]
shift.cola.votes.95hi <- shift.cola.votes.95hi[sort.order]
shift.coli.votes.mean <- shift.coli.votes.mean[sort.order]
shift.coli.votes.95lo <- shift.coli.votes.95lo[sort.order]
shift.coli.votes.95hi <- shift.coli.votes.95hi[sort.order]
shift.lali.votes.mean <- shift.lali.votes.mean[sort.order]
shift.lali.votes.95lo <- shift.lali.votes.95lo[sort.order]
shift.lali.votes.95hi <- shift.lali.votes.95hi[sort.order]
row.names.district <- row.names.district[sort.order]
row.names.winner <- row.names.winner[sort.order]
row.names.margin.winner <- row.names.margin.winner[sort.order]
row.names.second <- row.names.second[sort.order]
row.names.winnerpred <- row.names.winnerpred[sort.order]
y.axis <- length(row.names.district):1 #create indicator for y.axis, descending so that R orders vars from top to bottom on y-axis


# Identify suggested strategic voting by newspaper
# Create lists of recommendations
rec.independent.97 <- c("Portsmouth South", "Hazel Grove", "St. Ives", "Taunton", "Torridge and West Devon", "Southport", "Hereford", "Somerton and Frome", "Weston-Super-Mare", "Torbay", "Twickenham", "Oxford West and Abingdon", "Wells", "Eastbourne", "North Devon", "Cheltenham", "Bath", "Mid Dorset and North Poole", "Lewes", "Hastings and Rye", "South East Cornwall", "Richmond Park", "Ribble Valley", "West Dorset", "Winchester", "Salisbury", "Teignbridge", # Vote LD to keep Tories out
                        "Oldham East and Saddleworth", "Falmouth and Camborne", "Bristol West", "Colchester", # Vote Labour or LD
                        "Liverpool, Wavertree", "Birmingham, Yardley", "Greenwich and Woolwich", "Chesterfield", "Sheffield, Hillsborough" # Vote LD to keep Labour out
)
rec.lali.97 <- c("Portsmouth South", "Hazel Grove", "St. Ives", "Taunton", "Torridge and West Devon", "Southport", "Hereford", "Somerton and Frome", "Weston-Super-Mare", "Torbay", "Twickenham", "Oxford West and Abingdon", "Wells", "Eastbourne", "North Devon", "Cheltenham", "Bath", "Mid Dorset and North Poole", "Lewes", "Hastings and Rye", "South East Cornwall", "Richmond Park", "Ribble Valley", "West Dorset", "Winchester", "Salisbury", "Teignbridge","Oldham East and Saddleworth", "Falmouth and Camborne", "Bristol West", "Colchester")
rec.coli.97 <- c("Liverpool, Wavertree", "Birmingham, Yardley", "Greenwich and Woolwich", "Chesterfield", "Sheffield, Hillsborough")

rec.id <-  match(rec.independent.97, row.names.district)
rec.id <- rec.id[!is.na(rec.id)]
rec.lali <-  match(rec.lali.97, row.names.district)
rec.lali <-  rec.lali[!is.na(rec.lali)]


# Plot net margin table
pdf("../figures/figure3_table_netmargins_1997.pdf", height=12, width=18, family="URWTimes")  		
layout(matrix(c(1,2,3,4,5,6,7,8,9), 1, 9, byrow=T), widths = c(2.2,1,1,1,1,2,2,2,4.5))
par(mar=c(1,1,4,1), lheight = .8)
par(oma=c(1,3,1,1))
# Plot district labels
plot(rep(0,length(y.axis)), y.axis,  ylab="", xlab="",  cex=.7, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, lwd=.5, col="white")
title(main=expression(paste("Constituency")), line=2.75, cex.main=2, font=2)
text(x=1,y=y.axis[-rec.id], labels=row.names.district[-rec.id], cex=1.4, pos=2)
text(x=1,y=(length(y.axis)+1-sort(rec.id)), labels=row.names.district[sort(rec.id)], font=3, cex=1.4, pos=2, col="red")
#abline(h=seq(5.5,35.5, by=5), lty=1, lwd=1, col = "black")
# Plot margin of the winner
plotCI(net.margin.votes.mean, y.axis,  ui=net.margin.votes.95hi, li=net.margin.votes.95lo, err="x",  gap=0, sfrac=0, ylab="", xlab="",  cex=.7, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, lwd=.5, col="white")
title(main=expression(paste("Margin of")), line=2.75, cex.main=2, font=2)
title(main=expression(paste("victory")), line=1, cex.main=2, font=2)
text(x=0,y=y.axis, labels=row.names.margin.winner, cex=1.4, pos=2)
#abline(h=seq(5.5,35.5, by=5), lty=1, lwd=1, col = "black")
# Plot winner labels
plotCI(net.margin.votes.mean, y.axis,  ui=net.margin.votes.95hi, li=net.margin.votes.95lo, err="x", gap=0, sfrac=0, ylab="", xlab="",  cex=.7, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, lwd=.5, col="white")
title(main=expression(paste("Winner")), line=3, cex.main=2, font=2)
text(x=-4000,y=y.axis, labels=row.names.winner, cex=1.4)
#abline(h=seq(5.5,35.5, by=5), lty=1, lwd=1, col = "black")
# Plot second labels
plotCI(net.margin.votes.mean, y.axis,  ui=net.margin.votes.95hi, li=net.margin.votes.95lo, err="x",  gap=0, sfrac=0, ylab="", xlab="",  cex=.7, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, lwd=.5, col="white")
title(main=expression(paste("Second")), line=3, cex.main=2, font=2)
text(x=-4000,y=y.axis, labels=row.names.second, cex=1.4)
#abline(h=seq(5.5,35.5, by=5), lty=1, lwd=1, col = "black")
# Plot predicted winner under non-strategic voting labels
plotCI(net.margin.votes.mean, y.axis,  ui=net.margin.votes.95hi, li=net.margin.votes.95lo, err="x",  gap=0, sfrac=0, ylab="", xlab="",  cex=.7, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, lwd=.5, col="white")
title(main=expression(paste("Predicted")), line=3, cex.main=2, font=2)
title(main=expression(paste("winner")), line=1.25, cex.main=2, font=2)
text(x=-4000,y=y.axis, labels=row.names.winnerpred, cex=1.4)
#abline(h=seq(5.5,35.5, by=5), lty=1, lwd=1, col = "black")
# Plot shifts
plotCI(shift.cola.votes.mean, y.axis,  ui=shift.cola.votes.95hi, li=shift.cola.votes.95lo, err="x",  gap=0, sfrac=0, ylab="", xlab="",  cex=1.1, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, barcol="black", lwd=.5, xlim=c(-15000,15000), pch=20)
axis(1, at=seq(-15000,15000, by=5000), labels=seq(-15000,15000, by=5000), tick=T, line=-2, cex.axis=1.3)
axis(3, at=seq(-15000,15000, by=5000), labels=seq(-15000,15000, by=5000), tick=T, line=-2, cex.axis=1.3)
segments(0,min(y.axis)-1,0,max(y.axis)+1, lty=2)
title(main=expression(paste("Con " %->% " Lab")), line=3, cex.main=2, font=2)
#abline(h=seq(5.5,35.5, by=5), lty=1, lwd=1, col = "black")
plotCI(shift.coli.votes.mean, y.axis,  ui=shift.coli.votes.95hi, li=shift.coli.votes.95lo, err="x",  gap=0, sfrac=0, ylab="", xlab="",  cex=1.1, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, barcol="black", lwd=.5, xlim=c(-15000,15000), pch=20)
axis(1, at=seq(-15000,15000, by=5000), labels=seq(-15000,15000, by=5000), tick=T, line=-2, cex.axis=1.3)
axis(3, at=seq(-15000,15000, by=5000), labels=seq(-15000,15000, by=5000), tick=T, line=-2, cex.axis=1.3)
segments(0,min(y.axis)-1,0,max(y.axis)+1, lty=2)
title(main=expression(paste("Con " %->% " Lib")), line=3, cex.main=2, font=2)
#abline(h=seq(5.5,35.5, by=5), lty=1, lwd=1, col = "black")
plotCI(shift.lali.votes.mean, y.axis,  ui=shift.lali.votes.95hi, li=shift.lali.votes.95lo, err="x",  gap=0, sfrac=0, ylab="", xlab="",  cex=1.1, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, barcol="black", lwd=.5, xlim=c(-15000,15000), pch=20)
plotCI(shift.lali.votes.mean[sort(rec.lali)], (length(y.axis)+1-sort(rec.lali)),  ui=shift.lali.votes.95hi[sort(rec.lali)], li=shift.lali.votes.95lo[sort(rec.lali)], err="x",  gap=0, sfrac=0, ylab="", xlab="",  cex=1.1, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, lwd=.5, xlim=c(-15000,15000), pch=20, col="red", barcol="red", pt.bg="red", add=T)
axis(1, at=seq(-15000,15000, by=5000), labels=seq(-15000,15000, by=5000), tick=T, line=-2, cex.axis=1.3)
axis(3, at=seq(-15000,15000, by=5000), labels=seq(-15000,15000, by=5000), tick=T, line=-2, cex.axis=1.3)
segments(0,min(y.axis)-1,0,max(y.axis)+1, lty=2)
title(main=expression(paste("Lab " %->% " Lib")), line=3, cex.main=2, font=2)
#abline(h=seq(5.5,35.5, by=5), lty=1, lwd=1, col = "black")
# Plot net margin
plotCI(net.margin.votes.mean, y.axis,  ui=net.margin.votes.95hi, li=net.margin.votes.95lo, err="x", gap=0, sfrac=0, ylab="", xlab="",  cex=1.1, axes=F, cex.lab=1.3, cex.main=.5, cex.axis=.5, barcol="black", lwd=.5, xlim=c(-15000,5000), pch=20)
axis(1, at=c(-15000,-12500,-10000,-7500,-5000,-2500,0,2500,5000), labels=c(-15000,-12500,-10000,-7500,-5000,-2500,0,2500,5000), tick=T, line=-2, cex.axis=1.3)
axis(3, at=c(-15000,-12500,-10000,-7500,-5000,-2500,0,2500,5000), labels=c(-15000,-12500,-10000,-7500,-5000,-2500,0,2500,5000), tick=T, line=-2, cex.axis=1.3)
segments(0,min(y.axis)-1,0,max(y.axis)+1, lty=2)
title(main=expression(paste("Net margin")), line=3, cex.main=2, font=2)
#abline(h=seq(5.5,35.5, by=5), lty=1, lwd=1, col = "black")
dev.off()

