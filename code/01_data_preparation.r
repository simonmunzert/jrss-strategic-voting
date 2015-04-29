#----------------------------------------------------------------
# Determining the impact of strategic voting on election results
# Michael Herrmann, Simon Munzert, Peter Selb
# 2015
#----------------------------------------------------------------


## load packages and functions -------------------
source("packages.r")
source("functions.r")



## prepare geographic data

# Read in shape file
x <- readOGR("../data/PCON_DEC_2001_GB_BFC_lowres_rich.shp", layer="PCON_DEC_2001_GB_BFC_lowres_rich")

x$shapefile_idc <- as.character(x$PCON01CD)
x$shapefile_id <- as.numeric(x$shapefile_idc)
x$shapefile_id

# Union districts entailing multiple polygons
sp.districts <- unionSpatialPolygons(x, x$shapefile_id)

# Attach key and area-level data
df.key <- read.dta("../data/wardkey.dta", convert.factors = FALSE)
rownames(df.key) <- df.key$shapefile_id
df.area <- read.dta("../data/constituency_data.dta", convert.factors = FALSE)
df.constituency.data <- merge(df.key,df.area, all=T)
rownames(df.constituency.data) <- df.constituency.data$shapefile_id
df.constituency.data <- df.constituency.data[order(df.constituency.data$shapefile_id),]

# Create spatial polygon data frame
spdf.area <- SpatialPolygonsDataFrame(sp.districts, df.constituency.data, match.ID=TRUE)
spdf.area <- spdf.area[order(spdf.area$shapefile_id),]
spdf.area$area <- x$AREA # attach area size variable
spdf.england <- spdf.area[spdf.area$england==1 & spdf.area$shapefile_id!=231,] # exclude Wales, Scotland and Isle of Wight
spdf.england <- spdf.england[order(spdf.england$shapefile_id),]  # important: order data frame by shapefile_id
spdf.england$shapefile_id2 <- seq(1,528, by=1) # new serial ID after Isle of Wight is excluded
spdf.england <- spdf.england[order(spdf.england$shapefile_id2),]  # important: order data frame by shapefile_id2
spdf.england <- spChFIDs(spdf.england, as.character(spdf.england$shapefile_id2)) # change polygon ID
getSpPPolygonsIDSlots(spdf.england)




## prepare survey data

# Read in unit-level (i.e. survey) data
df.unit <- read.dta("../data/prefs_9602full.dta", convert.factors = FALSE)
summary(df.unit)

# Generate election-specific data frames
df.survey.97 <- df.unit[df.unit$year<1999 & df.unit$vote==1 & !is.na(df.unit$vote) & !is.na(df.unit$shapefile_id2) & df.unit$shapefile_id2<=528,]
df.survey.97 <- df.survey.97 [order(df.survey.97$shapefile_id2),]  # important: order data frame by shapefile_id2
dim(df.survey.97)

df.survey.01 <- df.unit[df.unit$year>1999 & df.unit$vote==1 & !is.na(df.unit$vote) & !is.na(df.unit$shapefile_id2) & df.unit$shapefile_id2<=528,]
df.survey.01 <- df.survey.01 [order(df.survey.01$shapefile_id2),]  # important: order data frame by shapefile_id2
dim(df.survey.01)

# Generate number of obs in district 
df.survey.97$nobs <- 0
for (i in 1:max(df.survey.97$shapefile_id2)) {
df.survey.97$nobs[df.survey.97$shapefile_id2==i] <- length(df.survey.97$shapefile_id2[df.survey.97$shapefile_id2==i])
}
summary(df.survey.97$nobs)
df.survey.01$nobs <- 0
for (i in 1:max(df.survey.01$shapefile_id2)) {
df.survey.01$nobs[df.survey.01$shapefile_id2==i] <- length(df.survey.01$shapefile_id2[df.survey.01$shapefile_id2==i])
}
summary(df.survey.01$nobs)

# Generate direct estimators
df.survey.97$conservative <-  ifelse(df.survey.97$partyid2==1,1,0)
df.survey.97$labour <-  ifelse(df.survey.97$partyid2==2,1,0)
df.survey.97$libdem <-  ifelse(df.survey.97$partyid2==3,1,0)
df.survey.01$conservative <-  ifelse(df.survey.01$partyid2==1,1,0)
df.survey.01$labour <-  ifelse(df.survey.01$partyid2==2,1,0)
df.survey.01$libdem <-  ifelse(df.survey.01$partyid2==3,1,0)

# Generate district-level data frames
df.districts.97 <- aggregate(df.survey.97, by=list(df.survey.97$shapefile_id2), na.rm=T, FUN="mean")
df.districts.01 <- aggregate(df.survey.01, by=list(df.survey.01$shapefile_id2), na.rm=T, FUN="mean")

# Create spatial polygon data frame
shapefile_id2 <- seq(1,528, by=1)
district.id <- as.data.frame(shapefile_id2)
df.districts.97.full  <- merge(df.districts.97, district.id, by="shapefile_id2", all=T) # add missing districts to data.frame
rownames(df.districts.97.full) <- df.districts.97.full$shapefile_id2
spdf.full.97 <- SpatialPolygonsDataFrame(spdf.england, df.districts.97.full)
df.districts.01.full  <- merge(df.districts.01, district.id, by="shapefile_id2", all=T) # add missing districts to data.frame
rownames(df.districts.01.full) <- df.districts.01.full$shapefile_id2
spdf.full.01 <- SpatialPolygonsDataFrame(spdf.england, df.districts.01.full)


# drop unneccessary objects in workspace
objects <- ls()
objects_needed <- objects %in% c("spdf.full.97", "df.survey.97", "spdf.full.01", "df.survey.01", "spdf.england", "df.constituency.data")
objects_not_needed <- objects[!objects_needed]
rm(list = objects_not_needed)

# save data
save.image("../data/data_strategic_uk9701.RData")




## survey summary statistics

# No. of cases per study
table(df.survey.97$study)
table(df.survey.01$study)

# Distribution of preferences
table(df.survey.97$partyid2)
table(df.survey.01$partyid2)

# Total number of cases
dim(df.survey.97)
dim(df.survey.01)

# Total coverage
length(spdf.full.97$nobs[!is.na(spdf.full.97$nobs)])/length(spdf.full.97$nobs)
length(spdf.full.01$nobs[!is.na(spdf.full.01$nobs)])/length(spdf.full.01$nobs)

# Average, minimum, maximum number of respondents per district
summary(spdf.full.97$nobs)
sd(spdf.full.97$nobs, na.rm=T)
summary(spdf.full.01$nobs)
sd(spdf.full.01$nobs, na.rm=T)




## plot distribution of preferences

preftable.97 <- table(df.survey.97$partyid2)
preftable.01 <- table(df.survey.01$partyid2)
x<- (1:5)
pref.names <- c("Con", "Lab", "Lib", "Other", "None")

pdf("../figures/figure_app_survey_preferences.pdf", height=5, width=9, family="URWTimes" )
par(mar=c(4,5,2,1))
par(oma=c(0,0,0,0))
par(mfrow=c(1,2))
# 1997
plot(x, (preftable.97/sum(preftable.97)), typ="h", xlim=c(.8,5.2), ylim=c(0,.52), xlab="",
      ylab="Fraction", main="", axes=F, lwd=2)
axis(1, at=seq(1,5, by=1), labels=pref.names, tick = T, las=1)
axis(2, at=seq(0,.5, by=.1), labels=seq(0,.5, by=.1), tick = T, pos=.7)
abline(h=c(0,.1,.2,.3,.4,.5), lty=2, col="grey")
title(main="Preference distribution, 1997", line=.2)
text(3, .52, paste("No. of respondents:", sum(preftable.97)))
text(x=c(1:5), y=((preftable.97/sum(preftable.97))+.04), preftable.97)
# Plot non-man workers
plot(x, (preftable.01/sum(preftable.01)), typ="h", xlim=c(.8,5.2), ylim=c(0,.52), xlab="",
      ylab="Fraction", main="", axes=F, lwd=2)
axis(1, at=seq(1,5, by=1), labels=pref.names, tick = T, las=1)
axis(2, at=seq(0,.5, by=.1), labels=seq(0,.5, by=.1), tick = T, pos=.7)
abline(h=c(0,.1,.2,.3,.4,.5), lty=2, col="grey")
title(main="Preference distribution, 2001", line=.2)
text(3, .52, paste("No. of respondents:", sum(preftable.01)))
text(x=c(1:5), y=((preftable.01/sum(preftable.01))+.04), preftable.01)
dev.off()

