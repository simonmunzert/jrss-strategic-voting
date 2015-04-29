#----------------------------------------------------------------
# Determining the impact of strategic voting on election results
# Michael Herrmann, Simon Munzert, Peter Selb
# 2015
#----------------------------------------------------------------


## load packages and functions -------------------
source("packages.r")
source("functions.r")

# execute separately for 1997 and 2001!


## load data
load("../data/data_strategic_bayesprep97.RData")
load("../data/data_strategic_bayesprep01.RData")


## prepare data and model
data1 <- list("N", "y.conser", "y.labour", "y.libdem", "ns", "s", "n", "cumn", "u.conser", "u.labour", "u.libdem", "loginvdistsize", "pension", "working", "region", "N.region", "P", "y", "incent.cola", "incent.coli", "incent.lali", "incent.laco", "incent.lico", "incent.lila", "incumbent.conser", "incumbent.labour", "incumbent.libdem", "turnout")
model.full <- paste(getwd(), "../bugs_simulations/stratvotemodel14.txt", sep = "")

## inits
inits1 <- source(paste(getwd(), "../bugs_simulations/stratvotemodel97_14_inits.txt", sep = ""))$value
inits1 <- source(paste(getwd(), "../bugs_simulations/stratvotemodel01_14_inits.txt", sep = ""))$value

## parameters to save
parameters.to.save <- source(paste(getwd(), "../bugs_simulations/stratvotemodel14_params.txt", sep = ""))$value

## run MCMC
model1.sim <- bugs(data=c(data1, spdata), model.file=model.full, inits=inits1, parameters.to.save=parameters.to.save, n.chains=3, n.iter=10000, n.burnin=5000, n.thin=1, bugs.directory="C:/WinBUGS14", debug=T) 

## analysis of output
attach.bugs(model1.sim)
plot(model1.sim)
print(model1.sim, digits=5)
coefs <- cbind(b.inc.conser.conser,b.inc.labour.conser,b.inc.libdem.conser,b.inc.conser.labour,b.inc.labour.labour,b.inc.libdem.labour,b.inc.conser.libdem,b.inc.labour.libdem,b.inc.libdem.libdem)
colMeans(coefs)

## save model workspace
save(model1.sim, file = "../bugs_simulations/stratvotemodel97_14.RData")
save(model1.sim, file = "../bugs_simulations/stratvotemodel01_14.RData")



