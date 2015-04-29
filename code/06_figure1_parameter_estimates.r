#----------------------------------------------------------------
# Determining the impact of strategic voting on election results
# Michael Herrmann, Simon Munzert, Peter Selb
# 2015
#----------------------------------------------------------------


## load packages and functions -------------------
source("packages.r")
source("functions.r")



## prepare variable name vector ------------------
var.names <- c(
expression(paste(italic("CON PREFERENCE MODEL      "))), 
	"Log inverse district size", 
	"Retired pop. in district (%)",
	"Labour force pop. in district (%)",
	"Con incumbent",
	"Lab incumbent",
	"Lib incumbent",
	expression(paste("SD of ", alpha)), 
	expression(paste("SD of ", eta)), 
	expression(paste("SD of ", nu)), 
expression(paste(italic("LAB PREFERENCE MODEL      "))), 
	"Log inverse district size", 
	"Retired pop. in district (%)",
	"Labour force pop. in district (%)",
	"Con incumbent",
	"Lab incumbent",
	"Lib incumbent",
	expression(paste("SD of ", alpha)), 
	expression(paste("SD of ", eta)), 
	expression(paste("SD of ", nu)), 
expression(paste(italic("LIB PREFERENCE MODEL      "))), 
	"Log inverse district size", 
	"Retired pop. in district (%)",
	"Labour force pop. in district (%)",
	"Con incumbent",
	"Lab incumbent",
	"Lib incumbent",
	expression(paste("SD of ", alpha)), 
	expression(paste("SD of ", eta)), 
	expression(paste("SD of ", nu)), 
expression(paste(italic("STRATEGIC VOTING MODEL      "))), 
	expression(paste("Strat. incentive Con " %->% " Lab (:10)")), expression(paste("Strat. incentive Con " %->% " Lib (:10)")), expression(paste("Strat. incentive Lab " %->% " Con (:10)")), expression(paste("Strat. incentive Lab " %->% " Lib (:10)")), expression(paste("Strat. incentive Lib " %->% " Con (:10)")), expression(paste("Strat. incentive Lib " %->% " Lab (:10)")),
	expression(paste("Con incumbent, gain from Lab")), expression(paste("Con incumbent, gain from Lib")),
	expression(paste("Lab incumbent, gain from Con")), expression(paste("Lab incumbent, gain from Lib")),
	expression(paste("Lib incumbent, gain from Con")), expression(paste("Lib incumbent, gain from Lab")),
	expression("Turnout (Con)"),expression("Turnout (Lab)"),expression("Turnout (Lib)"),
	expression(paste("Share of independents (Con)")),expression(paste("Share of independents (Lab)")), expression(paste("Share of independents (Lib)")),
	expression("Constant (Con)"),expression("Constant (Lab)"), expression("Constant (Lib)"),
	expression("SD (Con)"), 
	expression("SD (Lab)"), 
	expression("SD (Lib)"))
y.axis <- length(var.names):1 #create indicator for y.axis, descending so that R orders vars from top to bottom on y-axis
adjust <- .1 #create object that we will use to adjust points and lines up and down to distinguish between models



## retrieve estimates ------------------
load("../bugs_simulations/stratvotemodel97_14.RData")

attach.bugs(model1.sim)
b.loginvdistsize.conser.med <- median(b.loginvdistsize.conser)
b.pension.conser.med <- median(b.pension.conser)
b.worker.conser.med <- median(b.worker.conser)
b.inc.conser.conser.med <- median(b.inc.conser.conser)
b.inc.labour.conser.med <- median(b.inc.labour.conser)
b.inc.libdem.conser.med <- median(b.inc.libdem.conser)
b.inc.conser.labour.med <- median(b.inc.conser.labour)
b.inc.labour.labour.med <- median(b.inc.labour.labour)
b.inc.libdem.labour.med <- median(b.inc.libdem.labour)
b.inc.conser.libdem.med <- median(b.inc.conser.libdem)
b.inc.labour.libdem.med <- median(b.inc.labour.libdem)
b.inc.libdem.libdem.med <- median(b.inc.libdem.libdem)
sigmaregion.conser.med <- median(sigmaregion.conser)
sigmau.conser.med <- median(sigmau.conser)
sigmav.conser.med <- median(sigmav.conser)
b.loginvdistsize.labour.med <- median(b.loginvdistsize.labour)
b.pension.labour.med <- median(b.pension.labour)
b.worker.labour.med <- median(b.worker.labour)
sigmaregion.labour.med <- median(sigmaregion.labour)
sigmau.labour.med <- median(sigmau.labour)
sigmav.labour.med <- median(sigmav.labour)
b.loginvdistsize.libdem.med <- median(b.loginvdistsize.libdem)
b.pension.libdem.med <- median(b.pension.libdem)
b.worker.libdem.med <- median(b.worker.libdem)
sigmaregion.libdem.med <- median(sigmaregion.libdem)
sigmau.libdem.med <- median(sigmau.libdem)
sigmav.libdem.med <- median(sigmav.libdem)
b0.conser.med <- median(b0.conser)
b0.labour.med <- median(b0.labour)
b0.libdem.med <- median(b0.libdem)
b.incent.cola.med <- median(b.incent.cola*10)
b.incent.coli.med <- median(b.incent.coli*10)
b.incent.lali.med <- median(b.incent.lali*10)
b.incent.laco.med <- median(b.incent.laco*10)
b.incent.lico.med <- median(b.incent.lico*10)
b.incent.lila.med <- median(b.incent.lila*10)
b.incumb.conlab.med <- median(b.incumb.conlab)
b.incumb.conlib.med <- median(b.incumb.conlib)
b.incumb.labcon.med <- median(b.incumb.labcon)
b.incumb.lablib.med <- median(b.incumb.lablib)
b.incumb.libcon.med <- median(b.incumb.libcon)
b.incumb.liblab.med <- median(b.incumb.liblab)
b.turnout.conser.med <- median(b.turnout.conser)*100
b.turnout.labour.med <- median(b.turnout.labour)*100
b.turnout.libdem.med <- median(b.turnout.libdem)*100
b.independ.conser.med <- median(b.independ.conser)
b.independ.labour.med <- median(b.independ.labour)
b.independ.libdem.med <- median(b.independ.libdem)
sigma1.med <- median(sigma[1])
sigma2.med <- median(sigma[2])
sigma3.med <- median(sigma[3])
b.loginvdistsize.conser.95lo <- quantile(b.loginvdistsize.conser , probs=c(.025))
b.pension.conser.95lo <- quantile(b.pension.conser , probs=c(.025))
b.worker.conser.95lo <- quantile(b.worker.conser , probs=c(.025))
sigmaregion.conser.95lo <- quantile(sigmaregion.conser , probs=c(.025))
sigmau.conser.95lo <- quantile(sigmau.conser , probs=c(.025))
sigmav.conser.95lo <- quantile(sigmav.conser , probs=c(.025))
b.loginvdistsize.labour.95lo <- quantile(b.loginvdistsize.labour , probs=c(.025))
b.pension.labour.95lo <- quantile(b.pension.labour , probs=c(.025))
b.worker.labour.95lo <- quantile(b.worker.labour , probs=c(.025))
sigmaregion.labour.95lo <- quantile(sigmaregion.labour , probs=c(.025))
sigmau.labour.95lo <- quantile(sigmau.labour , probs=c(.025))
sigmav.labour.95lo <- quantile(sigmav.labour , probs=c(.025))
b.loginvdistsize.libdem.95lo <- quantile(b.loginvdistsize.libdem , probs=c(.025))
b.pension.libdem.95lo <- quantile(b.pension.libdem , probs=c(.025))
b.worker.libdem.95lo <- quantile(b.worker.libdem , probs=c(.025))
sigmaregion.libdem.95lo <- quantile(sigmaregion.libdem , probs=c(.025))
sigmau.libdem.95lo <- quantile(sigmau.libdem , probs=c(.025))
sigmav.libdem.95lo <- quantile(sigmav.libdem , probs=c(.025))
b0.conser.95lo <- quantile(b0.conser , probs=c(.025))
b0.labour.95lo <- quantile(b0.labour , probs=c(.025))
b0.libdem.95lo <- quantile(b0.libdem , probs=c(.025))
b.incent.cola.95lo <- quantile(b.incent.cola*10 , probs=c(.025))
b.incent.coli.95lo <- quantile(b.incent.coli*10 , probs=c(.025))
b.incent.lali.95lo <- quantile(b.incent.lali*10 , probs=c(.025))
b.incent.laco.95lo <- quantile(b.incent.laco*10 , probs=c(.025))
b.incent.lico.95lo <- quantile(b.incent.lico*10 , probs=c(.025))
b.incent.lila.95lo <- quantile(b.incent.lila*10 , probs=c(.025))
b.incumb.conlab.95lo <- quantile(b.incumb.conlab , probs=c(.025))
b.incumb.conlib.95lo <- quantile(b.incumb.conlib , probs=c(.025))
b.incumb.labcon.95lo <- quantile(b.incumb.labcon , probs=c(.025))
b.incumb.lablib.95lo <- quantile(b.incumb.lablib , probs=c(.025))
b.incumb.libcon.95lo <- quantile(b.incumb.libcon , probs=c(.025))
b.incumb.liblab.95lo <- quantile(b.incumb.liblab , probs=c(.025))
b.turnout.conser.95lo <- quantile(b.turnout.conser, probs=c(.025))*100
b.turnout.labour.95lo <- quantile(b.turnout.labour, probs=c(.025))*100
b.turnout.libdem.95lo <- quantile(b.turnout.libdem, probs=c(.025))*100
b.independ.conser.95lo <- quantile(b.independ.conser , probs=c(.025))
b.independ.labour.95lo <- quantile(b.independ.labour , probs=c(.025))
b.independ.libdem.95lo <- quantile(b.independ.libdem , probs=c(.025))
sigma1.95lo <- quantile(sigma[1], probs=c(.025))
sigma2.95lo <- quantile(sigma[2], probs=c(.025))
sigma3.95lo <- quantile(sigma[3], probs=c(.025))
b.loginvdistsize.conser.95hi <- quantile(b.loginvdistsize.conser , probs=c(.975))
b.pension.conser.95hi <- quantile(b.pension.conser , probs=c(.975))
b.worker.conser.95hi <- quantile(b.worker.conser , probs=c(.975))
sigmaregion.conser.95hi <- quantile(sigmaregion.conser , probs=c(.975))
sigmau.conser.95hi <- quantile(sigmau.conser , probs=c(.975))
sigmav.conser.95hi <- quantile(sigmav.conser , probs=c(.975))
b.loginvdistsize.labour.95hi <- quantile(b.loginvdistsize.labour , probs=c(.975))
b.pension.labour.95hi <- quantile(b.pension.labour , probs=c(.975))
b.worker.labour.95hi <- quantile(b.worker.labour , probs=c(.975))
sigmaregion.labour.95hi <- quantile(sigmaregion.labour , probs=c(.975))
sigmau.labour.95hi <- quantile(sigmau.labour , probs=c(.975))
sigmav.labour.95hi <- quantile(sigmav.labour , probs=c(.975))
b.loginvdistsize.libdem.95hi <- quantile(b.loginvdistsize.libdem , probs=c(.975))
b.pension.libdem.95hi <- quantile(b.pension.libdem , probs=c(.975))
b.worker.libdem.95hi <- quantile(b.worker.libdem , probs=c(.975))
sigmaregion.libdem.95hi <- quantile(sigmaregion.libdem , probs=c(.975))
sigmau.libdem.95hi <- quantile(sigmau.libdem , probs=c(.975))
sigmav.libdem.95hi <- quantile(sigmav.libdem , probs=c(.975))
b0.conser.95hi <- quantile(b0.conser , probs=c(.975))
b0.labour.95hi <- quantile(b0.labour , probs=c(.975))
b0.libdem.95hi <- quantile(b0.libdem , probs=c(.975))
b.incent.cola.95hi <- quantile(b.incent.cola*10 , probs=c(.975))
b.incent.coli.95hi <- quantile(b.incent.coli*10 , probs=c(.975))
b.incent.lali.95hi <- quantile(b.incent.lali*10 , probs=c(.975))
b.incent.laco.95hi <- quantile(b.incent.laco*10 , probs=c(.975))
b.incent.lico.95hi <- quantile(b.incent.lico*10 , probs=c(.975))
b.incent.lila.95hi <- quantile(b.incent.lila*10 , probs=c(.975))
b.incumb.conlab.95hi <- quantile(b.incumb.conlab , probs=c(.975))
b.incumb.conlib.95hi <- quantile(b.incumb.conlib , probs=c(.975))
b.incumb.labcon.95hi <- quantile(b.incumb.labcon , probs=c(.975))
b.incumb.lablib.95hi <- quantile(b.incumb.lablib , probs=c(.975))
b.incumb.libcon.95hi <- quantile(b.incumb.libcon , probs=c(.975))
b.incumb.liblab.95hi <- quantile(b.incumb.liblab , probs=c(.975))
b.turnout.conser.95hi <- quantile(b.turnout.conser, probs=c(.975))*100
b.turnout.labour.95hi <- quantile(b.turnout.labour, probs=c(.975))*100
b.turnout.libdem.95hi <- quantile(b.turnout.libdem, probs=c(.975))*100
b.independ.conser.95hi <- quantile(b.independ.conser , probs=c(.975))
b.independ.labour.95hi <- quantile(b.independ.labour , probs=c(.975))
b.independ.libdem.95hi <- quantile(b.independ.libdem , probs=c(.975))
sigma1.95hi <- quantile(sigma[1], probs=c(.975))
sigma2.95hi <- quantile(sigma[2], probs=c(.975))
sigma3.95hi <- quantile(sigma[3], probs=c(.975))
b.inc.conser.conser.95lo <- quantile(b.inc.conser.conser, probs=c(.025))
b.inc.labour.conser.95lo <- quantile(b.inc.labour.conser, probs=c(.025))
b.inc.libdem.conser.95lo <- quantile(b.inc.libdem.conser, probs=c(.025))
b.inc.conser.labour.95lo <- quantile(b.inc.conser.labour, probs=c(.025))
b.inc.labour.labour.95lo <- quantile(b.inc.labour.labour, probs=c(.025))
b.inc.libdem.labour.95lo <- quantile(b.inc.libdem.labour, probs=c(.025))
b.inc.conser.libdem.95lo <- quantile(b.inc.conser.libdem, probs=c(.025))
b.inc.labour.libdem.95lo <- quantile(b.inc.labour.libdem, probs=c(.025))
b.inc.libdem.libdem.95lo <- quantile(b.inc.libdem.libdem, probs=c(.025))
b.inc.conser.conser.95hi <- quantile(b.inc.conser.conser, probs=c(.975))
b.inc.labour.conser.95hi <- quantile(b.inc.labour.conser, probs=c(.975))
b.inc.libdem.conser.95hi <- quantile(b.inc.libdem.conser, probs=c(.975))
b.inc.conser.labour.95hi <- quantile(b.inc.conser.labour, probs=c(.975))
b.inc.labour.labour.95hi <- quantile(b.inc.labour.labour, probs=c(.975))
b.inc.libdem.labour.95hi <- quantile(b.inc.libdem.labour, probs=c(.975))
b.inc.conser.libdem.95hi <- quantile(b.inc.conser.libdem, probs=c(.975))
b.inc.labour.libdem.95hi <- quantile(b.inc.labour.libdem, probs=c(.975))
b.inc.libdem.libdem.95hi <- quantile(b.inc.libdem.libdem, probs=c(.975))
detach.bugs()

# prepare coefficient vectors
model3.97.med <- c(NA,b.loginvdistsize.conser.med,b.pension.conser.med,b.worker.conser.med,b.inc.conser.conser.med,b.inc.labour.conser.med, b.inc.libdem.conser.med,sigmaregion.conser.med,sigmau.conser.med,sigmav.conser.med,NA, b.loginvdistsize.labour.med,b.pension.labour.med,b.worker.labour.med,b.inc.conser.labour.med, b.inc.labour.labour.med,b.inc.libdem.labour.med,sigmaregion.labour.med,sigmau.labour.med,sigmav.labour.med,NA,b.loginvdistsize.libdem.med,b.pension.libdem.med,b.worker.libdem.med,b.inc.conser.libdem.med, b.inc.labour.libdem.med,b.inc.libdem.libdem.med, sigmaregion.libdem.med,sigmau.libdem.med,sigmav.libdem.med,NA,b.incent.cola.med,b.incent.coli.med,b.incent.laco.med,b.incent.lali.med,b.incent.lico.med,b.incent.lila.med,b.incumb.conlab.med,b.incumb.conlib.med,b.incumb.labcon.med,b.incumb.lablib.med,b.incumb.libcon.med,b.incumb.liblab.med,b.turnout.conser.med,b.turnout.labour.med,b.turnout.libdem.med,b.independ.conser.med,b.independ.labour.med,b.independ.libdem.med,b0.conser.med,b0.labour.med,b0.libdem.med,sigma1.med,sigma2.med,sigma3.med)
model3.97.95lo <-  c(NA,b.loginvdistsize.conser.95lo,b.pension.conser.95lo,b.worker.conser.95lo,b.inc.conser.conser.95lo,b.inc.labour.conser.95lo, b.inc.libdem.conser.95lo,sigmaregion.conser.95lo,sigmau.conser.95lo,sigmav.conser.95lo,NA, b.loginvdistsize.labour.95lo,b.pension.labour.95lo,b.worker.labour.95lo,b.inc.conser.labour.95lo, b.inc.labour.labour.95lo,b.inc.libdem.labour.95lo, sigmaregion.labour.95lo,sigmau.labour.95lo,sigmav.labour.95lo,NA,b.loginvdistsize.libdem.95lo,b.pension.libdem.95lo,b.worker.libdem.95lo,b.inc.conser.libdem.95lo, b.inc.labour.libdem.95lo,b.inc.libdem.libdem.95lo,sigmaregion.libdem.95lo,sigmau.libdem.95lo,sigmav.libdem.95lo,NA,b.incent.cola.95lo,b.incent.coli.95lo,b.incent.laco.95lo,b.incent.lali.95lo,b.incent.lico.95lo,b.incent.lila.95lo,b.incumb.conlab.95lo,b.incumb.conlib.95lo,b.incumb.labcon.95lo,b.incumb.lablib.95lo,b.incumb.libcon.95lo,b.incumb.liblab.95lo,b.turnout.conser.95lo,b.turnout.labour.95lo,b.turnout.libdem.95lo,b.independ.conser.95lo,b.independ.labour.95lo,b.independ.libdem.95lo,b0.conser.95lo,b0.labour.95lo,b0.libdem.95lo,sigma1.95lo,sigma2.95lo,sigma3.95lo)
model3.97.95hi <-  c(NA,b.loginvdistsize.conser.95hi,b.pension.conser.95hi,b.worker.conser.95hi,b.inc.conser.conser.95hi,b.inc.labour.conser.95hi, b.inc.libdem.conser.95hi,sigmaregion.conser.95hi,sigmau.conser.95hi,sigmav.conser.95hi,NA, b.loginvdistsize.labour.95hi,b.pension.labour.95hi,b.worker.labour.95hi,b.inc.conser.labour.95hi, b.inc.labour.labour.95hi,b.inc.libdem.labour.95hi, sigmaregion.labour.95hi,sigmau.labour.95hi,sigmav.labour.95hi,NA,b.loginvdistsize.libdem.95hi,b.pension.libdem.95hi,b.worker.libdem.95hi,b.inc.conser.libdem.95hi, b.inc.labour.libdem.95hi,b.inc.libdem.libdem.95hi,sigmaregion.libdem.95hi,sigmau.libdem.95hi,sigmav.libdem.95hi,NA,b.incent.cola.95hi,b.incent.coli.95hi,b.incent.laco.95hi,b.incent.lali.95hi,b.incent.lico.95hi,b.incent.lila.95hi,b.incumb.conlab.95hi,b.incumb.conlib.95hi,b.incumb.labcon.95hi,b.incumb.lablib.95hi,b.incumb.libcon.95hi,b.incumb.liblab.95hi,b.turnout.conser.95hi,b.turnout.labour.95hi,b.turnout.libdem.95hi,b.independ.conser.95hi,b.independ.labour.95hi,b.independ.libdem.95hi,b0.conser.95hi,b0.labour.95hi,b0.libdem.95hi,sigma1.95hi,sigma2.95hi,sigma3.95hi)


load("../bugs_simulations/stratvotemodel01_14.RData")

attach.bugs(model1.sim)
b.loginvdistsize.conser.med <- median(b.loginvdistsize.conser)
b.pension.conser.med <- median(b.pension.conser)
b.worker.conser.med <- median(b.worker.conser)
b.inc.conser.conser.med <- median(b.inc.conser.conser)
b.inc.labour.conser.med <- median(b.inc.labour.conser)
b.inc.libdem.conser.med <- median(b.inc.libdem.conser)
b.inc.conser.labour.med <- median(b.inc.conser.labour)
b.inc.labour.labour.med <- median(b.inc.labour.labour)
b.inc.libdem.labour.med <- median(b.inc.libdem.labour)
b.inc.conser.libdem.med <- median(b.inc.conser.libdem)
b.inc.labour.libdem.med <- median(b.inc.labour.libdem)
b.inc.libdem.libdem.med <- median(b.inc.libdem.libdem)
sigmaregion.conser.med <- median(sigmaregion.conser)
sigmau.conser.med <- median(sigmau.conser)
sigmav.conser.med <- median(sigmav.conser)
b.loginvdistsize.labour.med <- median(b.loginvdistsize.labour)
b.pension.labour.med <- median(b.pension.labour)
b.worker.labour.med <- median(b.worker.labour)
sigmaregion.labour.med <- median(sigmaregion.labour)
sigmau.labour.med <- median(sigmau.labour)
sigmav.labour.med <- median(sigmav.labour)
b.loginvdistsize.libdem.med <- median(b.loginvdistsize.libdem)
b.pension.libdem.med <- median(b.pension.libdem)
b.worker.libdem.med <- median(b.worker.libdem)
sigmaregion.libdem.med <- median(sigmaregion.libdem)
sigmau.libdem.med <- median(sigmau.libdem)
sigmav.libdem.med <- median(sigmav.libdem)
b0.conser.med <- median(b0.conser)
b0.labour.med <- median(b0.labour)
b0.libdem.med <- median(b0.libdem)
b.incent.cola.med <- median(b.incent.cola*10)
b.incent.coli.med <- median(b.incent.coli*10)
b.incent.lali.med <- median(b.incent.lali*10)
b.incent.laco.med <- median(b.incent.laco*10)
b.incent.lico.med <- median(b.incent.lico*10)
b.incent.lila.med <- median(b.incent.lila*10)
b.incumb.conlab.med <- median(b.incumb.conlab)
b.incumb.conlib.med <- median(b.incumb.conlib)
b.incumb.labcon.med <- median(b.incumb.labcon)
b.incumb.lablib.med <- median(b.incumb.lablib)
b.incumb.libcon.med <- median(b.incumb.libcon)
b.incumb.liblab.med <- median(b.incumb.liblab)
b.turnout.conser.med <- median(b.turnout.conser)*100
b.turnout.labour.med <- median(b.turnout.labour)*100
b.turnout.libdem.med <- median(b.turnout.libdem)*100
b.independ.conser.med <- median(b.independ.conser)
b.independ.labour.med <- median(b.independ.labour)
b.independ.libdem.med <- median(b.independ.libdem)
sigma1.med <- median(sigma[1])
sigma2.med <- median(sigma[2])
sigma3.med <- median(sigma[3])
b.loginvdistsize.conser.95lo <- quantile(b.loginvdistsize.conser , probs=c(.025))
b.pension.conser.95lo <- quantile(b.pension.conser , probs=c(.025))
b.worker.conser.95lo <- quantile(b.worker.conser , probs=c(.025))
sigmaregion.conser.95lo <- quantile(sigmaregion.conser , probs=c(.025))
sigmau.conser.95lo <- quantile(sigmau.conser , probs=c(.025))
sigmav.conser.95lo <- quantile(sigmav.conser , probs=c(.025))
b.loginvdistsize.labour.95lo <- quantile(b.loginvdistsize.labour , probs=c(.025))
b.pension.labour.95lo <- quantile(b.pension.labour , probs=c(.025))
b.worker.labour.95lo <- quantile(b.worker.labour , probs=c(.025))
sigmaregion.labour.95lo <- quantile(sigmaregion.labour , probs=c(.025))
sigmau.labour.95lo <- quantile(sigmau.labour , probs=c(.025))
sigmav.labour.95lo <- quantile(sigmav.labour , probs=c(.025))
b.loginvdistsize.libdem.95lo <- quantile(b.loginvdistsize.libdem , probs=c(.025))
b.pension.libdem.95lo <- quantile(b.pension.libdem , probs=c(.025))
b.worker.libdem.95lo <- quantile(b.worker.libdem , probs=c(.025))
sigmaregion.libdem.95lo <- quantile(sigmaregion.libdem , probs=c(.025))
sigmau.libdem.95lo <- quantile(sigmau.libdem , probs=c(.025))
sigmav.libdem.95lo <- quantile(sigmav.libdem , probs=c(.025))
b0.conser.95lo <- quantile(b0.conser , probs=c(.025))
b0.labour.95lo <- quantile(b0.labour , probs=c(.025))
b0.libdem.95lo <- quantile(b0.libdem , probs=c(.025))
b.incent.cola.95lo <- quantile(b.incent.cola*10 , probs=c(.025))
b.incent.coli.95lo <- quantile(b.incent.coli*10 , probs=c(.025))
b.incent.lali.95lo <- quantile(b.incent.lali*10 , probs=c(.025))
b.incent.laco.95lo <- quantile(b.incent.laco*10 , probs=c(.025))
b.incent.lico.95lo <- quantile(b.incent.lico*10 , probs=c(.025))
b.incent.lila.95lo <- quantile(b.incent.lila*10 , probs=c(.025))
b.incumb.conlab.95lo <- quantile(b.incumb.conlab , probs=c(.025))
b.incumb.conlib.95lo <- quantile(b.incumb.conlib , probs=c(.025))
b.incumb.labcon.95lo <- quantile(b.incumb.labcon , probs=c(.025))
b.incumb.lablib.95lo <- quantile(b.incumb.lablib , probs=c(.025))
b.incumb.libcon.95lo <- quantile(b.incumb.libcon , probs=c(.025))
b.incumb.liblab.95lo <- quantile(b.incumb.liblab , probs=c(.025))
b.turnout.conser.95lo <- quantile(b.turnout.conser, probs=c(.025))*100
b.turnout.labour.95lo <- quantile(b.turnout.labour, probs=c(.025))*100
b.turnout.libdem.95lo <- quantile(b.turnout.libdem, probs=c(.025))*100
b.independ.conser.95lo <- quantile(b.independ.conser , probs=c(.025))
b.independ.labour.95lo <- quantile(b.independ.labour , probs=c(.025))
b.independ.libdem.95lo <- quantile(b.independ.libdem , probs=c(.025))
sigma1.95lo <- quantile(sigma[1], probs=c(.025))
sigma2.95lo <- quantile(sigma[2], probs=c(.025))
sigma3.95lo <- quantile(sigma[3], probs=c(.025))
b.loginvdistsize.conser.95hi <- quantile(b.loginvdistsize.conser , probs=c(.975))
b.pension.conser.95hi <- quantile(b.pension.conser , probs=c(.975))
b.worker.conser.95hi <- quantile(b.worker.conser , probs=c(.975))
sigmaregion.conser.95hi <- quantile(sigmaregion.conser , probs=c(.975))
sigmau.conser.95hi <- quantile(sigmau.conser , probs=c(.975))
sigmav.conser.95hi <- quantile(sigmav.conser , probs=c(.975))
b.loginvdistsize.labour.95hi <- quantile(b.loginvdistsize.labour , probs=c(.975))
b.pension.labour.95hi <- quantile(b.pension.labour , probs=c(.975))
b.worker.labour.95hi <- quantile(b.worker.labour , probs=c(.975))
sigmaregion.labour.95hi <- quantile(sigmaregion.labour , probs=c(.975))
sigmau.labour.95hi <- quantile(sigmau.labour , probs=c(.975))
sigmav.labour.95hi <- quantile(sigmav.labour , probs=c(.975))
b.loginvdistsize.libdem.95hi <- quantile(b.loginvdistsize.libdem , probs=c(.975))
b.pension.libdem.95hi <- quantile(b.pension.libdem , probs=c(.975))
b.worker.libdem.95hi <- quantile(b.worker.libdem , probs=c(.975))
sigmaregion.libdem.95hi <- quantile(sigmaregion.libdem , probs=c(.975))
sigmau.libdem.95hi <- quantile(sigmau.libdem , probs=c(.975))
sigmav.libdem.95hi <- quantile(sigmav.libdem , probs=c(.975))
b0.conser.95hi <- quantile(b0.conser , probs=c(.975))
b0.labour.95hi <- quantile(b0.labour , probs=c(.975))
b0.libdem.95hi <- quantile(b0.libdem , probs=c(.975))
b.incent.cola.95hi <- quantile(b.incent.cola*10 , probs=c(.975))
b.incent.coli.95hi <- quantile(b.incent.coli*10 , probs=c(.975))
b.incent.lali.95hi <- quantile(b.incent.lali*10 , probs=c(.975))
b.incent.laco.95hi <- quantile(b.incent.laco*10 , probs=c(.975))
b.incent.lico.95hi <- quantile(b.incent.lico*10 , probs=c(.975))
b.incent.lila.95hi <- quantile(b.incent.lila*10 , probs=c(.975))
b.incumb.conlab.95hi <- quantile(b.incumb.conlab , probs=c(.975))
b.incumb.conlib.95hi <- quantile(b.incumb.conlib , probs=c(.975))
b.incumb.labcon.95hi <- quantile(b.incumb.labcon , probs=c(.975))
b.incumb.lablib.95hi <- quantile(b.incumb.lablib , probs=c(.975))
b.incumb.libcon.95hi <- quantile(b.incumb.libcon , probs=c(.975))
b.incumb.liblab.95hi <- quantile(b.incumb.liblab , probs=c(.975))
b.turnout.conser.95hi <- quantile(b.turnout.conser, probs=c(.975))*100
b.turnout.labour.95hi <- quantile(b.turnout.labour, probs=c(.975))*100
b.turnout.libdem.95hi <- quantile(b.turnout.libdem, probs=c(.975))*100
b.independ.conser.95hi <- quantile(b.independ.conser , probs=c(.975))
b.independ.labour.95hi <- quantile(b.independ.labour , probs=c(.975))
b.independ.libdem.95hi <- quantile(b.independ.libdem , probs=c(.975))
sigma1.95hi <- quantile(sigma[1], probs=c(.975))
sigma2.95hi <- quantile(sigma[2], probs=c(.975))
sigma3.95hi <- quantile(sigma[3], probs=c(.975))
b.inc.conser.conser.95lo <- quantile(b.inc.conser.conser, probs=c(.025))
b.inc.labour.conser.95lo <- quantile(b.inc.labour.conser, probs=c(.025))
b.inc.libdem.conser.95lo <- quantile(b.inc.libdem.conser, probs=c(.025))
b.inc.conser.labour.95lo <- quantile(b.inc.conser.labour, probs=c(.025))
b.inc.labour.labour.95lo <- quantile(b.inc.labour.labour, probs=c(.025))
b.inc.libdem.labour.95lo <- quantile(b.inc.libdem.labour, probs=c(.025))
b.inc.conser.libdem.95lo <- quantile(b.inc.conser.libdem, probs=c(.025))
b.inc.labour.libdem.95lo <- quantile(b.inc.labour.libdem, probs=c(.025))
b.inc.libdem.libdem.95lo <- quantile(b.inc.libdem.libdem, probs=c(.025))
b.inc.conser.conser.95hi <- quantile(b.inc.conser.conser, probs=c(.975))
b.inc.labour.conser.95hi <- quantile(b.inc.labour.conser, probs=c(.975))
b.inc.libdem.conser.95hi <- quantile(b.inc.libdem.conser, probs=c(.975))
b.inc.conser.labour.95hi <- quantile(b.inc.conser.labour, probs=c(.975))
b.inc.labour.labour.95hi <- quantile(b.inc.labour.labour, probs=c(.975))
b.inc.libdem.labour.95hi <- quantile(b.inc.libdem.labour, probs=c(.975))
b.inc.conser.libdem.95hi <- quantile(b.inc.conser.libdem, probs=c(.975))
b.inc.labour.libdem.95hi <- quantile(b.inc.labour.libdem, probs=c(.975))
b.inc.libdem.libdem.95hi <- quantile(b.inc.libdem.libdem, probs=c(.975))
detach.bugs()

# prepare coefficient vectors
model3.01.med <- c(NA,b.loginvdistsize.conser.med,b.pension.conser.med,b.worker.conser.med,b.inc.conser.conser.med,b.inc.labour.conser.med, b.inc.libdem.conser.med,sigmaregion.conser.med,sigmau.conser.med,sigmav.conser.med,NA, b.loginvdistsize.labour.med,b.pension.labour.med,b.worker.labour.med,b.inc.conser.labour.med, b.inc.labour.labour.med,b.inc.libdem.labour.med,sigmaregion.labour.med,sigmau.labour.med,sigmav.labour.med,NA,b.loginvdistsize.libdem.med,b.pension.libdem.med,b.worker.libdem.med,b.inc.conser.libdem.med, b.inc.labour.libdem.med,b.inc.libdem.libdem.med, sigmaregion.libdem.med,sigmau.libdem.med,sigmav.libdem.med,NA,b.incent.cola.med,b.incent.coli.med,b.incent.laco.med,b.incent.lali.med,b.incent.lico.med,b.incent.lila.med,b.incumb.conlab.med,b.incumb.conlib.med,b.incumb.labcon.med,b.incumb.lablib.med,b.incumb.libcon.med,b.incumb.liblab.med,b.turnout.conser.med,b.turnout.labour.med,b.turnout.libdem.med,b.independ.conser.med,b.independ.labour.med,b.independ.libdem.med,b0.conser.med,b0.labour.med,b0.libdem.med,sigma1.med,sigma2.med,sigma3.med)
model3.01.95lo <-  c(NA,b.loginvdistsize.conser.95lo,b.pension.conser.95lo,b.worker.conser.95lo,b.inc.conser.conser.95lo,b.inc.labour.conser.95lo, b.inc.libdem.conser.95lo,sigmaregion.conser.95lo,sigmau.conser.95lo,sigmav.conser.95lo,NA, b.loginvdistsize.labour.95lo,b.pension.labour.95lo,b.worker.labour.95lo,b.inc.conser.labour.95lo, b.inc.labour.labour.95lo,b.inc.libdem.labour.95lo, sigmaregion.labour.95lo,sigmau.labour.95lo,sigmav.labour.95lo,NA,b.loginvdistsize.libdem.95lo,b.pension.libdem.95lo,b.worker.libdem.95lo,b.inc.conser.libdem.95lo, b.inc.labour.libdem.95lo,b.inc.libdem.libdem.95lo,sigmaregion.libdem.95lo,sigmau.libdem.95lo,sigmav.libdem.95lo,NA,b.incent.cola.95lo,b.incent.coli.95lo,b.incent.laco.95lo,b.incent.lali.95lo,b.incent.lico.95lo,b.incent.lila.95lo,b.incumb.conlab.95lo,b.incumb.conlib.95lo,b.incumb.labcon.95lo,b.incumb.lablib.95lo,b.incumb.libcon.95lo,b.incumb.liblab.95lo,b.turnout.conser.95lo,b.turnout.labour.95lo,b.turnout.libdem.95lo,b.independ.conser.95lo,b.independ.labour.95lo,b.independ.libdem.95lo,b0.conser.95lo,b0.labour.95lo,b0.libdem.95lo,sigma1.95lo,sigma2.95lo,sigma3.95lo)
model3.01.95hi <-  c(NA,b.loginvdistsize.conser.95hi,b.pension.conser.95hi,b.worker.conser.95hi,b.inc.conser.conser.95hi,b.inc.labour.conser.95hi, b.inc.libdem.conser.95hi,sigmaregion.conser.95hi,sigmau.conser.95hi,sigmav.conser.95hi,NA, b.loginvdistsize.labour.95hi,b.pension.labour.95hi,b.worker.labour.95hi,b.inc.conser.labour.95hi, b.inc.labour.labour.95hi,b.inc.libdem.labour.95hi, sigmaregion.labour.95hi,sigmau.labour.95hi,sigmav.labour.95hi,NA,b.loginvdistsize.libdem.95hi,b.pension.libdem.95hi,b.worker.libdem.95hi,b.inc.conser.libdem.95hi, b.inc.labour.libdem.95hi,b.inc.libdem.libdem.95hi,sigmaregion.libdem.95hi,sigmau.libdem.95hi,sigmav.libdem.95hi,NA,b.incent.cola.95hi,b.incent.coli.95hi,b.incent.laco.95hi,b.incent.lali.95hi,b.incent.lico.95hi,b.incent.lila.95hi,b.incumb.conlab.95hi,b.incumb.conlib.95hi,b.incumb.labcon.95hi,b.incumb.lablib.95hi,b.incumb.libcon.95hi,b.incumb.liblab.95hi,b.turnout.conser.95hi,b.turnout.labour.95hi,b.turnout.libdem.95hi,b.independ.conser.95hi,b.independ.labour.95hi,b.independ.libdem.95hi,b0.conser.95hi,b0.labour.95hi,b0.libdem.95hi,sigma1.95hi,sigma2.95hi,sigma3.95hi)



## generate plot ---------------------------------
pdf ("../figures/figure1_parameter_estimates.pdf", height=16, width=14, family="URWTimes")			
#layout(matrix(seq(1,2, by=1), 1, 2, byrow=T), heights = c(rep(1,1),rep(2,1)))
#layout.show(n=15)
par(mfrow=c(1,2))
par(mar=c(2,0,2,0), lheight = .8)
par(oma=c(2,15,1,1))
par(xpd=F)
plotCI(model3.97.med, y.axis, uiw=model3.97.95hi-model3.97.med, liw=model3.97.med-model3.97.95lo, err="x", axes=F, xlab = "", ylab = "", pch = 19, xlim = c(-1,1), cex=.7, ylim = c(min(y.axis+1), max(y.axis-1)), main = "", gap=0, sfrac=0, col="white")
axis(1,at = round(seq(-1,1, by = .2), digits=1), label = round(seq(-1,1, by = .2), digits=1), cex.axis=1,1, line=-0.1)
axis(2, at = y.axis, label = var.names, las = 1, tick = F, font=1, cex.axis=1.2, line=-1)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
abline(v=0, lty = 2)
abline(v=seq(-1,1, by = .2), lty = 2, col="darkgrey")
axis(3, at = 0, label = "1997", las = 0, tick=F, outer=T, cex.axis=1.5, line=-2)
plotCI(model3.97.med, y.axis, uiw=model3.97.95hi-model3.97.med, liw=model3.97.med-model3.97.95lo, err="x", axes=F, xlab = "", ylab = "", pch = 19, xlim = c(-1,1), cex=.8, ylim = c(min(y.axis), max(y.axis)), main = "", gap=0, sfrac=0, add=T)
rect(xleft = -1, ybottom = 18.5, xright = 1, ytop = 24.5, col=rgb(160,160,160,50, max=255), border = NA)
rect(xleft = -1.1, ybottom = 54.5, xright = 1.1, ytop = 56.5, col="white", border = NA)
rect(xleft = -1.1, ybottom = 44.5, xright = 1.1, ytop = 45.5, col="white", border = NA)
rect(xleft = -1.1, ybottom = 34.5, xright = 1.1, ytop = 35.5, col="white", border = NA)
rect(xleft = -1.1, ybottom = 24.5, xright = 1.1, ytop = 25.5, col="white", border = NA)
rect(xleft = -1.1, ybottom = 0, xright = -1, ytop = 56, col="white", border = NA)
rect(xleft = 1, ybottom = 0, xright = 1.1, ytop = 56, col="white", border = NA)
rect(xleft = -1, ybottom = 45.5, xright = 1, ytop = 54.5, border = "black")
rect(xleft = -1, ybottom = 35.5, xright = 1, ytop = 44.5, border = "black")
rect(xleft = -1, ybottom = 25.5, xright = 1, ytop = 34.5, border = "black")
rect(xleft = -1, ybottom = 0, xright = 1, ytop = 24.5, border = "black")
axis(3,at = round(seq(-1,1, by = .2), digits=1), label = round(seq(-1,1, by = .2), digits=1), cex.axis=1,1, line=-2.05)

plotCI(model3.01.med, y.axis, uiw=model3.01.95hi-model3.01.med, liw=model3.01.med-model3.01.95lo, err="x", axes=F, xlab = "", ylab = "", pch = 19, xlim = c(-1,1), cex=.7, ylim = c(min(y.axis+1), max(y.axis-1)), main = "", gap=0, sfrac=0, col="white")
axis(1,at = round(seq(-1,1, by = .2), digits=1), label = round(seq(-1,1, by = .2), digits=1), cex.axis=1,1, line=-0.1)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
abline(v=0, lty = 2)
abline(v=seq(-1,1, by = .2), lty = 2, col="darkgrey")
axis(3, at = 0, label = "2001", las = 0, tick=F, outer=T, cex.axis=1.5, line=-2)
plotCI(model3.01.med, y.axis, uiw=model3.01.95hi-model3.01.med, liw=model3.01.med-model3.01.95lo, err="x", axes=F, xlab = "", ylab = "", pch = 19, xlim = c(-1,1), cex=.8, ylim = c(min(y.axis), max(y.axis)), main = "", gap=0, sfrac=0, add=T)
rect(xleft = -1, ybottom = 18.5, xright = 1, ytop = 24.5, col=rgb(160,160,160,50, max=255), border = NA)
rect(xleft = -1.1, ybottom = 54.5, xright = 1.1, ytop = 56.5, col="white", border = NA)
rect(xleft = -1.1, ybottom = 44.5, xright = 1.1, ytop = 45.5, col="white", border = NA)
rect(xleft = -1.1, ybottom = 34.5, xright = 1.1, ytop = 35.5, col="white", border = NA)
rect(xleft = -1.1, ybottom = 24.5, xright = 1.1, ytop = 25.5, col="white", border = NA)
rect(xleft = -1.1, ybottom = 0, xright = -1, ytop = 56, col="white", border = NA)
rect(xleft = 1, ybottom = 0, xright = 1.1, ytop = 56, col="white", border = NA)
rect(xleft = -1, ybottom = 45.5, xright = 1, ytop = 54.5, border = "black")
rect(xleft = -1, ybottom = 35.5, xright = 1, ytop = 44.5, border = "black")
rect(xleft = -1, ybottom = 25.5, xright = 1, ytop = 34.5, border = "black")
rect(xleft = -1, ybottom = 0, xright = 1, ytop = 24.5, border = "black")
axis(3,at = round(seq(-1,1, by = .2), digits=1), label = round(seq(-1,1, by = .2), digits=1), cex.axis=1,1, line=-2.05)
dev.off()



