#!/usr/bin/Rscript --no-init-file

# setup
require("mixtools", quietly=TRUE)

# definitions
input = "a2_nightsonly_sec.csv"                     # input data file
output = "soundear2.pdf"                             # output figure name
binwidth = 0.1                                      # histogram bin widths
xlim = c(45,70)                                     # x-axis plot limits
ylim = c(0,36000*binwidth)                          # y-axis plot limits
sublinecols = c("red","green1","blue")              # population line colours
sublinewidths = c(2,2,2)                            # population line widths
sublinetypes = c(1,1,1)                             # population line types
alllinecol = "purple"                               # total combined line colour
alllinewidth = 2                                    # total combined line width
alllinetype = 1                                     # total combined line type
int0col = "grey75"                                  # int=0 histogram fill colour
int1col = "grey75"                                  # int=1 histogram fill colour
bordercol = "white"                                 # int0/int1 histogram border colour
epsilon = 1e-5                                      # fitting convergance criterion
breaks = seq(xlim[1], xlim[2], by=binwidth)         # histogram bin breaks
x = seq(breaks[1],breaks[length(breaks)],by=0.1)    # gaussian fit curve x data points

# data input
dat = read.csv(input)
bad = which(dat[,"dBAslow"] < xlim[1] | dat[,"dBAslow"] > xlim[2])
dat = dat[-bad,]
int0 = dat[which(dat[,"int"]==0),"dBAslow"]
int1 = dat[which(dat[,"int"]==1),"dBAslow"]

# Gaussian fitting
cat("int=0 "); fit0 = normalmixEM(int0, epsilon=epsilon, lambda=c(0.4,0.2,0.4), mu=c(51,58,60), sigma=c(2.2,0.6,3.7))
cat("int=1 "); fit1 = normalmixEM(int1, epsilon=epsilon, lambda=c(0.5,0.5), mu=c(51,57), sigma=c(1.7,4.1))

# fitted population data
lambda = list(fit0$lambda, fit1$lambda)
mu = list(fit0$mu, fit1$mu)
sigma = list(fit0$sigma, fit1$sigma)

# generate int0 gaussian data
int0gausssub = {}
int0gausstotal = rep(0, length(x))
for(i in 1:length(lambda[[1]])){
    scalefact = binwidth * lambda[[1]][i] * length(int0)
    temp = dnorm(x=x, mean=mu[[1]][i], sd=sigma[[1]][i]) * scalefact
    int0gausssub = c(int0gausssub, list(temp))
    int0gausstotal = int0gausstotal + temp
}

# generate int1 gaussian data
int1gausssub = {}
int1gausstotal = rep(0, length(x))
for(i in 1:length(lambda[[2]])){
    scalefact = binwidth * lambda[[2]][i] * length(int1)
    temp = dnorm(x=x, mean=mu[[2]][i], sd=sigma[[2]][i]) * scalefact
    int1gausssub = c(int1gausssub, list(temp))
    int1gausstotal = int1gausstotal + temp
}

# open png
#png(file=output, width=8, height=6, units="in", res=300)
cairo_pdf(file=output, width=8, height=6)

# plot setup
par("mar"=c(0,0,0,0))
par("oma"=c(4,4,3,1))
layout(matrix(1:2))

# plot int0
intid = 1
hist(int0, breaks=breaks, xlim=xlim, ylim=ylim, axes=FALSE, main="", col=int0col, border=bordercol)
axis(side=3); axis(side=2); box()
lines(x, int0gausstotal, lty=alllinetype, lwd=alllinewidth, col=alllinecol)
for(i in 1:length(lambda[[intid]])){
    lines(x, int0gausssub[[i]], lty=sublinetypes[i], lwd=sublinewidths[i], col=sublinecols[i])
}
legend("topright", legend=c(paste(c("Pop. I : ","Pop. IIa : ","Pop. IIb : "), formatC(mu[[intid]], digits=1, format="f"), " dB (", formatC(lambda[[intid]]*100, digits=0, format="f"), "%)", sep=""), "Total"), lty=c(sublinetypes[1:length(lambda[[intid]])], alllinetype), col=c(sublinecols[1:length(lambda[[intid]])], alllinecol), lwd=c(sublinewidths[1:length(lambda[[intid]])], alllinewidth), bty="n")

# plot int1
intid = 2
hist(int1, breaks=breaks, xlim=xlim, ylim=ylim, axes=FALSE, main="", col=int1col, border=bordercol)
axis(side=1); axis(side=2); box()
lines(x, int1gausstotal, lty=alllinetype, lwd=alllinewidth, col=alllinecol)
for(i in 1:length(lambda[[intid]])){
    lines(x, int1gausssub[[i]], lty=sublinetypes[i], lwd=sublinewidths[i], col=sublinecols[i])
}
legend("topright", legend=c(paste(c("Pop. I : ","Pop. II : "), formatC(mu[[intid]], digits=1, format="f"), " dB (", formatC(lambda[[intid]]*100, digits=0, format="f"), "%)", sep=""), "Total"), lty=c(sublinetypes[1:length(lambda[[intid]])], alllinetype), col=c(sublinecols[1:length(lambda[[intid]])], alllinecol), lwd=c(sublinewidths[1:length(lambda[[intid]])], alllinewidth), bty="n")

# finish up
layout(1)
mtext(side=1, text="Background sound level dB(A) slow", line=2)
mtext(side=2, text="Frequency", line=2.5)
graphics.off()




