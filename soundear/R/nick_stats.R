library (mixtools)

# Definitions
binwidth = 0.1
xlim = c(45,70)
xlim2 = c(60,75)
ylim = c(0,36000*binwidth)

# Import data
pre=read.csv ("a2_int0.csv")
post=read.csv ("a2_int1.csv")
late=read.csv ("a2_int2.csv")

pre.night = subset(pre, night==1)
post.night = subset(post, night==1)
late.night = subset(late, night==1)

# Sanitise data - ambient sound
bad.pre = which(pre.night[,"ambient"] < xlim[1] | pre.night[,"ambient"] > xlim[2])
bad.post = which(post.night[,"ambient"] < xlim[1] | post.night[,"ambient"] > xlim[2])
bad.late = which(late.night[,"ambient"] < xlim[1] | late.night[,"ambient"] > xlim[2])

pre.bad=pre.night[-bad.pre]
post.bad=post.night[-bad.post]
late.bad=late.night[-bad.late]

int0 = pre.night[-bad.pre,"ambient"]
int1 = post.night[-bad.post,"ambient"]
int2 = late.night[-bad.late,"ambient"]

# Log transform into power
int0p = (10^(int0/10))*(20E-6)
int1p = (10^(int1/10))*(20E-6)
int2p = (10^(int2/10))*(20E-6)

# "Quiet" subsets (<55.0 dB)
pre.small=subset(pre.night, ambient<55.0)
post.small=subset(post.night, ambient<55.0)
late.small=subset(late.night, ambient<55.0)

# Sanitise data - peak sound
bad.pre2 = which(pre.night[,"peak"] < xlim2[1] | pre.night[,"peak"] > xlim2[2])
bad.post2 = which(post.night[,"peak"] < xlim2[1] | post.night[,"peak"] > xlim2[2])
bad.late2 = which(late.night[,"peak"] < xlim2[1] | late.night[,"peak"] > xlim2[2])

pre.bad2 = pre.night[-bad.pre2]
post.bad2 = post.night[-bad.post2]
late.bad2 = late.night[-bad.late2]

int0b = pre.night[-bad.pre2,"peak"]
int1b = post.night[-bad.post2,"peak"]
int2b = late.night[-bad.late2,"peak"]

# Frequency plots
par (mfrow=c(3,1))
hist(int0)
hist(int1)
hist(int2)
par (mfrow=c(1,1))

par (mfrow=c(3,1))
hist(int0b, xlab="Peak sound /dB", ylab="Density", main="Distribution of peak sound before intervention", freq=F)
hist(int1b, xlab="Peak sound /dB", ylab="Density", main="Distribution of peak sound after intervention", freq=F)
hist(int2b, xlab="Peak sound /dB", ylab="Density", main="Distribution of peak sound after intervention operating for four months", freq=F)
par (mfrow=c(1,1))

# Density plots
d.pre = density(int0)
d.post = density(int1)
d.late = density(int2)

par (mfrow=c(3,1))
plot(d.pre)
plot(d.post)
plot(d.late)
par (mfrow=c(1,1))

# Decompose peak levels
fit0 <- normalmixEM(int0, k=2)
fit1 <- normalmixEM(int1, k=2)
fit2 <- normalmixEM(int2, k=2)

fit0p <- normalmixEM(int0p, k=2)
fit1p <- normalmixEM(int1p, k=2)
fit2p <- normalmixEM(int2p, k=2)

summary(fit0)
summary(fit1)
summary(fit2)

summary(fit0p)
summary(fit1p)
summary(fit2p)


par (mfrow=c(3,1))
plot (fit0, whichplots=2, xlab2="Ambient sound /dB", ylab2="Density", main2="Distribution of ambient sound before intervention")
plot (fit1, whichplots=2, xlab2="Ambient sound /dB", ylab2="Density", main2="Distribution of ambient sound after intervention")
plot (fit2, whichplots=2, xlab2="Ambient sound /dB", ylab2="Density", main2="Distribution of ambient sound after intervention operating for four months")
par (mfrow=c(1,1))

par (mfrow=c(3,1))
plot (fit0, whichplots=2, xlab2="Ambient sound /dB", ylab2="Density", main2="Distribution of ambient sound before intervention")
plot (fit1, whichplots=2, xlab2="Ambient sound /dB", ylab2="Density", main2="Distribution of ambient sound after intervention")
plot (fit2, whichplots=2, xlab2="Ambient sound /dB", ylab2="Density", main2="Distribution of ambient sound after intervention operating for four months")
par (mfrow=c(1,1))


# fitted population data
lambda = list(fit0$lambda, fit1$lambda, fit2$lambda)
mu = list(fit0$mu, fit1$mu, fit2$mu)
sigma = list(fit0$sigma, fit1$sigma, fit2$sigma)