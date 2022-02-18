# Ambient data only

library (mixtools)

# Definitions
binwidth = 0.1
xlim = c(35,70)
ylim = c(0,36000*binwidth)

# Import data
pre=read.csv ("a2_int0.csv")
post=read.csv ("a2_int1.csv")
late=read.csv ("a2_int2.csv")
pre.night = subset(pre, night==1)
post.night = subset(post, night==1)
late.night = subset(late, night==1)

# Sanitise data
bad.pre = which(pre.night[,"ambient"] < xlim[1] | pre.night[,"ambient"] > xlim[2])
bad.post = which(post.night[,"ambient"] < xlim[1] | post.night[,"ambient"] > xlim[2])
bad.late = which(late.night[,"ambient"] < xlim[1] | late.night[,"ambient"] > xlim[2])
pre.night=pre.night[-bad.pre]
post.night=post.night[-bad.post]
late.night=late.night[-bad.late]

# Ambient by day only
int0 = pre.night[-bad.pre,c("day","ambient")]
int1 = post.night[-bad.post,c("day","ambient")]
int2 = late.night[-bad.late,c("day","ambient")]

# Fit models
fit0 <- normalmixEM(int0$ambient, k=2)
fit1 <- normalmixEM(int1$ambient, k=2)
fit2 <- normalmixEM(int2$ambient, k=2)

summary(fit0)
summary(fit1)
summary(fit2)

par (mfrow=c(3,1))
plot (fit0, whichplots=2, xlab2="Ambient sound /dB", ylab2="Density", main2="Distribution of ambient sound before intervention")
plot (fit1, whichplots=2, xlab2="Ambient sound /dB", ylab2="Density", main2="Distribution of ambient sound after intervention")
plot (fit2, whichplots=2, xlab2="Ambient sound /dB", ylab2="Density", main2="Distribution of ambient sound after intervention operating for four months")
par (mfrow=c(1,1))


# Split by day
## int0
int0.1 = subset(int0, day=="1")
int0.2 = subset(int0, day=="2")
int0.3 = subset(int0, day=="3")
int0.4 = subset(int0, day=="4")
int0.5 = subset(int0, day=="5")
int0.6 = subset(int0, day=="6")
int0.7 = subset(int0, day=="7")
int0.8 = subset(int0, day=="8")
int0.9 = subset(int0, day=="9")

int0.1.60=subset(int0.1, ambient<60.0); int0.1.55=subset(int0.1, ambient<55.0); int0.1.50=subset(int0.1, ambient<50.0); int0.1.45=subset(int0.1, ambient<45.0); int0.1.40=subset(int0.1, ambient<40.0)
int0.2.60=subset(int0.2, ambient<60.0); int0.2.55=subset(int0.2, ambient<55.0); int0.2.50=subset(int0.2, ambient<50.0); int0.2.45=subset(int0.2, ambient<45.0); int0.2.40=subset(int0.2, ambient<40.0)
int0.3.60=subset(int0.3, ambient<60.0); int0.3.55=subset(int0.3, ambient<55.0); int0.3.50=subset(int0.3, ambient<50.0); int0.3.45=subset(int0.3, ambient<45.0); int0.3.40=subset(int0.3, ambient<40.0)
int0.4.60=subset(int0.4, ambient<60.0); int0.4.55=subset(int0.4, ambient<55.0); int0.4.50=subset(int0.4, ambient<50.0); int0.4.45=subset(int0.4, ambient<45.0); int0.4.40=subset(int0.4, ambient<40.0)
int0.5.60=subset(int0.5, ambient<60.0); int0.5.55=subset(int0.5, ambient<55.0); int0.5.50=subset(int0.5, ambient<50.0); int0.5.45=subset(int0.5, ambient<45.0); int0.5.40=subset(int0.5, ambient<40.0)
int0.6.60=subset(int0.6, ambient<60.0); int0.6.55=subset(int0.6, ambient<55.0); int0.6.50=subset(int0.6, ambient<50.0); int0.6.45=subset(int0.6, ambient<45.0); int0.6.40=subset(int0.6, ambient<40.0)
int0.7.60=subset(int0.7, ambient<60.0); int0.7.55=subset(int0.7, ambient<55.0); int0.7.50=subset(int0.7, ambient<50.0); int0.7.45=subset(int0.7, ambient<45.0); int0.7.40=subset(int0.7, ambient<40.0)
int0.8.60=subset(int0.8, ambient<60.0); int0.8.55=subset(int0.8, ambient<55.0); int0.8.50=subset(int0.8, ambient<50.0); int0.8.45=subset(int0.8, ambient<45.0); int0.8.40=subset(int0.8, ambient<40.0)
int0.9.60=subset(int0.9, ambient<60.0); int0.9.55=subset(int0.9, ambient<55.0); int0.9.50=subset(int0.9, ambient<50.0); int0.9.45=subset(int0.9, ambient<45.0); int0.9.40=subset(int0.9, ambient<40.0)

## int1
int1.1 = subset(int1, day=="9")
int1.2 = subset(int1, day=="10")
int1.3 = subset(int1, day=="11")
int1.4 = subset(int1, day=="12")
int1.5 = subset(int1, day=="13")
int1.6 = subset(int1, day=="14")
int1.7 = subset(int1, day=="15")
int1.8 = subset(int1, day=="16")

int1.1.60=subset(int1.1, ambient<60.0); int1.1.55=subset(int1.1, ambient<55.0); int1.1.50=subset(int1.1, ambient<50.0); int1.1.45=subset(int1.1, ambient<45.0); int1.1.40=subset(int1.1, ambient<40.0)
int1.2.60=subset(int1.2, ambient<60.0); int1.2.55=subset(int1.2, ambient<55.0); int1.2.50=subset(int1.2, ambient<50.0); int1.2.45=subset(int1.2, ambient<45.0); int1.2.40=subset(int1.2, ambient<40.0)
int1.3.60=subset(int1.3, ambient<60.0); int1.3.55=subset(int1.3, ambient<55.0); int1.3.50=subset(int1.3, ambient<50.0); int1.3.45=subset(int1.3, ambient<45.0); int1.3.40=subset(int1.3, ambient<40.0)
int1.4.60=subset(int1.4, ambient<60.0); int1.4.55=subset(int1.4, ambient<55.0); int1.4.50=subset(int1.4, ambient<50.0); int1.4.45=subset(int1.4, ambient<45.0); int1.4.40=subset(int1.4, ambient<40.0)
int1.5.60=subset(int1.5, ambient<60.0); int1.5.55=subset(int1.5, ambient<55.0); int1.5.50=subset(int1.5, ambient<50.0); int1.5.45=subset(int1.5, ambient<45.0); int1.5.40=subset(int1.5, ambient<40.0)
int1.6.60=subset(int1.6, ambient<60.0); int1.6.55=subset(int1.6, ambient<55.0); int1.6.50=subset(int1.6, ambient<50.0); int1.6.45=subset(int1.6, ambient<45.0); int1.6.40=subset(int1.6, ambient<40.0)
int1.7.60=subset(int1.7, ambient<60.0); int1.7.55=subset(int1.7, ambient<55.0); int1.7.50=subset(int1.7, ambient<50.0); int1.7.45=subset(int1.7, ambient<45.0); int1.7.40=subset(int1.7, ambient<40.0)
int1.8.60=subset(int1.8, ambient<60.0); int1.8.55=subset(int1.8, ambient<55.0); int1.8.50=subset(int1.8, ambient<50.0); int1.8.45=subset(int1.8, ambient<45.0); int1.8.40=subset(int1.8, ambient<40.0)

## int2
int2.1 = subset(int2, day=="23")
int2.2 = subset(int2, day=="24")
int2.3 = subset(int2, day=="25")
int2.4 = subset(int2, day=="26")
int2.5 = subset(int2, day=="27")
int2.6 = subset(int2, day=="28")
int2.7 = subset(int2, day=="29")
int2.8 = subset(int2, day=="30")

int2.1.60=subset(int2.1, ambient<60.0); int2.1.55=subset(int2.1, ambient<55.0); int2.1.50=subset(int2.1, ambient<50.0); int2.1.45=subset(int2.1, ambient<45.0); int2.1.40=subset(int2.1, ambient<40.0)
int2.2.60=subset(int2.2, ambient<60.0); int2.2.55=subset(int2.2, ambient<55.0); int2.2.50=subset(int2.2, ambient<50.0); int2.2.45=subset(int2.2, ambient<45.0); int2.2.40=subset(int2.2, ambient<40.0)
int2.3.60=subset(int2.3, ambient<60.0); int2.3.55=subset(int2.3, ambient<55.0); int2.3.50=subset(int2.3, ambient<50.0); int2.3.45=subset(int2.3, ambient<45.0); int2.3.40=subset(int2.3, ambient<40.0)
int2.4.60=subset(int2.4, ambient<60.0); int2.4.55=subset(int2.4, ambient<55.0); int2.4.50=subset(int2.4, ambient<50.0); int2.4.45=subset(int2.4, ambient<45.0); int2.4.40=subset(int2.4, ambient<40.0)
int2.5.60=subset(int2.5, ambient<60.0); int2.5.55=subset(int2.5, ambient<55.0); int2.5.50=subset(int2.5, ambient<50.0); int2.5.45=subset(int2.5, ambient<45.0); int2.5.40=subset(int2.5, ambient<40.0)
int2.6.60=subset(int2.6, ambient<60.0); int2.6.55=subset(int2.6, ambient<55.0); int2.6.50=subset(int2.6, ambient<50.0); int2.6.45=subset(int2.6, ambient<45.0); int2.6.40=subset(int2.6, ambient<40.0)
int2.7.60=subset(int2.7, ambient<60.0); int2.7.55=subset(int2.7, ambient<55.0); int2.7.50=subset(int2.7, ambient<50.0); int2.7.45=subset(int2.7, ambient<45.0); int2.7.40=subset(int2.7, ambient<40.0)
int2.8.60=subset(int2.8, ambient<60.0); int2.8.55=subset(int2.8, ambient<55.0); int2.8.50=subset(int2.8, ambient<50.0); int2.8.45=subset(int2.8, ambient<45.0); int2.8.40=subset(int2.8, ambient<40.0)

# Compute averages
av=read.csv ("averages.csv")
av$db = as.factor(av$db)
av$int = as.factor(av$int)

# Build subsets for comparisons
av.45=subset(av, db=="45")
av.50=subset(av, db=="50")
av.55=subset(av, db=="55")
av.60=subset(av, db=="60")

# Pairwise comparisons
TukeyHSD(aov(av$perc ~ av$int+av$db))
TukeyHSD(aov(av.45$perc ~ av.45$int))
TukeyHSD(aov(av.50$perc ~ av.50$int))
TukeyHSD(aov(av.55$perc ~ av.55$int))
TukeyHSD(aov(av.60$perc ~ av.60$int))
