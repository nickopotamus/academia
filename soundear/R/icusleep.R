source ("day.R")
library (mixtools)

df <- read.filemap (fn=max)

df.max <- sample.set (df, res="1 min", fn=max)
plot.24h (df.max, fn=max, int.labels=c("Control", "Warnings", "Follow-up"))

df.n <- nightshift (df)
ggplot (df.n, aes(x=A.fast)) + 
    geom_density (aes(group=int, fill=as.factor(int)), alpha=0.25)

ss.0 <- subset (df, int==0)
ss.1 <- subset (df, int==1)
ss.2 <- subset (df, int==2)

library (urca)
summary (ur.df (ss.0$A.fast, lags=1))
summary (ur.df (ss.1$A.fast, lags=1))
summary (ur.df (ss.2$A.fast, lags=1))

par (mfrow=c(3,2), mar=c(2,1,2,2))
Acf(diff(ss.0$A.fast)); Pacf(diff(ss.0$A.fast))
Acf(diff(ss.1$A.fast)); Pacf(diff(ss.1$A.fast))
Acf(diff(ss.2$A.fast)); Pacf(diff(ss.2$A.fast))
par (mfrow=c(1,1), mar=c(5,4,4,2)+0.1)

dm.0 <- with (ss.0, normalmixEM(A.fast, k=2))
dm.1 <- with (ss.1, normalmixEM(A.fast, k=2))
dm.2 <- with (ss.2, normalmixEM(A.fast, k=2))

bs <- with (ss.0, boot.comp (y=A.fast, max.comp=5, mix.type="normalmix"))
