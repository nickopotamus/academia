library (mixtools)
library (tseries)
library (nor1mix)

d.mix <- function (mdl, dat, k, N=1000) {
    d.x <- seq(min(dat),max(dat),length.out=N)
    data.frame (x=d.x, y=dnorm(d.x, mdl$mu[k], mdl$sigma[k]))
}

mix.stats <- function (x) {
  print (Iter)
  Iter <<- Iter + 1
  mdl <- norMixEM (x, 2, trace=0)
  mdl[,"w"]
}

# Split data by intervention phase
a2 <- read.csv ("a2_all_nights.csv")
pre <- subset(a2, a2$int==0)
post <- subset(a2, a2$int==1)
late <- subset(a2, a2$int==2)


## PEAK LEVELS

# Decompose peak levels
mdl.C.pre  <- normalmixEM(pre$peak, k=2)
mdl.C.post <- normalmixEM(post$peak, k=2)
mdl.C.late <- normalmixEM(late$peak, k=2)

summary(mdl.C.pre)
summary(mdl.C.post)
summary(mdl.C.late)

# Plot peak levels
par (mfrow=c(3,1))
plot (mdl.C.pre, whichplots=2)
plot (mdl.C.post, whichplots=2)
plot (mdl.C.late, whichplots=2)
par (mfrow=c(1,1))


#bs.N      <- 1000
#bs.A.pre  <- boot.se (mdl.A.pre,  bs.N)
#bs.A.post <- boot.se (mdl.A.post, bs.N)
#bs.C.pre  <- boot.se (mdl.C.pre,  bs.N)
#bs.C.post <- boot.se (mdl.C.post, bs.N)



# Ambient mixtools

mdl.A.pre  <- normalmixEM(pre$ambient, k=2)
mdl.A.post <- normalmixEM(post$ambient, k=2)
mdl.A.late <- normalmixEM(late$ambient, k=2)

par (mfrow=c(3,1))
plot (mdl.A.pre, whichplots=2)
plot (mdl.A.post, whichplots=2)
plot (mdl.A.late, whichplots=2)
dpar (mfrow=c(1,1))

# Nor1mix

Iter <- 0
bs.A.pre <- tsbootstrap(pre$ambient, nb=100, statistic=mix.stats)
bs.A.post <- tsbootstrap(post$ambient, nb=100, statistic=mix.stats)
bs.A.late <- tsbootstrap(late$ambient, nb=100, statistic=mix.stats)


mix.stats <- function (x) {
    mdl <- normalmixEM(x, k=2)
    if (mdl$mu[1] < mdl$mu[2])
        c(mdl$lambda[1], mdl$lambda[2])
    else
        c(mdl$lambda[2], mdl$lambda[1])
}
mean.sd <- function (x) {
    c(mean(x), sd(x))
}


# Get lambdas of each peak+/-SE
mean (bs.A.pre$lambda[1,])
bs.A.pre$lambda.se[1]
mean (bs.A.pre$lambda[2,])
bs.A.pre$lambda.se[2]

mean (bs.A.post$lambda[1,])
bs.A.post$lambda.se[1]
mean (bs.A.post$lambda[2,])
bs.A.post$lambda.se[2]

bbh <- function (x, y) {
    h1 <- hist(x, plot=FALSE)
    h2 <- hist(y, plot=FALSE)
    
    h2$counts <- -h2$counts
    ymax <- max(h1$counts)
    ymin <- min(h2$counts)
    
    X <- c(h1$breaks, h2$breaks)
    xmax <- max(X)
    xmin <- min(X)
    
    plot (h1, ylim=c(ymin,ymax), col="green", xlim=c(xmin, xmax))
    lines (h2, col="blue")
}

dmult <- function (x) {
    h <- hist(x, plot=FALSE)
    (h$counts / h$density)[1]
}

plot.prepost.dens <- function (pre.mdl, pre.data, post.mdl, post.data, main="Pre-post", xlab="X", ylab="Frequency", xlim=range(pre.data,post.data)) {
    d1 <- density (pre.data, plot=FALSE)
    d2 <- density (post.data, plot=FALSE)
    
    ymin <- min(d1$y, d2$y)
    ymax <- max(d1$y, d2$y)
    
    d2$y <- -d2$y
    
    plot (d1, ylim=c(ymin,ymax), col="blue", xlim=xlim, main=main, xlab=xlab)
    lines (d2, col="green")
    
    curve (dnorm(x, mean=pre.mdl[1,"mu"], sd=pre.mdl[1,"sigma"]),
           from=min(pre.data), to=max(pre.data), add=T, col="red")
    curve (dnorm(x, mean=pre.mdl[2,"mu"], sd=pre.mdl[2,"sigma"]),
           from=min(pre.data), to=max(pre.data), add=T, col="purple")
    curve (dnorm(x, mean=post.mdl[1,"mu"], sd=post.mdl[1,"sigma"])*-1,
           from=min(post.data), to=max(post.data), add=T, col="red")
    curve (dnorm(x, mean=post.mdl[2,"mu"], sd=post.mdl[2,"sigma"])*-1,
           from=min(post.data), to=max(post.data), add=T, col="purple")
}

plot.prepost.mix <- function (pre.mdl, pre.data, post.mdl, post.data, main="Pre-post", xlab="X", ylab="Frequency", xlim=range(pre.data,post.data)) {
    h1 <- hist(pre.data, plot=FALSE)
    h2 <- hist(post.data, plot=FALSE)
    
    h2$counts <- -h2$counts
    ymax <- max(h1$counts)
    ymin <- min(h2$counts)
    
    #X <- c(h1$breaks, h2$breaks)
    #xmax <- max(X)
    #xmin <- min(X)
    xmin <- xlim[1]
    xmax <- xlim[2]
    
    plot (h1, ylim=c(ymin,ymax), col="blue", xlim=xlim, main=main, xlab=xlab)
    lines (h2, col="green")
    
    m1 <- length(pre.data)
    m2 <- -length(post.data)

    curve (dnorm(x, mean=pre.mdl[1,"mu"], sd=pre.mdl[1,"sigma"])*m1,
           from=min(pre.data), to=max(pre.data), add=T, col="red")
    curve (dnorm(x, mean=pre.mdl[2,"mu"], sd=pre.mdl[2,"sigma"])*m1,
           from=min(pre.data), to=max(pre.data), add=T, col="purple")
    curve (dnorm(x, mean=post.mdl[1,"mu"], sd=post.mdl[1,"sigma"])*m2,
           from=min(post.data), to=max(post.data), add=T, col="red")
    curve (dnorm(x, mean=post.mdl[2,"mu"], sd=post.mdl[2,"sigma"])*m2,
           from=min(post.data), to=max(post.data), add=T, col="purple")
}
plot.mixes <- function (dat, mdl) {
    plot (density(dat), ylim=c(0,max(density(dat)$y)*2))
    for (i in 1:dim(mdl)[1]) {
        curve (dnorm(x, mean=mdl[i,"mu"], sd=mdl[i,"sigma"]),
               from=min(dat), to=max(dat), add=T, col=i+1)
    }
}

mult <- length(pre$dBAslow)
curve (dnorm(x, mean=mdl.A.pre[1,"mu"], sd=mdl.A.pre[1,"sigma"])*mult, 
       from=min(pre$dBAslow), to=max(pre$dBAslow), add=T)
curve (dnorm(x, mean=mdl.A.pre[2,"mu"], sd=mdl.A.pre[2,"sigma"])*mult, 
       from=min(pre$dBAslow), to=max(pre$dBAslow), add=T)

mult <- length(post$dBAslow)
curve (-dnorm(x, mean=mdl.A.post[1,"mu"], sd=mdl.A.post[1,"sigma"])*mult, 
       from=min(post$dBAslow), to=max(post$dBAslow), add=T)
curve (-dnorm(x, mean=mdl.A.post[2,"mu"], sd=mdl.A.post[2,"sigma"])*mult, 
       from=min(post$dBAslow), to=max(post$dBAslow), add=T)

mu.pre     <- mdl.A.pre[1,"mu"]
mu.post    <- mdl.A.post[1,"mu"]
sigma.pre  <- mdl.A.pre[1,"sigma"]
sigma.post <- mdl.A.post[1,"sigma"]

welch <- function (mean1, sd1, N1, mean2, sd2, N2) {
    (mean1 - mean) / 
        sqrt(
            (sigma1/N1) +
            (sigma2/N2)
        )
}
