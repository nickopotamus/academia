library (dplyr)
library (ggplot2)
library (xts)
library (depmixS4)
options(stringsAsFactors = FALSE)

read.day <- function (csv, res="1 sec", int=0, threshold=50, fn=mean) {
    din <- read.csv (csv, sep=";")
    din$int <- int
    din <- subset (din, din$LAF > threshold)
    day <- data.frame (t = strptime(paste(din$Date, din$Time), "%d-%m-%Y %H:%M:%S"), A.fast = din$LAF, int=din$int)
    sample.day (day, res, fn)

}
sample.day <- function (day, res="1 sec", fn=mean) {
    as.data.frame (day %>% 
                    group_by (t=cut(t, breaks=res)) %>%
                    summarise (A.fast=fn(A.fast), int=mean(int)) %>%
                    mutate (t=as.POSIXct(t)))
}
sample.set <- function (day.set, res="1 sec", fn=mean, int.range=0:2) {
    out <- data.frame()
    for (i in int.range) {
        ss <- subset (day.set, int==i)
        out <- rbind(out, sample.day (ss, res, fn))
    }
    out
}
nightshift <- function (df, earlier=7, later=23) {
    as.data.frame (df %>%
                       mutate (H=as.numeric(strftime (t, "%H"))+
                                   as.numeric(strftime(t, "%m"))/60) %>%
                       filter (H <= earlier | H >= later))
}
se <- function (x) {
    sqrt (var(x) / length(x))
}
fn.24h <- function (df, fn=mean) {
    as.data.frame (df %>%
          mutate (H=as.numeric(strftime (t, "%H")) + 
                      as.numeric (strftime (t, "%M"))/60 + 
                      as.numeric (strftime (t, "%S"))/3600) %>%
          group_by (H, int) %>% 
          summarise (A.fn = fn(A.fast), t=t[0]))
}
plot.24h <- function (df, 
                      y.label="dB", 
                      x.label="Hour", 
                      int.labels=min(df$int):max(df$int), 
                      fn=mean,
                      alpha=0.5) {
    ggplot (fn.24h(df,fn), aes(H, A.fn)) + 
        geom_line(aes(col=as.factor(int))) +
        ylab (y.label) + xlim(0, 24) +
        xlab (x.label) +
        guides(colour = guide_legend(title="Intervention", override.aes = list(size=3))) +
        scale_shape_discrete (name="Intervention", 
                               breaks=min(df$int):max(df$int),
                               labels=int.labels) +
         scale_colour_discrete (name="Intervention", 
                               breaks=min(df$int):max(df$int),
                               labels=int.labels)
}
read.filemap <- function (csv="filemap.csv", res="1 sec", threshold=50, fn=mean) {
    df <- data.frame ()
    fm <- read.csv ("filemap.csv")
    for (i in 1:nrow(fm)) {
        print (fm[i,]$file)
        day <- read.day (fm[i,]$file, int=fm[i,]$int, res=res, threshold=threshold)
        df <- rbind (df, day)
    }
    df
}
Arima.diag <- function (mdl) {
    par (mfrow=c(2,2))
    Acf (mdl$residuals); Pacf (mdl$residuals)
    qqnorm (mdl$residuals); 
    par (mfrow=c(1,1))
}
