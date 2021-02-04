rm(list= ls())
library(data.table)
source('00_clean_trips.R')


plot_coverage<-function(samp, mu, sigprof, sigboost = 0L,m = 1000, n = 120){
    prof = intervals(sigprof, m, n)
    if(sigboost>0)
        boost = intervals(sigboost, m, n)
    A = trips[trip %in% samp][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
    if(sigboost>0)
        A[, mean(abs(tt-mu) <= boost[N]),N][N<=n][, lines(N, 100*V1, lwd = 2, lty=4, col = 'darkred')]
    A[, mean(abs(tt-mu) <= prof[N]),N][N<=n][, lines(N, 100*V1, lwd =1, lty = 2, col =  'black')]
    mtext("Empirical Coverage (%)",side=4,line=4, cex = 2) 
    axis(4, ylim=c(0,100),las=1, cex.axis = 1.5, cex.lab= 2)
    axis(4, at= 95,las=1, cex.axis = 1.5, cex.lab= 2)
    abline(h=95, col = 'black', lty=3, lwd=1)
}

trips_hist<-function(samp,  mu, sigprof, sigboot=0L,m, til= '',nbins = NULL){
    A = trips[trip %in% samp][order(trip, time)][ ,  .(.N, tt = sum(tt)) ,trip]
    S = A[, (tt - N*mu)/sqrt(N*(1+1/m))]
    par(mar = c(5,5,0,5)+0.1)
    hist(S, freq= FALSE,
         main = '',
         xlab = 'Normalized travel time (seconds)',
         ylab = 'Density',
         col = 'darkgrey', ylim = c(0,0.019), lty = 2,
         cex.lab=2, cex.axis=1.5,axes=TRUE,
         yaxs = 'i',
         xaxs = 'i',
         xlim = 180*c(-1,1),
         breaks = if(is.null(nbins)) 20 else nbins )
    x = seq(-10, 10, 0.1)
    if(sigboot>0) fboot = dt(x, m-1)/sigboot  else fboot=0
    fprof = dt(x, m-1)/sigprof
    q = qt(0.975, m-1)
    if(sigboot>0) lines(sigboot*x, fboot, col = 'darkred', type ='l', lwd = 2)
    lines(sigprof*x, fprof, col = 'black', type ='l', lwd = 2)
    empirical = quantile(S, c(0.025, .975))
    #abline(v = empirical, col = 'darkgrey', lty = 4, lwd = 3)
    #abline(v = sigboot*q*c(-1,1), col = 'black', lty =1, lwd =3)
                                        #abline(v = sigprof*q*c(-1,1), col = 'black', lty =4, lwd =3)
    if(sigboot>0){
        legend('topright', legend = c(expression(hat(sigma)[boot]  %*% T^{(m-1)}  ),expression(hat(sigma)[prof]  %*% T^{(m-1)}  ) ),
               cex = 1.5, lwd = 4,
               title = til,
               col = c('darkred', 'black'))
    }else{
        legend('topright', legend = expression(hat(sigma)[prof]  %*% T^{(m-1)}  ) ,
               cex = 1.5, lwd = 2,
               title = til,
               col = 'black')
        }
}



t = trips[, .N , by=trip][N>10][, trip]

### Random sampling
set.seed(12345)
M = 500
sample_rand  = t[sample.int(length(t), 1.5*M)]
A = trips[trip %in% sample_rand][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
samp = setdiff(sample_rand, readRDS('training_random.RDS'))
samp = samp[sample.int(length(samp),M)]
pdf('progressive_random.pdf')
est = read.est('estimates_random.csv')
## mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
mu = est$mu; sigprof = est$sigprof; sigboot = 0; m = est$m
plot_intervals(mu ,sigprof ,sigboot, lines = FALSE, n = 200, m = m)
plot_coverage(samp,mu ,sigprof ,sigboot, n = 200, m = m )
plot_trips(samp)
plot_intervals(mu ,sigprof ,sigboot, lines = TRUE, m = m, n = 200)
legend(x=50, y = 80, legend = c('95% prediction intervals ',
                                '% of empirical coverage',
                                'single trips'), lty =c(1,4,3),
       col = c('black','black', 'gray') ,
       lwd= c(2,2,2),
       cex =1.5,
       title = 'At random'
       )
dev.off()

pdf('trips_hist_random.pdf')
trips_hist(samp, mu, sigprof, sigboot, m = 1000, til = 'At random', nbins=25)
dev.off()

### AM rush hour
set.seed(1234)
M = 500
sample_timebin = trips[trip %in% t, .(timeBins= if(all(timeBins == timeBins[1])) timeBins[1] else 'remove' ), by=trip]
sample_timebin = sample_timebin[!timeBins == 'remove']
sample_timebin = sample_timebin[, trip[sample.int(.N, 2*M)] , timeBins]
sample_AM = sample_timebin[timeBins == 'MR']$V1
A = trips[trip %in% sample_AM][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
samp = setdiff(unique(A$trip), readRDS('training_timebin.RDS'))
samp = samp[sample.int(length(samp),M)]
est = read.est('estimates_MR.csv')
mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
mu = est$mu; sigprof = est$sigprof; sigboot = 0; m = est$m

pdf('progressive_AM.pdf')
plot_intervals(mu, sigprof, sigboot, m= m, n = 200)
plot_coverage(samp,mu, sigprof, sigboot, m =m, n = 200)
plot_trips(samp)
plot_intervals(mu, sigprof, sigboot, m= m, n = 200, lines=TRUE)
legend(x=50, y = 80, legend = c('95% prediction intervals',
                                '% of empirical coverage',
                                'single trips'),
       lty =c(1,4,3),
       col = c('black','black', 'gray') ,
       lwd= c(2,2,2),
       cex =1.5,
       title = 'AM-rush stratum'
       )
dev.off()

pdf('trips_hist_AM.pdf')
trips_hist(samp, mu, sigprof, sigboot,m, til = 'AM-rush stratum', nbins=33)
dev.off()

## Otherwise
set.seed(2)
M = 500
sample_other = sample_timebin[timeBins == 'Other']$V1
A = trips[trip %in% sample_other][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
samp = setdiff(unique(A$trip), readRDS('training_timebin.RDS'))
samp = samp[sample.int(length(samp),M)]
est = read.est('estimates_Other.csv')
## mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
mu = est$mu; sigprof = est$sigprof; sigboot =0; m = est$m

pdf('progressive_other.pdf')
plot_intervals(mu, sigprof, sigboot, m= m, n = 200)
plot_coverage(samp,mu, sigprof, sigboot, m = m, n = 200)
plot_trips(samp)
plot_intervals(mu, sigprof, sigboot, m= m, n = 200, lines= TRUE)
legend(x=50, y = 80, legend = c('95% prediction intervals',
                                '% of empirical coverage',
                                'single trips'),
       lty =c(1,4,3),
       col = c('black','black', 'gray') ,
       lwd= c(2,2,2),
       cex =1.5,
       title = 'Non-rush stratum'
       )
dev.off()

pdf('trips_hist_other.pdf')
trips_hist(samp, mu, sigprof, sigboot,m, til = 'Non-rush stratum', nbins=40)
dev.off()




## PM rush
set.seed(13)
M = 500
sample_PM = sample_timebin[timeBins == 'ER']$V1
A = trips[trip %in% sample_PM][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
samp = setdiff(unique(A$trip), readRDS('training_timebin.RDS'))
samp = samp[sample.int(length(samp),M)]
est = read.est('estimates_ER.csv')
## mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
mu = est$mu; sigprof = est$sigprof; sigboot = 0; m = est$m


pdf('progressive_PM.pdf')
plot_intervals(mu, sigprof, sigboot, m= m, n = 200)
plot_coverage(samp,mu, sigprof, sigboot, m = m, n = 200)
plot_trips(samp)
plot_intervals(mu, sigprof, sigboot, m= m, n = 200, lines= TRUE)
legend(x=50, y = 80, legend = c('95% prediction intervals',
                                '% of empirical coverage',
                                'single trips'),
       lty =c(1,4,3),
       col = c('black','black', 'gray') ,
       lwd= c(2,2,2),
       cex =1.5,
       title = 'PM-rush stratum'
       )
dev.off()


pdf('trips_hist_PM.pdf')
trips_hist(samp, mu, sigprof, sigboot,m, til = 'PM-rush stratum',nbins=35)
dev.off()

