rm(list= ls())
library(data.table)
source('00_clean_trips.R')
library(traveltimeHMM)


## set.seed(12345)
## test.trips = create_test_trips(M = 2000, trips)
## saveRDS(test.trips, '../testtrips.rds')

trips[, speed:=exp(logspeed)]
test.trips = readRDS('../testtrips.rds')
sp = data_split(trips, test.trips)
train = sp$train
test = sp$test
graph.stat.full = get.params.segment(train, L = 10)
## merging with the train dataset
train = merge(train, graph.stat.full, all.x = TRUE, by = c('linkId.from', 'linkId.to', 'timeBins'))[order(trip,time)]

a = get_rho(lag=1)
mean(a, na.rm=TRUE)

pdf(paste0('autocorr-lag-minN-10.pdf'))
par(mar = c(4,4,0,0.5)+0.1)
hist(a, xlab = 'Order-1 sample autocorrelation of speed',
     freq=FALSE, breaks=50, main = '', col = 'darkgrey',
     cex.lab=1.5, cex = 2,
     ylab = 'Density')
abline(v = mean(a, na.rm=TRUE), lty = 1, lwd =2)
bb = quantile(a, c(0.025,0.975), na.rm = TRUE)
cat('quantiles are:', quantile(a, c(0.025,0.975), na.rm = TRUE), '\n')
abline(v = bb, lty=2, lwd = 2, col = 'black')
axis(1, at = c(bb, mean(a, na.rm = TRUE)), labels =  round(c(bb, mean(a, na.rm  = TRUE)),1))
dev.off()

a = get_rho(lag=2)
mean(a, na.rm=TRUE)

pdf(paste0('autocorr-lag2-minN-10.pdf'))
par(mar = c(4,4,0,0.5)+0.1)
hist(a, xlab = 'Order-2 sample autocorrelation of speed',
     freq=FALSE, breaks=50, main = '', col = 'darkgrey',
     cex.lab=1.5, cex = 2,
     ylab = 'Density')
abline(v = mean(a, na.rm=TRUE), lty = 1, lwd =2)
bb = quantile(a, c(0.025,0.975), na.rm = TRUE)
cat('quantiles are:', quantile(a, c(0.025,0.975), na.rm = TRUE), '\n')
abline(v = bb, lty=2, lwd = 2, col = 'black')
axis(1, at = c(bb, mean(a, na.rm = TRUE)), labels =  round(c(bb, mean(a, na.rm  = TRUE)),1))
dev.off()

samplerho = round(mean(get_rho(lag=1), na.rm = TRUE),2)
samplerho

hist_trips<-function(db,v=1, til = ''){
    db[, hist((obstt - t.seq)/v.seq, freq=FALSE,
             breaks=25, main = '', col = 'darkgrey',
             cex = 2,
             cex.lab=1.7, cex.axis=1.5,
             ylab = 'Density',
             xlab = 'Normalized travel time (seconds)',
             yaxs = 'i',
             xaxs = 'i',
             xlim = c(-5,10),
             ylim = c(0,0.4))]
    x = seq(-5, 5, 0.1)
    lines(x, dnorm(x, 0,1), col  = 'black', lwd=2,lty=2)
    lines(x, dnorm(x, 0, v), col  = 'black', lwd=2, lty=1)
    std.q = round(qnorm(0.975)*c(-1,1),2)
    v.q = round(qnorm(0.975, 0, v)*c(-1,1),2)
    segments(std.q, 0, std.q, 0.2, lty=2, lwd =2, col = 'black')
    ## abline(v = std.q, lty=1, lwd = 2, col = 'black')
    segments(v.q, 0, v.q, 0.2, lty=1, lwd =2, col = 'black')
    ## abline(v = v.q, lty=2, lwd = 2, col = 'black')
    emp.q = db[, quantile((obstt - t.seq)/v.seq, c(0.025, 0.975))]
    segments(emp.q,0, emp.q,0.2, lty =2, lwd =2, col =  ' darkgrey')
    legend( 'topright',
           legend = c('N(0,1) denisty & 95% PI',
                      'N(0,v) density & 95% PI',
                      '95% empirical coverage'),
           cex = 1.4, lwd = 2,
           lty = c(2,1,2),
           title = til,
           col = c('black', 'black', 'darkgrey')
           )
}

time_bin_estimates <-function(timebin = 'MR',
                              nsamples = 2500,
                              ntestsamples = 1000,
                              til = 'AM'){
    if(timebin == 'random'){
        train.samp = setdiff(sample_random(M = 1.5*nsamples, n =10), test.trips)
        train.samp = train.samp[sample.int(length(train.samp), nsamples)]
        til = 'At Random'
        testset = test.trips[sample.int(length(test.trips),
                                        min(ntestsamples, length(test.trips)))]
    }else{
        train.samp = sample_time_bins(nsamples*1.5, timebin)
        train.samp = setdiff(train.samp, test.trips)
        train.samp = train.samp[sample.int(length(train.samp), nsamples)]
        til = paste0(til, '-rush stratum')
        testset = test[, timeBins[1], trip][V1 ==timebin, trip]
        testset = test.trips[sample.int(length(test.trips),
                                        min(ntestsamples, length(testset)))]
    }
    q = qnorm(0.975)
    r = get_rho(train.samp)
    rho = round(mean(r, na.rm=TRUE), 4);rho
    rho2 = round(mean(get_rho(train.samp,2), na.rm=TRUE), 4);rho2
    res  = residual_variance(train, rho, train.samp)
    res2  = residual_variance(train, rho, train.samp, rho2 = rho2)
    v = res$res.sd;v
    v2 = res2$res.sd;v2
    A = test[trip %in% testset,
             param_zeta(time[1],
                        rho,
                        linkId.from,
                        linkId.to,
                        length, sequence = TRUE), by = trip][, .(v.seq, t.seq, N=1:.N), trip]
    B = merge(test[, .(obstt = sum(tt)), trip], A[, .SD[.N], trip])
    
    A = merge(A, test[trip %in% testset, .(obstt = cumsum(tt), N=1:.N), trip],
              by.x = c('trip', 'N'), by.y = c('trip', 'N'))
    A[, cov.v := abs(obstt - t.seq) <= q*v.seq*v]
    A[, cov.nov :=abs(obstt - t.seq) <= q*v.seq]
    coverage = A[ ,.(emp.cov.v = mean(cov.v),
                     emp.cov.nov = mean(cov.nov),
                     ntrips=length(cov.nov),
                     len.v =2 *q * mean(v.seq) *v,
                     len = 2 * q * mean(v.seq)
                     ), by = N]

    pdf(paste0('hist_personalized_',til,'.pdf'))
    hist_trips(B,v = res$res.sd ,til = til)
    dev.off()

    A2 = test[trip %in% testset,
              param_zeta(time[1],
                         rho,
                         linkId.from,
                         linkId.to,
                         length,
                         sequence = TRUE,
                         rho2 = rho2), by = trip][, .(v.seq, t.seq, N=1:.N), trip]
    B2 = merge(test[, .(obstt = sum(tt)), trip], A2[, .SD[.N], trip])
    A2 = merge(A2, test[trip %in% testset, .(obstt = cumsum(tt), N=1:.N), trip],
              by.x = c('trip', 'N'), by.y = c('trip', 'N'))
    A2[, cov.v := abs(obstt - t.seq) <= q*v.seq*v2]
    A2[, cov.nov :=abs(obstt - t.seq) <= q*v.seq]
    coverage2 = A2[ ,.(emp.cov.v = mean(cov.v),
                     emp.cov.nov = mean(cov.nov),
                     ntrips=length(cov.nov),
                     len.v =2 *q * mean(v.seq) *v2,
                     len = 2 * q * mean(v.seq)), by = N]
    pdf(paste0('hist_personalized_2',til,'.pdf'))
    hist_trips(B2,v = res2$res.sd, til = til)
    dev.off()

    ## Calculating theoretical quantile 
    quant= qnorm(seq(0, 1, by = 0.01))
    B[, epsilon := (obstt - t.seq)/v.seq]
    empirical.quant = sapply(quant, function(a) mean(B$epsilon <= a))
    empirical.quant.v = sapply(quant*res$res.sd, function(a) mean(B$epsilon <= a))
    B2[, epsilon := (obstt - t.seq)/v.seq]
    empirical.quant2 = sapply(quant, function(a) mean(B2$epsilon <= a))
    empirical.quant.v2 = sapply(quant*res2$res.sd, function(a) mean(B2$epsilon <= a))
    B$v.seq2 = B2$v.seq
    results = list(
        sample_trips = testset,
        metrics.v = t(B[ ,numerical_res(obstt, t.seq,  v.seq*v)]),
        metrics.nov = t(B[ ,numerical_res(obstt, t.seq, v.seq)]),
        metrics.v2 = t(B2[ ,numerical_res(obstt, t.seq, v.seq * v2)]),
        metrics.nov2 = t(B2[ ,numerical_res(obstt, t.seq, v.seq )]),
        q = q,
        data = B,
        coverage = coverage,
        coverage2 = coverage2,
        nobs  = length(train.samp),
        nobstest = length(testset),
        theo.quant = seq(0,1,0.01),
        empirical.quant = empirical.quant,
        empirical.quant.v = empirical.quant.v,
        empirical.quant2 = empirical.quant2,
        empirical.quant.v2 = empirical.quant.v2,
        rho = rho,
        v  = v,
        rho2 = rho2,
        v2 = v2,
        title = til,
        timebin = timebin
        )
    results
}

## Sample AM
## AM bin
set.seed(12345)
res = time_bin_estimates('MR', til = 'AM', 2500, 1000)
saveRDS(res, 'estimates_PS_MR.RDS')
res

## PM Sample
set.seed(12345)
res = time_bin_estimates('ER', til = 'PM', 2500, 1000)
saveRDS(res, 'estimates_PS_ER.RDS')
res

## Other sample
set.seed(12345)
res= time_bin_estimates('Other', til = 'Non', 2500, 1000)
saveRDS(res, 'estimates_PS_Non.RDS')
res

## ## random 
## set.seed(123456)
## res= time_bin_estimates(timebin  = 'random', til= 'random', 4000,2000)
## saveRDS(res, 'estimates_PS_Random.RDS')
## res




## long.trips = trips[trip %in% test.trips, .N, trip][order(N, decreasing = TRUE)][1:10,trip]
## est = read.est('estimates_MR.csv')
## res = readRDS('estimates_PS_MR.RDS')
## mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
## rho = res$rho
## q = qnorm(0.975)
## v = res$v
## v2 = res$v2
## pdf('pred_seq_AM.pdf')

## i = 4
## plot_intervals(mu ,sigprof ,0, lines = FALSE, n = 200, m = m)
## plot_trips(long.trips[i])
## A = test[trip== long.trips[i],  param_zeta(time[1], rho, linkId.from, linkId.to, length, sequence = TRUE), trip]
## A$v.seq2 = test[trip== long.trips[i],  param_zeta(time[1], rho,
##                                                 linkId.from,
##                                                 linkId.to,
##                                                 length,
##                                                 sequence = TRUE,
##                                                 rho2 = res$rho2), trip]$v.seq
## B = A[, .(lwr =t.seq - q * v.seq * v,
##           upr = t.seq + q * v.seq * v,
##           lwr2 =t.seq - q * v.seq2 * v2,
##           upr2 = t.seq + q * v.seq2 * v2)][, .(lwr = lwr/1:.N,
##                                                upr = upr/1:.N,
##                                                lwr2 = lwr2/1:.N,
##                                                upr2 = upr2/1:.N)]


## B[, lines(1:.N, lwr, col = 'black', lwd = 2, lty = 4, type = 'l')]
## B[, lines(1:.N, upr, col = 'black', lwd = 2, lty = 4, type = 'l')]
## B[, lines(1:.N, lwr2, col = 'red', lwd = 2, lty = 4, type = 'l')]
## B[, lines(1:.N, upr2, col = 'red', lwd = 2, lty = 4, type = 'l')]

## dev.off()

## legend('topright',
##        legend = c('95% prediction intervals',
##                   'single trip',
##                   '95% trip-specific PS'),
##        lty =c(1,3,4),
##        col = c('black','black', 'black') ,
##        lwd= c(2,2,2),
##        cex =1.5,
##        title = 'AM-rush stratum'
##        )


