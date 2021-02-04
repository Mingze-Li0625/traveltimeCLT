rm(list = ls())
library(data.table)
source('00_clean_trips.R')

M = 1000
t = trips[, .N , by=trip][N>10][, trip]

set.seed(1234)
sample_timebin = trips[trip %in% t, .(timeBins= if(all(timeBins == timeBins[1])) timeBins[1] else 'remove' ), by=trip]
sample_timebin = sample_timebin[!timeBins == 'remove']
sample_timebin = sample_timebin[, trip[sample.int(.N, M)] , timeBins]$V1

parameter_est<-function(sampletr){
    A = trips[trip %in% sampletr][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
    xl = max(A$N)
    xl = 200
    yl = max(A$tt)
    mu  = A[, tt[.N], trip][,V1]
    En = A[,N[.N], trip][, mean(1/V1)]
    hatmu = mean(mu)
    m = length(unique(A$trip))
    q = qt(0.975, df = m-1)
    hatsigmu = var(mu)
    qn = qnorm(0.975, 0, 1)
    lwq = hatmu - q * sqrt(hatsigmu/m)
    upq = hatmu + q * sqrt(hatsigmu/m)
    ## abline(h = lwq, col = 'red', lty = 4, lwd=2)
    ## abline(h = upq, col = 'red', lty = 4, lwd=2)
    cat('mu hat ', hatmu, '(lwq, upq)', c(lwq, upq), 'sigma ', hatsigmu , 'En ', En, 'sigprof',sqrt(hatsigmu/En) ,  '\n')
     list(mu = hatmu,
         lwr = lwq,
         upr = upq,
         sigma = hatsigmu,
         En = En,
         sigprof = sqrt(hatsigmu/En)
         )
}

save_res<-function(samp, name){
    res = parameter_est(samp)
    res$m = length(unique(samp))
    write.csv(lapply(res, function(r) round(r, 3) ), paste0(name, '.csv'))
}

save_training<-function(samp, name){
    saveRDS(samp, paste0(name, '.RDS'))
}


trips[trip %in% sample_timebin, .(tbin = timeBins[1]), trip][, {cat(tbin, ' ');
    save_training(trip, paste0('training_', tbin[1]));
    save_res(trip, paste0('estimates_', tbin[1]))},tbin]

sigma_bootstrap<-function(sampletrip){
    TT = trips[trip %in% sampletrip]
    v = replicate(5000, TT[, tt[sample.int(.N,1)],trip][, var(V1)])
    q = sqrt(quantile(v, c(0.025, 0.975)))
    cat('bootstrapping', sqrt(mean(v)), '(lwr, upr)', q, '\n')
    list(v = v, sigboot = sqrt(mean(v)), lwr.boot = q[1], upr.boot = q[2] )
}


add_est<-function(est, filename){
    res = as.list(read.csv(filename))
    res$sigboot = est$sigboot
    res$lwr.boot = est$lwr.boot
    res$upr.boot = est$upr.boot
    res = lapply(res, function(r) round(r,3))
    write.csv(res, filename, row.names = FALSE)
}


set.seed(12453)
trips[trip %in% sample_timebin, .(tbin = timeBins[1]), trip][, {cat(tbin, ' '); add_est(
                                                                                    sigma_bootstrap(trip), paste0('estimates_', tbin[1], '.csv'))},tbin]

