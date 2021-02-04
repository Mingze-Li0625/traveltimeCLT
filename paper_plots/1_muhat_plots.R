rm(list = ls())
library(data.table)
source('00_clean_trips.R')

plot_hatmu<-function(sampletr, til = ''){
    A = trips[trip %in% sampletr][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
    xl = max(A$N)
    xl = 200
    yl = 120
    par(mar = c(5,5,0,0)+0.1)
    plot(A[trip==9, .(N, tt)], lwd = 1,lty = 3,
         type = 'l',
         ylim = c(0, yl),
         xlim = c(1,xl),
         xlab = 'Number of edges (n)',
         ylab = 'Average travel time (seconds)',
         cex.lab=2, cex.axis=1.5, col = 'grey', axes=FALSE)
    A[, lines(N, tt, type = 'l',lwd = 1, lty = 3, col = 'grey'), trip]
    abline(h = A[, tt[.N], trip][, mean(V1)], lwd =3, lty=1)
    lines(A[,mean(tt) , N], lty = 2, lwd = 3)
    legend('topright', legend = c('single trips (time average)', 'average over trips per edge',  expression(hat(mu))),
           lty = c(3,2,1), lwd = c(2,2,2),
       cex=1.5, pt.cex=1.5, title = til)
    mu  = A[, tt[.N], trip][,V1]
    En = A[,.N, trip][, mean(1/N)]
    hatmu = mean(mu)
    axis(2, at = c(0,40,80, 120), cex.axis = 1.5, cex.lab = 2)
    axis(2, at = round(hatmu,1), cex.axis = 1.5, cex.lab = 2)
    axis(1, at = c(0,50,100,150, 200), cex.axis = 1.5, cex.lab = 2)
    m = length(unique(A$trip))
    q = qt(0.975, df = m-1)
    hatsigmu = var(mu)
    qn = qnorm(0.975, 0, 1)
    lwq = hatmu - q * sqrt(hatsigmu/m)
    upq = hatmu + q * sqrt(hatsigmu/m)
    lwr<-function(n) q* sqrt((hatsigmu/En)/(n*m))
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

M = 1000
t = trips[, .N , by=trip][N>10][, trip]

### sampling per week hour
pdf('muhat_per_weekhour.pdf')
set.seed(12353)
sample_whour = trips[trip %in% t, .(whour= whour[1]), by=trip][order(whour)][,  trip[sample.int(.N,min(30,.N))], whour][, V1]
sample_whour = sample_whour[sample.int(length(sample_whour), M)]
res= plot_hatmu(sample_whour, 'Stratified by week hours')
res$m = length(unique(sample_whour))
dev.off()
write.csv(lapply(res, function(r) round(r, 3) ),'estimates_weekhour.csv')
saveRDS(sample_whour, 'training_whour.RDS')

### sampleing based on time_bin
pdf('muhat_per_timebin.pdf')
set.seed(12346)
uniquetimebins = 3
sample_timebin = trips[trip %in% t, .(timeBins= timeBins[1]), by=trip][,  trip[sample.int(.N, floor(1.5*M/uniquetimebins))], timeBins]$V1
sample_timebin = sample_timebin[sample.int(length(sample_timebin), M)]
res = plot_hatmu(sample_timebin, 'Stratified by traffic time bins')
res$m = length(unique(sample_timebin))
write.csv(lapply(res, function(r) round(r, 3) ),'estimates_timebin.csv')
dev.off()
saveRDS(sample_timebin, 'training_timebin.RDS')

### sampleing at random
pdf('muhat_random_sampling.pdf')
set.seed(1245678)
sample_rand  = t[sample.int(length(t), 1.5*M)]
sample_rand = sample_rand[sample.int(length(sample_rand), M)]
res = plot_hatmu(sample_rand, 'At random')
res$m = length(unique(sample_rand))
write.csv(lapply(res, function(r) round(r, 3) ),'estimates_random.csv')
dev.off()
saveRDS(sample_rand, 'training_random.RDS')

## Bootstrap sample
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

v = sigma_bootstrap(sample_rand)
add_est(v, 'estimates_random.csv')
hist(sqrt(v$v), breaks=100)

v = sigma_bootstrap(sample_whour)
add_est(v, 'estimates_weekhour.csv')
hist(sqrt(v$v), breaks=100)

v = sigma_bootstrap(sample_timebin)
add_est(v, 'estimates_timebin.csv')
hist(sqrt(v$v), breaks=100)

