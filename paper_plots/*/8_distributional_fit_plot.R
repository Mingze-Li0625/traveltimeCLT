## rm(list= ls())
## library(data.table)
## library(scales)
## source('00_clean_trips.R')
## library(traveltimeHMM)


set.seed(12345)
test.trips = create_test_trips(M = 2000, trips)
saveRDS(test.trips, '../testtrips.rds')
trips[, speed:=exp(logspeed)]
test.trips = readRDS('../testtrips.rds')


sp = data_split(trips, test.trips)
test = sp$test
train = sp$train 
graph.stat.full = get.params.segment(train, L = 10)
## merging with the train dataset
train = merge(train, graph.stat.full, all.x = TRUE, by = c('linkId.from', 'linkId.to', 'timeBins'))[order(trip,time)]
####
##

est = read.est('estimates_random.csv')
res = readRDS('estimates_PS_Random.RDS')
mu = est$mu; sigprof = est$sigprof; sigboot = 0; m = est$m

coverage_pooled<-function(sample_trips,sig, m){
    x = seq(0,1, by=0.01)
    sig = sig * sqrt(1 + 1/m)
    A = trips[trip %in% sample_trips, .(tt = sum(tt), N = .N), trip]
    A[, epsilon:= (tt - N*mu)/(sqrt(N)*sig) , trip]
    list(grid = x, 
         empirical = sapply( qnorm(seq(0,1, by=0.01)), function(s) mean(A$epsilon<= s)),
         data = A
         )
}

pdf('theo_vs_empirical_random.pdf')

par(mar = c(5,5,0,0)+0.1)
plot(res$theo.quant, res$empirical.quant.v ,
     lty = 5,
     lwd = 2,
     xlab = 'Theoretical coverage of predictive intervals',
     ylab = 'Empiricla coverage of predictive intervals',
     col ='black',
     pch = 1,
     cex.lab=1.5,
     cex.axis=1.5,
     type = 'l')
abline(a = 0, b =1, col = 'black', lwd = 2, lty =1)
lines(res$theo.quant, res$empirical.quant ,
      lty = 3,
      lwd = 2,
      pch = 3,
      col = 'darkred',
      type = 'l')
pooled_cov = coverage_pooled(res$sample_trips, sigprof, m)
lines(pooled_cov[[1]], pooled_cov[[2]],
      col = 'cyan3',
      lty =4,
      lwd = 2,
      pch = 5,
      type = 'l')
legend('topleft', legend = c('Ideal',
                             'Prediction sequences',
                             'Prediction sequences (v=1)',
                             'Pooled prediction intervals'
                             ),
       lty = c(1,5, 3, 4),
       lwd = c(2,2,2,2),
       col = c('black', 'black', 'darkred', 'cyan3'),
       cex =1.3,
       title = 'At random',
       box.lwd = 0,
)

dev.off()


## plotting interval length
par(mar = c(5,5,0,0)+0.1)
n = 150
plot(1:n, 2* q * sigprof * sqrt(1 + 1/m)*sqrt(1:n),
      col = 'cyan3',
     lwd = 2,
     cex.lab=1.5,
     cex.axis=1.5,
     lty =4,
     type = 'l',
     xlab = 'Number of edges (n)',
     ylab = 'Average interval length at 95% coverage level'
     )
res$coverage[1:n , lines(N, len.v,
                        lwd= 2,
                        lty =1,
                        col = 'black',
                        type = 'l')]
res$coverage[1:n , lines(N, len, col = 'darkred', lty = 3, lwd = 2)]
legend('topleft', legend = c(
                             'Prediction sequences',
                             'Prediction sequences (v=1)',
                             'Pooled prediction intervals'
                             ),
       lty = c(1, 3, 4),
       lwd = c(2,2,2),
       col = c('black', 'darkred', 'cyan3'),
       cex =1.3,
       title = 'At random',
       box.lwd = 0,
)

## res$coverage2[1:n , lines(N, len.v *res$v2/res$v,
##                         lwd= 2,
##                         lty =1,
##                         col = 'darkgreen',
##                         type = 'l')]

dev.off()


