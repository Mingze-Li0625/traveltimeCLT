rm(list= ls())
library(data.table)
source('00_clean_trips.R')
library(traveltimeHMM)
source('predict_length.R')
source('predict_coverage.R')

trips[, speed:=exp(logspeed)]
test.trips = readRDS('../testtrips.rds')

n = 160
sp = data_split(trips, test.trips)
test = sp$test
train = sp$train 
graph.stat.full = get.params.segment(train, L = 10)
## merging with the train dataset
train = merge(train, graph.stat.full, all.x = TRUE, by = c('linkId.from', 'linkId.to', 'timeBins'))[order(trip,time)]
####
q = qnorm(0.975)
res = readRDS('estimates_PS_Random.RDS')
rho = res$rho; rho
v = res$v; v
est = read.est('estimates_random.csv')
mu = est$mu; sigprof = est$sigprof; sigboot = 0; m = est$m

## linear model
rules = list(
    list(start='6:30',  end= '9:00',  days = 1:5, tag='MR'),
    list(start='15:00', end= '18:00', days = 2:5, tag='ER')
)
time_bins <- rules2timebins(rules)
pk.train = trips
pk.train$tripID = pk.train$trip; pk.train$trip = NULL
pk.train$linkID = pk.train$linkId; pk.train$linkId = NULL
pk.test = pk.train[tripID %in% test.trips][order(tripID, time)]
pk.train = pk.train[!tripID %in% test.trips][order(tripID, time)]
lm.train = pk.train[, .(t = sum(tt), d = sum(length), timeBins = timeBins[1]), tripID]
lm.test = pk.test[, .(t = sum(tt), d = sum(length), timeBins = timeBins[1]), tripID]

fit<-lm(log(t) ~ log(d) + timeBins, data = lm.train)
mu = trips[, mean(length)]

pred = predict(fit, newdata = data.frame(d = c(1:250)*mu, timeBins ='Other'),
        type = 'response', interval = 'prediction')
lm.length = exp(pred[, 'upr']) - exp(pred[, 'lwr'])

####
cat('traveltimeHMM no-dependence \n')
fit <- readRDS('no-dep-model_test.RDS')
A = pk.test[,.( predict.length(fit,
                               .SD,
                               starttime = as.POSIXlt(time[1]),
                               time_bins.fun= time_bins), N = 1:.N)
          , tripID]
no.dep.length = A[, .(len = mean(V1)), N]
fit <- readRDS(file = 'trip-hmm_test.RDS')
A = pk.test[,.( predict.length(fit,
                               .SD,
                               starttime = as.POSIXlt(time[1]),
                               time_bins.fun= time_bins), N = 1:.N)
          , tripID]
hmm_effect.length = A[, .(len = mean(V1)), N]

## plotting interval length
pdf('alternative_length_n.pdf')
par(mar = c(5,5,0,0)+0.1)
n = 160
q = qnorm(0.975)
plot(1:n, 2* q * sigprof * sqrt(1 + 1/m)*sqrt(1:n),
      col = 'cyan3',
     lwd = 2,
     cex.lab=1.6,
     cex.axis=1.2,
     ylim = c(0,2300),
     lty =4,
     type = 'l',
     xlab = 'Number of edges (n)',
     ylab = 'Average interval length (s) at 95% coverage level'
     )
res$coverage[1:n , lines(N, len.v,
                        lwd= 2,
                        lty =1,
                        col = 'black',
                        type = 'l')]
res$coverage[1:n , lines(N, len,
                        lwd= 2,
                        lty =6,
                        col = 'darkred',
                        type = 'l')]
##linear model
lines(1:n, lm.length[1:n],
      lwd= 2,
      lty =2,
      col = 'darkorange',
      type = 'l')
no.dep.length[, lines(N, len,
                      lwd= 2,
                      lty =3,
                      col = 'darkolivegreen4',
                      type = 'l')]
hmm_effect.length[, lines(N, len,
                      lwd= 2,
                      lty =5,
                      col = 'darkgray',
                      type = 'l')]
legend('topleft',
       legend = c('HMM+Trip-effect',
                  'No-dependence',
                  'Linear model',
                  'Population intervals',
                  'Trip-specific sequences',
                  'Trip-specific sequences (v=1)'
                  ),
       lty =c(5,3,2,4,1, 6),
       col = c( 'darkgrey',
               'darkolivegreen4',
               'darkorange',
               'cyan3',
               'black',
               'darkred'
               ),
       lwd= 2,
       cex =1.3,
       ##title = 'At random',
       box.lwd = 0,
       bty = 'n'
       )
dev.off()



#################################################3
### Coverage

samp = res$sample_trips
prof = intervals(sigprof, m, n = n)
mu = est$mu; sigprof = est$sigprof; sigboot = 0; m = est$m
A = trips[trip %in% samp][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
pooled_cov = A[, mean(abs(tt-mu) <= prof[N]),N][N<=n]
pooled_cov


####
fit <- readRDS('no-dep-model_test.RDS')
A = pk.test[,.( predict.cov(fit,
                            .SD,
                            starttime = as.POSIXlt(time[1]),
                            time_bins.fun= time_bins,
                            obs = cumsum(tt)), N = 1:.N)
          , tripID]
no.dep.cov = A[, .(cov= mean(V1)), N]

fit <- readRDS(file = 'trip-hmm_test.RDS')
A = pk.test[,.( predict.cov(fit,
                            .SD,
                            starttime = as.POSIXlt(time[1]),
                            time_bins.fun= time_bins,
                            obs = cumsum(tt)), N = 1:.N)
          , tripID]
hmm_effect.cov = A[, .(cov = mean(V1)), N]

## linear model
fit<-lm(log(t) ~ log(d) + timeBins, data = lm.train)
lm.test = pk.test[, .(t = cumsum(tt), d = cumsum(length), timeBins = timeBins[1], N = 1:.N), tripID]
pred = predict(fit, newdata = lm.test, type = 'response', interval = 'prediction')
lm.test[ , cov:= t <= exp(pred[ ,'upr'])  & t >= exp(pred[ ,'lwr'])]
lm.cov = lm.test[, .(cov = mean(cov)), N]

pdf('alternative_coverage_n.pdf')
par(mar = c(5,5,0,5)+0.1)
## with
plot(res$coverage$N,res$coverage$emp.cov.v,
     col = 'black',
     lwd = 2,
     xlim = c(0,n),
     ylim = c(0,1),
     cex.lab=1.6,
     cex.axis=1.2,
     lty =1,
     type = 'l',
     xlab = 'Number of edges (n)',
     ylab = 'Empirical coverage on test data at 95% level'
     )
pooled_cov[ ,lines(N, V1,
                   lwd =2,
                   lty = 4,
                   col =  'cyan3')]
## nov model
lines(res$coverage$N,res$coverage$emp.cov.nov,
     col = 'darkred',
     lwd = 2,
     lty =6,
     type = 'l'
     )
## linear model
lm.cov[, lines(N, cov,
               lwd= 2,
               lty =2,
               col = 'darkorange',
               type = 'l')]
no.dep.cov[, lines(N, cov,
                   lwd=2,
                   lty =3,
                   col = 'darkolivegreen4',
                   type = 'l')]
hmm_effect.cov[, lines(N, cov,
                       lwd= 2,
                       lty =5,
                       col = 'darkgray',
                       type = 'l')]
abline(h = 0.95, lty =2, col = 'black')
mtext("Number of trips",side=4,line=4, cex = 1.6) 
res$coverage[, lines(N, ntrips/2000,
                     lwd = 2,
                     lty = 1,
                     col = 'black',
                     type= 'l',
                     pch = 1
                     )]
res$coverage[N%%10 ==0, lines(N, ntrips/2000,
                     lwd = 2,
                     lty = 1,
                     col = 'black',
                     type= 'p',
                     pch = 1
                     )]
axis(4, at = seq(0,1,0.2) ,
     labels = seq(0,1,0.2) * 2000,
     las=3, cex.axis = 1.2,
     cex.lab= 1.56)

axis(2, at= 0.95, labels = 0.95,
     las=1, cex.axis = 1.2,
     cex.lab= 1.6)
legend('bottomleft',
       legend = c( 'Number of trips',
                  'HMM+Trip-effect',
                  'No-dependence',
                  'Linear model',
                  'Population intervals',
                  'Trip-specific sequences',
                  'Trip-specific sequences (v=1)'
                  ),
       lty =c(1,5,3,2,4,1,6),
       col = c('black',
               'darkgrey',
               'darkolivegreen4',
               'darkorange',
               'cyan3',
               'black',
               'darkred'
               ),
       pch = c(1, NA, NA, NA, NA, NA, NA),
       lwd= 2,
       cex =1.3,
       ##title = 'At random',
       box.lwd = 0,
       bty = 'n'
       )
dev.off()
