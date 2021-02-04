rm(list= ls())
library(data.table)
source('00_clean_trips.R')
library(traveltimeHMM)
source('predict_coverage.R')
source('predict_length.R')

trips[, speed:=exp(logspeed)]
test.trips = readRDS('../testtrips.rds')


sp = data_split(trips, test.trips)
test = sp$test
train = sp$train 
graph.stat.full = get.params.segment(train, L = 10)
## merging with the train dataset
train = merge(train, graph.stat.full, all.x = TRUE, by = c('linkId.from', 'linkId.to', 'timeBins'))[order(trip,time)]
####

empirical_coverage_woodard <-function(x, true_obs, probs = seq(0,1,0.01)){
    epsi  = true_obs - mean(x)
    x = x - mean(x)
    alpha1 = 1 - probs
    list(
        cov.levels = alpha1,
        coverage = sapply((1 - alpha1)/2, function(s)
            epsi >= quantile(x, s) & epsi <= quantile(x, 1-s))
    )
}

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

hist_trips<-function(db, til = ''){
    B[, hist((obstt - tt)/sd, freq=FALSE,
             breaks=50, main = '', col = 'lightgrey',
             cex.lab=1.2, cex = 2,
             ylab = 'density',
             xlab = 'normalized travel time',
             yaxs = 'i',
             xaxs = 'i',
             xlim = c(-5,10),
             ylim = c(0,0.4))]
    x = seq(-5, 5, 0.1)
    lines(x, dnorm(x, 0,1), col  = 'black', lwd=4)
    lines(x, dnorm(x, 0, res$res.sd), col  = 'blue', lwd=4)
    std.q = round(qnorm(0.975)*c(-1,1),2)
    v.q = round(qnorm(0.975, 0, v)*c(-1,1),2)
    abline(v = std.q, lty=2, lwd = 2, col = 'black')
    abline(v = v.q, lty=2, lwd = 2, col = 'blue')
    emp.q = B[, quantile((obstt-tt)/sd, c(0.025, 0.975))]
    abline(v = emp.q, lty =2, lwd =2, col =  ' lightgrey')
    legend( 'topright',
           legend = c('N(0,1)', 'N(0,v)', '95% PI', '95% emp. CI'),
           cex = 1.5, lwd = 4,
           lty = c(1,1,2,2),
           title = til,
           col = c('black', 'blue', 'black', 'lightgrey')
           )
}

report <-'alternative_models.txt'
report <-file(report, 'wt')
sink(report)
sink(report,type='message')

## random 
set.seed(123456)
res = readRDS('estimates_PS_Random.RDS')
rho = res$rho; rho
v = res$v; v
est = read.est('estimates_random.csv')
mu = est$mu; sigprof = est$sigprof; sigboot = 0; m = est$m

## Getting test-data results

### Settings
cat('nmber of test:', length(test.trips), '\n')
cat('Prediction sequence with v \n')
cat('v:',res$res.sd, 'rho:', rho)
cat('Prediction sequence with no v \n')

res

## pdf('hist_personalized_random.pdf')
## hist_trips(B, til = 'At random')
## dev.off()

## plotting
devtools::install_github("melmasri/traveltimeHMM", force = TRUE)
library(traveltimeHMM)
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

cat('traveltimeHMM no-dependence \n')
#fit <- traveltimeHMM(data = pk.train,nQ = 2,max.it = 20, model = "no-dependence")
fit <- readRDS('no-dep-model_test.RDS')
A = pk.test[, predict(fit,
                      .SD,
                      starttime = as.POSIXlt(time[1]),
                      time_bins.fun= time_bins)
          , tripID]
BB = merge(pk.test[, .(obstt= sum(tt)), tripID],A[, .(est = mean(V1),
                                                      lq = quantile(V1- mean(V1),0.025),
                                                      uq= quantile(V1- mean(V1),0.975) ),tripID])
t(BB[, numerical_res(obstt, est, sd = 1,uq, lq)])
A = merge(A, pk.test[, .(true_obs = sum(tt)), tripID])
a= A[, empirical_coverage_woodard(V1, true_obs[1]), tripID]
nodep.cov = a[, .(coverage = mean(coverage)), by = cov.levels]
saveRDS(fit, file = 'no-dep-model_test.RDS')

## cat('traveltimeHMM HMM\n')
## fit <- traveltimeHMM(data = pk.train,nQ = 2,max.it = 100, model = "HMM")
## A = pk.test[, predict(fit,.SD, starttime = as.POSIXlt(time[1]), time_bins.fun= time_bins), tripID]
## BB = merge(pk.test[, .(obstt= sum(tt)), tripID],A[, .(est = mean(V1),
##                                                       lq = quantile(V1- mean(V1),0.025),
##                                                       uq= quantile(V1- mean(V1),0.975) ),tripID])
## round(t(BB[, numerical_res(obstt, est, lq, uq)]), 2)



cat('traveltimeHMM TRIP-HMM\n')
#fit <- traveltimeHMM(data = pk.train,nQ = 2,max.it = 100, model = "trip-HMM")
fit <- readRDS(file = 'trip-hmm_test.RDS')
A = pk.test[, predict(fit,.SD, starttime = as.POSIXlt(time[1]), time_bins.fun= time_bins), tripID]
BB = merge(pk.test[, .(obstt= sum(tt)), tripID],A[, .(est = mean(V1),
                                                      lq = quantile(V1- mean(V1),0.025),
                                                      uq= quantile(V1- mean(V1),0.975) ),tripID])
t(BB[, numerical_res(obstt, est, sd = 1,uq, lq)])
A = merge(A, pk.test[, .(true_obs = sum(tt)), tripID])
a= A[, empirical_coverage_woodard(V1, true_obs[1]), tripID]
hmm_effect.cov = a[, .(coverage = mean(coverage)), by = cov.levels]
saveRDS(fit, file = 'trip-hmm_test.RDS')


## linear model
cat('Linear Model\n')

lm.train = pk.train[, .(t = sum(tt), d = sum(length), timeBins = timeBins[1]), tripID]
lm.test = pk.test[, .(t = sum(tt), d = sum(length), timeBins = timeBins[1]), tripID]

fit<-lm(log(t) ~ log(d) + timeBins, data = lm.train)
pred = predict(fit, newdata = lm.test, type = 'response', interval = 'prediction')
est = exp(pred[,'fit'])
lq = exp(pred[, 'lwr'])
uq = exp(pred[, 'upr'])
obs = lm.test$t
t(numerical_res(obs, est, sd = 1, uq -est, lq- est))
coverage_lm<-function(model, newdata, obs){
    obs = log(newdata[, sum(t), tripID][, V1])
    alpha1 = 1 - seq(0,1, by = 0.01)
    a = sapply(alpha1, function(r){
        pred = predict(model, newdata = lm.test, type = 'response', interval = 'prediction', level = r)
        mean(obs >= pred[, 'lwr'] & obs <= pred[,'upr'])
    })
    list(coverage = a, cov.levels = alpha1)
}
lm.cov = coverage_lm(fit, lm.test)



sink()
sink(type='message')
close(report)
q('n')


### Coverage plot
pdf('coverage_alternative_models.pdf')
par(mar = c(5,5,0,0)+0.1)
x = 1+ 0:20*5
plot(res$metrics.v['coverage.levels',]$coverage.levels[x],
     res$metrics.v['coverage',]$coverage[x],
     lty = 1,
     lwd = 1,
     xlab = 'Theoretical coverage of predictive intervals',
     ylab = 'Empirical coverage on test data',
     col ='black',
     pch = 0,
     cex.lab=1.6,
     cex.axis=1.2,
     type = 'o')
abline(a = 0, b =1, col = 'black', lwd = 2, lty =1)
lines(res$metrics.nov['coverage.levels',]$coverage.levels[x],
     res$metrics.nov['coverage',]$coverage[x],
      lty = 1,
      lwd = 1,
      pch = 1,
      col = 'darkred',
     type = 'o')
pooled_cov = coverage_pooled(res$sample_trips, sigprof, m)
lines(pooled_cov[[1]][x], pooled_cov[[2]][x],
      col = 'cyan3',
      lty =1,
      lwd = 1,
      pch = 2,
      type = 'o')
lines(nodep.cov$cov.levels[x], nodep.cov$coverage[x],
      col = 'darkgreen',
      lty =1,
      lwd = 1,
      pch = 5,
      type = 'o')
lines(hmm_effect.cov$cov.levels[x], hmm_effect.cov$coverage[x],
      col = 'darkgray',
      lty =1,
      lwd = 1,
      pch = 8,
      type = 'o')
lines(lm.cov$cov.levels[x], lm.cov$coverage[x],
      col = 'darkorange',
      lty =1,
      lwd = 1,
      pch = 9,
      type = 'o')
legend('topleft', legend = c('Ideal',
                             'HMM+Trip-effect',
                             'No-dependence',
                             'Linear model',
                             'Population intervals',
                             'Trip-specific sequences',
                             'Trip-specific sequences (v=1)'
                             ),
       lty = 1,
       lwd = 1,
       pch = c(NA, 8,5,9,2,0,1),
       col = c('black',
               'darkgray',
               'darkgreen',
               'darkorange',
               'cyan3',
               'black',
               'darkred'
               ),
       cex =1.3,
       #title = 'At random',
       box.lwd = 0,
       bty = 'n'
       )
dev.off()


