rm(list= ls())
library(data.table)
source('00_clean_trips.R')

intervals<-function(sig,m, n){
    qt(0.975, m-1) * sig * sqrt((1 + 1/m)/(1:n))
}

numerical_res<-function(samp, mu, sig,m){
    A = trips[trip %in% samp]
    B = A[, .(.N, tt= sum(tt), normtt = (sum(tt) - .N*mu)/(sig * sqrt(.N*(1+1/m)))) ,trip]
    q = qt(0.975, m-1)
    D = B[,
      .(
          #MAREgeo = 100*exp(mean(log(abs(tt - N*mu)/tt))),
          RMSE = sqrt(mean((tt - N*mu)^2)),
          MAE  = mean(abs(tt - N*mu)),
          ME  = mean(tt- N*mu),
          MAPE = 100*mean(abs(tt - N*mu)/tt),
          emprical.cov = 100*mean(abs(normtt)<=q),
          PI.length = mean(2*q*sqrt(N*(1+1/m))*sig),
          PI.rel.length = 100*mean(2*q*sqrt(N*(1+1/m))*sig/tt)
          )
      ]
    round(t(D),2)
}

## --------------------------------------------------
## Numerical resutls
## At random
set.seed(12345)
est = read.est('estimates_random.csv')
mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
samp = sample_random(M = 500, filename = 'training_random.RDS')
numerical_res(samp, mu, sigprof, m)
numerical_res(samp, mu, sigboot, m)

## AM bin
set.seed(12345)
samp_AM = sample_time_bins(500, 'MR', filename = 'training_MR.RDS')
est = read.est('estimates_MR.csv');
mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
numerical_res(samp_AM, mu, sigprof, m)
numerical_res(samp_AM, mu, sigboot, m)

## PM bin
set.seed(12345)
samp_PM = sample_time_bins(500, 'ER', filename = 'training_ER.RDS')
est = read.est('estimates_ER.csv');
mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
numerical_res(samp_PM, mu, sigprof, m)
numerical_res(samp_PM, mu, sigboot, m)

## Other bin
set.seed(12345)
samp_other = sample_time_bins(500, 'Other', filename = 'training_Other.RDS')
est = read.est('estimates_Other.csv');
mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
numerical_res(samp_other, mu, sigprof, m)
numerical_res(samp_other, mu, sigboot, m)


## Overall
set.seed(12345)
samp = setdiff(c(samp_AM, samp_PM, samp_other), readRDS('training_timebin.RDS'))
samp = samp[sample.int(samp,500)]
est = read.est('estimates_timebin.csv');
mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
numerical_res(samp, mu, sigprof, m)
numerical_res(samp, mu, sigboot, m)


##--------------------------------------------------
## Results with different legnth n
## --------------------------------------------------
## Numerical resutls
## At random
set.seed(12345)
est = read.est('estimates_random.csv')
mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
samp = sample_random(M = 500, filename = 'training_random.RDS', n = 1, n2= 40)
numerical_res(samp, mu, sigprof, m)
samp = sample_random(M = 500, filename = 'training_random.RDS', n = 40, n2=80)
numerical_res(samp, mu, sigprof, m)
samp = sample_random(M = 500, filename = 'training_random.RDS', n = 80, n2= 120)
numerical_res(samp, mu, sigprof, m)
samp = sample_random(M = 500, filename = 'training_random.RDS', n = 120)
numerical_res(samp, mu, sigprof, m)

## Overall
set.seed(12345)
samp = setdiff(c(samp_AM, samp_PM, samp_other), readRDS('training_timebin.RDS'))
samp = samp[sample.int(samp,500)]
est = read.est('estimates_timebin.csv');
mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
numerical_res(samp, mu, sigprof, m)
numerical_res(samp, mu, sigboot, m)

### tex table
files = c('estimates_random.csv',
          'estimates_timebin.csv',
          'estimates_MR.csv' ,
          'estimates_ER.csv',
          'estimates_Other.csv')

f = files[1]


table_est_format<-function(v){
    c(round(v$mu,1),
      round(v$sigma,1),
      round(v$En, 3),
      round(v$sigprof,1),
      round(v$sigboot, 1))
}

v = read.est(f)
table_est_format(v)
estimate.table = sapply(files, function(f)  table_est_format(read.est(f)))


library(xtable)


xtable(estimate.table)

