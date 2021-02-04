rm(list= ls())
library(data.table)
library(scales)
source('00_clean_trips.R')
library(traveltimeHMM)


## set.seed(12345)
## test.trips = create_test_trips(M = 2000, trips)
## saveRDS(test.trips, '../testtrips.rds')
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

long.trips = trips[, .N, trip][order(N, decreasing = TRUE)][1:20,]


96min over 126km, and 345 edges

est = read.est('estimates_random.csv')
res = readRDS('estimates_PS_Random.RDS')
mu = est$mu; sigprof = est$sigprof; sigboot = 0; m = est$m
intervals<-function(sig,m, n){
    qt(0.975, m-1) * sig * sqrt((1 + 1/m)/(1:n))
}

q = qnorm(0.975)

i = 3
N = long.trips[i, N];N
sq = trip_specific_sequence(long.trips[i, trip], train, res$rho, rho2=0, FALSE)
mean(2*q*sqrt(N*(1+1/m))*sigprof)
sq$sd*2*q*res$v

a = trips[ trip ==long.trips[i, trip]]
a[,.(tt = sum(tt)/60, dist = sum(length)/1000, .N, timeBins[1])]

