rm(list= ls())
library(data.table)
library(scales)
source('00_clean_trips.R')
#library(traveltimeHMM)


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

long.trips = trips[, .N, trip][order(N, decreasing = TRUE)][1:50,trip]
long.trips = test[, .N, trip][order(N, decreasing = TRUE)][1:50,trip]


est = read.est('estimates_random.csv')
res = readRDS('estimates_PS_Random.RDS')
mu = est$mu; sigprof = est$sigprof; sigboot = 0; m = est$m
n = 155
ymax = 70
ylim = c(0, 70)
q = qnorm(0.975)
pdf('pred_seq_random.pdf', height = 7, width = 14)
plot_intervals(mu ,sigprof ,sigboot, lines = FALSE,
               m = m,
               n = n,
               lwd = 2,
               lty = 1,
               type = 'l',
               col = 'cyan3',
               ylim = ylim,
               cex.lab = 1.4)
plot_trips(long.trips,col = alpha('grey', 0.5), lwd = 1, lty = 3)
plot_intervals(mu ,sigprof ,0, lines = TRUE, n = 200, m = m,
               lwd = 2,
               lty = 1,
               type = 'l',
               col = 'cyan3')
## plot main trip
i = 4
plot_trips(long.trips[i], lines = TRUE, col = 'black', lwd = 2, lty = 3)
A = trip_specific_sequence(long.trips[i], test, res$rho, 0, TRUE)
A$v.seq2 = trip_specific_sequence(long.trips[i], test,
                                  res$rho, res$rho2, TRUE)$v.seq
B = A[, .(lwr = t.seq - q * v.seq * res$v,
          upr = t.seq + q * v.seq * res$v,
          lwr0 = t.seq - q * v.seq,
          upr0 = t.seq + q * v.seq,
          lwr2 = t.seq - q * v.seq2 * res$v2,
          upr2 = t.seq + q * v.seq2 * res$v2)][, .(lwr = lwr/1:.N,
                                                   upr = upr/1:.N,
                                                   lwr0 = lwr0/1:.N,
                                                   upr0 = upr0/1:.N,
                                                   lwr2 = lwr2/1:.N,
                                                   upr2 = upr2/1:.N)]
B[, lines(1:.N, lwr, col = 'black', lwd = 2, lty = 1, type = 'l')]
B[, lines(1:.N, upr, col = 'black', lwd = 2, lty = 1, type = 'l')]
B[, lines(1:.N, lwr2, col = 'darkolivegreen4', lwd = 2, lty = 1, type = 'l')]
B[, lines(1:.N, upr2, col = 'darkolivegreen4', lwd = 2, lty = 1, type = 'l')]
B[, lines(1:.N, lwr0, col = 'darkred', lwd = 2, lty = 1, type = 'l')]
B[, lines(1:.N, upr0, col = 'darkred', lwd = 2, lty = 1, type = 'l')]
lines(res$coverage$N,res$coverage$emp.cov.v * ymax, lty = 2, lwd = 2)
lines(res$coverage$N,res$coverage$emp.cov.nov * ymax, lty = 2, lwd = 2, col = 'darkred')
lines(res$coverage2$N,res$coverage2$emp.cov.v * ymax, lty = 2, lwd = 2, col = 'darkolivegreen4')
#lines(res$coverage2$N,res$coverage2$emp.cov.v * 100, lty = 4, lwd = 1)
samp = res$sample_trips
prof = intervals(sigprof, m, n = n)
A = trips[trip %in% samp][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
A[, mean(abs(tt-mu) <= prof[N]),N][N<=n][, lines(N, ymax*V1, lwd =2, lty = 2, col =  'cyan3')]
mtext("Empirical coverage (%)",side=4,line=4, cex = 1.4) 
axis(4, at = c(70, 60, 50),
     labels = c(100, 90, 80),
     las=1, cex.axis = 1.2, cex.lab= 1.2)
axis(4, at= 65, labels = 95,
     las=1, cex.axis = 1.2, cex.lab= 1.2)
abline(h=65, col = 'black', lty=3, lwd=1)
legend(x=60,
       y = 55,
       legend = c('Population intervals (a)',
                  'Trip-specific sequences (b)',
                  'Trip-specific sequences (v=1) (c)',
                  'Trip-specific sequences (second order) (d)',
                  'single trip',
                  '% of empirical coverage (colored by interval type)',
                  'sample of test trips'
                  ),
       lty =c(1,1,1,1,3, 2),
       col = c('cyan3',
               'black',
               'darkred',
               'darkolivegreen4',
               'black',
               'black',
               'gray') ,
       lwd= c(2,2,2,2,2,2),
       cex =1.5,
       #title = 'At random',
       box.lwd = 0,
       bty = 'n'
       )
dev.off()


