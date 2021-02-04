
sample_rand = sample_random(500, n=10, n2 = 50)
res = plot_hatmu(sample_rand, 'At random')
v = sigma_bootstrap(sample_rand)
sample_rand = sample_random(500, n=50, n2=100)
res = plot_hatmu(sample_rand, 'At random')
v = sigma_bootstrap(sample_rand)
sample_rand = sample_random(300, n=150)
res = plot_hatmu(sample_rand, 'At random')
v = sigma_bootstrap(sample_rand)




mu hat  17.1036 (lwq, upq) 16.5309 17.6763 sigma  42.48282 En  0.0372777 sigprof 33.75842
bootstrapping 24.52251 (lwr, upr) 17.83026 36.198
mu hat  16.87209 (lwq, upq) 16.24054 17.50363 sigma  51.66206 En  0.01410738 sigprof 60.51495
bootstrapping 35.89718 (lwr, upr) 18.41044 39.82773
mu hat  16.227 (lwq, upq) 15.76974 16.68427 sigma  27.08302 En  0.007100028 sigprof 61.7616
bootstrapping 39.20889 (lwr, upr) 18.08463 84.78429



trips_hist(samp, mu, sigprof, sigboot, m = 1000, til = 'At random')
t = trips[, .N , by=trip][N>180][, trip]
t= trips[, sum(length), by = trip][order(V1, decreasing=TRUE)][1:200 , trip]

trips[trip %in% t][, which(sum(tt)/.N > 25),trip]

t = setdiff(t, c(trips[trip %in% t][, which(sum(tt)/.N > 25),trip][, trip]))

### Random sampling
set.seed(12345)
M = 500
sample_rand  = t
A = trips[trip %in% sample_rand][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
samp = sample_rand
pdf('progressive_random.pdf')
est = read.est('estimates_random.csv')
mu = est$mu; sigprof = est$sigprof; sigboot = est$sigboot; m = est$m
plot_intervals(mu ,sigprof ,sigboot, lines = FALSE, n =300, m = m)
plot_coverage(samp,mu ,sigprof ,sigboot, n = 300, m = m )
plot_trips(samp)

trips_hist(samp, mu, sigprof, sigboot, m = 1000, til = 'At random')





### tex table
files = c('estimates_random.csv',
          'estimates_timebin.csv',
          'estimates_MR.csv' ,
          'estimates_ER.csv',
          'estimates_Other.csv')

f = files[1]
sapply(files, function(f){
    v = read.est(f)
    v$sigboot/v$sigprof
        })




x = seq(0,1 ,by = 0.01)
qnorm(x)
B[, epsilon := (obstt - t.seq)/v.seq]
B[, lapply(x, function(a)  mean(epsilon < qnorm(a)))]

plot(quant, empirical.quant)
lines(quant, empirical.quant.v)
length(quant)
length(empirical.quant)


