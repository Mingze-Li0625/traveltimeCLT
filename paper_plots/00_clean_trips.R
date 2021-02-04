library(data.table)
library("rjson")
trips = fread('../trips.csv')
trips[, whour :=as.POSIXlt(time)$wday*24 + as.POSIXlt(time)$hour]
r = trips[3.6*exp(logspeed)>150][, trip[1],trip][, trip]
trips= trips[!trip %in% r][order(trip, time)]
## removing last observation
trips = trips[,.SD[-.N] , trip]
trips = trips[order(trip, time)]


read.est <- function(filename){
    est  = as.list(read.csv(filename))
    est
}


## Sample radnom trips
sample_random <-function(M = 500, n = 10, n2 = NULL, filename = NULL){
    t = trips[, .N , by=trip][N>= n][, trip]
    if(!is.null(n2)){
        t = trips[, .N, by = trip][N>=n & N<=n2][, trip]
    }
    sample_rand  = t[sample.int(length(t), 1.5*M)]
    if(!is.null(filename)){
        samp = setdiff(sample_rand, readRDS(filename))
        samp = samp[sample.int(length(samp),M)]
    }else{

        samp = sample_rand[sample.int(length(sample_rand), M)]
    }
    samp
}


## sample time bins
sample_time_bins<-function(M, bin = 'MR', filename = NULL){
    t = trips[, .N , by=trip][N>10][, trip]
    sample_timebin = trips[trip %in% t, .(timeBins= if(all(timeBins == timeBins[1])) timeBins[1] else 'remove' ), by=trip]
    sample_timebin = sample_timebin[!timeBins == 'remove']
    samp = sample_timebin[timeBins == bin][, trip]
    if(!is.null(filename)){
        samp = setdiff(unique(samp), readRDS(filename))
    }
    samp = samp[sample.int(length(samp),M)]
    samp
}



create_test_trips<-function(M = 500, db, min.n = 1){
    ## Setting the graph
    db[, linkId.from := linkId, by = trip]
    db[, linkId.to := shift(linkId, type = 'lead'), by = trip]
    db[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
    A = db[, .(N = N[1]), by =list(linkId.from, linkId.to, timeBins) ]
    A[, N.left := 0 ] 
    setkey(A, linkId.from, linkId.to, timeBins)
    
    bank = c()
    unique.trips =db[, min(N>1)  , trip][V1 ==1][, trip]

    repeat{
        n = length(unique.trips)
        car = unique.trips[sample.int(n,1)]
        k = db[trip==car, .(linkId.from, linkId.to, timeBins)]
        
        if(A[k, all(N -N.left - 1> 0 )]){
            bank = c(bank,car)              ## add the trip
            unique.trips = setdiff(unique.trips, bank) ## removing all smapled trips
            ## updating A
            A[k, N.left:= N.left + 1]
            ## break if number of samples reached
            if(length(bank) >=M)
                break
        }
    
    }
    bank
}


### Calculating the PS sequence for a trip
param_zeta<-function(t0, rho,linkfrom, linkto, len, sequence = FALSE, rho2=0){
    rules = list(
        list(start='6:30', end= '9:00', days = 1:5, tag='MR'),
        list(start='15:00', end= '18:00', days = 1:5, tag='ER')
    )
    t0 = as.POSIXlt(t0)
    time_bins <- rules2timebins(rules)
    tbin = time_bins(t0)
    g = graph.stat.full[linkId.from %in% linkfrom]
    t = 0                               # time (mean)
    vcumlative = 0                                 # variance
    vprevprev = vprev = 0                          # lag-{1,2} segment variance
    lenprevprev = lenprev = 0                      # lag-{1.2} length
    if(sequence){
        t.seq = v.seq = numeric(length(len))
    }
    for(i  in 1:(length(len) - 1)){
    ##for(i in 1:81){
        d = g[linkId.from == linkfrom[i] & linkId.to == linkto[i] & timeBins == tbin]
        t = t + len[i]*d$mean
        tbin = time_bins(t0 + t)
        vcumlative = vcumlative + len[i]^2  * d$sd^2  +
            2*rho * len[i] * lenprev * d$sd * vprev +
            2*rho2 * len[i] * lenprevprev *d$sd *vprevprev
        
        vprevprev=vprev
        lenprevprev = lenprev
        vprev = d$sd
        lenprev = len[i]
        if(sequence){
            v.seq[i] = vcumlative
            t.seq[i] = t
        }
    }
    ## last linkId.to is NA
    i = i+1
    if(is.na(linkto[i])){
        d = g[linkId.from == linkfrom[i] & timeBins == tbin & is.na(linkId.to)]
    }else{
        d = g[linkId.from == linkfrom[i] & linkId.to == linkto[i] & timeBins == tbin]
    }
    t = t + len[i]*d$mean
    vcumlative = vcumlative + len[i]^2 * d$sd^2 +
        2*rho * len[i] * lenprev * d$sd * vprev +
        2*rho2 * len[i] * lenprevprev * d$sd *vprevprev
    if(sequence){
        v.seq[i] = vcumlative
        t.seq[i] = t
    }
    s = sqrt(vcumlative)
    if(sequence){
        list(tt =t, sd = s, v.seq = sqrt(v.seq), t.seq = t.seq)
    }else{
        list(tt =t, sd = s)
    }
}


residual_variance<-function(db, rho, samp = NULL, rho2 = 0){
    if(!is.null(samp)){
        A = db[trip %in% samp][order(trip, time)]
    }else{
        A = db[order(trip, time)]
    }
    B = A[, param_zeta(time[1], rho, linkId.from, linkId.to, length, rho2=rho2), by = trip]
    D = merge(B, A[, .(obstt= sum(tt)), trip])
    D[, res := (obstt - tt)/sd]
    list(db = D, res.sd = sd(D$res))
}


numerical_res<-function(tt, est, sd = 1, uq = qnorm(0.975), lq = qnorm(0.025)){
    epsi = (tt - est)/sd
    alpha1 = 1 - seq(0,1, by = 0.01)
    list(
        RMSE = sqrt(mean((tt - est)^2)),
        MAE  = mean(abs(tt - est)),
        ME  = mean(tt- est),
        MAPE = 100*mean(abs(tt - est)/tt),
        emprical.cov = 100*mean( epsi >= lq & epsi <= uq),
        PI.length = mean((uq- lq)*sd),
        PI.rel.length = 100*mean((uq - lq)*sd /tt),
        WRMSE = sqrt(mean(epsi^2)),
        coverage.levels = list(alpha1),
        coverage = list(sapply((1 - alpha1)/2, function(s)
            mean(abs(epsi) <= abs(qnorm(s)))))
        )
}


get.params.segment <-function(db, L){
    L = 10                                  # minimum number of observatinos
    graph.stat = db[, .(mu = mean(1/speed),  sd2 = var(1/speed), N=N[1]) ,by = list(linkId.from, linkId.to, timeBins)]
    full.graph = graph.stat[, .(timeBins = c('Other', 'MR', 'ER')) ,by = list(linkId.from, linkId.to)]
    graph.stat = merge(full.graph, graph.stat, by= c('linkId.from', 'linkId.to', 'timeBins'), all.x = TRUE)
    graph.stat$N[is.na(graph.stat$N)]<-0
    graph.stat.from.tb = db[, .(mu = mean(1/speed),  sd2 = var(1/speed), N= .N) ,by = list(linkId.from, timeBins)]
    graph.stat.tb = db[, .(mu = mean(1/speed),  sd2 = var(1/speed), N= .N) ,by = list(timeBins)]
    graph.stat.full =  merge(graph.stat, graph.stat.from.tb[N>L, .(mu.from.tb = mu, sd2.from.tb = sd2, linkId.from, timeBins)], all.x = TRUE, by.x=c('linkId.from', 'timeBins'), by.y = c('linkId.from', 'timeBins'))
    graph.stat.full =  merge(graph.stat.full, graph.stat.tb[N>L, .(mu.tb = mu, sd2.tb = sd2, timeBins)], all.x = TRUE, by.x=c('timeBins'), by.y = c('timeBins'))


    ## imputation ( only impute when NA, not when < L)
    ## imputing average based on time bin average if missing
    graph.stat.full[, muspeed := ifelse(is.na(mu), ifelse(is.na(mu.from.tb), mu.tb, mu.from.tb), mu)]
    graph.stat.full[,  imputed_mean := ifelse(muspeed== mu, FALSE, TRUE )]
    ## imputing sd based on time bin average if missing
    graph.stat.full[, sdspeed := ifelse(is.na(sd2), ifelse(is.na(sd2.from.tb), sd2.tb, sd2.from.tb), sd2)]
    graph.stat.full[, imputed_sd := ifelse(sdspeed == sd2  & !is.na(sd2) , FALSE, TRUE)]
    ## imputing the end of the trip, since there is no next link
    ## you don't need to impute sd similarly, since they are already imputed from above.
    ## graph.stat.full[, muspeed := ifelse(is.na(linkId.to), ifelse(is.na(mu.from.tb), mu.tb, mu.from.tb), mu)]
    ## graph.stat.full[, sdspeed := ifelse(is.na(linkId.to), ifelse(is.na(sd2.from.tb), sd2.tb, sd2.from.tb), sdspeed)]

    ## shortining the output
    graph.stat.full = graph.stat.full[,.(linkId.from, linkId.to, timeBins, mean = muspeed, sd = sqrt(sdspeed), imputed_sd, imputed_mean )]
}


get_rho<-function(samp= NULL, lag =1){
    if(is.null(samp)){
        tt = train
        xx = tt[, .I[.N>10],by = trip]
    }else{
        tt = train[trip %in% samp]
        xx = tt[trip %in% samp, .I[.N>10],by = trip]
    }
    rho = tt[xx$V1][,    drop((acf((1/speed - mean)/(sd+1e-5), plot=FALSE, lag.max=5))[[1]]), by = trip]
    a = rho[, V1[1+lag] ,by = trip][, V1]
    a
}





## plotting
plot_trips<-function(samp, lines = TRUE, ...){
    A = trips[trip %in% samp][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
    if(!lines){
        ttt = A$trip[1]
        A[trip == ttt][,plot(N, tt, ylim = c(0,100), ...)]
    }
    A[, lines(N, tt, ... ), trip]
                
}

plot_intervals<-function(mu,
                         sigprof,
                         sigboost= 0L,
                         m = 1000,
                         lines = FALSE,
                         n =120
                         , ...){
    prof = intervals(sigprof, m, n)
    boost = intervals(sigboost, m, n)
    if(!lines){
        par(mar = c(5,5,0,5)+0.1)
        plot(1:n,  mu + prof,
             xlab = 'Number of edges (n)',
             ylab = 'Average travel time (seconds)',
             xaxs='i',
             cex.lab=1.2,
             cex.axis=1.2,
             axes=TRUE,
             ...
             )
    }else{
        lines(1:n,  mu + prof, ...)
    }
    lines(1:n, mu - prof, ...) 
    if(sigboost!=0){
        lines(1:n, mu + boost, ...)
        lines(1:n, mu - boost, ...)
    }
}


intervals<-function(sig,m, n){
    qt(0.975, m-1) * sig * sqrt((1 + 1/m)/(1:n))
}


plot_coverage<-function(samp, mu, sigprof, sigboost = 0L,m = 1000, n = 120){
    prof = intervals(sigprof, m, n)
    if(sigboost>0)
        boost = intervals(sigboost, m, n)
    A = trips[trip %in% samp][order(trip, time)][, .(N = 1:.N, tt = cumsum(tt)/(1:.N), whour = whour[1], timeBins = timeBins[1] ), by = trip]
    if(sigboost>0)
        A[, mean(abs(tt-mu) <= boost[N]),N][N<=n][, lines(N, 100*V1, lwd = 2, lty=4, col = 'darkred')]
    A[, mean(abs(tt-mu) <= prof[N]),N][N<=n][, lines(N, 100*V1, lwd =1, lty = 2, col =  'black')]
    mtext("Empirical Coverage (%)",side=4,line=4, cex = 2) 
    axis(4, ylim=c(0,100),las=1, cex.axis = 1.5, cex.lab= 2)
    axis(4, at= 95,las=1, cex.axis = 1.5, cex.lab= 2)
    abline(h=95, col = 'black', lty=3, lwd=1)
}



data_split <- function(dt, test.trip){
    test = dt[trip %in% test.trips]
    train = dt[!trip %in% test.trips]
    
    test[, 1, trip][, sum(V1)]
    train[, 1, trip][, sum(V1)]
    
    test[, 1, trip][, sum(V1)] + train[, 1, trip][, sum(V1)]
    
    ## Setting the graph
    train[, linkId.from := linkId, by = trip]
    train[, linkId.to := shift(linkId, type = 'lead'), by = trip]
    train[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
    
    test[, linkId.from := linkId, by = trip]
    test[, linkId.to := shift(linkId, type = 'lead'), by = trip]
    test[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
    list(train = train, test = test)
}


trip_specific_sequence <-function(tripIds, db, rho, rho2 = 0, sequence = TRUE){
    A = db[trip %in% tripIds,
           param_zeta(time[1],
                      rho,
                      linkId.from,
                      linkId.to,
                      length,
                      sequence = sequence,
                      rho2 = rho2), trip]
    A
   
}
