library(data.table)
trips = fread('../trips.csv')
trips[, whour :=as.POSIXlt(time)$wday*24 + as.POSIXlt(time)$hour]
trips[, week :=week(time)]

M = 1000
t = trips[, .N , by=trip][N>10][, trip]

## descriptive statistics
trips[, sum(tt), trip][, .(median(V1), mean(V1), max(V1))]/60

trips[, sum(length), trip][, .(median(V1), mean(V1), max(V1))]/1000

pdf('average_trips_per_weekhour.pdf', height = 5, width = 10)
par(mar = c(5,5,0,0)+0.1)
plot(trips[, .SD[, .(numTrips= length(unique(trip))),by=week],by=whour][order(whour)][, mean(numTrips),by=whour],
     type='b',
     xlab='Hour of the week',
     ylab = 'Average number of trips',
     cex.lab=2, cex.axis=1.5,
     axes=FALSE )
axis(side=1, at = 0:7*24, cex.axis = 1.5, cex.lab = 2)
axis(side=2, at = c(0,500), cex.axis = 1.5, cex.lab = 2)
dev.off()
?plot


