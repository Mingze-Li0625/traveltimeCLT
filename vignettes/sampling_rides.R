
library(traveltimeCLT)
library(data.table)
data(trips)

## Sampling autocorrelated uniforms
library(mvtnorm)
n <- 20
rho <- 0.3
S <-diag(n)
for (i in 1:n) {
    for (j in 2:n) {
        S[i, j] <- rho^(abs(i-j))
    }
}
S = S +t(S)
diag(S)<- 1
St = 2 * sin(S * pi/6)
U = pnorm(rmvnorm(1, sigma = St))

acf(U, lag.max =2, plot=FALSE)



### Testing
rm(list = ls())
library(mvtnorm)
library(data.table)
source('~/src/traveltime-clt-paper/plots/00_clean_trips.R')
library(ggplot2) 
set.seed(1234)

t = trips[, .N , by=trip][N>10][, trip]
trips = trips[trip %in% t]

freq = trips[, .(N=.N,
                 speed_mean_ms = mean(length/tt),
                 speed_std_ms = sd(length/tt),
                 length = mean(length)),
             list(linkId, timeBins)][order(N, decreasing=TRUE)]

freq[, index:=1:.N]

freq[, N:=N/sum(N)]
freq

my_plot<-ggplot(freq, aes(x =index , y = N)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "edge x TimeBins", y = "visit likelihood") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

ggsave(filename = "freq.png", plot = my_plot, width = 6, height = 4) 

my_plot_mean<-ggplot(freq, aes(x =index , y = speed_mean_ms)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "edge x TimeBins", y = "average speed (m/s)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

freq$speed_std_ms[is.na(freq$speed_std_ms)] <-0.1

my_plot_sd<-ggplot(freq, aes(x =index , y = speed_std_ms)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "edge x TimeBins", y = "standard deviation of speed (m/s)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 



ggsave(filename = "speed_mean.png", plot = my_plot_mean, width = 6, height = 4) 


ggsave(filename = "speed_std.png", plot = my_plot_sd, width = 6, height = 4) 


number_of_edges = trips[, .(N=.N), by=list(trip)]

num_edges = ggplot(number_of_edges, aes(x = N)) +
    geom_histogram(binwidth = 2, fill = 'black', color = "white") +
    labs(x = "Number of edges per ride", y = "Density") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(filename = "num_edges.png", plot =num_edges, width = 6, height = 4) 


dependent_uniform<-function(n, rho=0.31) {
    S <-diag(n)
    for (i in 1:n) {
        for (j in 2:n) {
            S[i, j] <- rho^(abs(i-j))
    }
}
    S = S +t(S)
    diag(S)<-1 
    St = 2 * sin(S * pi/6) # must be positive definite
    U = c(pnorm(rmvnorm(1, sigma = St)))
    U
}
    

road_graph_free_sampler<-function(trips, num_rides = 1000) {
    number_of_edges = trips[, .(N=.N), by=list(trip)]
    freq = trips[, .(N=.N,
                     speed_mean_ms = mean(log(length/tt)),
                     speed_std_ms = sd(log(length/tt)),
                     length = mean(length)),
                 list(linkId, timeBins)][order(N, decreasing=TRUE)]
    freq$speed_std_ms[is.na(freq$speed_std_ms)] <-0.1
    freq[, index:=1:.N]
    freq[, N:=N/sum(N)]
    n = sample(number_of_edges$N, num_rides, replace=TRUE)
    t = sapply(n, function(l) {
        ride = sample(freq$index, l, replace=FALSE, prob = freq$N)
        cond = freq$index %in% ride
        mu = freq[cond][, speed_mean_ms]
        sigma = freq[cond][, speed_std_ms]
        length = freq[cond][, length]
        U = dependent_uniform(l, rho = 0.31)
        Z = qnorm(U)
        speed = exp(mu + sigma*Z)
        tt= length/speed
        sum(tt)
    })
    t
    
}

t = road_graph_free_sampler(trips, 1000)

CDF_sample = ecdf(t)
CDF = trips[, .(tt = sum(tt)), by=trip][, ecdf(tt)]

png("travel_time_plot.png",width = 400, height = 600)  # Specify filename 
# Your plot code (as written in your previous example)

plot(CDF, 
main = "Comparison of Travel Time Distributions",
xlim=c(0, 4000), xlab= "Travel time (s)", ylab = "CDF")
lines(CDF_sample, col ='red')
legend("bottomright", legend=c("Empirical CDF", "Sampled CDF"), 
       col=c("black", "red"), lty=c(1, 1), cex=2) 
dev.off() 






