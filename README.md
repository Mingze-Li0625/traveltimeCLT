
# Estimate and predict travel time on road networks

Implements two methods for prediction of average travel time on a route
and its uncertainty (variance): a general population-based prediction,
and a trip-specific method. The population-based method provide an
estimate of average travel time and asymptotic Gaussian-based prediction
intervals. The trip-specific methods integrates route data to provide
tight route-specific Gaussian-based predictive distribution. From which,
average travel time and prediction intervals are supplied.

Package is based on [Elmasri et. al. (2020)](https://arxiv.org/abs/2004.11292).

[Package website](https://melmasri.github.io/traveltimeCLT/)

## Installation

Install from [GitHub](https://melmasri.github.io/traveltimeCLT/articles/traveltimeCLT.html).

``` r
# install.packages("devtools")
devtools::install_github("melmasri/traveltimeCLT")
```

## Example

This package includes a small data set (`trips`) that aggregates
map-matched anonymized mobile phone GPS data collected in Quebec city in
2014 using the Mon Trajet smartphone application developed by [Brisk
Synergies Inc](https://brisksynergies.com/). The precise duration of the
time period is kept confidential.

View the data with:

``` r
library(traveltimeCLT)
library(data.table)

data(trips)
head(trips)
 tripID linkID timeBin     speed duration_secs distance_meters            entry_time 
1   2700  10469 Weekday  5.431914     13.000000        70.61488  2014-04-28 06:07:27 
2   2700  10444 Weekday  9.219505     18.927792       174.50487  2014-04-28 06:07:41 
3   2700  10460 Weekday  9.052796      8.589937        77.76295  2014-04-28 06:07:58 
4   2700  10462 Weekday  6.850282     14.619859       100.15015  2014-04-28 06:08:07 
5   2700  10512 Weekday  6.075674      5.071986        30.81574  2014-04-28 06:08:21 
6   2700   5890 Weekday 10.771731     31.585355       340.22893  2014-04-28 06:08:26 
```

Splittig data into train and test sets.

``` r
test_trips = sample_trips(trips, 10)
train = trips[!trips$tripID %in% test_trips,]
test =  trips[trips$tripID %in% test_trips,]
```

Fitting and predicting the `trip-specific` model, with lag 1

``` r
fit <- traveltimeCLT(train, lag = 1)
predict(fit, test)
```

Fitting and predicting the `populaton` model, with lag 1

``` r
fit <- traveltimeCLT(train, model = 'population')
predict(fit, test)
```

## Bugs

For bugs and features, please refer to
[here](https://github.com/melmasri/traveltimeCLT/issues).

## References

Elmasri, M., Labbe, A., Larocque, D., Charlin, L,2020. “Predictive
inference for travel time on transportation networks”.
<https://arxiv.org/abs/2004.11292>
