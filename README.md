# traveltimeCLT

## Installation

The package is still under development in the Alpha stage.

Install from [GitHub](https://github.com/AdrienHdz/SUMO_Travel_time_estimation/traveltimeCLT) with:

``` r
# install.packages("devtools")
devtools::install_github("AdrienHdz/SUMO_Travel_time_estimation/traveltimeCLT")
```

## Example

This package includes a small data set (`tripset`) that aggregates
map-matched anonymized mobile phone GPS data collected in Quebec city in
2014 using the Mon Trajet smartphone application developed by [Brisk
Synergies Inc](https://brisksynergies.com/). The precise duration of the
time period is kept confidential.

View the data with:

``` r
library(traveltimeCLT)
library(traveltimeHMM)
library(data.table)

data(trips)
head(trips)
trip	time	timeBins	tt	logspeed	length	linkId
<int>	<chr>	<chr>	<dbl>	<dbl>	<dbl>	<int>
15	2014-05-05 07:32:41.000	MorningRush	8.00	1.801	48.422	24088
15	2014-05-05 07:32:50.000	MorningRush	64.49	0.147	74.703	23470
15	2014-05-05 07:33:54.000	MorningRush	17.34	2.621	238.410	34576
15	2014-05-05 07:34:11.000	MorningRush	4.54	2.875	80.557	34586
15	2014-05-05 07:34:16.000	MorningRush	3.03	2.905	55.259	34583
15	2014-05-05 07:34:19.000	MorningRush	8.56	2.810	142.211	6704

```
Transforming the variables

``` r
trips <- as.data.table(trips)
trips$speed <- exp(trips$logspeed)
```
Splittig data into train and test sets.

``` r
test.trips <- create_test_trips(M = 500, trips, min.n = 1)
test = trips[trip %in% test.trips]
train = trips[!trip %in% test.trips]

```
Creating rules and time-bins

``` r
myrules = list(
  list(start='6:30', end= '9:00', days = 0:6, tag='MorningRush'),
  list(start='15:00', end= '18:00', days = 0:6, tag='EveningRush'))

mytimebins = c("MorningRush", "EveningRush", "Other")
```

Fit the model using the following code:

``` r
ttCLTmodel <- traveltimeCLT(data.train = train, 
			    M = 1000,
			    L = 2,
			    bin = "MorningRush",
			    rules = myrules,
			    data.timebins = mytimebins)
```
To predict on the test set and get estimations:

``` r
ttCLTresults <- predict_traveltimeCLT(obj.traveltime = ttCLTmodel,
				      data.test = test,
				      bin = "MorningRush",
				      rules = myrules)
```


## References

Elmasri, M., Labbe, A., Larocque, D., Charlin, L,
2020. “Prediction intervals for travel time on transportation networks”.

<https://arxiv.org/abs/2004.11292>