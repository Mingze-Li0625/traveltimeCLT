#' A sample of 5000 trips over Quebec City in 2014
#'
#' A dataset containing 5000 driving trips for a total of 37105 observations.
#'
#' @format A data frame with 371705 rows and 7 variables:
#' \describe{
#'   \item{tripID}{a numeric unique trip ID}
#'   \item{linkID}{a numeric ID representing differet road links}
#'   \item{timeBin}{a character indicating one of 5 timeBins (Weekday, MorningRush, EveningRush. EveningNight, Weekendday)}
#'   \item{logspeed}{the average log-speed in meters/second for the corresponding trip and linkId}
#'   \item{traveltime}{the travel time of corresponding trip on the specified linkId in seconds}
#'   \item{length}{the trip driving length in meters over the corresponding linkId}
#'   \item{time}{the first observed GPS timestamp on the corresponding linkId of that trip}
#' }
#' @source NULL
"trips"
