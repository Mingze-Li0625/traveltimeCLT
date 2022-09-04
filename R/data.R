#' A sample of 4914 trips made in Quebec City in 2014
#'
#' A dataset containing 4914 driving trips with a total of 322799 individual observations.
#'
#' @format A data frame with 322799 rows and 7 variables:
#' \describe{
#'   \item{tripID}{a numeric unique trip ID}
#'   \item{linkID}{a numeric ID representing differet road links}
#'   \item{timeBin}{a character indicating one of 5 timeBins (Weekday, MorningRush, EveningRush. EveningNight, Weekendday)}
#'   \item{speed}{the average speed in meters/second for the corresponding trip and linkID}
#'   \item{duration_secs}{traversal duration in secs}
#'   \item{distance_meters}{traversal distance in meters}
#'   \item{entry_time}{the first observed GPS timestamp on the corresponding linkID}
#' }
#' @source NULL
"trips"
