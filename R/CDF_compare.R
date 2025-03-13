
#' @export
#' @importFrom ggplot2 ggplot stat_ecdf labs coord_cartesian scale_color_manual theme element_rect
plot_CDF_compare <- function(realtime,simulatetime,simulate_data_name="simulated_data",
                             x_lab="Total Travel Time (seconds)",title= "CDF of Travel Time",x_max=4000){
  travel_time <- data.frame(sampled_time=realtime,simulated_time=simulatetime)
  color_values <- c("sampled data" = "red", simulate_data_name = "black")
  names(color_values)[2] <- simulate_data_name
  plot1 <- ggplot2::ggplot(travel_time) +
    ggplot2::stat_ecdf(ggplot2::aes(x = sampled_time, color = "sampled data")) +
    ggplot2::stat_ecdf(ggplot2::aes(x = simulated_time, color = simulate_data_name)) +
    ggplot2::labs(title = title, x = x_lab, y = "Cumulative Probability") +
    ggplot2::coord_cartesian(xlim = c(0, x_max), ylim = c(0, 1)) +
    ggplot2::scale_color_manual(name = "Legend", values = color_values) +
    ggplot2::theme(
      legend.position = c(0.95, 0.5),
      legend.justification = c(1, 1),
      legend.text.align = 0,
      legend.background = ggplot2::element_rect(color = "black", fill = "white")
    )
  plot1
}
