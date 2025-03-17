#' Compare the CDFs of real data and simulated data in a plot
#' 
#' This function can plot he CDFs of real data and simulated data together.
#' 
#' @param realtime Numeric, vector of the real values
#' @param simulatetime Numeric, vector of simulated values
#' @param simulate_data_name Character, string of the simulated data (default: "simulated_data")
#' @param x_lab Character, string for x-axis label (default: "Total Travel Time (seconds)")
#' @param title Character, string for plot title (default: "CDF of Travel Time")
#' @param x_max Numeric, value of the x-axis maximum (default: 4000)
#'
#' @return A ggplot object displaying compared CDF curves
#'
#' @examples
#'  observed <- rchisq(10000,50)
#'  simulated <- rnorm(10001,50,10)
#'  plot_CDF_compare(observed, simulated, 
#'                  simulate_data_name = "model_output",
#'                  x_lab="numeric result",
#'                  title = "CDF of data",
#'                  x_max = 100)
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>                
#' @export
#' @importFrom ggplot2 ggplot stat_ecdf labs coord_cartesian scale_color_manual theme element_rect aes
plot_CDF_compare <- function(realtime, simulatetime, simulate_data_name = "simulated_data",
                             x_lab = "Total Travel Time (seconds)", title = "CDF of Travel Time", x_max = 4000) {
  real_df <- data.frame(time = realtime, type = "real data")
  simulate_df <- data.frame(time = simulatetime, type = simulate_data_name)
  combined_df <- rbind(real_df, simulate_df)
  color_values <- c("real data" = "red", "black")
  names(color_values)[2] <- simulate_data_name
  plot1 <- ggplot2::ggplot(combined_df, ggplot2::aes(x = time, color = type)) +
    ggplot2::stat_ecdf() +
    ggplot2::labs(title = title, x = x_lab, y = "Cumulative Probability") +
    ggplot2::coord_cartesian(xlim = c(0, x_max), ylim = c(0, 1)) +
    ggplot2::scale_color_manual(name = "Legend", values = color_values) +
    ggplot2::theme(
      legend.position = c(0.95, 0.5),
      legend.justification = c(1, 1),
      legend.text.align = 0,
      legend.background = ggplot2::element_rect(color = "black", fill = "white")
    )
  return(plot1)
}
