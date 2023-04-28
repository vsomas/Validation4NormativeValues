#' Calculate percentile values and percentile plot based on tabulated normative mean and SD
#'
#' @param mean A numeric value representing the normative mean values
#' @param sd A numeric value representing the normative standard deviation values
#' @param agecat A factor variable representing the age category of the normative values
#' @param data A data frame representing the sample data
#' @param agecatData A factor variable representing the age categories of each sample
#' @param sample A numeric vector representing the collected measurements
#' @param description A string representing the description of the normative values
#' @param title A string representing the plot title
#' @param linesize A numeric value representing the line size of the percentile curves (default = 1.5)
#' @param showData A logical value indicating whether to show data in the plot (default =FALSE)
#'
#' @return A list with normative values and a percentile plot
#' @export
#'
#' @examples

calc_pct <- function(mean, sd, agecat,
                     data = NULL, agecatData = NULL, sample = NULL,
                     description = NULL, title = NULL, linesize = 1.5,
                     showData = FALSE) {
  # Define percentiles
  pct <-  c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

  # Calculate percentiles
  ps <- lapply(as.list(pct), function(x) stats::qnorm(p=x, mean = mean, sd = sd))
  ps <- t(do.call("rbind", ps))
  colnames(ps) <- paste0("quantile", pct * 100)

  # Combine age category, mean and percentiles in a data frame
  df <- data.frame(agecat, mean, sd, ps)
  if (showData == FALSE){
    # Plot percentiles
    plt <- ggplot2::ggplot()+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile5, group = "0.05", col = "0.05"), size = linesize)+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile10, group = "0.10", col = "0.10"), size = linesize)+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile25, group = "0.25", col = "0.25"), size = linesize)+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile50, group = "0.50", col = "0.50"), size = linesize)+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile75, group = "0.75", col = "0.75"), size = linesize)+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile90, group = "0.95", col = "0.90"), size = linesize)+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile95, group = "0.95", col = "0.95"), size = linesize)+
      ggplot2::scale_color_manual("Quantile",
                         values = c("0.05"="indianred3",
                                    "0.10"="indianred2",
                                    "0.25"="lightskyblue2",
                                    "0.50"="lightskyblue3",
                                    "0.75"="lightseagreen",
                                    "0.90"="darkseagreen3",
                                    "0.95"="darkseagreen4"))+
      ggpubr::theme_pubclean()+
      ggplot2::xlab("Age Category")+
      ggplot2::ylab("Handgrip Strength (kg)")+
      ggplot2::labs(title = paste(title),
           caption = paste("Data Source:", description))+
      ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9),
            legend.key = element_rect(fill = "NA"))+
      ggplot2::theme(plot.title = element_text(face ="bold", hjust = 0.5),
            plot.caption = element_text(face = "italic"))+
      ggplot2::guides(colour = guide_legend(nrow = 1))

    return(list(NormativeValues = df, plot = plt))
  }
  if (is.null(data) == FALSE & showData == TRUE){
    plt <- ggplot2::ggplot()+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile5, group = "0.05", col = "0.05"))+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile10, group = "0.10", col = "0.10"))+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile25, group = "0.25", col = "0.25"))+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile50, group = "0.50", col = "0.50"))+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile75, group = "0.75", col = "0.75"))+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile90, group = "0.95", col = "0.90"))+
      ggplot2::geom_line(data = df, aes(x = agecat, y = quantile95, group = "0.95", col = "0.95"))+
      ggplot2::geom_point(data = data, aes(x = agecatData, y = sample), color = "orange")+
      ggplot2::scale_color_manual("Quantile",
                         values = c("0.05"="indianred3",
                                    "0.10"="indianred2",
                                    "0.25"="lightskyblue2",
                                    "0.50"="lightskyblue3",
                                    "0.75"="lightseagreen",
                                    "0.90"="darkseagreen3",
                                    "0.95"="darkseagreen4"))+
      ggpubr::theme_pubclean()+
      ggplot2::xlab("Age Category")+
      ggplot2::ylab("Handgrip Strength (kg)")+
      ggplot2::labs(title = paste(title),
           caption = paste("Data Source:", description))+
      ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.9),
            legend.key = element_rect(fill = "NA"))+
      ggplot2::theme(plot.title = element_text(face ="bold", hjust = 0.5),
            plot.caption = element_text(face = "italic"))+
      ggplot2::guides(colour = guide_legend(nrow = 1))
    return(list(NormativeValues = df, plot = plt))
  }
  else {
    print("Incorrect entry, did you provide data?")
  }
}
