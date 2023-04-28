#' Calculate sample size for a given confidence level, standard deviation and age category
#'
#' @param conf A numeric value representing the confidence level for the interval (e.g. 0.95 for a 95% CI)
#' @param mean A numeric value representing the normative mean values (optional)
#' @param sd A numeric value representing the normative standard deviation values
#' @param ME A numeric value representing the meaningful effect
#' @param agecat A factor variable representing the age category of the normative values
#'
#' @return A list with data, recommendation of minimum require sample and recommendation of the total sample for the study
#' @export
#'
#' @examples
#' # Normativ values from Werle et.al
#'
#' Werle_age <-c("18-19", "20-24", "25-29", "30-34", "35-39",
#' "40-44", "45-49", "50-54", "55-59", "60-64",
#' "65-69", "70-74", "75-79", "80-84", "85+")
#'
#' agecat <- factor(Werle_age, levels = c("18-19", "20-24", "25-29", "30-34", "35-39",
#'                                        "40-44", "45-49", "50-54", "55-59", "60-64",
#'                                        "65-69", "70-74", "75-79", "80-84", "85+"))
#' # Values for Females
#' Werle_females_sample <- c(31, 31, 30, 30, 42, 39, 40, 34, 28, 30, 34, 27, 26, 32, 28)
#' Werle_females_D_mean_kg <-  c(32.0, 33.4, 34.3, 33.8, 35.8, 34.0, 34.1, 33.7, 31.9,
#'  28.7, 29.5, 26.4, 25.0, 19.2, 16.9)
#' Werle_females_D_SD_kg <- c(4.8, 5.4, 5.7, 5.9, 6.7, 6.0, 5.3, 4.5, 4.9, 5.5, 3.6, 6.8,
#'  4.5, 5.2, 4.8)
#'
#' normWerleFemales <- data.frame(agecat,Werle_females_sample,
#' Werle_females_D_mean_kg, Werle_females_D_SD_kg)
#'
#'calc_samplesize(
#'   mean = normWerleFemales$Werle_females_D_mean_kg,
#'   sd = normWerleFemales$Werle_females_D_SD_kg,
#'   ME = 1.2,
#'   agecat = normWerleFemales$agecat,
#'   conf = 0.95)

calc_samplesize <- function(conf = 0.95, mean = NULL, sd, ME, agecat){
  if (is.null(mean) == FALSE){
    Z <- round(qnorm(p = 1-(1-conf)/2),2) # Calculate z-score
    n <- ceiling(((Z*sd)/ME)^2) # Calculate minimum required sample size
    dat <- data.frame(agecat, mean, sd, Z, ME, n) # Create data frame
    s <- max(dat["n"]) # Calculate maximum sample size
    ts <- max(dat["n"]) * length(agecat) # Calculate total sample size
    # Create output strings
    sample <- paste0("A sample size of minimum ", s , " for ME of ", ME ," kg for each age category is recommended.")
    totsample <- paste("This corresponds to a total study of",s,"x",length(agecat),"=", ts)
    return(list(Data = dat, SampleSize = sample, TotalSampleSize = totsample))
  }
  else{
    Z <- round(qnorm(p = 1-(1-conf)/2),2) # Calculate z-score
    n <- ceiling(((Z*sd)/ME)^2) # Calculate minimum required sample sizte
    dat <- data.frame(agecat, sd, Z, ME, n) # Create data frame
    s <- max(dat["n"]) # Calculate maximum sample size
    ts <- max(dat["n"]) * length(agecat) # Calculate total sample size
    # Create output strings
    sample <- paste0("A sample size of minimum ", s , " for ME of ", ME ," kg for each age category is recommended.")
    totsample <- paste("This corresponds to a total study of",s,"x",length(agecat),"=", ts)
    return(list(Data = dat, SampleSize = sample, TotalSampleSize = totsample))
  }
}
