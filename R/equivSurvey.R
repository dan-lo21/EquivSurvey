#' equivSurvey function
#'
#' Test equivalence of two non-overlaping survey samples
#'
#' @details This function will test the equivalence of two non-overlapping survey samples
#'
#' @param delta_l The lower bound of the equivalence test, chosen by the user
#' @param delta_u The upper bound of the equivalence test, chosen by the user
#' @param design12 The survey design, obtained from svydesign function
#' @param indicator The outcome of interest as a string
#' @param subset Column name for survey indicator
#'
#' @return Table with equivalence test results
#'
#' @import survey
#' @export
#'
#' @examples
#' # equivSurvey(-0.05, 0.05, design12, 'fp19b', 'inMethod1OR2')
#'
equivSurvey<-function(delta_l, delta_u, design12, indicator, subset){

  #define the indicator and the data subsets denoted by 1 and 2
  ind <- make.formula(paste("(",indicator,")"))
  sub <- make.formula(subset)

  #generate the weighted estimates
  p <- svyby(ind, sub, design=design12, FUN=svymean, ci=TRUE, vartype=c("ci","se","var"), na.rm=TRUE)

  #difference
  diff12 <- p[1, c(indicator)] - p[2, c(indicator)]

  #the variance of the difference is the sum of the two variances - ignore overlaps
  var_diff <- p$var[1] + p$var[2]

  #90% confidence interval
  upper90ci <- diff12 + qnorm(0.95) * sqrt(var_diff)
  lower90ci <- diff12 - qnorm(0.95) * sqrt(var_diff)

  #Test statistics for equivalence tests
  z_lo <- (diff12 - delta_l) / (sqrt(var_diff))
  z_up <- (diff12 - delta_u) / (sqrt(var_diff))
  p_lo <- pnorm(-abs(z_lo), lower.tail = TRUE)
  p_up <- pnorm(-abs(z_up), lower.tail = TRUE)

  #significant-> equivalent , 90%CI completely within bounds; nonsign - not completely within bounds, also nonsign-   totally outside
  #p-values is aligned with CI, so max of the two if at least part of the 90 CI falls with the threshold bands else   its 1 (nonsign)

  if (upper90ci < delta_l | lower90ci > delta_u) {p_equiv <- 1}
  else {p_equiv <- max(p_lo, p_up)}

  #output
  stats <- rbind(p[1, c(indicator)], p$var[1], p[2, c(indicator)], p$var[2],
                 diff12, var_diff, z_lo, p_lo, z_up, p_up, p_equiv, upper90ci, lower90ci)

  #return(stats)
  row.names(stats)<-c("Mean  (Subset 1)",
                      "Variance  (Subset 1)",
                      "Mean  (Subset 2)",
                      "Variance  (Subset 2)",
                      "Effect (Difference in Mean)",
                      "Variance of Difference",
                      "Z lower bound",
                      "p lower bound",
                      "Z upper bound",
                      "p upper bound",
                      "p value for test of equivalence",
                      "90% CI Lower",
                      "90% CI Upper")
  #attr(stats,'class') <- "EquivWeighted"

  stats
}
