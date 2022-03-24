prep_rci <- function(data, reliability) {
  sd_pre <- sd(data$pre)
  s_diff <- .calc_s_diff(sd_pre = sd_pre, reliability = reliability)

  data$change / s_diff
}


.calc_s_diff <- function(sd_pre, reliability) {
  s_e <- sd_pre * sqrt(1 - reliability)
  sqrt(2 * s_e^2)
}
