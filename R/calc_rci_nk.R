.calc_rci_nk <- function(data, m_pre, sd_pre, reliability_pre, reliability_post, direction = 1) {
  rci_data <- data %>%
    mutate(
      pre_adj = reliability_pre * (pre - m_pre) + m_pre,
      change_adj = post - pre_adj,
      denominator = sqrt((reliability_pre^2 * sd_pre ^2 * (1 - reliability_pre)) + (sd_pre^2 * (1 - reliability_post))),
      rci = change_adj / denominator
    )

  data_rci_categories <- .calc_improvement(
    data = rci_data,
    rci_cutoff = 1.96,
    direction = direction
  )

  list(
    reliability_post = reliability_post,
    data = data_rci_categories
  )
}
