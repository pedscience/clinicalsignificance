check_cutoff <- function(data, m_functional, sd_functional, type = "c", better_is = c("lower", "higher"), lower = 0, upper = 100, resolution = 200) {
  m_clinical <- mean(data$pre)
  sd_clinical <- sd(data$pre)


  # Calculate cutoff
  cutoff <- .calc_cutoff(
    m_clinical = m_clinical,
    sd_clinical = sd_clinical,
    m_functional = m_functional,
    sd_functional = sd_functional,
    type = type,
    better_is = better_is
  )

  a <- .calc_cutoff(m_clinical, sd_clinical, m_functional, sd_functional, "a", better_is)[[6]]
  b <- .calc_cutoff(m_clinical, sd_clinical, m_functional, sd_functional, "b", better_is)[[6]]
  c <- .calc_cutoff(m_clinical, sd_clinical, m_functional, sd_functional, "c", better_is)[[6]]

  ggplot() +
    geom_function(aes(color = "Functional"), fun = dnorm, args = list(mean = cutoff[[3]], sd = cutoff[[4]]), n = resolution) +
    geom_function(aes(color = "Clinical"), fun = dnorm, args = list(mean = cutoff[[1]], sd = cutoff[[2]])) +
    # geom_density(data = data, aes(pre)) +
    geom_vline(xintercept = a) +
    geom_vline(xintercept = b) +
    geom_vline(xintercept = c) +
    expand_limits(x = c(lower, upper))
}
