# Argument Check ----------------------------------------------------------
test_that("All arguments are set correctly", {
  expect_error(cs_distribution(claus_2020))
  expect_error(cs_distribution(claus_2020, id = id))
  expect_error(cs_distribution(claus_2020, id = id, time = time))
  expect_error(cs_distribution(claus_2020, id = id, time = time, reliability = "0.80"))
  expect_error(cs_distribution(claus_2020, id = id, time = time, reliability = 1.1))
  expect_error(cs_distribution(claus_2020, id = id, time = time, reliability = -0.8))

  expect_snapshot_error(cs_distribution(claus_2020))
  expect_snapshot_error(cs_distribution(claus_2020, id = id))
  expect_snapshot_error(cs_distribution(claus_2020, id = id, time = time))
  expect_snapshot_error(cs_distribution(claus_2020, id = id, time = time, outcome = hamd, reliability = "0.80"))
  expect_snapshot_error(cs_distribution(claus_2020, id = id, time = time, outcome = hamd, reliability = 1.1))
  expect_snapshot_error(cs_distribution(claus_2020, id = id, time = time, outcome = hamd, reliability = -0.8))
})



# Functionality -----------------------------------------------------------
test_that("The function runs smoothly", {
  expect_no_error(cs_distribution(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "JT"))
  expect_no_error(cs_distribution(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "EN"))
  expect_no_error(cs_distribution(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "HLL"))
  expect_no_error(cs_distribution(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "GLN"))
  expect_no_error(cs_distribution(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.80, reliability_post = 0.50, rci_method = "NK"))
  expect_no_error(cs_distribution(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "HA"))
  expect_no_error(cs_distribution(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.80, rci_method = "HLM"))
})
