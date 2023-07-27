# Argument Check ----------------------------------------------------------
test_that("All arguments are set correctly", {
  expect_error(cs_statistical(claus_2020))
  expect_error(cs_statistical(claus_2020, id = id))
  expect_error(cs_statistical(claus_2020, id = id, time = time))
  expect_error(cs_statistical(claus_2020, id = id, time = time, reliability = "0.80", cutoff_method = "HA"))
  expect_error(cs_statistical(claus_2020, id = id, time = time, reliability = 1.1, cutoff_method = "HA"))
  expect_error(cs_statistical(claus_2020, id = id, time = time, reliability = -0.8, cutoff_method = "HA"))

  expect_snapshot_error(cs_statistical(claus_2020))
  expect_snapshot_error(cs_statistical(claus_2020, id = id))
  expect_snapshot_error(cs_statistical(claus_2020, id = id, time = time))
  expect_snapshot_error(cs_statistical(claus_2020, id = id, time = time, reliability = "0.80", cutoff_method = "HA"))
  expect_snapshot_error(cs_statistical(claus_2020, id = id, time = time, reliability = 1.1, cutoff_method = "HA"))
  expect_snapshot_error(cs_statistical(claus_2020, id = id, time = time, reliability = -0.8, cutoff_method = "HA"))
})



# Functionality -----------------------------------------------------------
test_that("The function runs smoothly", {
  expect_no_error(cs_statistical(claus_2020, id, time, hamd, pre = 1, post = 4))
  expect_no_error(cs_statistical(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.80, cutoff_method = "HA"))
  expect_no_error(cs_statistical(anxiety, subject, measurement, anxiety, pre = 0, post = 4))
  expect_no_error(cs_statistical(anxiety, subject, measurement, anxiety, pre = 0, post = 4, group = treatment))
  expect_no_error(cs_statistical(anxiety, subject, measurement, anxiety, pre = 0, post = 4, group = treatment, reliability = 0.80, cutoff_method = "HA"))
})
