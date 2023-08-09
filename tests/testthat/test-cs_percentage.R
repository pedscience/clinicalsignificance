# Argument Check ----------------------------------------------------------
test_that("All arguments are set correctly", {
  expect_error(cs_percentage(claus_2020))
  expect_error(cs_percentage(claus_2020, id = id))
  expect_error(cs_percentage(claus_2020, id = id, time = time))
  expect_error(cs_percentage(claus_2020, id = id, time = time, outcome = hamd, pre = 1, post = 4, pct_improvement = "0.5"))
  expect_error(cs_percentage(claus_2020, id = id, time = time, outcome = hamd, pre = 1, post = 4, pct_improvement = 1.1))
  expect_error(cs_percentage(claus_2020, id = id, time = time, outcome = hamd, pre = 1, post = 4, pct_improvement = -0.5))

  expect_error(cs_percentage(claus_2020))
  expect_error(cs_percentage(claus_2020, id = id))
  expect_error(cs_percentage(claus_2020, id = id, time = time))
  expect_error(cs_percentage(claus_2020, id = id, time = time, outcome = hamd, pre = 1, post = 4, pct_improvement = "0.5"))
  expect_error(cs_percentage(claus_2020, id = id, time = time, outcome = hamd, pre = 1, post = 4, pct_improvement = 1.1))
  expect_error(cs_percentage(claus_2020, id = id, time = time, outcome = hamd, pre = 1, post = 4, pct_improvement = -0.5))
})



# Functionality -----------------------------------------------------------
test_that("The function runs smoothly", {
  expect_no_error(cs_percentage(claus_2020, id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5))
  expect_no_error(cs_percentage(claus_2020, id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5, pct_deterioration = 0.3))
})


test_that("Results are correct", {
  expect_snapshot(cs_percentage(claus_2020, id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5))
  expect_snapshot(cs_percentage(claus_2020, id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5, pct_deterioration = 0.3))
})
