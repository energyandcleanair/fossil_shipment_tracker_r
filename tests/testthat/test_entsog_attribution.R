library(testthat)

# The tests in this file don't use real data, they are simpler and easier to
# understand examples. The scenarios involved may not be real but are used
# to test the logic, assumptions, and edge cases that may not have been
# considered.

expect_flows_close <- function(actual, expected) {
  # If the rows are empty, then the dataframes are equal and we should succeed
  # the test.
  if (nrow(actual) == 0 && nrow(expected) == 0) {
    succeed()
    return()
  }

  # We round to make sure values are close and we don't care about the order of the rows.
  actual_clean <- actual %>%
    arrange(from_country, to_country) %>%
    mutate(value = round(value, 3)) %>%
    filter(value != 0)
  exected_clean <- expected %>%
    arrange(from_country, to_country) %>%
    mutate(value = round(value, 3)) %>%
    filter(value != 0)

  expect_equal(actual_clean, exected_clean)
}

arbitrary_date <- as.Date("2020-01-01")
different_date <- as.Date("2020-01-02")

test_that("creates the expected flows for basic model", {
  # Graph of flows (flow from left to right):
  # GB.      .BE
  #   8\    /6
  #     :FR:
  #   4/    \3
  # DE`      `ES
  initial_flows <- tibble(
    from_country = c("GB", "FR", "DE", "FR"),
    to_country = c("FR", "ES", "FR", "BE"),
    value = c(8, 3, 4, 6),
    date = rep(arbitrary_date, 4)
  )

  actual_flows <- process_iterative_for_day(initial_flows) %>%
    filter(value != 0) %>%
    select(from_country, to_country, value) %>%
    as_tibble()

  expected_flows <- tibble(
    from_country = c("GB", "DE", "GB", "GB", "DE", "DE"),
    to_country = c("FR", "FR", "ES", "BE", "ES", "BE"),
    value = c(2, 1, 2, 4, 1, 2),
  )

  expect_flows_close(actual_flows, expected_flows)
})

test_that("production doesn't affect flows and is removed", {
  # Graph of flows:
  # GB.  1   .BE
  #   8\ /\ /6
  #     :FR:
  #   4/    \3
  # DE`      `ES
  initial_flows <- tibble(
    from_country = c("GB", "FR", "DE", "FR", "FR"),
    to_country = c("FR", "ES", "FR", "BE", "FR"),
    value = c(8, 3, 4, 6, 1),
    date = rep(arbitrary_date, 5)
  )

  actual_flows <- process_iterative_for_day(initial_flows) %>%
    filter(value != 0) %>%
    select(from_country, to_country, value) %>%
    as_tibble()

  expected_flows <- tibble(
    from_country = c("GB", "DE", "GB", "GB", "DE", "DE"),
    to_country = c("FR", "FR", "ES", "BE", "ES", "BE"),
    value = c(2, 1, 2, 4, 1, 2),
  )

  expect_flows_close(actual_flows, expected_flows)
})

test_that("Ukraine doesn't import from Russia and transits through", {
  # Graph of flows:
  #      6        3
  # RU ----> UA ----> SK
  #            \----> PL
  #               2

  initial_flows <- tibble(
    from_country = c("RU", "UA", "UA"),
    to_country = c("UA", "SK", "PL"),
    value = c(3, 3, 2),
    date = rep(arbitrary_date, 3)
  )

  actual_flows <- process_iterative_for_day(initial_flows) %>%
    filter(value != 0) %>%
    select(from_country, to_country, value) %>%
    as_tibble()

  expected_flows <- tibble(
    from_country = c("RU", "RU"),
    to_country = c("SK", "PL"),
    value = c(3, 2),
  )

  expect_flows_close(actual_flows, expected_flows)
})

test_that("Ukraine can't reimport from Russia", {
  # Graph of flows:
  #      6        3
  # RU ----> PL ----> UA

  initial_flows <- tibble(
    from_country = c("RU", "PL"),
    to_country = c("PL", "UA"),
    value = c(6, 3),
    date = rep(arbitrary_date, 2)
  )

  actual_flows <- process_iterative_for_day(initial_flows) %>%
    filter(value != 0) %>%
    select(from_country, to_country, value) %>%
    as_tibble()

  expected_flows <- tibble(
    from_country = c("RU"),
    to_country = c("PL"),
    value = c(3),
  )

  expect_flows_close(actual_flows, expected_flows)
})

test_that("a loop takes from where gas is 'made' (made is not the same as production)", {
  # Graph of flows:
  #      3         1
  #  PL ----> SK ----> DE
  #   \<---------------/
  #           2
  initial_flows_different_orders <- list(
    tibble(
      from_country = c("PL", "SK", "DE"),
      to_country = c("SK", "DE", "PL"),
      value = c(3, 1, 2),
      date = rep(arbitrary_date, 3)
    ),
    tibble(
      from_country = c("SK", "DE", "PL"),
      to_country = c("DE", "PL", "SK"),
      value = c(1, 2, 3),
      date = rep(arbitrary_date, 3)
    ),
    tibble(
      from_country = c("DE", "PL", "SK"),
      to_country = c("PL", "SK", "DE"),
      value = c(2, 3, 1),
      date = rep(arbitrary_date, 3)
    )
  )

  for (initial_flows in initial_flows_different_orders) {
    actual_flows <- process_iterative_for_day(initial_flows) %>%
      filter(value != 0) %>%
      select(from_country, to_country, value) %>%
      as_tibble()

    # DE has to "make" 1 to send 2 to PL (which sends it onto SK)
    # PL has to "make" 1 to send 3 to SK
    # So, PL->SK is 1 and DE->SK is 1
    expected_flows <- tibble(
      from_country = c("PL", "DE"),
      to_country = c("SK", "SK"),
      value = c(1, 1),
    )

    expect_flows_close(actual_flows, expected_flows)
  }
})

test_that("a loop with nothing 'made' transmits nothing)", {
  # Graph of flows:
  #      3         3
  #  PL ----> SK ----> DE
  #   \<---------------/
  #           3
  initial_flows <- tibble(
    from_country = c("PL", "SK", "DE"),
    to_country = c("SK", "DE", "PL"),
    value = c(3, 3, 3),
    date = rep(arbitrary_date, 3)
  )

  actual_flows <- process_iterative_for_day(initial_flows) %>%
    filter(value != 0) %>%
    select(from_country, to_country, value) %>%
    as_tibble()

  # DE has to "make" 1 to send 2 to PL (which sends it onto SK)
  # PL has to "make" 1 to send 3 to SK
  # So, PL->SK is 1 and DE->SK is 1
  expected_flows <- tibble(
    from_country = c(),
    to_country = c(),
    value = c(),
  )

  expect_flows_close(actual_flows, expected_flows)
})

test_that("we bypass Bulgaria to Romania, Serbia, North Macedonia; use other sources in Bulgaria", {
  # Graph of flows:
  # GR ->3->\  /->1-> RO
  # RU ->3-> BG ->1-> RS
  #            \->1-> MK
  initial_flows <- tibble(
    from_country = c("GR", "RU", "BG", "BG", "BG"),
    to_country = c("BG", "BG", "RO", "RS", "MK"),
    value = c(3, 3, 1, 1, 1),
    date = rep(arbitrary_date, 5)
  )

  actual_flows <- process_iterative_for_day(initial_flows) %>%
    filter(value != 0) %>%
    select(from_country, to_country, value) %>%
    as_tibble()

  expected_flows <- tibble(
    from_country = c("GR", "RU", "RU", "RU"),
    to_country = c("BG", "RO", "RS", "MK"),
    value = c(3, 1, 1, 1),
  )

  expect_flows_close(actual_flows, expected_flows)
})


test_that("we bypass Bulgaria to Romania, Serbia, North Macedonia; leave extra in Bulgaria", {
  # Graph of flows:
  #            /->1-> RO
  # RU ->6-> BG ->1-> RS
  #            \->1-> MK
  initial_flows <- tibble(
    from_country = c("RU", "BG", "BG", "BG"),
    to_country = c("BG", "RO", "RS", "MK"),
    value = c(6, 1, 1, 1),
    date = rep(arbitrary_date, 4)
  )

  actual_flows <- process_iterative_for_day(initial_flows) %>%
    filter(value != 0) %>%
    select(from_country, to_country, value) %>%
    as_tibble()

  expected_flows <- tibble(
    from_country = c("RU", "RU", "RU", "RU"),
    to_country = c("BG", "RO", "RS", "MK"),
    value = c(3, 1, 1, 1),
  )

  expect_flows_close(actual_flows, expected_flows)
})

test_that("we bypass Bulgaria to Romania, Serbia, North Macedonia; Bulgaria creates extra", {
  # Graph of flows:
  #            /->2-> RO
  # RU ->3-> BG ->2-> RS
  #            \->2-> MK
  initial_flows <- tibble(
    from_country = c("RU", "BG", "BG", "BG"),
    to_country = c("BG", "RO", "RS", "MK"),
    value = c(3, 2, 2, 2),
    date = rep(arbitrary_date, 4)
  )

  actual_flows <- process_iterative_for_day(initial_flows) %>%
    filter(value != 0) %>%
    select(from_country, to_country, value) %>%
    as_tibble()

  expected_flows <- tibble(
    from_country = c("RU", "RU", "RU", "BG", "BG", "BG"),
    to_country = c("RO", "RS", "MK", "RO", "RS", "MK"),
    value = c(1, 1, 1, 1, 1, 1),
  )

  expect_flows_close(actual_flows, expected_flows)
})


test_that("multiple dates causes error", {
  initial_flows <- tibble(
    from_country = c("RU", "BG", "BG", "BG"),
    to_country = c("BG", "RO", "RS", "MK"),
    value = c(6, 1, 1, 1),
    date = c(rep(arbitrary_date, 3), different_date)
  )

  expect_error(process_iterative_for_day(initial_flows))
})
