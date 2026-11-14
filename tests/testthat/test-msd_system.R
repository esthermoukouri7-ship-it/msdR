test_that("msd_system returns the correct matrix", {
  A <- msd_system(m = 1, k = 1, c = 1)
  expect_equal(A[1, 1], 0); expect_equal(A[1, 2], 1)
  expect_equal(A[2, 1], -1); expect_equal(A[2, 2], -1)
  expect_error(msd_system(m = 0, k = 1, c = 1))
})

test_that("msd_eigen gives the analytic eigenvalues", {
  info <- msd_eigen(m = 1, k = 1, c = 1)
  expect_equal(info$omega_n, 1)
  expect_equal(info$zeta, 0.5)
  expect_equal(info$omega_d, sqrt(3) / 2)
  expect_equal(info$regime, "underdamped/sous_amorti")
  expect_equal(sort(Re(info$eigenvalues)), c(-0.5, -0.5))
})

test_that("regimes are correctly classified", {
  expect_equal(msd_eigen(1, 1, 0)$regime,   "undamped/non_amorti")
  expect_equal(msd_eigen(1, 1, 0.5)$regime, "underdamped/sous_amorti")
  expect_equal(msd_eigen(1, 1, 2)$regime,   "critically_damped/amorti_critique")
  expect_equal(msd_eigen(1, 1, 5)$regime,   "overdamped/sur_amorti")
  expect_equal(msd_eigen(1, 1, -1)$regime,  "unstable/instable")
})

test_that("msd_stability flags stability", {
  expect_true(msd_stability(1, 1, 1)$stable)
  expect_false(msd_stability(1, 1, -1)$stable)
  # cas marginal : c = 0, k > 0 → pôles imaginaires purs
  s <- msd_stability(1, 1, 0)
  expect_false(s$stable)
  expect_true(s$marginally_stable)
})

test_that("msd_simulate returns the expected columns", {
  out <- msd_simulate(1, 1, 1, times = seq(0, 1, by = 0.1))
  expect_true(all(c("time", "x", "v") %in% names(out)))
  expect_equal(nrow(out), 11)
})
