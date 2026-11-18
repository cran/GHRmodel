test_that(".rank_gof errors when mod_gof is not a data.frame", {
  expect_error(
    .rank_gof(list(model_id = "a", waic = 1), metric = "waic"),
    "must be a data.frame"
  )
  expect_error(
    .rank_gof(NULL, metric = "waic"),
    "must be a data.frame"
  )
})

test_that(".rank_gof errors when a metric is not a column of mod_gof", {
  gof <- data.frame(model_id = c("a", "b"), waic = c(2, 1))
  expect_error(
    .rank_gof(gof, metric = "dic"),
    "must be present as columns"
  )
  # One valid and one invalid metric still errors
  expect_error(
    .rank_gof(gof, metric = c("waic", "dic")),
    "must be present as columns"
  )
})

test_that(".rank_gof errors when metric is not a non-empty character vector", {
  gof <- data.frame(model_id = c("a", "b"), waic = c(2, 1))
  expect_error(.rank_gof(gof, metric = character(0)), "non-empty character")
  expect_error(.rank_gof(gof), "argument \"metric\" is missing")
  expect_error(.rank_gof(gof, metric = NULL), "non-empty character")
  expect_error(.rank_gof(gof, metric = 2), "non-empty character")
  expect_error(.rank_gof(gof, metric = NA_character_), "non-empty character")
})

test_that(".rank_gof errors when a metric column is not numeric", {
  gof <- data.frame(
    model_id = c("a", "b"),
    waic = c(2, 1),
    label = c("x", "y")
  )
  expect_error(
    .rank_gof(gof, metric = c("waic", "label")),
    "must be numeric columns.*label"
  )
})

test_that(".rank_gof ignores duplicated metrics", {
  gof <- data.frame(model_id = c("a", "b"), waic = c(2, 1))
  res <- .rank_gof(gof, metric = c("waic", "waic"))
  expect_named(
    res,
    c("model_id", "waic", "rank_waic", "rank_mean", "final_rank")
  )
  expect_equal(res$final_rank, c(2, 1))
})

test_that(".rank_gof ranks a single metric in ascending order", {
  gof <- data.frame(
    model_id = c("m1", "m2", "m3"),
    waic = c(30, 10, 20)
  )
  res <- .rank_gof(gof, metric = "waic")

  expect_s3_class(res, "data.frame")
  expect_named(
    res,
    c("model_id", "waic", "rank_waic", "rank_mean", "final_rank")
  )
  expect_equal(res$rank_waic, c(3, 1, 2))
  expect_equal(res$rank_mean, c(3, 1, 2))
  expect_equal(res$final_rank, c(3, 1, 2))
})

test_that(".rank_gof computes per-metric ranks and mean rank across metrics", {
  gof <- data.frame(
    model_id = c("m1", "m2", "m3", "m4"),
    waic = c(1, 2, 3, 4),
    lms = c(2, 1, 4, 3)
  )
  res <- .rank_gof(gof, metric = c("waic", "lms"))

  expect_equal(res$rank_waic, c(1, 2, 3, 4))
  expect_equal(res$rank_lms, c(2, 1, 4, 3))
  expect_equal(res$rank_mean, c(1.5, 1.5, 3.5, 3.5))
  # Ties in rank_mean are broken by order of appearance (ties.method = "first")
  # in the (internally sorted) data; final ranks must be a permutation of 1:n
  expect_setequal(res$final_rank, 1:4)
  expect_true(all(res$final_rank[res$rank_mean == 1.5] %in% 1:2))
  expect_true(all(res$final_rank[res$rank_mean == 3.5] %in% 3:4))
})

test_that(".rank_gof gives tied metric values the same (average) rank", {
  gof <- data.frame(
    model_id = c("a", "b", "c", "d"),
    x = c(1, 1, 2, 3),
    y = c(5, 4, 3, 3)
  )
  res <- .rank_gof(gof, metric = c("x", "y"))
  expect_equal(res$rank_x, c(1.5, 1.5, 3, 4))
  expect_equal(res$rank_y, c(4, 3, 1.5, 1.5))
  expect_equal(res$rank_mean, c(2.75, 2.25, 2.25, 2.75))
  expect_equal(res$final_rank, c(3, 1, 2, 4))
})

test_that(".rank_gof results do not depend on the order of metric", {
  gof <- data.frame(
    model_id = c("a", "b", "c", "d"),
    x = c(1, 1, 2, 3),
    y = c(5, 4, 3, 3)
  )
  r1 <- .rank_gof(gof, metric = c("x", "y"))
  r2 <- .rank_gof(gof, metric = c("y", "x"))
  expect_equal(r1$rank_x, r2$rank_x)
  expect_equal(r1$rank_y, r2$rank_y)
  expect_equal(r1$rank_mean, r2$rank_mean)
  expect_equal(r1$final_rank, r2$final_rank)
})

test_that(".rank_gof rounds rank_mean to two decimals", {
  gof <- data.frame(
    model_id = c("m1", "m2", "m3"),
    a = c(1, 2, 3),
    b = c(1, 3, 2),
    c = c(2, 1, 3)
  )
  res <- .rank_gof(gof, metric = c("a", "b", "c"))

  # Raw means: 4/3, 2, 8/3
  expect_equal(res$rank_mean, c(1.33, 2, 2.67))
  expect_equal(res$final_rank, c(1, 2, 3))
})

test_that(".rank_gof final_rank orders models by rank_mean", {
  gof <- data.frame(
    model_id = c("m1", "m2", "m3", "m4", "m5"),
    waic = c(5, 3, 4, 1, 2),
    lms = c(4, 5, 3, 1, 2)
  )
  res <- .rank_gof(gof, metric = c("waic", "lms"))

  expect_equal(order(res$final_rank), order(res$rank_mean))
  expect_equal(sort(res$final_rank), 1:5)
  expect_equal(res$model_id[res$final_rank == 1], "m4")
})

test_that(".rank_gof preserves the original row order", {
  gof <- data.frame(
    model_id = c("z", "a", "m", "b"),
    waic = c(40, 10, 30, 20)
  )
  res <- .rank_gof(gof, metric = "waic")

  expect_identical(res$model_id, gof$model_id)
  expect_equal(res$waic, gof$waic)
  expect_equal(res$final_rank, c(4, 1, 3, 2))
})

test_that(".rank_gof keeps extra columns and drops helper column", {
  gof <- data.frame(
    model_id = c("a", "b", "c"),
    waic = c(3, 1, 2),
    dic = c(9, 8, 7),
    note = c("x", "y", "z")
  )
  res <- .rank_gof(gof, metric = "waic")

  expect_true(all(c("dic", "note") %in% names(res)))
  expect_false(".orig_order" %in% names(res))
  # Metrics not requested are not ranked
  expect_false("rank_dic" %in% names(res))
  expect_equal(res$note, gof$note)
})

test_that(".rank_gof does not modify its input", {
  gof <- data.frame(model_id = c("a", "b"), waic = c(2, 1))
  gof_copy <- gof
  invisible(.rank_gof(gof, metric = "waic"))
  expect_identical(gof, gof_copy)
})

test_that(".rank_gof ranks missing metric values last", {
  gof <- data.frame(model_id = c("a", "b", "c"), waic = c(1, NA, 2))
  res <- .rank_gof(gof, metric = "waic")
  expect_equal(res$rank_waic, c(1, 3, 2))
})

test_that(".rank_gof works with a single model", {
  gof <- data.frame(model_id = "only", waic = 5, lms = 3)
  res <- .rank_gof(gof, metric = c("waic", "lms"))
  expect_equal(res$rank_waic, 1)
  expect_equal(res$rank_lms, 1)
  expect_equal(res$rank_mean, 1)
  expect_equal(res$final_rank, 1)
})

test_that(".rank_gof works on GOF table from example GHRmodels object", {
  f <- system.file("examples", "model_list.rds", package = "GHRmodel")
  skip_if(f == "", "example model_list.rds not available")
  model_list <- readRDS(f)
  res <- .rank_gof(model_list$mod_gof, metric = c("waic", "lms"))

  expect_equal(nrow(res), nrow(model_list$mod_gof))
  expect_identical(res$model_id, model_list$mod_gof$model_id)
  expect_setequal(res$final_rank, seq_len(nrow(res)))
})
