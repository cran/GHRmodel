data("map_MS", package = "GHRmodel")

model_f <- readRDS(testthat::test_path("testdata", "ghr_model_config_f.rds"))
model_t <- readRDS(testthat::test_path("testdata", "ghr_model_config_t.rds"))

mod_label_named <- c("mod1" = "Model A", "mod2" = "Model B", "mod3" = "Model C")

# --------- Spatial no replication ---------
test_that("plot_re: spatial no map, 1 model (config FALSE)", {
  p <- plot_re(models = model_f, mod_id = "mod1", re_id = "spat_id")
  expect_s3_class(p, "ggplot")
})

test_that("plot_re: spatial no map,  2 models (config FALSE)", {
  p <- plot_re(models = model_f, mod_id = c("mod1", "mod2"), re_id = "spat_id")
  expect_s3_class(p, "ggplot")
})

test_that("plot_re: spatial no map,  3 models (config FALSE)", {
  p <- plot_re(models = model_f, mod_id = c("mod1", "mod2", "mod3"),
               re_id = "spat_id", mod_label = mod_label_named)
  expect_s3_class(p, "ggplot")
})

test_that("plot_re: Spatial, no rep, 1 model (config TRUE)", {
  p <- plot_re(models = model_t, mod_id = "mod1", re_id = "spat_id")
  expect_s3_class(p, "ggplot")
})

test_that("plot_re: Spatial, no rep, map, 2 models (config FALSE)", {
  p <- plot_re(models = model_f, mod_id = c("mod1", "mod2"), re_id = "spat_id",
               map = map_MS, map_area = "code")
  expect_s3_class(p, "ggplot")
})

test_that("plot_re: Spatial,map, no rep, 1 model (config TRUE)", {
  p <- plot_re(models = model_t, mod_id = "mod1", re_id = "spat_id",
               map = map_MS, map_area = "code")
  expect_s3_class(p, "ggplot")
})

test_that("plot_re: Spatial, map, no rep, 1 model (config TRUE)", {
  p <- plot_re(models = model_t, mod_id = "mod1", re_id = "spat_id",
               map = map_MS, map_area = "code")
  expect_s3_class(p, "ggplot")
})


# --------- Temporal, with and without replication ---------
test_that("plot_re:temporal, rep, 1 model (config TRUE)", {
  p <- plot_re(models = model_t, mod_id = "mod8", re_id = "month_id", rep_id = "main_climate_f")
  expect_s3_class(p, "ggplot")
})

test_that("plot_re: temporal, rep, 2 models (config TRUE)", {
  p <- plot_re(models = model_t, mod_id = c("mod8", "mod9"),
               re_id = "month_id", rep_id = "main_climate_f",
               mod_label = c("mod8" = "Ref", "mod9" = "Alt"))
  expect_s3_class(p, "ggplot")
})


test_that("plot_re: temporal, no rep, 3 models (config FALSE)", {
  p <- plot_re(models = model_f, mod_id = c("mod1", "mod2", "mod3"), re_id = "year_id",
               mod_label = mod_label_named)
  expect_s3_class(p, "ggplot")
})



# --------- Error Tests for plot_re ----------
test_that("plot_re: missing re_id errors", {
  expect_error(
    plot_re(models = model_f, mod_id = "mod1"),
    "Both 're_id' and 'mod_id' must be provided"
  )
})

test_that("plot_re: missing mod_id errors", {
  expect_error(
    plot_re(models = model_f, re_id = "spat_id"),
    "Both 're_id' and 'mod_id' must be provided"
  )
})

test_that("plot_re: invalid mod_id errors", {
  expect_error(
    plot_re(models = model_f, mod_id = "not_a_model", re_id = "spat_id"),
    "Unknown mod_id"
  )
})

test_that("plot_re: invalid re_id errors", {
  expect_error(
    plot_re(models = model_f, mod_id = "mod1", re_id = "not_a_column"),
    "'re_id' must be a column name in models\\$data"
  )
})

test_that("plot_re: map provided without map_area errors", {
  expect_error(
    plot_re(models = model_f, mod_id = "mod1", re_id = "spat_id", map = map_MS),
    "If 'map' is provided, 'map_area' must be specified"
  )
})

test_that("plot_re: map_area not in map errors", {
  expect_error(
    plot_re(models = model_f, mod_id = "mod1", re_id = "spat_id",
            map = map_MS, map_area = "not_a_column"),
    "'map_area' is not a column in the provided map"
  )
})

test_that("plot_re: replicated effect but no rep_id errors", {
  expect_error(
    plot_re(models = model_t, mod_id = "mod8", re_id = "month_id"),
    "please provide 'rep_id'"
  )
})

test_that("plot_re: non-replicated effect but rep_id wrongly passed", {
  expect_error(
    plot_re(models = model_f, mod_id = "mod1", re_id = "spat_id", rep_id = "main_climate_f"),
    "rep_id' provided but random effect is not replicated"
  )
})

test_that("plot_re: re_label not in data errors", {
  expect_error(
    plot_re(models = model_f, mod_id = "mod1", re_id = "spat_id", re_label = "not_in_data"),
    "is not a column in 'models\\$data'"
  )
})

test_that("plot_re: rep_label not in data errors", {
  expect_error(
    plot_re(models = model_t, mod_id = "mod8", re_id = "year_id", rep_id = "main_climate_f",
            rep_label = "not_in_data"),
    "is not a column in 'models\\$data'"
  )
})

test_that("plot_re: map_area not in map errors", {
  expect_error(
    plot_re(models = model_f, mod_id = "mod1", re_id = "spat_id",
            map = map_MS, map_area = "micro_code"),
    "'map_area' is not a column in the provided map."
  )
})
