# Test graph_utils
data("adae")
data("adsl")
data("ae_pre_process")

ae_pre <- mentry(
  datain = ae_pre_process$data,
  subset = NA,
  byvar = "AEBODSYS",
  trtvar = "TRTA",
  trtsort = "TRTAN",
  subgrpvar = "SEX",
  trttotalyn = "N",
  add_grpmiss = "N",
  sgtotalyn = "N",
  pop_fil = "SAFFL"
)
## reverselog_trans testing
test_that("Case 1: Transformation works with expected input", {
  # Labels for adae data:
  trans2 <- reverselog_trans(2)
  expect_equal(trans2$name, "reverselog-2")
})

#############################################################################
## g_seriescol testing ##

test_that("Case 1: Works with expected input", {
  trt_cols <- g_seriescol(ae_pre, "red~cyan~forestgreen~black~pink~green", "TRTVAR")
  
  # Correct number of levels and colors assigned
  expect_equal(unname(trt_cols), c("red", "cyan", "forestgreen"))
  # Names as expected
  expect_named(
    trt_cols,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  )
})

test_that("Case 2: Works with default", {
  trt_na <- g_seriescol(ae_pre, NA, "TRTVAR")
  # Default colors:
  
  expect_equal(
    unname(trt_na),
    c("firebrick2", "blue4", "forestgreen")
  )
  expect_named(
    trt_na,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  )
})

test_that("Case 3: Works with character column", {
  trt_ch <- g_seriescol(ae_pre, NA, "SEX")
  # Colors:
  expect_equal(
    unname(trt_ch),
    c("firebrick2", "blue4")
  )
  expect_named(
    trt_ch,
    c("F", "M")
  )
})

#############################################################################
## g_seriessym ##

test_that("Case 1: Works with expected input", {
  trt_shp <- g_seriessym(
    ae_pre,
    "triangle~circle~square~asterisk", "TRTVAR"
  )
  
  # Correct number of levels and colors assigned
  expect_equal(unname(trt_shp), c(2, 1, 0))
  # Names as expected
  expect_named(
    trt_shp,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  )
  
  # Numeric Input:
  trt_shp1 <- g_seriessym(
    ae_pre,
    c(1, 21, 23, 3), "TRTVAR"
  )
  expect_equal(unname(trt_shp1), c(1, 21, 23))
  expect_named(
    trt_shp1,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  )
})

test_that("Case 2: Works with default", {
  trt_nashp <- g_seriessym(ae_pre, NA, "TRTVAR")
  # Default colors:
  
  expect_equal(
    unname(trt_nashp),
    c(16, 17, 15)
  )
  expect_named(
    trt_nashp,
    c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  )
})

test_that("Case 3: Works with character column", {
  trt_chshp <- g_seriessym(ae_pre, NA, "SEX")
  # Colors:
  expect_equal(
    unname(trt_chshp),
    c(16, 17)
  )
  expect_named(
    trt_chshp,
    c("F", "M")
  )
})

#############################################################################
# plot_display_bign

# Test Data
# data without treatment
adsl_entry1 <- mentry(
  datain = adsl,
  subset = "EFFFL=='Y'",
  byvar = NA,
  trtvar = NA,
  trtsort = NA,
  subgrpvar = NA,
  trttotalyn = "N",
  add_grpmiss = "N",
  sgtotalyn = "N",
  pop_fil = "Overall Population"
)
datain1 <- mcatstat(
  datain = adsl_entry1,
  dptvar = "AGEGR1"
)
adsl_entry2 <- mentry(
  datain = adsl,
  subset = "EFFFL=='Y'",
  byvar = NA,
  trtvar = "TRT01A",
  trtsort = "TRT01AN",
  subgrpvar = "SEX",
  trttotalyn = "N",
  add_grpmiss = "N",
  sgtotalyn = "N",
  pop_fil = "Overall Population"
)
datain2 <- msumstat(
  datain = adsl_entry2,
  dptvar = "AGE",
  statvar = "box"
)$gsum
# Test Cases

test_that("Test 1:Check for count and values of the output", {
  actual <- datain2 |>
    plot_display_bign(adsl_entry2, bignyn = "Y")
  expect_true(is.data.frame(actual))
  expected <- datain2 |>
    add_bigN(adsl_entry2, grpvar = c("TRTVAR", "SUBGRPVAR1"), modvar = "TRTVAR") |>
    rename("TRTTXT" = "TRTVAR_BIGN")
  expect_equal(actual, expected)
  # Testing with case "N" no bign
  actual1 <- datain2 |>
    plot_display_bign(adsl_entry2, bignyn = "N")
  expected1 <- datain2 |>
    mutate(TRTTXT = .data[["TRTVAR"]])
  expect_equal(actual1, expected1)
})

test_that("Test 2: Check bign without TRTVAR", {
  actual <- plot_display_bign(datain1, adsl_entry1, bignyn = "Y")
  expect_true(is.data.frame(actual))
  expect_equal(unique(actual$TRTVAR), "Total")
  expect_equal(
    unique(actual$TRTTXT),
    paste0("Total (N=", length(unique(adsl_entry1[["USUBJID"]])), ")")
  )
})

test_that("Test 1: test plot_aes_opts works", {
  actual <- plot_aes_opts(
    datain = adsl_entry2,
    series_var = "TRTVAR",
    series_color = NA,
    series_shape = NA,
    series_size = rep(1, 3),
    series_contrast = rep("black", 3)
  )
  expect_type(actual, "list")
  expect_equal(actual$color, g_seriescol(adsl_entry2, NA, "TRTVAR"))
  expect_equal(actual$shape, g_seriessym(adsl_entry2, NA, "TRTVAR"))
  expect_equal(actual$size, g_seriessym(adsl_entry2, rep(1, 3), "TRTVAR"))
  expect_equal(actual$contrast, g_seriescol(adsl_entry2, rep("black", 3), "TRTVAR"))
})

test_that("Test 2: Check plot_aes_opts without series var", {
  actual <- plot_aes_opts(
    datain = adsl_entry2,
    series_var = "NOVAR",
    series_color = "red~green",
    series_shape = "square~circle",
    series_size = rep(1, 3),
    series_contrast = rep("black", 3)
  )
  expect_type(actual, "list")
  expect_equal(actual, list(color = "red", shape = 0, size = 1, contrast = "black"))
})
#############################################################################
# plot_axis_opts

test_that("Test 1: Expected Inputs", {
  actual <- plot_axis_opts()
  expect_type(actual, "list")
  expected <- list(
    Ybrks = waiver(),
    Ylims = NULL,
    Yticks = waiver(),
    Xbrks = waiver(),
    Xlims = NULL,
    Xticks = waiver(),
    xsize = 12,
    xface = "plain",
    ysize = 12,
    yface = "plain",
    ytsize = 8,
    ytface = "plain",
    xtsize = 8,
    xtface = "plain",
    xtangle = 0,
    ytangle = 0,
    xaxis_scale = "identity",
    yaxis_scale = "identity",
    xaxis_label = "",
    yaxis_label = ""
  )
  expect_equal(actual, expected)
  actual1 <- plot_axis_opts(xlinearopts = list(limits = c(0, 100)))
  expected1 <- expected
  expected1[["Xlims"]] <- c(0, 100)
  expect_equal(actual1, expected1)
})

test_that("empty_plot works as expected", {
  actual <- empty_plot()
  static_label <- "No data available for these values"
  exp_ptly_obj <- actual$ptly$x$data
  
  expect_length(actual, 2)
  expect_length(actual$plot$data, 0)
  expect_equal(actual[["plot"]][["layers"]][[1]][["aes_params"]][["label"]], static_label)
  expect_snapshot(exp_ptly_obj)
})

test_that("theme_cleany works as expected", {
  actual <- theme_cleany(legend_opts = list(pos = "bottom", dir = "horizontal"))
  expect_true(all(class(actual) %in% c("theme", "gg")))
  expect_snapshot(actual)
})

test_that("theme_std works as expected", {
  actual <- theme_std()
  expect_true(all(class(actual) %in% c("theme", "gg")))
  expect_length(actual, 10)
  expect_equal(actual$legend.position, "bottom")
  expect_equal(actual$plot.title$hjust, 0.5)
  actual2 <- theme_std(griddisplay = "Y")
  expect_length(actual2, 12)
  expect_equal(actual2$panel.grid.major.x$colour, "grey")
  expect_equal(actual2$panel.grid.major.y$linewidth, 0.1)
})

test_that("plot_title_nsubj works as expected", {
  plotdata <- ae_pre |>
    msumstat(
      dptvar = "AGE",
      statvar = "mean",
      sigdec = 2
    )
  actual1 <- plot_title_nsubj(
    ae_pre,
    plotdata$gsum,
    "SUBGRPVAR1"
  )
  expect_true(is.data.frame(actual1))
  expected1 <- ae_pre |>
    group_by(across(all_of("SUBGRPVAR1"))) |>
    summarise(splitN = n_distinct(.data[["USUBJID"]])) |>
    (\(.) left_join(plotdata[["gsum"]], ., by = "SUBGRPVAR1"))()
  expect_equal(actual1, expected1, ignore_attr = TRUE)
  # No subgroup case
  actual2 <- plot_title_nsubj(
    ae_pre,
    plotdata$gsum,
    character(0)
  )
  expected2 <- plotdata[["gsum"]] |>
    mutate(splitN = n_distinct(ae_pre[["USUBJID"]]))
  expect_equal(actual2, expected2, ignore_attr = TRUE)
})

test_that("tbl_to_plot works as expected", {
  fig <- ggplot2::mpg |>
    mutate(CYL = as.character(.data[["cyl"]])) |>
    group_by(.data[["CYL"]]) |>
    mutate(HWY = round(mean(.data[["hwy"]]))) |>
    tbl_to_plot(
      "CYL",
      "manufacturer",
      "HWY"
    )
  expect_true("ggplot" %in% class(fig))
  purrr::walk(c("mapping", "labels"), \(x) expect_snapshot(fig[[x]]))
})

test_that("series_leg_lab works properly", {
  iris1 <- iris |> mutate(SPNEW = fct_inorder(c(rep("A", 50), rep("B", 50), rep("C", 50))))
  expect_equal(series_leg_lab(iris1, "Species", "SPNEW"), as.factor(c("A", "B", "C")))
  expect_equal(series_leg_lab(iris1, "Species", "Species"), waiver())
})
