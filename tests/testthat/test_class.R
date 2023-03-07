
test_that("Creation of 'fpplateus_data_frame's works.", {
    x <- fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8))
    expect_s3_class(x, "fpplateaus_data_frame")

    expect_error(is_demog_change_component_df(x), NA)

    y <- fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8),
                               differences = double(),
                               change_condition_as_proportion = double(),
                               filter_width = double(),
                               denominator_count_filename = character(),
                               stall_probability_thresholds = double(),
                               CP_range_condition_min = double(),
                               CP_range_condition_max = double(),
                               MDMM_range_condition_min = double(),
                               MDMM_range_condition_max = double(),
                               min_stall_length = double())
    expect_s3_class(y, "fpplateaus_data_frame")

    z <- fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8),
                               differences = 1,
                               change_condition_as_proportion = 1L,
                               filter_width = 1,
                               denominator_count_filename = "a",
                               stall_probability_thresholds = 1,
                               CP_range_condition_min = 1,
                               CP_range_condition_max = 1,
                               MDMM_range_condition_min = 1L,
                               MDMM_range_condition_max = 1,
                               min_stall_length = 1)
    expect_s3_class(z, "fpplateaus_data_frame")

    ## Only some attributes set
    w <- fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8),
                               differences = 1,
                               change_condition_as_proportion = 1)
    expect_s3_class(z, "fpplateaus_data_frame")

    ## Make sure existing attribute values retained
    u <- structure(data.frame(A = 1:4, B = 5:8),
                   differences = as.double(999),
                   denominator_count_filename = "TEST")
    expect_identical(attr(u, "differences"), 999)
    expect_identical(attr(u, "denominator_count_filename"), "TEST")
    u <- fpplateaus_data_frame(u)
    expect_identical(attr(u, "differences"), 999)
    expect_identical(attr(u, "denominator_count_filename"), "TEST")
})


test_that("Validation of 'fpplateus_data_frame's produces expected errors.", {
    expect_error(fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8),
                               differences = "a",
                               change_condition_as_proportion = 1L,
                               filter_width = 1,
                               denominator_count_filename = "a",
                               stall_probability_thresholds = 1,
                               CP_range_condition_min = 1,
                               CP_range_condition_max = 1,
                               MDMM_range_condition_min = 1,
                               MDMM_range_condition_max = 1,
                               min_stall_length = 1),
                 "'differences' is not 'numeric'")

    expect_error(fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8),
                               differences = 1,
                               change_condition_as_proportion = 1,
                               filter_width = 1,
                               denominator_count_filename = 1,
                               stall_probability_thresholds = 1L,
                               CP_range_condition_min = 1,
                               CP_range_condition_max = 1,
                               MDMM_range_condition_min = 1,
                               MDMM_range_condition_max = 1,
                               min_stall_length = 1),
                 "'denominator_count_filename' is not 'character'")
})


test_that("Subsetting of 'fpplateus_data_frame's works.", {
    x <- fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8))

    y <- x[1:3,]
    expect_s3_class(y, "fpplateaus_data_frame")
    expect_error(FPPlateaus:::validate_fpplateaus_data_frame(y), NA)

    y <- x[,1:2]
    expect_s3_class(y, "fpplateaus_data_frame")
    expect_error(FPPlateaus:::validate_fpplateaus_data_frame(y), NA)

    y <- x[1, ]
    expect_s3_class(y, "fpplateaus_data_frame")
    expect_error(FPPlateaus:::validate_fpplateaus_data_frame(y), NA)

    ## This time, should only get a vector
    y <- x[, 1]
    expect_s3_class(y, NA)
})


test_that("Subset-replacing in `fpplateus_data_frame`s works.", {
    x <- fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8))
    x[, 2] <- 9:12
    expect_s3_class(x, "fpplateaus_data_frame")
    expect_error(FPPlateaus:::validate_fpplateaus_data_frame(x), NA)

    y <- fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8))
    y[2, ] <- 20:21
    expect_s3_class(y, "fpplateaus_data_frame")
    expect_error(FPPlateaus:::validate_fpplateaus_data_frame(y), NA)
})


test_that("Method for `base::subset` works.", {
    x <- fpplateaus_data_frame(data.frame(A = c(0,1,1,4,4,4), B = 1:6, C = 7:12))

    y <- subset(x, A == 1)
    expect_s3_class(y, "fpplateaus_data_frame")
    expect_error(FPPlateaus:::validate_fpplateaus_data_frame(y), NA)

    y <- subset(x, select = c("A", "B"))
    expect_s3_class(y, "fpplateaus_data_frame")
    expect_error(FPPlateaus:::validate_fpplateaus_data_frame(y), NA)

    y <- subset(x, A == 1, select = c("A", "B"))
    expect_s3_class(y, "fpplateaus_data_frame")
    expect_error(FPPlateaus:::validate_fpplateaus_data_frame(y), NA)

    ## Subset will return a data frame by default
    y <- subset(x, A == 0)
    expect_s3_class(y, "fpplateaus_data_frame")
    expect_error(FPPlateaus:::validate_fpplateaus_data_frame(y), NA)

    ## This time, should only get a vector
    y <- subset(x, A == 0, drop = TRUE)
    expect_s3_class(y, NA)
})


test_that("`as.data.frame` method for `fpplateaus_data_frame`s works.", {
    x <- fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8))
    x <- as.data.frame(x)
    expect_false(is_fpplateaus_data_frame(x))
    for (n in FPPlateaus:::get_fpplateaus_attr_names()) {
        expect_null(attr(x, n))
    }
})
