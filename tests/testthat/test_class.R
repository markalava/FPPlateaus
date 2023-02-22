
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
                               change_condition_as_proportion = 1,
                               filter_width = 1,
                               denominator_count_filename = "a",
                               stall_probability_thresholds = 1,
                               CP_range_condition_min = 1,
                               CP_range_condition_max = 1,
                               MDMM_range_condition_min = 1,
                               MDMM_range_condition_max = 1,
                               min_stall_length = 1)
    expect_s3_class(z, "fpplateaus_data_frame")
})


test_that("Validation of 'fpplateus_data_frame's works.", {
    expect_error(fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8),
                               differences = "a",
                               change_condition_as_proportion = 1,
                               filter_width = 1,
                               denominator_count_filename = "a",
                               stall_probability_thresholds = 1,
                               CP_range_condition_min = 1,
                               CP_range_condition_max = 1,
                               MDMM_range_condition_min = 1,
                               MDMM_range_condition_max = 1,
                               min_stall_length = 1),
                 "differences is not 'numeric'")

    expect_error(fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8),
                               differences = 1,
                               change_condition_as_proportion = 1,
                               filter_width = 1,
                               denominator_count_filename = 1,
                               stall_probability_thresholds = 1,
                               CP_range_condition_min = 1,
                               CP_range_condition_max = 1,
                               MDMM_range_condition_min = 1,
                               MDMM_range_condition_max = 1,
                               min_stall_length = 1),
                 "denominator_count_filename is not 'character'")
})


test_that("Subsetting of 'fpplateus_data_frame's works.", {
    x <- fpplateaus_data_frame(data.frame(A = 1:4, B = 5:8))
    expect_s3_class(x[1:3,], "fpplateaus_data_frame")
    expect_s3_class(x[,1:2], "fpplateaus_data_frame")
})
