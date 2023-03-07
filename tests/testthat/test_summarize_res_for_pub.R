
test_that("`get_fp_plateau_countries()` works.", {
    data(sample_output_wra_all_res_df)
    x <- get_fp_plateau_countries(sample_output_wra_all_res_df, stall_probability = 0.8,
                                             indicator = "Modern")
    expect_s3_class(x, "data.frame")

    expect_error(get_fp_plateau_countries(sample_output_wra_all_res_df,
                                          stall_probability = 0.8),
                 'argument "indicator" is missing')

    expect_error(get_fp_plateau_countries(sample_output_wra_all_res_df,
                                          indicator = "Modern"),
                 'argument "stall_probability" is missing')
})
