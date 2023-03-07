
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


test_that("`make_main_results_df()` works.", {
    data(sample_output_mwra_all_res_df)
    x <- make_main_results_df(sample_output_mwra_all_res_df,
                              stall_probability = 0.5, indicator = "MetDemModMeth")
    expect_s3_class(x, "data.frame")

    expect_error(make_main_results_df(sample_output_mwra_all_res_df,
                                      stall_probability = 0.5),
                 'argument "indicator" is missing')

    expect_error(make_main_results_df(sample_output_mwra_all_res_df,
                                      indicator = "MetDemModMeth"),
                 'argument "stall_probability" is missing')
})


test_that("`make_main_results_table()` works.", {
    data(sample_output_mwra_all_res_df)

    x <- make_main_results_table(
        make_main_results_df(sample_output_mwra_all_res_df,
                             stall_probability = 0.5, indicator = "MetDemModMeth"))
    expect_s3_class(x, "tbl_df")

    x <- make_main_results_table(
        make_main_results_df(sample_output_mwra_all_res_df,
                             stall_probability = 0.5, indicator = "MetDemModMeth"),
        require_level_condition = FALSE)
    expect_s3_class(x, "tbl_df")
})


test_that("`make_n_stall_years_list()` works.", {
    data(sample_output_wra_all_res_df)
    x <- make_n_stall_years_list(
        make_main_results_df(sample_output_wra_all_res_df,
                             stall_probability = 0.5, indicator = "MetDemModMeth"))
    expect_true(is.list(x))
})


test_that("`make_all_results_list()` works.", {
    data(sample_output_mwra_all_res_df)

    x <- make_all_results_list(sample_output_mwra_all_res_df)
    expect_true(is.list(x))

    x <- make_all_results_list(sample_output_mwra_all_res_df, stall_probability = 0.5)
    expect_true(is.list(x))

    x <- make_all_results_list(sample_output_mwra_all_res_df, indicator = "MetDemModMeth")
    expect_true(is.list(x))
})


test_that("`count_n_plateau_countries()` works.", {
    data(sample_output_mwra_all_res_df)
    x <- count_n_plateau_countries(
        make_all_results_list(
            sample_output_mwra_all_res_df,
            stall_probability = 0.5, indicator = "MetDemModMeth"))
    expect_type(x, "integer")
})


test_that("`make_tbl_country_n_plateau_years()` works.", {
    data(sample_output_wra_all_res_df)
    x <- make_tbl_country_n_plateau_years(
        make_all_results_list(
            sample_output_wra_all_res_df,
            stall_probability = 0.5, indicator = "MetDemModMeth"))
    expect_s3_class(x, "tbl_df")
})


test_that("`make_period_compare_plot()` works.", {
    data(sample_output_mwra_all_res_df)
    x <- make_period_compare_plot(
        make_main_results_df(
            sample_output_mwra_all_res_df,
            stall_probability = 0.5, indicator = "MetDemModMeth"))
    expect_s3_class(x, "gg")
})
