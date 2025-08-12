

##' @export
smooth_trajectories_plot <- function(iso, iso_all = get_iso_all(),
                                indicator = c("Modern", "MetDemModMeth"),
                                smooth_type = c("Trajectories", "Moving Average", "Local Linear Smooth"),
                                range_condition_lower = NULL,
                                range_condition_upper = NULL,
                                change_condition = 0.5 / 100,
                                filter_width = 3,
                                stall_probability_threshold,
                                differences = 1,
                                output_dir, denominator_count_filename,
                                ylim = NULL,
                                title = NULL,
                                panel_counter_type = c("alpha", "roman"),
                                subcaption = "Vertical marks indicate stall years based on _all_ trajectories.") {

    indicator <- match.arg(indicator)
    smooth_type <- match.arg(smooth_type, several.ok = TRUE)
    if (is.null(range_condition_lower)) {
        range_condition_lower <-
            switch(indicator, Modern = 0.1, MetDemModMeth = 0.05, stop("'range_condition_lower' invalid."))
    }
    if (is.null(range_condition_upper)) {
        range_condition_upper <-
            switch(indicator, Modern = 0.6, MetDemModMeth = 0.85, stop("'range_condition_lower' invalid."))
    }
    panel_counter_type <- match.arg(panel_counter_type)
    n_smooths <- length(smooth_type)
    if (is.null(title)) title <- paste0(get_name(iso, iso_all), ", ", indicator)

    ## -------* Functions

    set_panel_counter <- function(counter, type = panel_counter_type) {
        stopifnot(is.numeric(counter))
        if (identical(type, "arabic")) return(as.character(counter))
        else if (identical(type, "alpha")) return(letters[counter])
        else return(tolower(as.roman(counter)))
    }

    set_ylab <- function(indicator) {
        switch(indicator, Modern = "MCP (%)", MetDemModMeth = "Met Demand, Modern (%)")
    }

    ## -------* Get Trajectories

    tra <- make_stall_prob_df(iso,
                              output_dir = output_dir, add_iso_column = TRUE,
                              differences = differences,
                              change_condition = change_condition,
                              filter_width = filter_width,
                              denominator_count_filename = denominator_count_filename,
                              .special_return_trajectories = TRUE)

    ## -------* Convert Proportions to Percentages !!!!

    tra <- tra * 100
    change_condition <- change_condition * 100
    range_condition_lower <- range_condition_lower * 100
    range_condition_upper <- range_condition_upper * 100

    ## -------* Stall Probabilities

    fpind_tra <- tra[, indicator, ]
    fpind_median <- apply(fpind_tra, 1, quantile, probs = 0.5)
    stall_prob <- rowMeans(apply(fpind_tra, 2, function(z) c(NA, diff(z)) < change_condition),
                           na.rm = TRUE)
    stall_prob_gt_thold <- stall_prob > stall_probability_threshold &
        is.finite(stall_prob) & fpind_median >= range_condition_lower &
        fpind_median <= range_condition_upper

    ## -------* Plots

    if (!is.na(title) && nchar(title)) {
        par(mar=c(5, 4, 2, 2) + 0.1)
        layout(rbind(rep(1, 2),
                     matrix(2:(3 + (n_smooths - 1) * 2), ncol = 2, byrow = TRUE)),
               heights = c(1, rep(3, n_smooths)))
        plot.new()
        text(0.5,0.5, title, cex = 2, font = 2)
    } else {
        par(mar=c(5, 4, 2, 2) + 0.1)
        layout(matrix(1:(2 + (n_smooths - 1) * 2), ncol = 2, byrow = TRUE),
               heights = rep(3, n_smooths))
    }

    x_vec <- as.numeric(dimnames(fpind_tra)[[1]]) - 0.5

    if (is.null(ylim)) {
    ylim <- c(min(fpind_tra[, 1:5]) - 0.01 * min(fpind_tra[, 1:5]),
              max(fpind_tra[, 1:5]) + 0.01 * max(fpind_tra[, 1:5]))
    }

    ylim_diff <- NULL

    panel_counter <- 0

    plateau_years <- list()

    ## -------** Raw Trajectories

    if ("Trajectories" %in% smooth_type) {

        ## Trajecotries and median

        panel_counter <- panel_counter + 1

        matplot(as.numeric(dimnames(fpind_tra)[[1]]) - 0.5, fpind_tra[, 3:5],
                type = "l", col = "grey", lty = 1, xlab = "year", ylab = set_ylab(indicator),
                main = paste0("(", set_panel_counter(panel_counter), ") Trajectories"),
                sub = subcaption,
                ylim = ylim)
        lines(as.numeric(dimnames(fpind_tra)[[1]]) - 0.5, fpind_tra[, 1],
              col = "red")
        lines(as.numeric(dimnames(fpind_tra)[[1]]) - 0.5, fpind_tra[, 2],
              col = "yellow4")
        lines(x = as.numeric(names(fpind_median)) - 0.5, fpind_median, lwd = 2, lty = 2)
        rug(x = x_vec[stall_prob_gt_thold], lwd = 2, ticksize = 0.08)
        legend("topleft", lty = c(2, rep(1, 3)), lwd = c(2, rep(1, 3)),
               col = c("black", "red", "yellow4", "grey"),
               legend = c("Median", "Trajectory 1", "Trajectory 2", "Other trajectories"))

        ## Annual Differences

        panel_counter <- panel_counter + 1

        y_diff <- apply(fpind_tra[, 3:5], 2,
                        function(z) c(NA, diff(z, differences = differences)))

        y_vec <- c(NA, diff(fpind_tra[, 1]))
        y_vec2 <- c(NA, diff(fpind_tra[, 2]))

        ylim_diff <- c(min(y_diff, na.rm = TRUE) - 0.1 * abs(min(y_diff, na.rm = TRUE)),
                       max(y_diff, na.rm = TRUE) + 0.1 * max(y_diff, na.rm = TRUE))

        matplot(x_vec, y_diff,
                col = "grey", type = "l", lty = 1, xlab = "year", ylab = "year-on-year difference (% points)",
                ## ylim = c(min(c(y_vec, y_vec2), na.rm = TRUE) * 1.2,
                ##          max(c(y_vec, y_vec2), na.rm = TRUE) * 1.1),
                main = paste0("(", set_panel_counter(panel_counter), ") Annual Differences"),
                sub = subcaption,
                ylim = ylim_diff)
        lines(x_vec, y_vec,
              col = "red", type = "l", xlab = "year", ylab = "year-on-year difference")
        lines(x_vec, y_vec2,
              col = "yellow4", type = "l", xlab = "year", ylab = "year-on-year difference")
        abline(h = change_condition, lty = 2, col = "blue")
        rug(x = x_vec[stall_prob_gt_thold], lwd = 2, ticksize = 0.08)
        legend("topleft", lty = c(1, 1, 1, 2),
               col = c("red", "yellow4", "grey", "blue"),
               legend = c("Trajectory 1", "Trajectory 2", "Other trajectories",
                          paste0(change_condition, "% threshold")))

        plateau_years <- c(plateau_years, list(trajectories = x_vec[stall_prob_gt_thold]))

    }

    ## -------** Columns for smooths

    if ("Moving Average" %in% smooth_type) {

        ## -------*** MA of values, then difference

        fpind_tra_f <- apply(fpind_tra, 2, stats::filter, f = rep(1/filter_width, filter_width), sides = 2)
        stall_prob_f <- rowMeans(apply(fpind_tra_f, 2, function(z) c(NA, diff(z)) < 0.5), na.rm = TRUE)

        stall_prob_f_gt_80 <- stall_prob_f > stall_probability_threshold &
            is.finite(stall_prob_f) & fpind_median >= range_condition_lower &
            fpind_median <= range_condition_upper

        ## Smoothed trajectories

        panel_counter <- panel_counter + 1

        matplot(as.numeric(dimnames(fpind_tra)[[1]]) - 0.5, fpind_tra_f[, 3:5],
                type = "l", col = "grey", lty = 1, xlab = "year", ylab = set_ylab(indicator),
                main = paste0("(", set_panel_counter(panel_counter), ") Moving Average",
                { if (identical(smooth_type, "Moving Average")) {
                      paste0(" (width ", filter_width, ")")
                  } else { "" }
                },
                " of Trajectories"),
                sub = subcaption,
                ylim = ylim)
        lines(as.numeric(dimnames(fpind_tra)[[1]]) - 0.5, fpind_tra_f[, 1],
              col = "red")
        lines(as.numeric(dimnames(fpind_tra)[[1]]) - 0.5, fpind_tra_f[, 2],
              col = "yellow4")
        lines(x = as.numeric(names(fpind_median)) - 0.5, fpind_median, lwd = 2, lty = 2)
        rug(x = x_vec[stall_prob_f_gt_80], lwd = 2, ticksize = 0.08)
        legend("topleft", lty = c(2, rep(1, 3)), lwd = c(2, rep(1, 3)),
               col = c("black", "red", "yellow4", "grey"),
               legend = c("Median", "Trajectory 1", "Trajectory 2", "Other trajectories"))

        ## Differences or coefficients

        panel_counter <- panel_counter + 1

        y_vec <- c(NA, diff(fpind_tra_f[, 1]))
        y_vec2 <- c(NA, diff(fpind_tra_f[, 2]))

        y_diff <- apply(fpind_tra_f[, 3:5], 2,
                             function(z) c(NA, diff(z, differences = differences)))
        if (is.null(ylim_diff))
            ylim_diff <- c(min(y_diff, na.rm = TRUE) - 0.1 * abs(min(y_diff, na.rm = TRUE)),
                           max(y_diff, na.rm = TRUE) + 0.1 * max(y_diff, na.rm = TRUE))

        matplot(x_vec, y_diff,
                col = "grey", type = "l", lty = 1, xlab = "year", ylab = "year-on-year difference (% points)",
                ## ylim = c(min(c(y_vec, y_vec2), na.rm = TRUE) * 1.2,
                ##          max(c(y_vec, y_vec2), na.rm = TRUE) * 1.1),
                main = paste0("(", set_panel_counter(panel_counter), ") Annual Differences of Moving Average"),
                sub = subcaption,
                ylim = ylim_diff)
        lines(x_vec, y_vec,
              col = "red", type = "l", xlab = "year", ylab = "year-on-year difference")
        lines(x_vec, y_vec2,
              col = "yellow4", type = "l", xlab = "year", ylab = "year-on-year difference")
        abline(h = change_condition, lty = 2, col = "blue")
        rug(x = x_vec[stall_prob_f_gt_80], lwd = 2, ticksize = 0.08)
        legend("topleft", lty = c(1, 1, 1, 2),
               col = c("red", "yellow4", "grey", "blue"),
               legend = c("Trajectory 1", "Trajectory 2", "Other trajectories",
                          paste0(change_condition, "% threshold")))

        plateau_years <- c(plateau_years, list(moving_average = x_vec[stall_prob_f_gt_80]))

    }

    if ("Local Linear Smooth" %in% smooth_type) {

        fpind_tra_lm_local <- lm_local_arr(fpind_tra, bandwidth = filter_width)
        fpind_tra_f <- fpind_tra_lm_local[, "Y_hat", ]
        stall_prob_f <- rowMeans(fpind_tra_lm_local[, "beta", ] < change_condition, na.rm = TRUE)

        stall_prob_f_gt_80 <- stall_prob_f > stall_probability_threshold &
            is.finite(stall_prob_f) & fpind_median >= range_condition_lower &
            fpind_median <= range_condition_upper

        ## Smoothed trajectories

        panel_counter <- panel_counter + 1

        matplot(as.numeric(dimnames(fpind_tra)[[1]]) - 0.5, fpind_tra_f[, 3:5],
                type = "l", col = "grey", lty = 1, xlab = "year", ylab = set_ylab(indicator),
                main = paste0("(", set_panel_counter(panel_counter), ") Local Linear Smooth",
                { if (identical(smooth_type, "Moving Average")) {
                      paste0(" (width ", filter_width, ")")
                  } else { "" }
                },
                " of Trajectories"),
                sub = subcaption,
                ylim = ylim)
        lines(as.numeric(dimnames(fpind_tra)[[1]]) - 0.5, fpind_tra_f[, 1],
              col = "red")
        lines(as.numeric(dimnames(fpind_tra)[[1]]) - 0.5, fpind_tra_f[, 2],
              col = "yellow4")
        lines(x = as.numeric(names(fpind_median)) - 0.5, fpind_median, lwd = 2, lty = 2)
        rug(x = x_vec[stall_prob_f_gt_80], lwd = 2, ticksize = 0.08)
        legend("topleft", lty = c(2, rep(1, 3)), lwd = c(2, rep(1, 3)),
               col = c("black", "red", "yellow4", "grey"),
               legend = c("Median", "Trajectory 1", "Trajectory 2", "Other trajectories"))

        ## Differences or coefficients

        panel_counter <- panel_counter + 1

        x_vec <- as.numeric(dimnames(fpind_tra)[[1]]) - 0.5
        y_vec <- fpind_tra_lm_local[, "beta", 1]
        y_vec2 <- fpind_tra_lm_local[, "beta", 2]

        y_diff <- fpind_tra_lm_local[, "beta", 3:5]
        if (is.null(ylim_diff))
            ylim_diff <- c(min(y_diff, na.rm = TRUE) - 0.1 * abs(min(y_diff, na.rm = TRUE)),
                           max(y_diff, na.rm = TRUE) + 0.1 * max(y_diff, na.rm = TRUE))

        matplot(x_vec, y_diff,
                col = "grey", type = "l", lty = 1, xlab = "year", ylab = "slope coefficients",
                ## ylim = c(min(c(y_vec, y_vec2), na.rm = TRUE) * 1.2,
                ##          max(c(y_vec, y_vec2), na.rm = TRUE) * 1.1),
                main = paste0("(", set_panel_counter(panel_counter), ") Slope Coefficients of Local Linear Smooth"),
                sub = subcaption,
                ylim = ylim_diff)
        lines(x_vec, y_vec,
               col = "red", xlab = "year", ylab = "change per year")
        lines(x_vec, y_vec2,
               col = "yellow4", xlab = "year", ylab = "change per year")
        abline(h = change_condition, lty = 2, col = "blue")
        rug(x = x_vec[stall_prob_f_gt_80], lwd = 2, ticksize = 0.08)
        legend("topleft", lty = c(rep(1, 3), 2),
               col = c("red", "yellow4", "grey", "blue"),
               legend = c("Trajectory 1", "Trajectory 2", "Other trajectories",
                          paste0(change_condition, "% threshold")))

        plateau_years <- c(plateau_years, list(local_linear_smooth = x_vec[stall_prob_f_gt_80]))

    }
return(invisible(plateau_years))
}
