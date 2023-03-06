###-----------------------------------------------------------------------------
### * Plots

##----------------------------------------------------------------------

##' @import ggplot2
##' @export
plot_q_diff <- function(df, iso_all, ylim = TRUE, xlim = c(1980, 2020)) {
    gp <- ggplot(data = df, aes(x = year, y = `annual_change_50%`)) +
        geom_hline(yintercept = 0, linetype = 2, col = "blue") +
        geom_line(na.rm = TRUE) +
        geom_line(aes(y = `annual_change_10%`), linetype = 2, na.rm = TRUE) +
        geom_line(aes(y = `annual_change_90%`), linetype = 2, na.rm = TRUE) +
        geom_ribbon(aes(ymin = `annual_change_2.5%`, ymax = `annual_change_97.5%`), alpha = 0.2) +
        facet_wrap(~ indicator) +
        labs(title = paste0(iso_all[i, "name"], " (",
                            iso_all[i, "region"], ")"),
             x = "Year", y = "%-age points")
    if (ylim) gp <- gp + ylim(c(-10, 10))
    if (is.numeric(xlim)) gp <- gp + xlim(xlim)
    return(gp)
}

##----------------------------------------------------------------------

##' @import ggplot2
##' @export
stall_plot <- function(plot_df, iso_all,
                       min_stall_length = attr(plot_df, "min_stall_length"),
                       xvar = c("year", "Modern_median", "Unmet_median"),
                       yvar = c("annual_change_50%", "stall_prob",
                                "Total_median", "Modern_median", "Traditional_median",
                                "Unmet_median", "TotalPlusUnmet_median", "TradPlusUnmet_median",
                                "Met Demand_median", "MetDemModMeth_median"),
                       ylab = yvar,
                       mid_year = TRUE,
                       stall_probability_threshold,
                       probability_scale = c("percent", "prop"),
                       CP_range_condition_min = attr(plot_df, "CP_range_condition_min"),
                       CP_range_condition_max = attr(plot_df, "CP_range_condition_max"),
                       MDMM_range_condition_min = attr(plot_df, "MDMM_range_condition_min"),
                       MDMM_range_condition_max = attr(plot_df, "MDMM_range_condition_max"),
                       Level_condition_variant = attr(plot_df, "Level_condition_variant"),
                       mark_all_plateaus = FALSE, # 'FALSE' = Do NOT mark plateaus when level condition fails
                       facet_by_indicator = length(unique(plot_df$indicator)) > 1,
                       CP_abbrev = if (facet_by_indicator) { "FP Indicator" } else { "MCP" },
                       CP_not_in_range_abbrev = if (facet_by_indicator) { "FP Indicator" } else { "MCP" },
                       legend_title = "Plateau and Stall Type",
                       add_range_ref_lines = TRUE,
                       add_TFR_stalls = TRUE,
                       add_FP_stalls = TRUE,
                       ylim_plot = NULL,
                       xlim_plot = if (identical(xvar, "year")) { c(1980, 2020) } else c(0, 0.6),
                       line_colour = "black",
                       ribbon_fill = line_colour,
                       use_ggpattern = TRUE) {

    null_arg_msg <- function(x) {
        paste0("Argument '", x, "' is 'NULL'. Check that 'attr(plot_df, ", x, ")' is not 'NULL'.")
    }
    if (is.null(min_stall_length)) stop(null_arg_msg("min_stall_length"))
    if (is.null(CP_range_condition_min)) stop(null_arg_msg("CP_range_condition_min"))
    if (is.null(CP_range_condition_max)) stop(null_arg_msg("CP_range_condition_max"))
    if (is.null(MDMM_range_condition_min)) stop(null_arg_msg("MDMM_range_condition_min"))
    if (is.null(MDMM_range_condition_max)) stop(null_arg_msg("MDMM_range_condition_max"))
    if (is.null(Level_condition_variant)) stop(null_arg_msg("Level_condition_variant"))

    ## Set up
    xvar <- match.arg(xvar)
    yvar <- match.arg(yvar)
    stopifnot(identical(length(stall_probability_threshold), 1L) &&
              is.numeric(stall_probability_threshold) &&
              stall_probability_threshold >= 0 && stall_probability_threshold <= 1)
    probability_scale <- match.arg(probability_scale)

    stall <- paste0("stall_year_prob_", stall_probability_threshold)

    plot_df[, stall] <-
        !is.na(plot_df[[paste0(stall, "_len_period")]]) &
        plot_df[[paste0(stall, "_len_period")]] >= min_stall_length

    cname <- plot_df[1, "name"]
    rname <- plot_df[1, "region"]

    #plot_df$xvar <- plot_df[, xvar]

    if(xvar == "year") {
        if (mid_year) jitt_1 <- jitt_2 <- 0.5
        else {
            plot_df[, xvar] <- floor(plot_df[, xvar])
            jitt_1 <- 0
            jitt_2 <- 1
        }
    } else jitt_1 <- jitt_2 <- 0.005

    if (is.null(ylim_plot)) {
        if (identical(yvar, "annual_change_50%")) {
            ylim_plot <- c(-10, 10)
            yscale_break <- 4
        } else if (identical(yvar, "stall_prob") && identical(probability_scale, "prop")) {
            ylim_plot <- c(0, 1)
            yscale_break <- 0.2
        } else if (identical(yvar, "stall_prob") && identical(probability_scale, "percent")) {
            ylim_plot <- c(0, 100)
            yscale_break <- 20
        } else if (identical(yvar, "Modern_median")) {
            ylim_plot <- c(0, 100)
            yscale_break <- 20
        } else {
            ylim_plot <- c(0, 100)
            yscale_break <- 20
        }
    }

    ## Make prevalences percentages
    if (yvar %in% c("Total_median", "Modern_median", "Traditional_median",
                    "Unmet_median", "TotalPlusUnmet_median", "TradPlusUnmet_median",
                    "Met Demand_median", "MetDemModMeth_median")) {
        indicator_name <- gsub("_median", "", yvar)
        prevalence_columns <- grep(paste0(indicator_name, "\\.[0-9.%]+"), colnames(plot_df), value = TRUE)
        plot_df[,prevalence_columns] <- 100 * plot_df[,prevalence_columns]
        plot_df[[yvar]] <- 100 * plot_df[[yvar]]
        ylab <- "%"
    } else if (identical(yvar, "stall_prob") && identical(probability_scale, "percent")) {
        plot_df[[yvar]] <- 100 * plot_df[[yvar]]
        ylab <- "%"
    }

    ## y-axis labels
    if (identical(yvar, "annual_change_50%")) {
        ylab <- "%-age points"
    } else if (identical(yvar, "stall_prob") && identical(probability_scale, "prop")) {
        ylab <- ""
    }


    ## Create stall period data frames

    stall_prob_exceed_df <- plot_df[!is.na(plot_df[,stall]) & plot_df[,stall],]
    if (!mark_all_plateaus) {
        if ("Level_condition_met" %in% colnames(stall_prob_exceed_df)) {
            stall_prob_exceed_df <- stall_prob_exceed_df[stall_prob_exceed_df$Level_condition_met, ]
        } else {
            warning("'mark_all_plateaus' is 'FALSE', but 'Level_condition_met' is not a column in 'plot_df'. All plateaus will be plotted.")
        }
    }
    stall_prob_exceed_df <- stall_prob_exceed_df |>
        plyr::ddply(.variables = c("indicator", xvar), .fun = function(z) {
                  data.frame(id = as.character(paste(z[,xvar], z[,"indicator"])), value = 1,
                             x = c(z[,xvar] - rep(jitt_1, 2), z[,xvar] + rep(jitt_2, 2)),
                             ## y = c(z$`annual_change_2.5%`, z$`annual_change_97.5%`,
                             ##       z$`annual_change_97.5%`, z$`annual_change_2.5%`),
                             y2 = ylim_plot[c(1,2,2,1)]
                             )
              })

    stall_tfr_df <- plot_df[!is.na(plot_df[,"stall_TFR"]) & plot_df[,"stall_TFR"],] |>
        plyr::ddply(.variables = c("indicator", xvar), .fun = function(z) {
                  data.frame(id = as.character(paste(z[,xvar], z[,"indicator"])), value = 1,
                             x = c(z[,xvar] - rep(jitt_1, 2), z[,xvar] + rep(jitt_2, 2)),
                             y = ylim_plot[c(1,2,2,1)])
              })

    stall_tfr_moderate_df <- plot_df[!is.na(plot_df[,"stall_TFR_moderate"]) &
                                     plot_df[,"stall_TFR_moderate"],] |>
        plyr::ddply(.variables = c("indicator", xvar), .fun = function(z) {
                  data.frame(id = as.character(paste(z[,xvar], z[,"indicator"])), value = 1,
                             x = c(z[,xvar] - rep(jitt_1, 2), z[,xvar] + rep(jitt_2, 2)),
                             y = ylim_plot[c(1,2,2,1)])
              })

    stall_tfr_weak_df <- plot_df[!is.na(plot_df[,"stall_TFR_weak"]) &
                                 plot_df[,"stall_TFR_weak"],] |>
        plyr::ddply(.variables = c("indicator", xvar), .fun = function(z) {
                  data.frame(id = as.character(paste(z[,xvar], z[,"indicator"])), value = 1,
                             x = c(z[,xvar] - rep(jitt_1, 2), z[,xvar] + rep(jitt_2, 2)),
                             y = ylim_plot[c(1,2,2,1)])
              })

    stall_dummy_df <- data.frame(indicator = unique(plot_df$indicator),
                                 x = median(plot_df[, xvar]), y = mean(plot_df[, yvar]),
                                 y2 = mean(plot_df[, yvar]),
                                 id = 1)

    ## CP_not_in_range_df <- plot_df[!plot_df[,"CP_in_range"] &
    ##                               plot_df$indicator %in% c("Modern", "Unmet"),] |>
    ##     plyr::ddply(.variables = c("indicator", xvar), .fun = function(z) {
    ##               data.frame(id = as.character(paste(z[,xvar], z[,"indicator"])), value = 1,
    ##                          x = c(z[,xvar] - rep(jitt_1, 2), z[,xvar] + rep(jitt_2, 2)),
    ##                          y = ylim_plot[c(1,2,2,1)])
    ##           })

    ## MDMM_not_in_range_df <- plot_df[!plot_df[,"MDMM_in_range"] &
    ##                               plot_df$indicator == "MetDemModMeth",] |>
    ##     plyr::ddply(.variables = c("indicator", xvar), .fun = function(z) {
    ##               data.frame(id = as.character(paste(z[,xvar], z[,"indicator"])), value = 1,
    ##                          x = c(z[,xvar] - rep(jitt_1, 2), z[,xvar] + rep(jitt_2, 2)),
    ##                          y = ylim_plot[c(1,2,2,1)])
    ##           })

    ## indicator_not_in_range_df <-
    ##     rbind(CP_not_in_range_df, MDMM_not_in_range_df)

    indicator_not_in_range_df <- plot_df[!plot_df[,"Level_condition_met"] &
                                         plot_df$indicator %in% c("Modern", "Unmet", "MetDemModMeth"),] |>
        plyr::ddply(.variables = c("indicator", xvar), .fun = function(z) {
                  data.frame(id = as.character(paste(z[,xvar], z[,"indicator"])), value = 1,
                             x = c(z[,xvar] - rep(jitt_1, 2), z[,xvar] + rep(jitt_2, 2)),
                             y = ylim_plot[c(1,2,2,1)])
              })

    ## Base plot
    gp <- ggplot(data = plot_df, aes(x = .data[[xvar]], y = .data[[yvar]]))

    ## Reference lines
    if (add_range_ref_lines) {
        if (identical(yvar, "annual_change_50%")) {
            gp <- gp + geom_hline(yintercept = 0.5 / 100, linetype = 2, col = "blue")

        } else if (yvar %in% c("MetDemModMeth_median", "Modern_median")) {
            if (identical(yvar, "Modern_median")) {
                ref_line_df_1 <- expand.grid(indicator = "Modern",
                                             yintercept = CP_range_condition_min * 100)
                ref_line_df_2 <- expand.grid(indicator = "Modern",
                                             yintercept = CP_range_condition_max * 100)
            } else if (identical(yvar, "MetDemModMeth_median")) {
                ref_line_df_1 <- expand.grid(indicator = "MetDemModMeth",
                                             yintercept = MDMM_range_condition_min * 100)
                ref_line_df_2 <- expand.grid(indicator = "MetDemModMeth",
                                             yintercept = MDMM_range_condition_max * 100)
            }
            gp <- gp +
                geom_hline(data = ref_line_df_1, aes(yintercept = yintercept), linetype = 2, col = "blue")

            if (!identical(Level_condition_variant, "v2 - SDG Only")) {
                gp <- gp +
                    geom_hline(data = ref_line_df_2, aes(yintercept = yintercept), linetype = 2, col = "blue")
            }

        } else if (identical(yvar, "stall_prob")) {

            prob_criterion <- as.numeric(gsub("stall_year_prob_", "", stall))
            if (identical(probability_scale, "percent")) prob_criterion <- 100 * prob_criterion
            ref_line_df <-
                data.frame(indicator = c("Modern", "Unmet", "MetDemModMeth")[c("Modern", "Unmet", "MetDemModMeth") %in%
                                                                             unique(plot_df$indicator)],
                           yintercept = prob_criterion)
            add_ref_line <- FALSE
            if (facet_by_indicator) add_ref_line <- TRUE
            else if (identical(length(unique(plot_df$indicator)), 1L)) {
                ref_line_df <- ref_line_df[ref_line_df$indicator == plot_df$indicator[1],]
                add_ref_line <- TRUE
            }
            if (add_ref_line) {
                if (nrow(ref_line_df)) {
                    gp <- gp +
                        geom_hline(data = ref_line_df, aes(yintercept = yintercept), linetype = 2, col = "blue")
                }
            }
        }
    }

    ## Fill and Pattern scales
    fill_alpha <- 0.25
    gp <- gp +
        scale_y_continuous(limits = ylim_plot,
                           breaks = seq(from = ylim_plot[1], to = ylim_plot[2], by = yscale_break)) +
        scale_fill_manual(values = setNames(c("grey75",
                                              RColorBrewer::brewer.pal(n = 11, "RdBu")[1],
                                              RColorBrewer::brewer.pal(n = 11, "RdBu")[11],
                                              RColorBrewer::brewer.pal(n = 11, "RdBu")[10],
                                              RColorBrewer::brewer.pal(n = 11, "RdBu")[8]),
                                            c("Level condition\nnot met",
                                              CP_abbrev,
                                              "TFR:\nStrong+\nevidence", "TFR:\nModerate\nevidence",
                                              "TFR:\nLimited\nevidence")),
                          name = legend_title)  +
        labs(title = paste0(cname, " (",
                            rname, ")"),
             x = xvar, y = ylab) +
        theme_bw() +
        theme(legend.position = "bottom", legend.title.align = 0.5) +
        guides(fill = guide_legend(title.position = "top"))

    if (use_ggpattern) {
        gp <- gp +
            ggpattern::scale_pattern_manual(values = setNames(c("pch",
                                                                "none", "none", "none", "none"),
                                                              c("Level condition\nnot met",
                                                                CP_abbrev,
                                                                "TFR:\nStrong+\nevidence", "TFR:\nModerate\nevidence",
                                                                "TFR:\nLimited\nevidence")),
                                            name = legend_title)
    }

    if (is.numeric(xlim_plot)) gp <- gp + xlim(xlim_plot)

    ## Facets
    if (facet_by_indicator) gp <- gp + facet_wrap(~ indicator)

    ## Level conditions
    add_range_regions <- FALSE
    if (facet_by_indicator) add_range_regions <- TRUE
    else if (identical(length(unique(plot_df$indicator)), 1L)) {
        indicator_not_in_range_df <-
            indicator_not_in_range_df[indicator_not_in_range_df$indicator == plot_df$indicator[1],]
        add_range_regions <- TRUE
    }
    if (add_range_regions) {
        if (nrow(indicator_not_in_range_df)) {
            if (use_ggpattern) {
                gp <- gp + ggpattern::geom_polygon_pattern(data = indicator_not_in_range_df,
                                                           aes(x = x, y = y, group = id,
                                                               fill = "Level condition\nnot met",
                                                               pattern = "Level condition\nnot met"),
                                                           pattern_shape = 20,
                                                           pattern_density = 0.1,
                                                           pattern_angle = 0,
                                                           pattern_fill = "darkgray",
                                                           pattern_colour = "darkgray",
                                                           pattern_alpha = fill_alpha,
                                                           alpha = fill_alpha)
            } else {
                gp <- gp + geom_polygon(data = indicator_not_in_range_df,
                                        aes(x = x, y = y, group = id,
                                            fill = "Level condition\nnot met"),
                                        colour = NA,
                                        alpha = fill_alpha)
            }
        }
    }

    if (add_TFR_stalls) {

        ## TFR moderate stalls
        if (nrow(stall_tfr_moderate_df)) {
            plot_df_tfr_stall_subtype <- stall_tfr_moderate_df
        } else {
            plot_df_tfr_stall_subtype <- stall_dummy_df
        }
        if (use_ggpattern) {
            gp <- gp + ggpattern::geom_polygon_pattern(data = plot_df_tfr_stall_subtype,
                                                       aes(x = x, y = y, group = id,
                                                           fill = "TFR:\nModerate\nevidence",
                                                           pattern = "TFR:\nModerate\nevidence"),
                                                       alpha = fill_alpha)
        } else {
            gp <- gp + geom_polygon(data = plot_df_tfr_stall_subtype,
                                    aes(x = x, y = y, group = id,
                                        fill = "TFR:\nModerate\nevidence"),
                                    alpha = fill_alpha)
        }

        ## TFR stalls
        if (nrow(stall_tfr_df)) {
            plot_df_tfr_stall_subtype <- stall_tfr_df
        } else {
            plot_df_tfr_stall_subtype <- stall_dummy_df
        }
        if (use_ggpattern) {
            gp <- gp + ggpattern::geom_polygon_pattern(data = plot_df_tfr_stall_subtype,
                                                       aes(x = x, y = y, group = id,
                                                           fill = "TFR:\nStrong+\nevidence",
                                                           pattern = "TFR:\nStrong+\nevidence"),
                                                       alpha = fill_alpha)
        } else {
            gp <- gp + geom_polygon(data = plot_df_tfr_stall_subtype,
                                    aes(x = x, y = y, group = id,
                                        fill = "TFR:\nStrong+\nevidence"),
                                    alpha = fill_alpha)
        }

        ## TFR weak stalls
        if (nrow(stall_tfr_weak_df)) {
            plot_df_tfr_stall_subtype <- stall_tfr_weak_df
        } else {
            plot_df_tfr_stall_subtype <- stall_dummy_df
        }
        if (use_ggpattern) {
            gp <- gp + ggpattern::geom_polygon_pattern(data = plot_df_tfr_stall_subtype,
                                                       aes(x = x, y = y, group = id,
                                                           fill = "TFR:\nLimited\nevidence",
                                                           pattern = "TFR:\nLimited\nevidence"),
                                                       alpha = fill_alpha)
        } else {
            gp <- gp + geom_polygon(data = plot_df_tfr_stall_subtype,
                                                       aes(x = x, y = y, group = id,
                                                           fill = "TFR:\nLimited\nevidence"),
                                                       alpha = fill_alpha)
        }
    }

    if (add_FP_stalls) {

        ## FP stalls
        if(nrow(stall_prob_exceed_df)) {
            plot_df_fp_stall <- stall_prob_exceed_df
        } else {
            plot_df_fp_stall <- stall_dummy_df
        }
        if (use_ggpattern) {
            gp <- gp + ggpattern::geom_polygon_pattern(data =  plot_df_fp_stall,
                                                aes(x = x, y = y2, group = id, fill = CP_abbrev,
                                                    pattern = CP_abbrev),
                                                alpha = fill_alpha)
        } else {
            gp <- gp + geom_polygon(data =  plot_df_fp_stall,
                                                aes(x = x, y = y2, group = id, fill = CP_abbrev),
                                                alpha = fill_alpha)
        }
    }

    ## Indicator Lines and Ribbons
    gp <- gp + geom_line(colour = line_colour, na.rm = TRUE)

    if (yvar %in% c("Total_median", "Modern_median", "Traditional_median",
                    "Unmet_median", "TotalPlusUnmet_median", "TradPlusUnmet_median",
                    "Met Demand_median", "MetDemModMeth_median")) {
        yvar_indicator <- gsub("_median", "", yvar)
        if (paste0(yvar_indicator, ".10%") %in% colnames(plot_df) &&
            paste0(yvar_indicator, ".90%") %in% colnames(plot_df)) {
            gp <- gp +
                geom_line(aes(y = .data[[paste0(yvar_indicator, ".10%")]]),
                          linetype = 2, colour = line_colour, na.rm = TRUE) +
                geom_line(aes(y = .data[[paste0(yvar_indicator, ".90%")]]),
                          linetype = 2, colour = line_colour, na.rm = TRUE)
        }
        if (paste0(yvar_indicator, ".2.5%") %in% colnames(plot_df) &&
            paste0(yvar_indicator, ".97.5%") %in% colnames(plot_df)) {
            gp <- gp +
                geom_ribbon(aes(ymin = .data[[paste0(yvar_indicator, ".2.5%")]],
                                       ymax = .data[[paste0(yvar_indicator, ".97.5%")]]),
                            alpha = 0.15, fill = ribbon_fill)
        }
    }

    if (identical(yvar, "annual_change_50%")) {
        gp <- gp +
            geom_line(aes(y = `annual_change_10%`), linetype = 2, colour = line_colour, na.rm = TRUE) +
            geom_line(aes(y = `annual_change_90%`), linetype = 2, colour = line_colour, na.rm = TRUE) +
            geom_ribbon(aes(ymin = `annual_change_2.5%`, ymax = `annual_change_97.5%`),
                        alpha = 0.15, fill = ribbon_fill)
    }

    return(gp)
}

##----------------------------------------------------------------------

##' @import ggplot2
##' @export
plot_stall_prob <- function(df, iso_all, ylim = c(0, 1), xlim = c(1980, 2020)) {
    gp <- ggplot(data = df, aes(x = as.numeric(year), y = as.numeric(stall_prob))) +
        geom_line(na.rm = TRUE) +
        geom_hline(aes(yintercept = 0.5), lty = 2, col = "blue") +
        facet_wrap(~ indicator) +
        labs(title = paste0(iso_all[i, "name"], " (",
                            iso_all[i, "region"], ")"),
             subtitle = "Plateau Probability",
             x = "Year", y = "probability")
    if (is.numeric(ylim)) gp <- gp + ylim(ylim)
    if (is.numeric(xlim)) gp <- gp + xlim(xlim)
    return(gp)
}

##----------------------------------------------------------------------

## Returns a plot for a single country. 'plot_df' must be for a single country.

##' @import ggplot2
##' @export
country_profile_plot <- function(plot_df, iso_all, stall_probability_threshold, use_ggpattern = TRUE) {

    stopifnot(identical(length(stall_probability_threshold), 1L) &&
              is.numeric(stall_probability_threshold) &&
              stall_probability_threshold >= 0 && stall_probability_threshold <= 1)

    plot_df_modern <- dplyr::filter(plot_df, indicator == "Modern")

    plot_df_modern$stall_stat <- "CP Modern"
    pl1 <- stall_plot(plot_df_modern, iso_all = iso_all, CP_abbrev = "CP Modern", yvar = "Modern_median",
                      facet_by_indicator = FALSE,
                      stall_probability_threshold = stall_probability_threshold,
                      CP_not_in_range_abbrev = "FP Indicator",
                      use_ggpattern = use_ggpattern) +
        facet_grid(indicator ~ stall_stat, switch = "y") +
        labs(title = "")

    plot_df_modern$stall_stat <- "Annual Change"
    pl2 <- stall_plot(plot_df_modern, iso_all = iso_all, CP_abbrev = "CP Modern", facet_by_indicator = FALSE,
                      stall_probability_threshold = stall_probability_threshold,
                      CP_not_in_range_abbrev = "FP Indicator",
                      use_ggpattern = use_ggpattern) +
        facet_grid(indicator ~ stall_stat, switch = "y") +
        labs(title = "")

    plot_df_modern$stall_stat <- "Plateau Probability"
    pl3 <- stall_plot(plot_df_modern, iso_all = iso_all, CP_abbrev = "CP Modern", yvar = "stall_prob",
                      facet_by_indicator = FALSE,
                      stall_probability_threshold = stall_probability_threshold,
                      CP_not_in_range_abbrev = "FP Indicator",
                      use_ggpattern = use_ggpattern) +
        facet_grid(indicator ~ stall_stat, switch = "y") +
        labs(title = "")


    plot_df_mdmm <- dplyr::filter(plot_df, indicator == "MetDemModMeth")

    plot_df_mdmm$stall_stat <- "Met Demand Modern"
    pl4 <- stall_plot(plot_df_mdmm, iso_all = iso_all, CP_abbrev = "Met Dem.\nMod. Meth.", yvar = "MetDemModMeth_median",
                      facet_by_indicator = FALSE,
                      stall_probability_threshold = stall_probability_threshold,
                      CP_not_in_range_abbrev = "FP Indicator",
                      use_ggpattern = use_ggpattern) +
        facet_grid(indicator ~ stall_stat, switch = "y") +
        labs(title = "")

    plot_df_mdmm$stall_stat <- "Annual Change"
    pl5 <- stall_plot(plot_df_mdmm, iso_all = iso_all, CP_abbrev = "Met Dem.\nMod. Meth.", facet_by_indicator = FALSE,
                      stall_probability_threshold = stall_probability_threshold,
                      CP_not_in_range_abbrev = "FP Indicator",
                      use_ggpattern = use_ggpattern) +
        facet_grid(indicator ~ stall_stat, switch = "y") +
        labs(title = "")

    plot_df_mdmm$stall_stat <- "Plateau Probability"
    pl6 <- stall_plot(plot_df_mdmm, iso_all = iso_all, CP_abbrev = "Met Dem.\nMod. Meth.", yvar = "stall_prob",
                      facet_by_indicator = FALSE,
                      stall_probability_threshold = stall_probability_threshold,
                      CP_not_in_range_abbrev = "FP Indicator",
                      use_ggpattern = use_ggpattern) +
        facet_grid(indicator ~ stall_stat, switch = "y") +
        labs(title = "")

    ggpubr::ggarrange(pl1, pl2, pl3, pl4, pl5, pl6, nrow = 2, ncol = 3,
                      common.legend = TRUE, legend = "bottom") |>
        ggpubr::annotate_figure(fig.lab = paste0(plot_df[1, "name"], " (",
                                                 plot_df[1, "region"], ")"),
                                fig.lab.pos = "top.left", fig.lab.size = 16)
}
