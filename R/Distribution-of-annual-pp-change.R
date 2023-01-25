########## BASED ON JM's SCRIPT 'notes/Distribution-of-annual-pp-change.R' ##########

##' @export
appx_rates_load_results <- function(path, level_condition) {
     df <- read.csv(path) #load data
     df <- df[df$Percentile == 0.5, ]
     df <- merge(df[,!names(df) %in% c("Percentile")], locWagg[,c("Id", "SDGRegion")], by.x="Iso", by.y="Id") # merge with aggregates
     df <- df[df$SDGRegion=="Sub-Saharan Africa",] # restrict data to SSA
     df <- reshape2::melt(df, id.var=c("Iso","Name", "SDGRegion")) # Convert from wide to long format
     df$variable <- as.numeric(gsub("X", "", df$variable)) - 0.5 # Convert years to usable format
     df$value <- df$value*100 # Convert rates to percentages

     df <- df %>%
         arrange(Iso, variable) %>% # Sort data by location and variable (which is year)
         filter(variable>=1980 & variable<=2020) %>% # Subset data to between 1980 and 2020
         group_by(Iso)%>%
         mutate(diff = value - lag(value)) %>% # Get percentage point differences by country
         filter(value>=level_condition[1] & value<=level_condition[2]) # Subset data to only include country-years where CPm is between 10 and 60

     return(df)
}

##' @export
appx_rates_stats_tbl <- function(df, quantiles = c(0.1,0.25,0.5,0.75,0.9),
                                     thresholds = c(0.1, 0.3, 0.5)) {
    df <- as.data.frame(df)

    ## calculate quantiles
    q <- data.frame(probability = quantiles,
                    quantile = quantile(df$diff, quantiles, na.rm = TRUE))

    ## calculate probabilities
    p <- data.frame(threshold = thresholds,
                    probability_lt = sapply(thresholds, function(z) mean(df$diff <= z, na.rm = TRUE)))

    ## five number summary
    fns <- summary(df$diff)
    fns <- data.frame(statistic = names(fns), value = as.numeric(fns))

    return(list(quantiles = q, probabilities = p, summary = fns))
}

##' @export
appx_rates_stats_quantile <- function(x, quantile) {# 'x' is the result from 'appx_rates_stats_tbl' OR a data frame
    stopifnot(is.numeric(quantile))
    if (!is.data.frame(x)) x <- x$quantiles
    stopifnot(identical(sort(colnames(x)), sort(c("probability", "quantile"))))
    if (!all(quantile %in% x$probability)) stop("At least some of '", quantile, "' are not quantiles in 'x$quantiles'.")
    return(x[x$probability %in% quantile, "quantile"])
}

##' @export
appx_rates_stats_IQR <- function(x) { # 'x' is the result from 'appx_rates_stats_tbl' OR a data frame
    if (!is.data.frame(x)) x <- x$quantiles
    stopifnot(identical(sort(colnames(x)), sort(c("probability", "quantile"))))
    if (!all(c(0.25, 0.75) %in% x$probability)) stop("'0.25' and '0.75' quantiles not in 'x$quantiles'.")
    return(appx_rates_stats_quantile(x, 0.75) - appx_rates_stats_quantile(x, 0.25))
}

##' @export
appx_rates_make_ggplot <- function(df, include_table = FALSE, title = NULL) {
    df <- as.data.frame(df)

    ## calculate quantiles /before/ trimming.
    q <- appx_rates_stats_tbl(df, quantiles = c(0.1,0.25,0.5,0.75,0.9))$quantiles$quantile

    ## Trim to remove very extreme values
    df <- df[df$diff < quantile(df$diff, 0.995, na.rm = TRUE), ]

    ## Create plot
    p <- ggplot(df, aes(x=diff))+
        geom_histogram(binwidth=0.1, color="black", fill="blue", alpha=0.2)+
        scale_x_continuous(breaks=seq(-1,4,0.5))+
        ylab("Country-years")+
        xlab("Annual Percentage Point Difference")

    ## Set visualization parameters
    y_axis_range <- ggplot_build(p)$layout$panel_params[[1]]$y.range
    s <- 3
    locY <- y_axis_range[2] * 1.05
    xLoc <- max(df$diff, na.rm = TRUE) - 0.6

    p <- p +
        scale_y_continuous(expand = c(0, 0), limits=c(y_axis_range[1], locY * 1.05))+
        geom_segment(x=q[1], xend=q[1], y=0, yend = y_axis_range[2], color="red", linetype="dashed")+
        geom_segment(x=q[2], xend=q[2], y=0, yend = y_axis_range[2], color="red", linetype="dashed")+
        geom_segment(x=q[3], xend=q[3], y=0, yend = y_axis_range[2], color="red", linetype="dashed")+
        geom_segment(x=q[4], xend=q[4], y=0, yend = y_axis_range[2], color="red", linetype="dashed")+
        geom_segment(x=q[5], xend=q[5], y=0, yend = y_axis_range[2], color="red", linetype="dashed") +
        annotate("text", x=q[1], y=locY, label=bquote(10^th), size=s)+
        annotate("text", x=q[2], y=locY, label=bquote(25^th), size=s)+
        annotate("text", x=q[3], y=locY, label=bquote(50^th), size=s)+
        annotate("text", x=q[4], y=locY, label=bquote(75^th), size=s)+
        annotate("text", x=q[5], y=locY, label=bquote(90^th), size=s)+
            theme(
                axis.text = element_text(color = "black", size=9),
                axis.line = element_line(color="black", size=1.05)
            ) +
    ggtitle(title)

    if (include_table)
        p <- p +
            annotate("text", x=xLoc, y=80, label="Percentage point difference\nat quantile x:", fontface="italic", hjust=0.5)+
            annotate("text", x=xLoc, y=70, label=paste0("10%:    ", round(q[1], digits=2)))+
            annotate("text", x=xLoc, y=65, label=paste0("25%:    ", round(q[2], digits=2)))+
            annotate("text", x=xLoc, y=60, label=paste0("50%:    ", round(q[3], digits=2)))+
            annotate("text", x=xLoc, y=55, label=paste0("75%:    ", round(q[4], digits=2)))+
            annotate("text", x=xLoc, y=50, label=paste0("90%:    ", round(q[5], digits=2)))

    return(p)
}
