LosersComparison <- function(Data, start_date, end_date, next_date) {

    # Calculate overall mean return during the first period

    threshold <- Data %>%
        filter(date >= start_date & date <= end_date) %>%
        summarise(threshold = quantile(Returns, 0.2)) %>%
        pull()

    # Filter the dataset to include only funds meeting the decided criteria in the first period.

    Data1 <- Data %>%
        filter(date >= start_date & date <= end_date) %>%
        group_by(Fund) %>%
        filter(mean(Returns) < threshold) %>%
        ungroup()

    # Filter the dataset for the second period, but simply get the funds who previously performed well, not the ones who currently performed well.

    Data2 <- Data %>%
        filter(date >= end_date & date <= next_date &
                   Fund %in% unique(Data1$Fund))

    # Create a data frame for plotting.

    plot_data <- data.frame(
        Variable = rep(c("Losers", "Subsequent"),
                       each = c(nrow(Data1), nrow(Data2))),
        Returns = c(Data1$Returns, Data2$Returns)
    )

    return(plot_data)
}
