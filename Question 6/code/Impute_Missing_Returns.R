impute_missing_returns <- function(ReturnMatrix,
                                   impute_returns_method = "NONE",
                                   Seed = 6578){

    if( !"date" %in% colnames(ReturnMatrix) ) stop("No 'date' column provided in ReturnMatrix. Try again please.")

    if( impute_returns_method %in% c("NONE", "None", "none") ) {
        if( any(is.na(ReturnMatrix)) ) warning("There are missing values in the return matrix.. Consider maybe using impute_returns_method = 'Drawn_Distribution_Own' / 'Drawn_Distribution_Collective'")
        return(ReturnMatrix)
    }

    if( impute_returns_method  == "Average") {

        ReturnMatrix <-
            ReturnMatrix %>% gather(Stocks, Returns, -date) %>%
            group_by(date) %>%
            mutate(Avg = mean(Returns, na.rm=T)) %>%
            mutate(Avg = coalesce(Avg, 0)) %>%
            ungroup() %>%
            mutate(Returns = coalesce(Returns, Avg)) %>% select(-Avg) %>% spread(Stocks, Returns)

        return(ReturnMatrix)
    } else

        if( impute_returns_method  == "Drawn_Distribution_Own") {

            set.seed(Seed)
            N <- nrow(ReturnMatrix)
            ReturnMatrix <-
                left_join(ReturnMatrix %>%
                              gather(Stocks, Returns, -date),
                          ReturnMatrix %>%
                              gather(Stocks, Returns, -date) %>%
                              group_by(Stocks) %>%
                              mutate(Dens = list(density(Returns, na.rm=T))) %>%
                              summarise(Random_Draws = list(sample(Dens[[1]]$x, N, replace = TRUE, prob=.$Dens[[1]]$y))),
                          by = "Stocks"
                ) %>%  group_by(Stocks) %>%
                mutate(Returns = coalesce(Returns, Random_Draws[[1]][row_number()])) %>%
                select(-Random_Draws) %>% ungroup() %>% spread(Stocks, Returns)
            return(ReturnMatrix)
        } else

            if( impute_returns_method  == "Drawn_Distribution_Collective") {

                set.seed(Seed)
                NAll <- nrow(ReturnMatrix %>% gather(Stocks, Returns, -date))
                ReturnMatrix <-
                    bind_cols(
                        ReturnMatrix %>% gather(Stocks, Returns, -date),
                        ReturnMatrix %>% gather(Stocks, Returns, -date) %>%
                            mutate(Dens = list(density(Returns, na.rm=T))) %>%
                            summarise(Random_Draws = list(sample(Dens[[1]]$x, NAll, replace = TRUE, prob=.$Dens[[1]]$y))) %>%
                            unnest(Random_Draws)
                    ) %>%
                    mutate(Returns = coalesce(Returns, Random_Draws)) %>% select(-Random_Draws) %>% spread(Stocks, Returns)
                return(ReturnMatrix)
            } else

                if( impute_returns_method  == "Zero") {
                    warning("Not Acceptable")
                    ReturnMatrix[is.na(ReturnMatrix)] <- 0
                    return(ReturnMatrix)
                } else
                    stop("Please provide a valid impute_returns_method method. Options include:\n'Average', 'Drawn_Distribution_Own', 'Drawn_Distribution_Collective' and 'Zero'.")

    ReturnMatrix

}