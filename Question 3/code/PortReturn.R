PortReturn <- function(data = ALSI, index = ""){

    fund <- data

    if(!index == ""){
        fund <- data %>% filter(Index_Name %in% index) %>%
            group_by(date) %>%
            mutate(J403 = J403/sum(J403, na.rm = TRUE),
                   J203 = J203/sum(J203, na.rm = TRUE)) %>%
            ungroup()
    }

    J203_weights <- fund %>%
        select(date, Tickers, J203) %>%
        mutate(J203 = coalesce(J203, 0)) %>%
        spread(Tickers, J203) %>%
        tbl_xts()

    J203_weights[is.na(J203_weights)] <- 0

    # J403 Now

    J403_weights <- fund %>%
        select(date, Tickers, J403) %>%
        spread(Tickers, J403) %>%
        tbl_xts()

    J403_weights[is.na(J403_weights)] <- 0

    ###

    FundRet <- fund %>%
        select(date, Tickers, Return) %>%
        spread(Tickers, Return)

    FundRet[is.na(FundRet)] <- 0

    FundRet_xts <- FundRet %>%
        tbl_xts() # to get it into the correct format for packages.

    ###

    J203_RetPort <- rmsfuns::Safe_Return.portfolio(FundRet_xts,
                                                   weights = J203_weights,
                                                   lag_weights = TRUE,
                                                   contribution = TRUE,
                                                   verbose = TRUE,
                                                   value = 1,
                                                   geometric = TRUE)

    J403_RetPort <- rmsfuns::Safe_Return.portfolio(R = FundRet_xts,
                                                   weights = J403_weights,
                                                   lag_weights = TRUE,
                                                   contribution = TRUE,
                                                   verbose = TRUE,
                                                   value = 1,
                                                   geometric = TRUE)

    # Clean and save portfolio returns and weights:

    J203Cont <- J203_RetPort$"contribution" %>%
        xts_tbl() %>%
        mutate(date = lag(date),
               date = coalesce(date, index(J203_weights)[1]) )

    J203BPWeight <- J203_RetPort$"BOP.Weight" %>%
        xts_tbl() %>%
        mutate(date = lag(date),
               date = coalesce(date, index(J203_weights)[1]) )

    J203BPValue <- J203_RetPort$"BOP.Value" %>%
        xts_tbl() %>%
        mutate(date = lag(date),
               date = coalesce(date, index(J203_weights)[1]) )

    # Clean and save portfolio returns and weights:

    J403Cont <- J403_RetPort$"contribution" %>%
        xts_tbl() %>%
        mutate(date = lag(date),
               date = coalesce(date, index(J403_weights)[1]) )

    J403BPWeight <- J403_RetPort$"BOP.Weight" %>%
        xts_tbl() %>%
        mutate(date = lag(date),
               date = coalesce(date, index(J403_weights)[1]) )

    J403BPValue <- J403_RetPort$"BOP.Value" %>%
        xts_tbl() %>%
        mutate(date = lag(date),
               date = coalesce(date, index(J403_weights)[1]) )

    # This procedure below I essentially got from the practical.

    names(J203Cont) <- c("date", names(J203_RetPort$"contribution"))
    names(J203BPWeight) <- c("date", names(J203_RetPort$"BOP.Weight"))
    names(J203BPValue) <- c("date", names(J203_RetPort$"BOP.Value"))
    names(J403Cont) <- c("date", names(J403_RetPort$"contribution"))
    names(J403BPWeight) <- c("date", names(J403_RetPort$"BOP.Weight"))
    names(J403BPValue) <- c("date", names(J403_RetPort$"BOP.Value"))

    # Bind all together:

    df_port_return_J203 <-
        left_join(fund %>% select(date, Tickers, Return),
                  J203BPWeight %>% gather(Tickers, weight, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(.,
                  J203BPValue %>% gather(Tickers, value_held, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(.,
                  J203Cont %>% gather(Tickers, Contribution, -date),
                  by = c("date", "Tickers"))

    df_port_return_J403 <-
        left_join(fund %>% select(date, Tickers, Return),
                  J403BPWeight %>% gather(Tickers, weight, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(.,
                  J403BPValue %>% gather(Tickers, value_held, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(.,
                  J403Cont %>% gather(Tickers, Contribution, -date),
                  by = c("date", "Tickers"))

    # Calculate Rolling Portfolio Returns:

    df_Portf_J203 <- df_port_return_J203 %>%
        group_by(date) %>%
        summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>%
        filter(PortfolioReturn != 0)

    df_Portf_J403 <- df_port_return_J403 %>%
        group_by(date) %>%
        summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>%
        filter(PortfolioReturn != 0)

    ###

    Output <- left_join(df_Portf_J203 %>% rename(J203 = PortfolioReturn),
                        df_Portf_J403 %>%  rename(J403 = PortfolioReturn),
                        by = "date") %>%
        pivot_longer(c("J203", "J403"),
                     names_to = "Methodod",
                     values_to = "Returns")

    Output

}