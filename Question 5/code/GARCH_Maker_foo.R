GARCH_Maker_foo <- function(data = ZARRet, model = "sGARCH"){

    data <- data %>% select(Date, dlogret) %>%  tbl_xts()

    gjrgarch11 = ugarchspec(variance.model = list(model = model,

                                                  garchOrder = c(1, 1)),

                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),

                            distribution.model = c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp", "jsu")[3])

    garchfit2 = ugarchfit(spec = gjrgarch11, data = as.matrix(data))

    garchfit2

}