Selection_foo <- function(data = ZARRet){

    data <- data %>% select(Date, dlogret) %>%  tbl_xts()

    models = 1:4

    model.list = list()

    for (i in models) {

        garchfit = ugarchspec(
            variance.model = list(model =
                             c("sGARCH","gjrGARCH","eGARCH","apARCH")[i],
                             garchOrder = c(1, 1)),

            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),

            distribution.model =
                c("norm", "snorm", "std", "sstd",
                  "ged", "sged", "nig", "ghyp", "jsu")[1])

        garchfit1 = ugarchfit(spec = garchfit,data=as.numeric(data))

        model.list[[i]] = garchfit1
    }

    names(model.list) <- c("sGARCH","gjrGARCH","eGARCH","apARCH")

    fit.mat = sapply(model.list, infocriteria)

    rownames(fit.mat) = rownames(infocriteria(model.list[[1]]))

    fit.mat

}