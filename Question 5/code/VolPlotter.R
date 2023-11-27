VolPlotter <- function(data = ZARRet,
                       fit = garch_fit){

    sigma <- sigma(fit) %>%
        xts_tbl()
    colnames(sigma) <- c("date", "sigma")
    sigma <- sigma %>% mutate(Date = as.Date(date))

    Plotdata <- data %>%
        mutate(Returns = dlogret,
               Returns_Sqd = dlogret^2,
               Returns_Abs = abs(dlogret)) %>%
        pivot_longer(c("Returns",
                       "Returns_Sqd",
                       "Returns_Abs"),
                     names_to = "ReturnType",
                     values_to = "Returns")

    gg <-

        ggplot() +
        geom_line(data = Plotdata %>% filter(ReturnType == "Returns_Sqd") %>% select(Date, Returns) %>%

                      unique() %>% mutate(Returns = sqrt(Returns)), aes(x = Date, y = Returns), color = "blue4", size = 1, alpha = 0.8) +

        geom_line(data = sigma,
                  aes(x = Date, y = sigma),
                  color = "red4",
                  size = 1,
                  alpha = 0.8) +
        geom_ribbon(data = sigma, aes(x = Date,
                                      ymin = 0,
                                      ymax = sigma),
                    fill = "red4",
                    alpha = 0.8) +
        labs(title = "Comparison with Garch", x = "", y = "") +
        fmxdat::theme_fmx(title = ggpts(25))
    fmxdat::finplot(gg, y.pct = T, y.pct_acc = 1)

}