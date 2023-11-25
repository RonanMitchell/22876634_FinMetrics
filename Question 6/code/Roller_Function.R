Roller <- function(ReturnMatrix, DateVector, LookBack = 24, Amat, bvec){

    return_df_used <- ReturnMatrix %>% filter(date >= DateVector %m-% months(LookBack))

    if(return_df_used %>% nrow() < LookBack) return(NULL)

    RetMatDateless <- data.matrix(ReturnMatrix[, -1])

    # Sigma and Mu

    HTT <- fitHeavyTail::fit_mvt(RetMatDateless)
    mu <- HTT$mu
    Sigma <- HTT$cov

    Sigma <- as.matrix( Matrix::nearPD(Sigma)$mat)

    # Fit optimisation function

    Weights <- left_join(
        Optimisation(type = "mv", mu, Sigma, bvec, Amat, printmsg = F),
        Optimisation(type = "minvol", mu, Sigma, bvec, Amat, printmsg = F),
        by = "stocks") %>%
        left_join(., Optimisation(type = "maxdecor", mu, Sigma, bvec, Amat, printmsg = F),
                  by = "stocks") %>%
        left_join(., Optimisation(type = "sharpe", mu, Sigma, bvec, Amat, printmsg = F),
                  by = "stocks") %>%
        mutate(date = DateVector , Look_Back_Period = LookBack)

}