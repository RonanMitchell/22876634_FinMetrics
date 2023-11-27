The goal of this README is to give a clear explanation behind the code I
provide in order to perform the Financial Econometrics examination, as
well as the thought processes which lead me to the specific code
selected or implemented. If you would like to see the write up for each
question, go see the individual PDFs for the questions in their
respective folders. This README is only a code explanation within the
chunks.

``` r
rm(list = ls()) # Clean your environment:

gc() 
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 454475 24.3     976378 52.2   644242 34.5
    ## Vcells 822149  6.3    8388608 64.0  1634746 12.5

``` r
###

tinytex::install_tinytex(force = TRUE)
```

    ## tlmgr install elsarticle

``` r
options(repos = "https://cran.mirror.ac.za/")

###

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 5, fig.pos="H", fig.pos = 'H')

###

library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.3

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(dplyr)

library(fmxdat)

library(rportfolios)
```

    ## Loading required package: truncdist

    ## Warning: package 'truncdist' was built under R version 4.2.3

    ## Loading required package: stats4
    ## Loading required package: evd

    ## Warning: package 'evd' was built under R version 4.2.3

``` r
library(PerformanceAnalytics)
```

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 4.2.3

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.2.3

    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric
    ## 
    ## 
    ## ######################### Warning from 'xts' package ##########################
    ## #                                                                             #
    ## # The dplyr lag() function breaks how base R's lag() function is supposed to  #
    ## # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
    ## # source() into this session won't work correctly.                            #
    ## #                                                                             #
    ## # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
    ## # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
    ## # dplyr from breaking base R's lag() function.                                #
    ## #                                                                             #
    ## # Code in packages is not affected. It's protected by R's namespace mechanism #
    ## # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
    ## #                                                                             #
    ## ###############################################################################
    ## 
    ## Attaching package: 'xts'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last
    ## 
    ## 
    ## Attaching package: 'PerformanceAnalytics'
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

``` r
library(RcppRoll)
```

    ## Warning: package 'RcppRoll' was built under R version 4.2.3

``` r
library(tbl2xts)

library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 4.2.3

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(cowplot)
```

    ## Warning: package 'cowplot' was built under R version 4.2.3

    ## 
    ## Attaching package: 'cowplot'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
library(tbl2xts)

library(knitr)
```

    ## Warning: package 'knitr' was built under R version 4.2.3

``` r
library(fitHeavyTail)
```

    ## Warning: package 'fitHeavyTail' was built under R version 4.2.3

``` r
library(zoo)

pacman::p_load(sugrrants, 
               rugarch)
```

# Question 1

``` r
# Importing data

rm(list = ls())

ASISA <- read_rds("data/ASISA_Rets.rds") %>% 
    filter(date >= as.Date("2010-01-31"))

BM <- read_rds("data/Capped_SWIX.rds") %>% 
    filter(date >= as.Date("2010-01-31")) %>% 
    select(-Tickers)

AIFund <- read_rds("data/AI_Max_Fund.rds") %>% 
    filter(date >= as.Date("2010-01-31"))

# Post global financial crisis. 
```

``` r
# Question exploration

# Too many funds in unique(ASISA$Fund) to  meaningfully consider. 

# I am going to select the best and worst performing index and active funds separately. 

# Not all funds have the same starting date as my AI... 

# I am selecting only funds that at least have a minimum date of 2010 and must have a maximum date of 2023. This excludes funds that do not exist over the entire sample period. 

ASISA <-
  ASISA %>%
  group_by(Fund) %>%
  filter(min(date) == as.Date("2010-01-31") & 
             max(date) == as.Date("2023-09-30")) %>%
  ungroup()

# Which funds perform best?

# Best Performing fund that is an index: A235
       # 0.00903

ASISA %>%
  filter(Index == "Yes") %>%
  group_by(Fund) %>%
  summarise(avg_returns = mean(Returns, na.rm = TRUE)) %>%
  arrange(desc(avg_returns)) %>%
  slice(1)
```

    ## # A tibble: 1 × 2
    ##   Fund  avg_returns
    ##   <chr>       <dbl>
    ## 1 A235      0.00903

``` r
# Worst performing Index Fund: R174
        # -0.00786

ASISA %>%
  filter(Index == "Yes") %>%
  group_by(Fund) %>%
  summarise(avg_returns = mean(Returns, na.rm = TRUE)) %>%
  arrange(avg_returns) %>%
  slice(1)
```

    ## # A tibble: 1 × 2
    ##   Fund  avg_returns
    ##   <chr>       <dbl>
    ## 1 R174      0.00786

``` r
# Best Performing Active Fund: S1007 (also best performing fund in general)
       # 0.0148

ASISA %>%
  filter(Index == "No") %>%
  group_by(Fund) %>%
  summarise(avg_returns = mean(Returns, na.rm = TRUE)) %>%
  arrange(desc(avg_returns)) %>%
  slice(1)
```

    ## # A tibble: 1 × 2
    ##   Fund  avg_returns
    ##   <chr>       <dbl>
    ## 1 S1007      0.0148

``` r
# Worst performing Active Fund: V412 (and worst overall)
        # 0.00312

ASISA %>%
  filter(Index == "No") %>%
  group_by(Fund) %>%
  summarise(avg_returns = mean(Returns, na.rm = TRUE)) %>%
  arrange(avg_returns) %>%
  slice(1)
```

    ## # A tibble: 1 × 2
    ##   Fund  avg_returns
    ##   <chr>       <dbl>
    ## 1 V412      0.00312

``` r
# Selection: 

Selected_Funds <- c("V412","S1007", "R174", "A235")

ASISA <- ASISA %>% 
    filter(Fund %in% Selected_Funds)

# I now only consider funds that have performed the best and worst, to frame the comparison better to my AI fund. 
```

``` r
library(ggplot2)

### I want to plot all return distributions on the same set of axes. 

wide_format_ASISA <- ASISA %>% 
    select(c(Fund, Returns, date)) %>%
    spread(Fund, Returns)

joined_data <- left_join(BM, AIFund,
                         by = "date") %>%
               left_join(., wide_format_ASISA,
                         by = "date")

joined_data$BM <- joined_data$Returns 

###

fee1 <- 0.025/12 # 2.5% fee per month annualized. (active managers)

fee2 <- 0.01/12 # 1% fee per month annualised. (index funds)
```

``` r
# Distribution of active fund returns against my AI Fund. 

ggplot(joined_data) +
  geom_density(aes(x = (S1007 - fee1), 
                   fill = "S1007"), 
                   alpha = 0.45) +
  geom_density(aes(x = AI_Fund, 
                   fill = "AI Fund"), 
                   alpha = 0.65) +
  geom_density(aes(x = (V412 - fee1), 
                   fill = "V412"), 
                   alpha = 0.45) +
  labs(title = "AI vs Active Managers",
       y = "",
       x = "") +
  theme_fmx() +
  scale_fill_manual(values = c("yellow", "blue", "blue4"), 
                    name = "Variables") +
  guides(fill = guide_legend(title = "Variables")) +
    xlim(c(-0.15,0.2)) +
    theme_fmx(title.size = ggpts(25), 
              CustomCaption = F)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-4-1.png" alt=" \label{Figure1.1}"  />
<p class="caption">
</p>

``` r
# Distribution of index fund returns against my AI Fund. 

ggplot(joined_data) +
  geom_density(aes(x = (R174 - fee2), 
                   fill = "R174"), 
                   alpha = 0.45) +
  geom_density(aes(x = AI_Fund, 
                   fill = "AI Fund"), 
                   alpha = 0.65) +
  geom_density(aes(x = (A235 - fee2), 
                   fill = "A235"), 
                   alpha = 0.45) +
  labs(title = "AI vs Index Funds",
       y = "",
       x = "") +
  theme_fmx() +
  scale_fill_manual(values = c("yellow", "blue", "blue4"), 
                    name = "Variables") +
  guides(fill = guide_legend(title = "Variables")) +
    xlim(c(-0.15,0.2)) +
    theme_fmx(title.size = ggpts(25), 
              CustomCaption = F)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-5-1.png" alt=" \label{Figure1.2}"  />
<p class="caption">
</p>

``` r
# Distribution of benchmark returns against my AI Fund. 

ggplot(joined_data) +
  geom_density(aes(x = AI_Fund, 
                   fill = "AI Fund"), 
                   alpha = 0.65) +
  geom_density(aes(x = BM, 
                   fill = "Benchmark"),
                   alpha = 0.45) +
  labs(title = "AI vs Capped SWIX",
       y = "",
       x = "") +
  theme_fmx() +
  scale_fill_manual(values = c("yellow", "blue", "blue4"), 
                    name = "Variables") +
  guides(fill = guide_legend(title = "Variables")) +
    xlim(c(-0.15,0.2)) +
    theme_fmx(title.size = ggpts(25), 
              CustomCaption = F)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-6-1.png" alt=" \label{Figure1.3}"  />
<p class="caption">
</p>

``` r
# Get data into the correct format. 

long_format_data <- joined_data %>% 
                    select(-Returns) %>%
                    pivot_longer(-date, 
                                 names_to = "Tickers", 
                                 values_to = "Returns")
```

``` r
# Cumulative returns of selected funds

long_format_data %>%  
arrange(date) %>%   
    group_by(Tickers) %>%   
    
mutate(Rets = coalesce(Returns, 0)) %>%   
mutate(CP = cumprod(1 + Rets)) %>%   
    
ungroup() %>%  
    
ggplot() + 
geom_line(aes(date, 
              CP, 
              color = Tickers), 
          alpha = 1, 
          size = 1) + 
    xlab("Date") +
    ylab("") +
labs(title = "Cumulative Returns of Selected Funds") + 
    theme_fmx(title.size = ggpts(20), 
              CustomCaption = F)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-8-1.png" alt=" \label{Figure1.4}"  />
<p class="caption">
</p>

``` r
# 3 year Rolling returns of selected funds. 

library(RcppRoll)

plotdf <- long_format_data %>%
    group_by(Tickers) %>% 
    
    mutate(
        
        RollRets = RcppRoll::roll_prod(1 + Returns, 
                                       36, 
                                       fill = NA, 
                                       align = "right")^(12/36) - 1
        
        ) %>% 
    group_by(date) %>% 
    filter(any(!is.na(RollRets))) %>%   
    ungroup()

g <- 
plotdf |>  
ggplot() + 
geom_line(aes(date, 
              RollRets, 
              color = Tickers), 
              alpha = 1, 
              size = 1) + 
labs(title = "Rolling 3-Year Returns", 
     subtitle = "", x = "", y = "", 
     caption = "") + 
theme_fmx(title.size = ggpts(20), 
          CustomCaption = F) + 
    
fmx_cols()

finplot(g, x.date.dist = "1 year", 
        x.date.type = "%Y", 
        x.vert = T, 
        y.pct = T, 
        y.pct_acc = 1)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-9-1.png" alt=" \label{Figure1.5}"  />
<p class="caption">
</p>

``` r
# rolling standard deviation of selected funds. 

plot_dlog <- long_format_data %>% 
  mutate(YM = format(date, "%Y%B")) %>%   
  arrange(date) %>%   
  group_by(Tickers, YM) %>% 
  filter(date == last(date)) %>%   
  group_by(Tickers) %>%  
  mutate(
      
      RollSD = RcppRoll::roll_sd(
          
          1 + Returns, 36, fill = NA, align = "right") * sqrt(12)
      
      ) %>%   
filter(!is.na(RollSD))

g <- 
plot_dlog %>%   
ggplot() + 
geom_line(aes(date,
              RollSD, 
              color = Tickers),
              alpha = 1, 
              size = 1) + 
labs(title = "Rolling Standard Deviation") + 
    theme_fmx(title.size = ggpts(20),
              CustomCaption = F) + 
    
fmx_cols()

finplot(g, x.date.dist = "1 year", 
        x.date.type = "%Y",
        x.vert = T, 
        y.pct = T,
        y.pct_acc = 1)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-10-1.png" alt=" \label{Figure1.6}"  />
<p class="caption">
</p>

# Question 2

``` r
rm(list = ls())

gc()
```

    ##           used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells 2998503 160.2    6101050 325.9  6101050 325.9
    ## Vcells 4961232  37.9   10146329  77.5  8388598  64.0

``` r
Indexes <- read_rds("data/Cncy_Hedge_Assets.rds")

# ALBI is bonds (local)
# J433 is equities (local)
# bbg agg is Bloomberg bond aggregation. (foreign)
# msci acwi is equities. (foreign)

ZAR <- read_rds("data/Monthly_zar.rds") %>% 
    filter(date >= as.Date(
        first(
            Indexes$date))) %>%
    select(-Tickers)

### How much should I hold in this portfolio given that I need a 60/40 split between equity/bonds and a 70/30 split between local/global split?

# for ALBI: 0.4 * 0.7 = 0.28
# for J433: 0.6 * 0.7 = 0.42
# for BBG: 0.4 * 0.3 = 0.12
# for MSCI: 0.6 * 0.3 = 0.18

# This sums to one. 

Weights <- c(0.18, 0.12, 0.42, 0.28)

Weights_Indexed <- Weights * (Indexes %>% select(-date))

Weights_Indexed$date <- Indexes$date

Weights_Indexed <- Weights_Indexed %>% slice(-1)

###

# Add a portfolio return column:

Weights_Indexed <- Weights_Indexed %>% 
    mutate(PortRet = rowSums(Weights_Indexed %>% select(-date)))

ZAR <- ZAR %>% 
    mutate(ZarRet = value/lag(value) - 1) %>% 
    slice(-1)

###

RetData <- as.data.frame( cbind(Weights_Indexed$PortRet, ZAR$ZarRet)) %>% 
    rename(PortRet = V1) %>% 
    rename(ZARRet = V2) %>% 
    na.omit(.)
```

``` r
# Some calculations of %
# I continually changed the directions of these greater than signs. 

(RetData %>% 
    filter(PortRet > 0, ZARRet < 0) %>% 
    nrow(.))/nrow(RetData)*100
```

    ## [1] 43.79845

``` r
# 20.16% top right 
# 9.31% bottom left corner 
# 26.36% bottom right.
# 43.79% top left. 
```

``` r
# Trying my best to perfectly replicate the findings in the study, and to make the graph look as good as possible. 

ggplot(RetData, 
       aes(x = ZARRet, y = PortRet)) +
    annotate("rect", # annotate adds squares
             xmin = -Inf, 
             xmax = 0, 
             ymin = -Inf, 
             ymax = 0, 
             fill = "red", 
             alpha = 0.18) +
    annotate("rect", 
             xmin = 0, 
             xmax = Inf, 
             ymin = -Inf, 
             ymax = 0, 
             fill = "lightblue", 
             alpha = 0.2) +
    annotate("rect", 
             xmin = -Inf,
             xmax = 0, 
             ymin = 0, 
             ymax = Inf,
             fill = "pink", 
             alpha = 0.2) +
    annotate("rect", 
             xmin = 0,
             xmax = Inf, 
             ymin = 0, 
             ymax = Inf, 
             fill = "blue", 
             alpha = 0.18) +
    geom_point(color = "steelblue4", 
               alpha = 0.45) +
    geom_smooth(method = "lm", # Line of best fit
                se = TRUE, 
                color = "grey") + 
  labs(title = "Scatter Plot of Return Correlation",
       x = "Portfolio",
       y = "ZAR/USD") +
    geom_vline(xintercept = -0.025, # 2.5% cost of hedging (based on origin)
               linetype = "dotted", 
               color = "black", 
               alpha = 0.5, 
               size = 1) +
    xlim(c(-0.2, 0.2)) +  # same as original
    ylim(c(-0.2, 0.2)) +  # same as original
    geom_label(aes(x = -0.1, 
                   y = 0.1, 
                   label = "43.79%"), # labeling the % calculated per area
               fill = "white", 
               color = "black") +
    geom_label(aes(x = 0.1, 
                   y = 0.1, 
                   label = "20.16%"), 
               fill = "white", 
               color = "black") +
    geom_label(aes(x = -0.1,
                   y = -0.1, 
                   label = "9.31%"), 
               fill = "white", 
               color = "black") +
  geom_label(aes(x = 0.1, 
                 y = -0.1, 
                 label = "26.36"), 
             fill = "white", 
             color = "black") +
  theme_bw() # this looked best for this plot. 
```

<img src="README_files/figure-markdown_github/unnamed-chunk-13-1.png" alt=" \label{Figure2.1}"  />
<p class="caption">
</p>

``` r
# Could not add densities at the top and side of ggplot the way original paper did, so I added them separately instead. 

ggplot(RetData, 
       aes(x = ZARRet, 
           fill = "ZAR/USD")) +
  geom_density(alpha = 0.5) +
  geom_density(aes(x = PortRet, 
                   fill = "Portfolio"), 
                   alpha = 0.5) +
  labs(title = "Distribution of Returns",
       x = "",
       y = "") +
  scale_fill_manual(name = "",
                    values = c("ZAR/USD" = "green",
                               "Portfolio" = "blue")) +
    xlim(c(-0.1,0.15)) +
  theme(legend.position = "bottom")
```

<img src="README_files/figure-markdown_github/unnamed-chunk-14-1.png" alt=" \label{Figure2.2}"  />
<p class="caption">
</p>

``` r
# restart for organisation purposes.

rm(list = ls())

gc()
```

    ##           used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells 3048730 162.9    6101050 325.9  6101050 325.9
    ## Vcells 5041020  38.5   12255594  93.6  8388598  64.0

``` r
###

Indexes <- read_rds("data/Cncy_Hedge_Assets.rds") 

ZAR <- read_rds("data/Monthly_zar.rds") %>% 
    filter(date >= as.Date(first(Indexes$date))) %>%
    select(-Tickers)

Weights <- c(0.18, 0.12, 0.42, 0.28)

### Plain ole re balancing portfolio.

RebalancedData <- Return.portfolio(
    Indexes, weights = Weights, rebalance_on = "quarters") %>% 
    xts_tbl(.) %>% 
    mutate(RollSD = RcppRoll::roll_sd(1 + portfolio.returns, 
                                      36, 
                                      fill = NA, 
                                      align = "right") * sqrt(12)) %>%  
                                      filter(!is.na(RollSD))

### Portfolio Including Rand

ZAR <- ZAR %>% 
    mutate(ZarRet = value/lag(value) - 1) %>% 
    filter(date >= "2005-01-31") 

RandRebalance <- RebalancedData %>% 
    mutate(Hedged = (0.9*portfolio.returns + 0.1*ZAR$ZarRet))

# I assume this is incredibly dubious, but I could not figure out another way to argue that I am holding a portfolio that re balances every quarter, and is exposed to 10% of the Rand, so I did this weighted average. 

RandRebalance <- RandRebalance %>% 
    mutate(RollSDHedged = RcppRoll::roll_sd(1 + Hedged, 
                                      36, 
                                      fill = NA, 
                                      align = "right") * sqrt(12)) %>%  
                                      filter(!is.na(RollSDHedged)) %>% 
    select(date, 
           RollSD,
           RollSDHedged) 

###

# rolling standard deviation

g <- RandRebalance %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = RollSD, color = "Excluding Rand"), size = 1) +
  geom_line(aes(y = RollSDHedged, color = "Including Rand"), size = 1) +
  labs(title = "Rolling Volatility",
       x = "Date",
       y = "Standard Deviation") +
  scale_color_manual(values = c("Excluding Rand" = "red", 
                                "Including Rand" = "red4")) +
    theme_fmx(title.size = ggpts(30),
              CustomCaption = F)
```

``` r
finplot(g, 
        x.date.dist = "1 year", 
        x.date.type = "%Y", 
        x.vert = T, 
        y.pct = T, 
        y.pct_acc = 1)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-16-1.png" alt=" \label{Figure2.3}"  />
<p class="caption">
</p>

# Question 3

``` r
rm(list = ls())

gc() 
```

    ##           used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells 3063864 163.7    6101050 325.9  6101050 325.9
    ## Vcells 5073188  38.8   12255594  93.6  8388598  64.0

``` r
# Loading in data

ALSI <- read_rds("data/ALSI.rds")

RebDays <- read_rds("data/Rebalance_days.rds") %>% 
    filter(date >= first(ALSI$date) & 
               Date_Type == "Effective Date" &
               date <= last(ALSI$date))

pacman::p_load(tbl2xts, PerformanceAnalytics)

###

# Both of these functions essentially wrap elements of the practicals offered by Nico inside functions, so that I can easily answer the question at hand. It is not necessarily "unique" code, but it works. You can go to the file locations to examine the functions in greater detail. 

source('Question 3/code/PortReturn.R')

source('Question 3/code/Proportional_Cap_Foo.R')
```

``` r
# Simply want to find high volatility years for the ZAR and add them to the ggplots later. I have chosen not to stratify; time pressure is a factor but also, I think that the graph at the end of this will be pretty neat. 

ZAR <- read_rds("data/Monthly_zar.rds") %>%
    select(-Tickers) %>% 
    mutate(Return = value/lag(value) - 1) %>% 
    filter(date >= first(ALSI$date)) %>% 
    select(-c(value)) %>% 
    na.omit()

ZARSD <- ZAR %>% 
  
  mutate(Year = format(date, "%Y")) %>% 
  
  group_by(Year) %>% summarise(SD = sd(Return)*sqrt(12)) %>% 
  
  mutate(TopQtile = quantile(SD, 0.67),
         BotQtile = quantile(SD, 0.33))

Hi_Vol <- ZARSD %>% filter(SD > TopQtile) %>% pull(Year)

# 2016, 2018, 2020, and 2023 are high volatility years. 

Low_Vol <- ZARSD %>% filter(SD < BotQtile) %>% pull(Year)

# 2014, 2015, 2017, 2021 are low volatility years. 
```

``` r
# Even though I only use it once, i decided that a PortReturn function would be best suited to this question because of how many times it had to be rerun and edited. Essentially, a function that takes a data set and performs large parts of the portfolio returns process outlined in the practical about portfolio creation and returns. 

Indexes <- ALSI %>%  pull(Index_Name) %>% na.omit(.) %>%  unique()

IndRet <- list()

# I want to estimate rolling returns instead, because I think for such similar funds, this will allow us to see a fair representation of their returns. 

for(i in 1:length(Indexes)){
    
    # use the PortReturn() function:
 
    IndRet[[i]] <- PortReturn(ALSI,index = Indexes[i]) %>% 
        group_by(Methodod) %>% 
    mutate(RollRets = RcppRoll::roll_prod(1 + Returns, 
                                          36, 
                                          fill = NA, 
                                          align = "right")^(12/36) - 1) %>%   
    group_by(date) %>%   
    filter(any(!is.na(RollRets))) %>%
    mutate(Index = Indexes[i])
}

names(IndRet) <- Indexes

indices_roll_ret <- rbind(IndRet[[1]],
                          IndRet[[2]], 
                          IndRet[[3]]) %>% 
    arrange(date)
    
### Make Plots

    # Earlier, I created the vectors of high volatility and low volatility dates of the rand. These will now be used to construct ranges that can further be used in a ggplot object to create shaded regions of high volatility and low volatility. 

# Ranges: 

   # High Vol

HV1 <- data.frame(
  xmin = as.Date("2016-01-01"),
  xmax = as.Date("2016-12-01"),
  ymin = -Inf,
  ymax = Inf
)

HV2 <- data.frame(
  xmin = as.Date("2018-01-01"),
  xmax = as.Date("2018-12-01"),
  ymin = -Inf,
  ymax = Inf
)

HV3 <- data.frame(
  xmin = as.Date("2020-01-01"),
  xmax = as.Date("2020-12-01"),
  ymin = -Inf,
  ymax = Inf
)

HV4 <- data.frame(
  xmin = as.Date("2023-01-01"),
  xmax = as.Date("2023-12-01"),
  ymin = -Inf,
  ymax = Inf
)

# Low vol 

LV1 <- data.frame(
  xmin = as.Date("2014-01-01"),
  xmax = as.Date("2014-12-01"),
  ymin = -Inf,
  ymax = Inf
)

LV2 <- data.frame(
  xmin = as.Date("2015-01-01"),
  xmax = as.Date("2015-12-01"),
  ymin = -Inf,
  ymax = Inf
)

LV3 <- data.frame(
  xmin = as.Date("2017-01-01"),
  xmax = as.Date("2017-12-01"),
  ymin = -Inf,
  ymax = Inf
)

LV4 <- data.frame(
  xmin = as.Date("2021-01-01"),
  xmax = as.Date("2021-12-01"),
  ymin = -Inf,
  ymax = Inf
)

###

alpha = 0.2 # so that all shaded regions are consistent. 
```

``` r
# I should have made a function.

# Large cap stocks

Large <- indices_roll_ret %>% 
     filter(Index == "Large_Caps") %>% 
        ggplot() +
    geom_rect(data = HV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = LV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_line(aes(date, RollRets, colour = Methodod), 
                  alpha = 0.8,
                  size = 0.5) + 
     fmxdat::fmx_cols() + 
        labs(title = "Large Cap Shares", 
             y = "",
             x = "") +
        theme_classic()

###

# Mid cap stocks

Mid <- indices_roll_ret %>% 
     filter(Index == "Mid_Caps") %>% 
        ggplot() +
    geom_rect(data = HV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = LV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_line(aes(date, RollRets, colour = Methodod), 
                  alpha = 0.8,
                  size = 0.5) + 
     fmxdat::fmx_cols() + 
        labs(title = "Mid Cap Shares", 
             y = "",
             x = "") +
        theme_classic()

###

# Small Cap shares 

Small <- indices_roll_ret %>% 
     filter(Index == "Small_Caps") %>% 
        ggplot() +
    geom_rect(data = HV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = LV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_line(aes(date, RollRets, colour = Methodod), 
                  alpha = 0.8,
                  size = 0.5) + 
     fmxdat::fmx_cols() + 
        labs(title = "Small Cap Shares", 
             y = "",
             x = "") +
        theme_classic()
```

``` r
# large cap stocks plot

finplot(Large, 
        x.date.dist = "1 year", 
        x.date.type = "%Y", 
        x.vert = T, 
        y.pct = T, 
        y.pct_acc = 1)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-21-1.png" alt="Large Cap Stocks \label{Figure3.1}"  />
<p class="caption">
Large Cap Stocks
</p>

``` r
# mid cap stocks plot

finplot(Mid, 
        x.date.dist = "1 year", 
        x.date.type = "%Y", 
        x.vert = T, 
        y.pct = T, 
        y.pct_acc = 1)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-22-1.png" alt="Mid Cap Stocks \label{Figure3.2}"  />
<p class="caption">
Mid Cap Stocks
</p>

``` r
# small cap stocks plot

finplot(Small, 
        x.date.dist = "1 year", 
        x.date.type = "%Y", 
        x.vert = T, 
        y.pct = T, 
        y.pct_acc = 1)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-23-1.png" alt="Small Cap Stocks \label{Figure3.3}"  />
<p class="caption">
Small Cap Stocks
</p>

``` r
# capping the portfolio. 

rebALSI <- ALSI %>% 
filter(date %in% RebDays$date) %>% 
mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    select(date, Tickers, Return, J203, RebalanceTime) %>% 
    rename(weight = J203) %>% 
    mutate(weight = coalesce(weight , 0))
  
Capped <- rebALSI %>% 
    group_split(RebalanceTime) %>% 
    map_df(~Proportional_Cap_Foo(., W_Cap = 0.1) ) %>% # weight specified
    select(-RebalanceTime)
 
ALSI_wts <- Capped %>% 
    tbl_xts(cols_to_xts = weight, 
            spread_by = Tickers)

ALSI_rts <- ALSI %>% 
    filter(Tickers %in% unique(Capped$Tickers)) %>% 
    tbl_xts(cols_to_xts = Return, 
            spread_by = Tickers)

ALSI_wts[is.na(ALSI_wts)] <- 0

ALSI_rts[is.na(ALSI_rts)] <- 0

ALSICapped <- rmsfuns::Safe_Return.portfolio(R = ALSI_rts, 
                                             weights = ALSI_wts, 
                                             lag_weights = T) %>% 
    xts_tbl() %>% 
    rename(ALSI = portfolio.returns)

# Capped Portfolio - SWIX

RebSWIX <- ALSI %>% 
filter(date %in% RebDays$date) %>% 
mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    select(date, Tickers, Return, J403, RebalanceTime) %>% 
    rename(weight = J403) %>% 
    mutate(weight = coalesce(weight , 0))
  
# Apply Proportional_Cap_Foo to ALSI to get capped return for cap of 5%

Capped <- RebSWIX %>% 
    group_split(RebalanceTime) %>% 
    map_df(~Proportional_Cap_Foo(., W_Cap = 0.05) ) %>% # weight specified
    select(-RebalanceTime)
 
SWIX_wts <- Capped %>% 
    tbl_xts(cols_to_xts = weight, 
            spread_by = Tickers)

SWIX_rts <- ALSI %>% 
    filter(Tickers %in% unique(Capped$Tickers)) %>% 
    tbl_xts(cols_to_xts = Return,
            spread_by = Tickers)

SWIX_wts[is.na(SWIX_wts)] <- 0

SWIX_rts[is.na(SWIX_rts)] <- 0

SWIX_capped <- rmsfuns::Safe_Return.portfolio(R = SWIX_rts, 
                                              weights = SWIX_wts, 
                                              lag_weights = T) %>% 
    xts_tbl() %>% 
    rename(SWIX = portfolio.returns)

# Combine

CapIndex <- left_join(ALSICapped, SWIX_capped, by = "date") %>% 
    pivot_longer(c("ALSI", "SWIX"), 
                 names_to = "Method", 
                 values_to = "returns")

# Uncapped Return - ALSI

ALSI_wts <- ALSI %>% 
filter(date %in% RebDays$date) %>% 
mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    rename(weight = J203) %>% 
    mutate(weight = coalesce(weight , 0)) %>% 
    select(date, Tickers, Return, weight, RebalanceTime) %>% 
     tbl_xts(cols_to_xts = weight, 
             spread_by = Tickers)

ALSI_wts[is.na(ALSI_wts)] <- 0

ALSI_rts[is.na(ALSI_rts)] <- 0

ALSICapped <- rmsfuns::Safe_Return.portfolio(R = ALSI_rts, weights = ALSI_wts, 
    lag_weights = T) %>% 
xts_tbl() %>% 
rename(ALSI = portfolio.returns)

# Uncapped Return - SWIX
 
SWIX_wts <- ALSI %>% 
filter(date %in% RebDays$date) %>% 
mutate(RebalanceTime = format(date, "%Y%B")) %>% 
    rename(weight = J403) %>% 
    mutate(weight = coalesce(weight , 0)) %>% 
    select(date, Tickers, Return, weight, RebalanceTime) %>% 
     tbl_xts(cols_to_xts = weight, spread_by = Tickers)

SWIX_wts[is.na(SWIX_wts)] <- 0

SWIX_rts[is.na(SWIX_rts)] <- 0

SWIX_capped <- rmsfuns::Safe_Return.portfolio(R = SWIX_rts, 
                                              weights = SWIX_wts, 
                                              lag_weights = T) %>% 
    xts_tbl() %>% 
    rename(SWIX = portfolio.returns)

# Combine

AlsiSwix <- left_join(ALSICapped, 
                      SWIX_capped, by = "date") %>% 
    pivot_longer(c("ALSI", "SWIX"), 
                 names_to = "Method", 
                 values_to = "Returns")
```

``` r
# Creating plots 

## capped indexes 

g1 <- CapIndex %>% 
    group_by(Method) %>%
    mutate(Idx = cumprod(1 + returns)) %>% 

     ggplot() + 
    
    geom_rect(data = HV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = LV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    
geom_line(aes(date, 
              Idx, 
              colour = Method), 
          alpha = 0.9) + 
labs(subtitle = "ALSI (10%) & SWIX (5%)", 
     x = "", 
     y = "") + 
    fmx_cols() +
fmxdat::theme_fmx()

## uncapped indexes

g2 <- AlsiSwix %>% 
    group_by(Method) %>%
    mutate(Idx = cumprod(1 + Returns)) %>% 

     ggplot() + 
    
    geom_rect(data = HV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = HV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "red4", 
              alpha = alpha) +
    geom_rect(data = LV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    geom_rect(data = LV4, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green4", 
              alpha = alpha) +
    
geom_line(aes(date, Idx, colour = Method), alpha = 0.9) + 
labs(subtitle = "Uncapped (ALSI & SWIX)", 
     x = "", 
     y = "") +
fmxdat::theme_fmx()
```

``` r
g1
```

<img src="README_files/figure-markdown_github/unnamed-chunk-26-1.png" alt=" \label{Figure3.4}"  />
<p class="caption">
</p>

``` r
g2
```

<img src="README_files/figure-markdown_github/unnamed-chunk-27-1.png" alt=" \label{Figure3.5}"  />
<p class="caption">
</p>

# Question 4

``` r
rm(list = ls())

gc()
```

    ##           used  (Mb) gc trigger  (Mb) max used  (Mb)
    ## Ncells 3080925 164.6    6101050 325.9  6101050 325.9
    ## Vcells 5139239  39.3   26485863 202.1 33107328 252.6

``` r
###

Rets <- read_rds("data/ASISA_Rets.rds") %>%
  mutate(Fund = str_trim(Fund)) 

Flows <- read_rds("data/ASISA_Flows.rds") %>%
  mutate(Fund = str_trim(Fund)) %>%
  filter(Fund %in% unique(Rets$Fund)) 

# I only want funds that have existed over the entire time period. 

Data <- left_join(Flows, Rets, by = c("date", "Fund", "Index", "FoF")) %>% 
    na.omit() %>% 
    filter(Index == "No") %>% 
    group_by(Fund) %>%
    filter(min(date) == as.Date("2003-10-31") & 
             max(date) == as.Date("2023-09-30")) %>%
    ungroup()

rm(Flows) # neatness
rm(Rets)
```

``` r
# This code is quite hectic (for me at least). It: 

   # finds winners in a given three year time period, as well as finding the performance of those same funds over the next three years. This way, we can compare winners in an arbitrary time period (for all time periods) to their subsequent performance in the folliwng three years ( for all winners in all time periods) to see whether past return distributions are good indicators of future returns distributions. 

source("Question 4/code/WinnersComparison.R")

# Get unique dates from the original dataset.

unique_dates <- sort(unique(Data$date))

# Apply the function to generate plot data for each date range.

plot_data_list <- lapply(1:(length(unique_dates)-2), function(i) {
  start_date <- unique_dates[i]
  end_date <- unique_dates[i + 1]
  next_date <- unique_dates[i + 2]
  
  # Call the function and generate the plot data.
  
  WinnersComparison((Data %>% filter(Index == "No")), 
                    start_date,
                    end_date, 
                    next_date)
})

# Combine the plot data into a single data frame.

combined_plot_data <- do.call(rbind, plot_data_list)

# Create a density plot using ggplot for the combined data.

g1 <-
ggplot(combined_plot_data, aes(x = Returns, fill = Variable)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribution of Winners and Subsequent Performance",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Winners" = "lightblue", 
                               "Subsequent" = "red4")) +
     xlim(c(-0.1, 0.16)) +
    guides(fill = guide_legend(title = "")) +
  theme_bw()
```

``` r
# this function works exactly the same as the winners function, except it finds losers in the bottom quantile of performs. 

source("Question 4/code/LosersComparison.R")

# Repeat the Process. 

unique_dates <- sort(unique(Data$date))

plot_data_list <- lapply(1:(length(unique_dates)-2), function(i) {
  start_date <- unique_dates[i]
  end_date <- unique_dates[i + 1]
  next_date <- unique_dates[i + 2]
  
  # Call the function and generate the plot data.
  
  LosersComparison((Data %>% filter(Index == "No")), start_date, end_date, next_date)
})

combined_plot_data <- do.call(rbind, plot_data_list)

# Create a density plot using ggplot for the combined data.

g2 <-
ggplot(combined_plot_data, aes(x = Returns, fill = Variable)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribution of Losers and Subsequent Performance",
       x = "",
       y = "") +
  scale_fill_manual(values = c("Losers" = "red4", 
                               "Subsequent" = "lightblue")) +
     xlim(c(-0.15, 0.15)) +
    guides(fill = guide_legend(title = "")) +
  theme_bw()
```

``` r
g1
```

<img src="README_files/figure-markdown_github/unnamed-chunk-28-1.png" alt=" \label{Figure4.1}"  />
<p class="caption">
</p>

``` r
g2
```

<img src="README_files/figure-markdown_github/unnamed-chunk-29-1.png" alt=" \label{Figure4.2}"  />
<p class="caption">
</p>

``` r
rm(list = ls())

###

Rets <- read_rds("data/ASISA_Rets.rds") %>%
  mutate(Fund = str_trim(Fund)) 

Flows <- read_rds("data/ASISA_Flows.rds") %>%
  mutate(Fund = str_trim(Fund)) %>%
  filter(Fund %in% unique(Rets$Fund)) 

# I only want funds that have existed over the entire time period. 

Data <- left_join(Flows, Rets, by = c("date", "Fund", "Index", "FoF")) %>% 
    na.omit() %>% 
    filter(Index == "No") %>% # active managers
    group_by(Fund) %>%
    filter(min(date) == as.Date("2003-10-31") & 
             max(date) == as.Date("2023-09-30")) %>%
    ungroup()

rm(Rets) # neatness
rm(Flows)
```

``` r
### Re-finding the average returns and flows today, and isolating these so that I can plot them. 

DataAverage <- Data %>%
  arrange(Fund, date) %>%
  group_by(Fund) %>%
  mutate(AvgReturnsPast3Years = zoo::rollapply(Returns, width = 36, FUN = mean, align = "right", partial = TRUE)) %>%
  ungroup() %>%
  na.omit() %>% 
    mutate(Flows = Flows/10000) # to make correlation easy to plot nicely on the ggplot without horrible numbers on the y axis. 
```

``` r
# Plot of correlation between average three year returns and Flows TODAY. Line of best fit plotted and % of data present in each quadrant once again discussed. 

ggplot(DataAverage, aes(x = AvgReturnsPast3Years, y = Flows)) +
    geom_smooth(method = "lm", se = TRUE, color = "grey") + # line
    annotate("rect", 
             xmin = -Inf, 
             xmax = median(DataAverage$AvgReturnsPast3Years),# above median
             ymin = -Inf, 
             ymax = median(DataAverage$Flows), 
             fill = "red", 
             alpha = 0.18) +
    annotate("rect", 
             xmin = median(DataAverage$AvgReturnsPast3Years), 
             xmax = Inf, 
             ymin = -Inf, 
             ymax = median(DataAverage$Flows), 
             fill = "lightblue", 
             alpha = 0.2) +
    annotate("rect", 
             xmin = -Inf,
             xmax = median(DataAverage$AvgReturnsPast3Years), 
             ymin = median(DataAverage$Flows), 
             ymax = Inf,
             fill = "pink", 
             alpha = 0.2) +
    annotate("rect", 
             xmin = median(DataAverage$AvgReturnsPast3Years),
             xmax = Inf, 
             ymin = median(DataAverage$Flows), 
             ymax = Inf, 
             fill = "blue", 
             alpha = 0.18) +
  labs(title = "Correlation Between Flows and Returns",
       x = "Average Returns (3yr)",
       y = "Flows") +
    geom_label(aes(x = -0.025, 
                   y = 12500, 
                   label = "19.9%"), # labelling by % calculated 
               fill = "white", 
               color = "black") +
    geom_label(aes(x = 0.1, 
                   y = 12500, 
                   label = "25.54%"), 
               fill = "white", 
               color = "black") +
    geom_label(aes(x = -0.025,
                   y = -5000, 
                   label = "30.1%"), 
               fill = "white", 
               color = "black") +
  geom_label(aes(x = 0.1, 
                 y = -5000, 
                 label = "24.46%"), 
             fill = "white", 
             color = "black") +
  theme_bw()
```

<img src="README_files/figure-markdown_github/unnamed-chunk-31-1.png" alt=" \label{Figure4.3}"  />
<p class="caption">
</p>

# Question 5

``` r
rm(list = ls())

gc()
```

    ##           used  (Mb) gc trigger   (Mb) max used   (Mb)
    ## Ncells 3085712 164.8   30763664 1643.0 38454580 2053.7
    ## Vcells 5509359  42.1   55545639  423.8 59757965  456.0

``` r
# Everything is in dollars. 

cncy <- read_rds("data/currencies.rds") 

cncy_Carry <- read_rds("data/cncy_Carry.rds") # Deutche Bank??

cncy_value <- read_rds("data/cncy_value.rds") # no clue

cncy_IV <- read_rds("data/cncyIV.rds") 

bbdxy <- read_rds("data/bbdxy.rds")

# bbdxy includes Japan, Canada, China, US, EU, India, Australia... 

###

# Once again, these functions wrap elements of the practicals and Nico's recoommendations into a replicable process. I only use each one once but creating the functions allowed for easy testing and experimenting. 

source("Question 5/code/VolPlotter.R") # plotting cleaned volatility. 

source("Question 5/code/Selection_foo.R") # selecting GARCH models

source("Question 5/code/GARCH_Maker_foo.R") # creating the chosen model
```

``` r
ZARRet <- cncy %>% 
    filter(date > as.Date("2010-12-31")) %>% # last ten years
    filter(Name %in% "SouthAfrica_Cncy") %>% 
    mutate(dlogret = log(Price) - lag(log(Price))) %>% 
    filter(date > first(date)) %>% 
    rename(Date = date) # for simple substitution into class code.

# Find best model

NumberOneModel <- Selection_foo(ZARRet)

#NumberOneModel

#               sGARCH  gjrGARCH    eGARCH    apARCH
#Akaike       -6.476002 -6.483637 -6.481459 -6.479932

ChosenGarch <- GARCH_Maker_foo(ZARRet, "gjrGARCH") 

# gjrGARCH has the lowest AIC statistic. 

# Ranking the Volatilites, I put this in a table later on. 

TableFrame <- cncy %>%
  filter(date > as.Date("2010-12-31")) %>%
  arrange(date) %>%
  group_by(Name) %>%
  mutate(dlogret = log(Price) - lag(log(Price))) %>%
  filter(date > first(date)) %>%
  rename(Date = date) %>%
  mutate(vol = dlogret^2) %>%
  summarise(AvGlobeVol = mean(vol)) %>%
  arrange(desc(AvGlobeVol)) %>%
  top_n(10, AvGlobeVol) %>%
  rename(Country = Name) %>%
  mutate(Country = gsub("_Cncy", "", Country)) %>% 
    rename(Vol = AvGlobeVol)

# Average global volatility

AvGlobeVol <- cncy_IV %>% 
    filter(date > as.Date("2010-12-31")) %>% 
    group_by(date) %>% 
    summarise(AvGlobeVol = mean(Price)) %>% 
        mutate(Global_vol = AvGlobeVol/max(AvGlobeVol))

# Add sigma

sigma <- sigma(ChosenGarch) %>% 
    xts_tbl() 

colnames(sigma) <- c("date", "sigma") 

sigma <- sigma %>% 
    mutate(Date = as.Date(date)) %>% 
    mutate(ZAR_sigma = sigma/max(sigma))%>% 
    left_join(., AvGlobeVol, 
              by = "date")

# Find Strong Dollar periods (measured by low volatility), I want to make my green rectangle sections on the ggplot again. 

USData <- bbdxy %>% 
    filter(date > as.Date("2010-12-31")) %>% 
    mutate(dlogret = log(Price) - lag(log(Price))) %>% 
    filter(date > first(date)) %>%
    mutate(Year = format(date, "%Y")) %>% 
    group_by(Year) %>%
    mutate(TopQtile = quantile(dlogret, 0.9))

USData <- USData %>% 
  
  mutate(Year = format(date, "%Y")) %>% 
  
  group_by(Year) %>% summarise(SD = sd(dlogret)*sqrt(12)) %>% 
  
  mutate(TopQtile = quantile(SD, 0.2))

# 2011, 2015, 2016, 2020 are stable dollars years. 

# once again constructing my data frames to be used in the ggplot. 

HV1 <- data.frame(
  xmin = as.Date("2021-01-01"),
  xmax = as.Date("2021-12-01"),
  ymin = -Inf,
  ymax = Inf
)

HV2 <- data.frame(
  xmin = as.Date("2014-01-01"),
  xmax = as.Date("2014-12-01"),
  ymin = -Inf,
  ymax = Inf
)

HV3 <- data.frame(
  xmin = as.Date("2019-01-01"),
  xmax = as.Date("2019-12-01"),
  ymin = -Inf,
  ymax = Inf
)

###

# ZAR vs global average vol with low vol US dollar periods on the same ggplot. 

gg2 <- sigma %>%
  select(-date) %>%
  pivot_longer(c("ZAR_sigma", "Global_vol"),
               names_to = "Vol_type",
               values_to = "VOL") %>%
  ggplot() +
    geom_rect(data = HV1, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green3", 
              alpha = 0.2) +
    geom_rect(data = HV2, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green3", 
              alpha = 0.2) +
    geom_rect(data = HV3, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = "green3", 
              alpha = 0.2) +
  geom_line(aes(Date, VOL, colour = Vol_type), size = 0.8, alpha = 0.7) +
  labs(
    title = "ZAR and Global Average Volatility",
    x = "",
    y = "") +
  scale_color_manual(values = c("ZAR_sigma" = "orange", 
                                "Global_vol" = "red4")) +
  theme_classic() +
  theme(
    legend.position = "bottom",  
    legend.box = "horizontal") +
  guides(color = guide_legend(title = ""))
```

``` r
# rand in top 10, #4

kable(TableFrame)
```

| Country     |       Vol |
|:------------|----------:|
| Ghana       | 0.0001859 |
| Russia      | 0.0001070 |
| Brazil      | 0.0000988 |
| SouthAfrica | 0.0000969 |
| Argentina   | 0.0000957 |
| Nigeria     | 0.0000933 |
| Zambia      | 0.0000912 |
| Egypt       | 0.0000834 |
| Turkey      | 0.0000826 |
| Mexico      | 0.0000624 |

``` r
# plotting cleaned volatility. 

VolPlotter(data = ZARRet, fit = ChosenGarch)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-35-1.png" alt=" \label{Figure5.1}"  />
<p class="caption">
</p>

``` r
# plotting average volatility, rand volatility, and US low volatility periods. 

gg2
```

<img src="README_files/figure-markdown_github/unnamed-chunk-36-1.png" alt=" \label{Figure5.2}"  />
<p class="caption">
</p>

# Question 6

``` r
rm(list = ls())

gc()
```

    ##           used  (Mb) gc trigger   (Mb) max used   (Mb)
    ## Ncells 3157577 168.7   24610932 1314.4 38454580 2053.7
    ## Vcells 5392262  41.2   44436512  339.1 59757965  456.0

``` r
# One again these functions are based on the practical sessions, and especially the Impute missing returns one is provided by Nico.  

source('Question 6/code/Impute_Missing_Returns.R') 

source('Question 6/code/Optimisation_Function.R')

source('Question 6/code/Roller_Function.R')

###

MAA <- read_rds("data/MAA.rds") %>% 
    select(-Name) %>% 
    arrange(date) %>% 
    rename(Tickers = Ticker) %>% 
    filter(date >= "2010-01-01") # specified in question

msci <- read_rds("data/msci.rds") %>%
        filter(Name %in% c("MSCI_ACWI", 
                           "MSCI_USA", 
                           "MSCI_RE", 
                           "MSCI_Jap")) %>% 
        filter(date >= "2010-01-01") %>% # specified in question
        rename(Tickers = Name) %>% 
        arrange(date) 
```

``` r
# Here I am trying to separate and combine all the different asset classes mentioned in the question. 

Combined_assets <- rbind(MAA, msci) %>% 
    arrange(date)

CombAss3Yr <- Combined_assets %>% 
    group_by(Tickers) %>% 
    filter(date == as.Date("2018/01/01")) %>% # last three years
    pull(Tickers) %>% 
    unique() 
    
Start <- Combined_assets %>% 
    filter(Tickers %in% CombAss3Yr) %>% 
    group_by(Tickers) %>% 
    summarise(date = dplyr::first(date)) %>%
    summarise(latest = dplyr::first(date))

# Now I need to focus on getting the dates in order to perform re-balancing. 

DateVector <- Combined_assets %>% 
    filter(Tickers %in% CombAss3Yr) %>% 
    filter(date >= Start[[1]]) %>% 
    select(date) %>% 
    unique %>% 
    mutate(YM = format(date, "%Y%B")) %>% 
    group_by(YM) %>% 
    filter(date == dplyr::last(date)) %>% 
    ungroup() %>% 
    pull(date) %>% 
    unique

# But the re-balancing is quarterly, as specified in question.

QuarterDates <- rmsfuns::dateconverter(as.Date("2010-01-31"), 
                                       as.Date("2021-10-29"), 
                                       "weekdayEOQ") 

# Now that we have the necessary dates, and re balancing dates, we can filter the data appropriately and start seeing what sorts of returns we are getting. 

Combined_assets <- Combined_assets %>% 
    filter(Tickers %in% CombAss3Yr) %>% 
    filter(date >= Start[[1]]) %>% 
    filter(date %in% QuarterDates) %>% 
    group_by(Tickers) %>% 
    mutate(ret = Price/lag(Price) - 1) %>% 
    filter(date > dplyr::first(date)) %>% 
    select(-Price) %>%
    spread(Tickers, ret)

# I have not actually checked whether there are missing values, but just to be sure, I use the function from the tutorial.  

# "Drawn_Distribution_Collective" because Nico recommends this.

options(scipen = 999)

ReturnMatrix <- 
  impute_missing_returns(Combined_assets, 
  impute_returns_method = "Drawn_Distribution_Collective", 
  Seed = as.numeric(format( Sys.time(), "%Y%d%H%M")))

RetMatDateless <- data.matrix(ReturnMatrix[, -1])

# Constraints

LB = 0.01 # it is a long only portfolio
UB = 0.2 # do not want too much of a good thing 
Equities = 0.6 # limit
Bonds = 0.25 # limit
meq = 1
Stonks <- ncol(RetMatDateless)

# Matrix 

BondMat <- rbind(matrix(0, 3, 6), -diag(6), matrix(0, 4, 6))

EquiMat <- rbind(matrix(0, nrow = 9, ncol = 4), -diag(4))

bvec <- c(1, rep(LB, Stonks), 
          -rep(UB, Stonks), 
          -rep(Equities, 4), 
          -rep(Bonds, 
               6))

Amat <- cbind(1, 
              diag(Stonks), 
              -diag(Stonks), 
              EquiMat, BondMat)

# Calculate optimal rolling weights for each type of portfolio optimization, which makes use of a function I created. 

PortfolioWeights <- 
QuarterDates %>% map_df(~Roller(ReturnMatrix, 
                                DateVector = ., 
                                Amat = Amat, 
                                bvec = bvec, 
                                LookBack = 12)) %>% 
    select(-Look_Back_Period)
```

``` r
# simply display the optimal portfolio

head(PortfolioWeights, 15)
```

    ## # A tibble: 15 × 5
    ##    stocks             mv minvol sharpe date      
    ##    <chr>           <dbl>  <dbl>  <dbl> <date>    
    ##  1 ADXY Index     0.0100 0.2    0.0769 2010-03-31
    ##  2 BCOMTR Index   0.01   0.0355 0.0769 2010-03-31
    ##  3 DXY Index      0.0100 0.2    0.0769 2010-03-31
    ##  4 LEATTREU Index 0.0100 0.0100 0.0769 2010-03-31
    ##  5 LGAGTRUH Index 0.0100 0.2    0.0769 2010-03-31
    ##  6 LGCPTRUH Index 0.0100 0.0100 0.0769 2010-03-31
    ##  7 LP05TREH Index 0.0100 0.0724 0.0769 2010-03-31
    ##  8 LUACTRUU Index 0.120  0.0100 0.0769 2010-03-31
    ##  9 LUAGTRUU Index 0.0100 0.2    0.0769 2010-03-31
    ## 10 MSCI_ACWI      0.200  0.01   0.0769 2010-03-31
    ## 11 MSCI_Jap       0.200  0.0320 0.0769 2010-03-31
    ## 12 MSCI_RE        0.200  0.01   0.0769 2010-03-31
    ## 13 MSCI_USA       0.200  0.0100 0.0769 2010-03-31
    ## 14 ADXY Index     0.0100 0.2    0.0769 2010-06-30
    ## 15 BCOMTR Index   0.01   0.0355 0.0769 2010-06-30
