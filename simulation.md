simulation
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(readxl)
```

Set seed for reproducibility.

``` r
set.seed(2)
```

## Simulation sample and sd

Here’s an old function:

``` r
sim_mean_sd = function(n, mu = 5, sigma = 1) {
  
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  sim_data |> 
    summarize(
      mean = mean(x),
      sigma_hat = sd(x)
    )
}
```

Let’s see what this does.

``` r
sim_mean_sd(n = 30)
```

    ## # A tibble: 1 × 2
    ##    mean sigma_hat
    ##   <dbl>     <dbl>
    ## 1  5.23      1.17

Let’s iterate to see how this works UNDER REPEATED SAMPLING!

``` r
output = vector("list", length = 100)

for(i in 1:100) {
  
  output[[i]] = sim_mean_sd(n = 30)
  
}

sim_results = 
  bind_rows(output)

sim_results |>
  ggplot(aes(x = mean)) + geom_density()
```

![](simulation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
sim_results |>
  summarize(
    mu_hat = mean(mean),
    sd_hat = sd(mean)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sd_hat
    ##    <dbl>  <dbl>
    ## 1   5.05  0.194

use a map function

``` r
sim_result_df =
  expand_grid(
    sample_size = c(30, 60, 120, 240),
    iter = 1:1000
  ) |>
  mutate(estimate_df = map(sample_size, sim_mean_sd)) |>
  unnest(estimate_df)

sim_result_df |>
  mutate(
    sample_size = str_c("n = ", sample_size),
    sample_size = fct_inorder(sample_size) 
    # arrange in order of `sample_size` in the dataframe
  ) |>
  ggplot(aes(x = sample_size, y = mean)) +
  geom_boxplot()
```

![](simulation_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
