writing_functions
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
set.seed(12345)
```

## Z score function

Z scores subtract the mean and devide by the sd.

``` r
x_vec = rnorm(20, mean = 5, sd = .3)
```

Compute Z score for \`x_vec.

``` r
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

Write a function to do this!

``` r
z_score = function(x) {
  
  if(!is.numeric(x)) {
    stop("Argument should be numbers")
  } else if (length(x) < 2) {
    stop("You need at least 2 numbers to get z scores")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
  
}
```

Check that this works.

``` r
z_score(x = x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

Keep checking.

``` r
z_score(x = 3) 
```

    ## Error in z_score(x = 3): You need at least 2 numbers to get z scores

``` r
z_score(c("my", "name", "is", "Jess")) 
```

    ## Error in z_score(c("my", "name", "is", "Jess")): Argument should be numbers

``` r
z_score(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(c(TRUE, TRUE, FALSE, TRUE)): Argument should be numbers

``` r
z_score(iris) 
```

    ## Error in z_score(iris): Argument should be numbers

### Multiple outputs.

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("Argument should be numbers")
  } else if (length(x) < 2) {
    stop("You need at least 2 numbers to get z scores")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

Double check I did this right …

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.02 0.250

### Start getting means ad sds

``` r
x_vec = rnorm(n = 30, mean = 5, sd = .5)

tibble(
  mean = mean(x_vec),
  sd = sd(x_vec)
)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.12 0.625

Let’s write a function that uses `n`, a true mean, and true SD as
inputs.

``` r
sim_mean_sd = function(n_obs, mu = 5, sigma = 1) {
  
  x_vec = rnorm(n = n_obs, mean = mu, sd = sigma)

  tibble(
    mean = mean(x_vec),
    sd = sd(x_vec)
  )
  
}

sim_mean_sd(n_obs = 30, mu = 50, sigma = .5)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  50.1 0.590

``` r
sim_mean_sd(n_obs = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.08  1.09

``` r
sim_mean_sd(12, 24, 4)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  24.6  2.89

``` r
sim_mean_sd(mu = 0, n_obs = 24, 4)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.648  3.53

## LoTR words.

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring") |>
  janitor::clean_names()

lotr_load_and_tidy = function(path = "data/LotR_Words.xlsx", cell_range, movie_name) {
  
  movie_df =
    readxl::read_excel(path, range = cell_range) |>
    mutate(movie = movie_name) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words"
    ) |>
    select(movie, everything())
  
  movie_df
  
}

lotr_df = bind_rows(
  lotr_load_and_tidy(cell_range = "B3:D6", movie_name = "fellowship_ring"),
  lotr_load_and_tidy(cell_range = "F3:H6", movie_name = "two_towers"),
  lotr_load_and_tidy(cell_range = "J3:L6", movie_name = "return_king")
)

lotr_df
```

    ## # A tibble: 18 × 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring Elf    female  1229
    ##  2 fellowship_ring Elf    male     971
    ##  3 fellowship_ring Hobbit female    14
    ##  4 fellowship_ring Hobbit male    3644
    ##  5 fellowship_ring Man    female     0
    ##  6 fellowship_ring Man    male    1995
    ##  7 two_towers      Elf    female   331
    ##  8 two_towers      Elf    male     513
    ##  9 two_towers      Hobbit female     0
    ## 10 two_towers      Hobbit male    2463
    ## 11 two_towers      Man    female   401
    ## 12 two_towers      Man    male    3589
    ## 13 return_king     Elf    female   183
    ## 14 return_king     Elf    male     510
    ## 15 return_king     Hobbit female     2
    ## 16 return_king     Hobbit male    2673
    ## 17 return_king     Man    female   268
    ## 18 return_king     Man    male    2459

### NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

Try to write a quick function.

``` r
nsduh_import = function(html, table_number, outcome_name) {
  
  html |> 
  html_table() |> 
  nth(table_number) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent),
    outcome = outcome_name) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
}

nsduh_import(html = nsduh_html, table_number = 1, outcome_name = "marj")
```

    ## # A tibble: 510 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    9.98 marj   
    ##  2 Alabama 12+   2014-2015    9.6  marj   
    ##  3 Alabama 12-17 2013-2014    9.9  marj   
    ##  4 Alabama 12-17 2014-2015    9.71 marj   
    ##  5 Alabama 18-25 2013-2014   27.0  marj   
    ##  6 Alabama 18-25 2014-2015   26.1  marj   
    ##  7 Alabama 26+   2013-2014    7.1  marj   
    ##  8 Alabama 26+   2014-2015    6.81 marj   
    ##  9 Alabama 18+   2013-2014    9.99 marj   
    ## 10 Alabama 18+   2014-2015    9.59 marj   
    ## # ℹ 500 more rows

``` r
nsduh_import(html = nsduh_html, table_number = 4, outcome_name = "cocaine")
```

    ## # A tibble: 510 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    1.23 cocaine
    ##  2 Alabama 12+   2014-2015    1.22 cocaine
    ##  3 Alabama 12-17 2013-2014    0.42 cocaine
    ##  4 Alabama 12-17 2014-2015    0.41 cocaine
    ##  5 Alabama 18-25 2013-2014    3.09 cocaine
    ##  6 Alabama 18-25 2014-2015    3.2  cocaine
    ##  7 Alabama 26+   2013-2014    1.01 cocaine
    ##  8 Alabama 26+   2014-2015    0.99 cocaine
    ##  9 Alabama 18+   2013-2014    1.31 cocaine
    ## 10 Alabama 18+   2014-2015    1.31 cocaine
    ## # ℹ 500 more rows
