Iteration_and_listcols
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
set.seed(1)
```

### lists

``` r
vec_numeric = 5:8
vec_char = c("My", "name", "is", "Jeff")
vec_logical = c(TRUE, TRUE, TRUE, FALSE)
```

Different stuff with different lengths

``` r
l = list(
  vec_numeric = 1:5,
  vec_char = LETTERS,
  matrix = matrix(1:10, nrow = 5, ncol = 2),
  summary = summary(rnorm(100))
)
```

Accessing lists

``` r
l$vec_char
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[[2]]
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.2147 -0.4942  0.1139  0.1089  0.6915  2.4016

### loops

``` r
list_norm_sample =
  list(
    a = rnorm(20, 2, 5),
    b = rnorm(20, 0, 7),
    c = rnorm(20, 10, .2),
    d = rnorm(20, -3, 1)
  )
```

mean and sd function

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

``` r
mean_and_sd(list_norm_sample$a)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.57  4.07

``` r
mean_and_sd(list_norm_sample$b)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.52  4.68

``` r
mean_and_sd(list_norm_sample$c)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.92 0.237

``` r
mean_and_sd(list_norm_sample$d)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.52 0.974

``` r
output = vector("list", length = 4)

for(i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm_sample[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.57  4.07
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.52  4.68
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.92 0.237
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.52 0.974

### use `map`

``` r
output = map(list_norm_sample, mean_and_sd)

map(list_norm_sample, summary)
```

    ## $a
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.5546 -0.5066  1.0382  2.5691  4.7491 10.8364 
    ## 
    ## $b
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -10.7551  -3.8040  -1.3795  -1.5209  -0.0923   9.4013 
    ## 
    ## $c
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   9.617   9.738   9.863   9.919  10.025  10.417 
    ## 
    ## $d
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.424  -3.168  -2.684  -2.519  -1.965  -0.692

### create DF

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm_sample
  )
```

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.57  4.07
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.52  4.68
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.92 0.237
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.52 0.974

``` r
listcol_df |>
  mutate(
    mean_sd = map(samp, mean_and_sd),
    median = map(samp, median)
    ) |>
  select(name, mean_sd) |>
  unnest(mean_sd)
```

    ## # A tibble: 4 × 3
    ##   name   mean    sd
    ##   <chr> <dbl> <dbl>
    ## 1 a      2.57 4.07 
    ## 2 b     -1.52 4.68 
    ## 3 c      9.92 0.237
    ## 4 d     -2.52 0.974

### NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

Import function.

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

nsduh_import(nsduh_html, 1, "marj")
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
nsduh_import(nsduh_html, 4, "cocaine")
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

``` r
nsduh_import(nsduh_html, 5, "heroine")
```

    ## # A tibble: 510 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    0.22 heroine
    ##  2 Alabama 12+   2014-2015    0.27 heroine
    ##  3 Alabama 12-17 2013-2014    0.1  heroine
    ##  4 Alabama 12-17 2014-2015    0.08 heroine
    ##  5 Alabama 18-25 2013-2014    0.45 heroine
    ##  6 Alabama 18-25 2014-2015    0.64 heroine
    ##  7 Alabama 26+   2013-2014    0.19 heroine
    ##  8 Alabama 26+   2014-2015    0.23 heroine
    ##  9 Alabama 18+   2013-2014    0.23 heroine
    ## 10 Alabama 18+   2014-2015    0.29 heroine
    ## # ℹ 500 more rows

import data using a `for` loop.

``` r
table_input = list(1, 4, 5)
name_input = list("marj", "cocaine", "heroine")

output = vector("list", length = 3)

for(i in 1:3) {
  
  output[[i]] = nsduh_import(nsduh_html, table_input[[i]], name_input[[i]])
  
}

nsduh_df = bind_rows(output)
```

Try again, using maps!

``` r
nsduh_import = function(html, table_number) {
  
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
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
}

nsduh_df = 
  tibble(
    name = c("marj", "cocaine", "heroine"),
    number = c(1, 4, 5)
  ) |>
  mutate(table = map(number, nsduh_import, html = nsduh_html)) |>
  unnest(table)
```

### revisit the weather df

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\10507\AppData\Local/R/cache/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-09-28 10:22:00.028023 (8.541)

    ## file min/max dates: 1869-01-01 / 2023-09-30

    ## using cached file: C:\Users\10507\AppData\Local/R/cache/R/rnoaa/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2023-10-31 11:06:56.090057 (1.71)

    ## file min/max dates: 1965-01-01 / 2023-06-30

    ## using cached file: C:\Users\10507\AppData\Local/R/cache/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2023-09-28 10:23:27.579521 (0.996)

    ## file min/max dates: 1999-09-01 / 2023-09-30

``` r
weather_nest_df = 
  weather_df |>
  nest(df = date:tmin)
```

can I regress `tmax` on `tmin` for each of these…?

``` r
central_park_df = 
  weather_nest_df |>
  pull(df) |>
  nth(1)
```

fit a linear regression for central park

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}

weather_lm(central_park_df)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

Let’s try a `for` loop

``` r
input_list = weather_nest_df |> pull(df)
output = vector("list", length = 3)

for(i in 1:3) {
  output[[i]] = weather_lm(input_list[[i]])
}

weather_nest_df |>
  mutate(models = map(df, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          df                 models
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>  
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>
