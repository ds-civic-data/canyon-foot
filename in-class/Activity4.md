Activity4
================
Canyon Foot
2/20/2018

``` r
pop_carrier <- function(carr) {
  flights %>% 
    filter(carrier == carr) %>%
    group_by(dest) %>%
    summarise(total = n()) %>%
    arrange(desc(total)) %>%
    head(5)
}

pop_carrier("WN")
```

    ## # A tibble: 5 x 2
    ##   dest  total
    ##   <chr> <int>
    ## 1 OAK    4109
    ## 2 SMF    3163
    ## 3 LAS    2690
    ## 4 DEN    2423
    ## 5 PHX    2345

``` r
pop_airport <- function(port) {
  flights %>% 
    filter(dest == port) %>%
    group_by(carrier) %>%
    summarise(total = n(), avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    head(5)
}

pop_airport("LAX")
```

    ## # A tibble: 5 x 3
    ##   carrier total avg_delay
    ##   <chr>   <int>     <dbl>
    ## 1 AS       5989      1.58
    ## 2 OO       2150      5.14
    ## 3 VX       1264     -2.79
    ## 4 DL        799      1.14
    ## 5 UA        253      5.87
