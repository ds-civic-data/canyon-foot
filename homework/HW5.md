HW4
================
Canyon Foot
3/5/2018

``` r
# Function that selects/tidys. It propably would have been a little more efficient to calculate the proportion after the sample is taken but I wanted to reduce the number of columns early on 
TIDYFUNC <- function(CD) {
  CD <- CD %>% 
    select(VOTER_ID, COUNTY, BIRTH_DATE, PRECINCT_NAME, EFF_REGN_DATE, PARTY_CODE, CONFIDENTIAL, STATUS, ZIP_CODE, 41:65) %>% 
  filter(!is.na(VOTER_ID)) %>% 
  gather(10:34, key = VOTE_DATE, value = VOTED) %>% 
  group_by(VOTER_ID) %>% 
  mutate(VOTE_PROP =sum(VOTED == "YES")/(sum(VOTED == "NO") + sum(VOTED == "YES"))) %>%   select(-VOTED, -VOTE_DATE) %>%
  distinct()
}
CD1 <- TIDYFUNC(CD1)
CD2 <- TIDYFUNC(CD2)
CD3 <- TIDYFUNC(CD3)
CD4 <- TIDYFUNC(CD4)
CD5 <- TIDYFUNC(CD5)
# Changing ZIPs that are nums to chars
CD1$ZIP_CODE <- as.character(CD1$ZIP_CODE)
CD4$ZIP_CODE <- as.character(CD4$ZIP_CODE)
CD_FULL <- rbind(CD1, CD2, CD3, CD4, CD5)
# Joining
CD_temp <- full_join(CD_FULL, or_vote_reg)
```

    ## Joining, by = c("VOTER_ID", "COUNTY", "BIRTH_DATE", "EFF_REGN_DATE", "PARTY_CODE", "CONFIDENTIAL", "STATUS")

``` r
oregon_voters <- full_join(CD_temp, omv_reg) %>% 
  ungroup()
```

    ## Joining, by = c("VOTER_ID", "COUNTY")

``` r
# Sampling
set.seed(123)
voter_sample <- sample_n(oregon_voters, 100000) 
```
