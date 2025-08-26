Hops Counties from NASS
================
Don A. Lloyd

Updated 24 August, 2025

<!-- 1 June 25 new Rmd for creation of hops_counties data -->

``` r
require(keyring)
require(rnassqs) # API
require(dplyr)
require(tidyr) # pivot_wider
require(here)
```

<a id="top"></a> [Obtain NASS census data](#obtain-nass-census-data)

[Verify census data](#verify-census-with-nass-survey-data)

### Overview

This exercise is part of a larger data project to analyze hop yields in
the PNW, and how hops production over time might be related climatic and
air quality measures. Ideally, we would want to work with environmental
measurements where hops are grown. Meteorological and air quality data
are collected and reported by widely dispersed samplers, often with well
known positions but rarely collocated with hops fields.

Crop yields can be affected by environmental factors regardless of how
far away they were produce. For example wildfire smoke might negatively
impact the quality and quantity of a harvest, whether the smoke
originated nearby or hundreds of miles away. Understanding where hops
are grown and where environmental measurements are taken will help us
decide later how to combine the data for modeling. While the locations
of environmental samplers are well documented, there are many other
obstacles to producing a dataset for modeling or other analysis. Hop
yields are reported by NASS surveys only at state level not county. Even
with county-level reporting, we would still not know where within the
county to look for the nearest environmental samplers.

This write-up develops the first steps toward understanding the
strengths and weaknesses of USDA NASS data sources.

### Obtain NASS census data

NASS quick stats provide access to two sources of agricultural data.
We’ll start by pulling the NASS census data at county level for PNW to
find which counties have the largest acreage devoted to hops.

``` r
key_get("NASS") %>%
  nassqs_auth()

nass_census_hops <- nassqs(
  agg_level_desc = "COUNTY"
  ,source_desc = "CENSUS"
  ,state_alpha = c("OR","WA","ID")
  ,commodity_desc = "HOPS"
  ,progress_bar = FALSE
)

# these counties are represented among the census
nass_census_hops %>%
  distinct(state_name, county_name)
```

    ##    state_name county_name
    ## 1       IDAHO    BOUNDARY
    ## 2       IDAHO  CLEARWATER
    ## 3       IDAHO    KOOTENAI
    ## 4       IDAHO      CANYON
    ## 5       IDAHO      OWYHEE
    ## 6      OREGON      BENTON
    ## 7      OREGON   CLACKAMAS
    ## 8      OREGON     CLATSOP
    ## 9      OREGON        LANE
    ## 10     OREGON      MARION
    ## 11     OREGON        POLK
    ## 12     OREGON     YAMHILL
    ## 13     OREGON       WASCO
    ## 14     OREGON    UMATILLA
    ## 15     OREGON     DOUGLAS
    ## 16     OREGON     JACKSON
    ## 17     OREGON   JOSEPHINE
    ## 18     OREGON   DESCHUTES
    ## 19     OREGON     MALHEUR
    ## 20 WASHINGTON        KING
    ## 21 WASHINGTON      PIERCE
    ## 22 WASHINGTON    SAN JUAN
    ## 23 WASHINGTON      BENTON
    ## 24 WASHINGTON    KITTITAS
    ## 25 WASHINGTON      YAKIMA
    ## 26 WASHINGTON     SPOKANE
    ## 27 WASHINGTON     STEVENS
    ## 28 WASHINGTON    FRANKLIN

The list of counties reported by NASS is not exhaustive for PNW, so we
might guess that hops are commercially cultivated at some level in each
of these counties. Let’s check what acreage is reported.

``` r
# consider the two most recent census years for highlighting counties that produce hops
census_yrs <- pull(nass_census_hops, year) %>% unique
earliest_census <- min(census_yrs)
latest_census <- max(census_yrs)
prior_census <- max(census_yrs[-which.max(census_yrs)])

(census_hops_counties <- 
    filter(nass_census_hops, 
           statisticcat_desc == "AREA HARVESTED",
           unit_desc == "ACRES",
           short_desc == "HOPS - ACRES HARVESTED",
           domain_desc == "TOTAL") %>%
    mutate(Value = as.numeric(Value)) %>%
    dplyr::select(state_name, county_name, year, Value) %>% 
    arrange(state_name, county_name, year) %>%
    pivot_wider(id_cols = c(state_name, county_name), names_from = year,
                values_from = Value) %>%
    # NA are unreported values, not just a rounded zero
    ungroup %>%
    arrange(desc(!!sym(latest_census)), desc(!!sym(prior_census)))
)
```

    ## # A tibble: 28 × 8
    ##    state_name county_name `1997` `2002` `2007` `2012` `2017` `2022`
    ##    <chr>      <chr>        <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ##  1 WASHINGTON YAKIMA       27737  16813  18587  16232  29031  32488
    ##  2 OREGON     MARION        7407   4593     NA   4277   7637   7793
    ##  3 OREGON     JACKSON         NA     NA     NA     NA     96      2
    ##  4 OREGON     JOSEPHINE       NA     NA     NA     NA     NA      2
    ##  5 WASHINGTON BENTON        4412   4020   4320     NA   9618     NA
    ##  6 WASHINGTON STEVENS         NA     NA     NA     NA      9     NA
    ##  7 OREGON     BENTON          NA     NA     NA     NA      4     NA
    ##  8 OREGON     DESCHUTES       NA     NA     NA     NA      4     NA
    ##  9 IDAHO      BOUNDARY        NA     NA     NA     NA     NA     NA
    ## 10 IDAHO      CANYON          NA     NA     NA     NA     NA     NA
    ## # ℹ 18 more rows

No suprise that we find Yakima and Marion counties reporting large
acreage. Benton County (WA) is unusual for its inconsistent data
collection since 2012. But most counties have no reporting whatsoever.
Single operators within a county are intentionally excluded from NASS
reporting to protect their anonymity, but this is an unlikely
explanation for counties with significant agricultural land use. In many
cases, individual operators or even state agricultural deparments may
simple not participate in the census. Most notably, Idaho apparently
declines to report *any* harvest acres, and cannot be ranked among its
peers. We know apart from these data that much of Idaho’s hops are grown
in Canyon County, but will have to look elsewhere to substantiate that
information quantitatively.

Back to [top](#top)

### Verify census with NASS survey data

Are the census data consistent with the annual survey reporting? The
availability of county data varies by crop. Unfortunately, we can only
pull hops survey data at state level, but we can compare state survey
acreage to the county acreage reported with census data.

``` r
nass_survey_hops <- nassqs(
  source_desc = "SURVEY"
  ,class_desc = "ALL CLASSES"
  ,agg_level_desc = "STATE"
  ,sector_desc = "CROPS"
  ,commodity_desc = "HOPS"
  ,statisticcat_desc = "AREA HARVESTED"
  ,year__GE = earliest_census
  ,reference_period_desc = "YEAR"
  ,unit_desc = "ACRES"
  ,progress_bar = FALSE
)

# survey results for ACRES HARVESTED
nass_survey_hops %>%
  filter(year %in% nass_census_hops$year) %>%
  arrange(year) %>%
  pivot_wider(id_cols = "state_name", names_from = "year", values_from = "Value")
```

    ## # A tibble: 3 × 7
    ##   state_name `1997` `2002` `2007` `2012` `2017` `2022`
    ##   <chr>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 IDAHO        3870   3399   2896   2596   7125   9561
    ## 2 OREGON       8352   5577   5270   4391   8216   7790
    ## 3 WASHINGTON  31080  20333  22745  22696  38648  42762

``` r
# summarize census data we pulled earlier for comparison
census_hops_counties %>%
  group_by(state_name) %>%
  summarise(across(where(is.numeric), function(x) sum(x, na.rm=T)))
```

    ## # A tibble: 3 × 7
    ##   state_name `1997` `2002` `2007` `2012` `2017` `2022`
    ##   <chr>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 IDAHO           0      0      0      0      0      0
    ## 2 OREGON       7407   4593      0   4277   7741   7797
    ## 3 WASHINGTON  32149  20833  22907  16232  38658  32488

Aside from an incongruous reporting gap for Oregon and the previously
mentioned omission of Idaho, both datasets show the same orders of
magnitude in reported acreage, with respective values sometimes within a
few percent. While census data allow us to identify some important hop
growing counties, they are not reported frequently or completely enough
for analysis of harvested acres much less yields.

Here is our final ranked list of hops producing counties, with FIPS. We
now have good candidate counties in Oregon and Washington to consider
further, but none yet for Idaho.

``` r
(nass_hops_counties <-
   census_hops_counties %>%
   pivot_longer(cols = where(is.numeric), names_to = "year", values_to = "acres", values_drop_na = T) %>%
   group_by(state_name, county_name) %>%
   # reminder acres = ACRES HARVESTED
   summarise(acres_mean = mean(acres)
             ,acres_max = max(acres)
             ,acres_50 = median(acres)
             ,.groups = "keep") %>%
   ungroup %>%
   arrange(desc(acres_mean)) %>%
   # assign the fips code to remaining counties
   left_join(nass_census_hops %>%
               mutate(fips = paste0(state_fips_code, county_code)) %>%
               select(state_name, county_name, fips) %>%
               distinct
   )
)
```

    ## Joining with `by = join_by(state_name, county_name)`

    ## # A tibble: 8 × 6
    ##   state_name county_name acres_mean acres_max acres_50 fips 
    ##   <chr>      <chr>            <dbl>     <dbl>    <dbl> <chr>
    ## 1 WASHINGTON YAKIMA          23481.     32488    23162 53077
    ## 2 OREGON     MARION           6341.      7793     7407 41047
    ## 3 WASHINGTON BENTON           5592.      9618     4366 53005
    ## 4 OREGON     JACKSON            49         96       49 41029
    ## 5 WASHINGTON STEVENS             9          9        9 53065
    ## 6 OREGON     BENTON              4          4        4 41003
    ## 7 OREGON     DESCHUTES           4          4        4 41017
    ## 8 OREGON     JOSEPHINE           2          2        2 41033

``` r
write.csv(nass_hops_counties, here("nass_hops_counties.csv"))
```

<!-- IDAHO for year of 2023. Canyon contains the most growers. -->

<!-- https://agri.idaho.gov/main/hop-growers-in-idaho/ -->

<!-- OREGON -->

<!-- only Marion and Polk counties in Oregon grow commercially -->

<!-- https://www.oregonencyclopedia.org/articles/hop_industry/ -->

<!-- other OR counties in Willamette Valley include: -->

<!-- Douglas, Lane, Linn, Benton, Yamhill, and Washington -->

<!-- ### Assumptions and Limitations -->

<!-- Counties included in this analysis were identified from state agricultural  -->

<!-- websites.  -->

<!-- _I have not yet found data about acreage or production by county over time._ This analysis assumes that the selected counties have continuously produced hops throughout the analysis period. This may overestimate the county level climate and air-quality variability if those counties' hops production have changed over time (transition to or from hops production from other crops, farmland lost to other development, etc.) -->

<!-- ### NCEI Global Summary of the Year (GSOY) -->

<!-- We can use geographic information from `tigris` to determine boundaries for each hops growing county from our list. -->

Back to [top](#top)

``` r
sessionInfo()
```

    ## R version 4.4.3 (2025-02-28)
    ## Platform: aarch64-apple-darwin20
    ## Running under: macOS Monterey 12.7.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: America/Los_Angeles
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] here_1.0.1    tidyr_1.3.1   dplyr_1.1.4   rnassqs_0.6.3 keyring_1.4.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] vctrs_0.6.5       httr_1.4.7        cli_3.6.5         knitr_1.50       
    ##  [5] rlang_1.1.6       xfun_0.52         purrr_1.1.0       generics_0.1.4   
    ##  [9] glue_1.8.0        rprojroot_2.1.0   htmltools_0.5.8.1 rmarkdown_2.29   
    ## [13] evaluate_1.0.4    tibble_3.3.0      fastmap_1.2.0     yaml_2.3.10      
    ## [17] lifecycle_1.0.4   compiler_4.4.3    pkgconfig_2.0.3   rstudioapi_0.17.1
    ## [21] digest_0.6.37     R6_2.6.1          utf8_1.2.6        tidyselect_1.2.1 
    ## [25] curl_6.4.0        pillar_1.11.0     magrittr_2.0.3    withr_3.0.2      
    ## [29] tools_4.4.3
