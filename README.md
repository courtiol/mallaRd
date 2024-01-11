
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mallaRd

<!-- badges: start -->
<!-- badges: end -->

The goal of mallaRd is to reproduces the analyses and results from the
paper “Housing search in the concrete jungle – breeding site selection
of urban mallards and implications for conservation management
strategies” by Engler et al. (in prep).

## Installation and usage

You can install our package mallaRd from [GitHub](https://github.com/)
with:

``` r
install.packages("remotes") ## install the package remotes if you don't have it
remotes::install_github("courtiol/mallaRd") ## install our package
```

Then load the package in your R session and check the main help page
where we placed all analyses:

``` r
load("mallaRd")
?mallaRd
```

## Raw data

You can access the raw data used for the paper
[here](inst/extdata/raw_data.csv) or via the R package:

``` r
rawdata <- read.csv(system.file("extdata/raw_data.csv", package = "mallaRd"))
head(rawdata) ## first 6 rows
```

<div class="kable-table">

|  id | year | date       | species      | breeding_site_lat | breeding_site_long | habitat_type | floor_level | hatch_date | clutch_size | brood_size | ring_number | body_mass_g | wing_length_mm | release_site_lat | release_site_long |     DNSW | PSW1000 | PSW2000 | trafficvolume500 | populationdensity500 | trafficvolume1000 | populationdensity1000 | trafficvolume2000 | populationdensity2000 |
|----:|-----:|:-----------|:-------------|------------------:|-------------------:|:-------------|------------:|:-----------|------------:|-----------:|:------------|------------:|---------------:|-----------------:|------------------:|---------:|--------:|--------:|-----------------:|---------------------:|------------------:|----------------------:|------------------:|----------------------:|
|   1 | 2005 | 01.04.2005 | mallard_duck |          52.52495 |           13.30129 | courtyard    |          NA |            |          NA |         NA | JC8002      |          NA |             NA |         52.54819 |          13.31221 | 276.1204 |       0 |       1 |        1018.6459 |            143.70396 |        1679.24304 |              88.71829 |         1297.3898 |              93.85891 |
|   2 | 2005 | 04.04.2005 | mallard_duck |          52.60742 |           13.23319 | other        |          NA |            |          NA |          1 |             |          NA |             NA |               NA |                NA | 929.5084 |       1 |       1 |         158.7811 |             25.50148 |          72.82336 |              23.18329 |          131.7400 |              14.51628 |
|   3 | 2005 | 25.04.2005 | mallard_duck |          52.52412 |           13.34539 | roof_terrace |          NA |            |          NA |          8 |             |          NA |             NA |               NA |                NA | 127.3921 |       0 |       1 |         843.3160 |            127.61779 |         743.98803 |             147.67042 |          858.6173 |              78.80484 |
|   4 | 2005 | 25.04.2005 | mallard_duck |          52.52412 |           13.34539 | roof_terrace |          NA |            |          NA |          8 |             |          NA |             NA |               NA |                NA | 127.3921 |       0 |       1 |         843.3160 |            127.61779 |         743.98803 |             147.67042 |          858.6173 |              78.80484 |
|   5 | 2005 | 25.04.2005 | mallard_duck |          52.52412 |           13.34539 | roof_terrace |          NA |            |          NA |          8 |             |          NA |             NA |               NA |                NA | 127.3921 |       0 |       1 |         843.3160 |            127.61779 |         743.98803 |             147.67042 |          858.6173 |              78.80484 |
|   6 | 2005 | 25.04.2005 | mallard_duck |          52.52412 |           13.34539 | roof_terrace |          NA |            |          NA |          8 |             |          NA |             NA |               NA |                NA | 127.3921 |       0 |       1 |         843.3160 |            127.61779 |         743.98803 |             147.67042 |          858.6173 |              78.80484 |

</div>

## Developer corner

Here is the information of the environment we used to run all the
analyses:

``` r
sessionInfo()
#> R version 4.3.1 (2023-06-16)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Debian GNU/Linux 12 (bookworm)
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/atlas/libblas.so.3.10.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/atlas/liblapack.so.3.10.3;  LAPACK version 3.11.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
#>  [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
#>  [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Europe/Berlin
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_4.3.1    fastmap_1.1.1     cli_3.6.2         tools_4.3.1      
#>  [5] htmltools_0.5.7   rstudioapi_0.15.0 yaml_2.3.8        rmarkdown_2.25   
#>  [9] knitr_1.45        xfun_0.41         digest_0.6.33     rlang_1.1.2      
#> [13] evaluate_0.23
```
