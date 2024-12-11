
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mallaRd

<!-- badges: start -->
<!-- badges: end -->

The goal of mallaRd is to reproduces the analyses and results from the
paper “Breeding site fidelity in the concrete jungle: implications for
the management of urban mallards.” by Engler et al. (accepted in Journal
of Urban Ecology).

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
head(data_raw) ## first 6 rows
```

<div class="kable-table">

|  ID | year | date       | species      | location_ID | location_lat | location_long | habitat_type | floor_level | body_mass_g | wing_length_mm | hatch_date | clutch_size | brood_size | ring_number | release_site_lat | release_site_long |     DNSW | PSW1000 | PSW2000 | trafficvolume500 | populationdensity500 | trafficvolume1000 | populationdensity1000 | trafficvolume2000 | populationdensity2000 |
|----:|-----:|:-----------|:-------------|------------:|-------------:|--------------:|:-------------|------------:|------------:|---------------:|:-----------|------------:|-----------:|:------------|-----------------:|------------------:|---------:|--------:|--------:|-----------------:|---------------------:|------------------:|----------------------:|------------------:|----------------------:|
|   1 | 2005 | 01.04.2005 | mallard_duck |         729 |     52.52495 |      13.30129 | courtyard    |          NA |          NA |             NA |            |          NA |         NA | JC8002      |         52.54819 |          13.31221 | 276.1204 |       1 |       1 |        1018.6459 |            143.70396 |        1679.24304 |              88.71829 |         1297.3898 |              93.85891 |
|   2 | 2005 | 04.04.2005 | mallard_duck |        1116 |     52.60742 |      13.23319 | other        |          NA |          NA |             NA |            |          NA |          1 |             |               NA |                NA | 929.5084 |       1 |       1 |         158.7811 |             25.50148 |          72.82336 |              23.18329 |          131.7400 |              14.51628 |
|   3 | 2005 | 25.04.2005 | mallard_duck |         720 |     52.52412 |      13.34539 | roof_terrace |          NA |          NA |             NA |            |          NA |          8 |             |               NA |                NA | 127.3921 |       1 |       1 |         843.3160 |            127.61779 |         743.98803 |             147.67042 |          858.6173 |              78.80484 |
|  11 | 2005 | 26.04.2005 | mallard_duck |         555 |     52.51043 |      13.19960 | balcony      |          NA |         750 |            257 | 26.04.2005 |          12 |         12 | JC54565     |         52.51088 |          13.20227 | 162.9485 |       1 |       1 |         734.5966 |             23.64657 |         529.66019 |              21.79157 |          339.1518 |              26.49088 |
|  12 | 2005 | 27.04.2005 | mallard_duck |         991 |     52.56289 |      13.20875 | courtyard    |          NA |          NA |             NA |            |          NA |          3 |             |               NA |                NA | 426.5027 |       1 |       1 |           0.0000 |            138.54895 |         117.49123 |              67.91985 |          111.4260 |              38.82886 |
|  13 | 2005 | 29.04.2005 | mallard_duck |         115 |     52.44341 |      13.58433 | balcony      |          NA |         830 |            272 | 29.04.2005 |          NA |         10 | JC52194     |         52.44400 |          13.62276 |  80.8951 |       1 |       1 |         108.0740 |            114.83922 |         172.58178 |              59.27967 |          208.3172 |              44.26453 |

</div>

## Developer corner

Our package relies on the following packages:

``` r
pkgs <- desc::desc_get_deps()
pkgs <- pkgs[pkgs$package != "R", ]
pkgs$version_used <- sapply(pkgs$package, \(pkg) paste(packageVersion(pkg), sep = "."))
pkgs
```

<div class="kable-table">

| type     | package    | version | version_used |
|:---------|:-----------|:--------|:-------------|
| Imports  | cowplot    | \*      | 1.1.3        |
| Imports  | dplyr      | \*      | 1.1.4        |
| Imports  | furrr      | \*      | 0.3.1        |
| Imports  | future     | \*      | 1.34.0       |
| Imports  | ggplot2    | \*      | 3.5.1        |
| Imports  | sf         | \*      | 1.0.19       |
| Imports  | spaMM      | \*      | 4.5.0        |
| Imports  | tibble     | \*      | 3.2.1        |
| Imports  | tidyr      | \*      | 1.3.1        |
| Imports  | tidyselect | \*      | 1.2.1        |
| Suggests | desc       | \*      | 1.4.3        |
| Suggests | devtools   | \*      | 2.4.5        |
| Suggests | doSNOW     | \*      | 1.0.20       |
| Suggests | DHARMa     | \*      | 0.4.7        |
| Suggests | spelling   | \*      | 2.3.1        |
| Suggests | showtext   | \*      | 0.9.7        |
| Suggests | knitr      | \*      | 1.49         |

</div>

Here is the information of the R & RStudio environment used to run all
the analyses:

``` r
sessionInfo()
#> R version 4.4.2 (2024-10-31)
#> Platform: x86_64-redhat-linux-gnu
#> Running under: Fedora Linux 41 (KDE Plasma)
#> 
#> Matrix products: default
#> BLAS/LAPACK: FlexiBLAS OPENBLAS-OPENMP;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Europe/Berlin
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices datasets  utils     methods   base     
#> 
#> other attached packages:
#> [1] mallaRd_0.1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] Matrix_1.7-1        gtable_0.3.6        crayon_1.5.3       
#>  [4] dplyr_1.1.4         compiler_4.4.2      tidyselect_1.2.1   
#>  [7] Rcpp_1.0.13-1       slam_0.1-53         parallel_4.4.2     
#> [10] scales_1.3.0        boot_1.3-31         yaml_2.3.10        
#> [13] fastmap_1.2.0       lattice_0.22-6      spaMM_4.5.0        
#> [16] ggplot2_3.5.1       R6_2.5.1            generics_0.1.3     
#> [19] knitr_1.49          backports_1.5.0     MASS_7.3-61        
#> [22] checkmate_2.3.2     ROI_1.0-1           tibble_3.2.1       
#> [25] desc_1.4.3          munsell_0.5.1       minqa_1.2.8        
#> [28] pillar_1.9.0        rlang_1.1.4         utf8_1.2.4         
#> [31] xfun_0.49           registry_0.5-1      cli_3.6.3          
#> [34] magrittr_2.0.3      digest_0.6.37       grid_4.4.2         
#> [37] rstudioapi_0.17.1   pbapply_1.7-2       CoprManager_0.5.7  
#> [40] lifecycle_1.0.4     nlme_3.1-166        vctrs_0.6.5        
#> [43] proxy_0.4-27        evaluate_1.0.1      glue_1.8.0         
#> [46] numDeriv_2016.8-1.1 fansi_1.0.6         colorspace_2.1-1   
#> [49] rmarkdown_2.29      tools_4.4.2         pkgconfig_2.0.3    
#> [52] htmltools_0.5.8.1
```
