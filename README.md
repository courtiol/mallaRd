---
output:
  github_document:
    df_print: kable
---

<!-- README.md is generated from README.Rmd. Please edit that file -->




# mallaRd

<!-- badges: start -->
<!-- badges: end -->

The goal of mallaRd is to reproduces the analyses and results from the paper "Housing search in the concrete jungle – breeding site fidelity of urban mallards and implications for conservation management strategies" by Engler et al. (in prep). 

## Installation and usage

You can install our package mallaRd from [GitHub](https://github.com/) with:


``` r
install.packages("remotes") ## install the package remotes if you don't have it
remotes::install_github("courtiol/mallaRd") ## install our package
```

Then load the package in your R session and check the main help page where we placed all analyses:


``` r
load("mallaRd")
?mallaRd
```

## Raw data

You can access the raw data used for the paper [here](inst/extdata/raw_data.csv) or via the R package:


``` r
head(data_raw) ## first 6 rows
#>   ID year       date      species location_ID location_lat location_long habitat_type floor_level body_mass_g wing_length_mm hatch_date clutch_size brood_size ring_number
#> 1  1 2005 01.04.2005 mallard_duck         729     52.52495      13.30129    courtyard          NA          NA             NA                     NA         NA      JC8002
#> 2  2 2005 04.04.2005 mallard_duck        1116     52.60742      13.23319        other          NA          NA             NA                     NA          1            
#> 3  3 2005 25.04.2005 mallard_duck         720     52.52412      13.34539 roof_terrace          NA          NA             NA                     NA          8            
#> 4 11 2005 26.04.2005 mallard_duck         555     52.51043      13.19960      balcony          NA         750            257 26.04.2005          12         12     JC54565
#> 5 12 2005 27.04.2005 mallard_duck         991     52.56289      13.20875    courtyard          NA          NA             NA                     NA          3            
#> 6 13 2005 29.04.2005 mallard_duck         115     52.44341      13.58433      balcony          NA         830            272 29.04.2005          NA         10     JC52194
#>   release_site_lat release_site_long     DNSW PSW1000 PSW2000 trafficvolume500 populationdensity500 trafficvolume1000 populationdensity1000 trafficvolume2000 populationdensity2000
#> 1         52.54819          13.31221 276.1204       1       1        1018.6459            143.70396        1679.24304              88.71829         1297.3898              93.85891
#> 2               NA                NA 929.5084       1       1         158.7811             25.50148          72.82336              23.18329          131.7400              14.51628
#> 3               NA                NA 127.3921       1       1         843.3160            127.61779         743.98803             147.67042          858.6173              78.80484
#> 4         52.51088          13.20227 162.9485       1       1         734.5966             23.64657         529.66019              21.79157          339.1518              26.49088
#> 5               NA                NA 426.5027       1       1           0.0000            138.54895         117.49123              67.91985          111.4260              38.82886
#> 6         52.44400          13.62276  80.8951       1       1         108.0740            114.83922         172.58178              59.27967          208.3172              44.26453
```

## Developer corner

Our package relies on the following packages:


``` r
pkgs <- desc::desc_get_deps()
pkgs <- pkgs[pkgs$package != "R", ]
pkgs$version_used <- sapply(pkgs$package, \(pkg) paste(packageVersion(pkg), sep = "."))
pkgs
#>        type    package version version_used
#> 1   Imports    cowplot       *        1.1.3
#> 2   Imports      dplyr       *        1.1.4
#> 3   Imports      furrr       *        0.3.1
#> 4   Imports     future       *       1.34.0
#> 5   Imports    ggplot2       *        3.5.1
#> 6   Imports         sf       *       1.0.17
#> 7   Imports   showtext       *        0.9.7
#> 8   Imports      spaMM       *        4.5.0
#> 9   Imports     tibble       *        3.2.1
#> 10  Imports      tidyr       *        1.3.1
#> 11  Imports tidyselect       *        1.2.1
#> 12 Suggests       desc       *        1.4.3
#> 13 Suggests     doSNOW       *       1.0.20
#> 14 Suggests     DHARMa       *        0.4.6
#> 15 Suggests   spelling       *        2.3.0
#> 16 Suggests   devtools       *        2.4.5
#> 17 Suggests      knitr       *         1.48
```

Here is the information of the R & RStudio environment used to run all the analyses:


``` r
sessionInfo()
#> R version 4.4.1 (2024-06-14)
#> Platform: x86_64-redhat-linux-gnu
#> Running under: Fedora Linux 40 (Forty)
#> 
#> Matrix products: default
#> BLAS/LAPACK: FlexiBLAS OPENBLAS-OPENMP;  LAPACK version 3.11.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: Europe/Berlin
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] mallaRd_0.1.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] gtable_0.3.5         xfun_0.47            ggplot2_3.5.1        htmlwidgets_1.6.4    devtools_2.4.5       remotes_2.5.0.9000   processx_3.8.4       lattice_0.22-6      
#>  [9] numDeriv_2016.8-1.1  callr_3.7.6          vctrs_0.6.5          tools_4.4.1          ps_1.8.0             generics_0.1.3       parallel_4.4.1       curl_5.2.3          
#> [17] proxy_0.4-27         tibble_3.2.1         crancache_0.0.0.9001 fansi_1.0.6          RSQLite_2.3.7        blob_1.2.4           pkgconfig_2.0.3      Matrix_1.7-0        
#> [25] checkmate_2.3.2      desc_1.4.3           lifecycle_1.0.4      compiler_4.4.1       munsell_0.5.1        httpuv_1.6.15        htmltools_0.5.8.1    usethis_3.0.0       
#> [33] later_1.3.2          pillar_1.9.0         crayon_1.5.3         urlchecker_1.0.1     MASS_7.3-61          ellipsis_0.3.2       cachem_1.1.0         sessioninfo_1.2.2   
#> [41] boot_1.3-31          nlme_3.1-166         spaMM_4.5.0          mime_0.12            tidyselect_1.2.1     digest_0.6.37        slam_0.1-53          dplyr_1.1.4         
#> [49] purrr_1.0.2          rematch2_2.1.2       fastmap_1.2.0        grid_4.4.1           colorspace_2.1-1     cranlike_1.0.3       cli_3.6.3            magrittr_2.0.3      
#> [57] pkgbuild_1.4.4       utf8_1.2.4           backports_1.5.0      scales_1.3.0         promises_1.3.0       rappdirs_0.3.3       bit64_4.0.5          registry_0.5-1      
#> [65] bit_4.5.0            ROI_1.0-1            pbapply_1.7-2        memoise_2.0.1        shiny_1.9.1          parsedate_1.3.1      evaluate_1.0.0       knitr_1.48          
#> [73] miniUI_0.1.1.1       profvis_0.4.0        rlang_1.1.4          Rcpp_1.0.13          xtable_1.8-4         glue_1.7.0           DBI_1.2.3            pkgload_1.4.0       
#> [81] minqa_1.2.8          debugme_1.2.0        jsonlite_1.8.9       R6_2.5.1             fs_1.6.4
```

