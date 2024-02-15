
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NMRcycling

<!-- badges: start -->
<!-- badges: end -->

The goal of `NMRcycling` is to provide the R code that is described in
the chapter *NMR based Metabolomics: monitoring physiological response
to physical exercise* in the book *A Practical Guide to Metabolomics
Applications in Health and Disease*. This package contains the script
and the functions to work with the NMR data. The data itself is stored
in a second repository,
[NMRcycling_data](https://github.com/CPM-Metabolomics-Lipidomics/NMRcycling_data).
The data is also in the package in order to build the vignette.

## Installation

Before you can install `NMRcycling` several packages are needed. These
packages can be installed with:

``` r
install.packages(c("factoextra", "ggplot2", "ggrepel", "gridExtra", "httpuv",
                   "patchwork", "INIFA", "knitr", "prospectr", "readxl",
                   "rmakrdown", "sessioninfo", "tidyr"))

# install.package("BiocManager")
BiocManager::install("AlpsNMR")
```

Next, you can install `NMRcycling` with:

``` r
# install.packages("remotes")
remotes::install_github("CPM-Metabolomics-Lipidomics/NMRcycling", build_vignettes = TRUE)
```

Have a look at the vignette with:

``` r
browseVignettes(package = "NMRcycling")
```

## Online version

You can also run the `NMRcycling` notebook on Google Colab. No software
installation necessary, you only need a Google account. Just go to
[NMRcycling Google Colab
site](https://colab.research.google.com/drive/1a71rNVgBCjXN-OeUnPt15IvdPgydXYXz?usp=sharing),
and click “Run all” in the “Runtime” pull down menu.
