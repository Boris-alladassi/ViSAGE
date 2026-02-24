
# **ViSAGE**
**Vi**sualization and **S**imulation for **A**dvancing **G**enetics **E**ducation. 

<!-- badges: start -->
<!-- badges: end -->
## About ViSAGE

**ViSAGE** is an R shiny app developed to conduct simulations and analyses of quantitative traits often encountered in plant and animal breeding.  
    
The app provides an interactive interface for users to:
- Simulate quantitative traits with additive, dominance, and epistasis architectures.
- Conduct multi-generation selection using either a directional, stabilizing or diruptive selection type.
- Conduct genome-wide association studies (GWAS) and genome-wide prediction analyses.  

This document provides instructions on how to install and run ViSAGE. Thanks for using **ViSAGE**!

## Installation

Run the code below to install the development version of ViSAGE from [GitHub](https://github.com/):

``` r
install.packages("pak")
pak::pak("Boris-alladassi/ViSAGE")
```

## Run ViSAGE 

After installing the package, you can run the app using the code below:

``` r
library(ViSAGE)
ViSAGE::run_visage()
```

## Authors

If you have any questions, comments, or suggestions, please feel free to reach out to us!  
Boris M.E. Alladassi [aboris@illinois.edu](mailto:aboris@illinois.edu)  
Alex E. Lipka [alipka@illinois.edu](mailto:alipka@illinois.edu)  

