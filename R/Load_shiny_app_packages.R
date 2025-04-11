### The code below will install and/or load all the require packages for the shiny app
## Install and or load the R package shiny
if(!require(shiny)){
  install.packages("shiny", dependencies = T)
  library(shiny)
}

if(!require(shinyjs)){
  install.packages("shinyjs", dependencies = T)
  library(shinyjs)
}
## Install and or load the R package tidyverse
if(!require(tidyverse)){
  install.packages("tidyverse", dependencies = T)
  library(tidyverse)
}

## This package helps to simulate the founder population with the haplotypes
## and also perform forward-in-time simulations.
if(!require(AlphaSimR)){
  install.packages("AlphaSimR", dependencies = T)
  library(AlphaSimR)
}

## Install the R package SNPRelate, a dependency of simplePHENOTYPES
if (!require("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

if (!require("SNPRelate", quietly = TRUE)){
  BiocManager::install("SNPRelate")
  library(SNPRelate)
}

## This package helps to simulate phenotypes of the populations given
## a particular genetic architecture.
if(!require(simplePHENOTYPES)){
  install.packages("simplePHENOTYPES", dependencies = T)
  library(simplePHENOTYPES)
}

## This package helps to customize the theme of the app.
if(!require(bslib)){
  install.packages("bslib", dependencies = T)
  library(bslib)
}

## Among other things, this package helps to arrange ggplots in a grid
if(!require(ggpubr)){
  install.packages("ggpubr", dependencies = T)
  library(ggpubr)
}

## This package helps to a GIF of many ggplots.
if(!require(gganimate)){
  install.packages("gganimate", dependencies = T)
  library(gganimate)
}

if(!require(reshape2)){
  install.packages("reshape2", dependencies = T)
  library(reshape2)
}