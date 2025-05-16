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

if(!require(dplyr)){
  install.packages("dplyr", dependencies = T)
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2", dependencies = T)
  library(ggplot2)
}

## and also perform forward-in-time simulations.
if(!require(AlphaSimR)){
  install.packages("AlphaSimR", dependencies = T)
  library(AlphaSimR)
}

## This package helps to customize the theme of the app.
if(!require(bslib)){
  install.packages("bslib", dependencies = T)
  library(bslib)
}

## Among other things, this package helps to arrange ggplots in a grid
if(!require(ggpubr)){
  install.packages("ggpubr", dependencies = T)
}

