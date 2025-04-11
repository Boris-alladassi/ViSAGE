# ViSAGE

## 1. Summary

This R shiny app was developed to use basic **Vi**sualization and **S**imulation for **A**dvancing Quantitative **G**enetics **E**ducation.\
This document provides instructions on how to test the R Shiny App being developed.

## 2. Instructions

### 2.1. Downloading the app

The repository can be downloaded *ad hoc* by click

### 2.2. Setting working directory

To begin, open R or R Studio and set your working directory to this folder ***ViSAGE***.\
You may use the keyboard shortcut `Ctrl+Shift+H` (for Windows, probably `Cmd+Shift+H` for Mac), or you may edit the code below to set the working directory to the path where the directory is located on your computer.

`setwd("C:/Users/aboris/Documents/Postdoc/code/ViSAGE")`

### 2.3. Source the *app.R* file

Next, source the *app.R* file to get the Shiny App ready for running. Please use the code below after you have set your working directory to ViSAGE.

`source("./app.R")`

This may take a while, as it will install all the required R packages, that you currently do not have.

### 2.4. Run the Shiny app

You can now run the Shiny app using the code below.

`shiny::runApp()`

### 2.5. Testing the app

At this point, the app should be up running. Please test the app as thoroughly as you can. Explore both the Standard and Fun panels.\
Record any abnormal behavior and share your feedback and suggestions with me at [**aboris\@illinois.edu**](mailto:aboris@illinois.edu)

## 3. Description

The app has two panels, *Standard* and *Fun Panel*. The Standard panel will simulate additive genetic architectures of plant height, girth (stem diameter), and tassel length of maize. The fun panel simulates relatively simple additive genetic architectures of three traits in maize, avocado, and strawberry.

### 3.1. User inputs

First the user needs to select the crop of interest, then trait to be simulated. Next, the user can control the following selection decisions:

- *selection direction* (higher values or lower values),
- *selection intensity* in terms of percentage of selected individuals per generation, and
- *genetic gain metric* (i.e., 25th, 50th, 75th, or 100th percentile).\

Once the selection decisions are made, the user should click the ***Simulate*** button. The app will produce three summary graphs showing the  trend of the population means across generations for the three traits. Additionally, the simulation results from R are used to create cartoons of plant phenotypes depicting phenotypic changes across generations using JavaScript.
