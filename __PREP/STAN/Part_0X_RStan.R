#' ---
#' title: "Getting Started with RStan"
#' author: "MATH 6635"
#' date: "`r Sys.Date()`"
#' output: 
#'  html_document:
#'    toc: true
#'    toc_depth: 4
#'    keep_md: yes
#' ---
#+ setup, include=FALSE, eval=FALSE
install.packages('devtools')
devtools::install_github('gmonette/spida2')
devtools::install_github('gmonette/p3d')
install.packages('magrittr')  # to use the '%>%' pipe imported from magrittr to dplyr
install.packages('latticeExtra')
# we will need more distribution than in the base and stats packages
install.packages('extraDistr')
#+ load, include=FALSE
library(magrittr) # the original package for the '%>%' pipe
library(lattice)
library(latticeExtra)
library(spida2)
library(knitr)
library(extraDistr)
library(rgl)
opts_chunk$set(comment = NA, warning = FALSE)
opts_knit$set(progress = TRUE, width = 90)
options(width=90)
knit_hooks$set(webgl = hook_webgl)

library(p3d)
library(spida2)
#'
#' ## Installing RStan
#' 
#' Installing RStan is a bit more complicated than installing an ordinary package because it needs
#' to use a C++ compiler which you must also install.
#' 
#' First go to this [site](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#prerequisites)
#' and in the section entitled "Installation" click on the link corresponding to your OS. Follow the
#' relatively lengthy instructions slowly and carefully.
#' 
#' Once you've finished the installation, continue with ['How to Use RStan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-use-rstan)
#' 
#' Work through the ['Eight-schools' examples](Eight_schools.R).
#' 
#' 
#' 
#' 
#' 
#'