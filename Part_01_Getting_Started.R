#' ---
#' title: "Getting Started with R"
#' author: 
#' - name: Georges Monette
#'   affiliation: York University
#' date: "`r format(Sys.time(), '%B %d, %Y at %H:%M')`"
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 6
#' ---
#' 
#' ## Installing R
#' 
#' To install R visit 
#' [this 'mirror' of the The Comprehensive R Archive Network](https://cloud.r-project.org/) 
#' and follow the instructions for your operating system: 
#' Mac OS X, Windows or Linux. 
#' 
#' Read the information carefully. 
#' If you use Windows, install 'Rtools' as suggested. 
#' If you use Mac OS X, consider whether you need 
#' to install XQuartz.
#' 
#' After installing R, next install 
#' [R Studio](https://www.rstudio.com/products/rstudio/#Desktop).
#' 
#' Once R Studio is installed, open it with the icon 
#' on your desktop. We will use the console in R Studio to
#' install some packages.
#'  
#' There are three main sources of packages for R:
#' 
#' 1. Many packages come with R when you install it initially.
#' 2. Most additional packages you might consider installing
#'    reside on 'CRAN', the Comprehensive R Archive Network, 
#'    that provides relative stable versions of packages 
#'    that have passed some automated tests, and
#' 3. 'github.com' where most developers provide access to the latest
#'    beta versions of their packages or to packages 
#'    that have not been sent for inclusion in CRAN.
#'     
#' The first package we install resides on CRAN and it is needed to 
#' install packages from GitHub.
#'
#+ eval=FALSE 
install.packages('devtools')
#' 
#' While we are at it we can install a few more packages from
#' CRAN:
#'
#+ eval=F 
install.packages(c('car','latticeExtra', 'rgl', 'Hmisc'))
#' 
#' Installing packages only needs to be done once every 
#' time you install a new version of R. You can 
#' update them occasionally, with:
#' 
#+ eval=FALSE
update.packages()
#' 
#' Then we can install two GitHub packages:
#' 
#+ eval=FALSE
devtools::install_github('gmonette/spida2')
devtools::install_github('gmonette/p3d')
#' 
#' These packages are updated frequently and you need to reinstall
#' them to have access to the latest verions.  
#' 
#' Each time you use R, you need to load the packages you need
#' for that session with the 'library' command: 
#' 
#+ eval=F
library(car)
library(spida2) 
library(p3d)
#'
#' Once everything is installed, try the following commands to see
#' if everything worked:
#' 
#+ eval=F
head(Smoking2)
#
# LE is a measure of life expectancy, 
# HE is health expenditures per capita and
# Smoking is average cigarette consumption per person per year.
#
Init3d()
Plot3d( LE ~ HE + Smoking | Region, Smoking2)
fg()
# 
# Drag the bottom right corner of the window to make 
# and drag the top of the window to position it 
# conveniently.
#
# Note that you can adjust the perspective 
# by depressing the right mouse button and moving
# the cursor up and down.
#
# You can rotate the plot 
# by depressing the left mouse button. 
# If you have a scroll wheel, you can use it
# to zoom in and out.
Axes3d()
Id3d()
# To identify points, depress the right mouse button
# and drag the cursor to form a box around the points you
# want to identify.
#
# To finish, drag a box around a region with no points.
#
# Here is a multiple regression of 
# Life Expectancy on Smoking alone:
fit <- lm( LE ~ Smoking, Smoking2)
summary(fit)
Fit3d(fit)
# of Life Expectancy on Smoking and Health Expenditures:
fit2 <- lm( LE ~ Smoking + HE, Smoking2)
summary(fit2)
Fit3d(fit2, col = 'green')
# - of Life Expectancy on Smoking, HE and log(HE):
fit3 <- lm( LE ~ Smoking + HE + log(HE), Smoking2)
summary(fit3)
Fit3d(fit3, col = 'red')
# - of Life Expectancy on Smoking and log(Health Expenditures):
#
# Most people find it easier to visualize the fitted surfaces
# if they adjust the perspective by holding the right
# mouse button and moving the cursor up or down until the
# amount of foreshortening creates as strong an impression 
# of 3 dimensions as possible.
#
#' 
#' ## Exercises:
#' 
#' 1. Examining the 3D plots above, what are the 3 countries with the
#'    lowest life expectancy?
#' 2. What principles do the three models above illustrate 
#'    if your main interest is to assess the 'effect' of cigarette
#'    consumption on life expectancy? 
#'    