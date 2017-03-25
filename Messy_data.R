#' ---
#' title: "Wrangling Messy Spreadsheets into Useable Data"
#' author: "Georges Monette and Heather Krause"
#' date: "`r format(Sys.time(), '%B %d, %Y at %H:%M')`"
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 6
#' ---
#' 
#' # Introduction
#' 
#' Probably the most important thing to do to ensure 'reproducible analyses' is
#' to '''never touch the raw data''' -- assuming the raw data comes from another
#' source: the web, a client, etc.
#' 
#' If you modify the data using a spreadsheet, you won't be able to justify the changes 
#' you made and ensure that you didn't introduce errors. When an updated version of the
#' raw data arrives, you will have to reproduce the whole process with no confidence that
#' you will achieve a comparable results. Therefore:
#' 
#' ___NEVER TOUCH THE RAW DATA___
#' 
#' 
#' as counterintuitive as that seems.
#' 
#' How do you change messy raw data into something you can analyze? You must
#' do this with a script so the steps are well documented, reproducible and
#' defensible.
#' 
#' This script illustrates 
#' the transformations when an updated version arrives 
#' 
#+ install_packages,echo=TRUE,eval=FALSE
# packages that may need to be installed:
install.packages('devtools', dependencies = TRUE)
install.packages('car', dependencies = TRUE)
install.packages('effects', dependencies = TRUE)
install.packages('snow', dependencies = TRUE)
install.packages('rbenchmark', dependencies = TRUE)
# install.packages('compiler', dependencies = TRUE) # no need to install, now in base
#' 
#+ setup,echo=FALSE,cache=FALSE,include=FALSE
z.size = c("normalsize", "tiny", "scriptsize", 
            "footnotesize", "small", "large", "Large", "LARGE", "huge", 
            "Huge")
knitr::opts_chunk$set(comment = "   ", prompt = FALSE, eval = FALSE,
                warning = TRUE,
                message = FALSE, 
                # eval = FALSE,
                error = TRUE, 
                size = "Huge",  # does not seem to do anything
                tidy = FALSE, cache = FALSE)
knitr::opts_knit$set(progress = TRUE, verbose = TRUE)
#' 
#' # Getting started -- and keeping going -- with R
#'   
#' 
#' This is an example of an R Markdown file written in 
#' R Notebook style, i.e. as an R script.
#' All your output from a series of R commands: the commands, 
#' the output and graphs are placed in a single HTML file. Optionally, output can also go into a pdf file or a doc file.
#' 
#' You can also include LaTeX in the text for the file, e.g. $Y = X \beta + \epsilon$.
#' 
#' Getting Started: 
#'   * [http://scs.math.yorku.ca/index.php/R](http://scs.math.yorku.ca/index.php/R)  
#' 
#' Installing two local packages
#' --------------------------
#' 
#' This scrips assumes that two packages available on GitHub have been installed,
#' yscs and p3d.
#' 
if(!require(spida2, quietly = TRUE)) devtools::install_github('gmonette/spida2')
if(!require(p3d, quietly = TRUE)) devtools::install_github('gmonette/p3d')
#' 
#' 
#' Importing data
#' --------------
#'   
#' Flat (rectangular) files:
#' - Easiest way of transferring to R: Use `.csv` files
#' - Main problem I've encountered: dates and times
#' - Missing data codes: 
#'     - Only one code in R
#'     - By default 'NA' in a character variable becomes missing data
#'     - Never been a problem until I had data on Namibia
#' 
#' Collections of hierarchical files:
#' - Transfer individual files and use `merge` in R to join files
#' 
#' An example where everything goes wrong.
#' 
#' Or "how to use R's strengths to deal with its weaknesses"
#' 
#' Usually, the easiest way to import a file is  
#' as a .csv file and this works 90% of the time  
#' 
#' Here's an example where everything goes wrong.   
#+ importing_data
# A .csv (comma-separated values) data set saved from Excel
library(spida2)
cat('time,country,gdp,smoke,health,cont
10/19/2012 08:48,CA,"40,000.00",50,75,America
12/31/2012 14:49,NA,"10,000.00",,50,Africa
01/09/2013 23:56,UK,"35,000.00",55,NA,Europe    
',
file = "dataex.csv")
ex <- read.csv("dataex.csv")
ex    # Namibia is missing!
#'
#' ####  Variables have different 'classes' depending on values
#' 
sapply(ex, class)
mean(ex$gdp)     # not numeric
mean(ex$time)    # not a time
mean(ex$smoke, na.rm = TRUE)  # OK
mean(ex$health, na.rm = TRUE) # OK
#'
#' ####  1: Fix Namibia
ex <- read.csv("dataex.csv", na.strings = NULL) 
#' now only blanks count as numeric missing values, so:
ex   # 'country' is OK
#' BUT:
sapply(ex, class) # The 'NA' for 'health' turns it into a factor
mean(ex$health, na.rm = TRUE)   # Not OK anymore
#'
#' #### 2: Fix health that is now a factor
#'
exfix <- ex
exfix$health <- as.numeric(as.character(ex$health))
exfix
mean(exfix$health, na.rm = TRUE)
#' Note: 'as.character' is essential above
as.numeric(ex$health)
unclass(ex$health)
ex$health
as.numeric( c("12.3","NA","2.1",NA,"1,000"))
#'
#' #### 3: Fix gdp using string substitution
#' 
#' * Note the power of 'regular expressions' for manipulating character variables
sub(",", "", as.character(ex$gdp))  # removes commas
exfix$gdp <- as.numeric(sub(",", "", as.character(ex$gdp)))
exfix$gdp 
mean(exfix$gdp)
sapply(exfix, class)
#'
#' #### 4. Fix time   
#'
x <- ex$time
x
print(x, quote = TRUE)
#' Transform to a date/time object with 'as.POSIXct'
as.POSIXct(x)  # not in ISO format
xfix <- as.POSIXct(x, format = "%m/%d/%Y %H:%M")
#' To get information on these codes, get help: ?strptime
xfix          # looks nice when it prints --- but in reality
class(xfix)   # the class of an object determines how it looks when it prints
#' Take away its class and you see the naked object:
unclass(xfix) # seconds since Jan 1, 1970, midnight GMT
#' As a POSIXct object, you can do lots of things with it
months(xfix)
#' What can you do with a POSIXct object
class(xfix)
methods(class = 'POSIXct')  # finding the tools that work on a POSIXct object
methods(class = 'POSIXt')   # finding the tools that work on a POSIXt object
mean(xfix)     # mean time
format(xfix,"%d")   # day of month
months(xfix)
weekdays(xfix)
quarters(xfix)
round(xfix,"hours")   # round to the nearest hour
xfix
xfix + 3600       # an hour later
xfix + 3600*24*7  # a week later
diff(xfix)        # elapsed time 
#' To get the hour as a character, use 'format'
format(xfix, format = "%H")
#' As a number:
as.numeric(format(xfix, format = "%H"))
#' to get the time of the day in hours
#' accurate to the nearest minute, write a function
hour <- function(x) {
  as.numeric( format(x,"%H")) + as.numeric( format(x, "%M")) /60
}
hour(xfix)
#' Finally you have a working data frame ready for analysis
exfix$time <- xfix
exfix$hour <- hour(xfix)
exfix
#' 
#' Early confusion
#' ---------------
#' 
#' ### Disparate methods to reference variables
#' 
#+ early_confusion
library(effects) # to get the 'Arrests' data set
?Arrests  
#' release or detention of individuals arrested for 
#' marijuana possession, Toronto 1997-2002
#' Source: Michael Friendly/John Fox
library(spida2)
#'
#' Using fully qualified names:
#'
head(Arrests$colour)
tab( Arrests$colour, Arrests$sex) # or
tab( Arrests[["colour"]], Arrests[["sex"]], pct = 1) 
#'
#' Using a formula evaluated in a data set:
#'
tab(~ colour + sex, Arrests, pct = 2, test = TRUE) %>% plot
#'
#' Using the name of the variable:
#'
with(Arrests, tab( colour, sex, pr = 2))
#'
#' Some functions use different conventions for different arguments
#'
#' ### Pitfalls with factors
#' 
#' See: [http://scs.math.yorku.ca/index.php/R/Traps_and_pitfalls](http://scs.math.yorku.ca/index.php/R/Traps_and_pitfalls)
#' 
#' Things you can do with R
#' ========================
#' 
#' Visualize 3D
#' ------------
#' 
#+ visualize_3d,eval=FALSE
library(p3d)
library(spida2)
#' Initialize a window
Init3d(cex=1.2)
Plot3d(LE ~ CigCon + HealthExpPC | Cont, Smoking, fov = 0, phi = 0, theta = 0,
    col = c('blue','red','orange','magenta','dark cyan','green'))
k <- function(stay = TRUE) rgl.bringtotop(stay = stay)
k()
Axes3d()
fit.lm <- lm(LE ~ CigCon, Smoking)
Fit3d(fit.lm, lwd = 2)
#' Is it significant?
summary(fit.lm)
coef(fit.lm)["CigCon"]
365 * coef(fit.lm)["CigCon"]   # Change in LE from inc. consumption by 1 cig./day
#' Should always look at data carefully
k()
Id3d(pad = 1)
k()
#'
#' Controlling for continent:
#' 
#' Stratification because we are analyzing within 'strata' or groups
#'
fit.lm.cont <- lm(LE ~ CigCon + Cont, Smoking)
summary(fit.lm.cont)
Fit3d(fit.lm.cont, lwd = 2)
#' getting output within each continent:
fit.lm.in.cont <- lm(LE ~ Cont/CigCon - 1, Smoking)
summary(fit.lm.in.cont)
Fit3d(fit.lm.in.cont, lwd = 2)
###
###  Controlling for another variable
###
Plot3d(LE ~ CigCon + HealthExpPC | Cont, Smoking, fov = 0, phi = 0, theta = 0,
col = c('blue','red','orange','magenta','dark cyan','green'))
Axes3d()
Id3d(pad = 1); k()
#' what does simple regression look like in 3d?
Fit3d(fit.lm)   # can't take HExp into account
Pop3d(2)
#' How can we try to take HExp into account?
#' Linear (on predictors) regression
fit.mr <- lm( LE ~ CigCon + HealthExpPC, Smoking)
Fit3d(fit.mr, col = 'pink')
Fit3d(fit.lm, col = 'blue')
rownames(Smoking)
#' Allowing for some curvature
fit.nl <- lm( LE ~ CigCon + HealthExpPC + log(HealthExpPC), Smoking)
Fit3d(fit.nl, col = 'cyan')
k()
spins()
#'
#'  - What happens if we use both Continent and HealthExp?
#'  - What happens with interactions
#' 
#' Estimating and visualizing complex models
#' -----------------------------------------
#' 
#+ complex_models
library(spida2)
library(effects)    # for the data
xqplot(Arrests)     # an exploratory
dd <- Arrests
dim(dd)
head(dd)
summary(dd)
plot( released ~ year, dd) 
#'
#' can use lapply to produce a plot for each year
#' 
par(mfrow = c(2,3))
lapply( levels(dd$Year), 
       function(yy) plot( released ~ colour, main = yy, subset(dd, year == yy)))
par(mfrow = c(1,1))
#' Try to model probability of being released
fit <- glm(released ~ colour + year + age + sex +
          employed + citizen + checks, dd,
          family = binomial)
summary(fit)   # note how R uses 'treatment' coding by default
#
#' Year is not significant!
#' But 
#' 1. the effect of year is not necessarily linear, and
#' 2. dynamics of police politics make it clear that
#'    races might not have been affected the same way
#'
#' Try year as a categorical variable interacting with
#' colour
#' 
dd$Year <- factor(dd$year)
fit2i <- glm(released ~ colour * Year + age + sex +
              employed + citizen + checks, dd,
              family = binomial)
summary(fit2i)
anova(fit, fit2i, test = "LRT")   # Likelihood ratio test
wald(fit2i, "Year")   # Uses regular expressions to match terms in model
wald(fit2i, "colour") 
wald(fit2i, ":")
#' 
#' Consider the Arrests data set:  The 'colour gap' seems to change considerably
#' from year to year.  We will estimate the adjusted color gap each year and
#' form year to year comparisons in the size of the color gap. In other words:

#' 1. Is there evidence of a gap each year?
#' 2. Is there evidence of a change in the gap each year?
#
#' Along the way we comment on 
#' Type I : sequential tests and   [use anova] 
#' Type II : added last among terms respecting the PoM [use car::Anova] 
library(car)       # for 'Anova' 
#
#' Suppose after some exploration and consideration we settle on
#' this working model:

fit0 <- glm( released ~ colour * (Year * age) + checks + citizen, dd,
            family = 'binomial')
summary(fit0)   #
#' could we simplify by getting rid of 3-way interactions?
wald(fit0,":.*:")   # using regular expressions to test a group of terms
#' fit a model with 2-way interactions:
fit <- update( fit0, . ~ colour * (Year + age) + checks + citizen)
summary(fit) 
#' What do the coefficients and p-values tell you?
#' 1. WHICH COEFFICIENTS CAN YOU INTERPRET? 
#' 2. AND WHAT DO THEY MEAN?
wald(fit, "colour") # overall test of colour
wald(fit, 'Year')   # note that lowest individual p-value is .03
wald(fit, ':Year')   # what does this tell you?
anova(fit, test= 'LRT')  # Type I
Anova(fit)               # Type II
#' Colour gap is a function of age and Year so we estimate it as
#' a function of age and year
cbind(coef(fit))
#' Note: Log odds of release:
#' eta.hat = b0 + b1 x colourWhite + b2 x Year1998 + ....
#' where terms are:
head( model.matrix( released ~ colour * (Year + age) + checks + citizen, dd))
summary(dd)
#' log odds of release for black 18 year old in 1997 with 0 checks and citizen
cbind(coef(fit))
L <- rbind( c(1, 0,   0,0,0,0,0,   18,  0, 1,  0,0,0,0,0, 0))
wald(fit, L)
qlogit <- function( p ) log( p/(1-p))  # p to log odds
plogit <- function( l ) 1/(1+exp(-l))  # log odds to p
plogit(2.025642)
#'
ww <- wald(fit, L)
coef(ww)
plogit(coef(ww))
#' Now, let's estimate the same thing for a 'white' person
cbind(coef(fit))
L.w18.98 <- rbind( c(1, 1,   0,0,0,0,0,   18,  0, 1,  0,0,0,0,0, 0))
wald(fit, L.w18.98)
plogit( coef( wald(fit, L.w18.98)))
#' Estimate the difference in log odds:
L.w18.98 - L
wald(fit, L.w18.98 - L)
#' We would like to do this for a range of ages and each year
#' You should be able to do it manually and will ask you to do this on exam
pred <- expand.grid( Year = levels( dd$Year),
                    age = c(18,20, 25, 40))
pred
cbind(coef(fit))
#' Use a list of expressions for each term
#' differencing with respect to colour (White - Black)
Lfx(fit)
Lfx(fit,
   list( 0,
         1 ,
         0 * M(Year),
         0 * age,
         0 ,
         0 ,
         1 * M(Year),
         1 * age 
   ),pred)
L <- Lfx(fit,
        list( 0,
              1 ,
              0 * M(Year),
              0 * age,
              0 ,
              0 ,
              1 * M(Year),
              1 * age 
        ),pred)
L
wald(fit, L)
ww <- wald(fit, L)
coef(ww)
pred <- as.data.frame(ww)
pred
td(col=rainbow(4), lty = 1, lwd = 2)
xyplot( coef ~ Year, pred, groups = age, type = 'l',
       auto.key = list(columns = 4, lines = T, points = F))
trellis.focus()
panel.abline(h = 0)
trellis.unfocus()

xyplot( coef+I(coef + 2*se)+
         I(coef - 2*se) ~ Year|age, pred, 
       auto.key = list(columns=4), type = 'b')
#' change colours
gd( lty = c(1,3,3), col = c('black','dark gray','dark gray'),
   lwd = 2)
xyplot( coef+I(coef + 2*se)+
         I(coef - 2*se) ~ Year|age, pred, 
       auto.key = list(columns=4), type = 'b')
#' more polishing
xyplot( exp(coef)+exp(coef + 2*se)+
         exp(coef - 2*se) ~ Year|
         paste("age:",age), pred, 
       type = 'b',
       ylab = "Odds ratio of release for White relative to Black",
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         panel.abline(h=1, lwd = 1)
       })
#'
#' #### Questions:
#'
#' 1. What would happen we had included the
#'    3-way interaction of colour*Year*age ?
#' 2. On reflection, might it be a good idea
#'    to include it even though it isn't 
#'    significant?
#' 3. How this relates to the general
#'    issue of a trade-off between bias and
#'    variance in statistics.
#' 4. Think of better ways of presenting the
#'    results for a lay audience.
#'
knitr::knit_exit()
#' Simulation and parallel processing
#' ----------------------------------
#'   
#'  Power by simulation for hierarchical data
#'  
#'  Find power to detect treatment effect on students in classes
#'  
#'   - within group, within class sd = 1
#'   - raw effect = 'effect size' in the sense of Cohen
#'
#' Parallel processing using Windows
#' see [http://homepage.stat.uiowa.edu/~luke/R/cluster/cluster.html](http://homepage.stat.uiowa.edu/~luke/R/cluster/cluster.html)
#' and [http://www.sfu.ca/~sblay/R/snow.html](http://www.sfu.ca/~sblay/R/snow.html)
#+ simulation_and_parallel_processing_NULL,eval=FALSE
library(snow)
cl <- makeCluster(4,type="SOCK") # Use the number of available cores, 
#' you can also use cores on other computers with which you are networked
system.time(zrn<- lapply(1:1000, function(x) mean(rnorm(100000))))
system.time(zrn<- parLapply(cl,1:1000, function(x) mean(rnorm(100000))))
#' parallel works on Macs and Linux:
install.packages('parallel', repos= "http://r-forge.r-project.org")
install.packages(file.choose(), repos= NULL)
library(multicore)
#' Generate a sample
#' N number of classes
#' M students per class
#' Exercise: extend to 3 levels
library(yscs)
library(p3d)
#
#'  Step 1:  Write a function that generates a single data set based on
#'  sample size, distribution assumptions and effect size(s)
#'  This is the step that makes you really think the model through
#
gen <- function(N,M,ICC,weff=0,beff=0){
  # generates a single data frame
  # N is number of classes (clusters)
  # M is number of students per class
  # ICC is the intraclass correlation for the response variable (without treatment)
  # weff is the effect size (relative to sigma) of a within-class treatment
  # beff is the effect size (relative to sigma) of a between-class treatment
  # make N and M even
  N <- round((2*N+1)/2)
  M <- round((2*M+1)/2)
  sigma2 <- 1                    # within cluster variance
  tau2 <- sigma2 * ICC/(1-ICC)   # between-cluster variance
  id <- rep(1:N,each = M)        # cluster id
  random.int <- sqrt(tau2)*rnorm(N)[id] # random intercept
  eps <- sqrt(sigma2)*rnorm(N*M)  # epsilon
  # Exercise: introduce correlation but beware 
  # of order relative to treatment assignment
  dd <- data.frame( id=id)
  dd$X <- 0:1   # note how easy this is because of recycling
  dd$W <- rep(0:1, each = N*M/2)
  sig <- sqrt(sigma2)
  dd$Y <- with(dd, X*weff*sig + W*beff*sig + random.int + eps  )
  dd 
}
#
gen(4,3,.5,1,1)
#
#
#' Step 2: Write a function that call the previous function, 
#' analyzes the data set and return observed p-values -- or
#' other output. 
#' 
#
gen.an <- function(N,M,ICC,weff=0,beff=0) {
 # generate and analyze, return p-vvalue
 fit <- try( lme( Y ~ X+W, gen(N,M,ICC,weff=weff,beff=beff), random = ~ 1 |id) )
 if( is(fit,"try-error")) return(NA)
 pvals <- as.matrix(wald(fit)[[1]]$estimate)[,'p-value']
 pvals
}
#
gen.an(100,5,.2,1,1)
#
#' Step 3: Write a function that does the above 'nsim' times 
#' and returns the proportion of p-values that are less that alpha (i.e. power)
#' The reason for the '...' will be obvious later.
#
gen.an.power <- function(N,M,ICC,weff=0,beff=0,alpha=.05,nsim=1,...){
 # proportion of rejections at level alpha
 ret <- replicate(nsim, gen.an(N,M,ICC,weff,beff), simplify = FALSE)
 # Proportion of p-values less that alpha
 nmiss <- sum( is.na(ret))
 nrep <- nsim - nmiss
 if (nrep==0) warning('no model converged')
 ret <- do.call(cbind, ret[!is.na(ret)])
 ret <- apply(ret<alpha, 1, mean)
 c(ret,nrep=nrep,nmiss=nmiss)
}
#
gen.an(4,3,.5,1,1)
gen.an.power(4,3,.5,1,1,nsim=10)
#
#
#' Step 4: Write a function that takes its input from the rows of a
#' data frame and appends power results as additional columns 
#
pow <- function(dd) {
 ret <- parSapply(1:nrow(dd), function( i) {
   do.call(gen.an.power, dd[i,])
   cat(".")
 })
cbind(dd,t(ret))
}
#
zd <- expand.grid( N = seq(10,30,10), M = 3:6,  weff = .2, beff = c(0, .2, .6))
zd$nsim <- 10
zd$alpha <- .05
zd$ICC <- .2
#
pow(zd)
#
#' Step 5: Create a data frame to explore power with a modest number of
#'         simulations
#
zd <- expand.grid( N = seq(10,100,10), M = 3:6,  beff = 0, weff = c(0, .2, .6))
zd$nsim <- 100
zd$alpha <- .05
zd$ICC <- .2
dim(zd)
#' 
#' Step 6: Explore with a modest number of simulations at first
#'     
#' Hours to increase simulations by factor of 10
system.time(
 dout <- pow(zd)
) *10 /3600
dout
#
#
xyplot( X ~ N | weff, dout, groups = M, type = 'b', lwd =2)
xyplot( X ~ I(M*N) | weff, dout, groups = M, type = 'b', lwd =2,xlim=c(0,100))
#
#
stopCluster(cl)

#' 
#' Speeding up loops with easy C functions
#' ---------------------------------------
#' 
#' Comparing raw R, compiled R and a C function.
#' 
#' This is a quick way to write a loop as a C function in R. This description assumes you are using Windows. A few small changes are required for other OSs.
#' First, install Rtools
#' The following R code illustrates a C function, its compilation and its inclusion in an R function.
#' 'benchmark' is used to compare the speed of of the raw R function, the compiled R function, the C function using a 'double' accumulator, and the C function using a 'long double' accumulator.
#' The speed advantage of using the C function compared with the raw R function is in the order of 100. The compiled R function runs roughly twice as fast as the raw function.
#' 
#' See http://scs.math.yorku.ca/index.php/R/C_functions_in_R_code
#+ speeding_up_loops_with_c_functions
install.packages('rbenchmark')
library(rbenchmark)
library(compiler)
#' Speeding up a function with a loop
#' Arises in HOA:
Rfun <- function(t, N = 100000) {
 tmp1 <- 0
 tmp2 <- 0
 tmp3 <- 0
 for (j in N:1) {
   tmp1 <- tmp1 + 0.5*log(j*(j+1)/(j*(j+1)-2*t))
   tmp2 <- tmp2 + 1/(j*(j+1)-2*t)
   tmp3 <- tmp3 + 2/((j*(j+1)-2*t))^2
 }
 c(tmp1, tmp2, tmp3)
}
Rfun( -475)
system.time(
 Rfun( -475)
)
#' If this is a function that needs to be called a very large number
#' of times, its speed could be a problem 
###
###  Using the recent R function compiler
###
#
Rfun.comp <- cmpfun(Rfun)
system.time(
 Rfun.comp( -475)
)
###
### Writing the inner loop in C (that's all the function is)
###
#
#' The function in C
cat('
   #include <R.h>
   #include <math.h>
   void fun(double *t, double *N, double *ret)
   {
   double tmp1, tmp2, tmp3, j;
   tmp1 = tmp2 = tmp3 = 0;
   for (j = *N; j > 0.9 ; j--) {
   tmp1 = tmp1 + 0.5*log(j*(j+1)/(j*(j+1)-2.0*t[0]));
   tmp2 = tmp2 + 1.0/(j*(j+1)-2.0*t[0]);
   tmp3 = tmp3 + 2.0/pow(((j*(j+1)-2.0*t[0])),2.0);
   };
   ret[0] = tmp1;
   ret[1] = tmp2;
   ret[2] = tmp3;
   ret[3] = j;
   }
   ', file = 'fun.c')
system( 'rm fun.dll')
system( "R CMD SHLIB fun.c" ) 
dyn.load("fun.dll")
#
#' The 'wrapper' function in R
#
Cfun <- function(t, N = 100000){
 ret <- double(4)
 ret2 <- .C('fun', t = as.double(t), N = as.double(N),
            ret = ret)  # ret reserves memory for the returned value
 ret2$ret 
} 
Cfun(-475)

#' Using long double as an accumulator to see what effect that has

cat('
   #include <R.h>
   #include <math.h>
   void funl(double *t, double *N, double *ret)
   {
   long double tmp1, tmp2, tmp3, j, Nl;
   tmp1 = tmp2 = tmp3 = 0;
   Nl = *N;
   for (j = Nl; j > 0.9 ; j--) {
   tmp1 = tmp1 + 0.5*log(j*(j+1)/(j*(j+1)-2.0*t[0]));
   tmp2 = tmp2 + 1.0/(j*(j+1)-2.0*t[0]);
   tmp3 = tmp3 + 2.0/pow(((j*(j+1)-2.0*t[0])),2.0);
   };
   ret[0] = tmp1;
   ret[1] = tmp2;
   ret[2] = tmp3;
   ret[3] = j;
 }
   ', file = 'funl.c')
system( 'rm funl.dll')
system( "R CMD SHLIB funl.c" ) 
dyn.load("funl.dll")

Cfunl <- function(t, N = 100000){
 ret <- double(4)
 ret2 <- .C('funl', t = as.double(t), N = as.double(N),
            ret = ret)
 ret2$ret 
}  

Cfunlp <- function(t, N = 100000){
 ret <- double(4)
 ret2 <- .C('funl', t = as.double(t), N = as.double(N),
            ret = ret, PACKAGE = "funl")
 ret2$ret 
}  

#' -475 is a plausible argument for this function
Rfun( -475) - Rfun.comp( -475,1000000)
Cfun( -475) - Cfunl( -475,1000000)
Rfun( -475) - Cfunl(-475,1000000)[1:3]
 

benchmark( Rfun( -475), Rfun.comp(-475),Cfun(-475),Cfunl(-475), 
          Cfunlp(-475), replications=10)

benchmark( Rfun( -475,1000000), Rfun.comp(-475,1000000),Cfun(-475,1000000),Cfunl(-475,1000000), replications=1)

#' 
#' Counting the number of packages available
#' ------------------------------------------
#'   
#+ counting_packages
repositories <- c(
 "http://probability.ca/cran/bin/windows/contrib/2.15",
 "http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.15",
 "http://download.r-forge.r-project.org/bin/windows/contrib/2.15"
)
names(repositories) <- repositories     # autonymous
repositories
packs <- lapply(repositories, available.packages)
packs <- lapply(packs, rownames)
lapply(packs, head)
lapply(packs, length)
sum(sapply(packs, length))     # number of available packages
length(unique(unlist(packs)))  # number of unique package names

#' 
#' Crazier things you can do with R
#' --------------------------------
#' 
#'   From http://zoonek2.free.fr/UNIX/48_R/02.html Programming in R
#' 
#+ crazy,eval=FALSE

#' Suppose you wanted to see all examples of the use of the 
#' 'match' function:

#' List all functions that use the function 'match'

a <- lapply(search(), ls)
names(a) <- search()
a <- unlist(a)
names(a) <- a
a <- a[ sapply(a, 
              function (x) { 
                try( x <- match.fun(x) )
                is.function(x)
              }) ]
a <- lapply(a, match.fun)
b <- lapply(a, deparse)
b <- lapply(b, length)
b <- order(unlist(b))
a <- a[b]
i <- lapply(a, function(x) { length(grep("match\\(", deparse(x)))>0 })
i <- unlist(i)
i
length(a[i])
length(a)
a[i]
