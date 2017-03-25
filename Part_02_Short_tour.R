#' ---
#' title: "Short Tour of R"
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
#+ echo=FALSE
library(knitr)
# opts_chunk$set(comment=NA, fig.width=6, fig.height=6))
opts_chunk$set(comment=NA)
#' 
#' This tour of R is inspired largely by the introduction to R in
#' Venables and Ripley (2002) Modern Applied Statistics with S, 
#' 4th ed., Springer [web site](http://www.stats.ox.ac.uk/pub/MASS4/)
#'
#' Objects like numbers in R come in vectors.  A single number
#' is a vector of length 1. There are four basic modes for
#' vectors: 
#' 
#' 1. logical: e.g. TRUE, FALSE, F, T, NA
#' 2. numeric: 1, 2.1, ...
#' 3. complex: 1 + 3.1i, 1i, ...
#' 4. character: "this is character string"
#' 
#' ### Logical
#' 
TRUE
TRUE + 2   # what happens here? This is an example of 'coercion'. 'TRUE' is coerced to the same mode as '2' before applying the addition operator
FALSE
FALSE + pi  # another example of coercion. 'pi' is a special symbol 
TRUE & FALSE # the AND logical operator
TRUE | FALSE # OR
! TRUE  # '!' means NOT
#'
#' R uses 3-valued logic, including NA, i.e. 'missing', as a logical value --- with interesting results:
#'
TRUE & NA  # of course
TRUE | NA  # think about this one!!
#'
#' It's easy to produce an operator table. For the '|' (OR) operator:
outer(c(TRUE,FALSE,NA),c(TRUE,FALSE,NA), '|')
#' The labels are not satisfying and we could have done this:
#' 
#' 1. Assign a vector to a name, using the 'c' (catenation) function:
x <- c(TRUE,FALSE,NA)
x
#' 2. Give the vector names:
names(x) <- c('TRUE','FALSE','NA') 
x
outer(x,x,'&')
#' Question: What happens if I used
names(x) <- x
x
outer(x,x,'&')
#' 'x' was coerced to a character vector and the logical 'NA' 
#' became '<NA>' to help distinguish it from the character vector
#' 'NA', e.g. a code for North America or Namibia.
#' 
#' ### Numeric and complex
#' 
2 + 3
sqrt(3/4)/(1/3 + 2/pi^2)
factorial(5)
sqrt(-1)       # square root of -1 (as a real number)
sqrt(-1+0i)    # square root of -1 (as a complex number)
#' R uses the extended reals including Inf, -Inf, NA and NaN (for not a number)
#'  
1/0
0/0
Inf/Inf
Inf + Inf
Inf - Inf
-1/0
(2-1i)/0  # 4 infinities!! In the complex plane there should only be one.
#' 
#' Loading some package we installed in the last chapter.
#' 
library(MASS)      # for special functions and data sets in MASS
library(lattice)   # for graphics -- I almost always use this -- 
library(spida2)
library(p3d)
#' 
#' A data set in MASS
chem      # a dataset in MASS
?chem     # to get a description
mean      # functions are 'objects' just like data
?mean
mean(chem)
#+ warning=FALSE,echo=FALSE
rm(m)
#+ error=T
m  # check to see if it's used
m <- mean(chem)
v <- var(chem)/length(chem)
m/sqrt(v)
pnorm
pt(m/sqrt(v),length(chem) -1)
(1-pt(m/sqrt(v),length(chem) -1))*2    # p - value

# Object oriented programming:
# e.g. How linear models work
#

z <- lm(chem ~ 1)
unclass(z)
summary(z)

# writing a function

std.dev <- function(x) sqrt(var(x))
std.dev( chem)
std.dev( c(1,2,3,4,3,4,5,6,67))
t.test.p <- function(x, mu=0) {
	n <- length(x)
	t <- sqrt(n) * ( mean(x) - mu ) / std.dev(x)
	2 * (1 - pt(abs(t), n - 1))
}

t.test.p(chem)

# t value and p value:

t.stat <- function(x, mu=0) {
	n <- length(x)
	t <- sqrt(n) * (mean(x) - mu) / std.dev(x)
	list(t = t, p = 2 * (1 - pt(abs(t), n - 1)))
}

t.stat(chem)

z <- rnorm(300, 1, 2)  # generate 300 N(1, 4) # simulation
densityplot(z)
t.stat(z)

unlist(t.stat(z))


# Lattice graphics

x <- rnorm(1000)

y <- rnorm(1000)

truehist(c(x,y+3), nbins = 25)

?truehist

plot(x, y)
dd <- con2tr(kde2d(x,y)) # create a data frame with densities

?kde2d
?con2tr


contourplot(z ~ x + y, data=dd, aspect=1)
wireframe(z ~ x + y , data=dd, drape=T)
levelplot(z ~ x + y , data=dd, aspect=1)

######################################################

x <- seq(1, 20, 0.5)
x
w <- 1 + x/2  # adding a scalar to a vector
y <- x + w*rnorm(x)

?rnorm

# creating a data frame

dum <- data.frame( x, y, w)
dum
rm (x,y,w)   # remove orginal x,y,w in working directory

fm <- lm( y ~ x , data=dum)
summary(fm)

fm1 <- lm( y ~ x , data=dum, weight= 1/w^2)
summary(fm1)

lrf <- loess( y ~ x, dum)      # smooth regression

with(dum, plot(x,y))							# standard scatterplot

with(dum, lines(spline(x,fitted(lrf)),col=2))		# add fit with spline interp.

abline(0,1,lty=3,col=3)			# "true" line

abline(fm, col=4)					# unweighted regression

abline(fm1, lty=4, col=5)		# weighted regression

plot(fitted(fm), resid(fm),
	xlab = "Fitted values",
	ylab = "Residuals")
	
qqnorm(resid(fm))
qqline(resid(fm))

rm(fm,fm1,lrf,dum)


#=================================================================
#
#  Statistical experimentation:
#  - the effect of outliers

plot(c(0,1),c(0,1),type="n")

xy <- locator(type='p')     # click on plot, include an outlier, end with ESC

xy

abline(lm(y ~ x, xy), col=4)
abline(rlm(y ~ x, xy, method="MM"), lty=3, col=2)

rm(xy)


#==================================================================
#
#  Interactive identification of points
#
#  and different ways of identifying variables in data frames  
#

head(Smoking2)  # in p3d
?Smoking2  # some data frames have help files
Smoking2$LE   # fully qualified name
with(Smoking2, LE)  # 'with' specifies environment in which 'LE' is found

plot(Smoking2$Smoking, Smoking2$LE)
with(Smoking2, plot(Smoking, LE))   # labels are nicer

?identify
with( Smoking2, identify(Smoking, LE, labels = Country))  # bug in RStudio, labels don't show up until end
abline(lm(LE ~ Smoking, Smoking2))     # we don't need 'with' because 'lm' uses a formula interface

windows()  # open a separate plotting device
with( Smoking2, plot(Smoking, LE))    
with( Smoking2, identify(Smoking, LE, labels = Country))  
abline(lm(LE ~ Smoking, Smoking2))    

with( Smoking2, lines( supsmu(Smoking, LE ), col = 'blue', lwd =2))   # non-parametric fit
with( Smoking2, lines( supsmu(Smoking, LE, bass = 5 ), col = 'red', lwd =2))   # non-parametric fit

# Lattice graphics

library(lattice)
library(spida2)
gd( pch = 16, cex = 1) # in spida2
xyplot( LE ~ Smoking, Smoking2, groups = Continent, auto.key = T)
xyplot( LE ~ Smoking | Continent, Smoking2,  groups = cut(HE,5),auto.key = T)
gd(5, cex = .8)
xyplot( LE ~ Smoking | Continent, Smoking2,  groups = cut(HE,5),auto.key = T)

#===================================================================
# 
#  Michelson Morley experiments on speed of light
#  - "Speed" is speed in km/sec - 299,000  ("true" is 734.5)
#
# Example of using variables by 'attach'ing a data frame: highly discouraged

attach(michelson)
summary(michelson)
search()

plot(Expt,Speed,
	main="Speed of light data",xlab="Experiment No.")

abline(h=734.5)

fm <- aov(Speed ~ Run + Expt)
summary(fm)


fm0 <- update(fm, . ~ . - Run)
summary(fm0)
anova(fm0,fm)

detach()
rm(fm, fm0)

#===================================================================
# 
#  3D interactive plots
#  Illustrating multiple regression, interaction, 
#    and the estimation of conditional effects
#

library(p3d)
Init3d( family = 'serif',cex=1.2)

Plot3d( LE ~ Smoking + HE | Continent, Smoking2 ,fov= 0, phi=0)
fg()
Id3d(pad=1)
fg()
Plot3d( LE ~ Smoking + HE | Continent, subset(Smoking2, Continent != "Africa") ,fov= 0, phi=0)

Id3d(pad=1)
Axes3d()
Plot3d( LE ~ Smoking + HE | Continent, Smoking2 ,fov= 0, phi=0)
Id3d(pad=1)
fitm <- lm( LE ~ Smoking , Smoking2)
summary( fitm )
Fit3d(fitm)

fit <- lm( LE ~ Smoking + HE, Smoking2)
summary(fit)
Fit3d( fit, col = 'green')
Pop3d(2)

# changing the model for the controlling variable changes the picture considerably
fit2 <- lm( LE ~ Smoking + HE + log(HE), Smoking2)
summary(fit2)
Fit3d( fit2, col ='red')

fit3 <- lm( LE ~ Smoking * (HE + log(HE)), Smoking2)
summary(fit3)
Fit3d( fit3, col ='blue')

wald(fit3)

# Linear hypothesis matrix and test of effect of CigCon | HE 
# How: differentiate model terms wrt Smoking

Lm <- list( "Change in LE assoc. with 1 unit change in CC" =   # creates a list of objects
  rbind(                                       # 'binds' rows together into a matrix
  'HE =100'   = c(0,1,0,0,100,log(100)),
  'HE = 1000' = c(0,1,0,0,1000,log(1000)),     # take the derivative wrt CigCon
  'HE = 3000' = c(0,1,0,0,3000,log(3000)),
  
  'HE = 5000' = c(0,1,0,0,5000,log(5000))))

Lm

wald(fit3, Lm)

# Linear hypothesis matrix and test of effect of CigCon | HE 
# How: differentiate 

wald(fit3)

Lm2 <- list("Change is LE assoc. with $1 increase in HealthExpPC" =
  rbind( 
  'HE = 100, CG = 1000'  = c(0,0,1,1/100,1000,1000/100),   # take the derivative wrt HE
  'HE = 3000, CG = 1000' = c(0,0,1,1/3000,1000,1000/3000),
  'HE = 6000, CG = 1000' = c(0,0,1,1/6000,1000,1000/6000)))

Lm2

wald(fit3, Lm2)
      
      


      
 
