###
###
###  Lab Session Section 3: Generalized linear mixed models and related topics
###

##
## Related Topics:
##

#*     Accelerated Longitudinal Designs and age-period-cohort linear confounding
#*     Modeling seasonal and periodic effects with Fourier Analysis
#*     Using general splines to model effect of age or time
#*     Linear, quadratic, cubic and natural cubic splines
#*     General spline generator: splines with arbitrary degrees and smoothness
#*     Defining hypothesis matrices and using Wald tests to explore splines
#*     Plotting splines and spline features with confidence bounds
#*     Plotting log-odds or probabilities
#*     Interpreting hypothesis tests using confidence bounds
#*     Bonferroni and Scheffe confidence bound adjustment factors
#*     Testing non-linear cohort effects
#*     Alternatives to glmmPQL: lmer, glmmML,GLMMGibbs,


library( spidadev )
library( p3ddev )


# Also need later:
#
# library( GLMMGibbs )
# library( lme4 )
# library( glmmML )
#

#
#  Data: Respiratory infections among pre-school children in Indonesia
#     - 274 children were oberved at community clinics up to 6 times 3 months apart
#     - Baseline age ranges from 4 to 80 months
#       Question: Experience suggests that boys get more infections than
#                 girls especially around 3 to 4 years of age.
#
#  This data set is particularly interesting because it has a structure very
#  similar to Statistics Canada longitudinal surveys like the NPHS and the
#  NLSCY.  There are 6 waves of data with subjects entering at different ages.
#
#  This kind of design has been called an "Accelerated Longitudinal Design"
#  -- also a cross-sequential design or a cohort design (Miyazaki and
#  Raudenbush, 2000). Its competitors are the cross-sectional design in which
#  individuals of different ages are observed at one point in time, and a
#  single cohort design in which a single cohort is followed over a very
#  long time.
#
#  In the cross-sectional design, age effects and cohort effects are perfectly
#  confounded.
#
#  In the single cohort design, age effects and period effects are perfectly
#  confounded.
#
#  In the ALD (accelerated longitudinal design) no pair of age, cohort.birth or period
#  are perfectly confounded but the LINEAR effect of any one is confounded with
#  a combination of the linear effects of the other two, in that the predictors
#  obey:
#
#                    period = cohort.birth + age
#
#  so that there are only two degrees of freedom to estimate three parameters.
#  Note that we let period = time of measurement, cohort = time of birth.
#
#  If we use all three variables, the model will be singular. Thus we drop one
#  of the three. If we drop 'cohort' on the basis that the effect of cohort
#  is more 'distal' than that of 'period' or 'age', we get the following
#  where we use 'beta[x]' for the 'true' but unknowable effect of 'x' and
#  'beta.estimated[x]' for its estimated value in a particular model.
#  
#  combined effect of period, cohort and age:
#  
#     =  beta[period] x period  +  beta[cohort.birth] x cohort.birth  +  beta[age] x age
#
#     =  beta[period] x period  +  beta[cohort.birth] x (period - age)  +  beta[age] x age
#
#     =  (beta[period] + beta[cohort.birth]) x period  +  (beta[age] - beta[cohort.birth]) x age
#
#  So
#         beta.estimated[period] =  beta[period] + beta[cohort.birth]
#  and
#         beta.estimated[age] = beta[age] - beta[cohort.birth]
#
#  So depending on our assumption about beta[cohort.birth] we get:
#
#         beta[period] =  beta.estimated[period] - beta[cohort.birth]
#  and
#         beta[age]    =  beta.estimated[age]    + beta[cohort.birth]
#
#  In other words, the same data can be explained by a 0 effect of cohort
#  or by a non-zero effect of cohort with corresponding adjustments to the
#  estimated effect of age and period.
#
#  Note that if cohort is identified by 'age at baseline' then the
#  sign of the cohort effect is the reverse of the sign if it is measured
#  by data of birth. So:
#
#         beta[period] =  beta.estimated[period] + beta[cohort.baseline.age]
#  and
#         beta[age]    =  beta.estimated[age]    - beta[cohort.baseline.age]
#
#
#  It may seem paradoxical that the separate effects of age, period and cohort
#  cannot be estimated and the reason is that they, in fact, can be.
#  'Non-linear' effects can be estimated separately under the assumption
#  that there are no interactions, i.e. that the separate effects are 
#  additive. 
#  It is only the linear effects that are fully confounded.
#
#  Miyazaki and Raudenbush (2000) propose an approach using mixed models to
#  test wether cohort effects can be neglected.  Essentially their tests ask
#  whether each cohorts trajectory 'splices' into those of adjoing cohorts.
#  If so the entire age trajectory can be explained by age effects only.
#  Their approach, however, assumes that there are no period effect, an effect
#  that can be irregular and potentially strong in my limited experience.
#
#  We propose later to test a model that includes period effects and test
#  for possible effect of cohort beyond linear. If higher order cohort
#  can be ignored, then the estimation of an age trajectory is simplified.
#
#

#
#  A first look at the data:
#

data(Indonesia)
?Indonesia
ind <- Indonesia
xqplot( ind )
scatterplot.matrix( ind , ell = TRUE )


indu <- up( ind, ~ id )
xqplot( indu )

# number of observations per child:

ind$n <- capply( ind, ~ id, nrow )

indu <- up( ind, ~ id )
xqplot( indu )
scatterplot.matrix( indu, ell = TRUE )

inda <- up( ind, ~ id , all = T )
xqplot( inda )
scatterplot.matrix( inda, ell = T )

#
# QUESTIONS:
#      How would you interpret
#       1) the correlation between n and time
#       2) the tapering of the season vs n plot
#

# Notes:
# Although there are 1154 observations on 274 subjects, there are only

tab( ind, ~ resp)

# 107 instances of respiratory infections in

tab( up( ind, ~ id, all=T)$resp > 0 )

# 81 subjects. For a dichotomous response, the 'effective sample size'
# (Harrell, 2001) is the number of cases in the smaller group, here 107.
#
# Despite the seemingly large number of observations, we need to
# interpret results cautiously.
#

#
# Looking at marginal relationships between resp and age
#
#
# Note: I do not carry out the following sequence of plots when
# I'm analyzing data unless I'm preparing a graphic for publication
# or I'm trying to impress someone. For exploratory analysis
# most plots are created with 1 or 2 lines of code.
#
# The following sequence shows how you might go
# from a simple plot for quick visualization to a more complex plot.
#


xyplot( resp ~ age, ind )

xyplot( resp ~ age, ind ,
        panel = function(x,y,...) {
          panel.xyplot( x, y, ...)
          panel.loess( x, y, ...)
        }
)
# fails for a subtle reason: the 'default' in panel.loess
# family = "symmetric" which tries to fit a ROBUST estimate
# of location. Here, however, a robust estimator is precisely
# what we don't want. We want to fit the curve with an
# ordinary mean to estimate the probability of 'resp'.
# We get that by specifying family = "gaussian"
#

?panel.loess

xyplot( resp ~ age, ind ,
        panel = function(x,y,...) {
          panel.xyplot( x, y, ...)
          panel.loess( x, y, ..., family = 'gaussian')
        }
)

# focus scale of y to see predicted proportion of resp

xyplot( resp ~ age, ind ,
        ylim = c(0, .2),
        panel = function(x,y,...) {
          panel.xyplot( x, y, ...)
          panel.loess( x, y, ..., family = 'gaussian')
        }
)

# create a factor for sex

ind$Sex <- factor( ifelse(  ind$sex == 0, "Male", "Female"))

xyplot( resp ~ age | Sex, ind , ylim = c(0, .2),
        panel = function(x,y,...) {
          panel.xyplot( x, y, ...)
          panel.loess( x, y, ..., family = 'gaussian')
        }
)

td( lwd = 2 )   # by specifying 'lwd' here instead of as an argument to
# to xyplot, the change in lwd will be applied to
# the key as well as to the main graph.
# But it will persist for subsequent graphs.
#

xyplot( resp ~ age , ind , ylim = c(0, .2), groups = Sex,
        panel = panel.superpose,
        panel.groups = function(x,y,...) {
          panel.loess( x, y, ..., family = 'gaussian')
        },
        auto.key = list( columns = 2, lines = T, points = F)
)

#
# Analysis
#

# Issues:
#
# -- data are longitudinal clustered in children so obs. are not independent
#
# -- the trajectories are not linear or quadratic, each might have a
#    a different degree as a polynomial
#
# -- age is confounded with time, hence possible period effects
#
# -- age and period are confounded with cohort, i.e. 'baseline_age'
#

#
# We used a generalized linear mixed model fitted by penalized quasi-likelihood
#
# Essentially, we fit a sequence of 'lme' models with changing weights
# much as a 'glm' involves fitting a sequence of 'lm' models with changing
# weights.
#
# With true 'glm's the result is equivalent to maximizing
# the likelihood of the the glm model.
#
# With 'glmm' models it is not,
# hence PQL = penalized QUASI likelihood.
# We cannot do LRTs to compare models but can use Wald tests.
#
# For binary outcomes with few observations per cluster, this approach
# can yield biased estimates, particularly of variance parameters.
# So we interpret results cautiously.
#
# The advantage of glmmPQL is that it is relatively fast and convergent
# and its approach is not too difficult to understand since it parallels
# lm and glm. Also glmmPQL allows correlation structures and realively complex
# structures for the G matrix, allowing, for example, the fitting of
# smoothing splines.
#
# One alternative is the 'lmer' function in 'lme4' that has a superior
# fitting algorithm but lacks some of the other features of glmmPQL.
# Currently it also lacks many methods.
#

library(MASS) # for glmmPQL

fit <- glmmPQL ( resp ~ time + season + age + sex + height_for_age , data = ind,
                 family = binomial, random = ~ 1 | id)
summary( fit )
#
# QUESTION: anything wrong?
#

fit <- glmmPQL ( resp ~ time + factor(season) + age * sex + height_for_age , data = ind,
                 family = binomial, random = ~ 1 | id)
summary(fit)
wald( fit, "season")  # straddling the Equator doesn't eliminate seasonal effects!!

# QUESTIONS:
#     1) What happened to season? it went from not significant to highly significant.
#     2) Note that sex and its interaction with age are not significant. Do we quit
#           looking for sex effects? Or why not?
#

ind$Season <- factor( c("Spring","Summer","Fall","Winter") [ ind$season ] )
ind$Season <- reorder( ind$Season, ind$season)   # QUESTION: Why are we doing this?

tab( ~ Season, ind)

# QUESTION: Why so imbalanced?

tab( ~ Season + time, ind )

# We have 5 degrees of freedom for time (WHY?) and we've used 4:
#    1 for linear time and
#    3 for season.
# We could try one more before moving on:

fit2 <- glmmPQL ( resp ~ time + I(time^2) + Season + age * Sex + height_for_age , data = ind,
                  family = binomial, random = ~ 1 | id)
summary( fit2 )
wald( fit2, 'time' )

#
# QUESTION: Should we drop quadratic time?
#     I would lean to drop because I'm concerned about the size of
#     the eventual model.
#
# So our provisional model is:

fit3 <- glmmPQL ( resp ~ time + Season + age * Sex + height_for_age ,
                  data = ind,
                  family = binomial, random = ~ 1 | id)
summary( fit3 )

wald( fit3, 'Season' )
wald( fit3, 'Sex' )
wald( fit3, list( Interaction =  ":"))    



#
# Modeling Seasonal effects
#

#
#  In this example time is discrete as if all subjects were observed
#  nearly at the same time on each occasion. Often time is more
#  continuous, measured to the day or to the month.
#
#  When the time scale of a study is a substantial part of a year
#  or stretches over many years, and the phenomemon might include
#  a periodic (seasonal) pattern that is repeated
#  each year, you can consider modeling periodic effects.
#
#  Another common context for periodic effects is modeling
#  diurnal periodic patterns. Circadian cycles that are not
#  locked into a fixed period can sometimes be approximated
#  by diurnal models or modeled through autoregressive models
#  on the R side of a mixed model.
#
#  The basic tool for periodic patterns is Fourier Analysis.
#  Fourier Analysis is to periodic effects what
#  exponential models are to asymptotic effects and what
#  lines and polynomials are to general smooth effects.
#
#  Fourier analysis sounds extremely advanced but it's only
#  slightly more complicated than fitting polynomials.
#
#  Fourier analysis involves fitting a sine 'wave' to the data, a wave
#  whose period is equal to the basic period (e.g. year, day).
#  This fundamental wave corresponds to a line in general analysis.
#  If a single sine wave doesn't do the job we consider adding
#  higher harmonics: sine waves whose periods are increasingly
#  smaller whole fractions of the basic period, i.e. 1/2 year,
#  1/3 year, 1/4 year, 1/5 year and so on. As in fitting polynomials
#  we can keep going until the is no significant improvement.
#
#  Just like it takes at least 2 points to fit a line, 3 points to fit
#  to fit a quadratic,4 points to fit a cubic and so on,
#  it takes takes at least
#
#      3 points within each period to fit a fundamental wave
#      5 points ... to fit a fundamental + a first harmonic ( 1/2 year)
#      7 points ... to fit up to the second harmonic (1/3 year)
#      9 points ... to fit up to the third harmonic (1/4 year)
#      ... etc
#
#  In our data we have 4 points so we can't go higher than the
#  fundamental. The fundamental is not a 'saturated' model for the
#  effect and we can test whether time used as a factor (the
#  saturated model) gives a better fit.
#
#  The equivalent of terms in a polynomial (i.e. x, x^2, x^3, etc.
#  are sine,cosine pairs. So additional terms come in twos
#
#  Suppose we have data measured in days stretching from -100 to 600,
#  almost two years.

days <- -100:600

#  Since we have 365 points
#  per year we could go very high with our harmonics but we
#  just look at the kinds of patterns generated by the first
#  few harmonics.
#
#  For a first look, we won't fit a model but we will look at
#  what models look like.  A bit like looking at a straight line
#  then a quadratic, then a cubic, etc.
#
#  Note that our time points DO NOT have to be evenly spaced NOR
#  at the same times in each period.
#
#  The period of the sin or cos function is 2*pi

2*pi

#  The period for our data is 365.25 days.  (The .25 is only
#  necessary if our data stretches over such a long time that
#  ignoring leap years would cause the model to get out of phase,
#  but there's no harm in illustrating how easy it is to
#  incorporate a fractional period.
#
#  To produce a sine curve with a period of 365.25 days, we need
#  to change our time scale from that of the data to that of the
#  the sin function. We do this by dividing by the period of
#  the data and multiplying by the period of the sin function.

plot( days, sin( 2*pi* days / 365.25) , type = 'l')
abline( h  = 0, col = 'gray' )
abline( v = 365.25*(-1:2), col = 'gray')

#  When we fit a model, sin( 2*pi*days/365.25) will be
#  a regressor. Fitting the model will fit a coeffiecient for
#  this regressor, for example, 0.345

lines( days, 0.345 * sin( 2*pi* days / 365.25), col = 'green3')

#  A single sin curve will always have value 0 at 0, i.e.
#  its 'phase' must be zero.  How could we fit other possibilities
#  for the phase (the first time when the curve is equal to 0).
#  Obviously we're talking about two parameters here:
#      the amplitude and the phase
#  We could use a formally non-linear model but the magic
#  of Fourier analysis is that we can accomplish this by
#  combining our sin curve with a cos curve of the same period
#  but possibly different amplitudes;

lines ( days, 0.789 * cos( 2*pi* days / 365.25), col = 'blue')

# Note that the cos curve is a sin curve with phase
# equal to 1/4 year.
#
# When we add them we get another 'sine' curve with some other
# phase:

lines( days,
       0.345 * sin( 2*pi* days / 365.25)
       + 0.789 * cos( 2*pi* days / 365.25),  col = 'red')

# The amplitude of the resulting curve is

sqrt( 0.345^2 + 0.789^2)

# and its (a) phase is

365.25 * atan2( -0.345, 0.789)

# Fitting sine curves ('sine' generic for 'sin' or 'cos' also
# called trigonometric curves)

fit2.sin <- glmmPQL ( resp ~ time +
                        sin( 2*3.1416 * (time + 6) / 4) +
                        cos( 2*3.1416 * (time + 6) / 4) + age * Sex +
                        height_for_age , data = ind,
                      family = binomial, random = ~ 1 | id)
summary( fit2.sin )

# QUESTIONS:
#    1 ) Why did we add 6 to time? What happens if we don't.
#    2 ) The 'sin' term is highly significant but the 'cos' term is
#        not. Should we drop the 'cos' term.
#
# The answer to the second question is too important to leave up
# in the air:
# If we drop the 'cos' term, then the 'sine' curve is force to
# have a phase of 0, i.e. the curve = 0 at time = 0.
# Thus, the model would not be invariant if we add a constant
# to the time variable and the model would violate a principle of
# invariance.
#
# Another way of thinking about it is that each term is
# 'marginal' to the other and dropping one without the other
# violates the principle of marginality.  Except in very rare
# circumstances, should be added or dropped as a pair. This
# means that we need to carry out a wald test with 2 numDFs:

wald ( fit2.sin, "sin|cos")

# Of course we knew this would be significant.
#
# If we had more time points, we could try  adding higher harmonics,
# the next being:

lines( days, sin(2 * 2 * 3.1416 * days / 365.25), lty = 2, col = 'green3')
lines( days, cos(2 * 2 * 3.1416 * days / 365.25), lty = 2, col = 'blue')

# Note how easy it is to generate each harmonic. You just multiply
# the whole argument of the 'sin' or 'cos' by the appropriate integer:
# 2 for the first harmonic
# 3 for the second, etc.
#
# Alas we have too few points per year to illustrate this here.
# If we did, we could keep adding other harmonics until we decide
# to stop. Note that lower harmonics are treated as 'marginal' to
# higher harmonics, just like polynomials.
#
# To make the process much cleaner and easier, we can define a
# 'Sine' function that generates the matrix with our cos,sin pair
# incorporating phase shifts and period scaling:

Sine <- function( x ) cbind( sin = sin( 2 * 3.1416 * (x + 6) / 4),
                             cos = cos( 2 * 3.1416 * (x + 6) / 4))

# to see what this does:

time.ex <- seq( -1, 6, .1)
head( Sine( time.ex ) )
matplot( time.ex, Sine( time.ex ), type = 'l')

# generating higher harmonics is a cinch:

matlines( time.ex , Sine( 2 * time.ex ), col = 'blue')
matlines( time.ex , Sine( 3 * time.ex ), col = 'red')

# combining all this with random coeffients produces white noise: try it a few times:

plot( time.ex,  cbind( Sine(time.ex) , Sine(2*time.ex), Sine(3 * time.ex)) %*% rnorm( 6 ), type = 'l')

# Back to Indonesia: (we can fit the same model with:)


Sine <- function( x ) cbind( sin = sin( 2 * 3.1416 * (x + 6) / 4),
                             cos = cos( 2 * 3.1416 * (x + 6) / 4))

fit2.sin <- glmmPQL ( resp ~ time +  Sine ( time )
                      + age * Sex + height_for_age , data = ind,
                      family = binomial, random = ~ 1 | id)
summary( fit2.sin )

wald( fit2.sin, 'Sine')

# QUESTIONS:
#    1) Sine( time )  is a function of 'time' NOT 'Season'.
#       Is is appropriate to think of it as modeling a
#       a Season effect.
#    2) With this model, what is the interpretation of the
#       coefficient for the intercept term.
#

# There are 3 degrees of freedom for Season (4 levels - 1).
# Our Sine function uses 2.
# We'd like to test the next harmonic by adding the term:
#
#      ... + Sine(2*time) + .....
#
# but this would require 4 degrees of freedom.
# How can we test whether we need to include the omitted
# degree of freedom in this case? Just include the 'cos'
# term of the next highest harmonic. Note: very rarely
# the cos term won't work and you should try again with
# the sin term.
#

fit2.sinS <- update( fit2.sin , . ~ . + Sine(2*time)[,2])
summary(fit2.sinS)

# Turns out to be highly significant so we need all available
# degrees of freedom to model Season.
#
# So we might as well just use the factor Season.
#
# This suggests that if we had more time points we would have
# needed at least the first harmonic in our model.
#
# With the same number of subjects and the same number of
# occasions for each subject BUT subjects coming in at different
# dates, we could have used the variability in dates to
# fit and test higher harmonics.
#
# So we are back where we started BUT much wiser!

# EXERCISE:
#
#     Try fitting a periodic curve to the sunspot data
#     in the 20th century
#     Note: we will use part of the same data later for splines.
#

data(sun)   # monthly sunspot data in 20th century

plot( spots ~ year , sun)

# This is not strictly periodic data because the phase is
# not fixed but it wanders randomly.

# Trying a model with a period of 9.5 years plus a few harmonics

Sine <- function( year ) cbind( sin = sin( 2*3.1416*year), cos = cos( 2.*3.1416*year))

# NOTE: By using a period of 1 in the Sine function we set things up so
# that when we use the Sine function
# we DIVIDE BY THE PERIOD and MULTIPLY FOR HARMONICS:

fitsun <- lm( spots ~ Sine( year / 9.5) + Sine( 2 * year / 9.5) + Sine( 3 * year / 9.5), sun)
summary( fitsun )
lines( predict( fitsun) ~ year, sun)

# EXERCISE:
#
#      Look at the graph, particularly at the peaks of the fitted curve
#      Is the fit too slow or too fast?
#      Adjust the period to see whether you can find one that fits better>
#


#
# Modeling age with splines
#

# Splines: a very simple idea
#
#    Problem with fitting a single polynomial:
#
#        -  Might need a very high degree for a good fit
#
#        -  But high degree polynomials go wild away from data
#                (Runge's phenomenon)
#
#        -  Points have influence at a distance -- generally not reasonable
#
#    Spline:
#
#        -  split the range into disjoint intervals (e.g. 3 intervals)
#
#        -  the points at the end points are called 'knots'
#                 ( 3 intervals => 2 knots ; p intervala => p-1 knots)
#
#        -  fit a separate polynomial in each interval
#
#        -  BUT do it in such a way that the polynomials join up
#           at the knots.
#
#        -  How? different 'degrees' of joining up:
#                - 0: just continuous but perhaps a kink
#                             - change in slope = 1st derivative
#                - 1: smooth: no kink but perhaps a sudden change in curvature
#                              - change in 2nd derivative = curvature = acceleration
#                - 2: smooth of degree 2: no sudden change in curvature but
#                            possible sudden change 3rd derivative = jerk
#                - 3, 4, 5: snap, crackle and pop
#
#        -  Difficult: generating regressors that do the right job
#           BUT if the regressors are generated for you, nothing's
#           easier than using splines.
#
#    gsp is a simple function to generate the columns of the X matrix
#           for arbitrary splines
#

#
#  Primer for splines
#
data(sun)   # monthly sunspot data in 20th century
spex <- sun[ sun$year > 1980,]
xqplot( spex )
plot( spots ~ year , spex)

#
# Linear spline: lines continuous at knots: choose knots where slope changes:
#


# General Spline Generator:

# Preliminary step:
# - transform time so it has a resonable range
# - max( abs(time) )^(highest degree)  should not be much greater than 10000
# - you can keep the original time scale for plotting

spex$ys <- spex$year - 1980

?gsp

# define a function to generate the spline:

# linear spline
splin  <- function( x ) {
  gsp( x,
       knots = c(  1986, 1990, 1996 )-1980,   # 3 knots => 4 intervals
       degree = c( 1,   1,    1,    1 ) ,     # linear in each interval
       smooth = c(   0,    0,    0   )        # continuous at each knot
  )
}

fitlin <- lm( spots ~ splin(ys), spex )
summary( fitlin )
lines( predict(fitlin) ~ year, spex)
abline( v = 1980 + splin(NULL)$knots , col = 'gray')



# quadratic spline: quadratic in each interval and smooth at knots
spquad  <- function( x ) {
  gsp( x,
       knots =  c(  1989, 1993 )-1980, # 2 knots => 3 intervals
       degree = c( 2,    2,    2 ) ,   # quadratic in each interval
       smooth = c(   1,    1    )      # smooth at each knot
  )
}

fitquad <- lm( spots ~ spquad(ys), spex )
summary( fitquad )
lines( predict(fitquad) ~ year, spex, col = 'red')
abline( v = 1980 + spquad(NULL)$knots , col = 'pink')
# note the appearance of the spline where it goes over the knot


# cubic spline

spcubic  <- function( x ) gsp( x,  1987 - 1980,  c(3, 3), 2)

fitcubic <- lm( spots ~ spcubic(ys), spex )
summary( fitcubic )
lines( predict(fitcubic) ~ year, spex, col = 'blue')
abline( v = 1980 + spcubic(NULL)$knots , col = 'blue')


AIC ( fitlin, fitquad, fitcubic )  # fit linear was best for 6 dfs
# but we might have done better
# by finding better places for knots

# natural cubic spline

#    add 'boundary knots' at the extreme values of the data and
#    extrapolate with linear term
#
spnat  <- function( x ) gsp( x, c(1980, 1987, 1998) - 1980,  c(1, 3, 3, 1), c(1,2,1))

fitnat <- lm( spots ~ spnat(ys), spex )
summary( fitnat)
lines( predict(fitnat) ~ year, spex, col = 'green')
abline( v = 1980 + spnat(NULL)$knots , col = 'green')

#
# Summary:
#   The right spline depends on the phenomenon being modeled
#
#   Cubic splines are very popular but for many situations they
#          don't bend fast enough.
#
#

AIC ( fitlin, fitquad, fitcubic, fitnat )

#
#  Using splines for age
#


quantile(ind$age, seq(0,100,5)/100)


ind$age.y <- ind$age / 12
# transform to age in years -- good to limit max(abs(x)) --
# when using raw polynomial and splines to control condition
# number of X matrix

# try a cubic

fit.3 <- glmmPQL ( resp ~ time + Season + ( age.y + I(age.y^2) + I(age.y^3)) * Sex + height_for_age , data = ind,
                   family = binomial, random = ~ 1 | id)

summary( fit.3 )
wald( fit.3, 'Sex' )
# does not reveal much

# Try a natural cubic spline

qs <- quantile( ind$age.y, c(0,1,2,3,4,5,6)/6)
qs    # natural cubic spline puts knots at both ends with linear terms to extrapolate if necessary

sp <- function( x ) gsp( x, qs, c(1,3,3,3,3,3,3,1), c(2,2,2,2,2,2,2))
# natural cubic spline using 5 interior
# knots separating intervals
# witn equal proportions of the data
# and two boundary knots


fitsp <- glmmPQL( resp ~ sp(age.y) * Sex + time + Season + height_for_age, ind,
                  random = ~ 1 | id, family = binomial )

summary( fitsp )

pred <- expand.grid(  age = 4:84, Sex = levels( ind$Sex),
                      height_for_age = 0,
                      Season = levels(ind$Season),
                      time = 1:6)
pred$age.y <- pred$age / 12
pred$resp.lo <- predict( fitsp, pred, level = 0)


wald( fitsp, 'Sex')
# No strong evidence for an overall effect of Sex
# But what if we target the question we wanted to ask

# Effect of Season:

xyplot( resp.lo ~ Season, pred, type = 'l',
        subset = Sex == "Male" & time == 1 & age == 50)
wald( fitsp, Ldiff( fitsp, "Season", reflevel = "Spring"))


# Overall period effect:

wald( fitsp, list( "period effects" = "time|Season") )

# QUESTION: What does this say? Recall that we are
# controlling for age at the time of observation


xyplot( resp.lo ~ age, pred, groups = Sex, type = 'l',
        subset = Season == "Summer" & time == 1,
        auto.key = list( columns = 2, lines = T, points =F))

#
#  Using Wald tests to explore splines
#

# We need an estimate of the difference between the two curves

wald( fitsp )

#
# We see that the columns of the model matrix consist of:
#        1: a column of 1s for the intercept,
#      2-7: a block of 6 columns for the spline,
#        8: male indicator
#        9: time
#    10-12: a block of 3 columns for Season
#       13: height_for_age
#    14-19: another block of 6 columns for the interaction between
#            age and Sex
#
# To estimate the difference between the two sexes at each age
# we need to generate the correct L matrix. This would seem formidable
# without a detailed understanding of the parametrization of the
# spline.
#
# However, there is a function to help with this. It generates the
# portions of the L matrix for the blocks that model the spline.
#
#
# To generate the portion of the matrix to evaluate the spline at
# at age.y = 2, 4, and 6, use

sc( sp , c(2,4,6) )

#
# This is the portion of the L matrix that would estimate the
# value of the response offset from the intercept.
#
# To evaluate the derivative (slope) of the spline at the same
# points:

sc( sp, c(2,4,6), D = 1)   # first derivative (slope) of spline

# the second, third and fourth derivatives:

sc( sp, c(2,4,6), D = 2)
sc( sp, c(2,4,6), D = 3)
sc( sp, c(2,4,6), D = 4)

#
# To handle possible discontinuites at knots, a fourth argument
# 'type' that can be set to 0, 1 or 2 signifies that the
# derivative should be evaluated from the left, from the right
# or that the saltus should be evaluated.

# To create an L matrix to estimate Female response at
# each month, given values of other predictors. The values
# are eventually arbitrary and we choose:
#
#           Season == "Spring" & time == 2
#
# We need:
#
# Lfemale = the L matrix is Season == "Summer" & time == 1,
#

Lfemale <- cbind( 1, sc( sp, seq(4,84,1)/12), 0, 1, 1, 0, 0, 0, 0*sc(sp,seq(4,84,1)/12) )
some( Lfemale )
zapsmall( some( Lfemale ))

Lmale   <- cbind( 1, sc( sp, seq(4,84,1)/12), 1, 1, 1, 0, 0, 0, 1*sc(sp,seq(4,84,1)/12) )
zapsmall( some( Lmale ))

Ldiff <- Lmale - Lfemale
zapsmall( some( Ldiff) )

(ww <- wald( fitsp, Ldiff ))  # estimates gap at each age

wald( fitsp, "Sex")

# QUESTION: How is this output similar to the previous wald output?

# Turn the output of the 'wald' function into a data frame for plotting
# include eta.hat +/- 2 SE

df.diff <- as.data.frame( wald( fitsp, Ldiff), se = 2)
df.diff$age <- seq(4,84,1)
some( df.diff )    # estimated coef with Upper and Lower confidence Bound

xyplot( coef + U2 + L2 ~ age, df.diff, type = 'l',
        panel = function(x,y,...) {
          panel.superpose(x,y,...)
          panel.abline( h = 0)
        })

# Better labels

td( lty = c(1,2,2), col = c('blue','blue','blue'), lwd = 2)

xyplot( coef + U2 + L2 ~ age, df.diff, type = 'l',
        sub = "Boy-Girl gap in log odds of respiratory infection",
        ylab = "Log odds difference +/- 2 SEs (single prior hypothesis)",
        xlab = "age (in months)",
        panel = function(x,y,...) {
          panel.superpose(x,y,...)
          panel.abline( h = 0)
        })

# To make this more interpretable, we would like to transform
# graphs to a probability scale where possible.

# Fitted responses are easy:  probability = 1/(1+exp(-log.odds))
# Let's write it as a function

Invlogit <- function ( lo ) 1/(1+exp(-lo))
Logit <- function( p ) log( p / (1-p) )  # i.e. log(odds(p))

# So the graph showing each sex is, instead of:

xyplot(resp.lo ~ age, pred, groups = Sex, type = 'l',
       subset = Season == "Summer" & time == 1,
       auto.key = list( columns = 2, lines = T, points =F))

# we get:

xyplot( Invlogit(resp.lo) ~ age, pred, groups = Sex, type = 'l',
        ylab = "Probability of respiratory infection",
        subset = Season == "Summer" & time == 1,
        auto.key = list( columns = 2, lines = T, points =F))

# QUESTION: Are the bumps and valleys artifacts of our knots?
#
# EXERCISE LATER: redo this playing with different knots,
#     possibly knots chosen on the basis of the initial plot
#     What price should you pay? The knot adjustments are
#     non-linear parameters and you could pay 1 DF per knot
#     to be safe.
#
#
# Plotting the difference:
#
# There isn't a direct way to turn the difference in log odds into
# a difference of probabilities. Probabilities are 1-1 functions of log odds
# but probability differences are not 1-1 functions of differences in log odds
#
# However:
#
#    the ratio of odds = exp( diff of log odds )
#    and if the probabilities are relatively small, then the ratio of odds
#    is approximately the proportion of probabilities which we can
#    wrap our minds around.
#
#

td( lty = c(1,2,2), col = c('blue','blue','blue'), lwd = 2)

xyplot( exp(coef) + exp(U2) + exp(L2) ~ age, df.diff, type = 'l',
        sub = "Boy/Girl odds ratio for respiratory infection",
        ylab = "Odds ratio +/- 2 SEs (single prior hypothesis)",
        xlab = "age (in months)",
        panel = function(x,y,...) {
          panel.superpose(x,y,...)
          panel.abline( h = 0)
        })
# oops


xyplot( exp(coef) + exp(U2) + exp(L2) ~ age, df.diff, type = 'l',
        ylim = c(.1,10),
        sub = "Boy/Girl odds ratio for respiratory infection",
        ylab = "Odds ratio +/- 2 SEs (single prior hypothesis)",
        xlab = "age (in months)",
        panel = function(x,y,...) {
          panel.superpose(x,y,...)
          panel.abline( h = 1)
        })

# not pretty. If the changing the plotted values does not work, then
# just change the labels on the axes!

xyplot( coef + U2 + L2 ~ age, df.diff, type = 'l',
        ylim = log( 2^c(-8,8)),
        scale = list(
          y = list(
            at =  log( 2^seq(-7,7,1)), labels = fractions( 2^seq(-7,7,1))),
          x = list( at = seq( 0,90,10))),
        
        sub = "Boy/Girl odds ratio for respiratory infection (on log odds scale)",
        ylab = "Odds ratio +/- 2 SEs (single prior hypothesis)",
        xlab = "age (in months)",
        panel = function(x,y,...) {
          panel.superpose(x,y,...)
          panel.abline( h = log( 2^seq(-7,7,1)), col = 'gray')
          panel.abline( h = 0)
          panel.abline( v = seq(0,90,5), col = 'gray')
        })
#
#  So ... at 35 months it seems that boys are estimated to be over 4 times
#  as likely as girls to get respiratory infections and
#  they are at least twice as likely as girls using a 95% CI.
#
#  BUT this strictly works only if we have one age as a prior hypothesis.
#  What if we have selected 35 months because it is significant.
#
#  We can adjust the CIs to take multiple prior hypotheses into
#  account using Bonferroni adjustment or to protect against any
#  number by using a Scheffe adjustment.
#

#
#  Buying a fishing license
#

#
# Rules for fishing licenses:
#
#                  The analogy doesn't quite work because
#                  you are not paying for the number of fish you catch,
#                  you are paying for the particular fish you plan to
#                  look at whether you decide to keep them or not.
#                  If you've paid to look at the fish,
#                  there's no extra cost for keeping it!
#
#                  This subtlety is critical: it's your intentions that
#                  count, not your actions!! You pay for the privilege
#                  to look at fish, not for keeping them.
#
# 1) No window shopping. You have to pay for every fish you PLAN to look at.
#
# 2) The first fish is free.
#
# 3) Bonferroni's franchise: if you plan to look at a fixed number of fish
#    you get your licence from Mr. Bonferroni who will charge you per
#    fish you plan to look at.
#
# 4) Scheffe's franchise: You can get an unlimited fishing licence for a
#    SUBSPACE of hypotheses from Mr. Scheffe. He charges according to
#    the dimension of the subspace.
#
# 5) If you find that a Scheffe license is cheaper for your purposes
#    than a Bonferroni license, you can get a full refund on your
#    Bonferroni license and use a Scheffe license instead.
#
#  Note: there are many other purveyors of fishing licences but
#  they are special purpose licences. They tend to be cheaper when
#  they are just right for you application. But they aren't as general
#  as Bonferroni and Scheffe.
#

# How do we buy fishing licences?
#
# It's extremely easy:
# When we use
#
#              estimated.value +/- 2 x SE
#
# the '2' comes from the fact that the interval -2 to +2 contains
# approximately 95% of the probability for a standard normal random
# variable.
#
# All we need to do is to use something other than a '2'
#
# For Bonferroni to test H hypotheses, we want to choose K (instead of 2)
# so that the probability that H normals (independent or not) will all
# fall in the interval (-K , K) is at least 95%.
#
# For Scheffe to fish in a SUBSPACE of dimension D, we want to choose K
# so that any linear combination chosen from a particular subspace of
# dimension D, will fall in the interval (-K , K)
#

# The following are two functions that will produce the right factor
# to use instead of '2':

Bonferroni.factor <- function( nhypotheses, alpha = .95, denDF = Inf)
  qt( 1 - (1-alpha)/(2*nhypotheses), denDF)

Scheffe.factor <- function( dimension , alpha = .95, denDF = Inf)
  sqrt( dimension * qf( alpha, dimension, denDF))

# Note:

qnorm( .975 )
Bonferroni.factor( 1 )
Scheffe.factor( 1 )

cat( "\n\nCost of protection:\n")
mat <- cbind( Bonf =  Bonferroni.factor( 1:20 )/qnorm(.975) ,
              Sch = Scheffe.factor( 1:20)/qnorm(.975))
rownames( mat ) <- 1:20
round( mat, 2)

#
# Note that costs are not really comparable because you aren't buying the
# same thing.
#
#
# Some scenarios:
#
# Scenario 1:
#
# Suppose we had preselected (credibly on some basis independent of the data)
# three ages we wanted to test, say 36,42 and 48 months. This could use
# a Bonferroni adjustment with nhypotheses = 3.
#
# Scenario 2:
#
# Suppose we want to test 36 months but we played with two of the knots to
# make the gap as big as possible.  We could use a Scheffe adjustment
# with dimension 3 (2 more than 1)
#
# Scenario 3:
#
# Suppose we're just looking for any Sex differences at all, we have
# no preconceived idea where to look. We could use s Scheffe adjustment
# with dimension equal to the dimension of the Sex comparisons. This is
# numDF in the wald output for Ldiff

wald(fitsp, Ldiff)

#  i.e. 7.
#
#  QUESTION: How many prior hypotheses do you think we would need to test
#  before a Scheffe adjustment is cheaper than a Bonferroni adjustment.
#
# Last question first:

uniroot( function(K) Bonferroni.factor(K) - Scheffe.factor(7), c(1,1000))

# Scheffe is, surprisingly, quite expensive.
#
# Now the first 3 questions:

ses = c(std = qnorm(.975),
        Bonf.3 = Bonferroni.factor( 3 ),
        Sch.3  = Scheffe.factor( 3 ),
        Sch.7  = Scheffe.factor( 7 ))

ses

dmult <- as.data.frame( wald( fitsp, Ldiff), se = ses)
some(dmult)
dmult$age <- seq(4,84,1)

td( col = c('blue','blue','green3','red','black','blue','green3','red','black'),
    lty = c(  1,2,3,4,5,2,3,4,5), lwd = 2)

xyplot( coef + Ustd + UBonf.3 + USch.3 + USch.7 + Lstd  + LBonf.3 +
          LSch.3 +  LSch.7  ~ age,
        dmult,
        type = 'l',
        ylim = log( 2^c(-8,8)),
        scale = list(
          y = list(
            at =  log( 2^seq(-7,7,1)), labels = fractions( 2^seq(-7,7,1))),
          x = list( at = seq( 0,90,10))),
        
        sub = "Boy/Girl odds ratio for respiratory infection (on log odds scale)",
        ylab = "Odds ratio with confidence intervals",
        xlab = "age (in months)",
        panel = function(x,y,...) {
          panel.superpose(x,y,...)
          panel.abline( h = log( 2^seq(-7,7,1)), col = 'gray')
          panel.abline( h = 0)
          panel.abline( v = seq(0,90,5), col = 'gray')
        },
        auto.key = list( text = c('Estimated','Ordinary 95% CI',
                                  "Bonferroni k=3", "Scheffe d=3", "Scheffe d=7"),
                         lines = T, points = F, columns = 2)
)

#
# EXERCISE:
#    What happens if, instead of using Scheffe(7) we resort to
#    Bonferroni on a fine grid of ages, e.g. every whole month.
#    Do we get significant results? Is this a reasonable approach?
#

# QUESTION:
#    Is there a connection between the p-value of

wald( fitsp, Ldiff)[[1]]$anova$`p-value`

# of 0.06 with 7 dfs in the overall F-test for Sex differences and the fact that
# the specific Sex differences at each age are never significant when
# we use the Scheffe( 7 ) adjustment?

#
# EXERCISES:
# 1) Experimenting with knots:
#     See what happens if you choose different knots.
#     Coordinate with participants near you to try
#     different experiments and then compare results.
#     Fit a model with different knots, then compare
#     results for testing sex differences:
#     Things to try:
#     1) Try 7 interior knots instead of 5
#     2) Try 3 interior knots and move them around
#        to increase the significance (decrease the
#        p-value) for sex comparisons
#     3) Try a natural quadratic spline with 5
#        interior knots
#

#  e.g. Try using 3 interior               
#     possibly knots chosen on the basis of the initial plot
#     What price should you pay? The knot adjustments are
#     non-linear parameters and you could pay 1 DF per knot
#     to be safe.
#

#
#   Age - period = cohort
#

# We performed the analysis above ignoring cohort (baseline_age), i.e.
# assuming there is no effect.
#
# Consider:
#
# If 20-year =-olds in 2000 are much healthier than 20-year-olds in 1990
# this could have two explanations:
#    - General health improved a lot from 1990 to 2000 (period effect), or
#    - People born in 1980 are much healthier than people born in
#               1970 (cohort effect)
# What if people of all ages saw steadily improving health from 1990 to
#     to 2000. Again this could be the period or it could be that
#     people born later are healthier than people born earlier.
#     Of course, if this is the explanation, we would expect the
#     the phenomenon to be persistent. If some cohorts improve steadily
#     from 1990 to 2000 and others do not, then the explanation that
#     it's only a period effect is harder to support.
#
# The problem is that steadily (linearly) improving health over a
# long period of time looks exactly the same as lineary improving
# cohorts over a long period of time. Age, period and cohort are
# collinear. The only way to break the collinearity would be to find
# a 20-year-old in 2000 who was born in 1970.
#
# So disentangling linear effects can't be done. However, non-linear
# effects are a different thing.  For example if everyone suddenly
# becomes less stressed between 1994 and 1995, it could be a period
# effect (the referendum?) or it could be that a characteristic of
# people born in 1970 was that they would become less stressed at the age of 25,
# people born in 1971 would become less stressed at the age of 24,
# people born in 1972 would become less stressed at the age of 23, etc.
# The only way to blame a sudden change in one calendar year without
# accepting that it's a period effect is to postulate a very complex
# and implausible interaction between cohort and age.
#
# In our analysis, we did not include cohort effects. If we had tried
# by including baseline_age, we would have had a collinearity in the model.
# Is there a way of verifying whether it's reasonable to ignore cohort
# effect and to attribute changes to age and period? One way is to test
# for cohort effects that are beyond linear. We can do this by
# generating a spline and then removing its linear component. If
# such cohort effects are negligible, we gain some confidence about
# ignoring them. If they are significant, we will wish to explore
# them.
#
#
# Since our model is already burdened with parameter, we will simplify
# it by removing all but time and age effects before adding higher-order
# cohort effects.
#
# In addition, the fact that we can never disentangle linear age
# effects from possibly compensating linear period and
# linear cohort effects implies that the non-linear effects
# of age should be particularly interesting since they
# are the only age effects that are identifiable.
#
#


summary( ind$baseline_age)

ind$cohort <- scale( ind$baseline_age )  # mean 0 sd 1
quantile( ind$cohort )

cqs <- quantile( ind$cohort, (1:5)/6)   # five knots
some(gsp( ind$cohort, cqs, c(2,2,2,2,2,2), c(1,1,1,1,1)))

spcohort <- function( x ) gsp( x, cqs, 2, 1)[,-1]  # remove 1st column


fitspc <- glmmPQL( resp ~ sp(age.y) + time + Season
                   + spcohort( cohort) ,
                   ind ,
                   random = ~ 1 | id, family = binomial )

summary( fitspc )
wald( fitspc, "cohort" )
# there is no striking evidence of a cohort effect suggesting that
# the earlier analysis is reasonable

# Recall that our effective sample size is somewhat small so this
# result needs to be interpreted cautiously

#
#  Alternatives to glmmPQL
#

# Advantages of glmmPQL:
#
# -- Based on lme and can model G and R side models like lme,
# -- Can be used for smoothing splines with mixed models
# -- Its limitations are somewhat understood.
# -- Genearally fast
# -- Wide set of methods for lme can be applied
#
# Disadvantages:
#
# -- Questionable estimates of components of variance
#    especially with binary
# -- not based on likelihood - can't do LRTs
#
# glmmPQL has flaws in theory but is still a popular workhorse
# because it generally converges and is easy to use
#
# Alternatives for generalized linear mixed models:
#
# glmmML in glmmML:
#     likelihood based, random intercept only.
#
# lmer in package lme4:
#            likelihood based, handles crossed effects well (so does lme
#            but not quite as well), very advanced numerical underpinnings,
#            no R side, more limited options for G side
#            hence no smoothing splines.
#            Many methods have not been developed yet.
#            lme4 has a function to perform a MCMC analysis on a
#            fitted model but there are bugs with the method
#            that need to be resolved.
#
# glmm in GLMMGibbs: beta release, limited choices: binomial and poisson
#            Use Bayesian inference. Can use glmmPQL for starting values.
#

#
#  glmmML
#

library(glmmML)
fit.ML <- glmmML( resp ~ sp(age.y) * Sex + time + Season + height_for_age,
                  data = ind,
                  cluster = id,
                  family = binomial )
summary( fit.ML )
coef( fit.ML )
str( fit.ML )

str(fitsp)
sqrt( diag( fitsp$varFix))

# comparison with glmmPQL

comparison <- data.frame( "PQL_coef" = pc <- fixef( fitsp ) ,          
                          "PQL_sd" = ps <- sqrt( diag( fitsp$varFix)),
                          "PQL_z" = pc/ps,
                          "ML_coef" = mc <- coef(fit.ML),
                          "ML_sd" = ms <- fit.ML$coef.sd,
                          "ML_z" = mc/ms)
comparison
# ML is more conservative. I would expect them to be more similar
# with a larger effective sample size and more observations per cluster.
#
# Some disadvantage:
# Limited G side, no R side, only two families, limited links,
# Few methods: (e.g. predict, resid, vcov do no exist yet) but it would
# probably be easy to write them
#

#
#  lmer
#

# recall:

fitsp <- glmmPQL( resp ~ sp(age.y) * Sex + time + Season + height_for_age, ind,
                  random = ~ 1 | id, family = binomial )

library( lme4 )  # this used to conflict with nlme and one would have
# to close and restart R before loading the other
# I have experienced some problems but the two
# should work much better now

fitlmer <- lmer( resp ~ sp(age.y) * Sex + time + Season + height_for_age
                 + (1|id), data = ind, family = binomial)
ss <- summary(fitlmer)
fixef( fitlmer)  # coefficient
sqrt( diag( vcov ( fitlmer )))   # their sds


# EXERCISE:
#
#    Add lmer to the comparison matrix.
#    Does lmer look closer to glmmPQL or to glmmML?
#    Plot the profile of coefs, sds and zs superposed for each method
#    What patterns emerge?
#

#
#
#  Future projects
#
#
1) Add robust covariance option to 'wald'
2) Add influence diagnostics to nlme
3) Wald test for non-linear functions of parameters
4) Bootstrapping for mixed models
5) Weighted bootstrapping for mixed models
6) Clean up documentation for 'spida' and 'p3d'
