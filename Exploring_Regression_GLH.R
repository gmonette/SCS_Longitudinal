#' ---
#' title: "SCS 2017: Exploring Regression with Linear Hypotheses"
#' author: "Georges Monette"
#' date: "`r format(Sys.time(), '%H:%M %d %B %Y')`"
#' output:
#'    html_document:
#'      toc: true
#'      toc_float: true
#'      toc_depth: 3
#' ---
#' Generated:
{{format(Sys.time(), '%B %d, %Y %H:%M')}}
#' <!-- This is an example of an invisible note in HTML.
#' (Unfortunately this trick doesn't work for pdf files.)
#' To Do: Reorganize GLH and add graphs to illustrate
#' Use gpanel.fit
#' 
#' History:
#' - 2016-01-15: First version
#' TODO: Add panel.fit around line 300
#' -->
#' 
#' # Introduction
#' 
#' This 'html-flavoured' [R Markdown](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf) script illustrates a number of skills
#' and concepts you should master to analyze data using
#' regression in R. 
#' 
#' 1. Formulating and interpreting models with 
#'    interaction terms.
#' 2. Graphical diagnostics using various residual plots,
#'    leverage plots and added-variable plots.
#' 3. Using lattice graphics to display the model and data.
#' 4. Refining lattice graphics for presentations.
#' 5. Formulating specific questions and addressing them
#'    with linear hypotheses and estimators.
#' 6. Dealing with heteroscedasticity: transforming the 
#'    response or adapting the model. In this example
#'    we illustrate how to use a generalized least-squares
#'    fit using the 'gls' function in the 'nlme' package.
#' 
#' # Exploring Regressions with Linear Hypotheses
#' 
#' This script illustrates the use of the general
#' linear hypothesis (GLH) and corresponding linear
#' estimators to explore and interpret 
#' regression analyses. Linear hypotheses allow us to
#' formulate and test many types of specific questions
#' about the subject matter of the study.
#' 
#' Each estimated coefficient that appears in the usual
#' output of a regression answers 
#' a specific question about the data through the model.
#' 
#' However, when you carefully 
#' interpret estimated coefficients, you often find that
#' most of them are of little or no interest in 
#' understanding the substance of the study. 
#' Conversely,
#' most questions of real interest are not answered directly
#' through the estimated coefficients.
#' 
#' So you need to know how to formulate and 
#' ask meaningful questions, indenpendently of the 
#' particular parametrization of your model. 
#' Although not all questions can be framed with 
#' linear hypotheses, knowing how
#' to use them greatly enlarges the scope of your analyses.
#' 
#' The 'wald' function in the 'spida2' package is 
#' designed to make it easier to find
#' answers to many common questions involving 
#' linear hypotheses.  For more complex questions, 
#' it can be used with the 'Lfx' function to
#' construct linear hypothesis matrices.
#' 
#' Linear hypotheses for models using general 
#' parametric splines 
#' (using the 'gsp' function in 'spida2') can 
#' be generated with 
#' the 'sc' function.
#' 
#' Note that in SAS, in PROC GLM, linear 
#' hypotheses are generated with the 
#' ['ESTIMATE' and 'CONTRAST' statements](https://support.sas.com/resources/papers/proceedings11/351-2011.pdf). 
#' 
#' ## Initial setup (this part would be hidden in a report)
#' 
#' We first set up some options. This is optional.
#' 
#' <!-- 
#'   And the following is a chunk we 'evaluate' 
#'   but we don't show the messy code
#'   'opts' is just an optional name for the chunk. 
#'   Chunk names, if you use them must be 
#'   unique: often a pain in the neck 
#' -->
#+ opts,echo=FALSE
library(knitr)  # this is the library used to generate documents
opts_knit$set(width=90)
options(width=90)
# The 'comment' argument is the string 
# that precedes any output in the document.
# The default is '##' which I find ugly but it's conventional
# Since output is shown in a shaded box, I find that 
# adequate as an  indicator that we are looking
# at output.  Your opinion may differ.
opts_chunk$set(tidy=FALSE,comment='  ',fig.height=8,fig.width=10)
#'
#' ## Loading packages
#' 
#' The first few time you use this script, you need to execute
#' it line by line to try to understand as much of it as
#' possible and to install any packages that need to be
#' installed.
#' 
#' If you get an error that a package is not installed
#' just install it with:
#' <!-- 
#'    The following 
#'    is an example of a chunk with option: eval=FALSE
#'    so the R commands are not executed when the script
#'    is compiled (using Ctrl-Shift-K in Rstudio)
#'    to make an output file 
#' -->
#+ eval=FALSE
# install.packages("<name of package>") 
# where you substitute the <name of the package> to install
#' Much of the really interesting work in R uses specialized packages. 
#' 'Mature' packages are usually uploaded to 'CRAN' and can be installed with, for example:
#+ eval=FALSE
# install.packages('car')
#' Many 'maturing' packages are incubated by their authors on GitHub and
#' can be installed (after having installed the 'devtools' package) with,
#' for example:
#+ eval=FALSE
devtools::install_github('gmonette/spida2')
#' Once a package is installed, you use it in an R session with the
#' 'library' function.
#'
#' If a package is undergoing frequent changes, as is the case for the 'spida2'
#' package, it's a good idea to reinstall it frequently.  To reinstall a 
#' package, you should install it ***before*** loading it with 'library(<name of package>)'.
#'     
#' Here are the packages we will need for this script.
#' Note that you can install each package anywhere in the script before
#' you actually need to use the functions or data in the package.
#' To get information on a package, use, for example:
#+ eval=FALSE
help(package=spida2)
#' 
library(spida2) # devtools::install_github('gmonette/spida2')
library(p3d)  # devtools::install_github('gmonette/p3d')
library(lattice) # for graphs with panels
library(latticeExtra) # to combine plots together
library(gridExtra)  # for grid.arrange
library(magrittr) # for function pipelines
library(car) # Companion to Applied Regression by John Fox
library(nlme) # for the 'gls' function 
#'
#' ## Exploring the Prestige data set
#' 
#' The 'Prestige' data set is in the car package so it's 
#' easy to load -- in contrast with typical data sets where 
#' most of your effort is spent getting the data in shape.
#' 
data(Prestige)  # with some packages you need to do this to 'load' the data frame
#+ eval=FALSE,echo=FALSE
?Prestige # this won't show in the document
#' A quick look at the data set:
dim(Prestige)
head(Prestige)
xqplot(Prestige) 
#' I find 'xqplot' invaluable for quick exploration but don't use it for polished reports or presentations.
#'
#' ### Regression of income on education and gender
#' 
xyplot( income ~ education , Prestige)
xyplot( income ~ education | type, Prestige)
xyplot( income ~ education, Prestige, groups = type)
gd(3) # in the spida2 package, uses a 'ggplot'-like theme for groups
xyplot( income ~ education, Prestige, groups = type)
xyplot( income ~ education | type, Prestige)
# The gd and gd_ functions in spida2 make lattice plots look more like ggplot2
gd_(col = 'blue', cex = 1.2, pch = 1)  # to change options when not using groups
xyplot( income ~ education | type, Prestige)
#' The followin only works interactively, so we set eval=FALSE
#+ eval=FALSE
xyplot( income ~ education, Prestige, groups = type)
trellis.focus()
zz <- panel.identify(labels=rownames(Prestige))
zz
trellis.unfocus()
#+
zz <- c(2, 90,96)
gd_(3,cex=0.9)
xyplot( income ~ education, Prestige, groups = type,
        labels = rownames(Prestige)) +
  layer( panel.text(x=x[zz],y=y[zz], labels=labels[zz],
                    cex = 1))
#'
#' ### Regression with interation
#' 
#' Fitting a regression with interaction between
#' education and type
#' 
fiti <- lm(income ~ education * type, Prestige)
summary(fiti)
#'
#' Testing the interaction term:
#'
#' Using likelihood ratio tests:
#' 
#' Type I Sums of Squares: Sequential hypotheses
anova(fiti)
#' Type II Sums of Squares: Extra SS respecting Principle of Marginality
library(car)
Anova(fiti)
#' Type III Sums of Squares
Anova(fiti, type = "III")
#'
#' Using Wald tests
#'
library(spida2)
#' Individual coefficients: can't use for tests involving
#' more than one degree of freedom 
wald(fiti)  
#' Joint test for interaction:
wald(fiti,":") 
#' Joint test for type:
wald(fiti,"type") 
#' Joint test for education:
wald(fiti,"education") 
#'
#' Visualizing the fitted model
#' 
pred <- Prestige
pred <- cbind(pred, predict(fiti, newdata = pred, se = T))
head(pred)
pred <- sortdf(pred, ~ education)
xyplot(fit ~ education, pred, 
       groups = type,
       type = 'l')
#' Adding uncertainty:
library(latticeExtra)
library(spida2)
xyplot(fit ~ education, pred, 
       groups = type,
       type = 'n',
       fit = pred$fit,
       upper = pred$fit + 2*pred$se.fit,
       lower = pred$fit - 2*pred$se.fit,
       subscripts = TRUE) +
  glayer(panel.fit(...))
# in panels
xyplot(fit ~ education | type, pred, 
       type = 'n',
       fit = pred$fit,
       upper = pred$fit + 2*pred$se.fit,
       lower = pred$fit - 2*pred$se.fit,
       subscripts = TRUE) +
  layer(panel.fit(...))
#'
#' In 3d:
#'
#+ eval=FALSE
library(p3d) # devtools::install_github('gmonette/p3d')
Init3d()
Plot3d(income ~ education + women | type, Prestige)
fg()
Id3d()
#'
#'
#' Fitting a regression:
#'
fit <- lm(income ~ education + women, Prestige)
summary(fit)
#'
#' ### Regression diagnostics
#'
#' Standard diagnostics for linear models:
#' 
#' 1. ressiduals vs yhat: look for heteroscedasticity, non-linearity
#' 2. normal quantile plot of residuals: look for gaps, clusters, kurtosis, skewness
#' 3. square root of absolute residual versus yhat: fitted line can show heteroscedasticity
#' 4. residual vs leverage (~ Mahalanobis distance in predictor space): great to identify
#'    type 1, 2 and 3 outliers and to help see their potential impact on regression. Note:
#'       * Type 1: in upper or lower left: Increases width of CIs, increases p-values
#'       * Type 2: in middle right: Shrinks CIs, shrinks p-values
#'       * Type 3: in upper of lower right: affects $\beta$s, indeterminate impact on width of CIs, p-values 
#'
plot(fit, id.n = 7)  # id.n is the number to identify
#'
#' Added-variable plots: ideal to view leverage and influence
#' on individual coefficients
#' 
avPlots(fit, id.n = 7)
#'
#' ## Exploring expected income
#' 
#' We explore expected income as a function of gender, education and type of occupation
#'
fit <- lm( income ~ education * women * type, Prestige)
#'
#' One of my favourite exam questions is to display output
#' like this and ask "what is exact interpretation of 
#' the coefficient labelled 'women'? the coefficient 
#' labelled 'typewc'? etc.
#'
summary(fit)
#' 
#' Some diagnostics:
#'
#' Looking at residuals is an opportunity to think of factors
#' that might have been omitted in the model.  Is there a clump of
#' residuals with a common attribute?
#'
#' Does the model look reasonable? What should we look for?
#' 
plot(fit, id.n = 6)
avPlots(fit, id.n = 6)
#'
#' ### Looking at the model in 3d
#'
#+ eval=FALSE
library(p3d)
Init3d()
Plot3d(income ~ education + women | type, Prestige,
       col = c('blue','green','orange'))
Axes3d()
Id3d()
Fit3d(fit)
fitq <- update(fit, 
        . ~ type * (education * women + 
                      I(education^2) + I(women^2)))
Pop3d(6)
Fit3d(fitq) # shows how quads don't extrapolate here
#'
#' Questions:
#' 
#' 1. How many parameters do we have? 
#' 2. How many observations? 
#' 3. Is the ratio reasonable? 
#' 4. How should we handle the large standard deviation of 
#'    residuals at the higher levels of predicted incomes?
#'    - transform the response? e.g. log(income) which would allow us to estimate the proportional
#'      'effect' of a change in x. 
#'    - modify the model so it adapts to heteroscedasticity.
#' 5. Should we simplify the model?
#'
xyplot( income ~ education | cut(women,5), Prestige, groups = type)
# we get a more even distribution with cut by using a transformation of a more uniformly distributed variable
xyplot( income ~ education | cut(log(women+1),5), Prestige, groups = type)
#'
#' The following commands will look very complex but there's a logic behind them.
#' 'Base graphics' are much easier to add to but they don't have panels. 
#' 'Lattice graphics' are much nicer in many ways but it is much more difficult
#' to add overlays.  The way I do things here, with 'latticeExtra' is a bit old-fashioned. 
#' 
#' The trendy
#' way is to use 'ggplot2' but I'm waiting for the next great thing to mature.
#' My bets are on 'ggvis' by Winston Chang to which I plan to convert as soon
#' as they decide to include panels (also called 'facets').
#' 
xyplot( income ~ education | cut(log(women+1),5), 
        Prestige, 
        groups = type,
        occ = rownames(Prestige)) + 
  glayer({ok <- (y > 10000); panel.text(x[ok],y[ok],occ[subscripts][ok],adj=1.1)})


Prestige$Type <- tr(Prestige$type,c("bc","wc","prof"),c('blue collar','white collar','professional'))
Prestige$Type <- with(Prestige, reorder(Type, education))
gd(col = c('blue','orange','green'), lwd = 2, lty = 1)
xyplot( income ~ education | cut(women,c(-1,0,5,10,25,101)), 
        Prestige, 
        groups = Type, subscripts = TRUE,
        occ = rownames(Prestige), 
        women = Prestige$women,
        auto.key = list(columns = 3, lines = T)) + 
  glayer({ok <- (y > 10000); panel.text(x[ok], y[ok], occ[subscripts][ok], adj=1.1)}) +
  glayer({ok <- (women[subscripts] < 1); panel.text(x[ok],y[ok],occ[subscripts][ok], adj=-0.1)}) +
  layer(panel.lmline(..., lwd = 2))+
  glayer(panel.lmline(..., col = 'blue', lty=3))

#'
#' ## Using Generalized Least Squares    
#'
#' We will use the 'gls' (generalized least-squares) 
#' function in the 'nlme' package to incorporate 
#' heteroscedasticity in the model.
#' 
library(nlme)
#+ eval=FALSE
?gls
#+
Prestige$occ <- rownames(Prestige)   # make the rownames a variable
fit <- gls(income ~ education * women * type, Prestige, na.action = na.omit)
summary(fit)
plot(fit, id = .1, idLabels = ~ occ)  # default not as much as with 'lm' but fit should be the same
#       - note how we use 'id' = proportion to identify
#         instead of 'id.n' = number to identify
#       - also the 'idLabels' are given with a formula
#'
#' There is heteroscedascity and we should do something to
#' take into account the much larger variance when the
#' fitted value is large.
#' 
#' Depending on circumstances, this could be addressed in
#' many ways some of which are:
#' 
#' 1. Perhaps the large residuals are anomalous or consequences of a process 
#'    not included in the model. They could be segregated or the model 
#'    enlarged to predict their value.
#' 2. Perhaps the natural model should be based on a transformation 
#'    of the response or of the predictors.  
#'    A transformation will sometimes restore linearity and homoscedasticity.  
#'    Find a good reference on Tukey's ladder of powers.  
#'    But don't transform mechanically. 
#'    If the relationship is not linear, often you want to use 
#'    a model that is not linear in the predictors, or 
#'    even not linear in the parameters.
#' 3. Perhaps it makes sense to have larger variance at 
#'    higher predicted values and the model should incorporate 
#'    a term to allow for heteroscedasticity. This is what we pursue below with 'gls' in the 'nlme' package.
#'
fit2 <- gls(income ~ education * women * type, Prestige, 
            weights = varPower(form = ~fitted(.)),
            na.action = na.omit,
            method = "ML")
summary(fit2)
wald(fit2, "women")

# refit with only education
fit2e <- gls(income ~ education , Prestige, 
            weights = varPower(form = ~fitted(.)),
            na.action = na.omit,
            method = "ML")
summary(fit2e)
# anova(fit2, fit2e) # should give LRT (likelihood ratio test) but it 
#          doesn't because the models have 
#          different y's due to missing 'type's
#'
#' Wald test to test the hypothesis that all coefficients except the intercept
#' and the 'main effect' of education can be set to 0
#' 
length(coef(fit2))
wald(fit2,3:12) 
#'
#' ***Note:*** None of the individual terms are significant, yet, together they are
#' hugely significant.
#' 
#' None of the terms involving 'women' are significant. Could we remove them all?
#' 
wald(fit2,'women') # uses regular expression to select terms to test 
#'
plot(fit2, id = .05, idLabels = ~ occ) # standardized residuals divided by estimated standard deviation
plot(fit2, resid(., type = 'p') ~ resid(., type = 'response'), 
     xlab = 'raw residual',
     ylab = 'standardized residual',
     id = .1, idLabels = ~ occ)
#'
#' Let's ask whether we can consider simplifying the model by dropping the 3-way interaction
#' 
wald(fit2, ':.*:' ) # we use a regular expression that matches terms 
#           with two or more ':'s, i.e. 3- and higher-way interactions
wald(fit2, ':' ) # we use a regular expression that matches 
#                all interactions 
# or more ':'s
wald(fit2, 'type')  # does type add to other variables?
wald(fit2, 'women') # etc ...
wald(fit2, 'education')
#'
#' Perhaps 'type' is almost equivalent to a categorical version of 'education'.
#' I will keep it nevertheless to illustrate estimation with a categorical variable.
#'  
#' #### Two-way interaction model:
#' 
#' To allow all interactions up to two-way interactions, you can use the 
#' notation below. 
fit3 <- gls(income ~ (education + women + type)^2, Prestige, 
            weights = varPower(form = ~fitted(.)),
            na.action = na.omit)
summary(fit3)
plot(fit3, resid(.,type='r') ~ education, id = .1, 
     idLabels = ~ occ)
plot(fit3, resid(.,type='p') ~ education, 
     id = .2, idLabels = ~occ)
#'
#' #### Model with quadratic terms in education and women
#' 
fit3q <- update(fit3, . ~ type * (education * women + I(education^2) +I(women^2)))
summary(fit3q)
#'
#' **Exercise:** Have a look at diagnostics and describe what they tell you.
#' 
#' #### Model formulas in R
#' 
#' I had trouble finding a good discussion of model formulas in the text book.
#' Here's a link to a 
#' [page with some information](http://science.nature.nps.gov/im/datamgmt/statistics/r/formulas/).
#' You can also get help in R with '?formula'.
#' 
#' Where in the textbook would you find the interpretation of the following
#' terms in the predictor formula:
#' 
#' 1. a categorical variable (preferably represented as a factor in R)
#' 2. a numeric variable
#' 3. an 'interaction' between numeric variables, e.g. 'education * women'
#' 4. an interaction between a numeric variable and a numeric variable
#' 5. an interaction between two categorical variables
#' 6. an interaction of a variable with itself: e.g. 'women * women', or 'women^2'
#'
#' ### Mathematical formula for a model
#'
#' To interpret coefficients, we need to be able to write the mathematical
#' prediction formula that corresponds to the model formula. 
#' 
#' One aspect of this is understanding the **X** matrix generated by the 
#' formula:
#' 
getX(fit3) %>% head  # top 6 rows
getX(fit3) %>% some  # from the 'car' package, prints 10 rows at random
getX(fit3q) %>% head
#'
#' The mathematical formula for 'fit3q', to pick a complex one, is
#' 
#' \[\begin{aligned}
#' \operatorname{E} (Y|W,E,T) =  
#' & {\beta _0} + {\beta _1}{T_p} + {\beta _2}{T_w} + {\beta _3}E 
#' + {\beta _4}W + {\beta _5}{E^2} + {\beta _6}{W^2} + {\beta _7}EW \\
#' &  + {\beta _8}{T_p}E + {\beta _9}{T_w}E + {\beta _{10}}{T_p}W 
#' + {\beta _{11}}{T_w}W + {\beta _{12}}{T_p}{E^2} + {\beta _{13}}{T_w}{E^2} \\
#' &  + {\beta _{14}}{T_p}{W^2} + {\beta _{15}}{T_w}{W^2} + {\beta _{16}}{T_p}EW + {\beta _{17}}{T_w}EW \\
#' \end{aligned} \]
#' 
#' where ${T_p} = 1$ if the occupation type is 'professional' and 0 otherwise and, similary for $T_w$ and 'white collar'.
#' Note that 'blue collar' is the *reference level*, i.e. the one that occupation type that correspondss to
#' $T_p = T_w = 0$.  With factors in R, the reference level is the first level of the factor, e.g.
levels(Prestige$type)
#'
#' ### General Linear Hypotheses and Estimators using Wald Tests  
#'
#' Once we express the formula mathematically, it is easy to see how we could use the model to 
#' ask all sorts of questions. For example, what is the increase in income associated with an increase in
#' education for a professional occupation that has no women? It is the ***partial derivative*** of 
#' $\operatorname{E}(Y|W,E,T)$ with respect to $E$:
#' 
#' \[\frac{\partial}{{\partial E}}\operatorname{E} (Y|W,E,T{\left. ) \right|_{W = 0;T = p}}\]
#' 
#' This, in general, will be a function of $W$, $E$ and $T$. So, for example, the association of
#' income with education for a professional male occupation is:
#' 
#' \[\frac{\partial}{{\partial E}}\operatorname{E} (Y|W,E,T{\left. ) \right|_{W = 0;T = p}}\]
#'
#' which may depend on $E$.
#' 
#' Thus there isn't a single number that describes the 'effect' of education. The 'effect' depends on the
#' context. We can estimate the coefficient for this association by differentiating with respect to 
#' $E$ and and setting $W=0$, $T_p = 1$ and $T_w = 0$:
#'
#' \[\begin{aligned}
#' \frac{\partial}{{\partial E}}\operatorname{E} (Y|W,E,T{\left. ) \right|_{W = 0;T = p}} =  
#' & {\beta _3} + 2{\beta _5}E + {\beta _7}W \\
#' &  + {\beta _8}{T_p} + {\beta _9}{T_w} + 2{\beta _{12}}{T_p}E + 2{\beta _{13}}{T_w}E \\ 
#' &  + {\beta _{16}}{T_p}W + {\beta _{17}}{T_w}W \\
#' \end{aligned} \]
#' 
#' We estimate this using the 'General Linear Hypothesis' as a linear combination of the vector of $\beta$s.
#' 
#' For example, to estimate the 'value of an extra year' when $E = 10$, we find 
#' \[ \hat{\eta} = L \hat{\beta} = \left[ {\begin{array}{*{20}{c}}
#' 0&0&0&1&0&{20}&0&0&1&0&0&0&20&0&0&0&0&0 
#' \end{array}} \right]\left[ {\begin{array}{*{20}{c}}
#' {{\hat{\beta} _0}} \\ 
#' {{\hat{\beta} _1}} \\ 
#' {{\hat{\beta} _2}} \\ 
#' {{\hat{\beta} _3}} \\ 
#' {{\hat{\beta} _4}} \\ 
#' {{\hat{\beta} _5}} \\ 
#' {{\hat{\beta} _6}} \\ 
#' {{\hat{\beta} _7}} \\ 
#' {{\hat{\beta} _8}} \\ 
#' {{\hat{\beta} _9}} \\ 
#' {{\hat{\beta} _{10}}} \\ 
#' {{\hat{\beta} _{11}}} \\ 
#' {{\hat{\beta} _{12}}} \\ 
#' {{\hat{\beta} _{13}}} \\ 
#' {{\hat{\beta} _{14}}} \\ 
#' {{\hat{\beta} _{15}}} \\ 
#' {{\hat{\beta} _{16}}} \\ 
#' {{\hat{\beta} _{17}}} 
#' \end{array}} \right]\] 
#' 
#' To estimate the variance of $\hat{\eta}$ we use $L \hat{Var}(\hat{\beta}) L'$.
#'
#' In R, we can do this with matrix multiplication:
L <- rbind("assoc. with Educ | male prof. E=10" = c(0,0,0,1,0,20,0,0,1,0,0,0,20,0,0,0,0,0))
L
#' With a 'gls' fit, we extract $\hat{\beta}$ with:
coef(fit3q)  
#' So the estimate is:
L %*% coef(fit3q)
#' Wow! -- don't do it!
#'
#' Now for the variance ... but there are functions that make this easier, 'lht' in 'car' and 'wald' in 'spida2':
wald(fit3q, L)
#'
#' What could be happening?
#' 
#' We could explore the association with education for various values of E, W and T by
#' building a larger L matrix. e.g.
#' 
L <- rbind(
  'male prof E=10' = c(0,0,0,1,0,20,0,0,1,0,0,0,20,0,0,0,0,0),
  'male prof E=12' = c(0,0,0,1,0,24,0,0,1,0,0,0,24,0,0,0,0,0),
  'male prof E=14' = c(0,0,0,1,0,28,0,0,1,0,0,0,28,0,0,0,0,0),
  'male prof E=16' = c(0,0,0,1,0,32,0,0,1,0,0,0,32,0,0,0,0,0))
wald(fit3q,L)
#'
#' But this would be extremely laborious.
#'  
#' Let's see the fitted model for various combination of education, women and type
#' 
#' First, create a data frame with the combinations for which we want to estimate the fit:
#' 
pred <- expand.grid(women = seq(0,100,20), education = seq(6,18,.2), type = levels(Prestige$type))
#' 'pred' is a data frame with the Cartesian product of its arguments
dim(pred)
head(pred,10)
tail(pred,10)
#' Now we add predicted income using our model:
pred$income.pred <- predict(fit3q, newdata = pred)
xyplot(income.pred ~ education | type, pred, groups = women, type = 'l')
xyplot(income.pred ~ education | type, pred, groups = women, type = 'l', 
       auto.key = T) + layer(panel.abline(v = 12))
gd(6, lty = 1, lwd = 2)
xyplot(income.pred ~ education | type, pred, groups = women, type = 'l', 
       auto.key = list(points = F, lines = T)) + 
  layer(panel.abline(v = 12))
#'
#' Estimating the 'value of education':
#' 
#' We print a list of expressions that generates a block of the X matrix:
#' 
Lfx(fit3q)
#'
#' We can use the list of expressions to generate the design matrix over
#' the data frame 'pred'.
#'
Led <- Lfx(
  fit3q,
  list( 1,
      1 * M(type),
      1 * education,
      1 * women,
      1 * M(I(education^2)),
      1 * M(I(women^2)),
      1 * education * women,
      1 * M(type) * education,
      1 * M(type) * women,
      1 * M(type) * M(I(education^2)),
      1 * M(type) * M(I(women^2)),
      1 * M(type) * education * women ),
  pred)
dim(Led)
head(Led)
#'
#' The list of expressions can be edited easily to differentiate with
#' respect to a continuous variable.
#'
#' To get the value of education, differentiate each term with respect to education.
#' Leave expression of the form 'M(...)' as is because they generate blocks
#' of the right size for factors. Also, products of 'M' objects generate the
#' columns of the X matrix for interaction terms.
#' 
Led.deriv <- Lfx(fit3q,
           list( 0,
                 0 * M(type),
                 1 ,
                 0 * women,
                 2 * M(I(education)),
                 0 * M(I(women^2)),
                 1 * women,
                 1 * M(type) * 1,
                 0 * M(type) * women,
                 2 * M(type) * M(I(education)),
                 0 * M(type) * M(I(women^2)),
                 1 * M(type) * 1 * women ),
           pred)
ed.deriv <- walddf(fit3q, Led.deriv, data = pred)
head(ed.deriv)
xyplot( coef ~ education | type, ed.deriv, groups = women, type = 'l',
        ylab = 'elope wrt education')
xyplot( coef ~ education | type, ed.deriv, groups = women, type = 'l',
        ylab = 'elope wrt education',
        layout = c(1,3), 
        auto.key = list(space = 'right',lines=T,points=F,title = "% women")) +
  layer(panel.abline(h=0))
#'
#' Most of the individual coefficients answer questions that are of little 
#' interest because they are ***marginal*** to other terms. e.g. dropping a non-significant intercept
#' just forces the model to go through the origin which, generally, would be
#' a completely unjustified arbitrary restriction on the model
#' 
#' Dropping a main effect, e.g. 'women' that also appears in an interactionExample of 'marginal' terms that should **almost never**
#'    be dropped:
#'    1. any term that is also included in a higher-order interaction
#'    2. any term in a polynomial except the highest order term
#'    3. the intercept which we can think of as being 'included' in all other terms
#'    
#' Are all quadratic terms jointly significant?
wald(fit3q, "2")   
#' Are all interaction terms jointly significant?
wald(fit3q, ":")  
#' Are all 3-way and higher interaction terms jointly significant?
wald(fit3q, ":.*:")   
#' Are all terms involving education jointly significant?
wald(fit3q, "education")  
#' Are all terms involving women jointly significant?
wald(fit3q, "women")   
#' Are all terms involving type jointly significant?
wald(fit3q, "type")
#'
#'
#' Some questions: Using this model:
#' 
#' 1. What does the coefficient for 'women' mean?
#' 2. What does the coefficient for 'women^2' mean?
#' 3. How would you estimate the increase in income associated with an extra
#'    year of education in a white collar occupation that is 40% female?
#' 4. How would you estimate the increase in income associated with an extra
#'    year of education for a professional occupation that is 20% female?
#'
#' ### Showing predicted income with graphs
#' 
#' How much of an increase in income is associated with an additional
#' year of education?  Note that I found it hard to say: 'how much does income
#' go up when you have an extra year of education? in order to avoid the 
#' causal implications. This is quite hard and feels unnatural.
#' 
#' This is a sequence of graphs showing how predicted income, using the model
#' fitted above, varies by gender
#' composition and type of occupation.
#' 
#' Since we're using 2-D graphs, we can only show the dependent variable,
#' income, as a continuous variable and one more continuous variable for the
#' x-axis. But we have two continuous predictors. Let's choose education for
#' the x-axis, which leaves the problem of what to do with the percentage of women.  
#' We need to choose a number of representative levels for percentage to use
#' as an ordinal categorical variable.
#' 
#' There are a number of approaches to show predicted values for a model. I'll
#' illustrate two of them. One way generated a data frame with crossed 
#' combinations of the predictor variable values and plot the resulting predicted
#' values.  A second way uses a data frame whose values for predictor variables
#' reflects the interdependencies in the data. We'll see both. We'll start with
#' the second method.       
#' 
#' We'll use the data but we can't use the raw values of the '% women' because
#' we want to use just a few typical levels of that variable. One approach is to
#' round the values into an appropriate number of categories.
#' 
pred <- subset(Prestige, !is.na(type)) # drop occupations where type is NA
# discretize gender
(20 * round(pred$women/20)) %>% unique
(20 * round(pred$women/20)) %>% tab
pred$women.orig <- pred$women  # save original value because we're about to overwrite them
pred$women <- 20 * round(pred$women/20)  # we need to overwrite to use the 'predict' function 
pred$income.fit3 <- predict(fit3, newdata = pred) # predicted income using model fit3 

xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l')

pred <- sortdf(pred, ~ education)
xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l')

gd(7, lwd = 2, lty = 1)
xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l',
        auto.key = T)

xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l',
        layout = c(1,3),
        auto.key = 
          list( space='right', title='% of women'))

xyplot( income.fit3 ~ education | type, pred, 
        groups = women, type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,3),
        auto.key = 
          list( space='right', title='% of women',
                points = FALSE, lines = TRUE))

pred$Type <- tr(pred$type, levels(pred$type),
            c("blue collar",'professional','white collar'))

pred$Type <- reorder(pred$Type, pred$education)
xyplot( income.fit3 ~ education | Type, pred, 
        groups = women, type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,3),
        auto.key = list( space='right', title='% of women',
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)))
#' 
#' Switching panel and grouping variables
#' 
xyplot( income.fit3 ~ education | women, pred, groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,6),
        auto.key = list( space='right', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)))
#' Note the gray bar in the strips that shows the level of 'women' in each 
#' panel.  It's okay for exploration, but a lay viewer won't know what it
#' means. 
#' 
#' Also the colours are "strooped" (look up "Stroop effect").
#'
#'  
pred$Women <- with(pred, paste0(women, "% women"))
gd(col = c('blue','orange','dark green'), lty = 1, lwd = 2)
xyplot( income.fit3 ~ education | Women, pred, groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,6),
        auto.key = list( space='right', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)),
        as.table = TRUE)
#'
#' Ooops! The % labels are in ***lexicographical order***. We want them in
#' order corresponding to the numerical value of the percentage.
#'
pred$Women <- with(pred, reorder(Women, women)) 
      # reorders the levels of 'Women' according to the mean of 'women'
gd(col = c('blue','orange','dark green'), lty = 1, lwd = 3)
xyplot( income.fit3 ~ education | Women, pred, groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(1,6),
        auto.key = list( space='right', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)),
        as.table = TRUE)
#'
#' This presentation is not very effective because it is difficult to compare
#' levels vertically. But we have too many levels to show them side by side
#' vertically.
#'
xyplot( income.fit3 ~ education | Women, 
        subset(pred, Women %in% c("0% women",'40% women','80% women')), 
        groups = Type, 
        type = 'l',
        ylab = 'predicted income (Cdn $ in 1970)',
        layout = c(3,1),
        auto.key = list( space='top', 
                         points = FALSE, lines = TRUE),
        scales = list(y=list(alternating=F)),
        as.table = TRUE)

#'
#' # What is the 'value' of an additional year of education?
#' 
#' ## Using GLH and graphs to probe complex questions (INCOMPLETE)
#' 
#' Using GLH we will be able to, among other things:
#' 
#' 1. put confidence bounds around the predicted values above
#' 2. estimated the magnitude and significance of differences between types for various combinations of gender composition and education
#' 3. estimate association between education and income for various combinations of gender and type
#' 
#' # Some R Markdown examples (This is a level 1 heading)
#' 
#' Note that headings are created with 'number signs' and the
#' level of the heading is indicated with the number of
#' 'number signs'
#' 
#' ## This is a subheading
#' 
#' This is a bullet list:
#' 
#' - item number 1
#' - item number 2
#'  
#' and this is a numbered list:
#' 
#' 1. first time
#' 1. second item (it doesn't matter what number you use in the source code.
#'  
#' This is an example of a link that happens to show you
#' where to get help on R Markdown:
#' 
#' 1. [R Markdown Cheat Sheet](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf)
#' 2. [More documentation](http://rmarkdown.rstudio.com/)
#' 
#' You can also use LaTeX to generate equations:
#' 
#' $$f(x|\mu) = \frac{1}{\sqrt{2 \pi}}e^{-(x-\mu)^2/2}$$
#' 
#' You can also insert R output in a text paragraph: e.g.
#' 2 + 3 = `r 2+3`. Did we get that right?
#' 