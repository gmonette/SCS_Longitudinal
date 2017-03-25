#
#  SCS 2017: Longitudinal and Nested Data
#  Part 3:  The R Language
#
#  -- much of the code is borrowed from Fox,
#     Introduction to the R Statistical Computing Environment
#     ICPSR Summer Program, 2010-2011          
#
# With R you use a command line to formulate your analysis
#    - that means learning a language
#    - but it also means gaining much more power over your
#      data and its analysis
#    - as well as learning many powerful concepts through
#      the language.
#    - Don't think of the language as just a set of codes
#      to memorize
#    - but as a new set of concepts and ideas
# 

# Basics

    # arithmetic, interacting with the interpreter

        # basic arithmetic operations

2 + 3 # addition
2 - 3 # subtraction
2*3   # multiplication
2/3   # division
2^3   # exponentiation

sqrt(3/4)/(1/3 + 2/pi^2)
factorial(5)
sqrt(-1)       # square root of -1 (as a real number)
sqrt(-1+0i)    # square root of -1 (as a complex number)

1/0            # things you're not supposed to do in school
0/0
Inf/Inf
Inf + Inf
Inf - Inf
TRUE            
TRUE + 2        # **coercion** of logical to numeric
FALSE
FALSE + 2

        # precedence of operators

(3*2)^3
3*(2^3)
3*2^3
4^2 - 3*2
1 - 6 + 4
2^-3

(4^2) - (3*2) # use parentheses to group, disambiguate
4 + 3^2
(4 + 3)^2

-2--3
-2 - -3 # use spaces to clarify


    # functions, arguments to functions, obtaining help

log(100)
log(100, base=10)
log10(100) # equivalent
log(100, b=10)  # argument abbreviation
log(100,10)  # positional argument

help(log)
?log
args(log)

log(100, 10)  # arguments by position

example("log")

apropos("log")
help.search("log")

RSiteSearch("loglinear", "functions")

`+`(2, 3) # binary operators are really functions with two arguments 

    # vectors

c(1, 2, 3, 4)       # combine
c(1, Inf, NA, NaN)  # NA means 'missing'; NaN means numerical operation that is undefined
0/0                 # for example!
c("This","is","a","character",'vector')   # can be character -- using single or double quotes
c("Everyone's",'back',"year")             # can use literal single quote in double quote 
c('Everyone\'s','back','year')            # or 'escaped' quote in same type of quote

1:4     # integer-sequence operator
4:1

-1:2      # '-' stickier than ';' 
(-1):2    # Best to use parentheses to avoid surprises
-(1:2)
     
seq(1, 4)           # more general sequences
seq(2, 8, by=2)     # specify interval
seq(0, 1, by=0.1)   # non-integer sequence
seq(0, 1, length=11) # specify number of elements

    # vectorized arithmetic and functions
    
c(1, 2, 3, 4)/2
c(1, 2, 3, 4)/c(4, 3, 2, 1)
log(c(0.1, 1, 10, 100), 10)

    # ** recycling rule **  if a vector is too short, just recycle

c(1, 2, 3, 4) + c(4, 3)     # no warning if multiple fits 
c(1, 2, 3, 4) + c(4, 3, 2)  # produces warning otherwise

    # variables

x <- c(1, 2, 3, 4) # assignment using 'gets'
x # print

x/2
(y <- sqrt(x)) # assign and print

(x <- rnorm(100))
summary(x)  # a "generic" function

    # character and logical data
    
(words <- c("To", "be", "or", "not", "to", "be"))
paste(words, collapse=" ")
paste(words ,'or', words)

(vals <- c(TRUE, TRUE, FALSE, TRUE))
!vals                                 # negation

sum(vals) # **coercion** to numeric
sum(!vals) # not operator

    # ** coercion ** -- all elements in a vector must have same mode

c("A", FALSE, 3.0) # coercion mode that can represent all elements: here, numeric
c(1, FALSE)
(y <- c("3.0","ABCD","NA",NA))
as.numeric(y)
    
  # basic indexing
    
x[12]       # 12th element
words[2]    # second element
vals[3]     # third element

x[6:15] # elements 6 through 15
x[-(11:100)] # omit elements 11 through 100 (note parentheses)


    # comparison and logical operators
  <-
  ==
  =


1 == 2   # note == to test equality
1 != 2
1 <= 2
1 < 1:3     # Note ** recycling ** rule
3:1 > 1:3
3:1 >= 1:3
TRUE & c(TRUE, FALSE)  # and [NOTE: recycling rule]
c(TRUE, FALSE, FALSE) | c(TRUE, TRUE, FALSE)  # or

! c(T, F)   # abbreviations of TRUE and FALSE, best avoided!

(z <- x[1:10])
z < -0.5
z > 0.5
z < -0.5 | z > 0.5  #  < and > of higher precedence than |
abs(z) > 0.5  # absolute value
z[abs(z) > 0.5] # indexing by a logical vector


    # user-defined functions

mean(x)
sum(x)/length(x)

myMean <- function(x) sum(x)/length(x)
myMean # can be printed like any object

myMean(x)
y # from sqrt(c(1, 2, 3, 4))
myMean(y)
myMean(1:100)
x # global x undisturbed

myVar <- function(x) sum((x - myMean(x))^2)/(length(x) - 1)
myVar(1:100)
var(1:100) # check

    # cleaning up

objects()
remove(x, y, z, vals, words)
objects()

    # debugging using traceback()

letters
myVar(letters)

traceback()


# Duncan example
    

    # creating a data frame from data stored in a file

# You can use Excel and save the file as a .csv file. 
# First row should have variable names
# You can read a file from your computer the same as a file from the internet

getwd()     # R's working directory -- depends on how your started R
Duncan <- read.csv("Duncan.csv")   # to read a file in the working directory
    # to read the same file on the internet
Duncan <- read.csv("http://www.math.yorku.ca/people/georges/Files/SCS2011_R/Duncan.csv") 

    # To practice using file.choose

download.file("http://www.math.yorku.ca/people/georges/Files/SCS2011_R/Duncan.csv","Duncan.csv")
Duncan <- read.csv(file.choose())
Duncan
summary(Duncan)  # generic summary function
xqplot(Duncan)   # class room picture plot (in spida.beta)


    # attaching a data frame (best avoided)

prestige # error!

attach(Duncan)
prestige

        # the search path

search()

    # distributions and bivariate relationships

hist(prestige)

pairs(cbind(prestige, income, education), 
    panel=function(x, y){
        points(x, y)
        abline(lm(y ~ x), lty="dashed")
        lines(lowess(x, y))
        },
    diag.panel=function(x){
        par(new=TRUE)
        hist(x, main="", axes=FALSE)
        }
    )

scatmat <- function(...) { # user-defined function
    pairs(cbind(...),
        panel=function(x, y){
            points(x, y)
            abline(lm(y ~ x), lty=2)
            lines(lowess(x, y))
        },
        diag.panel=function(x){
            par(new=TRUE)
            hist(x, main="", axes=FALSE)
        }
    )
}

scatmat(prestige, income, education)
scatmat(Duncan)

plot(income, education)
ids <- identify(income, education, occupation)
Duncan[ids,]
rownames(Duncan) <- Duncan$occupation
some(Duncan)

    # fitting a regression

(duncan.model <- lm(prestige ~ income + education))
summary(duncan.model)  # again, summary generic

        # regression diagnostics

library(car)   # already loaded but just to be sure
plot(duncan.model)
search()

hist(rstudent(duncan.model))
qqPlot(duncan.model)

plot(hatvalues(duncan.model))
abline(h = 2*3/45) # twice the average hat-value
identify(hatvalues(duncan.model), labels=row.names(Duncan))

plot(cooks.distance(duncan.model))
abline(h = 4/(45 - 3)) # rough cutoff for Cook's D
identify(cooks.distance(duncan.model), labels=row.names(Duncan))

avPlots(duncan.model, id.method="identify", 
    labels=rownames(Duncan))

crPlots(duncan.model, span=0.7)

spreadLevelPlot(duncan.model)

ncvTest(duncan.model)
ncvTest(duncan.model, var.formula= ~ income + education)

        # refitting the model

(remove <- whichNames(c("minister", "conductor"), Duncan))
summary(update(duncan.model, subset=-remove))

detach("Duncan")

# Generic functions, methods, and the class-based S3 object system
    
summary(Duncan$type)
summary(Duncan$prestige)
summary(Duncan)
summary(duncan.model)

class(Duncan$type)
class(Duncan$prestige)
class(Duncan)
class(duncan.model)

summary # the generic summary function
args(summary.lm) # the lm summary method
summary(duncan.model)
summary.lm(duncan.model) # equivalent, but bad form

methods(coef)
coef(duncan.model) # invokes default method

?Mroz
(mod.mroz <- glm(lfp ~ ., family=binomial, data=Mroz)) # logistic regression
class(mod.mroz)
args(summary.glm)
summary(mod.mroz)
summary.glm(mod.mroz)

methods(variable.names)
variable.names(duncan.model)
variable.names(mod.mroz)  # invokes the lm method