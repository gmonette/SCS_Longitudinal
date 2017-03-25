#'
#' TO DO
#' 
#' ###############################  MORE ------------------------------------
In Stan_examples.R:
  
- how to test multiple df hypothesis
- implement wald for stanfit objects: 
  - allow renaming of coefficients thru wald
- partial L matrices
- etc.
- Add easy effect displays

#' ## Do summaries:
#' 
#' * Correct CI of slope from Part 2
#' * Do funny regression Paradox: The empire will collapse!, Our parents were so lame, how 
#'   did they manage? But stepping outside we see the big picture and understand: 1) why
#'   things look different from different perpectives, why the picture picture is different
#'   from either perpectives point of view.
#' * Type I, II and III in residual vs leverage plot
#' * Manipulating multilevel data:
#'     * capply
#'     * up -- and agg for industrial up??
#'     * merge
#'     * tolong and towide
#'     *      
#' 
#' ## Add material on multivariate response
#' 
#' * Look at shortitudinal data
#' 
#' COnsider SOtirios Liaskos example
#' 
#' ## Add Snijders and Boskers Multilevel R-Squared
#' Thanks Tom, I sheepishly realized this shortly thereafter. I am so used to
# using Stata which has the mltools .ado, that I thought there was something
# more to it. But for those of you curious it is:
#   
#   r-square level-1: var(residual error for Yij)/var(Yij)
# r-square level-2: var(residual error for Yj)/var(Yj)
# 
# In SPSS (and any other software) you can run an empty model to get the
# within and between variance and then run your model and get the within and
# between residual error variance, and plug in.
# 
# _____________________________
# 
# *Nate Breznau*
#   Postdoctoral Fellow
# Mannheim Centre for European Social Research (MZES)
# Bldg. A, Office A113
# A5,6 Postfach D-68159
# (+49/0) 621-181-2819
# Academic Website <https://sites.google.com/site/nbreznau/>
#   
#   On Wed, Feb 1, 2017 at 10:00 PM, Snijders, T.A.B. <t.a.b.snijders@rug.nl>
#   wrote:
#   
#   > Dear all,
# >
#   > The formula is so simple that you can easily calculate by hand.
# > Best,
# > Tom
# >
#   > =========================================
#   > Tom A.B. Snijders
# > Professor of Statistics and Methodology, Dept of Sociology, University of
# > Groningen
# > Emeritus Fellow, Nuffield College, University of Oxford
# > http://www.stats.ox.ac.uk/~snijders
# >
#   > On Wed, Feb 1, 2017 at 7:01 AM, Nate Breznau <breznau.nate@gmail.com>
#   > wrote:
#   >
#   >> Dear List,
# >>
#   >> I wonder if anyone has example code to calculate the Snijders and Bosker
# >> multilevel r-square in SPSS that I could borrow?
# >>
#   >> Many thanks!
#   >> _____________________________
# >>
#   >> *Nate Breznau*
#   >> Postdoctoral Fellow
# >> Mannheim Centre for European Social Research (MZES)
# >> Bldg. A, Office A113
# >> A5,6 Postfach D-68159
# >> (+49/0) 621-181-2819
# >> Academic Website <https://sites.google.com/site/nbreznau/>
#   