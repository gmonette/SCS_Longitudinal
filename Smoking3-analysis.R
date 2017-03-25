#' ---
#' title: "NATS 1500: Why People Think Statistics Lie"
#' author: "Georges Monette"
#' date: "`r format(Sys.time(), '%H:%M %d %B %Y')`"
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 4
#' ---
#' Generated:
{{format(Sys.time(), '%H:%M %d %B %Y')}}
#' 

library(p3d)   # devtools::install_github('gmonette/p3d')
library(spida2) # devtools::install_github('gmonette/yscs')
getwd()   # make sure Smoking3.csv is in the same directory as the
          # active directory for R, otherwise move Smoking.csv 
          # or change the active directory


dd <- read.csv("Smoking3.csv")

library(latticeExtra)
head(dd)
dd$cig <- dd$consumption.cigPC
dd$Cigarettes <- dd$consumption.cigPC
dd$pop <- dd$Pop.Total
dd$Life <- dd$lifeexp.Birth
#dd$region <- dd$regioncode
dd$log.total <- log(dd$total)
dd$Health <- dd$total

ds <- subset( dd, sex == "BTSX")
rownames(ds) <- ds$country
head(ds)

####  ANIMATION

Init3d(cex=1)

Plot3d( Life ~ Cigarettes + Health |region,ds) 
spinto()




Axes3d()
fg()

fit <- lm( Life ~ Cigarettes, ds)
summary(fit)
wald(fit)
Fit3d(fit, lwd = 3)
fitsq <- lm( Life ~ Cigarettes+I(Cigarettes^2), ds)
Fit3d(fitsq, lwd = 3, col = 'red')
Pop3d(4)


Id3d(pad=1)


Id3d("Canada")
fg()


# "Controlling" for Health$

spinto(-90)

fit1 <- lm( Life ~ Cigarettes, ds)
summary(fit1)
Fit3d(fit1)

fitlin <-  lm( Life ~ Cigarettes 
               + Health 
               ,ds)
Fit3d(fitlin)

fith <- lm( Life ~ Cigarettes 
      + Health 
      + log( Health),ds)

Fit3d(fith, col = 'red')
Pop3d(2)

#' Controlling for region
#' 
fitr <- 
  lm( Life ~ Cigarettes * Health * region,
      ds)

Fit3d(fitr, lwd = 3)
Pop3d(2*6)



#################################   END


fit2 <-lm( Life ~ Cigarettes + I(Cigarettes^2),ds)
Fit3d(fit2, lwd = 3, col = 'red')
coef(fit2)
-(coef(fit2)[2]/(2*coef(fit2)[3]))/365

#### 2014 PREPARATION

Init3d(cex=1.2)
Plot3d( lifeexp ~ cig + total|region,ds) 
spinto()
fg()
Id3d(pad=1)

fit <- lm(  lifeexp ~ cig, ds)
summary(fit)
wald(fit)
Fit3d(fit,lwd=3)
Axes3d()


fit <- lm(  lifeexp ~ (cig + I(cig^2)) * (total + log(total)), ds,na.action = na.exclude)
Anova(fit, type = 2)
plot(fit)
dim(ds)
d

length(studres(fit))
#Plot3d( studres(fit) ~ hatvalues(fit) + fitted(fit)| ds$region)
Plot3d( lifeexp ~ cig + total|region,ds) 

# Id3d(pad=2)
Anova(fit, type = 2)


dds <- subset(dd, sex != 'BTSX')
levels(dds$sex)
tab(dds, ~ sex)

library(p3d)
Plot3d( lifeexp ~ cig + total | region, dds)

Id3d(pad = 1, labels = with(dds, paste(country, sex)))

Plot3d( lifeexp ~ cig + log.total | region, dds)

Id3d(pad = 1, labels = with(dds, paste(country, sex)))

quad <- function(x) cbind( Lin=x,Quad=x^2)
fit <- lm( lifeexp ~ quad(log.total)*sex*region,
           subset( dds, region != "AFR"),
           na.action = na.omit)
summary(fit)
Anova(fit, type = 2)
wald(fit, 'sex')
wald(fit, 'sex:|:sex')

ds <- subset(dd, sex == 'BTSX')

fit <- lm( lifeexp ~ quad(log.total)*region,
           subset(ds, region != "AFR"),
           na.action = na.omit)
Anova(fit, type = 2)

fit <- lm( lifeexp ~ quad(log.total)*region+
             I(country=="Equatorial Guinea") +
             I(country=="Sierra Leone") +
             I(country=="Swaziland") +
             I(country=="Angola")+
             I(country=="Lesotho"),
           ds,
           na.action = na.omit)
summary(fit)
Anova(fit, type = 2)
wald(fit)
wald(fit, "Quad")




