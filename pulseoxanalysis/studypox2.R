# Reversing signals

studynu1$poxA = 0 - studynu1$poxA
studynu1$poxB = 0 - studynu1$poxB

# Generating faux age data

studynu1$Age = rnorm(129,50,12)
# generating faux sex data
x <- rep( c("Male","Female"), 130*c(0.511,0.489) )
studynu1$Sex= sample(x)
studynu1$Sex = as.factor(studynu1$Sex)
summary(studynu1$Sex)
# Generating performance index variable
studynu$PerformanceA = ifelse(abs(studynu$Bias.nonin >= 7),"Bad","Good")
studynu$PerformanceA = as.factor(studynu$PerformanceA)
summary(studynu$performanceA)
summary(studynu)
(studynu$PerformanceA)
studynu$PerformanceB = ifelse(abs(studynu$Bias.philips >= 7),"Bad","Good")
studynu$PerformanceB = as.factor(studynu$PerformanceB)

# Sensitivity Specificity, Performance

summary(studynu1$PerformanceA)
summary(studynu1$PerformanceB)

library(caret)

confusionMatrix(studynu1$Nonin,studynu1$Reference,positive = "Hypoxia" )
confusionMatrix(studynu1$Philips,studynu1$Reference,positive = "Hypoxia" )


library(dplyr)

BPA = function(a,b,c) {
  library(dplyr)
x =studynu1 %>% filter(SAO2> a & SAO2< b ) %>% summarise(mean(c),sd(c)) 
y = sqrt(x[,1]^2 + x[,2]^2) 
z = c(x[,1],x[,2],y) 
z
}

summary(studynu1$poxA)
BPA(70,100,studynu1$poxA)
BPA(70,100,studynu1$poxB)
BPA(60,80,studynu1$poxA)
BPA(60,80,studynu1$poxB)

sqrt(0.456^2+1.67^2)

x = study %>% filter(study$SAO2 >= 20 & study$SAO2 <=  100 ) %>% summarise(mean(Bias),sd(Bias)) 
y= sqrt(x[,1]^2 + x[,2]^2)
z = c(x[,1],x[,2],y)
z

study$Bias  = 0 -study$Bias
study[study$SAO2 > 90 & study$SAO2 < 95]%>% mean(study$Bias)

summary(study$SAO2)
str(study)
study = data.frame(study)
study1 = study %>% filter(SAO2<100 & SAO2>95) 
sqrt(1.21^2+1.66^2)
summary(studynu1$poxA)
sd(studynu1$poxA)


# Bland altman plot
library(BlandAltmanLeh)
bland.altman.plot(studynu1$SPO2A, studynu1$SAO2, main="Bland Altman Plot- Nonin ", xlab="Means", ylab="Differences")
bland.altman.plot(study$Saturation, study$SAO2, main="Bland Altman Plot- Overall ", xlab="Means", ylab="Differences")

library(ggplot2)
pl <- bland.altman.plot(A1, B1, graph.sys = "ggplot2")
print(pl)
bland.altman.stats(A1,B1)
 bland.altman.stats(study$Saturation, study$SAO2)
 
 summary(studynu1$SPO2A)
 
 study$SPO2 = c(studynu1$SPO2A,studynu1$SPO2B)
 study$SPO2 = NULL
 
 cor.test(study$Bias,study$Lactate)
 cor.test(study$Bias,study$SAO2)
str(study)


rm(Pulseoximetrydata)
study$Age = c(studynu$Age,studynu$Age)
study$Sex = c(studynu$Sex,studynu$Sex)
library(papeR)
studyl = convert.labels(study)

plot(studyl ,variables = "Bias" , by = "PulseOximeter")

study$Vasopressor = as.factor(study$Vasopressor)
study$Sepsis = as.factor(study$Sepsis)
study$Perfusion = as.factor(study$Sepsis)

study$Bias = 0 - study$Bias
t.test(study$Bias~study$Sepsis)

t.test(study$Stabilization.time~study$PulseOximeter)
t.test(study$Stabilization.time~study$Perfusion)

latex.table.cont(studyl)
xtable(summarize(studyl,group = "PulseOximeter"))
xtable(summarize(study , type = c("numeric"),quantiles = FALSE ))

data("mtcars")
head(mtcars)
xdigits(mtcars, pad = FALSE)
xdigits(mtcars, pad = TRUE)
xalign(mtcars)
xdisplay(mtcars)
a = xtable(mtcars, align = xalign(mtcars), digits = xdigits(mtcars),
       display = xdisplay(mtcars))

study$Sex = ifelse(study$Sex== 1, "Male", "Female")
str(study$Sex)
study$Sex = as.factor(study$Sex)
str(study)
print.xtable(a)
# printing via grouping variable
summarize(study,variables = c("Age" , "MAP" ,"Sex", "Bias" ,"Stabilization.time"), group = c("PulseOximeter"), quantiles = FALSE)


summarize(study,variables = c("Age" , "MAP" ,"Sex", "Bias" ,"Stabilization.time"), group = c("PulseOximeter"), quantiles = FALSE)


library(dplyr)
library(psych)
describe(studynu)
library(papeR)

# Capturing output
# capture.output(summary(mod),file="captureoutput.txt")
# Letting copy whole output

#https://stat.ethz.ch/R-manual/R-devel/library/base/html/sink.html

sink(file = "/home/anupam/Desktop/Pulseox.txt")
describe(studynu)
str(study)
summarize(study, type = "factor")
studynu$Hypotension = ifelse(studynu$SBP <= 90 , "Hypotensive","Normotensive")
studynu$Hypotension = as.factor(studynu$Hypotension)
summary(studynu$Hypotension)

cat("NEXT") 

devtools::install_github("gaborcsardi/crayon")
library(crayon)
cat(blue("Hello", "world!\n"))
summarize(study)

cor.test(study$Bias,study$SAO2)
t.test(study$Bias~study$PulseOximeter,paired = TRUE)

var.test(studynu$Bias.philips,studynu$Bias.nonin)

library(arm)

# correlation plot
# Let us select numerical variable first
library(dplyr)
study = data.frame(study)
J = dplyr::select(study,Bias, Hb, Lactate,SAO2)
K = as.matrix(K)
# convert to matrix
J = as.matrix(J)
library(Hmisc)
#http://www.statmethods.net/stats/correlations.html
# Size of correlation  plus p value as well

rcorr(J)
# visualize,scatterplot matrix
library(gclus)
dta  = J
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" ) 

study$Hypotension = c(studynu$Hypotension,studynu$Hypotension)
study$Hypotension = ifelse(study$Hypotension == 2, "Normotensive","Hypotensive")
study$Hypotension = as.factor(study$Hypotension)
K = dplyr::select(study,Vasopressor,PulseOximeter,Sepsis,Hypotension)
str(K)
library(polycor)
tetrachoric(K)


# http://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable

# Multiple t test,plots,grouping(https://rpubs.com/kaz_yos/1204)


multi.tests <- function(fun = t.test, df, vars, group.var, ...) {
  sapply(simplify = FALSE,                                    # sapply(simplify=T) better, elements named
         vars,                                                # loop on vector of outcome variable names
         function(var) {
           formula <- as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
           fun(data = df, formula, ...)                     # perform test with a given fun, default t.test
         }
  )
}

res.multi.t.tests <-
  multi.tests(fun = t.test,
              df = study,
              vars = c("Age","MAP","Hb","Lactate","Bias"),
              group.var = "PulseOximeter",
              var.equal = TRUE)
res.multi.t.tests
res1.multi.t.tests <-
  multi.tests(fun = t.test,
              df = study,
              vars = c("Bias"),
              group.var = c("PulseOximeter","Vasopressor","Sepsis","Hypotension"),
              var.equal = TRUE)
res1.multi.t.tests



junk = multi.tests(fun = plot,
                   df = study,
                   vars = c("Age","MAP","Hb","Lactate","Bias"),
                   group.var = "PulseOximeter",
                   var.equal = TRUE)
hetcor(study$Bias,study$PulseOximeter)
hetcor(J,K)

#http://stackoverflow.com/questions/12196756/significance-level-added-to-matrix-correlation-heatmap-using-ggplot2?rq=1


# 
library(psych)
mixed.cor(x = J,d =K)

str(K)
cor(J,K)
linreg1 = lm (formula1 , study )
display(linreg1)
formula1 = abs(Bias) ~  PulseOximeter + SAO2 +SAO2:PulseOximeter  
fit1 = lm(Bias ~ PulseOximeter*SAO2,study)


cor.test(study$Bias,study$SAO2)

Ph  = dplyr::filter(study, PulseOximeter == "Philips" ) 
%>% cor.test(Ph$Bias,Ph$SAO2)


data("ChickWeight")
# fit "dummy" model.
fit <- lm(weight ~ Diet * Time, data = ChickWeight)




sjp.int(fit1,type ="eff")
display(fit1)


library(sjPlot)

