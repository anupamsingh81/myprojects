# Look at variable set
str(study)
# let us lookat ggvis
study$label = rownames(study)
# Deleting column 1 serial as it is empty

study = study[,-c(1)]

# same operation could have been done by dplyr
# select[study, -Serial]
# Add variables plus groups for subgroup analysis
#creating  bias variables poxA,poxB

study$poxA = study$SAO2-study$SPO2A
study$poxB = study$SAO2-study$SPO2B

# create a new variable Reference

study$Reference = ifelse(study$SAO2 < 90 ,"Hypoxia","Normal")
study$Nonin = ifelse(study$SPO2A < 90 ,"Hypoxia","Normal")
study$Philips = ifelse(study$SPO2B < 90 ,"Hypoxia","Normal")

#subsetting groupwise for subgroup analysis
# important subgroup analysis are likely to be for Hb,lactate,Vasopressor,perfusion,MAP
# First we need to clean up Bp variable and divide it into MAP,SBP,DBP

#http://www.endmemo.com/program/R/gsub.php
# First use gsub function to remove slash with space
newnu = gsub("/"," ",study$BP)
newnu

# Then split character into parts

#  using a loop by call 'do.call'..
new <- do.call( rbind , strsplit( as.character( newnu ) , " " ) )

study$SBP = as.numeric(new[,1])
study$DBP= as.numeric(new[,2])
study$MAP = (study$SBP+2*study$DBP)/3

# We look at vasopressor group it is empty,those with yes are marked,rest are no by default, bad formatting, let us handle this
# Modifying vasopressor variable by ifelse since it is incomplete

study$Vasopressor = ifelse(study$Vasopressor=="yes","yes","no")

# Let us create a perfusion grouping varaible depending upon SBP and vasopressor
study$Perfusion = ifelse( study$Vasopressor == "yes" | study$SBP <= 90,"LOW" ,"NORMAL")
# it means if vasopressor use or sbp less than equal to 90 then it means low perfusion else normal or varible |

# Modifying sepsis variable as it is looking incomplete
study$Sepsis = ifelse(study$Sepsis=="yes" | study$Sepsis == "y" | study$Sepsis == "yes-2","yes","no")

# Data looks better now
t.test(study$poxA,study$poxB)
cor.test(study$SAO2,study$Lactate)
describe(study)

# Look for missing data
# http://www.statmethods.net/input/missingdata.html
is.na(study)
#no
#The function complete.cases() returns a logical vector indicating which cases are complete.

# list rows of data that have missing values
#mydata[!complete.cases(mydata),]

study[!complete.cases(study),]

# Look for outliers
# https://stat.ethz.ch/pipermail/r-help/2010-February/228728.html
# https://rexplorations.wordpress.com/2015/09/05/simple-outlier-detection-in-r/

# My primary concern is to detect outliers in my primary/secondary outcome variable,i like to sort

# other technique might be boxplot and otliers package, let us try all techniques
#First, load the "outliers" package.

#outlier_tf = outlier(data_full$target column,logical=TRUE)
#This gives an array with all values False, except for the outlier (as
d#efined in the package documentation "Finds value with largest difference
#between it and sample mean, which can be an outlier").  That value is
#returned as True.
#find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#This finds the location of the outlier by finding that "True" value within
#the "outlier_tf" array.
#data_new = data_full[-find_outlier,]
#This creates a new dataset based on the old data, removing the one row that
#contains the outlier

# sorting
studynu1 = study[order(study$poxB),]

# we can clearly see 21,30 have maximum biases

# let us try outliers package
library(outliers)
outlier_tf = outlier(study$poxB,logical=TRUE)
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
# it points to 30

# Let us try boxplot approach, an interactive approach would be good

boxplot(study$poxB)

# ggplot
library(ggplot2)
# Modifying boxplot for single boxplot

ggplot(study, aes(y = study$poxB, x = factor(1)))+geom_boxplot()+  scale_x_discrete("")
# its a static boxplot we can seea few at -20



# we created a variable label to mark the rows

library(dplyr)
library(ggvis)


study %>% ggvis(~factor(1), ~poxB) %>% layer_points(key:= ~label, size := 15)%>% layer_boxplots %>%
set_options(width = "auto") %>%
  add_tooltip(function(data) data$label)



# you can see it from ggvis
library(tidyr)
studynu2 = study %>% select(TTSA,TTSB,SPO2A,SPO2B,poxA,poxB)
studynu3 = studynu2 %>% gather(Stabilization,Time,TTSA:TTSB) 
studynu4 = studynu3 %>% gather(Biaspox,Bias,poxA:poxB) 


# Now letus try to make it amenable to ggvis/ggplot ideal data format, some hacks

dplyr::rename(study, M.SPO2.A = SPO2A )
  study = rename(study, M.SPO2.A = SPO2A , M.SPO2.B = SPO2B , M.pox.A = poxA,M.pox.B = poxB,M.TTS.A = TTSA, M.TTS.B = TTSB, M.predict.A = Nonin, M.predict.B = Philips)

  # let us move these variables to front
  
  # http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe
  
  library(dplyr)
 study =  study %>%
    select(M.SPO2.A, M.SPO2.B,M.TTS.A,M.TTS.B,M.pox.A,M.pox.B,M.predict.A,M.predict.B, everything())
 study[,9]
  
#
 
 require(data.table) ## 1.9.5,
 study = melt(setDT(study),idvars = 9:21, measure= patterns("^M.SPO2","^M.TTS","^M.pox","^M.predict" ), 
           value.name=c("Saturation", "Stabilization.time","Bias","Prediction"),variable.name="PulseOximeter") #idvars are variable which are not to be changed

 study$PulseOximeter = ifelse(study$PulseOximeter==1, "Nonin","Philips")
 
 
 t.test(study$Bias~study$PulseOximeter)
 library(ggplot2)
 ggplot(study, aes(x= SAO2, y= Bias, color = PulseOximeter)) + 
   geom_point() + scale_colour_hue(l=50) + # Use a slightly darker palette than normal
   geom_smooth(method=lm,   # Add linear regression lines
               se=FALSE) 
 
 ggplot(study, aes(x= SAO2, y= Saturation, color = PulseOximeter)) + 
   geom_point() + scale_colour_hue(l=50) + # Use a slightly darker palette than normal
   geom_smooth(method=lm,   # Add linear regression lines
               se=FALSE) +ylab("SPO2")
write.csv(study, file = "/home/anupam/Downloads/study2.csv")

# Code from an R Graphics Cookbook recipe, by Winston Chang
ggplot(mydataframe, aes(x=myxcolname, y=myycolname)) + 
  geom_bar(stat="identity")
  
 study %>% 
   ggvis(~Bias, ~ SAO2, fill = ~factor(PulseOximeter)) %>% 
   layer_points() %>% 
   group_by(PulseOximeter) %>% 
   layer_model_predictions(model = "lm")
 
 study %>% 
   ggvis(~Bias, ~ SAO2, fill = ~factor(PulseOximeter)) %>% 
   layer_points(key:= ~label, size := 15) %>% 
   group_by(PulseOximeter) %>% set_options(width = "auto") %>%
   add_tooltip(function(data) data$label) %>%
   layer_model_predictions(model = "lm")
 
 # requires package directlabels
 library("directlabels")
 myplot <- ggplot(study, aes(x= Bias, y= SAO2, color= PulseOximeter))+ geom_point()
 
 direct.label(myplot, "smart.grid")
 
 #
 study %>% ggvis(~factor(1), ~ Bias) %>% layer_points(key:= ~label, size := 15)%>% layer_boxplots %>%
   set_options(width = "auto") %>%
   add_tooltip(function(data) data$label)
 
 str(study)
 study$PulseOximeter = as.factor(study$PulseOximeter)
 length(study$PulseOximeter)
 
 ggvis(study, ~ study$SAO2, ~Bias) %>% layer_boxplots()
 
  # let us try make it interactive,directly giving us row
# iplots package is good
#http://www.r-statistics.com/2012/01/interactive-graphics-with-the-iplots-package-from-r-in-action/
library(iplots)
ibox(study[c("poxB", "poxA")])


#
studyLowperfusion = study %>% filter( Vasopressor == "yes" | SBP <= 90 )








library(dplyr)
study90100 = study %>% filter(SAO2 >= 90)
study8090 = study %>% filter(SAO2 < 90 & SAO2 >80 )
study80 = study %>% filter(SAO2 < 80)
study7090 = study %>% filter(SAO2 < 90 & SAO2 >70 )



# 





summary(study)
library(psych)
describe(study)


#betterway Hmisc or pastec library describe

library(pastecs)
stat.desc(study) # round data as well
round(stat.desc(study),2)



# Bias



# Sorting data to know bias

# ordered data
studymeth2 <- study[order(study$poxB),]


# 21,30 appear to be outliers with more than 20 bias, let us keep them out n reanalyse

# Dropping Rows
# tutorials
# http://stackoverflow.com/questions/12328056/how-do-i-delete-rows-in-a-data-frame
# http://stackoverflow.com/questions/7112872/removing-specific-rows-from-a-dataframe

study = study[-c(21,30),]



# we see quite a few data wi


#summary of bias of pulse ox A
summary(study$poxA)
# compute precision of pulse ox A
sd(study$poxA)

?sq


#summary of bias of pulse ox B
summary(study$poxB)
# compute precision of pulse ox B
sd(study$poxB)

BiasA = mean(study$poxA)
precisionA = sd(study$poxA)

ARMSNonin = sqrt(BiasA^2 + precisionA^2)


BiasB = mean(study$poxB)

precisionB = sd(study$poxB)

ARMSPhilips = sqrt(BiasB^2 + precisionB^2)


# difference in biases
t.test(study$poxB,study$poxA,paired = TRUE)

# comparing standard deviation/variance/precision between two groups(F test)
?var.test
var.test(study$poxB,study$poxA)


# Plot SpO2A n SaO2
plot(study$SPO2A, study$SAO2, main="Scatter plot", xlab = "SPO2 Nonin", ylab = "SAO2")
abline(0,1)

# Plot SpO2B n SAO2
plot(study$SPO2B, study$SAO2, main="Scatter plot", xlab = "SPO2 Philips", ylab = "SAO2")
abline(0,1)

#Sensitivity,specificity calculation



library(caret)

confusionMatrix(study$Nonin,study$Reference,positive = "Hypoxia" )
confusionMatrix(study$Philips,study$Reference,positive = "Hypoxia" )


# Bias in low perfusion group
BiasAlo = mean(studyLowperfusion$poxA)
precisionAlo = sd(studyLowperfusion$poxA)

ARMSNoninlo = sqrt(BiasAlo^2 + precisionAlo^2)


BiasBlo = mean(studyLowperfusion$poxB)

precisionBlo = sd(studyLowperfusion$poxB)

ARMSPhilipslo = sqrt(BiasBlo^2 + precisionBlo^2)

sort(study)


# Thus there is a statistically difference in precision as well
# we will calculate Bland altman plot  statistics, good links()https://cran.r-project.org/web/packages/BlandAltmanLeh/vignettes/Intro.html)
#https://cran.r-project.org/web/packages/MethComp/MethComp.pdf
# three packages Paired data,Methcomp,Blandaltmanleh seem good, let's download

#creating non abs bias variables poxA,poxB

study$poxA = study$SAO2-study$SPO2A
study$poxB = study$SAO2-study$SPO2B

# Scatter plot of Bias
# creating two extra variables to prevent extra typing..:-)
A = study$poxA
B = study$poxB

plot(study$poxA, study$poxA, main="Scatter plot")
abline(0,1)

# Mean difference plot
plot((A+B)/2, A-B, main="Mean-Difference-Plot")
library(BlandAltmanLeh)
bland.altman.plot(A, B, main="This is a Bland Altman Plot", xlab="Means", ylab="Differences")

library(ggplot2)
pl <- bland.altman.plot(A, B, graph.sys = "ggplot2")
print(pl)

ba.stats <- bland.altman.stats(A, B)

# A nice link for meth comparison ()http://rstudio-pubs-static.s3.amazonaws.com/10921_fc7bce3fb29242d1b61f49d72aa295b1.html)

# using methcomp package

# Let us gather set with only relevant variables

studymeth = data.frame(study$SPO2A,study$SPO2B,study$SAO2)

#rename variables

library(dplyr)
rename(studymeth, study.SAO2 = ABG) , SPO2A = study.SPO2A , SPO2B = study.SPO2B )

libr
studymeth = plyr::rename(studymeth, c("study.SAO2"= "ABG"))

studymeth = plyr::rename(studymeth, c("study.SPO2A"= "SPO2A"))
studymeth = plyr::rename(studymeth, c("study.SPO2B"= "SPO2B"))
str(studymeth)

# we will have to gather it in a format,a brief tutorial

library(tidyr)
str(iri)
data(iris)
dat <- gather(iris, variable, value, -Species) # Here evry variable except species has been melted

studymeth1 = gather(studymeth,variable,value)
# better way
studymeth1 = gather(studymeth, key = Method ,value = Saturation)
item = c(1:393)
studymeth1$item = item
str(studymeth1)
library(MethComp)
data(sbp)
str(studymeth1)
studymeth1$Method = as.factor(studymeth1$Method)
str(studymeth1)
studymeth1$item = as.numeric(studymeth1$item)



# ordered data
studymeth2 <- study[order(study$biasB),]


# 21,30 appear to be outliers with more than 20 bias, let us keep them out n reanalyse

# Dropping Rows
# tutorials
# http://stackoverflow.com/questions/12328056/how-do-i-delete-rows-in-a-data-frame
# http://stackoverflow.com/questions/7112872/removing-specific-rows-from-a-dataframe

study = study[-c(21,30),]


# Rerun
# Scatter plot of Bias
# creating two extra variables to prevent extra typing..:-)
A1 = studynu$poxA
B1= studynu$poxB

plot(A1, B1, main="Scatter plot")
abline(0,1)

# Mean difference plot
plot((A1+B1)/2, A1-B1, main="Mean-Difference-Plot")
library(BlandAltmanLeh)
bland.altman.plot(A1, B1, main="This is a Bland Altman Plot", xlab="Means", ylab="Differences")

library(ggplot2)
pl <- bland.altman.plot(A1, B1, graph.sys = "ggplot2")
print(pl)
bland.altman.stats(A1,B1)
ba.stats <- bland.altman.stats(A1, B1)
summary(ba.stats)

?bland.altman.plot()

t.test(A1,B1,paired = TRUE)
sd(A1)
sd(B1)

var.test(A1,B1)



cor.test(studynu$biasA,studynu$SAO2)
cor.test(studynu$biasB,studynu$SAO2)


# comparison

compframe = data.frame(studynu$biasA,studynu$biasB,studynu$BP,studynu$Hb,studynu$Lactate,studynu$Vasopressor)

compframe = plyr::rename(compframe, c("studynu.biasA"= "Pulseoxnonin"))
compframe = plyr::rename(compframe, c("studynu.biasB"= "PulseoxPhilips"))
# https://blog.rstudio.org/2014/07/22/introducing-tidyr/
compframe1 = compframe %>% gather(pulseox, bias, Pulseoxnonin:PulseoxPhilips)
compframe1$SAO2 = studynu$SAO2
str(compframe1)

#ggplot
#tricks # http://www.cookbook-r.com/Graphs/Scatterplots_%28ggplot2%29/



ggplot(compframe1, aes(x= bias, y= SAO2, color = pulseox)) + 
  geom_point() + scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE) 

 # requires package directlabels
 library("directlabels")
 myplot <- ggplot(compframe1, aes(x= bias, y= SAO2, color= pulseox)) + geom_point()

  direct.label(myplot, "smart.grid")
  
  
 
 

# Rerun
studymethnu = data.frame(studynu$SPO2A,studynu$SPO2B,studynu$SAO2)

studymethnu = plyr::rename(studymethnu, c("studynu.SAO2"= "ABG"))

studymethnu = plyr::rename(studymethnu, c("studynu.SPO2A"= "SPO2A"))
studymethnu = plyr::rename(studymethnu, c("studynu.SPO2B"= "SPO2B"))
str(studymethnu)

studymethnu1 = gather(studymethnu, key = meth ,value = y)
item = c(1:129,1:129,1:129)
studymethnu1$item = item
str(studymethnu1)

repl = rep(1,387)
studymethnu1$repl = repl

studymethnu1$meth = as.factor(studymethnu1$meth)

str(studymethnu1)
data(ox)
str(ox)

BA.est(studymethnu1)
plotByMeth <- ggplot(data = studymethnu1,
                     mapping = aes(x = meth, y = y, group = item)) +
  layer(geom = "point") +
  layer(geom = "line") +
  theme_bw() +
  theme(legend.key = element_blank())
plotByMeth



# moregraphs
#FirstGraph
s<-seq(length(pre))
par(bty="l")
boxplot(compframe$Pulseoxnonin,compframe$PulseoxPhilips,main="Raw data",xlab="Pulse oximeters",ylab="Bias",names=c("Nonin","Philips"),col=c("lightblue","lightgreen"))
pre = compframe$Pulseoxnonin
post = compframe$PulseoxPhilips
stripchart(list(pre,post),vertical=T,pch=16,method="jitter",cex=0.5,add=T)
segments(rep(0.95,length(pre))[s],pre[s],rep(2,length(pre))[s],post[s],col=1,lwd=0.5)
#Secondgraph
#Confidenceintervals eitherparametric (t.test) or non-parametric (wilcox.text)
#res&lt;-t.test(post,prä,paired=T,conf.int=T)
res<-wilcox.test(post,pre,paired=T,conf.int=T)

stripchart(post-pre,vertical=T,pch=16,method="jitter",main="Difference",ylab="Difference:Post–Prä",xlab="Median+/-95%CI")
points(1,res$estimate,col="red",pch=16,cex=2)
arrows(1,res$conf.int[1],1,res$conf.int[2],col="red",code=3,lwd=3,angle=90)
abline(h=0,lty=2)#Zero-effectline





#
M.width.A = rnorm(10,1,0.5)
M.width.B = rnorm(10,2,0.5)
M.length.A = rnorm(10,5,0.7)
M.length.B = rnorm(10,7,0.8)
M.height.A = rnorm(10,3,0.5)
M.height.B = rnorm(10,6,0.8)
df = data.frame(M.width.A,M.width.B,M.height.A,M.height.B,M.length.A,M.length.B)

library(tidyr)
df %>%
  gather(key, value) %>% extract(key, c("question", "loop_number"), "(M.\\..)\\.(.)")

require(data.table) ## 1.9.5,
df = melt(setDT(df),idvars =1:2, measure=patterns("^M.width", "^M.height","^M.length" ), 
          value.name=c("width", "height","length"), variable.name="species") #idvars are variable which are not to be changed
as.factor(df$sp)
df$species = ifelse(df$species==1, "A","B")




?BA.est
data(fat)