# Look at variable set
str(study)
#creating new variables bias A, bias B, absolute values
study$biasA = abs(study$SAO2-study$SPO2A)
study$biasB = abs(study$SAO2-study$SPO2B)

# plot data

barplot(study$biasA)
barplot(study$biasB)

barplot(study)

# we see quite a few data wi


#summary of bias of pulse ox A
summary(study$biasA)
# compute precision of pulse ox A
sd(study$biasA)

#summary of bias of pulse ox B
summary(study$biasB)
# compute precision of pulse ox B
sd(study$biasB)

# difference in biases
t.test(study$biasB,study$biasA,paired = TRUE)

# comparing standard deviation/variance/precision between two groups(F test)
?var.test
var.test(study$biasB,study$biasA)

# Thus there is a statistically difference in precision as well
# we will calculate Bland altman plot and statistics, good links()https://cran.r-project.org/web/packages/BlandAltmanLeh/vignettes/Intro.html)
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

studynu = study[-c(21,30),]


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







?BA.est
data(fat)