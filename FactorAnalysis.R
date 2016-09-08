#Factor Analysis
str(jhar)
# complete cases only
jhar = jhar[complete.cases(jhar),]
# converting factor into integer
jhar$Nine = as.numeric(jhar$Nine) 
# converting, eliminating outlier
jhar$Nine = ifelse(jhar$Nine>5,5,jhar$Nine)
# Looking again 
str(jhar)
# RowMean
jhar$MeanScore<-rowMeans(jhar[,7:29])

summary(jhar$MeanScore)
sd(jhar$MeanScore)
x = mean(jhar$MeanScore) + sd(jhar$MeanScore)
y = mean(jhar$MeanScore) - sd(jhar$MeanScore)

library(psych)
describe(jhar$SatisfactionGroups)

# Converting numeric into category
library(dplyr)
jhar = mutate(jhar,SatisfactionGroups = ifelse(MeanScore < y,"Low",ifelse(MeanScore > x,"High","Medium")))

# Reorder factors for efficient display
jhar$SatisfactionGroups1 <- factor(jhar$SatisfactionGroups, c("Low","Medium","High"))
library(dplyr)
jhar = rename(jhar,Satisfactiongroup = SatisfactionGroups1)


# plotting graphs aginst satisfaction Scores
library(ggplot2)
Ap =ggplot(aes(x = SatisfactionGroups1 , y = Age), data = jhar) + geom_boxplot (aes(fill= SatisfactionGroups))
Ap
Bp =ggplot(aes(x = SatisfactionGroups1 , y = Education), data = jhar) + geom_boxplot (aes(fill= SatisfactionGroups))
Bp
Cp =ggplot(aes(x = SatisfactionGroups1 , y = Occupation), data = jhar) + geom_boxplot (aes(fill= SatisfactionGroups))
Cp
Dp = ggplot(aes(x = SatisfactionGroups1 , y = Income), data = jhar) + geom_boxplot (aes(fill= SatisfactionGroups))
Dp
# Plotting against Sex
Ep = ggplot(jhar, aes(SatisfactionGroups1, ..count..)) + geom_bar(aes(fill = Sex), position = "dodge")
Ep

# one Way Anova for Satisfaction Groups
# One Way Anova (Completely Randomized Design)
fit <- aov(Age ~ SatisfactionGroups1, data= jhar) 
summary(fit)
plot(fit)
TukeyHSD(fit)

# one Way Anova for Satisfaction Groups
# One Way Anova (Completely Randomized Design)
fit1 <- aov(Occupation ~ Satisfactiongroup, data= jhar) 
summary(fit1)
TukeyHSD(fit1)
plot(fit1)

# one Way Anova for Satisfaction Groups
# One Way Anova (Completely Randomized Design)
fit2 <- aov(Income ~ Satisfactiongroup, data= jhar) 
summary(fit2)
TukeyHSD(fit2)
plot(fit2)
# one Way Anova for Satisfaction Groups
# One Way Anova (Completely Randomized Design)
fit3 <- aov(Education ~ Satisfactiongroup, data= jhar) 
summary(fit3)
TukeyHSD(fit3)

# Looking at a variable in detail(Descriptive)
library(psych)
library(pastecs)
describe(jhar$Education)
round(stat.desc(jhar$Education),2)

summary(jhar)
describe(jhar)
describeBy(jhar$Education,group=jhar$Satisfactiongroup)

statsgr = statsByjhar$Education(,c("Satisfactiongroup"))
statsgr

hist(jhar$one)
describe(jhar$one)
# centering and transformations
one.c = (jhar$one - mean(jhar$one))/sd(jhar$one)
hist(one.c)

describe(one.c)
ggplot(jhar,aes(x= Education))+geom_histogram() # Try with 10,20 and 30 bins
ggplot(jhar,aes(x= Education))+geom_boxplot()
library(ggvis)
jhar %>% 
  ggvis(~Education) %>% 
  layer_histograms(width =  input_slider(0, 2, step = 0.10, label = "width"),
                   center = input_slider(0, 2, step = 0.05, label = "center"))


# Multivariate summaries
cor(jhar[,-c(1,3,30:32)]) # Pair correlations for all except columns 1,3,30,31,32
pairs(jhar[,-c(1,3,30:32)])

# Aggregate,lookat summary by one or two factors
aggregate(Age~Satisfactiongroup,data= jhar,FUN = mean)
tapply(jhar$Age,jhar$Satisfactiongroup,mean)

aggregate(Age~Satisfactiongroup+Sex,data= jhar,FUN = mean)

funs = function(x){c(mean(x),sd(x))}

aggregate(Age~Satisfactiongroup+Sex,data= jhar,FUN = funs)


aggregate(Age~Satisfactiongroup+Sex,data= jhar,function(x){c(mean(x),sd(x))})

lapply(jhar[,-c(1,3)], funs)

# 1 variable Property and two by two table

table(jhar$Sex)
table(jhar$Satisfactiongroup)
table(jhar$Sex,jhar$Satisfactiongroup)

# Studying apply functions
str(apply) # apply function,use with matrix and array
# let us create matrix
jhar1 =as.matrix(jhar[-c(1,3,30:32)]) # created a subset without these columns
apply(jhar1,2,funs) # used mean and sd of all columns of matrix,2 =colum
apply(jhar1,1,funs) #  used mean and sd of all rows of matrix,1 =row
apply(jhar[,4:29],2,funs) # apply works with both rows and columns depending upon 1 or 2 while lapply only with columns and no need to specify margins
lapply(jhar[,4:29],funs)
#sapply same as lapply returns as vector not as list/matrix if possible
sapply(jhar[,4:29],funs)
lapply(jhar[,4:29],mean)
sapply(jhar[,4:29],mean)

apply(jhar[,c(2,4:29)],2,funs)
apply(jhar[-c(1,3,30:32)],2,funs)
# Preparing data

#1. exclude variables 
myvars <- names(jhar) %in% c("Patient.name", "Age", "Sex","Education","Occupation","X","Income") 
newdata <- jhar[!myvars]

str(newdata)
# converting factor into integer
newdata$Nine = as.numeric(newdata$Nine) 
# converting, eliminating outlier
 newdata$Nine = ifelse(newdata$Nine>5,5,newdata$Nine)
# Looking again at newdata
 str(newdata)
 # RowMean
jhar$MeanScore<-rowMeans(newdata[,1:23])
#Removing some extra unnedded variables not needed and not seen at first time
myvars2 = names(newdata) %in% c("X.1","X.2","X.3","X.4","X.5")
newdata <- newdata[!myvars2]
str(newdata)
# 2. Remove Rows with NA, only complete cases

newdata <- newdata[complete.cases(newdata),]
str(newdata)
newdata1 = data.matrix(newdata)
str(newdata1)

# Adding new column to a matrix,different from adding to data frame(http://stackoverflow.com/questions/14972926/r-programming-adding-extra-column-to-existing-matrix)
# Not needed newdata2<- cbind(newdata1,apply(newdata[,1:23],1,mean))

#library(dplyr)

#rowwise(m) %>%
 # mutate(ClANer = mean(c(j,k)))


jharu = data.matrix(newdata2)

labels(jharu)



# reconverting jharu to data frame as have to rename a column and it is easy in data frame
jharu = data.frame(jharu)
library(plyr)
rename(jharu, c("v24"="M"))

# was not able to rename

# summary statistic in R of each variable by psych function,here we are interested in v24
library(psych)
describe(jhar)

# calculated mean of v24 as 4.48,mean+sd = 4.94,mean-sd = 4.02, now aim is to create a factor label of high,medium,low
attach(jhar)

library(dplyr)
jhar = mutate(jhar,GRP = ifelse(MeanScore<4.08,"Low",ifelse(MeanScore>4.94,"High","Medium")))

str(jharu)

# reconverting jharu to matrix as it is easy to perform factor analysis in matrix only



str(newdata)
names(newdata)
#master = data.table(newdata)
#names(newdata) =c("Q1","Q2","Q3","Q4","Q5",Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23")
#setnames(master,c("Q1","Q2","Q3","Q4","Q5",Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23"))
 # 3. convert new data variables to numeric



# 4. Renaming columns in R
#library(data.table)
#setnames(newdata, old=c("one","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen","Twenty","Twenty.one","twenty.two","Twenty.three"), new=c("Q1", "Q2","Q3","Q4","Q5",Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23"))
# http://stackoverflow.com/questions/4605206/drop-columns-in-r-data-frame
#http://www.cookbook-r.com/Manipulating_data/Renaming_columns_in_a_data_frame/

# Adding a column containing rowmeans to matrix

str(raqData)
class(raqData)
# Real Data
getwd()

write.csv(jhar, file = "jhajjar.csv", sep="\t") #Export Manipulated data to desktop


# 4. Factor Analysis

# nFactors Tutorial
## INITIALISATION
#data(dFactors)                      # Load the nFactors dataset
#attach(dFactors)
#vect         <- Raiche              # Uses the example from Raiche
#eigenvalues  <- vect$eigenvalues    # Extracts the observed eigenvalues
#nsubjects    <- vect$nsubjects      # Extracts the number of subjects
#variables    <- length(eigenvalues) # Computes the number of variables
#rep          <- 100                 # Number of replications for PA analysis
#cent         <- 0.95                # Centile value of PA analysis

## PARALLEL ANALYSIS (qevpea for the centile criterion, mevpea for the
## mean criterion)
#aparallel    <- parallel(var     = variables,
    #                     subject = nsubjects, 
     #                    rep     = rep, 
      #                   cent    = cent
#)$eigen$qevpea  # The 95 centile

## NUMBER OF FACTORS RETAINED ACCORDING TO DIFFERENT RULES
#results      <- nScree(x=eigenvalues, aparallel=aparallel)
#results
#summary(results)

## PLOT ACCORDING TO THE nScree CLASS 
#plotnScree(results)


# Real Data

write.table(jharu, "F:/mydata.csv", sep="\t") #Export Manipulated data to desktop

jharu = newdata
library(nFactors)

ev <- eigen(cor(jharu))# get eigenvalues
ev
ap <- parallel(subject=nrow(newdata),var=ncol(newdata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
nS # Sisplays number of components retained by various methods kaiser(Eigenvalues>mean,Parallel analysis,optimal coordinates and Acceleration Factor)
summary(nS)
plotnScree(nS)
