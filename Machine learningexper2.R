# to get nice looking tables
remove.packages("plotROC")
devtools::install_github("hadley/ggplot2")
library(pander)
library(caret)
library(ggplot2)
library(psych)
library(OptimalCutpoints)
describe(ichscore$outcome)


# a utility function for % freq tables
frqtab <- function(x, caption) {
  round(100*prop.table(table(x)), 1)
}
# utility function to summarize model comparison results
sumpred <- function(cm) {
  summ <- list(TN=cm$table[1,1],  # true negatives
               TP=cm$table[2,2],  # true positives
               FN=cm$table[1,2],  # false negatives
               FP=cm$table[2,1],  # false positives
               acc=cm$overall["Accuracy"],  # accuracy
               sens=cm$byClass["Sensitivity"],  # sensitivity
               spec=cm$byClass["Specificity"])  # specificity
  lapply(summ, FUN=round, 2)
}

# amelia to look for misssing data

install.packages("Amelia")
library(Amelia)
missmap(ichscore, main = "Missingness Map Train")
str(ichscore)
summary(ichscore)

str(Book1)

# Recode integer/factors
ichscore$sex = as.factor(ichscore$sex)
ichscore$tentorium = as.factor(ichscore$tentorium)
ichscore$VE = as.factor(ichscore$VE)
ichscore$volume = as.factor(ichscore$volume)
ichscore$midline_shift = as.factor(ichscore$midline_shift)

# Recode oucome as factor
ichscore$outcome = as.factor(ichscore$outcome)




# Divide data into train and test
trainindex <- createDataPartition(ichscore$outcome, p=0.75, list=FALSE)
trainset <- ichscore[trainindex,]
testset <- ichscore[-trainindex,]

# Data visualisation and exploration
summary(trainset)
summary(testset)


# crosstabs for categorical variables
p = table(trainset[,c("outcome", "sex")])
q= table(trainset[,c("outcome", "VE")])
r= table(trainset[,c("outcome", "tentorium")])
s =table(trainset[,c("outcome", "volume")])

# List multiple tables together

t = list(p,q,r,s)
t
# Boxplots for continuous cariables
install.packages("fields")
library(fields)
par(mfrow=c(2,2))
bplot.xy(trainset$outcome, trainset$Age)
bplot.xy(trainset$outcome, trainset$RBS)
bplot.xy(trainset$outcome, trainset$GCS)
bplot.xy(trainset$outcome, trainset$MAP)
str(trainset)



# ggplot objects

# Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



Ap =ggplot(aes(x =outcome , y = Age), data = ichscore) + geom_boxplot (aes(fill= outcome))
Bp = ggplot(aes(x =outcome , y = RBS), data = ichscore) + geom_boxplot (aes(fill= outcome))
Cp = ggplot(aes(x =outcome , y = GCS), data = ichscore) + geom_boxplot (aes(fill= outcome))
Dp = ggplot(aes(x =outcome , y = MAP), data = ichscore) + geom_boxplot (aes(fill= outcome))


Mp  =multiplot(Ap , Bp, Cp, Dp,Ep,Fp,Gp,Hp, cols=2)


# Plotting categorical by categorical variable
Ep = ggplot(ichscore, aes(outcome, ..count..)) + geom_bar(aes(fill = volume), position = "dodge")
Fp =ggplot(ichscore, aes(outcome, ..count..)) + geom_bar(aes(fill = VE), position = "dodge")
Gp = ggplot(ichscore, aes(outcome, ..count..)) + geom_bar(aes(fill = tentorium), position = "dodge")
Hp = ggplot(ichscore, aes(outcome, ..count..)) + geom_bar(aes(fill = sex), position = "dodge")
Ip= ggplot(ichscore, aes(outcome, ..count..)) + geom_bar(aes(fill = midline_shift), position = "dodge")

Mp  = multiplot(Ap , Bp, Cp, Dp,Ep,Fp,Gp,Hp, cols=2)



# ICH score
# Roc curve

# Generation of derived variables for GCS and Age
ichscore$GCSscore2 =  ifelse(ichscore$GCS<=4,2,ifelse(ichscore$GCS<=12,1,ifelse(ichscore$GCS<=15,0)))
ichscore$Age2 = ifelse(ichscore$Age<80,0,1)

# Calculation of Revised ICH score

ichscore$rev.ich = as.numeric(ichscore$VE)+ as.numeric(ichscore$volume) + as.numeric(ichscore$tentorium) + ichscore$Age2+ichscore$GCSscore2

# Plotting ROC curve and optimal cut off

library(ROCR)
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

# creating prediction and performance objects

ichscore_pred = prediction(hemorrhage$ICH_score, hemorrhage$outcome)
perf_ich <- performance(ichscore_pred, "tpr", "fpr")
plot(perf_ich, add = TRUE, col = "green")
print(opt.cut(perf_ich,ichscore_pred ))

# creating prediction and performance objects

ichscore_pred = prediction(ichscore$rev.ich, ichscore$outcome)
perf_ich <- performance(ichscore_pred, "tpr", "fpr")
plot(perf_ich, add = TRUE, col = "green")

x = print(opt.cut(perf_ich,ichscore_pred ))

plot(perf)
library(plotROC)

ROCd = calculate_roc(ichscore$rev.ich, ichscore$outcome)
ggroc(ROCd)

hemorrhage




# Data Pre processing

# A. Handling missing values/unrelated variables in 

# 1. Remove columns/variables  with many NAs or which are unrelated like this 
myvars <- names(trainset) %in% c("RMCH.score", "X", "X.1","sICH.score", "midline_shift") 
trainset <- trainset[!myvars]
myvars2 = names(trainset) %in% c( "midline_shift") 
trainset <- trainset[!myvars2]

# Remove these columns from test set as well
myvars3 <- names(testset) %in% c("RMCH.score", "X", "X.1","sICH.score", "midline_shift") 
testset <- testset[!myvars3]


# Putting mean/median values in NAs
trainset$midline_shift <- ifelse(is.na(trainset$midline_shift), mean(trainset$midline_shift, na.rm = TRUE), trainset$midline_shift)

# Putting median values in NA

# Putting Principal Component/Dimensionality reduction

# Taking complete cases only from remaining cases

# 1list rows of data that have missing values 
 trainset[!complete.cases(trainset),]
#trainset <- na.omit(trainset)








# 2. Data transformations




library(caret)
library(randomForest)
 
 str(trainset)




# You should train the data on newichtrain and test accuracy on newichtest
 
 
#RF
 
 
# training and boot sampling can be optimized like
 fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 5,
 ## Estimate class probabilities 
    classProbs = TRUE,
 ## Evaluate performance using 
 ## the following function
 summaryFunction = twoClassSummary)

# metric = "ROC" can be added
 # see example in modelSvm2
 
ichscore$outcome = ifelse(ichscore$outcome==1,"Dead","Alive")
trainset$outcome = ifelse(trainset$outcome==1,"Dead","Alive")
testset$outcome = ifelse(testset$outcome==1,"Dead","Alive")

 
 
 # ROCmetric
 
 modelRF2 <- train(as.factor(outcome) ~ Age + sex + MAP +   
                      RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                    data = trainset, # Use the trainSet dataframe as the training data
                    method = "rf",# Use the "random forest" algorithm
                    metric = "ROC",
                    trControl = fitControl)
 
 modelRF2
 modelLR2 <- train(as.factor(outcome) ~ Age + sex + MAP +   
                      RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                    data = trainset, # Use the trainSet dataframe as the training data
                    method = "glm",# Use the "random forest" algorithm
                    family = "binomial",
                    metric = "ROC",
                    trControl = fitControl
 )
 
 modelLR2
 
 modelNB2 <- train(as.factor(outcome) ~ Age + sex + MAP +   
                      RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                    data = trainset, # Use the trainSet dataframe as the training data
                    method = "nb",# Use the "random forest" algorithm
                    metric = "ROC",
                    trControl = fitControl
 )
 modelNB2
 
 
 
 # comparative Results in ROC
 results2 <- resamples(list( LR = modelLR2, RF = modelRF2 , NB = modelNB2))
 summary(results2)
 
 # ROC curve draw
 library(pROC)
 library(ROCR)

  rf_pred <- predict(modelRF2, newdata = testset, type = "prob")
  nb_pred <- predict(modelNB2, newdata = testset, type = "prob")
  lr_pred <- predict(modelLR2, newdata = testset, type = "prob")
 

  
   predRF <- prediction(rf_pred[,2], testset$outcome)
  perfRF <- performance(predRF, "tpr", "fpr")
  plot(perfRF, main = "ROC curves for Randomforest,NaiveBayes,LogisticRegression")
  
  # Generate an ROC curve for the NewBayes method
  predNB <- prediction(nb_pred[,2], testset$outcome)
  perf_nb <- performance(predNB, "tpr", "fpr")
  plot(perf_nb, add = TRUE, col = "blue")
  
  # Generate an ROC curve for the NewBayes method
  predLR <- prediction(lr_pred[,2], testset$outcome)
  perf_lr <- performance(predLR, "tpr", "fpr")
  plot(perf_lr, add = TRUE, col = "red")
 
  # Gnerate ROC curve for ICHScore
  
  ichscore_pred = prediction(ichscore$rev.ich, ichscore$outcome)
  perf_ich <- performance(ichscore_pred, "tpr", "fpr")
  plot(perf_ich, add = TRUE, col = "green")
  
  
  # Add legends to plot
  legend("right", legend = c("randomForest", "
                             nb", "lr","ICHscore"), bty = "n", cex = 1, lty = 1,
         col = c("black", "blue", "red","green"))
  

# Generating ggROC
  detach("package:ggplot2", unload=TRUE)
  detach("package:plotROC", unload=TRUE)
  detach("package:caret", unload=TRUE)
  devtools::install_github("hadley/ggplot2")
  devtools::install_github("sachsmc/plotROC")
 library(ggplot2) 
library(plotROC)
library(plotROC)
test = data.frame(lr_pred[,2],rf_pred[,2],nb_pred[,2],testset$ICH_score,testset$outcome)
str(test)
library(reshape2)
K =melt(test)
longtest <- melt_roc(test, "testset.outcome", c("lr_pred", "rf_pred","nb_pred"))
ggplot(K, aes(d = testset.outcome, m = value, color = variable)) + geom_roc() + style_roc()


# ROC for trainset
rf_predA <- predict(modelRF2, newdata = trainset, type = "prob")
nb_predA <- predict(modelNB2, newdata = trainset, type = "prob")
lr_predA <- predict(modelLR2, newdata = trainset, type = "prob")

# prpearing data in format for ggROC,melt and cast with reshape 2
testA = data.frame(lr_predA[,2],rf_predA[,2],nb_predA[,2],trainset$ICH_score,trainset$outcome)
longtestA = melt(testA)
str(longtestA)
# Rename variable Factors in longtest

library(plyr)
longtestA$variable = revalue(longtestA$variable, c("lr_predA...2."="LR", "rf_predA...2."="RF" ,"nb_predA...2." ="NB","trainset.ICH_score" = "ICHscore" ))
ggplot(longtestA, aes(d = trainset.outcome, m = value, color = variable)) + geom_roc() + style_roc()
 

testset$outcome = as.factor(testset$outcome) 
 # confusion matrix
 RFpredict <- predict(modelRF2, newdata = testset)
 cm1 <- confusionMatrix(RFpredict, reference = trainset$outcome,positive="Dead" )
 LRpredict = predict(modelLR2, newdata = testset)
 cm2 <- confusionMatrix(LRpredict, trainset$outcome,positive="Dead" )
 NBpredict = predict(modelNB2, newdata = testset)
 cm3 = confusionMatrix(NBpredict, trainset$outcome,positive="Dead" )
 
 m1 <- sumpred(cm1)
 m2 <- sumpred(cm2)
 m3 <- sumpred(cm3)
 
 
 model_comp <- as.data.frame(rbind(m1, m2, m3))
 rownames(model_comp) <- c("RandomForest", "LogisticRegression", "NaiveBayes")
 pander(model_comp, style="rmarkdown", split.tables=Inf, keep.trailing.zeros=TRUE,
        caption="Model results when comparing predictions and test set")
 
 library(randomForest)
 # Plot of Importance
 varImpPlot(modelRF2$finalModel)
 
 
set.seed(7)
modelRF <- train(outcome ~ Age + sex + MAP +   
                 RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
               data = trainset, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                         number = 10) # Use 5 folds for cross-validation
)
modelRF
#GBM
set.seed(7)
modelGbm <- train(outcome ~ Age + sex + MAP +   
                   RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                 data = trainset, # Use the trainSet dataframe as the training data
                 method = "gbm",# Use the "random forest" algorithm
                 trControl = trainControl(method = "cv", # Use cross-validation
                                          number = 10) # Use 5 folds for cross-validation
)

#
#
set.seed(7)

modelGbm2 <- train(outcome ~ Age + sex + MAP +   
                    RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                  data = trainset, # Use the trainSet dataframe as the training data
                  method = "gbm",# Use the "random forest" algorithm
                  verbose = FALSE,
                  tuneGrid = gbmGrid,
                  ## Specify which metric to optimize
                  metric = "ROC"
                  trControl = trainControl(method = "cv", # Use cross-validation
                                           repeats = 10,
                                           ## Estimate class probabilities
                                           classProbs = TRUE,
                                           ## Evaluate performance using 
                                           ## the following function
                                           summaryFunction = twoClassSummary
                                           number = 10) # Use 5 folds for cross-validation
)


modelGbm2
# SVM
set.seed(7)
modelSvm <- train(outcome ~ Age + sex + MAP +   
                    RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                  data = trainset, # Use the trainSet dataframe as the training data
                  method = "svmRadial",# Use the "random forest" algorithm
                  trControl = trainControl(method = "cv", # Use cross-validation
                                           number = 10) # Use 5 folds for cross-validation
)

modelSVm2 <- train(outcome ~ Age + sex + MAP +   
                     RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                   data = trainset, # Use the trainSet dataframe as the training data
                   method = "svmRadial",# Use the "random forest" algorithm
                   metric = "ROC",
                   trControl = fitControl
)
#LVQ
set.seed(7)
modelLvq <- train(outcome ~ Age + sex + MAP +   
                    RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                  data = trainset, # Use the trainSet dataframe as the training data
                  method = "lvq",# Use the "random forest" algorithm
                  trControl = trainControl(method = "cv", # Use cross-validation
                                           number = 10) # Use 5 folds for cross-validation
)

#LR
set.seed(7)
modelLR <- train(outcome ~ Age + sex + MAP +   
                    RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                  data = ichscore, # Use the trainSet dataframe as the training data
                  method = "glm",# Use the "random forest" algorithm
                 family = "binomial", # for logistic
                  trControl = trainControl(method = "cv", # Use cross-validation
                                           number = 10) # Use 5 folds for cross-validation
)
 # Naive Bayes
set.seed(7)
modelNB <- train(outcome ~ Age + sex + MAP +   
                   RBS + GCS + tentorium + VE + volume , # Survived is a function of the variables we decided to include
                 data = trainset, # Use the trainSet dataframe as the training data
                 method = "nb",# Use the "random forest" algorithm
                 
                 trControl = trainControl(method = "cv", # Use cross-validation
                                          number = 10) # Use 5 folds for cross-validation
)

results <- resamples(list( LR = modelLR, RF = modelRF , NB = modelNB))
summary(results)

# Predicting on test


#Checking structure of testset
str(testset)
# Fixing NAs in testset

# Fixing NAs in test case

# Putting median values in NA

# Putting Principal Component/Dimensionality reduction

# Taking complete cases only from remaining cases

# 1list rows of data that have missing values 
#testset[!complete.cases(testset),]
#testset <- na.omit(testset)

RFpredict1 <- predict(modelRF, newdata = testset)
cm1 <- confusionMatrix(RFpredict1, reference = testset$outcome,positive="Dead" )
LRpredict = predict(modelLR, newdata = testset)
cm2 <- confusionMatrix(LRpredict, testset$outcome,positive="Dead" )
Gbmpredict = predict(modelGbm, newdata = testset)
cm3 = confusionMatrix(Gbmpredict, testset$outcome,positive="1" )
Lvqpredict =  predict(modelLvq, newdata = testset)
cm4 = confusionMatrix(Lvqpredict, testset$outcome,positive="1" )
NBpredict = predict(modelNB, newdata = testset)
cm5 = confusionMatrix(NBpredict, testset$outcome,positive="Dead" )
Svmpredict = predict(modelSvm, newdata = testset)
cm6 = confusionMatrix(Svmpredict, testset$outcome,positive="1" )
 
m1 <- sumpred(cm1)
m2 <- sumpred(cm2)
m3 <- sumpred(cm3)
m4 <- sumpred(cm4)
m5 <- sumpred(cm5)
m6 <- sumpred(cm6)

model_comp <- as.data.frame(rbind(m1, m2, m3, m4,m5,m6))
# model_comp <- as.data.frame(rbind(m1, m2,m5))
rownames(model_comp) <- c("RandomForest", "LogisticRegression", "GradientBoostingMachine", "LearningVectorQuantization","NaiveBayes","SupportVectorMachine")
# rownames(model_comp) <- c("RandomForest", "LogisticRegression", "NaiveBayes")
pander(model_comp, style="rmarkdown", split.tables=Inf, keep.trailing.zeros=TRUE,
       caption="Model results when comparing predictions and test set")


# Plot of Importance
varImpPlot(modelRF2$finalModel)


# Predicting new data

temps = data.frame(Age <- c(80),
                   sex <- as.factor(c(1)),
                   MAP <- c(220),
                   RBS <- c(340),
                   GCS <- c(7),
                   Age <- c(80),
                   tentorium <- as.factor(c(1)),
                   VE <- as.factor(c(1)),
                   volume <- as.factor(c(1))
                   )
temps$outcome <- predict(modelRF, newdata = temps)
temps$outcome

# calculation of new ich


temps = data.frame(Age <- c(input$a),
                   
                   Sex <- as.factor(input$b),
                   
                   MAP <- c(input$c),
                   
                   RBS <- c(input$e),
                   
                   GCS <- c(input$d),
                   
                   
                   
                   tentorium <- as.factor(input$f),
                   
                   VE <- as.factor(input$h),
                   
                   Volume <- as.factor(input$g)
                   
)

i =  ifelse(input$d<=4,2,ifelse(input$d<=12,1,ifelse(input$d<=15,0)))

j = ifelse(input$a<80,0,1)
k =  input$g + input$h + input$f + i + j


newich = ifelse(k<=3,"Alive","Dead")

p  = c ( " Machine learning algorithms used in Study " )
p
# ROC curve

# not working

install.packages("ROCR")
library(ROCR)
RFpredict1 <- predict(modelRF, newdata = testset)
cm1 <- confusionMatrix(RFpredict1, refernce = testset$outcome,positive="1" )

RFpredict2 <- predict(modelSVm2, newdata = testset, type= "prob")

gbmProbs <- predict(modelGbm, newdata = testset, type = "prob")

head(RFpredict2, n=4)

adult.rf <-randomForest(outcome ~ Age + sex + MAP +   
                          RBS + GCS + tentorium + VE + volume,data= trainset, mtry=2, ntree=1000,
                        keep.forest=TRUE, importance=TRUE,test= testset)

# generate probabilities instead of class labels type="prob" ensures that

#randomForest generates probabilities for both the class labels,

#we are selecting one of the value [2] at the end does that

<span style="line-height: 1.5;">
  adult.rf.pr = predict(adult.rf,type="prob",newdata=testset)[,2]
  
  #prediction is ROCR function
  
  adult.rf.pred = prediction(adult.rf.pr, testset$outcome)

#performance in terms of true and false positive rates
adult.rf.perf = performance(adult.rf.pred,"tpr","fpr")

#plot the curve
plot(adult.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#compute area under curve

auc <- performance(adult.rf.pred,"auc")

auc <- unlist(slot(auc, "y.values"))

minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")


###

ichscore$rev.ich = hemorrhage$VE + hemorrhage$volume + hemorrhage$tentorium + hemorrhage$Age2+ hemorrhage$GCSscore2
hemorrhage$nu.ich = ichscore
hemorrhage$VE = ifelse(hemorrhage$VE==1,0,1)
hemorrhage$volume = ifelse(hemorrhage$volume==1,0,1)
hemorrhage$tentorium = ifelse(hemorrhage$tentorium==1,0,1)


hemorrhage$VE = as.integer(factor(hemorrhage$VE))
hemorrhage$volume = as.integer(factor(hemorrhage$volume))
hemorrhage$tentorium = as.integer(factor(hemorrhage$tentorium))

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
ichscore$VE = as.numeric.factor(ichscore$VE)
ichscore$volume = as.numeric.factor(ichscore$volume)
ichscore$tentorium = as.numeric.factor(ichscore$tentorium)

# Optimal cut off better
library(OptimalCutpoins)



optimal.cutpoint.Youden<-optimal.cutpoints(X = "rev.ich", status = "outcome", tag.healthy = 0, 
                                           methods = "Youden", data = df)
 optimal.cutpoint.Youden<-optimal.cutpoints(X = "rev.ich", status = "outcome", tag.healthy = 0, 
methods = "Youden", data = ichscore)

summary(optimal.cutpoint.Youden)
print(optimal.cutpoint.Youden)
str(opimal.cutpoint.Youden)

G =list (optimal.cutpoint.Youden,optimal.cutpoint.Youden$Youden$Global$optimal.cutoff)
G

# Correlation matrix in R

library(magrittr)
library(dplyr)
library(corrplot)
n = Book1 %>% dplyr::select(Age,MAP,RBS,GCS)
mycorrelationmatrix <- cor(n)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(mycorrelationmatrix, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black")  

corrplot::corrplot(n, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black")  

# Rename variable in R with dplyr
library(dplyr)
n = select(df,  newvalue = oldvalue ,Age = X2 , RBS = X3 , GCS =X4)
select(Book1, Shift = Midlineshift)
 Book2 = rename(Book1, shift = 
                  Midlineshift) 
 Book4 = rename(Book2,shift = Shift, age = Age)
 # Recode a variable with R
 library(plyr)
 str(Book2)
 Book2$Outcome = revalue(as.factor(Book2$Outcome) , c("0" = "Dead", "1" ="Alive"))
 str(Book2)
 levels(Book2$Outcome)
 
 # This has two levels Dead and Alive
 # Relevel
 
 #http://www.cookbook-r.com/Manipulating_data/Recoding_data/
 #https://stat545-ubc.github.io/block014_factors.html
 # data$scode[data$sex=="M"] <- "1"
 data$scode[data$sex=="F"] <- "2"
 
 # Convert the column to a factor
 data$scode <- factor(data$scode)

