library(caret)
library(randomForest)
trainSet <- read.table("train.csv", sep = ",", header = TRUE)
testSet <- read.table("test.csv", sep = ",", header = TRUE)
head(trainSet)
str(trainSet)
head(testSet)
# crosstabs for categorical variables
table(trainSet[,c("Survived", "Pclass")])
table(trainSet[,c("Survived", "Embarked")])
table(trainSet[,c("Survived", "Sex")])
# Boxplots for continuous cariables
install.packages("fields")
library(fields)
bplot.xy(trainSet$Survived, trainSet$Age)

# The box plot of age for those who survived and and those who died are nearly the same. That means that Age probably did not have a large effect on whether one survived or not. The y-axis is Age and the x-axis is Survived (Survived = 1 if the person survived, 0 if not).
#Also, there are lots of NA's. Let's exclude the variable Age, because it probably doesn't have a big impact on Survived, and also because the NA's make it a little tricky to work with.
summary(trainSet$Age)
bplot.xy(trainSet$Survived, trainSet$Fare)

# As you can see, the boxplots for Fares are much different for those who survived and those who died. Again, the y-axis is Fare and the x-axis is Survived (Survived = 1 if the person survived, 0 if not).
summary(trainSet$Fare)

# No Nas for fair, good varaiable.

# Training the model uses a pretty simple command in caret, but it's important to understand each piece of the syntax. First, we have to convert Survived to a Factor data type, so that caret builds a classification instead of a regression model. Then, we use the train command to train the model (go figure!). You may be asking what a random forest algorithm is. You can think of it as training a bunch of different decision trees and having them vote
#(remember, this is an irresponsibly fast tutorial). Random forests work pretty well in *lots* of different situations, so I often try them first.


# Convert Survived to Factor
trainSet$Survived <- factor(trainSet$Survived)
# Set a random seed (so you will get the same results as me)
set.seed(42)
# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +   
                 Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
               data = trainSet, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation
)

# Cross-validation is a way to evaluate the performance of a model without needing any other data than the training data. It sounds complicated, but it's actually a pretty simple trick. Typically, you randomly split the training data into 5 equally sized pieces called "folds" (so each piece of the data contains 20% of the training data). Then, you train the model on 4/5 of the data, and check its accuracy on the 1/5 of the data you left out. You then repeat this process with each split of the data. At the end, you average the percentage accuracy across the five different splits of the data to get an average accuracy. Caret does this for you, and you can see the scores by looking at the model output:
model
testSet$Survived <- predict(model, newdata = testSet)
summary(testSet)
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)
testSet$Survived <- predict(model, newdata = testSet)
submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")

# For newdata
Pclass = c(1)
Sex = c("male")
SibSp = c(1)
Embarked = c("S")
Parch = c(0)
Fare = c(79)
h = data.frame(Pclass,Sex,SibSp,Embarked,Parch,Fare)
str(h)
h$Survived <- predict(model, newdata = h)

# Similar
temps = data.frame(Pclass <- c(1),
                   Sex <- c("male"),
                    SibSp <- c(1),
                    Parch <- c(0),
                    Fare <- c(79),
                    Age <- c(80))
str(temps)


h$Survived
temps$Survived <- predict(model, newdata = temps)
temps$Survived

