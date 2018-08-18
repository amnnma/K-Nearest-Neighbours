# Building Knn algorithm With standaradization 
# Building Knn algorithm Without standaradization 
# importance of data standardization
# Playing with the number of neighbours and checing the model performance  
# Condesing points 
# Building the knn algorithm using the Condesing points 
# validating the model performance 

# Cearing the workspace
rm(list=ls(all=TRUE))
# Setting the working directory
setwd("E:/MachineLearning/20170917_KNN-CF_Lab")

# Loading the required libraries

# read UniversalBank.csv 
bank.data <- read.csv("UniversalBank.csv")
# Getting the summary stats of the data
summary(bank.data)
# Subset the data by ignoring ID,ZIP.Code and columns
 # to remove the columns ID & ZIP Code from the data
bank.data <- subset(bank.data, select = -c(ZIP.Code,ID))
# convert the education variable as factor 
bank.data$Education <- as.factor(bank.data$Education)
str(bank.data)

# Create dummies for education variable ? how 
library(dummies)
education = dummy(bank.data$Education)

# and remove this variables from the original data 
bank.data$Education <- NULL
str(bank.data)
View(bank.data)

# data set and combine the dummified variable 
bank.data <-cbind(bank.data,education) 
str(bank.data)

#########################
# without standardization
#########################
# Choose the seed value 
set.seed(234)
# Take a random sample of  60% of the records for train data 
train_rows <- sample(1:nrow(bank.data), nrow(bank.data)*0.6)
bank_train <- bank.data[train_rows,]
bank_test <- bank.data[-train_rows,]

# Verifying the ratio of loan takers and non loan takers
table(bank_train$Personal.Loan)
table(bank_test$Personal.Loan)
table(bank.data$Personal.Loan)
# create data frames excluding the class variable "Personal.Loan"
bank_train_without_class <- subset(bank_train,select=-c(Personal.Loan))
bank_test_without_class <- subset(bank_test,select=-c(Personal.Loan))



# bankdata_trainwithclass = bankdata_train
# bankdata_testwithclass = bankdata_test

# N = 1/3/5/7
# Applying the model 
library(class)
neighbour = 1
pred1 = knn(train = bank_train_without_class,
            test = bank_test_without_class,
            cl = bank_train$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat1 = table(bank_test$Personal.Loan, pred1)
mat1
acc1 = sum(diag(mat1))/sum(mat1);acc1

# Activity : Repeat this for 3,5,7 and 9 nearest nieghbours and 
# create data frame of the results 

neighbour = 3
pred2 = knn(train = bank_train_without_class,
            test = bank_test_without_class,
            cl = bank_train$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat2 = table(bank_test$Personal.Loan, pred2)
mat2
acc2 = sum(diag(mat2))/sum(mat2);acc2

neighbour = 5
pred3 = knn(train = bank_train_without_class,
            test = bank_test_without_class,
            cl = bank_train$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat3 = table(bank_test$Personal.Loan, pred3)
mat3
acc3 = sum(diag(mat3))/sum(mat3);acc3

neighbour = 7
pred4 = knn(train = bank_train_without_class,
            test = bank_test_without_class,
            cl = bank_train$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat4 = table(bank_test$Personal.Loan, pred4)
mat4
acc4 = sum(diag(mat4))/sum(mat4);acc4


neighbour = 9
pred5 = knn(train = bank_train_without_class,
            test = bank_test_without_class,
            cl = bank_train$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat5 = table(bank_test$Personal.Loan, pred5)
mat5
acc5 = sum(diag(mat5))/sum(mat5);acc5



#table for diff nieghbours and respective accuracies

neighbours = c(1,3,5,7,9)

accuracies = c(acc1,acc2,acc3,acc4,acc5)



before <-data.frame(neighbours,accuracies)

######################
# With standardization
######################
# Standardize the data using 'Range' method
library(vegan)
bank.data.std <-decostand(bank.data, method = 'range') 
# split the data into train (60%) and test (40%) 
train_rows <- sample(1:nrow(bank.data.std), nrow(bank.data.std)*0.6)
bank_train_std <- bank.data.std[train_rows,]
bank_test_std <- bank.data.std[-train_rows,]


# bankdata_trainwithclass = bankdata_train
# bankdata_testwithclass = bankdata_test
bankstd_trainwithoutclass = subset(bank_train_std,select=-c(Personal.Loan))
bankstd_testwithoutclass = subset(bank_test_std,select=-c(Personal.Loan))

# Apply the knn model on these data sets
library(class)
neighbour = 1
pred1.std = knn(train = bankstd_trainwithoutclass,
            test = bankstd_testwithoutclass,
            cl = bank_train_std$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat1.std = table(bank_test_std$Personal.Loan, pred1.std)
mat1.std
acc1.std = sum(diag(mat1.std))/sum(mat1.std);acc1.std

# Create confusion matrix and compute the accuracy
# N = 1/3/5/7

# Activity : Repeat this for 3,5,7 and 9 nearest nieghbours and 
# create data frame of the results 
library(class)
neighbour = 3
pred2.std = knn(train = bankstd_trainwithoutclass,
                test = bankstd_testwithoutclass,
                cl = bank_train_std$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat2.std = table(bank_test_std$Personal.Loan, pred2.std)
mat2.std
acc2.std = sum(diag(mat2.std))/sum(mat2.std);acc2.std

library(class)
neighbour = 5
pred3.std = knn(train = bankstd_trainwithoutclass,
                test = bankstd_testwithoutclass,
                cl = bank_train_std$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat3.std = table(bank_test_std$Personal.Loan, pred3.std)
mat3.std
acc3.std = sum(diag(mat3.std))/sum(mat3.std);acc3.std


library(class)
neighbour = 7
pred4.std = knn(train = bankstd_trainwithoutclass,
                test = bankstd_testwithoutclass,
                cl = bank_train_std$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat4.std = table(bank_test_std$Personal.Loan, pred4.std)
mat4.std
acc4.std = sum(diag(mat4.std))/sum(mat4.std);acc4.std



library(class)
neighbour = 9
pred5.std = knn(train = bankstd_trainwithoutclass,
                test = bankstd_testwithoutclass,
                cl = bank_train_std$Personal.Loan, k = neighbour)


# Create confusion matrix and compute the accuracy
mat5.std = table(bank_test_std$Personal.Loan, pred5.std)
mat5.std
acc5.std = sum(diag(mat5.std))/sum(mat5.std);acc5.std


neighbours1 = c(1,3,5,7,9)

accuracies1 = c(acc1.std,acc2.std,acc3.std,acc4.std,acc5.std)




after <- data.frame(neighbours1,accuracies1) 
after


result <- cbind(before,after)

result
#using merge function

result_merge <- merge(before,after, by.x = "neighbours", by.y = "neighbours1", all.y = TRUE)

result_merge
######################################################################################
# Condensing to reduce the complexity of the model - 
# To understand drop=FALSE parameter have a look at http://adv-r.had.co.nz/Subsetting.html
#######################################################################################
keep = condense(bankstd_trainwithoutclass,bank_train_std$Personal.Loan)
dim(bank_train_std)
length(keep)
# condensing the number of records to compute distances from a test record 
condensed_bank_train <- bank_train_std[keep,]
condensed_bank_train_withoutclass <- subset(condensed_bank_train, select = -c(Personal.Loan))



dim(condensed_bank_train_withoutclass)

# take condensed data and run the model

# Apply the knn model on these data points
test_pred3 <- knn(train = condensed_bank_train_withoutclass, test =bankstd_trainwithoutclass,
                  cl =condensed_bank_train$Personal.Loan, k = neighbour )

mat3 = table(bank_train_std$Personal.Loan, test_pred3);mat3
acc3condnsd <- sum(diag(mat3))/sum(mat3);acc3condnsd

# Create confusion matrix and compute the accuracy

#################################
# run the model using FNN library
#################################
# apply the model and check the accuracy
install.packages("FNN")
library(FNN)
predFNN <- FNN::knn(train =bankstd_trainwithoutclass[keep,],
                    test = bankstd_testwithoutclass,
                    cl =bank_train_std[keep,]$Personal.Loan, algorithm = "kd_tree",k=3)
a <- table( bank_test_std$Personal.Loan,predFNN)
a
accFnn <- sum(diag(a))/sum(a);accFnn

# getting the nearest neighbours for each record of test

indicies <- knn.index(condensed_bank_train_withoutclass, k = 3, algorithm = "kd_tree")

indicies
head(indicies)
length(indicies)


########the goal of FNN is to make the algorithm work faster(it
#may or may not increase the accuracy)
# Indices of the 5 nearest neighbors to row 20 of test dataset :
