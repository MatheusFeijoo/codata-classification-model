library(plyr)
library(readr)
library(dplyr)
library(caret)

dat <- read_csv("data.csv")
glimpse(dat)

#Checking continuous variables
continuous <- select_if(dat, is.numeric)
summary(continuous)

#Plotting the distribution of the variables with the outliers
#ApplicantIncome
library(ggplot2)
ggplot(continuous, aes(x = ApplicantIncome)) +
  geom_density(alpha = .2, fill = "green")
applicant_income <- quantile(dat$ApplicantIncome, .98)
applicant_income
dat_drop <- dat %>%
  filter(ApplicantIncome<applicant_income)
dim(dat_drop)

#Standardizing the continuous variables
dat_rescale <- dat_drop %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(dat_rescale)

#Checking factor variables
#Selecting categorical column
dat_rescale <- as.data.frame(unclass(dat_rescale), stringsAsFactors = TRUE)
factor <- data.frame(select_if(dat_rescale, is.factor))
ncol(factor)

#Creating graph for each column
graph <- lapply(names(factor),
                function(x)
                  ggplot(factor, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))
graph

#Summary statistic
#Plotting gender income
ggplot(dat_rescale, aes(x = Gender, fill = Loan_status)) +
  geom_bar(position = "fill") + 
  theme_classic()

#Plotting education loan_status
ggplot(dat_rescale, aes(x = Education, fill = Loan_status)) +
  geom_bar(position = "fill") +
  theme_classic()

#Plotting property_area loan_status
ggplot(dat_rescale, aes(x = Property_area, fill = Loan_status)) +
  geom_bar(position = "fill") +
  theme_classic()
  
#Checking if the application income is related to loan amount
ggplot(dat_rescale, aes(x = ApplicantIncome, y = Loan_amount)) +
  geom_point(aes(color = Loan_status), size = 0.5) +
  stat_smooth(method = "lm", 
              formula = y~poly(x, 2), 
              se = TRUE, 
              aes(color = Loan_status)) +
  theme_classic()
  
#Visualizing the correlation between the variables
install.packages('GGally')
library(GGally)
# Convert data to numeric
corr <- data.frame(lapply(dat_rescale, as.integer))
# Plot the graph
ggcorr(corr,
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")

##Train/test set
dat2 <- dat
dat2$Loan_status <- ifelse(dat2$Loan_status == "Y", 1, 0)
set.seed(1234)
trainIndex <- createDataPartition(dat2$Loan_status, p = 0.7, list = FALSE, times = 1)
trainData <- dat2[trainIndex,]
testData <-  dat2[-trainIndex,]
print(dim(trainData)); print(dim(testData))

#Building the model
model_glm = glm(Loan_status~., family="binomial", data = trainData)
summary(model_glm)

#Baseline Accuracy
prop.table(table(trainData$Loan_status))

#Assessing the performance of the model
# Predictions on the training set
predictTrain <- predict(model_glm, trainData, type = "response")
# Confusion matrix on training data
train_mat <- table(trainData$Loan_status, predictTrain >= 0.5)
train_mat
# Predictions on the test set
predictTest <- predict(model_glm, testData, type = "response")
# Confusion matrix on test data
test_mat <- table(testData$Loan_status, predictTest >= 0.5)
test_mat

accuracy_train <- sum(diag(train_mat)) / sum(train_mat)
accuracy_train
accuracy_test <- sum(diag(test_mat)) / sum(test_mat)
accuracy_test
  
  
  
  
