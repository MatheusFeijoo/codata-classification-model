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
ggplot(dat_rescale, aes(x = Gender, fill = Loan_status))+
  geom_bar(position = "fill") + 
  theme_classic()
