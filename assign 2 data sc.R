setwd("~/Documents/datasets")
Skip to content
Search or jump to…
Pull requests
Issues
Codespaces
Marketplace
Explore

@Div123-star 
Heulito
/
  Data_Sience_R_Assignment_2
Private
Code
Issues
Pull requests
Actions
Projects
Security
Insights
Data_Sience_R_Assignment_2/clean data.R
@jemcghee3
jemcghee3 Create labels, convert to matrix, create validation set
Latest commit f16207c yesterday
History
2 contributors
@Heulito@jemcghee3
166 lines (136 sloc)  8.38 KB

install.packages("tidyverse")
install.packages("tidymodels")
install.packages("tune")
install.packages("workflows")
install.packages("tictoc")
install.packages("fastDummies")
install.packages("recipes")
install.packages("themis")
install.packages("keras")
install.packages("ROSE")


# load library
library(tidyverse)
library(tidymodels)   # packages for modeling and statistical analysis
library(tune)         # For hyperparemeter tuning
library(workflows)    # streamline process
library(tictoc)       # for timimg
library(fastDummies)
library(recipes)
library(themis)
library(keras)
library(ROSE)


#load data 
raw.data <- read.csv("Dataset-part-2.csv")

#check data type
str(raw.data)
summary(raw.data)

#delete not used column
clean.data <- select(raw.data, -c("ID", "FLAG_MOBIL"))

# delete rows >=7 fam members
clean.data <- filter(clean.data, CNT_FAM_MEMBERS < 7)
# rename NA
clean.data$OCCUPATION_TYPE[is.na(clean.data$OCCUPATION_TYPE)] <- "No data"

#handle high income
clean.data$AMT_INCOME_TOTAL <- ifelse(clean.data$AMT_INCOME_TOTAL >= 1000000,
                                      clean.data$AMT_INCOME_TOTAL / 100,
                                      clean.data$AMT_INCOME_TOTAL)

#transform Days_Birth into AGE
clean.data$DAYS_BIRTH <- clean.data$DAYS_BIRTH*-1/365
clean.data$DAYS_BIRTH <- round(clean.data$DAYS_BIRTH, digits = 0)
clean.data <- rename(clean.data, "AGE" = "DAYS_BIRTH")

#transform Days Employed to:
# 0 = Not employed
# 1 = 0-1   Year employed
# 2 = 1-5   Year employed
# 3 = 5-10  Year employed
# 4 = 10-20 Year employed
# 5 = 20 +  Year employed
clean.data$DAYS_EMPLOYED <- case_when(clean.data$DAYS_EMPLOYED > 0 ~ 0,
                                      clean.data$DAYS_EMPLOYED < 0 & clean.data$DAYS_EMPLOYED >= -365 ~ 1,
                                      clean.data$DAYS_EMPLOYED < 365 & clean.data$DAYS_EMPLOYED >= -(5*365) ~ 2,
                                      clean.data$DAYS_EMPLOYED < -(5*365) & clean.data$DAYS_EMPLOYED >= -(10*365) ~ 3,
                                      clean.data$DAYS_EMPLOYED < -(10*365) & clean.data$DAYS_EMPLOYED >= -(20*365) ~ 4,
                                      clean.data$DAYS_EMPLOYED < -(20*365) ~ 5)
clean.data <- rename(clean.data, "YEAR_EMPLOYED" = "DAYS_EMPLOYED")


#Transform Status = C to 6 and X to 7
clean.data$status <- case_when(clean.data$status == "C" ~ 6,
                               clean.data$status == "X" ~ 7,
                               clean.data$status == "0" ~ 0,
                               clean.data$status == "1" ~ 1,
                               clean.data$status == "2" ~ 2,
                               clean.data$status == "3" ~ 3,
                               clean.data$status == "4" ~ 4,
                               clean.data$status == "5" ~ 5
)




#replace space between words with underline
clean.data$NAME_INCOME_TYPE <- gsub(" ", "_", clean.data$NAME_INCOME_TYPE)
clean.data$NAME_EDUCATION_TYPE <- gsub(" ", "_", clean.data$NAME_EDUCATION_TYPE)
clean.data$NAME_FAMILY_STATUS <- gsub(" ", "_", clean.data$NAME_FAMILY_STATUS)
clean.data$NAME_HOUSING_TYPE <- gsub(" ", "_", clean.data$NAME_HOUSING_TYPE)
clean.data$OCCUPATION_TYPE <- gsub(" ", "_", clean.data$OCCUPATION_TYPE)
clean.data$OCCUPATION_TYPE <- gsub("-", ".", clean.data$OCCUPATION_TYPE)
clean.data$NAME_EDUCATION_TYPE <- gsub("/", "_", clean.data$NAME_EDUCATION_TYPE)
clean.data$NAME_HOUSING_TYPE <- gsub("/", "_", clean.data$NAME_HOUSING_TYPE)
clean.data$NAME_FAMILY_STATUS <- gsub("/", "", clean.data$NAME_FAMILY_STATUS)
clean.data$NAME_HOUSING_TYPE <- gsub("-", ".", clean.data$NAME_HOUSING_TYPE)
clean.data$OCCUPATION_TYPE <- gsub("/", "_", clean.data$OCCUPATION_TYPE)


#change data Typ
clean.data$CODE_GENDER         <- as.factor(clean.data$CODE_GENDER)        # Male Female
clean.data$FLAG_OWN_CAR        <- as.factor(clean.data$FLAG_OWN_CAR)       # Yes NO
clean.data$FLAG_OWN_REALTY     <- as.factor(clean.data$FLAG_OWN_REALTY)    # Yes No
clean.data$CNT_CHILDREN        <- as.integer(clean.data$CNT_CHILDREN)      # one person with 6 children, two with 7 children, one with 12, one with 19
clean.data$AMT_INCOME_TOTAL    <- as.integer(clean.data$AMT_INCOME_TOTAL)  #
clean.data$NAME_INCOME_TYPE    <- as.factor(clean.data$NAME_INCOME_TYPE)   # 4 Types
clean.data$NAME_EDUCATION_TYPE <- as.factor(clean.data$NAME_EDUCATION_TYPE)# 5 Types
clean.data$NAME_FAMILY_STATUS  <- as.factor(clean.data$NAME_FAMILY_STATUS) # 5 Types
clean.data$NAME_HOUSING_TYPE   <- as.factor(clean.data$NAME_HOUSING_TYPE)  # 6 Types
clean.data$AGE                 <- as.integer(clean.data$AGE)        # 
clean.data$YEAR_EMPLOYED       <- as.integer(clean.data$YEAR_EMPLOYED)     # 
#clean.data$FLAG_MOBIL          <- as.factor(clean.data$FLAG_MOBIL)         # 1 type 1
clean.data$FLAG_WORK_PHONE     <- as.factor(clean.data$FLAG_WORK_PHONE)    # 2 types 0 / 1
clean.data$FLAG_EMAIL          <- as.factor(clean.data$FLAG_EMAIL)         # 2 types 0 / 1
clean.data$OCCUPATION_TYPE     <- as.factor(clean.data$OCCUPATION_TYPE)    # 19 types
clean.data$CNT_FAM_MEMBERS     <- as.integer(clean.data$CNT_FAM_MEMBERS)   # I would delete rows with >= 7 fam members. we so would also outliers in cnt_children 7 and higher children
clean.data$status              <- as.factor(clean.data$status)             # Target




#Scale all columns which are integer
clean.data$CNT_CHILDREN     <- scale(clean.data$CNT_CHILDREN, center = TRUE, scale = TRUE)
clean.data$AMT_INCOME_TOTAL <- scale(clean.data$AMT_INCOME_TOTAL, center = TRUE, scale = TRUE)
clean.data$AGE              <- scale(clean.data$AGE, center = TRUE, scale = TRUE)
clean.data$YEAR_EMPLOYED    <- scale(clean.data$YEAR_EMPLOYED, center = TRUE, scale = TRUE)
clean.data$CNT_FAM_MEMBERS  <- scale(clean.data$CNT_FAM_MEMBERS, center = TRUE, scale = TRUE)

#create dummy columns
clean.data <- dummy_columns(clean.data, select_columns = "CODE_GENDER", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "FLAG_OWN_CAR", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "FLAG_OWN_REALTY", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "FLAG_WORK_PHONE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "FLAG_PHONE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "FLAG_EMAIL", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "NAME_INCOME_TYPE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "NAME_EDUCATION_TYPE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "NAME_FAMILY_STATUS", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "NAME_HOUSING_TYPE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
clean.data <- dummy_columns(clean.data, select_columns = "OCCUPATION_TYPE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)

#check for class imbalance
round(prop.table(table(clean.data$status)),8) # data is imbalanced 

# Split data into train and test data and create resamples for tuning
set.seed(2020)
train_test_split_data <- initial_split(clean.data, prop = 3/4)
clean.data_train <- training(train_test_split_data)
clean.data_test <-  testing(train_test_split_data)


# Create labels
train_labels <- clean.data_train[, "status"]
clean.data_train <- subset(clean.data_train, select = -c(status))
test_labels <- clean.data_test[, "status"]
test_data <- subset(clean.data_test, select = -c(status))

# Convert to matrices for tensorflow
train_data <- as.matrix(clean.data_train)
train_labels <- to_categorical(matrix(as.numeric(train_labels)-1))
test_data <- as.matrix(clean.data_test)
test_labels <- to_categorical(matrix(as.numeric(test_labels)-1))

# Create validation set
set.seed(1)
validate <- sample(1:nrow(train_data), 0.1 * nrow(train_data))
val_data <- train_data[validate, ]
val_labels <- train_labels[validate, ]
train_data <- train_data[-validate, ]
train_labels <- train_labels[-validate, ]

Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About

Model building----------------------------------------
  
  Skip to content
Search or jump to…
Pull requests
Issues
Codespaces
Marketplace
Explore

@Div123-star 
Heulito
/
  Data_Sience_R_Assignment_2
Private
Code
Issues
Pull requests
Actions
Projects
Security
Insights
Data_Sience_R_Assignment_2/model_building.Rmd
@jemcghee3
jemcghee3 Create labels, convert to matrix, create validation set
Latest commit f16207c yesterday
History
1 contributor
720 lines (611 sloc)  26.3 KB

---
  title: "Data Cleaning"
output: html_notebook
---
  
  ```{r}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```

```{r}
library(doParallel)
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
library(keras)
library(fastDummies)
library(tidyverse)
library(caret)
library(kerastuneR)
library(autokeras)
library(MASS)
```

load the data
```{r}
raw_data <- read.csv2("./Dataset-part-2.csv", header = TRUE, row.names=NULL, sep=",")
my_data <- raw_data
```

investigate the data
```{r}
summary(my_data)
colSums(is.na(my_data)) # occupation type is the only one with NA
```


fix data where there is a space
```{r}
levels(factor(my_data$NAME_INCOME_TYPE))
my_data$NAME_INCOME_TYPE <- ifelse(my_data$NAME_INCOME_TYPE == "Commercial associate", "Commercial", my_data$NAME_INCOME_TYPE)
my_data$NAME_INCOME_TYPE <- ifelse(my_data$NAME_INCOME_TYPE == "State servant", "State", my_data$NAME_INCOME_TYPE)
levels(factor(my_data$NAME_EDUCATION_TYPE))
my_data$NAME_EDUCATION_TYPE <- ifelse(my_data$NAME_EDUCATION_TYPE == "Academic degree", "Academic", my_data$NAME_EDUCATION_TYPE)
my_data$NAME_EDUCATION_TYPE <- ifelse(my_data$NAME_EDUCATION_TYPE == "Higher education", "Higher", my_data$NAME_EDUCATION_TYPE)
my_data$NAME_EDUCATION_TYPE <- ifelse(my_data$NAME_EDUCATION_TYPE == "Incomplete higher", "Incomplete", my_data$NAME_EDUCATION_TYPE)
my_data$NAME_EDUCATION_TYPE <- ifelse(my_data$NAME_EDUCATION_TYPE == "Lower secondary", "Lower", my_data$NAME_EDUCATION_TYPE)
my_data$NAME_EDUCATION_TYPE <- ifelse(my_data$NAME_EDUCATION_TYPE == "Secondary / secondary special", "Secondary", my_data$NAME_EDUCATION_TYPE)
levels(factor(my_data$NAME_FAMILY_STATUS))
my_data$NAME_FAMILY_STATUS <- ifelse(my_data$NAME_FAMILY_STATUS == "Civil marriage", "Civil", my_data$NAME_FAMILY_STATUS)
my_data$NAME_FAMILY_STATUS <- ifelse(my_data$NAME_FAMILY_STATUS == "Single / not married", "Single", my_data$NAME_FAMILY_STATUS)
levels(factor(my_data$NAME_HOUSING_TYPE))
my_data$NAME_HOUSING_TYPE <- ifelse(my_data$NAME_HOUSING_TYPE == "Co-op apartment", "Co-op", my_data$NAME_HOUSING_TYPE)
my_data$NAME_HOUSING_TYPE <- ifelse(my_data$NAME_HOUSING_TYPE == "House / apartment", "House", my_data$NAME_HOUSING_TYPE)
my_data$NAME_HOUSING_TYPE <- ifelse(my_data$NAME_HOUSING_TYPE == "Municipal apartment", "Municipal", my_data$NAME_HOUSING_TYPE)
my_data$NAME_HOUSING_TYPE <- ifelse(my_data$NAME_HOUSING_TYPE == "Office apartment", "Office", my_data$NAME_HOUSING_TYPE)
my_data$NAME_HOUSING_TYPE <- ifelse(my_data$NAME_HOUSING_TYPE == "Rented apartment", "Rented", my_data$NAME_HOUSING_TYPE)
my_data$NAME_HOUSING_TYPE <- ifelse(my_data$NAME_HOUSING_TYPE == "With parents", "With", my_data$NAME_HOUSING_TYPE)
levels(factor(my_data$OCCUPATION_TYPE))
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Accountants", "Accountant", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Cleaning staff", "Cleaning", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Cooking staff", "Cooking", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Core staff", "Core", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Drivers", "Driver", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "HR staff", "HR", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "High skill tech staff", "High", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "IT staff", "IT", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Laborers", "Laborer", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Low-skill Laborers", "Low", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Managers", "Manager", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Medicine staff", "Medicine", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Private service staff", "Private", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Realty agents", "Realty", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Sales staff", "Sales", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Secretaries", "Secretary", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Security staff", "Security", my_data$OCCUPATION_TYPE)
my_data$OCCUPATION_TYPE <- ifelse(my_data$OCCUPATION_TYPE == "Waiters/barmen staff", "Waiters", my_data$OCCUPATION_TYPE)
```
dummy columns (easy to do)
```{r}
my_data <- dummy_columns(my_data, select_columns = "CODE_GENDER", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "FLAG_OWN_CAR", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "FLAG_OWN_REALTY", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "FLAG_WORK_PHONE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "FLAG_PHONE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "FLAG_EMAIL", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "NAME_INCOME_TYPE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "NAME_EDUCATION_TYPE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "NAME_FAMILY_STATUS", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "NAME_HOUSING_TYPE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
my_data <- dummy_columns(my_data, select_columns = "OCCUPATION_TYPE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
```

fix data type
```{r}
head(my_data$AMT_INCOME_TOTAL)
my_data$AMT_INCOME_TOTAL <- as.integer(my_data$AMT_INCOME_TOTAL)
summary(my_data$AMT_INCOME_TOTAL)
head(my_data$NAME_INCOME_TYPE)
ggplot() +
  geom_histogram(data = my_data, aes(x = AMT_INCOME_TOTAL))
my_data$AMT_INCOME_TOTAL <- ifelse(my_data$AMT_INCOME_TOTAL >= 1000000, my_data$AMT_INCOME_TOTAL / 100, my_data$AMT_INCOME_TOTAL)
```

Fix income with boxcox
```{r}
out <- boxcox(my_data$AMT_INCOME_TOTAL, lambda = seq(-1,1,0.0001), plotit = F)
```


Where OCCUPATION_TYPE is NA, set it to 0
```{r}
my_data$OCCUPATION_TYPE_Cleaning[is.na(my_data$OCCUPATION_TYPE_Cleaning)] <- 0
my_data$OCCUPATION_TYPE_Cooking[is.na(my_data$OCCUPATION_TYPE_Cooking)] <- 0
my_data$OCCUPATION_TYPE_Core[is.na(my_data$OCCUPATION_TYPE_Core)] <- 0
my_data$OCCUPATION_TYPE_Driver[is.na(my_data$OCCUPATION_TYPE_Driver)] <- 0
my_data$OCCUPATION_TYPE_HR[is.na(my_data$OCCUPATION_TYPE_HR)] <- 0
my_data$OCCUPATION_TYPE_High[is.na(my_data$OCCUPATION_TYPE_High)] <- 0
my_data$OCCUPATION_TYPE_IT[is.na(my_data$OCCUPATION_TYPE_IT)] <- 0
my_data$OCCUPATION_TYPE_Laborer[is.na(my_data$OCCUPATION_TYPE_Laborer)] <- 0
my_data$OCCUPATION_TYPE_Low[is.na(my_data$OCCUPATION_TYPE_Low)] <- 0
my_data$OCCUPATION_TYPE_Manager[is.na(my_data$OCCUPATION_TYPE_Manager)] <- 0
my_data$OCCUPATION_TYPE_Medicine[is.na(my_data$OCCUPATION_TYPE_Medicine)] <- 0
my_data$OCCUPATION_TYPE_Private[is.na(my_data$OCCUPATION_TYPE_Private)] <- 0
my_data$OCCUPATION_TYPE_Realty[is.na(my_data$OCCUPATION_TYPE_Realty)] <- 0
my_data$OCCUPATION_TYPE_Sales[is.na(my_data$OCCUPATION_TYPE_Sales)] <- 0
my_data$OCCUPATION_TYPE_Secretary[is.na(my_data$OCCUPATION_TYPE_Secretary)] <- 0
my_data$OCCUPATION_TYPE_Security[is.na(my_data$OCCUPATION_TYPE_Security)] <- 0
my_data$OCCUPATION_TYPE_Waiters[is.na(my_data$OCCUPATION_TYPE_Waiters)] <- 0
my_data$OCCUPATION_TYPE_NA[is.na(my_data$OCCUPATION_TYPE_NA)] <- 1
```

Factorize the data
Note: this made it worse in Assignment 1
```{r}
summary(my_data)
my_data$CODE_GENDER_M <- as.factor(my_data$CODE_GENDER_M)
my_data$FLAG_OWN_CAR_Y <- as.factor(my_data$FLAG_OWN_CAR_Y)
my_data$FLAG_OWN_REALTY_Y <- as.factor(my_data$FLAG_OWN_REALTY_Y)
my_data$FLAG_WORK_PHONE_1 <- as.factor(my_data$FLAG_WORK_PHONE_1)
my_data$FLAG_PHONE_1 <- as.factor(my_data$FLAG_PHONE_1)
my_data$FLAG_EMAIL_1 <- as.factor(my_data$FLAG_EMAIL_1)
my_data$NAME_INCOME_TYPE_Pensioner <- as.factor(my_data$NAME_INCOME_TYPE_Pensioner)
my_data$NAME_INCOME_TYPE_State <- as.factor(my_data$NAME_INCOME_TYPE_State)
my_data$NAME_INCOME_TYPE_Student <- as.factor(my_data$NAME_INCOME_TYPE_Student)
my_data$NAME_INCOME_TYPE_Working <- as.factor(my_data$NAME_INCOME_TYPE_Working)
my_data$NAME_EDUCATION_TYPE_Higher <- as.factor(my_data$NAME_EDUCATION_TYPE_Higher)
my_data$NAME_EDUCATION_TYPE_Incomplete <- as.factor(my_data$NAME_EDUCATION_TYPE_Incomplete)
my_data$NAME_EDUCATION_TYPE_Lower <- as.factor(my_data$NAME_EDUCATION_TYPE_Lower)
my_data$NAME_EDUCATION_TYPE_Secondary <- as.factor(my_data$NAME_EDUCATION_TYPE_Secondary)
my_data$NAME_FAMILY_STATUS_Married <- as.factor(my_data$NAME_FAMILY_STATUS_Married)
my_data$NAME_FAMILY_STATUS_Separated <- as.factor(my_data$NAME_FAMILY_STATUS_Separated)
my_data$NAME_FAMILY_STATUS_Single <- as.factor(my_data$NAME_FAMILY_STATUS_Single)
my_data$NAME_FAMILY_STATUS_Widow <- as.factor(my_data$NAME_FAMILY_STATUS_Widow)
my_data$NAME_HOUSING_TYPE_House <- as.factor(my_data$NAME_HOUSING_TYPE_House)
my_data$NAME_HOUSING_TYPE_Municipal <- as.factor(my_data$NAME_HOUSING_TYPE_Municipal)
my_data$NAME_HOUSING_TYPE_Office <- as.factor(my_data$NAME_HOUSING_TYPE_Office)
my_data$NAME_HOUSING_TYPE_Rented <- as.factor(my_data$NAME_HOUSING_TYPE_Rented)
my_data$NAME_HOUSING_TYPE_With <- as.factor(my_data$NAME_HOUSING_TYPE_With)
my_data$OCCUPATION_TYPE_Cleaning <- as.factor(my_data$OCCUPATION_TYPE_Cleaning)
my_data$OCCUPATION_TYPE_Cooking <- as.factor(my_data$OCCUPATION_TYPE_Cooking)
my_data$OCCUPATION_TYPE_Core <- as.factor(my_data$OCCUPATION_TYPE_Core)
my_data$OCCUPATION_TYPE_Driver <- as.factor(my_data$OCCUPATION_TYPE_Driver)
my_data$OCCUPATION_TYPE_HR <- as.factor(my_data$OCCUPATION_TYPE_HR)
my_data$OCCUPATION_TYPE_High <- as.factor(my_data$OCCUPATION_TYPE_High)
my_data$OCCUPATION_TYPE_IT <- as.factor(my_data$OCCUPATION_TYPE_IT)
my_data$OCCUPATION_TYPE_Laborer <- as.factor(my_data$OCCUPATION_TYPE_Laborer)
my_data$OCCUPATION_TYPE_Low <- as.factor(my_data$OCCUPATION_TYPE_Low)
my_data$OCCUPATION_TYPE_Manager <- as.factor(my_data$OCCUPATION_TYPE_Manager)
my_data$OCCUPATION_TYPE_Medicine <- as.factor(my_data$OCCUPATION_TYPE_Medicine)
my_data$OCCUPATION_TYPE_Private <- as.factor(my_data$OCCUPATION_TYPE_Private)
my_data$OCCUPATION_TYPE_Realty <- as.factor(my_data$OCCUPATION_TYPE_Realty)
my_data$OCCUPATION_TYPE_Sales <- as.factor(my_data$OCCUPATION_TYPE_Sales)
my_data$OCCUPATION_TYPE_Secretary <- as.factor(my_data$OCCUPATION_TYPE_Secretary)
my_data$OCCUPATION_TYPE_Security <- as.factor(my_data$OCCUPATION_TYPE_Security)
my_data$OCCUPATION_TYPE_Waiters <- as.factor(my_data$OCCUPATION_TYPE_Waiters)
my_data$OCCUPATION_TYPE_NA <- as.factor(my_data$OCCUPATION_TYPE_NA)
```

filter(my_data, CNT_CHILDREN > 4) # do we need to do anything about them? Small number, two are huge outliers at 19 and 12

head(my_data)
levels(factor(my_data$status)) # these are my y-values
count(my_data, status) # these are my y-values
status     n
1      0 52133
2      1  6491
3      2   712
4      3   195
5      4   114
6      5   374
7      C  1805
8      X  5790

Need to adjust for different class sizes
```{r}
my_data$status <- as.factor(my_data$status)
```


summary(my_data)

```{r}
# Create a training, validation, and test set
set.seed(1)
train <- sample(1:nrow(my_data), 0.7 * nrow(my_data))
train_data <- my_data[train, ]
train_data <- subset(train_data, select = -c(ID, status))
train_labels <- my_data[train, "status"]
test_data <- my_data[-train, ]
test_data <- subset(test_data, select = -c(ID, status))
test_labels <- my_data[-train, "status"]
```


```{r}
mean <- apply(train_data[,c("CNT_CHILDREN", "AMT_INCOME_TOTAL", "DAYS_BIRTH", "DAYS_EMPLOYED", "CNT_FAM_MEMBERS")], 2, mean)
std <- apply(train_data[,c("CNT_CHILDREN", "AMT_INCOME_TOTAL", "DAYS_BIRTH", "DAYS_EMPLOYED", "CNT_FAM_MEMBERS")], 2, sd)
train_data[,c("CNT_CHILDREN", "AMT_INCOME_TOTAL", "DAYS_BIRTH", "DAYS_EMPLOYED", "CNT_FAM_MEMBERS")] <- scale(train_data[,c("CNT_CHILDREN", "AMT_INCOME_TOTAL", "DAYS_BIRTH", "DAYS_EMPLOYED", "CNT_FAM_MEMBERS")], center = mean, scale = std)
test_data[,c("CNT_CHILDREN", "AMT_INCOME_TOTAL", "DAYS_BIRTH", "DAYS_EMPLOYED", "CNT_FAM_MEMBERS")] <- scale(test_data[,c("CNT_CHILDREN", "AMT_INCOME_TOTAL", "DAYS_BIRTH", "DAYS_EMPLOYED", "CNT_FAM_MEMBERS")], center = mean, scale = std)
```
# This leads to some very high maximums because the average is so low. 
# Doesn't really work, and factors are probably better.
summary(train_data)

```{r}
train_data <- as.matrix(train_data)
train_labels <- to_categorical(matrix(as.numeric(train_labels)-1))
test_data <- as.matrix(test_data)
test_labels <- to_categorical(matrix(as.numeric(test_labels)-1))
```
# This concverts the data to a matrix, which is what keras wants.

create validation set
```{r}
set.seed(1)
validate <- sample(1:nrow(train_data), 0.1 * nrow(train_data))
val_data <- train_data[validate, ]
val_labels <- train_labels[validate, ]
train_data <- train_data[-validate, ]
train_labels <- train_labels[-validate, ]
```



# first network
```{r}
model <- keras_model_sequential() %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 8, activation = "softmax")
```

```{r}
model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
```



Need validation set

```{r echo=TRUE, results='hide'}
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(val_data, val_labels)
)
```

#kerastuneR

## create a df of hyperparameters
```{r}
hp = HyperParameters()
#hp$Choice('learning_rate', c(1e-1, 1e-3))
hp$Int('num_layers', 2L, 20L)
hp$Int('units', 32, 512)
```

## build a function to build models
```{r ECHO = TRUE}
build_model = function(hp) {
  
  model = keras_model_sequential()
  model %>% layer_dense(units=352,
                        input_shape = ncol(train_data),
                        activation='relu') %>% 
    layer_dense(units=hp$Int('units',
                             min_value=32,
                             max_value=512,
                             step=32),
                activation='relu') %>% 
    layer_dense(units=8, activation='softmax') %>% 
    compile(
      optimizer= tf$keras$optimizers$Adam( # use "rmsprop" instead?
        hp$Choice('learning_rate',
                  values=c(1e-2, 1e-3, 1e-4))),
      loss='categorical_crossentropy',
      metrics='accuracy') 
  return(model)
}
```

## try again to build models with a function
```{r}
make_model = function(hp) {
  
  inputs = layer_input(ncol(train_data))
  model = keras_model_sequential()
  model %>% layer_dense(units=hp$Int('units',
                                     min_value=32,
                                     max_value=512,
                                     step=32),
                        input_shape = ncol(train_data),
                        activation='relu') 
  for (i in 1:(hp$Int('num_layers', 1, 10))) {
    model %>% layer_dense(hp$Int(paste('units_', toString(i)), min_value=32, max_value=512, step=32), activation='relu')  
    model %>% layer_dropout(
      hp$Float('dense_dropout', min_value = 0., max_value = 0.7))
  } %>% 
    layer_dense(units = 8, activation='softmax') %>%
    compile(
      optimizer = tf$keras$optimizers$Adam(
        hp$Choice('learning_rate',
                  values=c(1e-2, 1e-3, 1e-4))),
      loss = 'binary_crossentropy',
      metrics = 'accuracy')
  model %>% summary()
  return(model)
}
```

## build 1-D convolutional models with a function
```{r}
make_model = function(hp) {
  
  inputs = layer_input(ncol(train_data))
  model = keras_model_sequential()
  model %>% layer_dense(256,
                        input_shape = ncol(train_data),
                        activation='relu')
  model %>% layer_reshape(list(256, 1))
  for (i in 1:(hp$Int('num_layers', 1, 10))) {
    model %>% layer_conv_1d(hp$Int(paste('conv_', toString(i), '_units'), min_value=32, max_value=512, step=32), kernel_size = hp$Int(paste('conv_', toString(i), '_size'), min_value=2, max_value=16, step = 1), activation='relu')
    #    model %>% layer_dropout(hp$Float('conv_', toString(i), '_dropout', min_value = 0., max_value = 0.7))
    model %>% layer_max_pooling_1d(pool_size = 2)
  } %>% 
    layer_dense(units = 8, activation='softmax') %>%
    compile(
      optimizer = tf$keras$optimizers$Adam(
        hp$Choice('learning_rate',
                  values=c(1e-2, 1e-3, 1e-4))),
      loss = 'binary_crossentropy',
      metrics = 'accuracy')
  model %>% summary()
  return(model)
}
```

## instantiate a tuner


```{r ECHO = TRUE}
tuner = RandomSearch(
  hypermodel = make_model,
  objective = 'val_accuracy',
  max_trials = 5,
  executions_per_trial = 3,
  directory = 'my_dir',
  project_name = 'test',
  overwrite = TRUE)
```

## print a summary of the search space
```{r ECHO = TRUE}
tuner %>% search_summary()
```

## early quit
```{r}
tensorboard("my_log_dir")
callbacks = list(
  callback_tensorboard(log_dir = "my_log_dir",
                       histogram_freq = 1,
                       embeddings_freq = 1),
  callback_early_stopping(monitor = 'val_accuracy', mode = 'max', 
                          patience = 3)
)
```

## start the search
```{r ECHO = TRUE}
tuner %>% fit_tuner(train_data,train_labels,
                    epochs=100, 
                    validation_data = list(val_data,val_labels),
                    callbacks = list(callbacks), 
                    verbose=1)
```
Search: Running Trial #2

Hyperparameter    |Value             |Best Value So Far 
units             |352               |?                 
  learning_rate     |0.001             |?     
  
  ## Plot results
  ```{r}
result = kerastuneR::plot_tuner(tuner)
result
```

## plot keras model

```{r}
best_5_models = tuner %>% get_best_models(5)
best_5_models[[1]] %>% plot_keras_model()
```

# autokeras

```{r echo=TRUE, results='hide'}
cf <- model_structured_data_classifier(max_trials = 10) %>%
  fit(train_data, train_labels, validation_data = list(val_data, val_labels))
```



```{r}
cf %>% evaluate(test_data, test_labels)
```
```{r}
cf %>% predict(test_data)
```
```{r}
auto_model1 <- export_model(cf)
summary(auto_model1)
plot(auto_model1)
```

## take two
```{r echo=TRUE, results='hide'}
clf <- model_image_classifier(max_trials = 10) %>%
  fit(train_data, train_labels, validation_data = list(val_data, val_labels))
```
Will not work because it's not the correct number of dimensions. I don't see how to force 1D

# And use it to evaluate, predict
```{r}
clf %>% evaluate(test_data, test_labels)
```

```{r}
clf %>% predict(test_data)
```

```{r}
auto_model1 <- export_model(cf)
summary(auto_model1)
plot(auto_model1)
```


# build that model 


```{r}
auto_model_man <- keras_model_sequential() %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dense(units = 8, activation = "softmax")
auto_model_man %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy"))

history <- auto_model_man %>% fit(
  train_data,
  train_labels,
  epochs = 1500,
  batch_size = 512,
  validation_data = list(val_data, val_labels))
```

# what if we built a conv model anyway?

```{r}
model <- keras_model_sequential() %>% 
  layer_dense(units = 1024, activation = "relu") %>% # first is a regular layer
  # reshape to 1D
  layer_reshape(list(1024, 1)) %>% # do I need this?
  layer_conv_1d(filters = 64, kernel_size = 16, activation = "relu", input_shape = c(1024, 1)) %>% 
  layer_max_pooling_1d(pool_size = 8) %>% 
  # continue convoluting and pooling
  layer_conv_1d(filters = 128, kernel_size = 8, activation = "relu") %>% 
  layer_max_pooling_1d(pool_size = 4) %>%
  # flatten
  layer_flatten() %>% 
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dense(units = 8, activation = "softmax") %>% 
  compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy"))
```

## build it smaller
```{r}
model <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "relu") %>% # first is a regular layer
  # reshape to 1D
  layer_reshape(list(256, 1)) %>% 
  layer_conv_1d(filters = 64, kernel_size = 4, activation = "relu", input_shape = c(256, 1)) %>% 
  layer_max_pooling_1d(pool_size = 2) %>% 
  # continue convoluting and pooling
  layer_conv_1d(filters = 128, kernel_size = 2, activation = "relu") %>% 
  layer_max_pooling_1d(pool_size = 2) %>%
  # flatten
  layer_flatten() %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dense(units = 8, activation = "softmax") %>% 
  compile(
    optimizer = "rmsprop",
    loss = "categorical_crossentropy",
    metrics = c("accuracy"))
```

```{r}
model
```

set logging and callbacks
```{r echo=TRUE, results='hide'}
tensorboard("my_log_dir")
callbacks = list(
  callback_tensorboard(log_dir = "my_log_dir",
                       histogram_freq = 1,
                       embeddings_freq = 1),
  callback_early_stopping(monitor = 'val_accuracy', mode = 'max', 
                          patience = 3))
```

## adjust weights
note this is new

"0" "1" "2" "3" "4" "5" "C" "X"
status     n
1      0 52133
2      1  6491
3      2   712
4      3   195
5      4   114
6      5   374
7      C  1805
8      X  5790

## weights attempted on the small model above

This is not repeatable. I think I did not clear the model each time and so messed up the data
```{r}
# weights <- list("0" = 1, "1" = 2, "2" = 5, "3" = 5, "4" = 5, "5" = 5, "6" = 2, "7" = 2) # 0.7627 at epoch 1, then downhill
# weights <- list("0" = 1, "1" = 2, "2" = 10, "3" = 10, "4" = 10, "5" = 10, "6" = 2, "7" = 2) # 0.7625 in epoch 1, then downhill
# weights <- list("0" = 1, "1" = 5, "2" = 10, "3" = 10, "4" = 10, "5" = 10, "6" = 5, "7" = 2) # never breaks 0.80, exits after 5 epochs
# weights <- list("0" = 1, "1" = 2, "2" = 20, "3" = 20, "4" = 20, "5" = 20, "6" = 2, "7" = 2) # 0.8109 on small model
weights <- list("0" = 1, "1" = 2, "2" = 3, "3" = 3, "4" = 3, "5" = 3, "6" = 2, "7" = 2) # 0.8109 on small model
```

## train model

Small model with no weights. val_accuracy of 0.7988 after 20 epochs
0.7923 after 4 more (calls back)
0.8058 after 4 more (calls back after 7)
0.7817 after 4 more (calls back)

Large model with no weights. val_accuracy of 0.7800 after 9 epochs, callback after 12
0.8001 after 1 more epoch, callback after 4 (16 epochs)
0.8071 after 4, callback after 7 (23 epochs)
0.8043 after 6, callback after 9 (32 epochs)
0.8104 after 3, callback after 6 (38 epochs)
0.8107 after 4, callback after 7
```{r echo=TRUE, results='hide'}
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(val_data, val_labels),
  callbacks = list(callbacks),
  #class_weight = weights, # this is new
  verbose = 1
)
```


## second attempt to build it smaller
I tried max and average, and also with the third convolution and without, but I did not improve the model over the baseline of 0.7593 on the validation set

changed from relu to tanh because relu can't deal with negative values
```{r}
model <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "tanh") %>% # first is a regular layer
# reshape to 1D
  layer_reshape(list(256, 1)) %>% 
  layer_conv_1d(filters = 64, kernel_size = 2, activation = "tanh", input_shape = c(256, 1)) %>% 
  layer_max_pooling_1d(pool_size = 2) %>% 
# continue convoluting and pooling
#  layer_conv_1d(filters = 128, kernel_size = 4, activation = "relu") %>% 
#  layer_max_pooling_1d(pool_size = 2) %>%
# flatten
  layer_flatten() %>% 
  layer_dense(units = 128, activation = "tanh") %>% 
  layer_dense(units = 8, activation = "softmax") %>% 
  compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy"))
```

## third attempt to build it smaller
```{r}
model <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = "tanh") %>% # first is a regular layer
# reshape to 1D
  layer_reshape(list(256, 1)) %>% 
  layer_conv_1d(filters = 64, kernel_size = 2, activation = "tanh", input_shape = c(256, 1)) %>% 
  layer_max_pooling_1d(pool_size = 2) %>% 
# continue convoluting and pooling
  layer_conv_1d(filters = 64, kernel_size = 2, activation = "tanh") %>% 
  layer_max_pooling_1d(pool_size = 2) %>%
# flatten
  layer_flatten() %>% 
  layer_dense(units = 128, activation = "tanh") %>% 
  layer_dense(units = 8, activation = "softmax") %>% 
  compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy"))
```

```{r}
model
```

set logging and callbacks
```{r echo=TRUE, results='hide'}
tensorboard("my_log_dir")
callbacks = list(
  callback_tensorboard(log_dir = "my_log_dir",
                       histogram_freq = 1,
                       embeddings_freq = 1),
  callback_early_stopping(monitor = 'val_accuracy', mode = 'max', 
                                  patience = 3))
```


## weights attempted on the small model above

This is not repeatable. I think I did not clear the model each time and so messed up the data
```{r}
weights <- list("0" = 1, "1" = 2, "2" = 3, "3" = 3, "4" = 3, "5" = 3, "6" = 2, "7" = 2) # 0.8109 on small model
```

#train model

results with relu activation
0.7828 after 20 epochs
0.7931 after 7 epochs, callback after 10 (30 total)
0.7980 after 3 epochs, callback after 6 (36 total)
0.8064 after 5 epochs, callback after 8 (44 total)
0.8026 after 2 epochs, callback after 5 (49 total)
0.8011 after 4 epochs, callback after 7 (56 total)
0.8100 after 5 epochs, callback after 8 (64 total)
0.8092 after 1 epoch, callback after 4 (68 total)
0.8056 after 6 epochs, callback after 9 (77 total)
0.8100 after 4 epochs, callback after 7 (84 total)
0.8058 after 4 epochs, callback after 7 (91 total)
0.8028 after 1 epoch, callback after 4 (95 total)
0.8071 after 1 epoch, callback after 4 (99 total)
```{r echo=TRUE, results='hide'}
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(val_data, val_labels),
  callbacks = list(callbacks),
  class_weight = weights,
  verbose = 1
)
```
```{r}
model
```

Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
Data_Sience_R_Assignment_2/model_building.Rmd at main · Heulito/Data_Sience_R_Assignment_2
Data_Sience_R_Assignment_2/clean data.R at main · Heulito/Data_Sience_R_Assignment_2