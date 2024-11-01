---
  title: "MSDS 6306: Case Study Project 1"
author: "Megan Huy"
date: "10/31/2024"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#Case Study 1 Data 

```{r}
#Install Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret) 
library(e1071)  
install.packages("klaR")
library(klaR)
install.packages("readr")
library(readr)
library(class)

# Import Case Study Data
CaseStudy1_data <- read_csv("C:/Users/Megan Huy/Downloads/CaseStudy1-data.csv")
CaseStudy1_data


#Find Number of Missing Values
sum(is.na(CaseStudy1_data))
    
#Select Columns of Study
Case_Study_Clean <- CaseStudy1_data %>% dplyr::select(Attrition, Department, EnvironmentSatisfaction,
                                               JobSatisfaction, MonthlyIncome, OverTime,PerformanceRating,PercentSalaryHike, RelationshipSatisfaction, 
                                               TotalWorkingYears, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole)
Case_Study_Clean

#Find Number of Observations
nrow(Case_Study_Clean)
```
#Company Attrition

```{r}
#Attrition Status at Company
ggplot(Case_Study_Clean, aes(x = Attrition, fill = Attrition)) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Employee Attrition Status", x = "Attrition", y = "Count", fill = "Attrition Status") +
  theme_minimal()

#Attrition by Department
ggplot(Case_Study_Clean, aes(x = Attrition, fill = Attrition)) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Employee Attrition Status by Department",
    x = "Attrition",
    y = "Count",
    fill = "Attrition Status"
  ) +
  facet_wrap(~ Department) +
  theme_minimal() 

# Percentage Bar Graph for Attrition Within Each Department
ggplot(Case_Study_Clean, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill") +  
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Percent Proportion of Attrition by Department",
    x = "Department",
    y = "Percent Proportion",
    fill = "Attrition Status"
  ) +
  scale_y_continuous(labels = scales::percent) +  
  theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
#Company Ratings

```{r}
# Ratings: Environment Satisfaction for Attrition = Yes
environmental_yes <- Case_Study_Clean %>%
  filter(Attrition == "Yes") %>%
  dplyr::select(Department, EnvironmentSatisfaction) %>%
  mutate(RatingType = "Environment Satisfaction") %>%
  rename(Value = EnvironmentSatisfaction)

# Ratings: Relationship Satisfaction for Attrition = Yes
relationship_yes <- Case_Study_Clean %>%
  filter(Attrition == "Yes") %>%
  dplyr::select(Department, RelationshipSatisfaction) %>%
  mutate(RatingType = "Relationship Satisfaction") %>%
  rename(Value = RelationshipSatisfaction)

#Ratings: Performance Ratings for Attrition = Yes
performancerating_yes <- Case_Study_Clean %>%
  filter(Attrition == "Yes") %>%
  dplyr::select(Department, PerformanceRating) %>%
  mutate(RatingType = "Performance Rating") %>%
  rename(Value = PerformanceRating) 

# Combine Environment, Relationship, and Performance Ratings Attrition = Yes
employeeleft <- bind_rows(environmental_yes, relationship_yes, performancerating_yes)
ggplot(employeeleft, aes(x = Department, y = Value, fill = Department)) +
  geom_bar(stat = "identity", position = "dodge") +  
  facet_wrap(~ RatingType, scales = "free_y") +  
  labs(
    title = "Rating Scores by Department and Rating Type (Attrition = Yes)",
    x = "Department",
    y = "Rating Score",
    fill = "Department"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Ratings: Environment Satisfaction for Attrition = No
environmental_no <- Case_Study_Clean %>%
  filter(Attrition == "No") %>%
  dplyr::select(Department, EnvironmentSatisfaction) %>%
  mutate(RatingType = "Environment Satisfaction") %>%
  rename(Value = EnvironmentSatisfaction)

# Ratings: Relationship Satisfaction for Attrition = No
relationship_no <- Case_Study_Clean %>%
  filter(Attrition == "No") %>%
  dplyr::select(Department, RelationshipSatisfaction) %>%
  mutate(RatingType = "Relationship Satisfaction") %>%
  rename(Value = RelationshipSatisfaction)

#Ratings: Performance Ratings for Attrition = No
performancerating_no <- Case_Study_Clean %>%
  filter(Attrition == "No") %>%
  dplyr::select(Department, PerformanceRating) %>%
  mutate(RatingType = "Performance Rating") %>%
  rename(Value = PerformanceRating) 

# Combine Environment, Relationship, and Performance Ratings for Attrition = No
employeestayed <- bind_rows(environmental_no, relationship_no, performancerating_no)
ggplot(employeestayed, aes(x = Department, y = Value, fill = Department)) +
  geom_bar(stat = "identity", position = "dodge") +  
  facet_wrap(~ RatingType, scales = "free_y") +  
  labs(
    title = "Rating Scores by Department and Rating Type (Attrition = No)",
    x = "Department",
    y = "Rating Score",
    fill = "Department"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
```{r}
#Work Life Balance for Attrition Yes
WLB <- Case_Study_Clean %>%
  filter(Attrition == "Yes") %>%
  dplyr::select(Department, WorkLifeBalance) %>%
  mutate(RatingType = "Work Life Balance Rating") %>%
  rename(Value = WorkLifeBalance)

#Work Life Balance for Attrition = Yes Graph
ggplot(WLB, aes(x = Department, y = Value, fill = Department)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Work Life Balance Rating by Department (Attrition = Yes)",
    x = "Department",
    y = "Work Life Balance Rating",
    fill = "Department"
  ) +
  scale_fill_brewer(palette = "Blues") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Work Life Balance for Attrition = No
WLB_No <- Case_Study_Clean %>%
  filter(Attrition == "No") %>%
  dplyr::select(Department, WorkLifeBalance) %>%
  mutate(RatingType = "Work Life Balance Rating") %>%
  rename(Value = WorkLifeBalance)

#Work Life Balance for Attrition = No Graph
ggplot(WLB_No, aes(x = Department, y = Value, fill = Department)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Work Life Balance Rating by Department (Attrition = No)",
    x = "Department",
    y = "Work Life Balance Rating",
    fill = "Department"
  ) +
  scale_fill_brewer(palette = "Blues") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))     
                
```
#Overtime Status by Department

```{r}
#Overtime Status for Attrition = Yes
rd_attrition_yes <- Case_Study_Clean %>%
  filter(Attrition == "Yes", YearsAtCompany < 10)

# Bar Plot Overtime Status Attrition = Yes
ggplot(rd_attrition_yes, aes(x = Department, fill = OverTime)) +
  geom_bar(position = "stack", width = 0.5) +
  labs(
    title = "Overtime Status for Employees with Attrition = Yes",
    x = "Department",
    y = "Count",
    fill = "Overtime"
  ) +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal() 

#Overtime Status for Attrition = No
rd_attrition_no <- Case_Study_Clean %>%
  filter(Attrition == "No", YearsAtCompany < 10)

# Bar Plot Overtime Status Attrition = No
ggplot(rd_attrition_no, aes(x = Department, fill = OverTime)) +
  geom_bar(position = "stack", width = 0.5) +
  labs(
    title = "Overtime Status for Employees with Attrition = No",
    x = "Department",
    y = "Count",
    fill = "Overtime"
  ) +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal() 

```
#Years at Company vs. Monthly Salary

```{r}
#Scatterplot: Years with the Company vs. Monthly Salary for Attrition = Yes
ggplot(attrition_yes, aes(x = YearsAtCompany, y = MonthlyIncome, color = Department)) +
  geom_jitter(alpha = 0.4, size = 2)  +  
  labs(
    title = "Years with the Company vs. Monthly Salary for Attrition = Yes",
    x = "Years with the Company",
    y = "Monthly Salary (in dollars)",
    color = "Department"
  ) +
  theme_minimal() 
 
#Scatterplot: Years with the Company vs. Monthly Salary for Attrition = No
ggplot(attrition_no, aes(x = YearsAtCompany, y = MonthlyIncome, color = Department)) +
  geom_jitter(alpha = 0.4, size = 2) +  
  labs(
    title = "Years with the Company vs. Monthly Salary for Attrition = No",
    x = "Years with the Company",
    y = "Monthly Salary (in dollars)",
    color = "Department"
  ) +
  theme_minimal() 

```  
#Percent Salary Hike

```{r}
#Filter < 10 Years at Company making < $5000
under10yes <- attrition_yes %>%
  filter(YearsAtCompany < 10, MonthlyIncome < 5000)

# Box Plot for Percent Salary Hike by Department for Attrition = Yes <10 Years
ggplot(under10yes, aes(x = Department, y = PercentSalaryHike, fill = Department)) +
  geom_boxplot() +
  labs(
    title = "Percent Salary Hike by Department (Employees with < 10 Years and < $5,000 Monthly Income)",
    x = "Department",
    y = "Percent Salary Hike"
  ) +
  theme_minimal()

#Box Plot for Percent Salary Hike by Deparment for Attrition = No <10 Years
under10no <- attrition_no %>%
  filter(YearsAtCompany < 10, MonthlyIncome < 5000)

# Create a box plot for Percent Salary Hike by Department
ggplot(under10no, aes(x = Department, y = PercentSalaryHike, fill = Department)) +
  geom_boxplot() +
  labs(
    title = "Percent Salary Hike by Department (Employees with < 10 Years and < $5,000 Monthly Income)",
    x = "Department",
    y = "Percent Salary Hike"
  ) +
  theme_minimal()

# Attrition Status for <10 Years
under10_combined <- bind_rows(
  data.frame(under10yes, AttritionStatus = "Yes"),
  data.frame(under10no, AttritionStatus = "No")
)

# Combine Box Plots for Percent Salary Hike by Department
ggplot(under10_combined, aes(x = Department, y = PercentSalaryHike, fill = Department)) +
  geom_boxplot() +
  facet_wrap(~ AttritionStatus) +  
  labs(
    title = "Percent Salary Hike (Employees with < 10 Years and < $5,000 Monthly Income)",
    x = "Department",
    y = "Percent Salary Hike"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```
#Monthly Income by Attrition Status

```{r}
# Filter Sales Group with < 10 years
sales_attrition_no <- Case_Study_Clean %>%
  filter(Attrition == "Yes", Department == "Sales", YearsAtCompany < 10)
sales_attrition_yes <- Case_Study_Clean %>%
  filter(Attrition == "No", Department == "Sales", YearsAtCompany < 10)

# Combine Data Sets for Sales
sales_attrition_combined <- bind_rows(
  data.frame(sales_attrition_yes, AttritionStatus = "Yes"),
  data.frame(sales_attrition_no, AttritionStatus = "No")
)

# Scatter Plot for Years at Company vs Years in Current Role
ggplot(sales_attrition_combined, aes(x = YearsAtCompany, y = YearsInCurrentRole, color = AttritionStatus)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid")+
  scale_color_manual(values = c(Yes= "lightblue", No = "darkblue"))+
  facet_wrap(~ AttritionStatus) + 
  labs(
    title = "Sales: Years at Company vs. Years in Current Role (<10 Years)",
    x = "Years at Company",
    y = "Years in Current Role",
    color = "Sales Attrition Status"
  ) +
  theme_minimal()

#HR Group With <10 Years for Attrition 
HR_yes <- Case_Study_Clean %>%
  filter(Attrition == "Yes", Department == "Human Resources", YearsAtCompany < 10)
HR_no <- Case_Study_Clean %>%
  filter(Attrition == "No", Department == "Human Resources", YearsAtCompany < 10)

# Combine Data Set for HR
HR_attrition_combined <- bind_rows(
  data.frame(HR_yes, AttritionStatus = "Yes"),
  data.frame(HR_no, AttritionStatus = "No")
)

# Scatter Plot for Years at Company vs Years in Current Role
ggplot(HR_attrition_combined, aes(x = YearsAtCompany, y = YearsInCurrentRole, color = AttritionStatus)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid")+
  facet_wrap(~ AttritionStatus) +  
  scale_color_manual(values = c(Yes= "lightblue", No = "darkblue"))+
  labs(
    title = "HR: Years at Company vs. Years In Current Role (< 10 Years)",
    x = "Years at Company",
    y = "Years in Current Role",
    color = "HR Attrition Status"
  ) +
  theme_minimal()

#Research and Development Group with < 10 years 
rd_attrition_yes <- Case_Study_Clean %>%
  filter(Attrition == "Yes", Department == "Research & Development", YearsAtCompany < 10)
rd_attrition_no <- Case_Study_Clean %>%
  filter(Attrition == "No", Department == "Research & Development", YearsAtCompany < 10)

# Combine Data Set for Research and Development
research_attrition_combined <- bind_rows(
  data.frame(rd_attrition_no, AttritionStatus = "No"),
  data.frame(rd_attrition_yes, AttritionStatus = "Yes")
)

#Scatter Plot to Compare Years at Company vs Years In Current Role
ggplot(research_attrition_combined, aes(x = YearsAtCompany, y = YearsInCurrentRole, color = AttritionStatus)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid")+
  scale_color_manual(values = c(Yes= "lightblue", No = "darkblue"))+
  facet_wrap(~ AttritionStatus) +  
  labs(
    title = "R & D: Years at Company vs. Current Role(< 10 Years)",
    x = "Years at Company",
    y = "Years in Current Role",
    color = "Research & Development Attrition Status"
  ) +
  theme_minimal()
```
#KNN and NB Models
```{r}

Case_Study_Clean <- CaseStudy1_data %>% dplyr::select(Attrition, Department, EnvironmentSatisfaction,
                                                      JobSatisfaction, MonthlyIncome, OverTime,PerformanceRating,PercentSalaryHike, RelationshipSatisfaction, 
                                                      TotalWorkingYears, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole)
Case_Study_Clean
nrow(Case_Study_Clean)

#KNN: Set Parameters
set.seed(1)
k_value <- 21    
splitPerc <- 0.7
threshold <- 0.5 

# Convert Over Time to Numeric Value (0 = "No" and 1 = "Yes")
Case_Study_Clean$OverTime <- ifelse(Case_Study_Clean$OverTime == "Yes", 1, 0)

# Factor: Attrition with "Yes" As the Positive Class
Case_Study_Clean$Attrition <- factor(Case_Study_Clean$Attrition, levels = c("No", "Yes"))

# 70/30 Train-Test Split
trainIndex <- createDataPartition(Case_Study_Clean$Attrition, p = splitPerc, list = FALSE)
train_data <- Case_Study_Clean[trainIndex, ]
test_data <- Case_Study_Clean[-trainIndex, ]

# Apply Undersampling to the Training Set
train_data_balanced <- downSample(x = train_data[, c("MonthlyIncome", "YearsAtCompany", "OverTime", "PercentSalaryHike", "YearsInCurrentRole")],
                                  y = train_data$Attrition)
names(train_data_balanced)[names(train_data_balanced) == "Class"] <- "Attrition"

# K-NN Model Train on Balanced Training Set
knn_model <- train(
  Attrition ~ MonthlyIncome + YearsAtCompany + OverTime + PercentSalaryHike + YearsInCurrentRole,
  data = train_data_balanced,
  method = "knn",
  tuneGrid = data.frame(k = k_value),
  trControl = trainControl(classProbs = TRUE)
)

# Make Predictions with the Adjusted Threshold
knn_probabilities <- predict(knn_model, test_data, type = "prob")[, "Yes"]
knn_predictions_adjusted <- ifelse(knn_probabilities >= threshold, "Yes", "No")

# Confusion Matrix 
CM <- confusion_matrix <- confusionMatrix(factor(knn_predictions_adjusted, levels = c("No", "Yes")), test_data$Attrition, positive = "Yes")
print(CM)

# Store Metrics For Accuracy, Precision, Recall, and F1
masterAcc[j] <- CM$overall["Accuracy"]
masterPrecision[j] <- CM$byClass["Pos Pred Value"]  
masterRecall[j] <- CM$byClass["Sensitivity"]       
masterF1[j] <- 2 * (masterPrecision[j] * masterRecall[j]) / (masterPrecision[j] + masterRecall[j])  

#Calculate Mean Metrics 
MeanAcc <- mean(masterAcc, na.rm = TRUE)
MeanPrecision <- mean(masterPrecision, na.rm = TRUE)
MeanRecall <- mean(masterRecall, na.rm = TRUE)
MeanF1 <- mean(masterF1, na.rm = TRUE)
MeanAcc
MeanPrecision
MeanRecall
MeanF1

#NB Model
# Set Parameters
set.seed(1)
iterations <- 100             
splitPerc <- 0.7              
threshold <- 0.50       
masterAcc <- numeric(iterations)        
masterPrecision <- numeric(iterations)  
masterRecall <- numeric(iterations)     
masterF1 <- numeric(iterations)         

# Naive Bayes Loop with Adjustable Threshold and Under Sampling
for(j in 1:iterations) {
  
  # Train/Test Split
  trainIndices <- sample(1:nrow(Case_Study_Clean), round(splitPerc * nrow(Case_Study_Clean)))
  train <- Case_Study_Clean[trainIndices, ]
  test <- Case_Study_Clean[-trainIndices, ]
  
  # Apply Under Sampling to Training Set
  train_balanced <- downSample(x = train[, c("MonthlyIncome", "YearsAtCompany", "OverTime", "PercentSalaryHike", "YearsInCurrentRole")],
                               y = train$Attrition)
  
  train_balanced$Attrition <- train_balanced$Class  
  train_balanced$Class <- NULL  
  
  # Train the Naive Bayes Model on Balanced Training Set
  nb_model <- naiveBayes(Attrition ~ MonthlyIncome + YearsAtCompany + OverTime + PercentSalaryHike + YearsInCurrentRole,
                      data = train_balanced, laplace = 1)
  
  # Predict for Positive Class
  prob_predictions <- predict(nb_model, test, type = "raw")[, "Yes"]
  class_predictions <- ifelse(prob_predictions >= threshold, "Yes", "No")
  
  # Generate Confusion Matrix
  CM <- confusionMatrix(factor(class_predictions, levels = c("No", "Yes")), test$Attrition, positive = "Yes")
  print(CM)
  
  # Store Metrics for Accuracy, Precision, Recall, and F1
  masterAcc[j] <- CM$overall["Accuracy"]
  masterPrecision[j] <- CM$byClass["Pos Pred Value"] 
  masterRecall[j] <- CM$byClass["Sensitivity"]        
  masterF1[j] <- 2 * (masterPrecision[j] * masterRecall[j]) / (masterPrecision[j] + masterRecall[j])  # F1 calculation


# Calculate Mean Metrics Across All Iterations
MeanAcc <- mean(masterAcc, na.rm = TRUE)
MeanPrecision <- mean(masterPrecision, na.rm = TRUE)
MeanRecall <- mean(masterRecall, na.rm = TRUE)
MeanF1 <- mean(masterF1, na.rm = TRUE)
MeanAcc
MeanPrecision
MeanRecall
MeanF1
```
#Competition Set
```{r}

# Load the Competition Data Set
competition_set <- read.csv("CaseStudy1CompSet No Attrition.csv")

competition_set$OverTime <- factor(competition_set$OverTime, levels = levels(train_balanced$OverTime))

# Predictions
prob_predictions_competition <- predict(nb_model, competition_set, type = "raw")[, "Yes"]
class_predictions_competition <- ifelse(prob_predictions_competition >= threshold, "Yes", "No")

# Add Prediction to Competition Data Frame
competition_set$Attrition_Prediction <- class_predictions_competition

# Sort by ID 
competition_set <- competition_set[order(competition_set$ID), ]
competition_set

# Save predictions to CSV 
write.csv(competition_set[, c("ID", "Attrition_Prediction")], 
          "Case1PredictionsHuy Attrition.csv", 
          row.names = FALSE)

predictions <- read.csv("Case1PredictionsHuy Attrition.csv")
view(predictions)
```




