---
title: "FritoLay Attrition Project"
author: "Megan Huy"
date: "2025-09-16"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
#Library Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret) 
library(e1071)  
library(klaR)
library(readr)
library(class)
```
```{r}
# Import Case Study Data
CaseStudy1_data = read_csv("C:/Users/Megan Huy/Downloads/CaseStudy1-data.csv") #Replace with your CSV file here
print(CaseStudy1_data)
```
There are 36 columns and 870 rows.
```{r}
#Find Number of Missing Values
sum(is.na(CaseStudy1_data))
```
There are no missing values present.
```{r}
#Select Columns of Interest
Case_Study_Clean = CaseStudy1_data %>% dplyr::select(Attrition, Department, EnvironmentSatisfaction,
                                               JobSatisfaction, MonthlyIncome,OverTime,PerformanceRating,PercentSalaryHike, RelationshipSatisfaction, 
                                               TotalWorkingYears, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole)
print(Case_Study_Clean)
```
The target feature is attrition. Other features include overtime, performance rating,percent salary hike, relationship satisfaction, total working years, work-life balance, years at company, and years in current role.

```{r}
#EDA Bar Plot: Attrition Status Count at Company
ggplot(Case_Study_Clean, aes(x = Attrition, fill = Attrition)) +
  geom_bar(position = "stack", color = "black") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),  
    vjust = -0.5,
    color = "black") +      
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Employee Attrition Status", x = "Attrition", y = "Count", fill = "Attrition Status") +
  theme_minimal()
```

Out of the 870 employees, 730 did not leave the company and 140 left the company.

```{r}
 # Summarize Data w/ Counts and Percentages
plot_data = Case_Study_Clean %>%
  count(Department, Attrition) %>%
  group_by(Department) %>%
  mutate(percent = n / sum(n) * 100)
```

```{r}
# Percent Bar Plot by Department
ggplot(plot_data, aes(x = Department, y = percent, fill = Attrition)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(
    aes(label = paste0(round(percent), "%")),
    position = position_stack(vjust = 0.5),
    color = "black", size = 3
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Distribution of Attrition by Department",
    x = "Department", y = "Percentage",
    fill = "Attrition Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
```
The greatest attrition is in the sale department (22%), followed by human resources (17%), and research & development (13%).

```{r}
# Ratings: Environment Satisfaction for Attrition = Yes
environmental_yes = Case_Study_Clean %>%
  filter(Attrition == "Yes") %>%
  dplyr::select(Department, EnvironmentSatisfaction) %>%
  mutate(RatingType = "Environment Satisfaction") %>%
  rename(Value = EnvironmentSatisfaction)
```
```{r}
# Ratings: Relationship Satisfaction for Attrition = Yes
relationship_yes = Case_Study_Clean %>%
  filter(Attrition == "Yes") %>%
  dplyr::select(Department, RelationshipSatisfaction) %>%
  mutate(RatingType = "Relationship Satisfaction") %>%
  rename(Value = RelationshipSatisfaction)
```
```{r}
#Ratings: Performance Ratings for Attrition = Yes
performancerating_yes = Case_Study_Clean %>%
  filter(Attrition == "Yes") %>%
  dplyr::select(Department, PerformanceRating) %>%
  mutate(RatingType = "Performance Rating") %>%
  rename(Value = PerformanceRating) 
```
```{r}
# Combine Environment, Relationship, and Performance Ratings Attrition = Yes
employeeleft = bind_rows(environmental_yes, relationship_yes, performancerating_yes)
ggplot(employeeleft, aes(x = Department, y = Value, fill = Department)) +
  geom_col(position = "dodge") +  
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
```
The rating scores were high across all departments, so this does not visually appear to be predictor for attrition.

```{r}
#Ratings: Environment Satisfaction for Attrition = No
environmental_no = Case_Study_Clean %>%
  filter(Attrition == "No") %>%
  dplyr::select(Department, EnvironmentSatisfaction) %>%
  mutate(RatingType = "Environment Satisfaction") %>%
  rename(Value = EnvironmentSatisfaction)
```
```{r}
# Ratings: Relationship Satisfaction for Attrition = No
relationship_no = Case_Study_Clean %>%
  filter(Attrition == "No") %>%
  dplyr::select(Department, RelationshipSatisfaction) %>%
  mutate(RatingType = "Relationship Satisfaction") %>%
  rename(Value = RelationshipSatisfaction)
```
```{r}
#Ratings: Performance Ratings for Attrition = No
performancerating_no = Case_Study_Clean %>%
  filter(Attrition == "No") %>%
  dplyr::select(Department, PerformanceRating) %>%
  mutate(RatingType = "Performance Rating") %>%
  rename(Value = PerformanceRating) 
```
```{r}
# Combine Environment, Relationship, and Performance Ratings for Attrition = No
employeestayed = bind_rows(environmental_no, relationship_no, performancerating_no)
ggplot(employeestayed, aes(x = Department, y = Value, fill = Department)) +
  geom_col(position = "dodge") +  
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
The rating scores were high across all departments, so this does not visually appear to be predictor for attrition.

```{r}
#Work Life Balance for Attrition Yes
WLB_yes = Case_Study_Clean %>%
  filter(Attrition == "Yes") %>%
  dplyr::select(Department, WorkLifeBalance) %>%
  mutate(RatingType = "Work Life Balance Rating") %>%
  rename(Value = WorkLifeBalance)
```
```{r}
#Work Life Balance for Attrition = Yes Graph
ggplot(WLB_yes, aes(x = Department, y = Value, fill = Department)) +
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
```
```{r}
#Work Life Balance for Attrition = No
WLB_No = Case_Study_Clean %>%
  filter(Attrition == "No") %>%
  dplyr::select(Department, WorkLifeBalance) %>%
  mutate(RatingType = "Work Life Balance Rating") %>%
  rename(Value = WorkLifeBalance)
```
```{r}
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
Work-life balance appears high for those that left too. Work life balance doesn't appear to be a clear predictor for attrition.

```{r}
# Filter Employees Serving for Less Than 10 Years
rd_attrition_yes = Case_Study_Clean %>%
  filter(Attrition == "Yes", YearsAtCompany < 10)
```
```{r}
# Summarize Counts and Percentage
plot_data = rd_attrition_yes %>%
  count(Department, OverTime) %>%
  group_by(Department) %>%
  mutate(percent = n / sum(n) * 100)
```
```{r}
# Percent Bar Plot
ggplot(plot_data, aes(x = Department, y = percent, fill = OverTime)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(
    aes(label = paste0(round(percent), "%")),
    position = position_stack(vjust = 0.5),
    color = "black", size = 3
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Overtime Status for Employees with Attrition = Yes (< 10 Years at Company)",
    x = "Department",
    y = "Percentage",
    fill = "Overtime"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
```
After narrowing it down to employees that worked for the company within 10 years, majority of employees that left worked overtime in the research & development and sales department.
```{r}
# Filter Data to Employees That Stayed
rd_attrition_no = Case_Study_Clean %>%
  filter(Attrition == "No", YearsAtCompany < 10)
```
```{r}
# Summarize counts + percentages
plot_data_no <- rd_attrition_no %>%
  count(Department, OverTime) %>%
  group_by(Department) %>%
  mutate(percent = n / sum(n) * 100)
```
```{r}
# Percent bar plot
ggplot(plot_data_no, aes(x = Department, y = percent, fill = OverTime)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(
    aes(label = paste0(round(percent), "%")),
    position = position_stack(vjust = 0.5),
    color = "black", size = 3
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Overtime Status for Employees with Attrition = No (< 10 Years at Company)",
    x = "Department",
    y = "Perc",
     fill = "Overtime"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
```
After narrowing down to employees that stayed with the company within 10 years, majority of those that stayed reported they did not work overtime. 

```{r}
# Create Dataset for Attrition = Yes
attrition_yes = Case_Study_Clean %>%
  filter(Attrition == "Yes")
```
```{r}
#Scatterplot: Years with the Company vs. Monthly Salary for Attrition = Yes
ggplot(attrition_yes, aes(x = YearsAtCompany, y = MonthlyIncome, color = Department)) +
  geom_jitter(alpha = 0.4, size = 2)  +  
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Years with the Company vs. Monthly Salary for Attrition = Yes",
    x = "Years with the Company",
    y = "Monthly Salary (in dollars)",
    color = "Department"
  ) +
  theme_minimal() 
```
Most employees that left made less than $5,000 a month. Human resource was the only department with a negative trend between years with company and monthly income. This could be a factor as to why, those in human resources left. 
```{r}
# Create Dataset for Attrition = No
attrition_no <- Case_Study_Clean %>%
  filter(Attrition == "No")
```
```{r}
#Scatterplot for Attrition = No
ggplot(attrition_no, aes(x = YearsAtCompany, y = MonthlyIncome, color = Department)) +
  geom_jitter(alpha = 0.4, size = 2) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Years with the Company vs. Monthly Salary for Attrition = No",
    x = "Years with the Company",
    y = "Monthly Salary (in dollars)",
    color = "Department"
  ) +
  theme_minimal() 
```  
While there are clusters under $5,000/month, employees in research & development and sales were paid much more than the people within the same department that left. Overall, there is positive trend for all departments for those that stayed. Scatter plot suggest that employees that stay longer see salary increase.

```{r}
##Filter < 10 Years at Company making < $5000
under10yes <- attrition_yes %>%
  filter(YearsAtCompany < 10, MonthlyIncome < 5000)
under10no = attrition_no %>%
  filter(YearsAtCompany < 10, MonthlyIncome < 5000)
```

```{r}
# Attrition Status for <10 Years
under10_combined = bind_rows(
  data.frame(under10yes, AttritionStatus = "Yes"),
  data.frame(under10no, AttritionStatus = "No")
)
```
```{r}
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
Employees that left Human Resources and Sales had lower percent salary hikes.

```{r}
# Filter Sales Group with < 10 years
sales_attrition_no = Case_Study_Clean %>%
  filter(Attrition == "Yes", Department == "Sales", YearsAtCompany < 10)
sales_attrition_yes = Case_Study_Clean %>%
  filter(Attrition == "No", Department == "Sales", YearsAtCompany < 10)
```

```{r}
# Combine Data Sets for Sales
sales_attrition_combined = bind_rows(
  data.frame(sales_attrition_yes, AttritionStatus = "Yes"),
  data.frame(sales_attrition_no, AttritionStatus = "No")
)
```
```{r}
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
```
```{r}
#HR Group With <10 Years for Attrition 
HR_yes = Case_Study_Clean %>%
  filter(Attrition == "Yes", Department == "Human Resources", YearsAtCompany < 10)
HR_no = Case_Study_Clean %>%
  filter(Attrition == "No", Department == "Human Resources", YearsAtCompany < 10)
```
```{r}
# Combine Data Set for HR
HR_attrition_combined = bind_rows(
  data.frame(HR_yes, AttritionStatus = "Yes"),
  data.frame(HR_no, AttritionStatus = "No")
)
```
```{r}
# Scatter plot for Years at Company vs Years in Current Role
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
```
```{r}
#Research and Development Group with < 10 years 
rd_attrition_yes = Case_Study_Clean %>%
  filter(Attrition == "Yes", Department == "Research & Development", YearsAtCompany < 10)
rd_attrition_no = Case_Study_Clean %>%
  filter(Attrition == "No", Department == "Research & Development", YearsAtCompany < 10)
```
```{r}
# Combine Data Set for Research and Development
research_attrition_combined = bind_rows(
  data.frame(rd_attrition_no, AttritionStatus = "No"),
  data.frame(rd_attrition_yes, AttritionStatus = "Yes")
)
```
```{r}
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
Employees tend to spend similar duration in their current role as they have overall with the company, indicating minimal role changes, despite attrition status.

```{r}
#Set Up for Prediction Models
Case_Study_Clean = CaseStudy1_data %>% dplyr::select(Attrition, Department, EnvironmentSatisfaction,
                                                      JobSatisfaction, MonthlyIncome, OverTime,PerformanceRating,PercentSalaryHike, RelationshipSatisfaction, 
                                                      TotalWorkingYears, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole)

print(Case_Study_Clean)
```
```{r}
#Parameters
set.seed(1)
iterations = 50
splitPerc = 0.7
threshold = 0.5
k_value = 21
```
```{r}
#Ensure Factor Level
Case_Study_Clean = Case_Study_Clean %>%
  mutate(
    Attrition = factor(Attrition, levels = c("No", "Yes")),
    OverTime  = factor(OverTime, levels = c("No", "Yes"))
  )
```
```{r}
#Set up Metrics
masterAcc = numeric(iterations)
masterPrecision = numeric(iterations)
masterRecall = numeric(iterations)
masterF1 = numeric(iterations)
```
```{r}
#Loop
for (j in 1:iterations) {
  
# Train/Test Split 70-30
trainIndex = createDataPartition(Case_Study_Clean$Attrition, p = splitPerc, list = FALSE)
train_data = Case_Study_Clean[trainIndex, ]
test_data  = Case_Study_Clean[-trainIndex, ]

#Balance Training Data
x_cols = c("MonthlyIncome", "YearsAtCompany", "OverTime", "PercentSalaryHike", "YearsInCurrentRole")
train_data_balanced = downSample(x = train_data[, x_cols], y = train_data$Attrition)
names(train_data_balanced)[names(train_data_balanced) == "Class"] = "Attrition"
train_data_balanced$Attrition = factor(train_data_balanced$Attrition, levels = c("No", "Yes"))

# Convert OverTime to Numeric (0/1)
to_numeric = function(df) {
    df %>%
      mutate(OverTime = ifelse(OverTime == "Yes", 1, 0)) %>%
      transmute(
        Attrition,
        MonthlyIncome, YearsAtCompany, OverTime, PercentSalaryHike, YearsInCurrentRole
      )
  }
  
  train_knn = to_numeric(train_data_balanced)
  test_knn  = to_numeric(test_data)

# Standardize Predictors
pre = preProcess(train_knn[, -1], method = c("center", "scale"))
train_x = predict(pre, train_knn[, -1])
test_x  = predict(pre, test_knn[, -1])
  
train_y = train_knn$Attrition
test_y  = test_knn$Attrition

#Fit KNN With Probabilities
knn_model = knn3(train_x, train_y, k = k_value)
prob_predictions = predict(knn_model, test_x, type = "prob")[, "Yes"]
class_predictions = ifelse(prob_predictions >= threshold, "Yes", "No")

#Confusion Matrix
CM = confusionMatrix(
data = factor(class_predictions, levels = c("No", "Yes")),
reference = test_y,
    positive = "Yes"
  )
  
# Store Metrics
masterAcc[j] = CM$overall["Accuracy"]
masterPrecision[j] = CM$byClass["Pos Pred Value"]
masterRecall[j] = CM$byClass["Sensitivity"]
  
if (!is.na(masterPrecision[j]) && !is.na(masterRecall[j]) &&
      (masterPrecision[j] + masterRecall[j]) > 0) {
    masterF1[j] = 2 * (masterPrecision[j] * masterRecall[j]) /
                  (masterPrecision[j] + masterRecall[j])
  } else {
    masterF1[j] = NA_real_
  }
}
```
```{r}
#Mean Metrics
MeanAcc = mean(masterAcc, na.rm = TRUE)
MeanPrecision = mean(masterPrecision, na.rm = TRUE)
MeanRecall = mean(masterRecall, na.rm = TRUE)
MeanF1 = mean(masterF1, na.rm = TRUE)

MeanAcc; MeanPrecision; MeanRecall; MeanF1
```
```{r}
#NB Model
# Set Parameters
set.seed(1)
iterations = 100             
splitPerc = 0.7              
threshold = 0.50       
masterAcc = numeric(iterations)        
masterPrecision = numeric(iterations)  
masterRecall = numeric(iterations)     
masterF1 = numeric(iterations)         
```
```{r}
# Naive Bayes Loop with Adjustable Threshold and Under Sampling
for(j in 1:iterations) {
  
# Train/Test Split 70-30
trainIndices = sample(1:nrow(Case_Study_Clean), round(splitPerc * nrow(Case_Study_Clean)))
train = Case_Study_Clean[trainIndices, ]
test = Case_Study_Clean[-trainIndices, ]
  
#Apply Under Sampling to Training Set
train_balanced = downSample(x = train[, c("MonthlyIncome", "YearsAtCompany", "OverTime", "PercentSalaryHike", "YearsInCurrentRole")],
                               y = train$Attrition)
  
train_balanced$Attrition = train_balanced$Class  
train_balanced$Class = NULL  
  
# Train the Naive Bayes Model on Balanced Training Set
nb_model = naiveBayes(Attrition ~ MonthlyIncome + YearsAtCompany + OverTime + PercentSalaryHike + YearsInCurrentRole,
                      data = train_balanced, laplace = 1)
  
# Predict for Positive Class
prob_predictions = predict(nb_model, test, type = "raw")[, "Yes"]
class_predictions = ifelse(prob_predictions >= threshold, "Yes", "No")
  
# Generate Confusion Matrix
CM_NB = confusionMatrix(factor(class_predictions, levels = c("No", "Yes")), test$Attrition, positive = "Yes")
print(CM_NB)
  
# Store Metrics for Accuracy, Precision, Recall, and F1
masterAcc[j] = CM_NB$overall["Accuracy"]
masterPrecision[j] = CM_NB$byClass["Pos Pred Value"] 
masterRecall[j] = CM_NB$byClass["Sensitivity"]        
masterF1[j] = 2 * (masterPrecision[j] * masterRecall[j]) / (masterPrecision[j] + masterRecall[j]) }  # F1 calculation
```
```{r}
# Calculate Mean Metrics Across All Iterations
MeanAcc = mean(masterAcc, na.rm = TRUE)
MeanPrecision = mean(masterPrecision, na.rm = TRUE)
MeanRecall = mean(masterRecall, na.rm = TRUE)
MeanF1 = mean(masterF1, na.rm = TRUE)
MeanAcc
MeanPrecision
MeanRecall
MeanF1
```





