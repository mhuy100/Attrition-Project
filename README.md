# Attrition Project by Megan Huy
# Executive Summary

## Overview
The goal of this study was to investigate the factors contributing to employee attrition within the Frito Lay Company. By analyzing various employee attributes, the study aimed to develop a predictive model that identifies employees at risk of leaving, providing insights to improve retention strategies.

The analysis allows our stakeholders to find meaningful insight of attrition to help retain employees. This could improve employee satisfaction, employee growth, reduce burnout, and reduce cost or time to train new hires.
<p align="center">
<img src="Images/Status.png" alt="Status" width="400"/>
</p>

## Significant Factors Related to Attrition
Through exploratory data analysis and feature selection, the study identified the following factors as significantly related to attrition:
### Over Time

<p align="center">
  <img src="Images/Overtime Status - Yes.png" alt="Overtime Status - Yes" width="400"/>
</p>

### Percent Salary Hike

<p align="center">
  <img src="Images/Box Plot.png" alt="Box Plot" width="400"/>
</p>

### Years at Company

<p align="center">
  <img src="Images/Sale -Years vs Current .png" alt="Sale -Years vs Current" width="400"/>
  <img src="Images/HR Years vs. Current.png" alt="HR Years vs. Current" width="400"/>
  <img src="Images/Years vs Current R & d.png" alt="Years vs Current R & d" width="400"/>
</p>
### Monthly Income
<p align="center">
  <img src="Images/Scatterplot- Yes.png" alt="Scatterplot- Yes" width="400"/>
</p>

<p>These factors were found to be the most predictive of whether an employee would stay or leave.</p>

## Model Performance
The Naive Bayes and k-Nearest Neighbors (k-NN) models were tested for predicting attrition. The Naive Bayes model was chosen as the final model, achieving a sensitivity of 65% and a specificity of 62% on the validation set. These metrics meet the required minimum for identifying at-risk employees while minimizing false positives.

## Additional Inference
Employees that had low monthly income and worked overtime were more likely to leave, particularly in Sales and Research & Development. Most employees that left Human Resources worked for less than five years.
