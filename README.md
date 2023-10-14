# Topic : Prediction of Medical Care Costs (charges) using Multiple Linear and Polynomial Regression Model

# Introduction : 
Medical Care Charges are one of the most common recurring
expenses in a person’s life and the rising health care costs have become a major
economic and public health issue Worldwide. That’s why people’s health-care
cost prediction nowadays has become a valuable tool to improve
accountability in health care. Moreover, it has become very tough for our health to
cope up with today’s jet fast life routine, unavoidable bad habits, mental stress etc.
causing lots of health disorders, resulting in huge medical expenses.
So, an application can be made to make people understand some particular factors
which are making them unfit, and creating a lot of medical expenses, and it could
identify and estimate the medical expense if someone has such factors. Based
on different research studies, ageing, smoking, bmi and other factors are found
to be related to greater personal medical care costs.

# Objective of the overall analysis : 
The major focus of this study is to
estimate or predict the Medical Care Costs (charges) incurred due to various
factors in the population. Considering that the medical expense depends on
certain factors like age, sex, bmi, children, smoker, region of stay, the aim is to
predict the changes in patients’ health care costs and to identify the factors
contributing substantially to this prediction. Multiple Regression Model was
applied to predict the medical expenses for the patients based on the particular
factors.

# Dataset description :
Dataset download link : https://www.kaggle.com/mirichoi0218/insurance

Here the “insurance.csv” file includes 1,338 examples of beneficiaries
currently enrolled in the insurance plan, with factors indicating characteristics of
the patient as well as the total medical expenses charged. The factors are : Age,
Sex, Bmi (indicating body mass index), Children, Smoker and Region.
- age: age of primary beneficiary
- sex: insurance contractor gender, female, male
- bmi: Body mass index, providing an understanding of body,
weights that are relatively high or low relative to height,
objective index of body weight (kg / m ^ 2) using the ratio of
height to weight, ideally 18.5 to 24.9
- children: Number of children covered by health insurance /
Number of dependents
- smoker: Smoking
- region: the beneficiary's residential area in the US, northeast,
southeast, southwest, northwest.
- charges: Individual medical costs billed by health insurance

# Overall Method of the analysis
We want to predict the health care cost of a patient incurred due to various
factors in the population. So we have taken the ‘charges’ column as the target
column and the rest others as independent columns which will predict the
outcome. At first we have explored the data set to get a glimpse of our
overall analysis and after that data cleaning was done to check if there are
any missing values or not. And stored the dataset in the table named
‘No_Duplicates’ after removing the duplicate values. Then, as there are
three categorical columns: ‘sex’, ‘smoker’ and ‘region’ in our dataset, we
have encoded them as per need ie. the categorical values were converted
to numerical values. Finally,the main table named ‘Updated_Data’ was
created which was our cleaned dataset used for further analysis. Following
that, we tried to visualize our dataset using various plots including Pie-
Charts, Bar diagrams, Histograms for Univariate data analysis, Scatter
plots, density plots, box plots for Bivariate data analysis and bubble charts
for Multivariate data analysis. Next we have tested different hypothesis
including t test for ‘Sex’ and ‘Smoker’ columns. Anova test for ‘Region’ and
‘Children’ columns and chi square test. Lastly, we have fitted various
regression models including ‘Multiple Linear Regression Model’ and
‘Multiple Polynomial Regression Model’ of different orders to smoothly
predict future medical expenses for different patients in the population.

# Software used : RStudio 2022.02.2
