# Installing Required Packages
install.packages("dplyr") # to manipulate, clean and summarize unstructured data
install.packages("ggplot2") # for necessary diagrams

# Loading Required Libraries
library(dplyr)
library(ggplot2)

# Importing the data set
Data = read.csv("C:/Users/Sruba Sarkar/OneDrive/Desktop/insurance.csv",header=T)

# Displaying the whole data set
View(Data)

# ------------------------------------------
## EXPLORING THE DATA SET
# ------------------------------------------
# Getting the Structure of the whole data set
str(Data)
# Getting the Summary of the whole data set
summary(Data)
# Getting the total number of Rows and Columns
dim(Data)

# ------------------------------------------
## CLEANING THE DATA SET
# ------------------------------------------
# Checking for missing Values
table(is.na(Data))
# Checking for duplicate Values and omitting them
sum(duplicated(Data))
which(duplicated(Data))
# Creating cleaned data set with no duplicates
No_Duplicates <- Data[!duplicated(Data), ]
# Checking again
dim(No_Duplicates)
sum(duplicated(No_Duplicates))

# ------------------------------------------
## DATA PRE-PROCESSING
# ------------------------------------------
# Converting Categorical values to Numerical values
Sex <- unclass(as.factor(No_Duplicates$sex))
Smoker <- unclass(as.factor(No_Duplicates$smoker))
Region <- unclass(as.factor(No_Duplicates$region))
# ----------------------------------------------------------------------------------------------------------------------------------------
# Creating the main table
Updated_Data <- data.frame(No_Duplicates$age,Sex,No_Duplicates$bmi,No_Duplicates$children,Smoker,Region,No_Duplicates$charges)
# Assigning new names to the columns of the main table
colnames(Updated_Data) <-c('Age','Sex','Bmi','Children','Smoker','Region','Charges')
# Displaying the “Updated_Data”
View(Updated_Data)

# -------------------------------------------------------------------
## CALCULATING SKEWNESS-KURTOSIS FOR THE DATASET
# -------------------------------------------------------------------
# Installing ‘moments’ package
install.packages("moments")
# Loading the library
library("moments")
# Getting skewness of each variable of dataset
skewness=round(skewness(Updated_Data),2)
skewness
# Getting kurtosis of each variable of dataset
kurtosis=round(kurtosis(Updated_Data),2)
excess_kurtosis=kurtosis-3
excess_kurtosis

# ------------------------------------------
## DATA VISUALIZATION
# ------------------------------------------
# Creating frequency table for 'Yes-No' with respect to the “Smoker” column
smoker_freq <- table(Updated_Data$Smoker)
# Finding percentages of 'Yes-No'
percentageYN <- round(smoker_freq/1337*100,digits=2)
# Drawing a Pie Chart for "Smokers"
pie(smoker_freq,labels = paste(names(table(No_Duplicates$smoker)),"--
",percentageYN,"%"),main = "Distribution of Smokers",border="black",col =
      c(2,5))

# Creating frequency table for 'Female-Male' with respect to the “Sex”column
sex_freq <- table(Updated_Data$Sex)
# Finding percentages of 'Female-Male'
percentageFM <- round(sex_freq/1337*100,digits=2)
# Drawing a Pie Chart for "Sex" column
pie(sex_freq,labels = paste(names(table(No_Duplicates$sex)),"--
",percentageFM,"%"),
    main = "Distribution of MALE~FEMALE",border="black",col = c("lightpink","lightblue"),density=50,lty = 2)

# Creating frequency table with respect to the 'region' column
region_freq <- table(Updated_Data$Region)
# Finding percentages of 4 categories
percentageReg <- round(region_freq/1337*100,digits=2)
# Creating label for pie chart
newLbl <- paste(names(table(No_Duplicates$region)),"--",percentageReg,"%")
# Drawing a pie chart
pie(region_freq,labels = newLbl, main = "Distribution of Region of stay",col = c(5,6,8,7),density=50)
# Creating Bar diagram for number of children
ggplot(data.frame(Updated_Data$Children),aes(x=as.factor(Updated_Data$Children), fill=as.factor(Updated_Data$Children))) +
  geom_bar() + labs(title = "Bar plot for number of children",x = "Number of
children per person",y = "Count",caption = "(based on data from
insurance.csv)")+ scale_fill_manual(values=c("violet","lightblue", "lightgreen", "yellow", "pink", "grey") ) + theme(legend.position="none",plot.title= element_text(face="italic",hjust = 0.5))
# Assigning the ‘age’ values to a variable
AGE = Updated_Data$Age
# Creating histogram for distribution of Age
ggplot(data.frame(AGE), aes(x=AGE)) +
  geom_histogram(aes(y=..density..), fill="grey",alpha=0.5)+
  geom_density(alpha=.2,col="black",size=2) + labs(title="Distribution of
AGE")
# Assigning the ‘bmi values to a variable
BMI = Updated_Data$Bmi
# Creating histogram for distribution of BMI
ggplot(data.frame(BMI), aes(x=BMI)) +
  geom_histogram(aes(y=..density..),fill="orange",alpha=0.5) +
  geom_density(alpha=.2,col="orange",size=2) + labs(title="Distribution of
BMI")
# Assigning the ‘charges’ values to a variable
CHARGES = Updated_Data$Charges
# Creating histogram for distribution of CHARGES
ggplot(data.frame(CHARGES), aes(x=CHARGES)) +
  geom_histogram(aes(y=..density..),fill="light pink",alpha=0.5) +
  geom_density(alpha=.2,col="violet",size=2) + labs(title="Distribution of
CHARGES")
# Creating histogram for distribution of CHARGES
ggplot(data.frame(CHARGES), aes(x=log(CHARGES)))+
  geom_histogram(aes(y=..density..),fill="yellow",alpha=0.5) +
  geom_density(alpha=.01,col="lightgreen",size=2) + labs(title="Distribution
of CHARGES after log transformation")
# Creating Scatter plots to show the variation
ggplot(Updated_Data,aes(x=Age,y=Charges))+geom_point(col="lightgreen")+geom_smooth(method="auto", se=TRUE, fullrange=FALSE,
                    level=0.95)+labs(title="Scatter Plot -- Charges ~ Age")
# Creating Scatter plots to show the variation
ggplot(Updated_Data,aes(x=Bmi,y=Charges))+geom_point(col="orange")+geom_smooth(method="auto", se=TRUE, fullrange=FALSE,
          level=0.95)+labs(title="Scatter Plot -- Charges ~ Bmi")
# Applying factors to ‘Sex’ column
SEX <- as.factor(Updated_Data$Sex)
# Creating Density plot for Females and Males
ggplot(Updated_Data,aes(Charges))+geom_density(aes(fill=SEX),color=NA,
                                               alpha=0.6)+labs(title = " Density plot for Females and Males ")
Sex_cat=as.factor(levels(SEX))
# Creating tables for subsetting the dataset
new_table1<-subset(Updated_Data,Charges & Sex==1)
new_table2<-subset(Updated_Data,Charges & Sex==2)
# Calculating mean charges for two subsets and creating new table
Charges=c(mean(new_table1$Charges),mean(new_table2$Charges))
new_table3=data.frame(Sex_cat,Charges)
# Creating Bar Diagram for Females and Males
ggplot(new_table3,aes(Sex_cat,Charges))+
  geom_bar(stat = "identity",width = 0.4, fill = "light green", alpha = 0.6)+
  labs(title = "Bar plot -- Charges ~ Sex")
# Applying factors to ‘Children’ column
CHILDREN <- as.factor(Updated_Data$Children)
# Creating Density plot for number of children
ggplot(Updated_Data,aes(Charges))+geom_density(aes(fill=CHILDREN),color=NA,alpha=0.6)+labs(title = "Density plot for number of children")
Children_cat=as.factor(levels(CHILDREN))
# Creating tables for subsetting the dataset
new_table4<-subset(Updated_Data,Charges & Children==0)
new_table5<-subset(Updated_Data, Charges & Children==1)
new_table6<-subset(Updated_Data, Charges & Children==2)
new_table7<-subset(Updated_Data, Charges & Children==3)
new_table8<-subset(Updated_Data, Charges & Children==4)
new_table9<-subset(Updated_Data, Charges & Children==5)
# Calculating mean charges for six subsets and creating new table
Charges=c(mean(new_table4$Charges),mean(new_table5$Charges),
          mean(new_table6$Charges),
          mean(new_table7$Charges),mean(new_table8$Charges),mean(new_table9
                                                                 $Charges))
new_table10=data.frame(Children_cat,Charges)
# Creating Bar Diagram for people having different number of children
ggplot(new_table10,aes(Children_cat,Charges))+geom_bar(stat =
                                                         "identity",width = 0.4, fill = c("orange","yellow","green","red","pink","blue"),
                                                       alpha = 0.6)+
  labs(title = "Bar plot -- Charges ~ Children")
# Applying factors to ‘Region’ column
REGION <- as.factor(Updated_Data$Region)
# Creating Density plot for inhabitants in different regions
ggplot(Updated_Data,aes(Charges))+geom_density(aes(fill=REGION),color=
                                                 NA,alpha=0.6)+labs(title = "Density plot for inhabitants in different
regions")
Region_cat=as.factor(levels(REGION))
# Creating tables for subsetting the dataset
new_table4<-subset(Updated_Data,Charges & Region==1)
new_table5<-subset(Updated_Data, Charges & Region==2)
new_table6<-subset(Updated_Data, Charges & Region==3)
new_table7<-subset(Updated_Data, Charges & Region==4)
# Calculating mean charges for four subsets and creating new table
Charges=c(mean(new_table4$Charges),mean(new_table5$Charges),
          mean(new_table6$Charges), mean(new_table7$Charges))
new_table8=data.frame(Region_cat,Charges)
# Creating Bar Diagram for different regions
ggplot(new_table8,aes(Region_cat,Charges))+geom_bar(stat =
                                                      "identity",width = 0.4, fill = c("orange","yellow","green","red"), alpha = 0.6)+
  labs(title = "Bar plot -- Charges ~ Region")
# Applying factors to ‘Smoker’ column
SMOKER <- as.factor(Updated_Data$Smoker)
# Creating Density plot for Non-Smokers and Smokers
ggplot(Updated_Data,aes(Charges))+geom_density(aes(fill=SMOKER),color
                                               =NA,alpha=0.6)+labs(title = " Density plot for Non-Smokers and Smokers ")
Smoker_cat=as.factor(levels(SMOKER))
# Creating tables for subsetting the dataset
new_table4<-subset(Updated_Data,Charges & Smoker==1)
new_table5<-subset(Updated_Data,Charges & Smoker==2)
# Calculating mean charges for two subsets and creating new table
Charges=c(mean(new_table4$Charges),mean(new_table5$Charges))
new_table6=data.frame(Smoker_cat,Charges)
# Creating Bar Diagram for Females and Males
ggplot(new_table6,aes(Smoker_cat,Charges))+ geom_bar(stat =
                                                       "identity",width = 0.4, fill = "orange", alpha = 0.6)+ labs(title = "Bar plot --
Charges ~ Smoker")
# Creating Boxplots for Charges ~ Number of Children
boxplot(Updated_Data$Charges~Updated_Data$Children, xlab = "Number
of Children", ylab = "Charges", main = "Box Plot",col=c("lightblue","orange","yellow","lightgreen","grey","violet"))
# Creating Boxplots for Charges ~ Smoker Categories
boxplot(Updated_Data$Charges~Updated_Data$Smoker, xlab = "Smoker
categories",ylab = "Charges", main = "Box Plot",col=c("lightgreen","violet"))
# Creating Boxplots for Charges ~ Sex Categories
boxplot(Updated_Data$Charges~Updated_Data$Sex, xlab = "Sex
categories",ylab = "Charges", main = "Box Plot",col=c("lightpink","lightblue"))
# Creating Boxplots for Charges ~ Region Categories
boxplot(Updated_Data$Charges~Updated_Data$Region, xlab = "REGION
Categories",ylab = "Charges", main = "Box Plot",col=c("lightpink","lightblue","lightyellow","lightgreen"))
# Plotting data to visualize the structure
plot(Updated_Data)
# Plotting the relation of Charges with Bmi, age, and smoker
ggplot(Updated_Data,aes(x=Charges,y=Bmi,color=SMOKER,size=Age)) +
  geom_point(alpha = .6) + labs(title = "Relation of Charges with Bmi, age,
and smoker")
# Plotting the relation of Charges with Bmi, age, and region
ggplot(Updated_Data,aes(x=Charges,y=Bmi,color=REGION,size=Age)) +
  geom_point(alpha = .6) + labs(title = "Relation of Charges with Bmi, age,
and region")
# Plotting the relation of Charges with Bmi, age, and sex
ggplot(Updated_Data,aes(x=Charges,y=Bmi,color=SEX,size=Age)) +
  geom_point(alpha = .6) + labs(title = "Relation of Charges with Bmi, age,
and sex")


# Applying t-test on ‘Sex’ column
t.test(Updated_Data$Charges~Updated_Data$Sex,mu=0,conf=0.95,var.eq=F,
       Paired=F)
# Applying t-test on ‘Smoker’ column
t.test(Updated_Data$Charges~Updated_Data$Smoker,mu=0,conf=0.95,var.eq=F,Paired=F)
# Applying ANOVA-test on ‘Children’ column
a<-aov(Updated_Data$Charges~factor(Updated_Data$Children))
summary(a)
# Applying ANOVA-test on ‘Region’ column
b<-aov(Updated_Data$Charges~factor(Updated_Data$Region))
summary(b)
# Applying Chi-Square test
data = data.frame(Updated_Data$Sex,Updated_Data$Smoker)
data = table(Updated_Data$Sex,Updated_Data$Smoker)
print(chisq.test(data))
n_train <- round(0.8 * nrow(Updated_Data)) #0.8 of df is Train
train_indices <- sample(1:nrow(Updated_Data), n_train)
df_train <- Updated_Data[train_indices, ]
df_test <- Updated_Data[-train_indices, ]


# Checking the dimension of the splitted data
dim(df_train)
dim(df_test)

# Creating the relationship model.
multiple.regression1 <-lm(Charges~Age+Sex+Bmi+Children+Smoker+Region, data = df_train )
# Showing the model
print(multiple.regression1)
# Getting the summary of the model
summary(multiple.regression1)
# Showing the plot of ‘Residuals vs Fitted’
plot(multiple.regression1)
# Creating the relationship model
multiple.regression2 <- lm(Charges~ Age+ Bmi+ Children+ Smoker, data =df_train)
# Showing the model
print(multiple.regression2)
# Getting the summary of the model
summary(multiple.regression2)
# Showing the plot of ‘Residuals vs Fitted’
plot(multiple.regression2)
# Creating the relationship model
Poly1=lm(Charges ~ polym(Bmi,degree=2, raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly1)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly1)
# Creating the relationship model
Poly2=lm(Charges ~ polym(Children,degree=2, raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly2)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly2)
# Creating the relationship model
Poly3=lm(Charges ~ polym(Smoker,degree=2, raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly3)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly3)
# Creating the relationship model
Poly4=lm(Charges ~ polym(Sex,degree=2, raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly4)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly4)
# Creating the relationship model
Poly5=lm(Charges ~ polym(Region,degree=2, raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly5)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly5)
# Creating the relationship model
Poly6=lm(Charges ~ polym(Age,degree=2, raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly6)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly6)
# Creating the relationship model
Poly_Reg_Fit1 = lm(Charges ~ polym(Age, Sex, Bmi, Children, Smoker,Region,degree=2, raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly_Reg_Fit1)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly_Reg_Fit1)
# Creating the relationship model
Poly_Reg_Fit2 = lm(Charges ~ polym(Age,Bmi,Smoker,Children,degree=2,raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly_Reg_Fit2)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly_Reg_Fit2)
# Creating the relationship model
Poly_Reg_Fit3 = lm(Charges ~ polym(Age, Sex, Bmi, Children, Smoker,Region,degree=4, raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly_Reg_Fit3)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly_Reg_Fit3)
# Creating the relationship model
Poly_Reg_Fit4 = lm(df_train$Charges ~
                     polym(Age,Bmi,Smoker,Children,degree=4, raw=TRUE),data=df_train)
# Getting the summary of the model
summary(Poly_Reg_Fit4)
# Showing the plot of ‘Residuals vs Fitted’
plot(Poly_Reg_Fit4)
# Creating a data frame to store the predicted values
Prediction=data.frame(predict(multiple.regression1,df_test))
# Checking dimension of the created data frame
dim(Prediction)
# Using ‘cbind’ function to insert the column with predicted values
output <- cbind(df_test,Prediction)
# Checking dimension of the ‘output’ variable
dim(output)
# Showing the final Output
output
# Creating a data frame with actual and predicted values
Res1=data.frame(df_test$Charges,Prediction)
# Calculating RMSE value
sqrt(mean((Res1$df_test.Charges -
             Res1$predict.multiple.regression1..df_test.)^2))
# Calculating R^2 value
summary(multiple.regression1)$r.squared
# Creating a data frame with observed and predicted values
data_plot1 <- data.frame(Observed = df_test$Charges,
                         Predicted = predict(multiple.regression1,df_test))
# plotting “Observed vs Predicted”
ggplot(data_plot1,aes(x = Predicted,y = Observed)) + geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red",
              size = 2)+labs(title="Observed vs Predicted values plot
(multiple.regression1)")
# Creating a data frame to store the predicted values
Prediction_1=data.frame(predict(multiple.regression2,df_test))
# Checking dimension of the created data frame
dim(Prediction_1)
# Using ‘cbind’ function to insert the column with predicted values
output1 <- cbind(df_test,Prediction_1)
# Showing the final Output
output1
# Creating a data frame with actual and predicted values
Res2=data.frame(df_test$Charges,Prediction_1)
# Calculating RMSE value
sqrt(mean((Res2$df_test.Charges -
             Res2$predict.multiple.regression2..df_test.)^2))
# Calculating R^2 value
summary(multiple.regression2)$r.squared
# Creating a data frame with observed and predicted values
data_plot2 <- data.frame(Observed = df_test$Charges,
                         Predicted = predict(multiple.regression2,df_test))
# plotting “Observed vs Predicted”
ggplot(data_plot2,aes(x = Predicted,y = Observed)) + geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red",size = 2)+
  labs(title="Observed vs Predicted values plot (multiple.regression2)")
# Creating a data frames to store the predicted values
pred1=data.frame(predict(Poly_Reg_Fit1,df_test))
pred2=data.frame(predict(Poly_Reg_Fit2,df_test))
pred3=data.frame(predict(Poly_Reg_Fit3,df_test))
Res_data=data.frame(df_test$Charges,pred1)
Res_data0=data.frame(df_test$Charges,pred2)
Res_data1=data.frame(df_test$Charges,pred3)
# Calculating RMSE value
sqrt(mean((Res_data$df_test.Charges -
             Res_data$predict.Poly_Reg_Fit1..df_test.)^2))
sqrt(mean((Res_data0$df_test.Charges -
             Res_data0$predict.Poly_Reg_Fit2..df_test.)^2))
sqrt(mean((Res_data1$df_test.Charges -
             Res_data1$predict.Poly_Reg_Fit3..df_test.)^2))
# Calculating R^2 value
r_sq_0 <- summary(Poly_Reg_Fit1)$r.squared
r_sq_0
r_sq_1 <- summary(Poly_Reg_Fit2)$r.squared
r_sq_1
r_sq_2 <- summary(Poly_Reg_Fit3)$r.squared
r_sq_2
# Creating a data frame with observed and predicted values
data_plot3 <- data.frame(Observed = df_test$Charges,
                         Predicted = predict(Poly_Reg_Fit1,df_test))
# plotting “Observed vs Predicted”
ggplot(data_plot3,aes(x = Predicted,y = Observed)) + geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red",size = 2)+
  labs(title="Observed vs Predicted values plot (Poly_Reg_Fit1)")
# Creating a data frame with observed and predicted values
data_plot4 <- data.frame(Observed = df_test$Charges,
                         Predicted = predict(Poly_Reg_Fit2,df_test))
# plotting “Observed vs Predicted”
ggplot(data_plot4,aes(x = Predicted,y = Observed)) + geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red",size = 2)+
  labs(title="Observed vs Predicted values plot (Poly_Reg_Fit2)")
# Creating a data frame with observed and predicted values
data_plot5 <- data.frame(Observed = df_test$Charges,
                         Predicted = predict(Poly_Reg_Fit3,df_test))
# plotting “Observed vs Predicted”
ggplot(data_plot5,aes(x = Predicted,y = Observed)) + geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red",size = 2)+
  labs(title="Observed vs Predicted values plot (Poly_Reg_Fit3)")
# Creating a data frame to store the predicted values
Prediction_2=data.frame(predict(Poly_Reg_Fit4,df_test))
# Checking dimension of the created data frame
dim(Prediction_2)
# Using ‘cbind’ function to insert the column with predicted values
output2 <- cbind(df_test,Prediction_2)
# Showing the final Output
output2
# Creating a data frame with actual and predicted values
pred4=data.frame(predict(Poly_Reg_Fit4,df_test))
Res_data2=data.frame(df_test$Charges,pred4)
# Calculating RMSE value
sqrt(mean((Res_data2$df_test.Charges -
             Res_data2$predict.Poly_Reg_Fit4..df_test.)^2))
# Calculating R^2 value
r_sq_3 <- summary(Poly_Reg_Fit4)$r.squared
r_sq_3
# Creating a data frame with observed and predicted values
data_plot6 <- data.frame(Observed = df_test$Charges,
                         Predicted = predict(Poly_Reg_Fit4,df_test))
# plotting “Observed vs Predicted”
ggplot(data_plot6,aes(x = Predicted,y = Observed)) + geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red",size = 2)+
  labs(title="Observed vs Predicted values plot (Poly_Reg_Fit4)")
shapiro.test(df_train$Charges)
# Performing log transformation
Charges_l=log(df_train$Charges)
# Fitting model
model=lm(Charges_l~Age+Sex+Bmi+Children+Smoker+Region, data =
           df_train )
Pred=predict(model,df_test)
shapiro.test(Charges_l)
# Creating a data frame with actual and predicted values
Res_l=data.frame(df_test$Charges,pred=exp(Pred))
# Calculating RMSE value
sqrt(mean((Res_l$df_test.Charges - Res_l$pred)^2))
# Calculating R^2 value
#summary(fit)$r.squared
# Creating a data frame with observed and predicted values
data_plot8 <- data.frame(Observed = df_test$Charges,
                         Predicted = exp(predict(model,df_test)))
# plotting “Observed vs Predicted”
ggplot(data_plot8,aes(x = Predicted,y = Observed)) + geom_point() +
  geom_abline(intercept = 0,slope = 1,color = "red",
              size = 2)+labs(title="Observed vs Predicted values plot (model)")
# Comparing R Squared value using Bar Chart
r_sq1 <- summary(multiple.regression1)$r.squared
r_sq2 <- summary(multiple.regression2)$r.squared
R2 <- c(r_sq1,r_sq2,r_sq_1,r_sq_3)
models=c("model 1","model 2","model 3","model 4")
barplot(R2,names.arg=models,ylab="R Squared value",col=c("grey","lightblue","pink","lightyellow"),
        main="R SQUARE Comparison Chart",border="black")
# Calculating RMSE values for different models
m1=sqrt(mean((Res1$df_test.Charges -
                Res1$predict.multiple.regression1..df_test.)^2))
m2=sqrt(mean((Res2$df_test.Charges -
                Res2$predict.multiple.regression2..df_test.)^2))
m3=sqrt(mean((Res_data0$df_test.Charges -
                Res_data0$predict.Poly_Reg_Fit2..df_test.)^2))
m4=sqrt(mean((Res_data2$df_test.Charges -
                Res_data2$predict.Poly_Reg_Fit4..df_test.)^2))
# Creating a vector to store the RMSE Values
RMSE_Comp=c(m1,m2,m3,m4)
# Comparing RMSE values using Bar Chart
barplot(RMSE_Comp,names.arg=models,ylab="RMSE",col=c("grey","lightblue","pink","lightyellow"),
        main="RMSE Comparison Chart",border="black")

