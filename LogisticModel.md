---
title: "LogisticRegression"
author: "Jonathan Flores"
date: "April 7, 2018"
output: 
  html_document:
    keep_md: yes
---
## Load Raw Data from GitHub


```r
## RCurl library reads saved csvs documents on GitHub.The code below process information from GitHub instead of our local computers

library(RCurl)
```

```
## Warning: package 'RCurl' was built under R version 3.4.3
```

```
## Loading required package: bitops
```

```
## Warning: package 'bitops' was built under R version 3.4.1
```

```r
library(psych)
```

```
## Warning: package 'psych' was built under R version 3.4.4
```

```r
attritionraw <- data.frame(read.csv(text = getURL("https://raw.githubusercontent.com/cyberkoolman/msds.6306.case.study.2/master/data/CaseStudy2-data1.csv"),header = TRUE,sep=","))
```


## Changing the entire data set into numeric information in order to run a logistic regression analysis


```r
library(knitr)

must_convert<-sapply(attritionraw,is.factor) # logical vector telling if a variable needs to be displayed as numeric 

M2<-sapply(attritionraw[,must_convert],unclass) # data.frame of all categorical variables now displayed as numeric

attritionclean <-cbind(attritionraw[,!must_convert],M2) # complete data.frame with all variables put together

knitr::kable(head(attritionclean)) 
```



 Age   DailyRate   DistanceFromHome   Education   EmployeeCount   EmployeeNumber   EnvironmentSatisfaction   HourlyRate   JobInvolvement   JobLevel   JobSatisfaction   MonthlyIncome   MonthlyRate   NumCompaniesWorked   PercentSalaryHike   PerformanceRating   RelationshipSatisfaction   StandardHours   StockOptionLevel   TotalWorkingYears   TrainingTimesLastYear   WorkLifeBalance   YearsAtCompany   YearsInCurrentRole   YearsSinceLastPromotion   YearsWithCurrManager   Attrition   BusinessTravel   Department   EducationField   Gender   JobRole   MaritalStatus   Over18   OverTime
----  ----------  -----------------  ----------  --------------  ---------------  ------------------------  -----------  ---------------  ---------  ----------------  --------------  ------------  -------------------  ------------------  ------------------  -------------------------  --------------  -----------------  ------------------  ----------------------  ----------------  ---------------  -------------------  ------------------------  ---------------------  ----------  ---------------  -----------  ---------------  -------  --------  --------------  -------  ---------
  41        1102                  1           2               1                1                         2           94                3          2                 4            5993         19479                    8                  11                   3                          1              80                  0                   8                       0                 1                6                    4                         0                      5           2                3            3                2        1         8               3        1          2
  49         279                  8           1               1                2                         3           61                2          2                 2            5130         24907                    1                  23                   4                          4              80                  1                  10                       3                 3               10                    7                         1                      7           1                2            2                2        2         7               2        1          1
  37        1373                  2           2               1                4                         4           92                2          1                 3            2090          2396                    6                  15                   3                          2              80                  0                   7                       3                 3                0                    0                         0                      0           2                3            2                5        2         3               3        1          2
  33        1392                  3           4               1                5                         4           56                3          1                 3            2909         23159                    1                  11                   3                          3              80                  0                   8                       3                 3                8                    7                         3                      0           1                2            2                2        1         7               2        1          2
  27         591                  2           1               1                7                         1           40                3          1                 2            3468         16632                    9                  12                   3                          4              80                  1                   6                       3                 3                2                    2                         2                      2           1                3            2                4        2         3               2        1          1
  32        1005                  2           2               1                8                         4           79                3          1                 4            3068         11864                    0                  13                   3                          3              80                  0                   8                       2                 2                7                    7                         3                      6           1                2            2                2        2         3               3        1          1

## Converting respective categorical variabes to run the model


```r
attritionclean$Attrition <- as.factor(attritionclean$Attrition)
attritionclean$BusinessTravel <- as.factor(attritionclean$BusinessTravel)
attritionclean$Department <- as.factor(attritionclean$Department)
attritionclean$Education <- as.factor(attritionclean$Education)
attritionclean$EducationField <- as.factor(attritionclean$EducationField)
attritionclean$EnvironmentSatisfaction <- as.factor(attritionclean$EnvironmentSatisfaction)
attritionclean$Gender <- as.factor(attritionclean$Gender)
attritionclean$JobInvolvement <- as.factor(attritionclean$JobInvolvement)
attritionclean$JobLevel <- as.factor(attritionclean$JobLevel)
attritionclean$JobRole <- as.factor(attritionclean$JobRole)
attritionclean$JobSatisfaction <- as.factor(attritionclean$JobSatisfaction)
attritionclean$Department <- as.factor(attritionclean$Department)
attritionclean$MaritalStatus <- as.factor(attritionclean$MaritalStatus)
attritionclean$OverTime <- as.factor(attritionclean$OverTime)
attritionclean$PerformanceRating <- as.factor(attritionclean$PerformanceRating)
attritionclean$RelationshipSatisfaction <- as.factor(attritionclean$RelationshipSatisfaction)
attritionclean$StockOptionLevel <- as.factor(attritionclean$StockOptionLevel)
attritionclean$WorkLifeBalance <- as.factor(attritionclean$WorkLifeBalance)


str((attritionclean))
```

'data.frame':	1470 obs. of  35 variables:
 $ Age                     : int  41 49 37 33 27 32 59 30 38 36 ...
 $ DailyRate               : int  1102 279 1373 1392 591 1005 1324 1358 216 1299 ...
 $ DistanceFromHome        : int  1 8 2 3 2 2 3 24 23 27 ...
 $ Education               : Factor w/ 5 levels "1","2","3","4",..: 2 1 2 4 1 2 3 1 3 3 ...
 $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
 $ EmployeeNumber          : int  1 2 4 5 7 8 10 11 12 13 ...
 $ EnvironmentSatisfaction : Factor w/ 4 levels "1","2","3","4": 2 3 4 4 1 4 3 4 4 3 ...
 $ HourlyRate              : int  94 61 92 56 40 79 81 67 44 94 ...
 $ JobInvolvement          : Factor w/ 4 levels "1","2","3","4": 3 2 2 3 3 3 4 3 2 3 ...
 $ JobLevel                : Factor w/ 5 levels "1","2","3","4",..: 2 2 1 1 1 1 1 1 3 2 ...
 $ JobSatisfaction         : Factor w/ 4 levels "1","2","3","4": 4 2 3 3 2 4 1 3 3 3 ...
 $ MonthlyIncome           : int  5993 5130 2090 2909 3468 3068 2670 2693 9526 5237 ...
 $ MonthlyRate             : int  19479 24907 2396 23159 16632 11864 9964 13335 8787 16577 ...
 $ NumCompaniesWorked      : int  8 1 6 1 9 0 4 1 0 6 ...
 $ PercentSalaryHike       : int  11 23 15 11 12 13 20 22 21 13 ...
 $ PerformanceRating       : Factor w/ 2 levels "3","4": 1 2 1 1 1 1 2 2 2 1 ...
 $ RelationshipSatisfaction: Factor w/ 4 levels "1","2","3","4": 1 4 2 3 4 3 1 2 2 2 ...
 $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
 $ StockOptionLevel        : Factor w/ 4 levels "0","1","2","3": 1 2 1 1 2 1 4 2 1 3 ...
 $ TotalWorkingYears       : int  8 10 7 8 6 8 12 1 10 17 ...
 $ TrainingTimesLastYear   : int  0 3 3 3 3 2 3 2 2 3 ...
 $ WorkLifeBalance         : Factor w/ 4 levels "1","2","3","4": 1 3 3 3 3 2 2 3 3 2 ...
 $ YearsAtCompany          : int  6 10 0 8 2 7 1 1 9 7 ...
 $ YearsInCurrentRole      : int  4 7 0 7 2 7 0 0 7 7 ...
 $ YearsSinceLastPromotion : int  0 1 0 3 2 3 0 0 1 7 ...
 $ YearsWithCurrManager    : int  5 7 0 0 2 6 0 0 8 7 ...
 $ Attrition               : Factor w/ 2 levels "1","2": 2 1 2 1 1 1 1 1 1 1 ...
 $ BusinessTravel          : Factor w/ 3 levels "1","2","3": 3 2 3 2 3 2 3 3 2 3 ...
 $ Department              : Factor w/ 3 levels "1","2","3": 3 2 2 2 2 2 2 2 2 2 ...
 $ EducationField          : Factor w/ 6 levels "1","2","3","4",..: 2 2 5 2 4 2 4 2 2 4 ...
 $ Gender                  : Factor w/ 2 levels "1","2": 1 2 2 1 2 2 1 2 2 2 ...
 $ JobRole                 : Factor w/ 9 levels "1","2","3","4",..: 8 7 3 7 3 3 3 3 5 1 ...
 $ MaritalStatus           : Factor w/ 3 levels "1","2","3": 3 2 3 2 2 3 2 1 3 2 ...
 $ Over18                  : int  1 1 1 1 1 1 1 1 1 1 ...
 $ OverTime                : Factor w/ 2 levels "1","2": 2 1 2 2 1 1 2 1 1 1 ...


## Removing non-value added variables from the data set:

Justification for dropping the following variables:

1. EmployeeCount: Always 1, since the data set is by employee
2. Over18: All employees are above 18. Since we have Age, this variable is more useful
3. StandardHours: All are Yes


```r
nonvalueaddedvariabes <- c("EmployeeCount","Over18","StandardHours")
attrition <- attritionclean[ , !(names(attritionclean) %in% nonvalueaddedvariabes)]
knitr::kable(head(attrition))
```



 Age   DailyRate   DistanceFromHome  Education    EmployeeNumber  EnvironmentSatisfaction    HourlyRate  JobInvolvement   JobLevel   JobSatisfaction    MonthlyIncome   MonthlyRate   NumCompaniesWorked   PercentSalaryHike  PerformanceRating   RelationshipSatisfaction   StockOptionLevel    TotalWorkingYears   TrainingTimesLastYear  WorkLifeBalance    YearsAtCompany   YearsInCurrentRole   YearsSinceLastPromotion   YearsWithCurrManager  Attrition   BusinessTravel   Department   EducationField   Gender   JobRole   MaritalStatus   OverTime 
----  ----------  -----------------  ----------  ---------------  ------------------------  -----------  ---------------  ---------  ----------------  --------------  ------------  -------------------  ------------------  ------------------  -------------------------  -----------------  ------------------  ----------------------  ----------------  ---------------  -------------------  ------------------------  ---------------------  ----------  ---------------  -----------  ---------------  -------  --------  --------------  ---------
  41        1102                  1  2                         1  2                                  94  3                2          4                           5993         19479                    8                  11  3                   1                          0                                   8                       0  1                               6                    4                         0                      5  2           3                3            2                1        8         3               2        
  49         279                  8  1                         2  3                                  61  2                2          2                           5130         24907                    1                  23  4                   4                          1                                  10                       3  3                              10                    7                         1                      7  1           2                2            2                2        7         2               1        
  37        1373                  2  2                         4  4                                  92  2                1          3                           2090          2396                    6                  15  3                   2                          0                                   7                       3  3                               0                    0                         0                      0  2           3                2            5                2        3         3               2        
  33        1392                  3  4                         5  4                                  56  3                1          3                           2909         23159                    1                  11  3                   3                          0                                   8                       3  3                               8                    7                         3                      0  1           2                2            2                1        7         2               2        
  27         591                  2  1                         7  1                                  40  3                1          2                           3468         16632                    9                  12  3                   4                          1                                   6                       3  3                               2                    2                         2                      2  1           3                2            4                2        3         2               1        
  32        1005                  2  2                         8  4                                  79  3                1          4                           3068         11864                    0                  13  3                   3                          0                                   8                       2  2                               7                    7                         3                      6  1           2                2            2                2        3         3               1        

```r
str(attrition)
```

'data.frame':	1470 obs. of  32 variables:
 $ Age                     : int  41 49 37 33 27 32 59 30 38 36 ...
 $ DailyRate               : int  1102 279 1373 1392 591 1005 1324 1358 216 1299 ...
 $ DistanceFromHome        : int  1 8 2 3 2 2 3 24 23 27 ...
 $ Education               : Factor w/ 5 levels "1","2","3","4",..: 2 1 2 4 1 2 3 1 3 3 ...
 $ EmployeeNumber          : int  1 2 4 5 7 8 10 11 12 13 ...
 $ EnvironmentSatisfaction : Factor w/ 4 levels "1","2","3","4": 2 3 4 4 1 4 3 4 4 3 ...
 $ HourlyRate              : int  94 61 92 56 40 79 81 67 44 94 ...
 $ JobInvolvement          : Factor w/ 4 levels "1","2","3","4": 3 2 2 3 3 3 4 3 2 3 ...
 $ JobLevel                : Factor w/ 5 levels "1","2","3","4",..: 2 2 1 1 1 1 1 1 3 2 ...
 $ JobSatisfaction         : Factor w/ 4 levels "1","2","3","4": 4 2 3 3 2 4 1 3 3 3 ...
 $ MonthlyIncome           : int  5993 5130 2090 2909 3468 3068 2670 2693 9526 5237 ...
 $ MonthlyRate             : int  19479 24907 2396 23159 16632 11864 9964 13335 8787 16577 ...
 $ NumCompaniesWorked      : int  8 1 6 1 9 0 4 1 0 6 ...
 $ PercentSalaryHike       : int  11 23 15 11 12 13 20 22 21 13 ...
 $ PerformanceRating       : Factor w/ 2 levels "3","4": 1 2 1 1 1 1 2 2 2 1 ...
 $ RelationshipSatisfaction: Factor w/ 4 levels "1","2","3","4": 1 4 2 3 4 3 1 2 2 2 ...
 $ StockOptionLevel        : Factor w/ 4 levels "0","1","2","3": 1 2 1 1 2 1 4 2 1 3 ...
 $ TotalWorkingYears       : int  8 10 7 8 6 8 12 1 10 17 ...
 $ TrainingTimesLastYear   : int  0 3 3 3 3 2 3 2 2 3 ...
 $ WorkLifeBalance         : Factor w/ 4 levels "1","2","3","4": 1 3 3 3 3 2 2 3 3 2 ...
 $ YearsAtCompany          : int  6 10 0 8 2 7 1 1 9 7 ...
 $ YearsInCurrentRole      : int  4 7 0 7 2 7 0 0 7 7 ...
 $ YearsSinceLastPromotion : int  0 1 0 3 2 3 0 0 1 7 ...
 $ YearsWithCurrManager    : int  5 7 0 0 2 6 0 0 8 7 ...
 $ Attrition               : Factor w/ 2 levels "1","2": 2 1 2 1 1 1 1 1 1 1 ...
 $ BusinessTravel          : Factor w/ 3 levels "1","2","3": 3 2 3 2 3 2 3 3 2 3 ...
 $ Department              : Factor w/ 3 levels "1","2","3": 3 2 2 2 2 2 2 2 2 2 ...
 $ EducationField          : Factor w/ 6 levels "1","2","3","4",..: 2 2 5 2 4 2 4 2 2 4 ...
 $ Gender                  : Factor w/ 2 levels "1","2": 1 2 2 1 2 2 1 2 2 2 ...
 $ JobRole                 : Factor w/ 9 levels "1","2","3","4",..: 8 7 3 7 3 3 3 3 5 1 ...
 $ MaritalStatus           : Factor w/ 3 levels "1","2","3": 3 2 3 2 2 3 2 1 3 2 ...
 $ OverTime                : Factor w/ 2 levels "1","2": 2 1 2 2 1 1 2 1 1 1 ...

```r
## Export to CSV

##write.csv(attrition,file="attrition.csv")
```

## Creating a Training and Testing data set from sampling




```r
set.seed(1234)
ind <- sample(2,nrow(attrition),replace=T,prob = c(0.8,0.2))
train <- attrition[ind==1,]
test <- attrition[ind==2,]
```




## Model 1 - Creating a Logistic Regression Model with all variables without any exclusions



```r
model1 <- glm(Attrition ~ Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel+ JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, data=train,family = 'binomial')

model1$aic
```

[1] 759.6022

```r
## Prediction

predmodel1 <- predict(model1,train,type='response')
knitr::kable(head(predmodel1))
```

              x
---  ----------
1     0.8080197
2     0.0013051
3     0.5991765
4     0.2709360
6     0.0480885
7     0.3075483

```r
comparison1 <- cbind(predmodel1,attrition$Attrition,attrition$EmployeeNumber)
```

```
## Warning in cbind(predmodel1, attrition$Attrition, attrition
## $EmployeeNumber): number of rows of result is not a multiple of vector
## length (arg 1)
```

```r
knitr::kable(head(comparison1,40))
```



 predmodel1          
-----------  ---  ---
  0.8080197    2    1
  0.0013051    1    2
  0.5991765    2    4
  0.2709360    1    5
  0.0480885    1    7
  0.3075483    1    8
  0.0497978    1   10
  0.0656125    1   11
  0.0104790    1   12
  0.0214624    1   13
  0.0939150    1   14
  0.1680177    1   15
  0.8888314    1   16
  0.0496312    1   18
  0.0724494    2   19
  0.0012902    1   20
  0.2057407    1   21
  0.0015970    1   22
  0.4074386    1   23
  0.0013087    1   24
  0.0434134    1   26
  0.0930637    2   27
  0.8919687    1   28
  0.0734306    1   30
  0.0815824    2   31
  0.0247049    1   32
  0.2382711    2   33
  0.0130000    1   35
  0.3580474    1   36
  0.0006348    1   38
  0.8435524    1   39
  0.2074206    1   40
  0.0159246    1   41
  0.0174467    2   42
  0.9041213    2   45
  0.0850191    1   46
  0.0093185    2   47
  0.0580234    1   49
  0.0707695    1   51
  0.3052018    1   52

```r
##Missclassification error - train data

pred1 <- ifelse(predmodel1>0.5,2,1)
tab1 <- table(Predicted = pred1, Actual = train$Attrition)
knitr::kable(head(tab1))
```



   1     2
----  ----
 947    83
  29   115


## Model 2 - Creating a Logistic Regression Model by removing variables that are not statistically significant

Removing: TotalWorkingYears, PercentSalaryHike, PerformanceRating, MonthlyRate, MonthlyIncome, MaritalStatus, HourlyRate, EducationField, Education,Gender,Department,DailyRate 


```r
model2 <- glm(Attrition ~ Age + BusinessTravel +    DistanceFromHome +   EnvironmentSatisfaction +   JobInvolvement + JobLevel+ JobRole + JobSatisfaction +   NumCompaniesWorked + OverTime +   RelationshipSatisfaction + StockOptionLevel + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, data=train,family = 'binomial')

model2$aic
```

[1] 746.901

```r
model2$rank
```

[1] 42

```r
##knitr::kable(summary(model2))
## Prediction

predmodel2 <- predict(model2,train,type='response')
knitr::kable(head(predmodel2))
```

              x
---  ----------
1     0.8105934
2     0.0011984
3     0.6529079
4     0.4053224
6     0.0702674
7     0.3082209

```r
comparison2 <- cbind(predmodel2,attrition$Attrition,attrition$EmployeeNumber)
```

```
## Warning in cbind(predmodel2, attrition$Attrition, attrition
## $EmployeeNumber): number of rows of result is not a multiple of vector
## length (arg 1)
```

```r
knitr::kable(head(comparison2,40))
```



 predmodel2          
-----------  ---  ---
  0.8105934    2    1
  0.0011984    1    2
  0.6529079    2    4
  0.4053224    1    5
  0.0702674    1    7
  0.3082209    1    8
  0.0964861    1   10
  0.0542264    1   11
  0.0134904    1   12
  0.0192291    1   13
  0.0945600    1   14
  0.2231070    1   15
  0.8166819    1   16
  0.0491551    1   18
  0.1024034    2   19
  0.0063888    1   20
  0.1712715    1   21
  0.0029974    1   22
  0.5583352    1   23
  0.0012948    1   24
  0.0412859    1   26
  0.0947991    2   27
  0.9362978    1   28
  0.0870488    1   30
  0.0956893    2   31
  0.0317208    1   32
  0.1729768    2   33
  0.0083020    1   35
  0.3594952    1   36
  0.0015557    1   38
  0.7215967    1   39
  0.1669872    1   40
  0.0175858    1   41
  0.0391593    2   42
  0.9000515    2   45
  0.1434261    1   46
  0.0202162    2   47
  0.0294874    1   49
  0.0467589    1   51
  0.2972428    1   52

```r
##Missclassification error - train data

pred2 <- ifelse(predmodel2>0.5,2,1)
tab2 <- table(Predicted = pred2, Actual = train$Attrition)
knitr::kable(head(tab2))
```



   1     2
----  ----
 948    87
  28   111

```r
##Missclassification percentage
1-sum(diag(tab2))/sum(tab2)
```

[1] 0.09795571

```r
##Missclassification error - testing data

p2t <- predict(model2,test,type = 'response')
pred2t <- ifelse(p2t>0.5,2,1)
tab2t <- table(Predicted = pred2t, Actual=test$Attrition)
knitr::kable(head(tab2t))
```



   1    2
----  ---
 242   24
  15   15

```r
1-sum(diag(tab2t))/sum(tab2t)
```

[1] 0.1317568

```r
# Goodness of fit test

with(model2,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))
```

[1] 6.813085e-61


