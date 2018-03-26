Source Documents
----------------

``` r
## Reading from CaseStudy2-data.xlsx. The Excel file is on local
library("readxl")
library("tidyr")
library("devtools")
library("ggplot2")
library("dplyr")
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library("knitr")

case_data <- data.frame(read_excel("data/CaseStudy2-data.xlsx"))

factor_cols <- c("Attrition", "BusinessTravel", "Department", "Education", "EducationField", "EnvironmentSatisfaction", "Gender", "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus", "OverTime")
case_data[factor_cols] <- lapply(case_data[factor_cols], factor)

summary(case_data)
```

      Age        Attrition            BusinessTravel   DailyRate     

Min. :18.00 No :1233 Non-Travel : 150 Min. : 102.0  
1st Qu.:30.00 Yes: 237 Travel\_Frequently: 277 1st Qu.: 465.0  
Median :36.00 Travel\_Rarely :1043 Median : 802.0  
Mean :36.92 Mean : 802.5  
3rd Qu.:43.00 3rd Qu.:1157.0  
Max. :60.00 Max. :1499.0

                  Department  DistanceFromHome Education

Human Resources : 63 Min. : 1.000 1:170  
Research & Development:961 1st Qu.: 2.000 2:282  
Sales :446 Median : 7.000 3:572  
Mean : 9.193 4:398  
3rd Qu.:14.000 5: 48  
Max. :29.000

          EducationField EmployeeCount EmployeeNumber  

Human Resources : 27 Min. :1 Min. : 1.0  
Life Sciences :606 1st Qu.:1 1st Qu.: 491.2  
Marketing :159 Median :1 Median :1020.5  
Medical :464 Mean :1 Mean :1024.9  
Other : 82 3rd Qu.:1 3rd Qu.:1555.8  
Technical Degree:132 Max. :1 Max. :2068.0

EnvironmentSatisfaction Gender HourlyRate JobInvolvement 1:284
Female:588 Min. : 30.00 1: 83  
2:287 Male :882 1st Qu.: 48.00 2:375  
3:453 Median : 66.00 3:868  
4:446 Mean : 65.89 4:144  
3rd Qu.: 83.75  
Max. :100.00

JobLevel JobRole JobSatisfaction MaritalStatus 1:543 Sales Executive
:326 1:289 Divorced:327  
2:534 Research Scientist :292 2:280 Married :673  
3:218 Laboratory Technician :259 3:442 Single :470  
4:106 Manufacturing Director :145 4:459  
5: 69 Healthcare Representative:131  
Manager :102  
(Other) :215  
MonthlyIncome MonthlyRate NumCompaniesWorked Over18  
Min. : 1009 Min. : 2094 Min. :0.000 Length:1470  
1st Qu.: 2911 1st Qu.: 8047 1st Qu.:1.000 Class :character  
Median : 4919 Median :14236 Median :2.000 Mode :character  
Mean : 6503 Mean :14313 Mean :2.693  
3rd Qu.: 8379 3rd Qu.:20462 3rd Qu.:4.000  
Max. :19999 Max. :26999 Max. :9.000

OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction No
:1054 Min. :11.00 Min. :3.000 Min. :1.000  
Yes: 416 1st Qu.:12.00 1st Qu.:3.000 1st Qu.:2.000  
Median :14.00 Median :3.000 Median :3.000  
Mean :15.21 Mean :3.154 Mean :2.712  
3rd Qu.:18.00 3rd Qu.:3.000 3rd Qu.:4.000  
Max. :25.00 Max. :4.000 Max. :4.000

StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear
Min. :80 Min. :0.0000 Min. : 0.00 Min. :0.000  
1st Qu.:80 1st Qu.:0.0000 1st Qu.: 6.00 1st Qu.:2.000  
Median :80 Median :1.0000 Median :10.00 Median :3.000  
Mean :80 Mean :0.7939 Mean :11.28 Mean :2.799  
3rd Qu.:80 3rd Qu.:1.0000 3rd Qu.:15.00 3rd Qu.:3.000  
Max. :80 Max. :3.0000 Max. :40.00 Max. :6.000

WorkLifeBalance YearsAtCompany YearsInCurrentRole Min. :1.000 Min. :
0.000 Min. : 0.000  
1st Qu.:2.000 1st Qu.: 3.000 1st Qu.: 2.000  
Median :3.000 Median : 5.000 Median : 3.000  
Mean :2.761 Mean : 7.008 Mean : 4.229  
3rd Qu.:3.000 3rd Qu.: 9.000 3rd Qu.: 7.000  
Max. :4.000 Max. :40.000 Max. :18.000

YearsSinceLastPromotion YearsWithCurrManager Min. : 0.000 Min. : 0.000  
1st Qu.: 0.000 1st Qu.: 2.000  
Median : 1.000 Median : 3.000  
Mean : 2.188 Mean : 4.123  
3rd Qu.: 3.000 3rd Qu.: 7.000  
Max. :15.000 Max. :17.000

``` r
## Quick glance at attrition data

attrition_data <- case_data[which(case_data$Attrition == 'Yes'), ]
summary(attrition_data)
```

      Age        Attrition           BusinessTravel   DailyRate     

Min. :18.00 No : 0 Non-Travel : 12 Min. : 103.0  
1st Qu.:28.00 Yes:237 Travel\_Frequently: 69 1st Qu.: 408.0  
Median :32.00 Travel\_Rarely :156 Median : 699.0  
Mean :33.61 Mean : 750.4  
3rd Qu.:39.00 3rd Qu.:1092.0  
Max. :58.00 Max. :1496.0

                  Department  DistanceFromHome Education

Human Resources : 12 Min. : 1.00 1:31  
Research & Development:133 1st Qu.: 3.00 2:44  
Sales : 92 Median : 9.00 3:99  
Mean :10.63 4:58  
3rd Qu.:17.00 5: 5  
Max. :29.00

          EducationField EmployeeCount EmployeeNumber

Human Resources : 7 Min. :1 Min. : 1  
Life Sciences :89 1st Qu.:1 1st Qu.: 514  
Marketing :35 Median :1 Median :1017  
Medical :63 Mean :1 Mean :1010  
Other :11 3rd Qu.:1 3rd Qu.:1486  
Technical Degree:32 Max. :1 Max. :2055

EnvironmentSatisfaction Gender HourlyRate JobInvolvement 1:72 Female: 87
Min. : 31.00 1: 28  
2:43 Male :150 1st Qu.: 50.00 2: 71  
3:62 Median : 66.00 3:125  
4:60 Mean : 65.57 4: 13  
3rd Qu.: 84.00  
Max. :100.00

JobLevel JobRole JobSatisfaction MaritalStatus 1:143 Laboratory
Technician :62 1:66 Divorced: 33  
2: 52 Sales Executive :57 2:46 Married : 84  
3: 32 Research Scientist :47 3:73 Single :120  
4: 5 Sales Representative :33 4:52  
5: 5 Human Resources :12  
Manufacturing Director:10  
(Other) :16  
MonthlyIncome MonthlyRate NumCompaniesWorked Over18  
Min. : 1009 Min. : 2326 Min. :0.000 Length:237  
1st Qu.: 2373 1st Qu.: 8870 1st Qu.:1.000 Class :character  
Median : 3202 Median :14618 Median :1.000 Mode :character  
Mean : 4787 Mean :14559 Mean :2.941  
3rd Qu.: 5916 3rd Qu.:21081 3rd Qu.:5.000  
Max. :19859 Max. :26999 Max. :9.000

OverTime PercentSalaryHike PerformanceRating RelationshipSatisfaction No
:110 Min. :11.0 Min. :3.000 Min. :1.000  
Yes:127 1st Qu.:12.0 1st Qu.:3.000 1st Qu.:2.000  
Median :14.0 Median :3.000 Median :3.000  
Mean :15.1 Mean :3.156 Mean :2.599  
3rd Qu.:17.0 3rd Qu.:3.000 3rd Qu.:4.000  
Max. :25.0 Max. :4.000 Max. :4.000

StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear
Min. :80 Min. :0.0000 Min. : 0.000 Min. :0.000  
1st Qu.:80 1st Qu.:0.0000 1st Qu.: 3.000 1st Qu.:2.000  
Median :80 Median :0.0000 Median : 7.000 Median :2.000  
Mean :80 Mean :0.5274 Mean : 8.245 Mean :2.624  
3rd Qu.:80 3rd Qu.:1.0000 3rd Qu.:10.000 3rd Qu.:3.000  
Max. :80 Max. :3.0000 Max. :40.000 Max. :6.000

WorkLifeBalance YearsAtCompany YearsInCurrentRole Min. :1.000 Min. :
0.000 Min. : 0.000  
1st Qu.:2.000 1st Qu.: 1.000 1st Qu.: 0.000  
Median :3.000 Median : 3.000 Median : 2.000  
Mean :2.658 Mean : 5.131 Mean : 2.903  
3rd Qu.:3.000 3rd Qu.: 7.000 3rd Qu.: 4.000  
Max. :4.000 Max. :40.000 Max. :15.000

YearsSinceLastPromotion YearsWithCurrManager Min. : 0.000 Min. : 0.000  
1st Qu.: 0.000 1st Qu.: 0.000  
Median : 1.000 Median : 2.000  
Mean : 1.945 Mean : 2.852  
3rd Qu.: 2.000 3rd Qu.: 5.000  
Max. :15.000 Max. :14.000

``` r
## Quick glance at attrition data by department
attrition_by_dept <- case_data %>% select(Attrition, Department) %>% group_by(Department) %>% arrange(Department) %>% table()

knitr::kable(attrition_by_dept)
```

|     |  Human Resources|  Research & Development|  Sales|
|-----|----------------:|-----------------------:|------:|
| No  |               51|                     828|    354|
| Yes |               12|                     133|     92|
