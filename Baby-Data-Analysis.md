STAT 420 Analysis Project
================
Spring 2018 JangHyunChoi (jhchoi3), Eldon Hsiao (ehsiao4), Tony Shen (tonys2), Alan Yu (ayu24)
Due: Monday, May 7th by 11:00 AM CT

-   [Predicting a Baby's Health Based on NCHS' Vital Statistics Natality Birth Data](#predicting-a-babys-health-based-on-nchs-vital-statistics-natality-birth-data)
    -   [Introduction](#introduction)
        -   [Statement](#statement)
        -   [Background Information](#background-information)
    -   [Description of the original data set and relevant variables](#description-of-the-original-data-set-and-relevant-variables)
        -   [Description of additional data preparation](#description-of-additional-data-preparation)
    -   [Methods](#methods)
        -   [Procedure](#procedure)
        -   [Decision making process](#decision-making-process)
-   [Results](#results)
    -   [Final Model Selected](#final-model-selected)
        -   [Interpretation of the Final model](#interpretation-of-the-final-model)
    -   [Conclusion](#conclusion)

Predicting a Baby's Health Based on NCHS' Vital Statistics Natality Birth Data
==============================================================================

Introduction
------------

### Statement

The Centers for Disease Control and Prevention (CDC) is one of the main anchors in the Healthcare Industry in the United States. Since the main goal of this institute is to protect and improve public health, the CDC collects alot of data about our healthcare. One specific facet of the healthcare industry we like to explore is about babies and infant mortality.

### Background Information

In this project, we want to create a model that can accurately predict a baby's weight. We believe that a baby's weight at birth is a good indicator of health (the heavier the healthier). The data set is from the Center for Disease Control (CDC). The data was accessed from the National Bureau of Economic Research. Specifically, we used NCHS' Vital Statistics Natality Birth Data (<https://www.nber.org/data/vital-statistics-natality-data.html>). We want to create a model to predict birth weight because it is associated with many later-life conditions, such as diabetes, obesity, intelligence, and neonatal infection.

Description of the original data set and relevant variables
-----------------------------------------------------------

From the data, we choose eight continuous variables and two categorical variables. We chosed these specific predictor variables based on our prior experiences and what we think can be important in explaining a baby's potential weight at birth.

**Response variable:**

-   dbwt - Birth Weight in grams (g)

**Continuous variables:**

-   cig\_0 - number of cigarettes smoked before pregnancy
-   cig\_1 - number of cigarettes during first trimester
-   cig\_2 - number of cigarettes during second trimester
-   cig\_3 - number of cigarettes during third trimester
-   mager - Mothers age (years)
-   bmi - Body mass index of the mother
-   previs - Number of prenatal visits
-   precare - month prenatal care began

**Categorical variables:**

-   meduc - mother's education background

    = 1 8th grade or less

    = 2 9th to 12th grade with no diploma

    = 3 High School Graduate or GED completed

    = 4 Some college credit, but not a degree

    = 5 Associate Degree (AA, AS)

    = 6 Bachelor's Degree (BA, AB, BS)

    = 7 Master's Degree (MA, MS, MEng, MEd, MSW, MBA)

    = 8 Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, LLB, JD)

-   dmeth\_rec - delivery method

    = 1 "Vaginal"

    = 2 "C-Section"

### Description of additional data preparation

To prepare the data, we created a new Excel sheet with the selected predictor variables and the response variable. We used Excel to filter out data points that were reported as unknown or missing. We then parsed and deleted the observations with missing data points. Since the actual dataset is rather large so we limited this study to use 2000 observations. The raw data file can be obtained from (<https://www.nber.org/data/vital-statistics-natality-data.html>) under United States -- Data & Documentation, 2016 Birth Data csv file.

Methods
-------

### Procedure

For our Initial setp up, We will be using the "faraway", "lmtest", "MASS" to do this analysis.

``` r
library(faraway)
```

    ## Warning: package 'faraway' was built under R version 3.4.3

``` r
library(lmtest)
```

    ## Warning: package 'lmtest' was built under R version 3.4.4

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 3.4.4

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(MASS)
```

    ## Warning: package 'MASS' was built under R version 3.4.3

We load the dataset.

``` r
birth_data = data.frame(read.csv("birthdatacleaned2k.csv"))
```

Now we will build the full model with all the predictor variables that we selected from earlier.

``` r
birth_model = lm(dbwt ~ as.factor(meduc) + precare + as.factor(dmeth_rec) + mager + previs + bmi + cig_0 + cig_1 + cig_2 + cig_3, data = birth_data)
summary(birth_model)
```

    ## 
    ## Call:
    ## lm(formula = dbwt ~ as.factor(meduc) + precare + as.factor(dmeth_rec) + 
    ##     mager + previs + bmi + cig_0 + cig_1 + cig_2 + cig_3, data = birth_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2882.48  -304.04    16.29   369.36  1652.11 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           3076.632    173.806  17.702  < 2e-16 ***
    ## as.factor(meduc)2     -147.079    150.351  -0.978 0.328076    
    ## as.factor(meduc)3     -156.345    145.989  -1.071 0.284327    
    ## as.factor(meduc)4     -108.513    146.161  -0.742 0.457918    
    ## as.factor(meduc)5     -125.848    150.960  -0.834 0.404576    
    ## as.factor(meduc)6     -113.613    147.713  -0.769 0.441897    
    ## as.factor(meduc)7     -159.115    152.534  -1.043 0.297010    
    ## as.factor(meduc)8       10.802    174.786   0.062 0.950729    
    ## precare                 22.261      9.603   2.318 0.020537 *  
    ## as.factor(dmeth_rec)2 -101.290     30.036  -3.372 0.000760 ***
    ## mager                    1.902      2.663   0.714 0.475252    
    ## previs                  16.044      3.197   5.019 5.67e-07 ***
    ## bmi                      7.627      1.980   3.852 0.000121 ***
    ## cig_0                   -2.826      3.097  -0.912 0.361708    
    ## cig_1                   -3.270      5.964  -0.548 0.583551    
    ## cig_2                   -5.534     10.687  -0.518 0.604662    
    ## cig_3                   -6.013     11.393  -0.528 0.597698    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 556.4 on 1982 degrees of freedom
    ## Multiple R-squared:  0.04263,    Adjusted R-squared:  0.0349 
    ## F-statistic: 5.515 on 16 and 1982 DF,  p-value: 1.035e-11

Next, we conduct a F-test to test model significance.

The null and alternative hypotheses
*H*<sub>0</sub> : *β*<sub>*a**s*.*f**a**c**t**o**r*(*m**e**d**u**c*)</sub> = *β*<sub>*p**r**e**c**a**r**e*</sub> = ... = *β*<sub>*c**i**g*<sub>3</sub></sub> = 0
*H*<sub>1</sub> : *A**t* *l**e**a**s**t* *o**n**e* *o**f* *β* ≠ 0

The p-value of the test

``` r
f = summary(birth_model)$fstatistic
p = pf(f[1], f[2], f[3], lower.tail = FALSE)
p
```

    ##        value 
    ## 1.034907e-11

-   A statistical decision at *α* = 0.01.

Reject the null hypothesis, the full model is significant, at least 1 beta is significant in explaining weight of the baby.

We need to conduct an individual t-test to see any of the predictor variables are significant. For this test we will reject the null hypothesis at *α* = 0.05. If we reject thet null hypothesis that is to say that the predictor variable is significant in explaining birth weight of a baby.

``` r
round(summary(birth_model)$coefficients[ , "Pr(>|t|)"], 5)
```

    ##           (Intercept)     as.factor(meduc)2     as.factor(meduc)3 
    ##               0.00000               0.32808               0.28433 
    ##     as.factor(meduc)4     as.factor(meduc)5     as.factor(meduc)6 
    ##               0.45792               0.40458               0.44190 
    ##     as.factor(meduc)7     as.factor(meduc)8               precare 
    ##               0.29701               0.95073               0.02054 
    ## as.factor(dmeth_rec)2                 mager                previs 
    ##               0.00076               0.47525               0.00000 
    ##                   bmi                 cig_0                 cig_1 
    ##               0.00012               0.36171               0.58355 
    ##                 cig_2                 cig_3 
    ##               0.60466               0.59770

There are numerous predictor variables that are insignificant in explaining birth weight. Let's make a reduced model by removing insignificant variables that have a p-value greater than .05 and see if there are any improvements.

``` r
birth_redmodel = lm(dbwt ~ . - meduc - mager - cig_0 - cig_1 - cig_2 - cig_3, data = birth_data)
summary(birth_redmodel)
```

    ## 
    ## Call:
    ## lm(formula = dbwt ~ . - meduc - mager - cig_0 - cig_1 - cig_2 - 
    ##     cig_3, data = birth_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2901.39  -309.99    19.43   368.49  1698.81 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 3084.766     77.288  39.913  < 2e-16 ***
    ## precare       17.745      9.489   1.870 0.061632 .  
    ## previs        16.785      3.200   5.246 1.72e-07 ***
    ## bmi            7.508      1.933   3.884 0.000106 ***
    ## dmeth_rec    -95.356     29.745  -3.206 0.001368 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 559.4 on 1994 degrees of freedom
    ## Multiple R-squared:  0.02651,    Adjusted R-squared:  0.02455 
    ## F-statistic: 13.57 on 4 and 1994 DF,  p-value: 6.409e-11

The model is still significant, but the r.squared values went down and "precare" became insignficant. Nevertheless let's conduct a nested models test to see which model we prefer.

``` r
anova(birth_redmodel, birth_model)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: dbwt ~ (mager + meduc + precare + previs + cig_0 + cig_1 + cig_2 + 
    ##     cig_3 + bmi + dmeth_rec) - meduc - mager - cig_0 - cig_1 - 
    ##     cig_2 - cig_3
    ## Model 2: dbwt ~ as.factor(meduc) + precare + as.factor(dmeth_rec) + mager + 
    ##     previs + bmi + cig_0 + cig_1 + cig_2 + cig_3
    ##   Res.Df       RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1994 623914244                                  
    ## 2   1982 613582572 12  10331672 2.7811 0.0009077 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

-   The null and alternative hypotheses.
    *H*<sub>0</sub> : *β*<sub>*a**s*.*f**a**c**t**o**r*(*m**e**d**u**c*2)</sub> = *β*<sub>*p**r**e**c**a**r**e*</sub> = ... = *β*<sub>*c**i**g*<sub>3</sub></sub> = 0
    *H*<sub>1</sub> : *A**t* *l**e**a**s**t* *o**n**e* *o**f* *β* ≠ 0
-   The value of the test statistic.

``` r
anova(birth_redmodel, birth_model)$"F"
```

    ## [1]       NA 2.781122

-   The p-value of the test.

``` r
anova(birth_redmodel, birth_model)$"Pr(>F)"
```

    ## [1]           NA 0.0009076864

-   A statistical decision at *α* = 0.01.

Reject the null hypothesis at alpha = 0.01, The "reduced model" (birth\_redmodel) appears to be most appropriate model in this case.

While the test indicates that the reduced model is the preferred model we need to look into model diagnostics to see if this reduced model is really the "preferred" model.

### Decision making process

One of the most important aspects of regression analysis is verifying that our results are not being influenced by assumption violations. let's check to see if any model assumptions are being violated.

``` r
plot(birth_model)
```

![](Baby-Data-Analysis_files/figure-markdown_github/checking%20model%20assumptions%20for%20the%20fullmodel-1.png)![](Baby-Data-Analysis_files/figure-markdown_github/checking%20model%20assumptions%20for%20the%20fullmodel-2.png)![](Baby-Data-Analysis_files/figure-markdown_github/checking%20model%20assumptions%20for%20the%20fullmodel-3.png)![](Baby-Data-Analysis_files/figure-markdown_github/checking%20model%20assumptions%20for%20the%20fullmodel-4.png)

``` r
vif(birth_model)
```

    ##     as.factor(meduc)2     as.factor(meduc)3     as.factor(meduc)4 
    ##             11.720035             28.286377             27.229206 
    ##     as.factor(meduc)5     as.factor(meduc)6     as.factor(meduc)7 
    ##             10.897087             20.303277             10.171370 
    ##     as.factor(meduc)8               precare as.factor(dmeth_rec)2 
    ##              3.202808              1.173184              1.044518 
    ##                 mager                previs                   bmi 
    ##              1.365209              1.153732              1.086103 
    ##                 cig_0                 cig_1                 cig_2 
    ##              2.712985              4.838006              7.228748 
    ##                 cig_3 
    ##              5.828093

``` r
bptest(birth_model)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  birth_model
    ## BP = 77.521, df = 16, p-value = 4.641e-10

``` r
shapiro.test(sample(resid(birth_model), 1500))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sample(resid(birth_model), 1500)
    ## W = 0.97021, p-value < 2.2e-16

We will output various graphs to check for any violations in our model. To check if the errors follow a normal distribution we use the shapiro-wilk test and Q-Q plots. To check the if the assumption of constant variance of errors (also known as homoscedasticity) we use the Breuch Pagan test (we will refer to it as "bptest" in this report) and Residual plots. We will check for unnecessary outliers using cooks.distance. And Finally we will check for serious multicollinearity using variance inflation factor (we will refer to it as "vif" in this report).

The p-value for the Breuch Pagan test is low enough that we reject *H*<sub>0</sub> so we believe that the constant variance assumption has been violated.

The shapiro test is rejected so we believe that the normality assumption has been violated.

``` r
pairs(birth_data, panel = panel.smooth)
```

![](Baby-Data-Analysis_files/figure-markdown_github/scatterplot%20matrices-1.png)

The scatterplot matrix is very small but just from eye balling the plots, there are predictor variables showing signs of multicollinearity that vif has identified from before. We can also see the general trend of each variable as well.

The variable with the highest value is as.factor(meduc)4, Values greater than 5 means multicollinearity is present. "cig\_2" and "cig\_3" as well as many of the levels of "meduc" could be a cause for concern. So let's get rid of "cig\_2", "cig\_3" and "meduc" to see if there is still any multicollinearity concerns.

Model that has "cig\_2", "cig\_3" and "meduc" removed.

``` r
birth_model2 = lm(dbwt ~ . - cig_2 - cig_3 - meduc, data = birth_data)
summary(birth_model2)
```

    ## 
    ## Call:
    ## lm(formula = dbwt ~ . - cig_2 - cig_3 - meduc, data = birth_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2908.59  -303.74    11.88   363.95  1659.50 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 3030.069     95.426  31.753  < 2e-16 ***
    ## mager          2.862      2.344   1.221 0.222130    
    ## precare       21.951      9.521   2.305 0.021243 *  
    ## previs        16.409      3.183   5.155 2.78e-07 ***
    ## cig_0         -3.039      3.085  -0.985 0.324661    
    ## cig_1         -9.401      4.456  -2.110 0.035023 *  
    ## bmi            7.251      1.937   3.742 0.000187 ***
    ## dmeth_rec   -102.605     29.923  -3.429 0.000618 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 556.3 on 1991 degrees of freedom
    ## Multiple R-squared:  0.03873,    Adjusted R-squared:  0.03535 
    ## F-statistic: 11.46 on 7 and 1991 DF,  p-value: 2.48e-14

The model is still significant based on the low p-value.

Let's check the assumptions again

``` r
plot(birth_model2)
```

![](Baby-Data-Analysis_files/figure-markdown_github/checking%20assumptions%20for%20model2-1.png)![](Baby-Data-Analysis_files/figure-markdown_github/checking%20assumptions%20for%20model2-2.png)![](Baby-Data-Analysis_files/figure-markdown_github/checking%20assumptions%20for%20model2-3.png)![](Baby-Data-Analysis_files/figure-markdown_github/checking%20assumptions%20for%20model2-4.png)

``` r
bptest(birth_model2)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  birth_model2
    ## BP = 67.822, df = 7, p-value = 4.066e-12

``` r
shapiro.test(sample(resid(birth_model2), 1500))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sample(resid(birth_model2), 1500)
    ## W = 0.96333, p-value < 2.2e-16

``` r
vif(birth_model2)
```

    ##     mager   precare    previs     cig_0     cig_1       bmi dmeth_rec 
    ##  1.057967  1.153970  1.144224  2.693260  2.702470  1.040279  1.037194

The vif for the predictor variables are all lower than 5. Ideally we would want all vifs to be 1, so that would mean no predictors are correlated. The assumption of no multicollinearity is met. The assumption constant variance and normality of errors are still violated. Now, we should also look for unsual observations that have high leverage.

``` r
birth_model_lev = hatvalues(birth_model2)
sum(birth_model_lev > 2 * mean(birth_model_lev))
```

    ## [1] 155

There are 155 observations that can be considered high leverage. They have a high potential to influence model fit.

We also need to check for influential observations.

``` r
birth_model_cook = cooks.distance(birth_model2)
sum(birth_model_cook > 4 / length(birth_model_cook))
```

    ## [1] 120

There are 120 observations that can be considered influential.

Now lets make a model without any points we've identified as influential.

``` r
birth_model_noinf = lm(dbwt ~ . - cig_2 - cig_3 - meduc, data = birth_data, subset = birth_model_cook <= 4 / length(birth_model_cook))

summary(birth_model_noinf)
```

    ## 
    ## Call:
    ## lm(formula = dbwt ~ . - cig_2 - cig_3 - meduc, data = birth_data, 
    ##     subset = birth_model_cook <= 4/length(birth_model_cook))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1543.71  -301.43    -0.66   321.57  1516.93 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2977.6742    84.2559  35.341  < 2e-16 ***
    ## mager          4.3272     2.0139   2.149  0.03179 *  
    ## precare       17.9035     8.2614   2.167  0.03035 *  
    ## previs        14.1380     2.9095   4.859 1.28e-06 ***
    ## cig_0         -0.3959     3.9999  -0.099  0.92117    
    ## cig_1        -14.5741     5.3876  -2.705  0.00689 ** 
    ## bmi            7.8069     1.7419   4.482 7.85e-06 ***
    ## dmeth_rec    -53.3494    25.7510  -2.072  0.03843 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 457.1 on 1871 degrees of freedom
    ## Multiple R-squared:  0.04603,    Adjusted R-squared:  0.04246 
    ## F-statistic:  12.9 on 7 and 1871 DF,  p-value: 2.774e-16

This model is still significant, r-squared value also increased compared to the previous model.

Let's now check the assumptions for the influential model we created from earlier.

``` r
plot(birth_model_noinf)
```

![](Baby-Data-Analysis_files/figure-markdown_github/check%20assumptions%20for%20influential%20model-1.png)![](Baby-Data-Analysis_files/figure-markdown_github/check%20assumptions%20for%20influential%20model-2.png)![](Baby-Data-Analysis_files/figure-markdown_github/check%20assumptions%20for%20influential%20model-3.png)![](Baby-Data-Analysis_files/figure-markdown_github/check%20assumptions%20for%20influential%20model-4.png)

``` r
bptest(birth_model_noinf)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  birth_model_noinf
    ## BP = 17.729, df = 7, p-value = 0.01325

``` r
shapiro.test(sample(resid(birth_model_noinf), 1500))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sample(resid(birth_model_noinf), 1500)
    ## W = 0.99876, p-value = 0.3754

``` r
vif(birth_model_noinf)
```

    ##     mager   precare    previs     cig_0     cig_1       bmi dmeth_rec 
    ##  1.057867  1.174117  1.163593  3.739476  3.754604  1.030859  1.031288

When we got rid of the influential variables, p-value for the bptest is high enough that we do not reject the null hypothesis at *α* = 0.01, the errors have constant variance. The residual plots shows alot of points concentrated in a certain area but the mean is close to zero and there is no "cone" shaped present. The shapiro test also has a high p-value so we do not reject the null hypothesis, the data could have been sampled from a normal distirbution. The noraml Q-Q plot also looks like a perfectly straight line which agrees with the assumption of normality of errors. The vif are all below 5 so there is not serious signs of multicollinearity. The cooks distance is also a straight line so that is a good sign.

We cannot do an anova test since the influential model has reduced number of observations. We can use "leave one out cross validation root mean squared error" or "LOOCV RMSE" to measure how well our model predicts.

``` r
get_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}
```

``` r
get_loocv_rmse(birth_model_noinf)
```

    ## [1] 457.8615

``` r
get_loocv_rmse(birth_model)
```

    ## [1] 558.9924

Using LOOCV RMSE, we prefer the model with no influential variables ("birth\_model\_noinf").

``` r
rmse = sqrt(mean(resid(birth_model_noinf) ^ 2))
rmse
```

    ## [1] 456.1712

``` r
rmse = sqrt(mean(resid(birth_model) ^ 2))
rmse
```

    ## [1] 554.026

``` r
summary(birth_model_noinf)$adj.r.squared
```

    ## [1] 0.04246445

``` r
summary(birth_model)$adj.r.squared
```

    ## [1] 0.03489718

Looking at just the root mean squared errors (RMSE), we see the values are similar. The adjusted R squared is also an improvement over our original model. Even though the error values are quite large, it is an improvement over the original full model.

``` r
summary(birth_model_noinf)
```

    ## 
    ## Call:
    ## lm(formula = dbwt ~ . - cig_2 - cig_3 - meduc, data = birth_data, 
    ##     subset = birth_model_cook <= 4/length(birth_model_cook))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1543.71  -301.43    -0.66   321.57  1516.93 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2977.6742    84.2559  35.341  < 2e-16 ***
    ## mager          4.3272     2.0139   2.149  0.03179 *  
    ## precare       17.9035     8.2614   2.167  0.03035 *  
    ## previs        14.1380     2.9095   4.859 1.28e-06 ***
    ## cig_0         -0.3959     3.9999  -0.099  0.92117    
    ## cig_1        -14.5741     5.3876  -2.705  0.00689 ** 
    ## bmi            7.8069     1.7419   4.482 7.85e-06 ***
    ## dmeth_rec    -53.3494    25.7510  -2.072  0.03843 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 457.1 on 1871 degrees of freedom
    ## Multiple R-squared:  0.04603,    Adjusted R-squared:  0.04246 
    ## F-statistic:  12.9 on 7 and 1871 DF,  p-value: 2.774e-16

Looking back at our model with influential variables we removed there is a predictor variable that is insignificant (cig\_0)

let's remove "cig\_0" and see if there are anymore improvements to the model.

``` r
birth_redmodel_noinf = lm(dbwt ~ . - cig_0 - cig_2 - cig_3 - meduc, data = birth_data, subset = birth_model_cook <= 4 / length(birth_model_cook))

summary(birth_redmodel_noinf)
```

    ## 
    ## Call:
    ## lm(formula = dbwt ~ . - cig_0 - cig_2 - cig_3 - meduc, data = birth_data, 
    ##     subset = birth_model_cook <= 4/length(birth_model_cook))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1543.59  -300.76    -0.51   321.75  1517.12 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2977.184     84.088  35.406  < 2e-16 ***
    ## mager          4.341      2.009   2.161   0.0308 *  
    ## precare       17.936      8.253   2.173   0.0299 *  
    ## previs        14.133      2.908   4.860 1.27e-06 ***
    ## cig_1        -15.030      2.799  -5.370 8.83e-08 ***
    ## bmi            7.804      1.741   4.482 7.84e-06 ***
    ## dmeth_rec    -53.375     25.743  -2.073   0.0383 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 457 on 1872 degrees of freedom
    ## Multiple R-squared:  0.04603,    Adjusted R-squared:  0.04297 
    ## F-statistic: 15.05 on 6 and 1872 DF,  p-value: < 2.2e-16

Check our assumptions again

``` r
plot(birth_redmodel_noinf)
```

![](Baby-Data-Analysis_files/figure-markdown_github/reduced%20model%20assumptions-1.png)![](Baby-Data-Analysis_files/figure-markdown_github/reduced%20model%20assumptions-2.png)![](Baby-Data-Analysis_files/figure-markdown_github/reduced%20model%20assumptions-3.png)![](Baby-Data-Analysis_files/figure-markdown_github/reduced%20model%20assumptions-4.png)

``` r
bptest(birth_redmodel_noinf)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  birth_redmodel_noinf
    ## BP = 16.191, df = 6, p-value = 0.01277

``` r
shapiro.test(sample(resid(birth_redmodel_noinf), 1500))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sample(resid(birth_redmodel_noinf), 1500)
    ## W = 0.99859, p-value = 0.26

``` r
vif(birth_redmodel_noinf)
```

    ##     mager   precare    previs     cig_1       bmi dmeth_rec 
    ##  1.052936  1.172258  1.163238  1.013620  1.030617  1.031187

The plots look very similar to our preferred model, "birth\_model\_noinf". The vif are all close to 1, so there is no sign of multicollinearity. The p-value for the Shapiro-Wilk test is high so we do not reject the null; the population is normally distributed. The Breusch-Pagan test also has a high enough p-value that we do not reject the null hypothesis at *a**l**p**h**a* = 0.01, the residuals seem to have an approximately equal "spread" around the regression line.

We should also conduct a nested models test to see if the variables removed in "birth\_redmodel\_oninf" is better than "birth\_model\_noinf" we had from earlier.

-   The null and alternative hypotheses.
    *H*<sub>0</sub> : *β*<sub>*c**i**g*<sub>0</sub></sub> = *β*<sub>*p**r**e**c**a**r**e*</sub> = ... = *β*<sub>*c**i**g*<sub>3</sub></sub> = 0
    *H*<sub>1</sub> : *A**t* *l**e**a**s**t* *o**n**e* *o**f* *β* ≠ 0
-   The value of the test statistic.

``` r
anova(birth_redmodel_noinf, birth_model_noinf)$"F"
```

    ## [1]          NA 0.009794589

-   The p-value of the test.

``` r
anova(birth_redmodel_noinf, birth_model_noinf)$"Pr(>F)"
```

    ## [1]        NA 0.9211746

-   A statistical decision at *α* = 0.01. Do not reject the null hypothesis at alpha = 0.01, The "full model" (birth\_model\_noinf) appears to be most appropriate model in this case.

We can also try other models to see if it is better than "birth\_model\_noinf". We will use Akaike information criterion (AIC) as a means for model selection.

``` r
birth_aicmodel = stepAIC(birth_model, subset = birth_model_cook <= 4 / length(birth_model_cook), direction = "both")
```

    ## Start:  AIC=25290.21
    ## dbwt ~ as.factor(meduc) + precare + as.factor(dmeth_rec) + mager + 
    ##     previs + bmi + cig_0 + cig_1 + cig_2 + cig_3
    ## 
    ##                        Df Sum of Sq       RSS   AIC
    ## - as.factor(meduc)      7   1743270 615325841 25282
    ## - cig_2                 1     83000 613665572 25289
    ## - cig_3                 1     86239 613668811 25289
    ## - cig_1                 1     93068 613675639 25289
    ## - mager                 1    157863 613740435 25289
    ## - cig_0                 1    257673 613840245 25289
    ## <none>                              613582572 25290
    ## - precare               1   1663747 615246319 25294
    ## - as.factor(dmeth_rec)  1   3520689 617103261 25300
    ## - bmi                   1   4593480 618176051 25303
    ## - previs                1   7796993 621379564 25314
    ## 
    ## Step:  AIC=25281.88
    ## dbwt ~ precare + as.factor(dmeth_rec) + mager + previs + bmi + 
    ##     cig_0 + cig_1 + cig_2 + cig_3
    ## 
    ##                        Df Sum of Sq       RSS   AIC
    ## - cig_3                 1     76176 615402017 25280
    ## - cig_1                 1     94272 615420114 25280
    ## - cig_2                 1    112857 615438698 25280
    ## - cig_0                 1    295873 615621714 25281
    ## - mager                 1    460078 615785919 25281
    ## <none>                              615325841 25282
    ## - precare               1   1684946 617010787 25285
    ## + as.factor(meduc)      7   1743270 613582572 25290
    ## - as.factor(dmeth_rec)  1   3717036 619042877 25292
    ## - bmi                   1   4354145 619679986 25294
    ## - previs                1   8147974 623473815 25306
    ## 
    ## Step:  AIC=25280.13
    ## dbwt ~ precare + as.factor(dmeth_rec) + mager + previs + bmi + 
    ##     cig_0 + cig_1 + cig_2
    ## 
    ##                        Df Sum of Sq       RSS   AIC
    ## - cig_1                 1    117770 615519787 25279
    ## - cig_0                 1    284323 615686340 25279
    ## - mager                 1    457077 615859094 25280
    ## <none>                              615402017 25280
    ## - cig_2                 1    675881 616077898 25280
    ## + cig_3                 1     76176 615325841 25282
    ## - precare               1   1709014 617111031 25284
    ## + as.factor(meduc)      7   1733206 613668811 25289
    ## - as.factor(dmeth_rec)  1   3735877 619137894 25290
    ## - bmi                   1   4348125 619750142 25292
    ## - previs                1   8152267 623554284 25304
    ## 
    ## Step:  AIC=25278.51
    ## dbwt ~ precare + as.factor(dmeth_rec) + mager + previs + bmi + 
    ##     cig_0 + cig_2
    ## 
    ##                        Df Sum of Sq       RSS   AIC
    ## - mager                 1    444947 615964733 25278
    ## <none>                              615519787 25279
    ## - cig_0                 1    817189 616336975 25279
    ## + cig_1                 1    117770 615402017 25280
    ## + cig_3                 1     99673 615420114 25280
    ## - precare               1   1666865 617186652 25282
    ## - cig_2                 1   1935120 617454907 25283
    ## + as.factor(meduc)      7   1732876 613786910 25287
    ## - as.factor(dmeth_rec)  1   3772427 619292214 25289
    ## - bmi                   1   4375270 619895057 25291
    ## - previs                1   8128307 623648094 25303
    ## 
    ## Step:  AIC=25277.96
    ## dbwt ~ precare + as.factor(dmeth_rec) + previs + bmi + cig_0 + 
    ##     cig_2
    ## 
    ##                        Df Sum of Sq       RSS   AIC
    ## <none>                              615964733 25278
    ## + mager                 1    444947 615519787 25279
    ## - cig_0                 1    892353 616857086 25279
    ## + cig_1                 1    105639 615859094 25280
    ## + cig_3                 1     94941 615869793 25280
    ## - precare               1   1531303 617496036 25281
    ## - cig_2                 1   1901892 617866625 25282
    ## + as.factor(meduc)      7   2030974 613933759 25285
    ## - as.factor(dmeth_rec)  1   3471838 619436572 25287
    ## - bmi                   1   4795203 620759936 25292
    ## - previs                1   8155420 624120154 25302

``` r
plot(birth_aicmodel)
```

![](Baby-Data-Analysis_files/figure-markdown_github/check%20aic%20assumptions-1.png)![](Baby-Data-Analysis_files/figure-markdown_github/check%20aic%20assumptions-2.png)![](Baby-Data-Analysis_files/figure-markdown_github/check%20aic%20assumptions-3.png)![](Baby-Data-Analysis_files/figure-markdown_github/check%20aic%20assumptions-4.png)

``` r
bptest(birth_aicmodel)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  birth_aicmodel
    ## BP = 66.941, df = 6, p-value = 1.73e-12

``` r
shapiro.test(sample(resid(birth_aicmodel), 1500))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sample(resid(birth_aicmodel), 1500)
    ## W = 0.97851, p-value = 3.455e-14

``` r
vif(birth_aicmodel)
```

    ##               precare as.factor(dmeth_rec)2                previs 
    ##              1.143084              1.014231              1.144226 
    ##                   bmi                 cig_0                 cig_2 
    ##              1.024390              1.768630              1.783149

The aic model has no predictor variables with multicollinearity. However, the assumptions about constant variance and normality is violated based on the low p-values as shown in the bptest and the shapiro test.

We could also try to create the largest model and use backwards Akaike Information Criterion to possible find a good model.

``` r
birth_hugemodel = lm(dbwt ~ . ^ 2 + I(mager ^ 2) + I(precare ^ 2) + I(previs ^ 2) + I(cig_0 ^ 2) +I(cig_1 ^ 2) + I(as.factor(meduc ^ 2)) + I(as.factor(dmeth_rec ^ 2)) + I(bmi ^ 2), data = birth_data, subset = birth_model_cook <= 4 / length(birth_model_cook))

fit_aic = step(birth_hugemodel, direction = "backward", trace = 0)
summary(fit_aic)
```

    ## 
    ## Call:
    ## lm(formula = dbwt ~ mager + meduc + precare + previs + cig_1 + 
    ##     cig_2 + cig_3 + bmi + dmeth_rec + I(mager^2) + I(previs^2) + 
    ##     I(cig_1^2) + I(bmi^2) + mager:previs + mager:bmi + mager:dmeth_rec + 
    ##     meduc:bmi + meduc:dmeth_rec + precare:cig_1 + precare:cig_2 + 
    ##     previs:cig_1 + previs:cig_2 + cig_2:cig_3 + bmi:dmeth_rec, 
    ##     data = birth_data, subset = birth_model_cook <= 4/length(birth_model_cook))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1590.73  -297.73     0.32   321.08  1522.67 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     2737.5825   384.1025   7.127 1.46e-12 ***
    ## mager             23.8894    19.9026   1.200 0.230172    
    ## meduc            -21.2804    40.0673  -0.531 0.595401    
    ## precare           17.5346     8.7336   2.008 0.044820 *  
    ## previs             4.4054    16.2257   0.272 0.786031    
    ## cig_1            -44.0943    30.4981  -1.446 0.148401    
    ## cig_2             37.4972    38.8707   0.965 0.334837    
    ## cig_3            -36.5297    15.6804  -2.330 0.019932 *  
    ## bmi               22.4972    13.7994   1.630 0.103208    
    ## dmeth_rec       -286.8557   177.4190  -1.617 0.106086    
    ## I(mager^2)        -0.5893     0.3155  -1.868 0.061934 .  
    ## I(previs^2)       -0.5192     0.3522  -1.474 0.140629    
    ## I(cig_1^2)        -0.7760     0.3768  -2.060 0.039579 *  
    ## I(bmi^2)          -0.5595     0.2032  -2.754 0.005952 ** 
    ## mager:previs       0.7850     0.4930   1.592 0.111489    
    ## mager:bmi          0.6235     0.3749   1.663 0.096466 .  
    ## mager:dmeth_rec   -9.7339     5.4763  -1.777 0.075654 .  
    ## meduc:bmi         -2.0695     1.2975  -1.595 0.110877    
    ## meduc:dmeth_rec   68.0885    18.3505   3.710 0.000213 ***
    ## precare:cig_1      9.7140     4.5249   2.147 0.031938 *  
    ## precare:cig_2     -7.9768     5.4542  -1.463 0.143770    
    ## previs:cig_1       2.7683     1.7140   1.615 0.106471    
    ## previs:cig_2      -3.2537     2.1886  -1.487 0.137279    
    ## cig_2:cig_3        2.2877     0.8959   2.553 0.010745 *  
    ## bmi:dmeth_rec      7.8139     4.1054   1.903 0.057153 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 453.2 on 1854 degrees of freedom
    ## Multiple R-squared:  0.0709, Adjusted R-squared:  0.05888 
    ## F-statistic: 5.895 on 24 and 1854 DF,  p-value: < 2.2e-16

Check for model assumptions again.

``` r
plot(fit_aic)
```

![](Baby-Data-Analysis_files/figure-markdown_github/huge%20aic%20model%20assumptions-1.png)![](Baby-Data-Analysis_files/figure-markdown_github/huge%20aic%20model%20assumptions-2.png)![](Baby-Data-Analysis_files/figure-markdown_github/huge%20aic%20model%20assumptions-3.png)![](Baby-Data-Analysis_files/figure-markdown_github/huge%20aic%20model%20assumptions-4.png)

``` r
bptest(fit_aic)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  fit_aic
    ## BP = 33.721, df = 24, p-value = 0.08975

``` r
shapiro.test(sample(resid(fit_aic), 1500))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  sample(resid(fit_aic), 1500)
    ## W = 0.9986, p-value = 0.2677

``` r
vif(fit_aic)
```

    ##           mager           meduc         precare          previs 
    ##      105.121280       34.740696        1.335081       36.820698 
    ##           cig_1           cig_2           cig_3             bmi 
    ##      122.411126      100.404972       10.782612       65.826524 
    ##       dmeth_rec      I(mager^2)     I(previs^2)      I(cig_1^2) 
    ##       49.808076       86.533753       11.019370       10.863888 
    ##        I(bmi^2)    mager:previs       mager:bmi mager:dmeth_rec 
    ##       52.630267       36.505984       76.117993       60.630300 
    ##       meduc:bmi meduc:dmeth_rec   precare:cig_1   precare:cig_2 
    ##       32.454832       25.081568       47.442318       35.982851 
    ##    previs:cig_1    previs:cig_2     cig_2:cig_3   bmi:dmeth_rec 
    ##       41.939871       34.817443        7.497697       32.489656

As with the previous model, there is still violations in assumptions so this is not a good model in explaining birth weight of a baby.

Results
=======

Final Model Selected
--------------------

Our final model is

``` r
summary(birth_model_noinf)$coefficients[,1]
```

    ##  (Intercept)        mager      precare       previs        cig_0 
    ## 2977.6741535    4.3271506   17.9034503   14.1379733   -0.3958644 
    ##        cig_1          bmi    dmeth_rec 
    ##  -14.5740844    7.8068810  -53.3494060

### Interpretation of the Final model

*β*<sub>0</sub>intercept = 2977.6742 is the estimated weight of a baby at birth with 0 mager(years), precare, previs, cig\_0, cig\_1, bmi and dmeth\_rec.

*β*<sub>1</sub>mager = 4.3272 is the estimated change in mean weight of a baby at birth for a 1 year increase in mother's age with a certain precare, previs, cig\_0, cig\_1, bmi, dmeth\_rec.

*β*<sub>2</sub>precare = 17.9035 is the estimated change in mean weight of a baby at birth for a 1 month increase in length of prenatal care with a certain mager, previs, cig\_0, cig\_1, bmi, dmeth\_rec.

*β*<sub>3</sub>previs = 14.1380 is the estimated change in mean weight of a baby at birth for 1 increase in number of prenatal visits with a certain mager, precare, cig\_0, cig\_1, bmi, dmeth\_rec.

*β*<sub>4</sub>cig\_0 = −.3959 is the estimated change in mean weight of a baby at birth for 1 increase in number of cigarettes smoked before pregnancy with a certain mager, precare, previs, cig\_1, bmi, dmeth\_rec.

*β*<sub>5</sub>cig\_1 = −14.5741 is the estimated change in mean weight of a baby at birth for 1 increase in body mass index with a certain mager, precare, previs, cig\_0, bmi, dmeth\_rec.

*β*<sub>6</sub>bmi = 7.8069 is the estimated change in mean weight of a baby at birth for 1 increase in number of cigarettes smoked in the 1st trimester with a certain mager, precare, previs, cig\_0, cig\_1, dmeth\_rec.

*β*<sub>7</sub>dmeth\_rec = −53.3494 is the estimated change in mean weight of a baby at birth if method of delivery is "C-section" with a certain mager, precare, previs, cig\_0, cig\_1, bmi.

We can also plot the data to see if there are any individual relationships the predictor variables have with the response variable

``` r
par(mfrow = c(2,4))
plot(dbwt ~ mager, data = birth_data)
plot(dbwt ~ precare, data = birth_data)
plot(dbwt ~ previs, data = birth_data)
plot(dbwt ~ cig_0, data = birth_data)
plot(dbwt ~ cig_1, data = birth_data)
plot(dbwt ~ bmi, data = birth_data)
plot(dbwt ~ dmeth_rec, data = birth_data)
```

![](Baby-Data-Analysis_files/figure-markdown_github/scatter%20plot%20of%20each%20variable-1.png)

Looking at the scatter plots there does not seem to be any definite positive or negative relationship any between the predictor variables and response variables but there seems to be an upward trend.AA

Conclusion
----------

The results were somewhat to be expected. Since this is dealing with real life data, there is no definitive or obvious pattern in the data so our measures and r squared values may seem inaccurate and low; our dataset is not very "clean". As seen in our final model, Cigarettes smoked before and during pregnancy is known to negatively impact not only the health of a baby but also the mother. "precare" and "previs" are all positively correlated with birth weight since, the more doctors are able to monitor the mother's health, the better the babies health. In terms of mother's age, we believe that as the mother gets older, they become more mature and make better decisions about their health so older mother's generally have healthier babies. The coefficient for "dmeth\_rec" seems to indicate that having a C-section is bad for the baby. We were suprised that mother's education was not significant since we believed that education background is related to making better health decisions. In the future, it might be interesting to explore this dataset using more [advanced methods of analysis](https://en.wikipedia.org/wiki/Statistical_learning_theory) to find a better prediction model and using a larger subset of the data.
