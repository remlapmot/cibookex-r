# 13. Standardization and the parametric G-formula{-}

## Program 13.1

- Estimating the mean outcome within levels of treatment and confounders
- Data from NHEFS


```r
library(here)
```


```r
#install.packages("readxl") # install package if required
library("readxl")
nhefs <- read_excel(here("data", "NHEFS.xls"))

# some preprocessing of the data
nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)

fit <-
  glm(
    wt82_71 ~ qsmk + sex + race + age + I(age * age) + as.factor(education)
    + smokeintensity + I(smokeintensity * smokeintensity) + smokeyrs
    + I(smokeyrs * smokeyrs) + as.factor(exercise) + as.factor(active)
    + wt71 + I(wt71 * wt71) + qsmk * smokeintensity,
    data = nhefs
  )
summary(fit)
```

```
## 
## Call:
## glm(formula = wt82_71 ~ qsmk + sex + race + age + I(age * age) + 
##     as.factor(education) + smokeintensity + I(smokeintensity * 
##     smokeintensity) + smokeyrs + I(smokeyrs * smokeyrs) + as.factor(exercise) + 
##     as.factor(active) + wt71 + I(wt71 * wt71) + qsmk * smokeintensity, 
##     data = nhefs)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -42.056   -4.171   -0.343    3.891   44.606  
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        -1.5881657  4.3130359  -0.368 0.712756    
## qsmk                                2.5595941  0.8091486   3.163 0.001590 ** 
## sex                                -1.4302717  0.4689576  -3.050 0.002328 ** 
## race                                0.5601096  0.5818888   0.963 0.335913    
## age                                 0.3596353  0.1633188   2.202 0.027809 *  
## I(age * age)                       -0.0061010  0.0017261  -3.534 0.000421 ***
## as.factor(education)2               0.7904440  0.6070005   1.302 0.193038    
## as.factor(education)3               0.5563124  0.5561016   1.000 0.317284    
## as.factor(education)4               1.4915695  0.8322704   1.792 0.073301 .  
## as.factor(education)5              -0.1949770  0.7413692  -0.263 0.792589    
## smokeintensity                      0.0491365  0.0517254   0.950 0.342287    
## I(smokeintensity * smokeintensity) -0.0009907  0.0009380  -1.056 0.291097    
## smokeyrs                            0.1343686  0.0917122   1.465 0.143094    
## I(smokeyrs * smokeyrs)             -0.0018664  0.0015437  -1.209 0.226830    
## as.factor(exercise)1                0.2959754  0.5351533   0.553 0.580298    
## as.factor(exercise)2                0.3539128  0.5588587   0.633 0.526646    
## as.factor(active)1                 -0.9475695  0.4099344  -2.312 0.020935 *  
## as.factor(active)2                 -0.2613779  0.6845577  -0.382 0.702647    
## wt71                                0.0455018  0.0833709   0.546 0.585299    
## I(wt71 * wt71)                     -0.0009653  0.0005247  -1.840 0.066001 .  
## qsmk:smokeintensity                 0.0466628  0.0351448   1.328 0.184463    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 53.5683)
## 
##     Null deviance: 97176  on 1565  degrees of freedom
## Residual deviance: 82763  on 1545  degrees of freedom
##   (63 observations deleted due to missingness)
## AIC: 10701
## 
## Number of Fisher Scoring iterations: 2
```

```r
nhefs$predicted.meanY <- predict(fit, nhefs)

nhefs[which(nhefs$seqn == 24770), c(
  "predicted.meanY",
  "qsmk",
  "sex",
  "race",
  "age",
  "education",
  "smokeintensity",
  "smokeyrs",
  "exercise",
  "active",
  "wt71"
)]
```

```
## # A tibble: 1 x 11
##   predicted.meanY  qsmk   sex  race   age education smokeintensity smokeyrs
##             <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>          <dbl>    <dbl>
## 1           0.342     0     0     0    26         4             15       12
## # ... with 3 more variables: exercise <dbl>, active <dbl>, wt71 <dbl>
```

```r
summary(nhefs$predicted.meanY[nhefs$cens == 0])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -10.876   1.116   3.042   2.638   4.511   9.876
```

```r
summary(nhefs$wt82_71[nhefs$cens == 0])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -41.280  -1.478   2.604   2.638   6.690  48.538
```

## Program 13.2

- Standardizing the mean outcome to the baseline confounders
- Data from Table 2.2


```r
id <- c(
  "Rheia",
  "Kronos",
  "Demeter",
  "Hades",
  "Hestia",
  "Poseidon",
  "Hera",
  "Zeus",
  "Artemis",
  "Apollo",
  "Leto",
  "Ares",
  "Athena",
  "Hephaestus",
  "Aphrodite",
  "Cyclope",
  "Persephone",
  "Hermes",
  "Hebe",
  "Dionysus"
)
N <- length(id)
L <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
A <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
Y <- c(0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)
interv <- rep(-1, N)
observed <- cbind(L, A, Y, interv)
untreated <- cbind(L, rep(0, N), rep(NA, N), rep(0, N))
treated <- cbind(L, rep(1, N), rep(NA, N), rep(1, N))
table22 <- as.data.frame(rbind(observed, untreated, treated))
table22$id <- rep(id, 3)

glm.obj <- glm(Y ~ A * L, data = table22)
summary(glm.obj)
```

```
## 
## Call:
## glm(formula = Y ~ A * L, data = table22)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.66667  -0.25000   0.04167   0.33333   0.75000  
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)  2.500e-01  2.552e-01   0.980    0.342
## A           -4.164e-16  3.608e-01   0.000    1.000
## L            4.167e-01  3.898e-01   1.069    0.301
## A:L          3.237e-16  4.959e-01   0.000    1.000
## 
## (Dispersion parameter for gaussian family taken to be 0.2604167)
## 
##     Null deviance: 5.0000  on 19  degrees of freedom
## Residual deviance: 4.1667  on 16  degrees of freedom
##   (40 observations deleted due to missingness)
## AIC: 35.385
## 
## Number of Fisher Scoring iterations: 2
```

```r
table22$predicted.meanY <- predict(glm.obj, table22)

mean(table22$predicted.meanY[table22$interv == -1])
```

```
## [1] 0.5
```

```r
mean(table22$predicted.meanY[table22$interv == 0])
```

```
## [1] 0.5
```

```r
mean(table22$predicted.meanY[table22$interv == 1])
```

```
## [1] 0.5
```


## Program 13.3

- Standardizing the mean outcome to the baseline confounders:
- Data from NHEFS


```r
# create a dataset with 3 copies of each subject
nhefs$interv <- -1 # 1st copy: equal to original one

interv0 <- nhefs # 2nd copy: treatment set to 0, outcome to missing
interv0$interv <- 0
interv0$qsmk <- 0
interv0$wt82_71 <- NA

interv1 <- nhefs # 3rd copy: treatment set to 1, outcome to missing
interv1$interv <- 1
interv1$qsmk <- 1
interv1$wt82_71 <- NA

onesample <- rbind(nhefs, interv0, interv1) # combining datasets

# linear model to estimate mean outcome conditional on treatment and confounders
# parameters are estimated using original observations only (nhefs)
# parameter estimates are used to predict mean outcome for observations with
# treatment set to 0 (interv=0) and to 1 (interv=1)

std <- glm(
  wt82_71 ~ qsmk + sex + race + age + I(age * age)
  + as.factor(education) + smokeintensity
  + I(smokeintensity * smokeintensity) + smokeyrs
  + I(smokeyrs * smokeyrs) + as.factor(exercise)
  + as.factor(active) + wt71 + I(wt71 * wt71) + I(qsmk * smokeintensity),
  data = onesample
)
summary(std)
```

```
## 
## Call:
## glm(formula = wt82_71 ~ qsmk + sex + race + age + I(age * age) + 
##     as.factor(education) + smokeintensity + I(smokeintensity * 
##     smokeintensity) + smokeyrs + I(smokeyrs * smokeyrs) + as.factor(exercise) + 
##     as.factor(active) + wt71 + I(wt71 * wt71) + I(qsmk * smokeintensity), 
##     data = onesample)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -42.056   -4.171   -0.343    3.891   44.606  
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        -1.5881657  4.3130359  -0.368 0.712756    
## qsmk                                2.5595941  0.8091486   3.163 0.001590 ** 
## sex                                -1.4302717  0.4689576  -3.050 0.002328 ** 
## race                                0.5601096  0.5818888   0.963 0.335913    
## age                                 0.3596353  0.1633188   2.202 0.027809 *  
## I(age * age)                       -0.0061010  0.0017261  -3.534 0.000421 ***
## as.factor(education)2               0.7904440  0.6070005   1.302 0.193038    
## as.factor(education)3               0.5563124  0.5561016   1.000 0.317284    
## as.factor(education)4               1.4915695  0.8322704   1.792 0.073301 .  
## as.factor(education)5              -0.1949770  0.7413692  -0.263 0.792589    
## smokeintensity                      0.0491365  0.0517254   0.950 0.342287    
## I(smokeintensity * smokeintensity) -0.0009907  0.0009380  -1.056 0.291097    
## smokeyrs                            0.1343686  0.0917122   1.465 0.143094    
## I(smokeyrs * smokeyrs)             -0.0018664  0.0015437  -1.209 0.226830    
## as.factor(exercise)1                0.2959754  0.5351533   0.553 0.580298    
## as.factor(exercise)2                0.3539128  0.5588587   0.633 0.526646    
## as.factor(active)1                 -0.9475695  0.4099344  -2.312 0.020935 *  
## as.factor(active)2                 -0.2613779  0.6845577  -0.382 0.702647    
## wt71                                0.0455018  0.0833709   0.546 0.585299    
## I(wt71 * wt71)                     -0.0009653  0.0005247  -1.840 0.066001 .  
## I(qsmk * smokeintensity)            0.0466628  0.0351448   1.328 0.184463    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 53.5683)
## 
##     Null deviance: 97176  on 1565  degrees of freedom
## Residual deviance: 82763  on 1545  degrees of freedom
##   (3321 observations deleted due to missingness)
## AIC: 10701
## 
## Number of Fisher Scoring iterations: 2
```

```r
onesample$predicted_meanY <- predict(std, onesample)

# estimate mean outcome in each of the groups interv=0, and interv=1
# this mean outcome is a weighted average of the mean outcomes in each combination
# of values of treatment and confounders, that is, the standardized outcome
mean(onesample[which(onesample$interv == -1), ]$predicted_meanY)
```

```
## [1] 2.56319
```

```r
mean(onesample[which(onesample$interv == 0), ]$predicted_meanY)
```

```
## [1] 1.660267
```

```r
mean(onesample[which(onesample$interv == 1), ]$predicted_meanY)
```

```
## [1] 5.178841
```


## Program 13.4

- Computing the 95% confidence interval of the standardized means and their difference
- Data from NHEFS


```r
#install.packages("boot") # install package if required
library(boot)

# function to calculate difference in means
standardization <- function(data, indices) {
  # create a dataset with 3 copies of each subject
  d <- data[indices, ] # 1st copy: equal to original one`
  d$interv <- -1
  d0 <- d # 2nd copy: treatment set to 0, outcome to missing
  d0$interv <- 0
  d0$qsmk <- 0
  d0$wt82_71 <- NA
  d1 <- d # 3rd copy: treatment set to 1, outcome to missing
  d1$interv <- 1
  d1$qsmk <- 1
  d1$wt82_71 <- NA
  d.onesample <- rbind(d, d0, d1) # combining datasets
  
  # linear model to estimate mean outcome conditional on treatment and confounders
  # parameters are estimated using original observations only (interv= -1)
  # parameter estimates are used to predict mean outcome for observations with set
  # treatment (interv=0 and interv=1)
  fit <- glm(
    wt82_71 ~ qsmk + sex + race + age + I(age * age) +
      as.factor(education) + smokeintensity +
      I(smokeintensity * smokeintensity) + smokeyrs + I(smokeyrs *
                                                          smokeyrs) +
      as.factor(exercise) + as.factor(active) + wt71 + I(wt71 *
                                                           wt71),
    data = d.onesample
  )
  
  d.onesample$predicted_meanY <- predict(fit, d.onesample)
  
  # estimate mean outcome in each of the groups interv=-1, interv=0, and interv=1
  return(c(
    mean(d.onesample$predicted_meanY[d.onesample$interv == -1]),
    mean(d.onesample$predicted_meanY[d.onesample$interv == 0]),
    mean(d.onesample$predicted_meanY[d.onesample$interv == 1]),
    mean(d.onesample$predicted_meanY[d.onesample$interv == 1]) -
      mean(d.onesample$predicted_meanY[d.onesample$interv == 0])
  ))
}

# bootstrap
results <- boot(data = nhefs,
                statistic = standardization,
                R = 5)

# generating confidence intervals
se <- c(sd(results$t[, 1]),
        sd(results$t[, 2]),
        sd(results$t[, 3]),
        sd(results$t[, 4]))
mean <- results$t0
ll <- mean - qnorm(0.975) * se
ul <- mean + qnorm(0.975) * se

bootstrap <-
  data.frame(cbind(
    c(
      "Observed",
      "No Treatment",
      "Treatment",
      "Treatment - No Treatment"
    ),
    mean,
    se,
    ll,
    ul
  ))
bootstrap
```

```
##                         V1             mean                se               ll
## 1                 Observed 2.56188497106103 0.287150423730676 1.99908048240349
## 2             No Treatment 1.65212306626746 0.326510630251429 1.01217399040518
## 3                Treatment 5.11474489549347 0.696737536049109 3.74916441816003
## 4 Treatment - No Treatment 3.46262182922601  0.81191849560491 1.87129081945844
##                 ul
## 1 3.12468945971857
## 2 2.29207214212973
## 3  6.4803253728269
## 4 5.05395283899357
```
