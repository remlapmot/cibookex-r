# 12. IP Weighting and Marginal Structural Models{-}

## Program 12.1

- Descriptive statistics from NHEFS data (Table 12.1)


```r
library(here)
```


```r
# install.packages("readxl") # install package if required
library("readxl")

nhefs <- read_excel(here("data", "NHEFS.xls"))
nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)

# provisionally ignore subjects with missing values for weight in 1982
nhefs.nmv <-
  nhefs[which(!is.na(nhefs$wt82)),] 

lm(wt82_71 ~ qsmk, data = nhefs.nmv)
```

```
## 
## Call:
## lm(formula = wt82_71 ~ qsmk, data = nhefs.nmv)
## 
## Coefficients:
## (Intercept)         qsmk  
##       1.984        2.541
```

```r
# Smoking cessation
predict(lm(wt82_71 ~ qsmk, data = nhefs.nmv), data.frame(qsmk = 1))
```

```
##        1 
## 4.525079
```

```r
# No smoking cessation
predict(lm(wt82_71 ~ qsmk, data = nhefs.nmv), data.frame(qsmk = 0)) 
```

```
##        1 
## 1.984498
```

```r
# Table
summary(nhefs.nmv[which(nhefs.nmv$qsmk == 0),]$age)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   25.00   33.00   42.00   42.79   51.00   72.00
```

```r
summary(nhefs.nmv[which(nhefs.nmv$qsmk == 0),]$wt71)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   40.82   59.19   68.49   70.30   79.38  151.73
```

```r
summary(nhefs.nmv[which(nhefs.nmv$qsmk == 0),]$smokeintensity)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00   15.00   20.00   21.19   30.00   60.00
```

```r
summary(nhefs.nmv[which(nhefs.nmv$qsmk == 0),]$smokeyrs)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00   15.00   23.00   24.09   32.00   64.00
```

```r
summary(nhefs.nmv[which(nhefs.nmv$qsmk == 1),]$age)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   25.00   35.00   46.00   46.17   56.00   74.00
```

```r
summary(nhefs.nmv[which(nhefs.nmv$qsmk == 1),]$wt71)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   39.58   60.67   71.21   72.35   81.08  136.98
```

```r
summary(nhefs.nmv[which(nhefs.nmv$qsmk == 1),]$smokeintensity)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0    10.0    20.0    18.6    25.0    80.0
```

```r
summary(nhefs.nmv[which(nhefs.nmv$qsmk == 1),]$smokeyrs)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00   15.00   26.00   26.03   35.00   60.00
```

```r
table(nhefs.nmv$qsmk, nhefs.nmv$sex)
```

```
##    
##       0   1
##   0 542 621
##   1 220 183
```

```r
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$sex), 1)
```

```
##    
##             0         1
##   0 0.4660361 0.5339639
##   1 0.5459057 0.4540943
```

```r
table(nhefs.nmv$qsmk, nhefs.nmv$race)
```

```
##    
##       0   1
##   0 993 170
##   1 367  36
```

```r
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$race), 1)
```

```
##    
##              0          1
##   0 0.85382631 0.14617369
##   1 0.91066998 0.08933002
```

```r
table(nhefs.nmv$qsmk, nhefs.nmv$education)
```

```
##    
##       1   2   3   4   5
##   0 210 266 480  92 115
##   1  81  74 157  29  62
```

```r
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$education), 1)
```

```
##    
##              1          2          3          4          5
##   0 0.18056750 0.22871883 0.41272571 0.07910576 0.09888220
##   1 0.20099256 0.18362283 0.38957816 0.07196030 0.15384615
```

```r
table(nhefs.nmv$qsmk, nhefs.nmv$exercise)
```

```
##    
##       0   1   2
##   0 237 485 441
##   1  63 176 164
```

```r
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$exercise), 1)
```

```
##    
##             0         1         2
##   0 0.2037833 0.4170249 0.3791917
##   1 0.1563275 0.4367246 0.4069479
```

```r
table(nhefs.nmv$qsmk, nhefs.nmv$active)
```

```
##    
##       0   1   2
##   0 532 527 104
##   1 170 188  45
```

```r
prop.table(table(nhefs.nmv$qsmk, nhefs.nmv$active), 1)
```

```
##    
##             0         1         2
##   0 0.4574377 0.4531384 0.0894239
##   1 0.4218362 0.4665012 0.1116625
```


## Program 12.2

- Estimating IP weights
- Data from NHEFS


```r
# Estimation of ip weights via a logistic model
fit <- glm(
  qsmk ~ sex + race + age + I(age ^ 2) +
    as.factor(education) + smokeintensity +
    I(smokeintensity ^ 2) + smokeyrs + I(smokeyrs ^ 2) +
    as.factor(exercise) + as.factor(active) + wt71 + I(wt71 ^ 2),
  family = binomial(),
  data = nhefs.nmv
)
summary(fit)
```

```
## 
## Call:
## glm(formula = qsmk ~ sex + race + age + I(age^2) + as.factor(education) + 
##     smokeintensity + I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + 
##     as.factor(exercise) + as.factor(active) + wt71 + I(wt71^2), 
##     family = binomial(), data = nhefs.nmv)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.5127  -0.7907  -0.6387   0.9832   2.3729  
## 
## Coefficients:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -2.2425191  1.3808360  -1.624 0.104369    
## sex                   -0.5274782  0.1540496  -3.424 0.000617 ***
## race                  -0.8392636  0.2100665  -3.995 6.46e-05 ***
## age                    0.1212052  0.0512663   2.364 0.018068 *  
## I(age^2)              -0.0008246  0.0005361  -1.538 0.124039    
## as.factor(education)2 -0.0287755  0.1983506  -0.145 0.884653    
## as.factor(education)3  0.0864318  0.1780850   0.485 0.627435    
## as.factor(education)4  0.0636010  0.2732108   0.233 0.815924    
## as.factor(education)5  0.4759606  0.2262237   2.104 0.035384 *  
## smokeintensity        -0.0772704  0.0152499  -5.067 4.04e-07 ***
## I(smokeintensity^2)    0.0010451  0.0002866   3.647 0.000265 ***
## smokeyrs              -0.0735966  0.0277775  -2.650 0.008061 ** 
## I(smokeyrs^2)          0.0008441  0.0004632   1.822 0.068398 .  
## as.factor(exercise)1   0.3548405  0.1801351   1.970 0.048855 *  
## as.factor(exercise)2   0.3957040  0.1872400   2.113 0.034571 *  
## as.factor(active)1     0.0319445  0.1329372   0.240 0.810100    
## as.factor(active)2     0.1767840  0.2149720   0.822 0.410873    
## wt71                  -0.0152357  0.0263161  -0.579 0.562625    
## I(wt71^2)              0.0001352  0.0001632   0.829 0.407370    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1786.1  on 1565  degrees of freedom
## Residual deviance: 1676.9  on 1547  degrees of freedom
## AIC: 1714.9
## 
## Number of Fisher Scoring iterations: 4
```

```r
p.qsmk.obs <-
  ifelse(nhefs.nmv$qsmk == 0,
         1 - predict(fit, type = "response"),
         predict(fit, type = "response"))

nhefs.nmv$w <- 1 / p.qsmk.obs
summary(nhefs.nmv$w)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.054   1.230   1.373   1.996   1.990  16.700
```

```r
sd(nhefs.nmv$w)
```

```
## [1] 1.474787
```

```r
# install.packages("geepack") # install package if required
library("geepack")
msm.w <- geeglm(
  wt82_71 ~ qsmk,
  data = nhefs.nmv,
  weights = w,
  id = seqn,
  corstr = "independence"
)
summary(msm.w)
```

```
## 
## Call:
## geeglm(formula = wt82_71 ~ qsmk, data = nhefs.nmv, weights = w, 
##     id = seqn, corstr = "independence")
## 
##  Coefficients:
##             Estimate Std.err  Wald Pr(>|W|)    
## (Intercept)   1.7800  0.2247 62.73 2.33e-15 ***
## qsmk          3.4405  0.5255 42.87 5.86e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)    65.06   4.221
## 
## Correlation: Structure = independenceNumber of clusters:   1566   Maximum cluster size: 1
```

```r
beta <- coef(msm.w)
SE <- coef(summary(msm.w))[, 2]
lcl <- beta - qnorm(0.975) * SE
ucl <- beta + qnorm(0.975) * SE
cbind(beta, lcl, ucl)
```

```
##              beta   lcl  ucl
## (Intercept) 1.780 1.340 2.22
## qsmk        3.441 2.411 4.47
```

```r
# no association between sex and qsmk in pseudo-population
xtabs(nhefs.nmv$w ~ nhefs.nmv$sex + nhefs.nmv$qsmk)
```

```
##              nhefs.nmv$qsmk
## nhefs.nmv$sex     0     1
##             0 763.6 763.6
##             1 801.7 797.2
```

```r
# "check" for positivity (White women)
table(nhefs.nmv$age[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1],
      nhefs.nmv$qsmk[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1])
```

```
##     
##       0  1
##   25 24  3
##   26 14  5
##   27 18  2
##   28 20  5
##   29 15  4
##   30 14  5
##   31 11  5
##   32 14  7
##   33 12  3
##   34 22  5
##   35 16  5
##   36 13  3
##   37 14  1
##   38  6  2
##   39 19  4
##   40 10  4
##   41 13  3
##   42 16  3
##   43 14  3
##   44  9  4
##   45 12  5
##   46 19  4
##   47 19  4
##   48 19  4
##   49 11  3
##   50 18  4
##   51  9  3
##   52 11  3
##   53 11  4
##   54 17  9
##   55  9  4
##   56  8  7
##   57  9  2
##   58  8  4
##   59  5  4
##   60  5  4
##   61  5  2
##   62  6  5
##   63  3  3
##   64  7  1
##   65  3  2
##   66  4  0
##   67  2  0
##   69  6  2
##   70  2  1
##   71  0  1
##   72  2  2
##   74  0  1
```

## Program 12.3

- Estimating stabilized IP weights
- Data from NHEFS


```r
# estimation of denominator of ip weights
denom.fit <-
  glm(
    qsmk ~ as.factor(sex) + as.factor(race) + age + I(age ^ 2) +
      as.factor(education) + smokeintensity +
      I(smokeintensity ^ 2) + smokeyrs + I(smokeyrs ^ 2) +
      as.factor(exercise) + as.factor(active) + wt71 + I(wt71 ^ 2),
    family = binomial(),
    data = nhefs.nmv
  )
summary(denom.fit)
```

```
## 
## Call:
## glm(formula = qsmk ~ as.factor(sex) + as.factor(race) + age + 
##     I(age^2) + as.factor(education) + smokeintensity + I(smokeintensity^2) + 
##     smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) + 
##     wt71 + I(wt71^2), family = binomial(), data = nhefs.nmv)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.513  -0.791  -0.639   0.983   2.373  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -2.242519   1.380836   -1.62  0.10437    
## as.factor(sex)1       -0.527478   0.154050   -3.42  0.00062 ***
## as.factor(race)1      -0.839264   0.210067   -4.00  6.5e-05 ***
## age                    0.121205   0.051266    2.36  0.01807 *  
## I(age^2)              -0.000825   0.000536   -1.54  0.12404    
## as.factor(education)2 -0.028776   0.198351   -0.15  0.88465    
## as.factor(education)3  0.086432   0.178085    0.49  0.62744    
## as.factor(education)4  0.063601   0.273211    0.23  0.81592    
## as.factor(education)5  0.475961   0.226224    2.10  0.03538 *  
## smokeintensity        -0.077270   0.015250   -5.07  4.0e-07 ***
## I(smokeintensity^2)    0.001045   0.000287    3.65  0.00027 ***
## smokeyrs              -0.073597   0.027777   -2.65  0.00806 ** 
## I(smokeyrs^2)          0.000844   0.000463    1.82  0.06840 .  
## as.factor(exercise)1   0.354841   0.180135    1.97  0.04885 *  
## as.factor(exercise)2   0.395704   0.187240    2.11  0.03457 *  
## as.factor(active)1     0.031944   0.132937    0.24  0.81010    
## as.factor(active)2     0.176784   0.214972    0.82  0.41087    
## wt71                  -0.015236   0.026316   -0.58  0.56262    
## I(wt71^2)              0.000135   0.000163    0.83  0.40737    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1786.1  on 1565  degrees of freedom
## Residual deviance: 1676.9  on 1547  degrees of freedom
## AIC: 1715
## 
## Number of Fisher Scoring iterations: 4
```

```r
pd.qsmk <- predict(denom.fit, type = "response")

# estimation of numerator of ip weights
numer.fit <- glm(qsmk ~ 1, family = binomial(), data = nhefs.nmv)
summary(numer.fit)
```

```
## 
## Call:
## glm(formula = qsmk ~ 1, family = binomial(), data = nhefs.nmv)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -0.771  -0.771  -0.771   1.648   1.648  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.0598     0.0578   -18.3   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1786.1  on 1565  degrees of freedom
## Residual deviance: 1786.1  on 1565  degrees of freedom
## AIC: 1788
## 
## Number of Fisher Scoring iterations: 4
```

```r
pn.qsmk <- predict(numer.fit, type = "response")

nhefs.nmv$sw <-
  ifelse(nhefs.nmv$qsmk == 0, ((1 - pn.qsmk) / (1 - pd.qsmk)),
         (pn.qsmk / pd.qsmk))

summary(nhefs.nmv$sw)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.331   0.867   0.950   0.999   1.079   4.298
```

```r
msm.sw <- geeglm(
  wt82_71 ~ qsmk,
  data = nhefs.nmv,
  weights = sw,
  id = seqn,
  corstr = "independence"
)
summary(msm.sw)
```

```
## 
## Call:
## geeglm(formula = wt82_71 ~ qsmk, data = nhefs.nmv, weights = sw, 
##     id = seqn, corstr = "independence")
## 
##  Coefficients:
##             Estimate Std.err Wald Pr(>|W|)    
## (Intercept)    1.780   0.225 62.7  2.3e-15 ***
## qsmk           3.441   0.525 42.9  5.9e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)     60.7    3.71
## 
## Correlation: Structure = independenceNumber of clusters:   1566   Maximum cluster size: 1
```

```r
beta <- coef(msm.sw)
SE <- coef(summary(msm.sw))[, 2]
lcl <- beta - qnorm(0.975) * SE
ucl <- beta + qnorm(0.975) * SE
cbind(beta, lcl, ucl)
```

```
##             beta  lcl  ucl
## (Intercept) 1.78 1.34 2.22
## qsmk        3.44 2.41 4.47
```

```r
# no association between sex and qsmk in pseudo-population
xtabs(nhefs.nmv$sw ~ nhefs.nmv$sex + nhefs.nmv$qsmk)
```

```
##              nhefs.nmv$qsmk
## nhefs.nmv$sex   0   1
##             0 567 197
##             1 595 205
```

## Program 12.4

- Estimating the parameters of a marginal structural mean model
- with a continuous treatment Data from NHEFS


```r
# Analysis restricted to subjects reporting <=25 cig/day at baseline
nhefs.nmv.s <- subset(nhefs.nmv, smokeintensity <= 25)

# estimation of denominator of ip weights
den.fit.obj <- lm(
  smkintensity82_71 ~ as.factor(sex) +
    as.factor(race) + age + I(age ^ 2) +
    as.factor(education) + smokeintensity + I(smokeintensity ^ 2) +
    smokeyrs + I(smokeyrs ^ 2) + as.factor(exercise) + as.factor(active) + wt71 +
    I(wt71 ^ 2),
  data = nhefs.nmv.s
)
p.den <- predict(den.fit.obj, type = "response")
dens.den <-
  dnorm(nhefs.nmv.s$smkintensity82_71,
        p.den,
        summary(den.fit.obj)$sigma)

# estimation of numerator of ip weights
num.fit.obj <- lm(smkintensity82_71 ~ 1, data = nhefs.nmv.s)
p.num <- predict(num.fit.obj, type = "response")
dens.num <-
  dnorm(nhefs.nmv.s$smkintensity82_71,
        p.num,
        summary(num.fit.obj)$sigma)

nhefs.nmv.s$sw.a <- dens.num / dens.den
summary(nhefs.nmv.s$sw.a)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.19    0.89    0.97    1.00    1.05    5.10
```

```r
msm.sw.cont <-
  geeglm(
    wt82_71 ~ smkintensity82_71 + I(smkintensity82_71 * smkintensity82_71),
    data = nhefs.nmv.s,
    weights = sw.a,
    id = seqn,
    corstr = "independence"
  )
summary(msm.sw.cont)
```

```
## 
## Call:
## geeglm(formula = wt82_71 ~ smkintensity82_71 + I(smkintensity82_71 * 
##     smkintensity82_71), data = nhefs.nmv.s, weights = sw.a, id = seqn, 
##     corstr = "independence")
## 
##  Coefficients:
##                                          Estimate  Std.err  Wald Pr(>|W|)
## (Intercept)                               2.00452  0.29512 46.13  1.1e-11
## smkintensity82_71                        -0.10899  0.03154 11.94  0.00055
## I(smkintensity82_71 * smkintensity82_71)  0.00269  0.00242  1.24  0.26489
##                                             
## (Intercept)                              ***
## smkintensity82_71                        ***
## I(smkintensity82_71 * smkintensity82_71)    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)     60.5     4.5
## 
## Correlation: Structure = independenceNumber of clusters:   1162   Maximum cluster size: 1
```

```r
beta <- coef(msm.sw.cont)
SE <- coef(summary(msm.sw.cont))[, 2]
lcl <- beta - qnorm(0.975) * SE
ucl <- beta + qnorm(0.975) * SE
cbind(beta, lcl, ucl)
```

```
##                                              beta      lcl      ucl
## (Intercept)                               2.00452  1.42610  2.58295
## smkintensity82_71                        -0.10899 -0.17080 -0.04718
## I(smkintensity82_71 * smkintensity82_71)  0.00269 -0.00204  0.00743
```

## Program 12.5

- Estimating the parameters of a marginal structural logistic model
- Data from NHEFS


```r
table(nhefs.nmv$qsmk, nhefs.nmv$death)
```

```
##    
##       0   1
##   0 963 200
##   1 312  91
```

```r
# First, estimation of stabilized weights sw (same as in Program 12.3)
# Second, fit logistic model below
msm.logistic <- geeglm(
  death ~ qsmk,
  data = nhefs.nmv,
  weights = sw,
  id = seqn,
  family = binomial(),
  corstr = "independence"
)
```

```
## Warning in eval(family$initialize): non-integer #successes in a binomial
## glm!
```

```r
summary(msm.logistic)
```

```
## 
## Call:
## geeglm(formula = death ~ qsmk, family = binomial(), data = nhefs.nmv, 
##     weights = sw, id = seqn, corstr = "independence")
## 
##  Coefficients:
##             Estimate Std.err   Wald Pr(>|W|)    
## (Intercept)  -1.4905  0.0789 356.50   <2e-16 ***
## qsmk          0.0301  0.1573   0.04     0.85    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)        1  0.0678
## 
## Correlation: Structure = independenceNumber of clusters:   1566   Maximum cluster size: 1
```

```r
beta <- coef(msm.logistic)
SE <- coef(summary(msm.logistic))[, 2]
lcl <- beta - qnorm(0.975) * SE
ucl <- beta + qnorm(0.975) * SE
cbind(beta, lcl, ucl)
```

```
##                beta    lcl    ucl
## (Intercept) -1.4905 -1.645 -1.336
## qsmk         0.0301 -0.278  0.338
```

## Program 12.6

- Assessing effect modification by sex using a marginal structural mean model
- Data from NHEFS


```r
table(nhefs.nmv$sex)
```

```
## 
##   0   1 
## 762 804
```

```r
# estimation of denominator of ip weights
denom.fit <-
  glm(
    qsmk ~ as.factor(sex) + as.factor(race) + age + I(age ^ 2) +
      as.factor(education) + smokeintensity +
      I(smokeintensity ^ 2) + smokeyrs + I(smokeyrs ^ 2) +
      as.factor(exercise) + as.factor(active) + wt71 + I(wt71 ^ 2),
    family = binomial(),
    data = nhefs.nmv
  )
summary(denom.fit)
```

```
## 
## Call:
## glm(formula = qsmk ~ as.factor(sex) + as.factor(race) + age + 
##     I(age^2) + as.factor(education) + smokeintensity + I(smokeintensity^2) + 
##     smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) + 
##     wt71 + I(wt71^2), family = binomial(), data = nhefs.nmv)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.513  -0.791  -0.639   0.983   2.373  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -2.242519   1.380836   -1.62  0.10437    
## as.factor(sex)1       -0.527478   0.154050   -3.42  0.00062 ***
## as.factor(race)1      -0.839264   0.210067   -4.00  6.5e-05 ***
## age                    0.121205   0.051266    2.36  0.01807 *  
## I(age^2)              -0.000825   0.000536   -1.54  0.12404    
## as.factor(education)2 -0.028776   0.198351   -0.15  0.88465    
## as.factor(education)3  0.086432   0.178085    0.49  0.62744    
## as.factor(education)4  0.063601   0.273211    0.23  0.81592    
## as.factor(education)5  0.475961   0.226224    2.10  0.03538 *  
## smokeintensity        -0.077270   0.015250   -5.07  4.0e-07 ***
## I(smokeintensity^2)    0.001045   0.000287    3.65  0.00027 ***
## smokeyrs              -0.073597   0.027777   -2.65  0.00806 ** 
## I(smokeyrs^2)          0.000844   0.000463    1.82  0.06840 .  
## as.factor(exercise)1   0.354841   0.180135    1.97  0.04885 *  
## as.factor(exercise)2   0.395704   0.187240    2.11  0.03457 *  
## as.factor(active)1     0.031944   0.132937    0.24  0.81010    
## as.factor(active)2     0.176784   0.214972    0.82  0.41087    
## wt71                  -0.015236   0.026316   -0.58  0.56262    
## I(wt71^2)              0.000135   0.000163    0.83  0.40737    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1786.1  on 1565  degrees of freedom
## Residual deviance: 1676.9  on 1547  degrees of freedom
## AIC: 1715
## 
## Number of Fisher Scoring iterations: 4
```

```r
pd.qsmk <- predict(denom.fit, type = "response")

# estimation of numerator of ip weights
numer.fit <-
  glm(qsmk ~ as.factor(sex), family = binomial(), data = nhefs.nmv)
summary(numer.fit)
```

```
## 
## Call:
## glm(formula = qsmk ~ as.factor(sex), family = binomial(), data = nhefs.nmv)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -0.825  -0.825  -0.719   1.576   1.720  
## 
## Coefficients:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -0.9016     0.0799  -11.28   <2e-16 ***
## as.factor(sex)1  -0.3202     0.1160   -2.76   0.0058 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1786.1  on 1565  degrees of freedom
## Residual deviance: 1778.4  on 1564  degrees of freedom
## AIC: 1782
## 
## Number of Fisher Scoring iterations: 4
```

```r
pn.qsmk <- predict(numer.fit, type = "response")

nhefs.nmv$sw.a <-
  ifelse(nhefs.nmv$qsmk == 0, ((1 - pn.qsmk) / (1 - pd.qsmk)),
         (pn.qsmk / pd.qsmk))

summary(nhefs.nmv$sw.a)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.29    0.88    0.96    1.00    1.08    3.80
```

```r
sd(nhefs.nmv$sw.a)
```

```
## [1] 0.271
```

```r
# Estimating parameters of a marginal structural mean model
msm.emm <- geeglm(
  wt82_71 ~ as.factor(qsmk) + as.factor(sex)
  + as.factor(qsmk):as.factor(sex),
  data = nhefs.nmv,
  weights = sw.a,
  id = seqn,
  corstr = "independence"
)
summary(msm.emm)
```

```
## 
## Call:
## geeglm(formula = wt82_71 ~ as.factor(qsmk) + as.factor(sex) + 
##     as.factor(qsmk):as.factor(sex), data = nhefs.nmv, weights = sw.a, 
##     id = seqn, corstr = "independence")
## 
##  Coefficients:
##                                  Estimate  Std.err  Wald Pr(>|W|)    
## (Intercept)                       1.78445  0.30984 33.17  8.5e-09 ***
## as.factor(qsmk)1                  3.52198  0.65707 28.73  8.3e-08 ***
## as.factor(sex)1                  -0.00872  0.44882  0.00     0.98    
## as.factor(qsmk)1:as.factor(sex)1 -0.15948  1.04608  0.02     0.88    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)     60.8    3.71
## 
## Correlation: Structure = independenceNumber of clusters:   1566   Maximum cluster size: 1
```

```r
beta <- coef(msm.emm)
SE <- coef(summary(msm.emm))[, 2]
lcl <- beta - qnorm(0.975) * SE
ucl <- beta + qnorm(0.975) * SE
cbind(beta, lcl, ucl)
```

```
##                                      beta    lcl   ucl
## (Intercept)                       1.78445  1.177 2.392
## as.factor(qsmk)1                  3.52198  2.234 4.810
## as.factor(sex)1                  -0.00872 -0.888 0.871
## as.factor(qsmk)1:as.factor(sex)1 -0.15948 -2.210 1.891
```

## Program 12.7

- Estimating IP weights to adjust for selection bias due to censoring
- Data from NHEFS


```r
table(nhefs$qsmk, nhefs$cens)
```

```
##    
##        0    1
##   0 1163   38
##   1  403   25
```

```r
summary(nhefs[which(nhefs$cens == 0),]$wt71)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    39.6    59.5    69.2    70.8    79.8   151.7
```

```r
summary(nhefs[which(nhefs$cens == 1),]$wt71)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    36.2    63.1    72.1    76.6    87.9   169.2
```

```r
# estimation of denominator of ip weights for A
denom.fit <-
  glm(
    qsmk ~ as.factor(sex) + as.factor(race) + age + I(age ^ 2) +
      as.factor(education) + smokeintensity +
      I(smokeintensity ^ 2) + smokeyrs + I(smokeyrs ^ 2) +
      as.factor(exercise) + as.factor(active) + wt71 + I(wt71 ^ 2),
    family = binomial(),
    data = nhefs
  )
summary(denom.fit)
```

```
## 
## Call:
## glm(formula = qsmk ~ as.factor(sex) + as.factor(race) + age + 
##     I(age^2) + as.factor(education) + smokeintensity + I(smokeintensity^2) + 
##     smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) + 
##     wt71 + I(wt71^2), family = binomial(), data = nhefs)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.465  -0.804  -0.646   1.058   2.355  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -1.988902   1.241279   -1.60  0.10909    
## as.factor(sex)1       -0.507522   0.148232   -3.42  0.00062 ***
## as.factor(race)1      -0.850231   0.205872   -4.13  3.6e-05 ***
## age                    0.103013   0.048900    2.11  0.03515 *  
## I(age^2)              -0.000605   0.000507   -1.19  0.23297    
## as.factor(education)2 -0.098320   0.190655   -0.52  0.60607    
## as.factor(education)3  0.015699   0.170714    0.09  0.92673    
## as.factor(education)4 -0.042526   0.264276   -0.16  0.87216    
## as.factor(education)5  0.379663   0.220395    1.72  0.08495 .  
## smokeintensity        -0.065156   0.014759   -4.41  1.0e-05 ***
## I(smokeintensity^2)    0.000846   0.000276    3.07  0.00216 ** 
## smokeyrs              -0.073371   0.026996   -2.72  0.00657 ** 
## I(smokeyrs^2)          0.000838   0.000443    1.89  0.05867 .  
## as.factor(exercise)1   0.291412   0.173554    1.68  0.09314 .  
## as.factor(exercise)2   0.355052   0.179929    1.97  0.04846 *  
## as.factor(active)1     0.010875   0.129832    0.08  0.93324    
## as.factor(active)2     0.068312   0.208727    0.33  0.74346    
## wt71                  -0.012848   0.022283   -0.58  0.56423    
## I(wt71^2)              0.000121   0.000135    0.89  0.37096    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1876.3  on 1628  degrees of freedom
## Residual deviance: 1766.7  on 1610  degrees of freedom
## AIC: 1805
## 
## Number of Fisher Scoring iterations: 4
```

```r
pd.qsmk <- predict(denom.fit, type = "response")

# estimation of numerator of ip weights for A
numer.fit <- glm(qsmk ~ 1, family = binomial(), data = nhefs)
summary(numer.fit)
```

```
## 
## Call:
## glm(formula = qsmk ~ 1, family = binomial(), data = nhefs)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -0.781  -0.781  -0.781   1.635   1.635  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.0318     0.0563   -18.3   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1876.3  on 1628  degrees of freedom
## Residual deviance: 1876.3  on 1628  degrees of freedom
## AIC: 1878
## 
## Number of Fisher Scoring iterations: 4
```

```r
pn.qsmk <- predict(numer.fit, type = "response")

# estimation of denominator of ip weights for C
denom.cens <- glm(
  cens ~ as.factor(qsmk) + as.factor(sex) +
    as.factor(race) + age + I(age ^ 2) +
    as.factor(education) + smokeintensity +
    I(smokeintensity ^ 2) + smokeyrs + I(smokeyrs ^ 2) +
    as.factor(exercise) + as.factor(active) + wt71 + I(wt71 ^ 2),
  family = binomial(),
  data = nhefs
)
summary(denom.cens)
```

```
## 
## Call:
## glm(formula = cens ~ as.factor(qsmk) + as.factor(sex) + as.factor(race) + 
##     age + I(age^2) + as.factor(education) + smokeintensity + 
##     I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + as.factor(exercise) + 
##     as.factor(active) + wt71 + I(wt71^2), family = binomial(), 
##     data = nhefs)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.097  -0.287  -0.207  -0.157   2.996  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)   
## (Intercept)            4.014466   2.576106    1.56   0.1192   
## as.factor(qsmk)1       0.516867   0.287716    1.80   0.0724 . 
## as.factor(sex)1        0.057313   0.330278    0.17   0.8622   
## as.factor(race)1      -0.012271   0.452489   -0.03   0.9784   
## age                   -0.269729   0.117465   -2.30   0.0217 * 
## I(age^2)               0.002884   0.001114    2.59   0.0096 **
## as.factor(education)2 -0.440788   0.419399   -1.05   0.2933   
## as.factor(education)3 -0.164688   0.370547   -0.44   0.6567   
## as.factor(education)4  0.138447   0.569797    0.24   0.8080   
## as.factor(education)5 -0.382382   0.560181   -0.68   0.4949   
## smokeintensity         0.015712   0.034732    0.45   0.6510   
## I(smokeintensity^2)   -0.000113   0.000606   -0.19   0.8517   
## smokeyrs               0.078597   0.074958    1.05   0.2944   
## I(smokeyrs^2)         -0.000557   0.001032   -0.54   0.5894   
## as.factor(exercise)1  -0.971471   0.387810   -2.51   0.0122 * 
## as.factor(exercise)2  -0.583989   0.372313   -1.57   0.1168   
## as.factor(active)1    -0.247479   0.325455   -0.76   0.4470   
## as.factor(active)2     0.706583   0.396458    1.78   0.0747 . 
## wt71                  -0.087887   0.040012   -2.20   0.0281 * 
## I(wt71^2)              0.000635   0.000226    2.81   0.0049 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 533.36  on 1628  degrees of freedom
## Residual deviance: 465.36  on 1609  degrees of freedom
## AIC: 505.4
## 
## Number of Fisher Scoring iterations: 7
```

```r
pd.cens <- 1 - predict(denom.cens, type = "response")

# estimation of numerator of ip weights for C
numer.cens <-
  glm(cens ~ as.factor(qsmk), family = binomial(), data = nhefs)
summary(numer.cens)
```

```
## 
## Call:
## glm(formula = cens ~ as.factor(qsmk), family = binomial(), data = nhefs)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -0.347  -0.254  -0.254  -0.254   2.628  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -3.421      0.165  -20.75   <2e-16 ***
## as.factor(qsmk)1    0.641      0.264    2.43    0.015 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 533.36  on 1628  degrees of freedom
## Residual deviance: 527.76  on 1627  degrees of freedom
## AIC: 531.8
## 
## Number of Fisher Scoring iterations: 6
```

```r
pn.cens <- 1 - predict(numer.cens, type = "response")

nhefs$sw.a <-
  ifelse(nhefs$qsmk == 0, ((1 - pn.qsmk) / (1 - pd.qsmk)),
         (pn.qsmk / pd.qsmk))
nhefs$sw.c <- pn.cens / pd.cens
nhefs$sw <- nhefs$sw.c * nhefs$sw.a

summary(nhefs$sw.a)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.33    0.86    0.95    1.00    1.08    4.21
```

```r
sd(nhefs$sw.a)
```

```
## [1] 0.284
```

```r
summary(nhefs$sw.c)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.94    0.98    0.99    1.01    1.01    7.58
```

```r
sd(nhefs$sw.c)
```

```
## [1] 0.178
```

```r
summary(nhefs$sw)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.35    0.86    0.94    1.01    1.08   12.86
```

```r
sd(nhefs$sw)
```

```
## [1] 0.411
```

```r
msm.sw <- geeglm(
  wt82_71 ~ qsmk,
  data = nhefs,
  weights = sw,
  id = seqn,
  corstr = "independence"
)
summary(msm.sw)
```

```
## 
## Call:
## geeglm(formula = wt82_71 ~ qsmk, data = nhefs, weights = sw, 
##     id = seqn, corstr = "independence")
## 
##  Coefficients:
##             Estimate Std.err Wald Pr(>|W|)    
## (Intercept)    1.662   0.233 51.0  9.3e-13 ***
## qsmk           3.496   0.526 44.2  2.9e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)     61.8    3.83
## 
## Correlation: Structure = independenceNumber of clusters:   1566   Maximum cluster size: 1
```

```r
beta <- coef(msm.sw)
SE <- coef(summary(msm.sw))[, 2]
lcl <- beta - qnorm(0.975) * SE
ucl <- beta + qnorm(0.975) * SE
cbind(beta, lcl, ucl)
```

```
##             beta  lcl  ucl
## (Intercept) 1.66 1.21 2.12
## qsmk        3.50 2.47 4.53
```
