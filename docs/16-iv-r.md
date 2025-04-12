# 16. Instrumental variables estimation{-}



## Program 16.1

- Estimating the average causal using the standard IV estimator via the calculation of sample averages
- Data from NHEFS


``` r
library(here)
```


``` r
#install.packages("readxl") # install package if required
library("readxl")
nhefs <- read_excel(here("data", "NHEFS.xls"))

# some preprocessing of the data
nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)
summary(nhefs$price82)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>   1.452   1.740   1.815   1.806   1.868   2.103      92

# for simplicity, ignore subjects with missing outcome or missing instrument
nhefs.iv <- nhefs[which(!is.na(nhefs$wt82) & !is.na(nhefs$price82)),]
nhefs.iv$highprice <- ifelse(nhefs.iv$price82>=1.5, 1, 0)

table(nhefs.iv$highprice, nhefs.iv$qsmk)
#>    
#>        0    1
#>   0   33    8
#>   1 1065  370

t.test(wt82_71 ~ highprice, data=nhefs.iv)
#> 
#> 	Welch Two Sample t-test
#> 
#> data:  wt82_71 by highprice
#> t = -0.10179, df = 41.644, p-value = 0.9194
#> alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#> 95 percent confidence interval:
#>  -3.130588  2.830010
#> sample estimates:
#> mean in group 0 mean in group 1 
#>        2.535729        2.686018
```

## Program 16.2

- Estimating the average causal effect using the standard IV estimator via two-stage-least-squares regression
- Data from NHEFS

``` r
#install.packages ("sem") # install package if required
library(sem)

model1 <- tsls(wt82_71 ~ qsmk, ~ highprice, data = nhefs.iv)
summary(model1)
#> 
#>  2SLS Estimates
#> 
#> Model Formula: wt82_71 ~ qsmk
#> 
#> Instruments: ~highprice
#> 
#> Residuals:
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -43.34863  -4.00206  -0.02712   0.00000   4.17040  46.47022 
#> 
#>              Estimate Std. Error t value Pr(>|t|)
#> (Intercept)  2.068164   5.085098 0.40671  0.68428
#> qsmk         2.396270  19.840037 0.12078  0.90388
#> 
#> Residual standard error: 7.8561141 on 1474 degrees of freedom
confint(model1)  # note the wide confidence intervals
#>                  2.5 %   97.5 %
#> (Intercept)  -7.898445 12.03477
#> qsmk        -36.489487 41.28203
```

## Program 16.3

- Estimating the average causal using the standard IV estimator via additive marginal structural models
- Data from NHEFS
- G-estimation: Checking one possible value of psi
- See Chapter 14 for program that checks several values and computes 95% confidence intervals


``` r
nhefs.iv$psi <- 2.396
nhefs.iv$Hpsi <- nhefs.iv$wt82_71-nhefs.iv$psi*nhefs.iv$qsmk

#install.packages("geepack") # install package if required
library("geepack")
g.est <- geeglm(highprice ~ Hpsi, data=nhefs.iv, id=seqn, family=binomial(),
                corstr="independence")
summary(g.est)
#> 
#> Call:
#> geeglm(formula = highprice ~ Hpsi, family = binomial(), data = nhefs.iv, 
#>     id = seqn, corstr = "independence")
#> 
#>  Coefficients:
#>              Estimate   Std.err  Wald Pr(>|W|)    
#> (Intercept) 3.555e+00 1.652e-01 463.1   <2e-16 ***
#> Hpsi        2.748e-07 2.273e-02   0.0        1    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation structure = independence 
#> Estimated Scale Parameters:
#> 
#>             Estimate Std.err
#> (Intercept)        1  0.7607
#> Number of clusters:   1476  Maximum cluster size: 1

beta <- coef(g.est)
SE <- coef(summary(g.est))[,2]
lcl <- beta-qnorm(0.975)*SE
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)
#>                  beta      lcl     ucl
#> (Intercept) 3.555e+00  3.23152 3.87917
#> Hpsi        2.748e-07 -0.04456 0.04456
```

## Program 16.4

- Estimating the average causal using the standard IV estimator with altnerative proposed instruments
- Data from NHEFS

``` r
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.6, 1, 0), data = nhefs.iv))
#> 
#>  2SLS Estimates
#> 
#> Model Formula: wt82_71 ~ qsmk
#> 
#> Instruments: ~ifelse(price82 >= 1.6, 1, 0)
#> 
#> Residuals:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   -55.6   -13.5     7.6     0.0    12.5    56.4 
#> 
#>             Estimate Std. Error t value Pr(>|t|)
#> (Intercept)    -7.89      42.25  -0.187    0.852
#> qsmk           41.28     164.95   0.250    0.802
#> 
#> Residual standard error: 18.6055 on 1474 degrees of freedom
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.7, 1, 0), data = nhefs.iv))
#> 
#>  2SLS Estimates
#> 
#> Model Formula: wt82_71 ~ qsmk
#> 
#> Instruments: ~ifelse(price82 >= 1.7, 1, 0)
#> 
#> Residuals:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   -54.4   -13.4    -8.4     0.0    18.1    75.3 
#> 
#>             Estimate Std. Error t value Pr(>|t|)
#> (Intercept)    13.16      48.08   0.274    0.784
#> qsmk          -40.91     187.74  -0.218    0.828
#> 
#> Residual standard error: 20.591 on 1474 degrees of freedom
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.8, 1, 0), data = nhefs.iv))
#> 
#>  2SLS Estimates
#> 
#> Model Formula: wt82_71 ~ qsmk
#> 
#> Instruments: ~ifelse(price82 >= 1.8, 1, 0)
#> 
#> Residuals:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  -49.37   -8.31   -3.44    0.00    7.27   60.53 
#> 
#>             Estimate Std. Error t value Pr(>|t|)
#> (Intercept)    8.086      7.288   1.110    0.267
#> qsmk         -21.103     28.428  -0.742    0.458
#> 
#> Residual standard error: 13.0188 on 1474 degrees of freedom
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.9, 1, 0), data = nhefs.iv))
#> 
#>  2SLS Estimates
#> 
#> Model Formula: wt82_71 ~ qsmk
#> 
#> Instruments: ~ifelse(price82 >= 1.9, 1, 0)
#> 
#> Residuals:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  -47.24   -6.33   -1.43    0.00    5.52   54.36 
#> 
#>             Estimate Std. Error t value Pr(>|t|)
#> (Intercept)    5.963      6.067   0.983    0.326
#> qsmk         -12.811     23.667  -0.541    0.588
#> 
#> Residual standard error: 10.3637 on 1474 degrees of freedom
```

## Program 16.5

- Estimating the average causal using the standard IV estimator
- Conditional on baseline covariates
- Data from NHEFS

``` r
model2 <- tsls(wt82_71 ~ qsmk + sex + race + age + smokeintensity + smokeyrs +
                      as.factor(exercise) + as.factor(active) + wt71,
             ~ highprice + sex + race + age + smokeintensity + smokeyrs + as.factor(exercise) +
               as.factor(active) + wt71, data = nhefs.iv)
summary(model2)
#> 
#>  2SLS Estimates
#> 
#> Model Formula: wt82_71 ~ qsmk + sex + race + age + smokeintensity + smokeyrs + 
#>     as.factor(exercise) + as.factor(active) + wt71
#> 
#> Instruments: ~highprice + sex + race + age + smokeintensity + smokeyrs + as.factor(exercise) + 
#>     as.factor(active) + wt71
#> 
#> Residuals:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -42.234  -4.287  -0.619   0.000   3.868  46.738 
#> 
#>                       Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)          17.280330   2.335402   7.399  2.3e-13 ***
#> qsmk                 -1.042295  29.987369  -0.035   0.9723    
#> sex                  -1.644393   2.630831  -0.625   0.5320    
#> race                 -0.183255   4.650386  -0.039   0.9686    
#> age                  -0.163640   0.240548  -0.680   0.4964    
#> smokeintensity        0.005767   0.145504   0.040   0.9684    
#> smokeyrs              0.025836   0.161421   0.160   0.8729    
#> as.factor(exercise)1  0.498748   2.171239   0.230   0.8184    
#> as.factor(exercise)2  0.581834   2.183148   0.267   0.7899    
#> as.factor(active)1   -1.170145   0.607466  -1.926   0.0543 .  
#> as.factor(active)2   -0.512284   1.308451  -0.392   0.6955    
#> wt71                 -0.097949   0.036271  -2.701   0.0070 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 7.7162 on 1464 degrees of freedom
```
