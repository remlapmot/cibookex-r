# 14. G-estimation of Structural Nested Models{-}



## Program 14.1

- Preprocessing, ranks of extreme observations, IP weights for censoring
- Data from NHEFS


``` r
library(here)
```


``` r
# install.packages("readxl") # install package if required
library("readxl")
nhefs <- read_excel(here("data", "NHEFS.xls"))

# some processing of the data
nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)

# ranking of extreme observations
#install.packages("Hmisc")
library(Hmisc)
#> 
#> Attaching package: 'Hmisc'
#> The following objects are masked from 'package:base':
#> 
#>     format.pval, units
describe(nhefs$wt82_71)
#> nhefs$wt82_71 
#>        n  missing distinct     Info     Mean      Gmd      .05      .10 
#>     1566       63     1510        1    2.638    8.337   -9.752   -6.292 
#>      .25      .50      .75      .90      .95 
#>   -1.478    2.604    6.690   11.117   14.739 
#> 
#> lowest : -41.2805 -30.5019 -30.0501 -29.0258 -25.9706
#> highest: 34.0178  36.9693  37.6505  47.5113  48.5384

# estimation of denominator of ip weights for C
cw.denom <- glm(cens==0 ~ qsmk + sex + race + age + I(age^2)
                     + as.factor(education) + smokeintensity + I(smokeintensity^2)
                     + smokeyrs + I(smokeyrs^2) + as.factor(exercise)
                     + as.factor(active) + wt71 + I(wt71^2),
                     data = nhefs, family = binomial("logit"))
summary(cw.denom)
#> 
#> Call:
#> glm(formula = cens == 0 ~ qsmk + sex + race + age + I(age^2) + 
#>     as.factor(education) + smokeintensity + I(smokeintensity^2) + 
#>     smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) + 
#>     wt71 + I(wt71^2), family = binomial("logit"), data = nhefs)
#> 
#> Coefficients:
#>                         Estimate Std. Error z value Pr(>|z|)   
#> (Intercept)           -4.0144661  2.5761058  -1.558  0.11915   
#> qsmk                  -0.5168674  0.2877162  -1.796  0.07242 . 
#> sex                   -0.0573131  0.3302775  -0.174  0.86223   
#> race                   0.0122715  0.4524887   0.027  0.97836   
#> age                    0.2697293  0.1174647   2.296  0.02166 * 
#> I(age^2)              -0.0028837  0.0011135  -2.590  0.00961 **
#> as.factor(education)2  0.4407884  0.4193993   1.051  0.29326   
#> as.factor(education)3  0.1646881  0.3705471   0.444  0.65672   
#> as.factor(education)4 -0.1384470  0.5697969  -0.243  0.80802   
#> as.factor(education)5  0.3823818  0.5601808   0.683  0.49486   
#> smokeintensity        -0.0157119  0.0347319  -0.452  0.65100   
#> I(smokeintensity^2)    0.0001133  0.0006058   0.187  0.85171   
#> smokeyrs              -0.0785973  0.0749576  -1.049  0.29438   
#> I(smokeyrs^2)          0.0005569  0.0010318   0.540  0.58938   
#> as.factor(exercise)1   0.9714714  0.3878101   2.505  0.01224 * 
#> as.factor(exercise)2   0.5839890  0.3723133   1.569  0.11675   
#> as.factor(active)1     0.2474785  0.3254548   0.760  0.44701   
#> as.factor(active)2    -0.7065829  0.3964577  -1.782  0.07471 . 
#> wt71                   0.0878871  0.0400115   2.197  0.02805 * 
#> I(wt71^2)             -0.0006351  0.0002257  -2.813  0.00490 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 533.36  on 1628  degrees of freedom
#> Residual deviance: 465.36  on 1609  degrees of freedom
#> AIC: 505.36
#> 
#> Number of Fisher Scoring iterations: 7
nhefs$pd.c <- predict(cw.denom, nhefs, type="response")
nhefs$wc <- ifelse(nhefs$cens==0, 1/nhefs$pd.c, NA)
# observations with cens=1 only contribute to censoring models
```


## Program 14.2

- G-estimation of a 1-parameter structural nested mean model
- Brute force search
- Data from NHEFS


### G-estimation: Checking one possible value of psi

``` r
#install.packages("geepack")
library("geepack")

nhefs$psi <- 3.446
nhefs$Hpsi <- nhefs$wt82_71 - nhefs$psi*nhefs$qsmk

fit <- geeglm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
           + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
           + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
           + wt71 + I(wt71*wt71) + Hpsi, family=binomial, data=nhefs,
           weights=wc, id=seqn, corstr="independence")
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
summary(fit)
#> 
#> Call:
#> geeglm(formula = qsmk ~ sex + race + age + I(age * age) + as.factor(education) + 
#>     smokeintensity + I(smokeintensity * smokeintensity) + smokeyrs + 
#>     I(smokeyrs * smokeyrs) + as.factor(exercise) + as.factor(active) + 
#>     wt71 + I(wt71 * wt71) + Hpsi, family = binomial, data = nhefs, 
#>     weights = wc, id = seqn, corstr = "independence")
#> 
#>  Coefficients:
#>                                      Estimate    Std.err   Wald Pr(>|W|)    
#> (Intercept)                        -2.403e+00  1.329e+00  3.269 0.070604 .  
#> sex                                -5.137e-01  1.536e-01 11.193 0.000821 ***
#> race                               -8.609e-01  2.099e-01 16.826 4.10e-05 ***
#> age                                 1.152e-01  5.020e-02  5.263 0.021779 *  
#> I(age * age)                       -7.593e-04  5.296e-04  2.056 0.151619    
#> as.factor(education)2              -2.894e-02  1.964e-01  0.022 0.882859    
#> as.factor(education)3               8.771e-02  1.726e-01  0.258 0.611329    
#> as.factor(education)4               6.637e-02  2.698e-01  0.061 0.805645    
#> as.factor(education)5               4.711e-01  2.247e-01  4.395 0.036036 *  
#> smokeintensity                     -7.834e-02  1.464e-02 28.635 8.74e-08 ***
#> I(smokeintensity * smokeintensity)  1.072e-03  2.650e-04 16.368 5.21e-05 ***
#> smokeyrs                           -7.111e-02  2.639e-02  7.261 0.007047 ** 
#> I(smokeyrs * smokeyrs)              8.153e-04  4.490e-04  3.298 0.069384 .  
#> as.factor(exercise)1                3.363e-01  1.828e-01  3.384 0.065844 .  
#> as.factor(exercise)2                3.800e-01  1.889e-01  4.049 0.044187 *  
#> as.factor(active)1                  3.412e-02  1.339e-01  0.065 0.798778    
#> as.factor(active)2                  2.135e-01  2.121e-01  1.012 0.314308    
#> wt71                               -7.661e-03  2.562e-02  0.089 0.764963    
#> I(wt71 * wt71)                      8.655e-05  1.582e-04  0.299 0.584233    
#> Hpsi                               -1.903e-06  8.839e-03  0.000 0.999828    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Correlation structure = independence 
#> Estimated Scale Parameters:
#> 
#>             Estimate Std.err
#> (Intercept)   0.9969 0.06717
#> Number of clusters:   1566  Maximum cluster size: 1
```


### G-estimation: Checking multiple possible values of psi

``` r
#install.packages("geepack")
grid <- seq(from = 2,to = 5, by = 0.1)
j = 0
Hpsi.coefs <- cbind(rep(NA,length(grid)), rep(NA, length(grid)))
colnames(Hpsi.coefs) <- c("Estimate", "p-value")

for (i in grid){
  psi = i
  j = j+1
  nhefs$Hpsi <- nhefs$wt82_71 - psi * nhefs$qsmk

  gest.fit <- geeglm(qsmk ~ sex + race + age + I(age*age) + as.factor(education)
                  + smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs
                  + I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active)
                  + wt71 + I(wt71*wt71) + Hpsi, family=binomial, data=nhefs,
                  weights=wc, id=seqn, corstr="independence")
  Hpsi.coefs[j,1] <- summary(gest.fit)$coefficients["Hpsi", "Estimate"]
  Hpsi.coefs[j,2] <- summary(gest.fit)$coefficients["Hpsi", "Pr(>|W|)"]
}
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
Hpsi.coefs
#>         Estimate  p-value
#>  [1,]  0.0267219 0.001772
#>  [2,]  0.0248946 0.003580
#>  [3,]  0.0230655 0.006963
#>  [4,]  0.0212344 0.013026
#>  [5,]  0.0194009 0.023417
#>  [6,]  0.0175647 0.040430
#>  [7,]  0.0157254 0.067015
#>  [8,]  0.0138827 0.106626
#>  [9,]  0.0120362 0.162877
#> [10,]  0.0101857 0.238979
#> [11,]  0.0083308 0.337048
#> [12,]  0.0064713 0.457433
#> [13,]  0.0046069 0.598235
#> [14,]  0.0027374 0.755204
#> [15,]  0.0008624 0.922101
#> [16,] -0.0010181 0.908537
#> [17,] -0.0029044 0.744362
#> [18,] -0.0047967 0.592188
#> [19,] -0.0066950 0.457169
#> [20,] -0.0085997 0.342360
#> [21,] -0.0105107 0.248681
#> [22,] -0.0124282 0.175239
#> [23,] -0.0143523 0.119841
#> [24,] -0.0162831 0.079580
#> [25,] -0.0182206 0.051347
#> [26,] -0.0201649 0.032218
#> [27,] -0.0221160 0.019675
#> [28,] -0.0240740 0.011706
#> [29,] -0.0260389 0.006792
#> [30,] -0.0280106 0.003847
#> [31,] -0.0299893 0.002129
```

## Program 14.3

- G-estimation for 2-parameter structural nested mean model
- Closed form estimator
- Data from NHEFS

### G-estimation: Closed form estimator linear mean models #

``` r
logit.est <- glm(qsmk ~ sex + race + age + I(age^2) + as.factor(education)
                 + smokeintensity + I(smokeintensity^2) + smokeyrs
                 + I(smokeyrs^2) + as.factor(exercise) + as.factor(active)
                 + wt71 + I(wt71^2), data = nhefs, weight = wc,
                 family = binomial())
#> Warning in eval(family$initialize): non-integer #successes in a binomial glm!
summary(logit.est)
#> 
#> Call:
#> glm(formula = qsmk ~ sex + race + age + I(age^2) + as.factor(education) + 
#>     smokeintensity + I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + 
#>     as.factor(exercise) + as.factor(active) + wt71 + I(wt71^2), 
#>     family = binomial(), data = nhefs, weights = wc)
#> 
#> Coefficients:
#>                        Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)           -2.40e+00   1.31e+00   -1.83  0.06743 .  
#> sex                   -5.14e-01   1.50e-01   -3.42  0.00062 ***
#> race                  -8.61e-01   2.06e-01   -4.18  2.9e-05 ***
#> age                    1.15e-01   4.95e-02    2.33  0.01992 *  
#> I(age^2)              -7.59e-04   5.14e-04   -1.48  0.13953    
#> as.factor(education)2 -2.89e-02   1.93e-01   -0.15  0.88079    
#> as.factor(education)3  8.77e-02   1.73e-01    0.51  0.61244    
#> as.factor(education)4  6.64e-02   2.66e-01    0.25  0.80301    
#> as.factor(education)5  4.71e-01   2.21e-01    2.13  0.03314 *  
#> smokeintensity        -7.83e-02   1.49e-02   -5.27  1.4e-07 ***
#> I(smokeintensity^2)    1.07e-03   2.78e-04    3.85  0.00012 ***
#> smokeyrs              -7.11e-02   2.71e-02   -2.63  0.00862 ** 
#> I(smokeyrs^2)          8.15e-04   4.45e-04    1.83  0.06722 .  
#> as.factor(exercise)1   3.36e-01   1.75e-01    1.92  0.05467 .  
#> as.factor(exercise)2   3.80e-01   1.82e-01    2.09  0.03637 *  
#> as.factor(active)1     3.41e-02   1.30e-01    0.26  0.79337    
#> as.factor(active)2     2.13e-01   2.06e-01    1.04  0.30033    
#> wt71                  -7.66e-03   2.46e-02   -0.31  0.75530    
#> I(wt71^2)              8.66e-05   1.51e-04    0.57  0.56586    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 1872.2  on 1565  degrees of freedom
#> Residual deviance: 1755.6  on 1547  degrees of freedom
#>   (63 observations deleted due to missingness)
#> AIC: 1719
#> 
#> Number of Fisher Scoring iterations: 4
nhefs$pqsmk <- predict(logit.est, nhefs, type = "response")
describe(nhefs$pqsmk)
#> nhefs$pqsmk 
#>        n  missing distinct     Info     Mean      Gmd      .05      .10 
#>     1629        0     1629        1   0.2622   0.1302   0.1015   0.1261 
#>      .25      .50      .75      .90      .95 
#>   0.1780   0.2426   0.3251   0.4221   0.4965 
#> 
#> lowest : 0.0514466 0.0515703 0.0543802 0.0558308 0.0593059
#> highest: 0.672083  0.686432  0.713913  0.733299  0.78914
summary(nhefs$pqsmk)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.0514  0.1780  0.2426  0.2622  0.3251  0.7891

# solve sum(w_c * H(psi) * (qsmk - E[qsmk | L]))  = 0
# for a single psi and H(psi) = wt82_71 - psi * qsmk
# this can be solved as
# psi = sum( w_c * wt82_71 * (qsmk - pqsmk)) / sum(w_c * qsmk * (qsmk - pqsmk))

nhefs.c <- nhefs[which(!is.na(nhefs$wt82)),]
with(nhefs.c, sum(wc*wt82_71*(qsmk-pqsmk)) / sum(wc*qsmk*(qsmk - pqsmk)))
#> [1] 3.446
```


### G-estimation: Closed form estimator for 2-parameter model

``` r
diff = with(nhefs.c, qsmk - pqsmk)
diff2 = with(nhefs.c, wc * diff)

lhs = matrix(0,2,2)
lhs[1,1] = with(nhefs.c, sum(qsmk * diff2))
lhs[1,2] = with(nhefs.c, sum(qsmk * smokeintensity  * diff2))
lhs[2,1] = with(nhefs.c, sum(qsmk * smokeintensity * diff2))
lhs[2,2] = with(nhefs.c, sum(qsmk * smokeintensity * smokeintensity * diff2))

rhs = matrix(0,2,1)
rhs[1] = with(nhefs.c, sum(wt82_71 * diff2))
rhs[2] = with(nhefs.c, sum(wt82_71 * smokeintensity * diff2))

psi = t(solve(lhs,rhs))
psi
#>       [,1]    [,2]
#> [1,] 2.859 0.03004
```
