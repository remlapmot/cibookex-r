# 16. Instrumental variables estimation: Stata{-}


```r
library(Statamarkdown)
```

```
/***************************************************************
Stata code for Causal Inference: What If by Miguel Hernan & Jamie Robins
Date: 10/10/2019
Author: Eleanor Murray 
For errors contact: ejmurray@bu.edu
***************************************************************/
```

## Program 16.1

- Estimating the average causal effect using the standard IV estimator via the calculation of sample averages
- Data from NHEFS
- Section 16.2


```stata
use ./data/nhefs-formatted, clear

summarize price82

/* ignore subjects with missing outcome or missing instrument for simplicity*/
foreach var of varlist wt82 price82 {
  drop if `var'==.
}

/*Create categorical instrument*/
gen byte highprice = (price82 > 1.5 & price82 < .)

save ./data/nhefs-highprice, replace

/*Calculate P[Z|A=a]*/
tab highprice qsmk, row

/*Calculate P[Y|Z=z]*/
ttest wt82_71, by(highprice)

/*Final IV estimate, OPTION 1: Hand calculations*/
/*Numerator: num = E[Y|Z=1] - E[Y|Z=0] = 2.686 - 2.536 = 0.150*/
/*Denominator: denom = P[A=1|Z=1] - P[A=1|Z=0] = 0.258 - 0.195 = 0.063 */ 
/*IV estimator: E[Ya=1] - E[Ya=0] = 
(E[Y|Z=1]-E[Y|Z=0])/(P[A=1|Z=1]-P[A=1|Z=0]) = 0.150/0.063 = 2.397*/
display "Numerator, E[Y|Z=1] - E[Y|Z=0] =", 2.686 - 2.536
display "Denominator: denom = P[A=1|Z=1] - P[A=1|Z=0] =", 0.258 - 0.195
display "IV estimator =", 0.150/0.063

/*OPTION 2 2: automated calculation of instrument*/
/*Calculate P[A=1|Z=z], for each value of the instrument, 
and store in a matrix*/
quietly summarize qsmk if (highprice==0)
matrix input pa = (`r(mean)')
quietly summarize qsmk if (highprice==1)
matrix pa = (pa ,`r(mean)')
matrix list pa

/*Calculate P[Y|Z=z], for each value of the instrument, 
and store in a second matrix*/
quietly summarize wt82_71 if (highprice==0)
matrix input ey = (`r(mean)')
quietly summarize wt82_71 if (highprice==1)
matrix ey = (ey ,`r(mean)')
matrix list ey

/*Using Stata's built-in matrix manipulation feature (Mata), 
calculate numerator, denominator and IV estimator*/
*Numerator: num = E[Y|Z=1] - E[Y|Z=0]*mata
*Denominator: denom = P[A=1|Z=1] - P[A=1|Z=0]*
*IV estimator: iv_est = IV estimate of E[Ya=1] - E[Ya=0] *
mata 
pa = st_matrix("pa")
ey = st_matrix("ey")
num = ey[1,2] - ey[1,1] 
denom = pa[1,2] - pa[1,1]
iv_est = num / denom 
num
denom
st_numscalar("iv_est", iv_est)
end
di scalar(iv_est)
```

```
    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
     price82 |      1,476    1.805989    .1301703   1.451904   2.103027

(0 observations deleted)
(90 observations deleted)


file ./data/nhefs-highprice.dta saved


+----------------+
| Key            |
|----------------|
|   frequency    |
| row percentage |
+----------------+

           | quit smoking between
           |   baseline and 1982
 highprice | No smokin  Smoking c |     Total
-----------+----------------------+----------
         0 |        33          8 |        41 
           |     80.49      19.51 |    100.00 
-----------+----------------------+----------
         1 |     1,065        370 |     1,435 
           |     74.22      25.78 |    100.00 
-----------+----------------------+----------
     Total |     1,098        378 |     1,476 
           |     74.39      25.61 |    100.00 


Two-sample t test with equal variances
------------------------------------------------------------------------------
   Group |     Obs        Mean    Std. err.   Std. dev.   [95% conf. interval]
---------+--------------------------------------------------------------------
       0 |      41    2.535729    1.461629    9.358993   -.4183336    5.489792
       1 |   1,435    2.686018    .2084888    7.897848    2.277042    3.094994
---------+--------------------------------------------------------------------
Combined |   1,476    2.681843    .2066282    7.938395    2.276527    3.087159
---------+--------------------------------------------------------------------
    diff |           -.1502887    1.257776               -2.617509    2.316932
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -0.1195
H0: diff = 0                                     Degrees of freedom =     1474

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.4525         Pr(|T| > |t|) = 0.9049          Pr(T > t) = 0.5475

Numerator, E[Y|Z=1] - E[Y|Z=0] = .15

Denominator: denom = P[A=1|Z=1] - P[A=1|Z=0] = .063

IV estimator = 2.3809524






pa[1,2]
           c1         c2
r1  .19512195  .25783972






ey[1,2]
           c1         c2
r1   2.535729  2.6860178

------------------------------------------------- mata (type end to exit) ------------
: pa = st_matrix("pa")

: ey = st_matrix("ey")

: num = ey[1,2] - ey[1,1] 

: denom = pa[1,2] - pa[1,1]

: iv_est = num / denom 

: num
  .1502887173

: denom
  .06271777

: st_numscalar("iv_est", iv_est)

: end
--------------------------------------------------------------------------------------

2.3962701
```

## Program 16.2

- Estimating the average causal effect using the standard IV estimator via two-stage-least-squares regression
- Data from NHEFS
- Section 16.2


```stata
use ./data/nhefs-highprice, clear

/* ivregress fits the model in two stages:
- first model: qsmk = highprice
- second model: wt82_71 = predicted_qsmk */
ivregress 2sls wt82_71 (qsmk = highprice)
```

```
Instrumental variables 2SLS regression            Number of obs   =      1,476
                                                  Wald chi2(1)    =       0.01
                                                  Prob > chi2     =     0.9038
                                                  R-squared       =     0.0213
                                                  Root MSE        =     7.8508

------------------------------------------------------------------------------
     wt82_71 | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
        qsmk |    2.39627   19.82659     0.12   0.904    -36.46313    41.25567
       _cons |   2.068164   5.081652     0.41   0.684     -7.89169    12.02802
------------------------------------------------------------------------------
Endogenous: qsmk
Exogenous:  highprice
```

## Program 16.3

- Estimating the average causal effect using the standard IV estimator via an additive marginal structural model
- Data from NHEFS
- Checking one possible value of psi.
- See Chapter 14 for program that checks several values and computes 95% confidence intervals   
- Section 16.2


```stata
use ./data/nhefs-highprice, clear

gen psi = 2.396
gen hspi = wt82_71 - psi*qsmk

logit highprice hspi
```

```
Iteration 0:  Log likelihood = -187.34948  
Iteration 1:  Log likelihood = -187.34948  

Logistic regression                                     Number of obs =  1,476
                                                        LR chi2(1)    =   0.00
                                                        Prob > chi2   = 1.0000
Log likelihood = -187.34948                             Pseudo R2     = 0.0000

------------------------------------------------------------------------------
   highprice | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
        hspi |   2.75e-07   .0201749     0.00   1.000    -.0395419    .0395424
       _cons |   3.555347   .1637931    21.71   0.000     3.234319    3.876376
------------------------------------------------------------------------------
```

## Program 16.4

- Estimating the average causal effect using the standard IV estimator based on alternative proposed instruments
- Data from NHEFS
- Section 16.5


```stata
use ./data/nhefs-highprice, clear

/*Instrument cut-point: 1.6*/
replace highprice = .
replace highprice = (price82 >1.6 & price82 < .)

ivregress 2sls wt82_71 (qsmk = highprice)

/*Instrument cut-point: 1.7*/
replace highprice = .
replace highprice = (price82 >1.7 & price82 < .)

ivregress 2sls wt82_71 (qsmk = highprice)

/*Instrument cut-point: 1.8*/
replace highprice = .
replace highprice = (price82 >1.8 & price82 < .)

ivregress 2sls wt82_71 (qsmk = highprice)

/*Instrument cut-point: 1.9*/
replace highprice = .
replace highprice = (price82 >1.9 & price82 < .)

ivregress 2sls wt82_71 (qsmk = highprice)
```

```
(1,476 real changes made, 1,476 to missing)

(1,476 real changes made)


Instrumental variables 2SLS regression            Number of obs   =      1,476
                                                  Wald chi2(1)    =       0.06
                                                  Prob > chi2     =     0.8023
                                                  R-squared       =          .
                                                  Root MSE        =     18.593

------------------------------------------------------------------------------
     wt82_71 | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
        qsmk |   41.28124   164.8417     0.25   0.802    -281.8026     364.365
       _cons |  -7.890182   42.21833    -0.19   0.852    -90.63659    74.85623
------------------------------------------------------------------------------
Endogenous: qsmk
Exogenous:  highprice

(1,476 real changes made, 1,476 to missing)

(1,476 real changes made)


Instrumental variables 2SLS regression            Number of obs   =      1,476
                                                  Wald chi2(1)    =       0.05
                                                  Prob > chi2     =     0.8274
                                                  R-squared       =          .
                                                  Root MSE        =     20.577

------------------------------------------------------------------------------
     wt82_71 | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
        qsmk |  -40.91185   187.6162    -0.22   0.827    -408.6328    326.8091
       _cons |   13.15927   48.05103     0.27   0.784    -81.01901    107.3375
------------------------------------------------------------------------------
Endogenous: qsmk
Exogenous:  highprice

(1,476 real changes made, 1,476 to missing)

(1,476 real changes made)


Instrumental variables 2SLS regression            Number of obs   =      1,476
                                                  Wald chi2(1)    =       0.55
                                                  Prob > chi2     =     0.4576
                                                  R-squared       =          .
                                                  Root MSE        =      13.01

------------------------------------------------------------------------------
     wt82_71 | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
        qsmk |  -21.10342   28.40885    -0.74   0.458    -76.78374    34.57691
       _cons |   8.086377   7.283314     1.11   0.267    -6.188657    22.36141
------------------------------------------------------------------------------
Endogenous: qsmk
Exogenous:  highprice

(1,476 real changes made, 1,476 to missing)

(1,476 real changes made)


Instrumental variables 2SLS regression            Number of obs   =      1,476
                                                  Wald chi2(1)    =       0.29
                                                  Prob > chi2     =     0.5880
                                                  R-squared       =          .
                                                  Root MSE        =     10.357

------------------------------------------------------------------------------
     wt82_71 | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
        qsmk |  -12.81141   23.65099    -0.54   0.588    -59.16649    33.54368
       _cons |   5.962813   6.062956     0.98   0.325    -5.920362    17.84599
------------------------------------------------------------------------------
Endogenous: qsmk
Exogenous:  highprice
```

## Program 16.5

- Estimating the average causal effect using the standard IV estimator conditional on baseline covariates
- Data from NHEFS
- Section 16.5


```stata
use ./data/nhefs-highprice, clear

replace highprice = .
replace highprice = (price82 >1.5 & price82 < .)

ivregress 2sls wt82_71 sex race c.age c.smokeintensity ///
  c.smokeyrs i.exercise i.active c.wt7 ///
  (qsmk = highprice)
```

```
(1,476 real changes made, 1,476 to missing)

(1,476 real changes made)


Instrumental variables 2SLS regression            Number of obs   =      1,476
                                                  Wald chi2(11)   =     135.18
                                                  Prob > chi2     =     0.0000
                                                  R-squared       =     0.0622
                                                  Root MSE        =     7.6848

--------------------------------------------------------------------------------
       wt82_71 | Coefficient  Std. err.      z    P>|z|     [95% conf. interval]
---------------+----------------------------------------------------------------
          qsmk |  -1.042295   29.86522    -0.03   0.972    -59.57705    57.49246
           sex |  -1.644393   2.620115    -0.63   0.530    -6.779724    3.490938
          race |  -.1832546   4.631443    -0.04   0.968    -9.260716    8.894207
           age |    -.16364   .2395678    -0.68   0.495    -.6331844    .3059043
smokeintensity |   .0057669    .144911     0.04   0.968    -.2782534    .2897872
      smokeyrs |   .0258357   .1607639     0.16   0.872    -.2892558    .3409271
               |
      exercise |
            1  |   .4987479   2.162395     0.23   0.818    -3.739469    4.736964
            2  |   .5818337   2.174255     0.27   0.789    -3.679628    4.843296
               |
        active |
            1  |  -1.170145   .6049921    -1.93   0.053    -2.355908    .0156176
            2  |  -.5122842   1.303121    -0.39   0.694    -3.066355    2.041787
               |
          wt71 |  -.0979493    .036123    -2.71   0.007     -.168749   -.0271496
         _cons |   17.28033    2.32589     7.43   0.000     12.72167    21.83899
--------------------------------------------------------------------------------
Endogenous: qsmk
Exogenous:  sex race age smokeintensity smokeyrs 1.exercise 2.exercise
            1.active 2.active wt71 highprice
```
