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
```

```
end of do-file
```


```stata
summarize price82

/* ignore subjects with missing outcome or missing instrument for simplicity*/
foreach var of varlist wt82 price82 {
drop if `var'==.
}

/*Create categorical instrument*/
gen byte highprice  = (price82 > 1.5 & price82 < .)

/*Calculate P[Z|A=a]*/
tab highprice qsmk, row

/*Calculate P[Y|Z=z]*/
ttest wt82_71, by(highprice)

/*Final IV estimate, OPTION 1: Hand calculations*/
/*Numerator: num = E[Y|Z=1] - E[Y|Z=0] = 2.686 - 2.536 = 0.150*/
/*Denominator: denom = P[A=1|Z=1] - P[A=1|Z=0] = 0.258 - 0.195 = 0.063 */ 
/*IV estimator: E[Ya=1] - E[Ya=0] = (E[Y|Z=1]-E[Y|Z=0])/(P[A=1|Z=1]-P[A=1|Z=0]) = 0.150/0.063 = 2.397*/
display 2.686 - 2.536
display 0.258 - 0.195 
display 0.150/0.063

/*OPTION 2 2: automated calculation of instrument*/
*Calculate P[A=1|Z=z], for each value of the instrument, and store in a matrix*
quietly summarize qsmk if (highprice==0)
matrix input pa = (`r(mean)')
quietly summarize qsmk if (highprice==1)
matrix pa = (pa ,`r(mean)')
matrix list pa
*Calculate P[Y|Z=z], for each value of the instrument, and store in a second matrix*
quietly summarize wt82_71 if (highprice==0)
matrix input ey = (`r(mean)')
quietly summarize wt82_71 if (highprice==1)
matrix ey = (ey ,`r(mean)')
matrix list ey
*Using Stata's built-in matrix manipulation feature (Mata), calculate numerator, denominator and IV estimator*
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
iv_est
end
```

```
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
     price82 |      1,476    1.805989    .1301703   1.451904   2.103027

(0 observations deleted)
(90 observations deleted)



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
   Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
---------+--------------------------------------------------------------------
       0 |      41    2.535729    1.461629    9.358993   -.4183336    5.489792
       1 |   1,435    2.686018    .2084888    7.897848    2.277042    3.094994
---------+--------------------------------------------------------------------
combined |   1,476    2.681843    .2066282    7.938395    2.276527    3.087159
---------+--------------------------------------------------------------------
    diff |           -.1502887    1.257776               -2.617509    2.316932
------------------------------------------------------------------------------
    diff = mean(0) - mean(1)                                      t =  -0.1195
Ho: diff = 0                                     degrees of freedom =     1474

    Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
 Pr(T < t) = 0.4525         Pr(|T| > |t|) = 0.9049          Pr(T > t) = 0.5475

.15

.063

2.3809524






pa[1,2]
           c1         c2
r1  .19512195  .25783972






ey[1,2]
           c1         c2
r1   2.535729  2.6860178

------------------------------------------------- mata (type end to exit) ------
: pa = st_matrix("pa")

: ey = st_matrix("ey")

: num = ey[1,2] - ey[1,1] 

: denom = pa[1,2] - pa[1,1]

: iv_est = num / denom 

: num
  .1502887173

: denom
  .06271777

: iv_est

: end
--------------------------------------------------------------------------------
```

## Program 16.2
- Estimating the average causal effect using the standard IV estimator via two-stage-least-squares regression
- Data from NHEFS
- Section 16.2


```stata
/*ivregress fits the model in two stages: */
/*first model: qsmk = highprice*/
/*second model: wt82_71 = predicted_qsmk*/
ivregress 2sls wt82_71 (qsmk = highprice)
```

```
variable highprice not found
r(111);

end of do-file
r(111);
```

## Program 16.3
- Estimating the average causal effect using the standard IV estimator via an additive marginal structural model
- Data from NHEFS
- Checking one possible value of psi.
- See Chapter 14 for program that checks several values and computes 95% confidence intervals   
- Section 16.2


```stata
gen psi = 2.396
gen hspi = wt82_71 -psi*qsmk

logit highprice hspi
```

```
variable highprice not found
r(111);

end of do-file
r(111);
```

## Program 16.4
- Estimating the average causal effect using the standard IV estimator based on alternative proposed instruments
- Data from NHEFS
- Section 16.5


```stata
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
variable highprice not found
r(111);

end of do-file
r(111);
```

## Program 16.5
- Estimating the average causal effect using the standard IV estimator conditional on baseline covariates
- Data from NHEFS
- Section 16.5


```stata
replace highprice = .
replace highprice = (price82 >1.5 & price82 < .)

ivregress 2sls wt82_71 sex race c.age c.smokeintensity c.smokeyrs i.exercise i.active c.wt7 (qsmk = highprice)
```

```
variable highprice not found
r(111);

end of do-file
r(111);
```
