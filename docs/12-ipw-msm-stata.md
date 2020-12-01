# 12. IP Weighting and Marginal Structural Models: Stata{-}


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
  
## Program 12.1

- Descriptive statistics from NHEFS data (Table 12.1)


```stata
use ./data/nhefs, clear

/*Provisionally ignore subjects with missing values for follow-up weight*/
/*Sample size after exclusion: N = 1566*/
drop if wt82==.

/* Calculate mean weight change in those with and without smoking cessation*/
label define qsmk 0 "No smoking cessation" 1 "Smoking cessation"
label values qsmk qsmk
by qsmk, sort: egen years = mean(age) if age < . 
label var years "Age, years"
by qsmk, sort: egen male = mean(100 * (sex==0)) if sex < . 
label var male "Men, %"
by qsmk, sort: egen white = mean(100 * (race==0)) if race < . 
label var white "White, %"
by qsmk, sort: egen university = mean(100 * (education == 5)) if education < .
label var university "University, %"
by qsmk, sort: egen kg = mean(wt71) if wt71 < .
label var kg "Weight, kg"
by qsmk, sort: egen cigs = mean(smokeintensity) if smokeintensity < . 
label var cigs "Cigarettes/day"
by qsmk, sort: egen meansmkyrs = mean(smokeyrs) if smokeyrs < .
label var kg "Years smoking"
by qsmk, sort: egen noexer = mean(100 * (exercise == 2)) if exercise < . 
label var noexer "Little/no exercise"
by qsmk, sort: egen inactive = mean(100 * (active==2)) if active < . 
label var inactive "Inactive daily life"
qui save ./data/nhefs-formatted, replace
```

```
(63 observations deleted)

```



```stata
use ./data/nhefs-formatted, clear

/*Output table*/
foreach var of varlist years male  white university kg cigs meansmkyrs noexer inactive {
  tabdisp qsmk, cell(`var') format(%3.1f)
}
```

```
quit smoking between |
baseline and 1982    | Age, years
---------------------+-----------
No smoking cessation |       42.8
   Smoking cessation |       46.2
---------------------------------

---------------------------------
quit smoking between |
baseline and 1982    |     Men, %
---------------------+-----------
No smoking cessation |       46.6
   Smoking cessation |       54.6
---------------------------------

---------------------------------
quit smoking between |
baseline and 1982    |   White, %
---------------------+-----------
No smoking cessation |       85.4
   Smoking cessation |       91.1
---------------------------------

------------------------------------
quit smoking between |
baseline and 1982    | University, %
---------------------+--------------
No smoking cessation |           9.9
   Smoking cessation |          15.4
------------------------------------

------------------------------------
quit smoking between |
baseline and 1982    | Years smoking
---------------------+--------------
No smoking cessation |          70.3
   Smoking cessation |          72.4
------------------------------------

-------------------------------------
quit smoking between |
baseline and 1982    | Cigarettes/day
---------------------+---------------
No smoking cessation |           21.2
   Smoking cessation |           18.6
-------------------------------------

---------------------------------
quit smoking between |
baseline and 1982    | meansmkyrs
---------------------+-----------
No smoking cessation |       24.1
   Smoking cessation |       26.0
---------------------------------

-----------------------------------------
quit smoking between |
baseline and 1982    | Little/no exercise
---------------------+-------------------
No smoking cessation |               37.9
   Smoking cessation |               40.7
-----------------------------------------

------------------------------------------
quit smoking between |
baseline and 1982    | Inactive daily life
---------------------+--------------------
No smoking cessation |                 8.9
   Smoking cessation |                11.2
------------------------------------------
```

## Program 12.2

- Estimating IP weights for Section 12.2
- Data from NHEFS


```stata
use ./data/nhefs-formatted, clear

/*Fit a logistic model for the IP weights*/ 
logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 

/*Output predicted conditional probability of quitting smoking for each individual*/
predict p_qsmk, pr

/*Generate nonstabilized weights as P(A=1|covariates) if A = 1 and 1-P(A=1|covariates) if A = 0*/
gen w=.
replace w=1/p_qsmk if qsmk==1
replace w=1/(1-p_qsmk) if qsmk==0
/*Check the mean of the weights; we expect it to be close to 2.0*/
summarize w

/*Fit marginal structural model in the pseudopopulation*/
/*Weights assigned using pweight = w*/
/*Robust standard errors using cluster() option where 'seqn' is the ID variable*/
regress wt82_71 qsmk [pweight=w], cluster(seqn) 
```

```
Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -839.70016  
Iteration 2:   log likelihood = -838.45045  
Iteration 3:   log likelihood = -838.44842  
Iteration 4:   log likelihood = -838.44842  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(18)       =     109.16
                                                Prob > chi2       =     0.0000
Log likelihood = -838.44842                     Pseudo R2         =     0.0611

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         sex |  -.5274782   .1540497    -3.42   0.001      -.82941   -.2255463
        race |  -.8392636   .2100668    -4.00   0.000    -1.250987   -.4275404
         age |   .1212052   .0512663     2.36   0.018     .0207251    .2216853
             |
 c.age#c.age |  -.0008246   .0005361    -1.54   0.124    -.0018753    .0002262
             |
   education |
          1  |  -.4759606   .2262238    -2.10   0.035    -.9193511   -.0325701
          2  |  -.5047361    .217597    -2.32   0.020    -.9312184   -.0782538
          3  |  -.3895288   .1914353    -2.03   0.042    -.7647351   -.0143226
          4  |  -.4123596   .2772868    -1.49   0.137    -.9558318    .1311126
             |
smokeinten~y |  -.0772704   .0152499    -5.07   0.000    -.1071596   -.0473812
             |
          c. |
smokeinten~y#|
          c. |
smokeinten~y |   .0010451   .0002866     3.65   0.000     .0004835    .0016068
             |
    smokeyrs |  -.0735966   .0277775    -2.65   0.008    -.1280395   -.0191538
             |
  c.smokeyrs#|
  c.smokeyrs |   .0008441   .0004632     1.82   0.068    -.0000637    .0017519
             |
    exercise |
          0  |   -.395704   .1872401    -2.11   0.035    -.7626878   -.0287201
          1  |  -.0408635   .1382674    -0.30   0.768    -.3118627    .2301357
             |
      active |
          0  |   -.176784   .2149721    -0.82   0.411    -.5981215    .2445535
          1  |  -.1448395   .2111472    -0.69   0.493    -.5586806    .2690015
             |
        wt71 |  -.0152357   .0263161    -0.58   0.563    -.0668144     .036343
             |
      c.wt71#|
      c.wt71 |   .0001352   .0001632     0.83   0.407    -.0001846     .000455
             |
       _cons |   -1.19407   1.398493    -0.85   0.393    -3.935066    1.546925
------------------------------------------------------------------------------


(1,566 missing values generated)

(403 real changes made)

(1,163 real changes made)

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
           w |      1,566    1.996284    1.474787   1.053742   16.70009

(sum of wgt is 3,126.18084549904)

Linear regression                               Number of obs     =      1,566
                                                F(1, 1565)        =      42.81
                                                Prob > F          =     0.0000
                                                R-squared         =     0.0435
                                                Root MSE          =     8.0713

                               (Std. Err. adjusted for 1,566 clusters in seqn)
------------------------------------------------------------------------------
             |               Robust
     wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        qsmk |   3.440535   .5258294     6.54   0.000     2.409131     4.47194
       _cons |   1.779978   .2248742     7.92   0.000     1.338892    2.221065
------------------------------------------------------------------------------
```

## Program 12.3

- Estimating stabilized IP weights for Section 12.3
- Data from NHEFS


```stata
use ./data/nhefs-formatted, clear

/*Fit a logistic model for the denominator of the IP weights and predict the conditional probability of smoking*/ 
logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71  
predict pd_qsmk, pr

/*Fit a logistic model for the numerator of ip weights and predict Pr(A=1) */ 
logit qsmk 
predict pn_qsmk, pr

/*Generate stabilized weights as f(A)/f(A|L)*/
gen sw_a=.
replace sw_a=pn_qsmk/pd_qsmk if qsmk==1
replace sw_a=(1-pn_qsmk)/(1-pd_qsmk) if qsmk==0

/*Check distribution of the stabilized weights*/
summarize sw_a

/*Fit marginal structural model in the pseudopopulation*/
regress wt82_71 qsmk [pweight=sw_a], cluster(seqn) 

/**********************************************************
FINE POINT 12.2
Checking positivity
**********************************************************/

/*Check for missing values within strata of covariates, for example: */
tab age qsmk if race==0 & sex==1 & wt82!=.
tab age qsmk if race==1 & sex==1 & wt82!=.
```

```
Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -839.70016  
Iteration 2:   log likelihood = -838.45045  
Iteration 3:   log likelihood = -838.44842  
Iteration 4:   log likelihood = -838.44842  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(18)       =     109.16
                                                Prob > chi2       =     0.0000
Log likelihood = -838.44842                     Pseudo R2         =     0.0611

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         sex |  -.5274782   .1540497    -3.42   0.001      -.82941   -.2255463
        race |  -.8392636   .2100668    -4.00   0.000    -1.250987   -.4275404
         age |   .1212052   .0512663     2.36   0.018     .0207251    .2216853
             |
 c.age#c.age |  -.0008246   .0005361    -1.54   0.124    -.0018753    .0002262
             |
   education |
          1  |  -.4759606   .2262238    -2.10   0.035    -.9193511   -.0325701
          2  |  -.5047361    .217597    -2.32   0.020    -.9312184   -.0782538
          3  |  -.3895288   .1914353    -2.03   0.042    -.7647351   -.0143226
          4  |  -.4123596   .2772868    -1.49   0.137    -.9558318    .1311126
             |
smokeinten~y |  -.0772704   .0152499    -5.07   0.000    -.1071596   -.0473812
             |
          c. |
smokeinten~y#|
          c. |
smokeinten~y |   .0010451   .0002866     3.65   0.000     .0004835    .0016068
             |
    smokeyrs |  -.0735966   .0277775    -2.65   0.008    -.1280395   -.0191538
             |
  c.smokeyrs#|
  c.smokeyrs |   .0008441   .0004632     1.82   0.068    -.0000637    .0017519
             |
    exercise |
          0  |   -.395704   .1872401    -2.11   0.035    -.7626878   -.0287201
          1  |  -.0408635   .1382674    -0.30   0.768    -.3118627    .2301357
             |
      active |
          0  |   -.176784   .2149721    -0.82   0.411    -.5981215    .2445535
          1  |  -.1448395   .2111472    -0.69   0.493    -.5586806    .2690015
             |
        wt71 |  -.0152357   .0263161    -0.58   0.563    -.0668144     .036343
             |
      c.wt71#|
      c.wt71 |   .0001352   .0001632     0.83   0.407    -.0001846     .000455
             |
       _cons |   -1.19407   1.398493    -0.85   0.393    -3.935066    1.546925
------------------------------------------------------------------------------



Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -893.02712  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(0)        =       0.00
                                                Prob > chi2       =          .
Log likelihood = -893.02712                     Pseudo R2         =     0.0000

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       _cons |  -1.059822   .0578034   -18.33   0.000    -1.173114    -.946529
------------------------------------------------------------------------------


(1,566 missing values generated)

(403 real changes made)

(1,163 real changes made)

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        sw_a |      1,566    .9988444    .2882233   .3312489   4.297662

(sum of wgt is 1,564.19025221467)

Linear regression                               Number of obs     =      1,566
                                                F(1, 1565)        =      42.81
                                                Prob > F          =     0.0000
                                                R-squared         =     0.0359
                                                Root MSE          =     7.7972

                               (Std. Err. adjusted for 1,566 clusters in seqn)
------------------------------------------------------------------------------
             |               Robust
     wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        qsmk |   3.440535   .5258294     6.54   0.000     2.409131     4.47194
       _cons |   1.779978   .2248742     7.92   0.000     1.338892    2.221065
------------------------------------------------------------------------------

           | quit smoking between
           |   baseline and 1982
       age | No smokin  Smoking c |     Total
-----------+----------------------+----------
        25 |        24          3 |        27 
        26 |        14          5 |        19 
        27 |        18          2 |        20 
        28 |        20          5 |        25 
        29 |        15          4 |        19 
        30 |        14          5 |        19 
        31 |        11          5 |        16 
        32 |        14          7 |        21 
        33 |        12          3 |        15 
        34 |        22          5 |        27 
        35 |        16          5 |        21 
        36 |        13          3 |        16 
        37 |        14          1 |        15 
        38 |         6          2 |         8 
        39 |        19          4 |        23 
        40 |        10          4 |        14 
        41 |        13          3 |        16 
        42 |        16          3 |        19 
        43 |        14          3 |        17 
        44 |         9          4 |        13 
        45 |        12          5 |        17 
        46 |        19          4 |        23 
        47 |        19          4 |        23 
        48 |        19          4 |        23 
        49 |        11          3 |        14 
        50 |        18          4 |        22 
        51 |         9          3 |        12 
        52 |        11          3 |        14 
        53 |        11          4 |        15 
        54 |        17          9 |        26 
        55 |         9          4 |        13 
        56 |         8          7 |        15 
        57 |         9          2 |        11 
        58 |         8          4 |        12 
        59 |         5          4 |         9 
        60 |         5          4 |         9 
        61 |         5          2 |         7 
        62 |         6          5 |        11 
        63 |         3          3 |         6 
        64 |         7          1 |         8 
        65 |         3          2 |         5 
        66 |         4          0 |         4 
        67 |         2          0 |         2 
        69 |         6          2 |         8 
        70 |         2          1 |         3 
        71 |         0          1 |         1 
        72 |         2          2 |         4 
        74 |         0          1 |         1 
-----------+----------------------+----------
     Total |       524        164 |       688 

           | quit smoking between
           |   baseline and 1982
       age | No smokin  Smoking c |     Total
-----------+----------------------+----------
        25 |         3          1 |         4 
        26 |         3          0 |         3 
        28 |         3          1 |         4 
        29 |         1          0 |         1 
        30 |         4          0 |         4 
        31 |         3          0 |         3 
        32 |         8          0 |         8 
        33 |         2          0 |         2 
        34 |         2          1 |         3 
        35 |         3          0 |         3 
        36 |         5          0 |         5 
        37 |         3          1 |         4 
        38 |         4          2 |         6 
        39 |         1          1 |         2 
        40 |         2          2 |         4 
        41 |         3          0 |         3 
        42 |         3          0 |         3 
        43 |         4          2 |         6 
        44 |         3          0 |         3 
        45 |         1          3 |         4 
        46 |         5          0 |         5 
        47 |         3          0 |         3 
        48 |         4          0 |         4 
        49 |         1          1 |         2 
        50 |         2          0 |         2 
        51 |         4          0 |         4 
        52 |         1          0 |         1 
        53 |         2          0 |         2 
        54 |         2          0 |         2 
        55 |         3          0 |         3 
        56 |         2          1 |         3 
        57 |         2          1 |         3 
        61 |         1          1 |         2 
        67 |         1          0 |         1 
        68 |         1          0 |         1 
        69 |         2          0 |         2 
        70 |         0          1 |         1 
-----------+----------------------+----------
     Total |        97         19 |       116 
```

## Program 12.4

- Estimating the parameters of a marginal structural mean model with a continuous treatment Data from NHEFS
- Section 12.4


```stata
use ./data/nhefs-formatted, clear

* drop sw_a

/*Analysis restricted to subjects reporting <=25 cig/day at baseline: N = 1162*/
keep if smokeintensity <=25

/*Fit a linear model for the denominator of the IP weights and calculate the mean expected smoking intensity*/ 
regress smkintensity82_71 sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71
quietly predict p_den

/*Generate the denisty of the denomiator expectation using the mean expected smoking intensity and the residuals, assuming a normal distribution*/
/*Note: The regress command in STATA saves the root mean squared error for the immediate regression as e(rmse), thus there is no need to calculate it again. */
gen dens_den = normalden(smkintensity82_71, p_den, e(rmse))

/*Fit a linear model for the numerator of ip weights, calculate the mean expected value, and generate the density*/
quietly regress smkintensity82_71
quietly predict p_num
gen dens_num = normalden( smkintensity82_71, p_num, e(rmse))

/*Generate the final stabilized weights from the estimated numerator and denominator, and check the weights distribution*/
gen sw_a=dens_num/dens_den
summarize sw_a

/*Fit a marginal structural model in the pseudopopulation*/
regress wt82_71  c.smkintensity82_71##c.smkintensity82_71 [pweight=sw_a], cluster(seqn)

/*Output the estimated mean Y value when smoke intensity is unchanged from baseline to 1982 */
lincom _b[_cons]

/*Output the estimated mean Y value when smoke intensity increases by 20 from baseline to 1982*/
lincom _b[_cons] + 20*_b[smkintensity82_71 ] +400*_b[c.smkintensity82_71#c.smkintensity82_71]
```

```
(404 observations deleted)

      Source |       SS           df       MS      Number of obs   =     1,162
-------------+----------------------------------   F(18, 1143)     =      5.39
       Model |  9956.95654        18  553.164252   Prob > F        =    0.0000
    Residual |   117260.18     1,143  102.589834   R-squared       =    0.0783
-------------+----------------------------------   Adj R-squared   =    0.0638
       Total |  127217.137     1,161  109.575484   Root MSE        =    10.129

------------------------------------------------------------------------------
smkintens~71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         sex |   1.087021   .7425694     1.46   0.144    -.3699308    2.543973
        race |   .2319789   .8434739     0.28   0.783    -1.422952     1.88691
         age |  -.8099902   .2555388    -3.17   0.002    -1.311368   -.3086124
             |
 c.age#c.age |   .0066545   .0026849     2.48   0.013     .0013865    .0119224
             |
   education |
          1  |   1.508097   1.184063     1.27   0.203    -.8150843    3.831278
          2  |    2.02692   1.133772     1.79   0.074    -.1975876    4.251428
          3  |   2.240314   1.022556     2.19   0.029     .2340167    4.246611
          4  |   2.528767    1.44702     1.75   0.081    -.3103458     5.36788
             |
smokeinten~y |  -.3589684   .2246653    -1.60   0.110     -.799771    .0818342
             |
          c. |
smokeinten~y#|
          c. |
smokeinten~y |   .0019582   .0085753     0.23   0.819    -.0148668    .0187832
             |
    smokeyrs |   .3857088   .1416765     2.72   0.007     .1077336    .6636841
             |
  c.smokeyrs#|
  c.smokeyrs |  -.0054871   .0023837    -2.30   0.022    -.0101641   -.0008101
             |
    exercise |
          0  |   1.996904   .9080421     2.20   0.028      .215288    3.778521
          1  |    .988812   .6929239     1.43   0.154    -.3707334    2.348357
             |
      active |
          0  |   .8451341   1.098573     0.77   0.442    -1.310312    3.000581
          1  |    .800114    1.08438     0.74   0.461    -1.327485    2.927712
             |
        wt71 |  -.0656882    .136955    -0.48   0.632    -.3343996    .2030232
             |
      c.wt71#|
      c.wt71 |   .0005711    .000877     0.65   0.515    -.0011496    .0022918
             |
       _cons |   16.86761   7.109189     2.37   0.018      2.91909    30.81614
------------------------------------------------------------------------------

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        sw_a |      1,162    .9968057    .3222937   .1938336   5.102339

(sum of wgt is 1,158.28818286955)

Linear regression                               Number of obs     =      1,162
                                                F(2, 1161)        =      12.75
                                                Prob > F          =     0.0000
                                                R-squared         =     0.0233
                                                Root MSE          =     7.7864

                               (Std. Err. adjusted for 1,162 clusters in seqn)
------------------------------------------------------------------------------
             |               Robust
     wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
smkintens~71 |  -.1089889   .0315762    -3.45   0.001    -.1709417   -.0470361
             |
          c. |
smkintens~71#|
          c. |
smkintens~71 |   .0026949   .0024203     1.11   0.266    -.0020537    .0074436
             |
       _cons |   2.004525    .295502     6.78   0.000     1.424747    2.584302
------------------------------------------------------------------------------


 ( 1)  _cons = 0

------------------------------------------------------------------------------
     wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         (1) |   2.004525    .295502     6.78   0.000     1.424747    2.584302
------------------------------------------------------------------------------


 ( 1)  20*smkintensity82_71 + 400*c.smkintensity82_71#c.smkintensity82_71 +
       _cons = 0

------------------------------------------------------------------------------
     wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         (1) |   .9027234   1.310533     0.69   0.491    -1.668554    3.474001
------------------------------------------------------------------------------
```

## Program 12.5

- Estimating the parameters of a marginal structural logistic model
- Data from NHEFS
- Section 12.4


```stata
use ./data/nhefs, clear

/*Provisionally ignore subjects with missing values for follow-up weight*/
/*Sample size after exclusion: N = 1566*/
drop if wt82==.

/*Estimate the stabilized weights for quitting smoking as in PROGRAM 12.3*/
/*Fit a logistic model for the denominator of the IP weights and predict the conditional probability of smoking*/ 
logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71  
predict pd_qsmk, pr
/*Fit a logistic model for the numerator of ip weights and predict Pr(A=1) */ 
logit qsmk 
predict pn_qsmk, pr
/*Generate stabilized weights as f(A)/f(A|L)*/
gen sw_a=.
replace sw_a=pn_qsmk/pd_qsmk if qsmk==1
replace sw_a=(1-pn_qsmk)/(1-pd_qsmk) if qsmk==0
summarize sw_a

/*Fit marginal structural model in the pseudopopulation*/
/*NOTE: Stata has two commands for logistic regression, logit and logistic*/
/*Using logistic allows us to output the odds ratios directly*/
/*We can also output odds ratios from the logit command using the or option (default logit output is regression coefficients*/
logistic death qsmk [pweight=sw_a], cluster(seqn) 
```

```
(63 observations deleted)


Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -839.70016  
Iteration 2:   log likelihood = -838.45045  
Iteration 3:   log likelihood = -838.44842  
Iteration 4:   log likelihood = -838.44842  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(18)       =     109.16
                                                Prob > chi2       =     0.0000
Log likelihood = -838.44842                     Pseudo R2         =     0.0611

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         sex |  -.5274782   .1540497    -3.42   0.001      -.82941   -.2255463
        race |  -.8392636   .2100668    -4.00   0.000    -1.250987   -.4275404
         age |   .1212052   .0512663     2.36   0.018     .0207251    .2216853
             |
 c.age#c.age |  -.0008246   .0005361    -1.54   0.124    -.0018753    .0002262
             |
   education |
          1  |  -.4759606   .2262238    -2.10   0.035    -.9193511   -.0325701
          2  |  -.5047361    .217597    -2.32   0.020    -.9312184   -.0782538
          3  |  -.3895288   .1914353    -2.03   0.042    -.7647351   -.0143226
          4  |  -.4123596   .2772868    -1.49   0.137    -.9558318    .1311126
             |
smokeinten~y |  -.0772704   .0152499    -5.07   0.000    -.1071596   -.0473812
             |
          c. |
smokeinten~y#|
          c. |
smokeinten~y |   .0010451   .0002866     3.65   0.000     .0004835    .0016068
             |
    smokeyrs |  -.0735966   .0277775    -2.65   0.008    -.1280395   -.0191538
             |
  c.smokeyrs#|
  c.smokeyrs |   .0008441   .0004632     1.82   0.068    -.0000637    .0017519
             |
    exercise |
          0  |   -.395704   .1872401    -2.11   0.035    -.7626878   -.0287201
          1  |  -.0408635   .1382674    -0.30   0.768    -.3118627    .2301357
             |
      active |
          0  |   -.176784   .2149721    -0.82   0.411    -.5981215    .2445535
          1  |  -.1448395   .2111472    -0.69   0.493    -.5586806    .2690015
             |
        wt71 |  -.0152357   .0263161    -0.58   0.563    -.0668144     .036343
             |
      c.wt71#|
      c.wt71 |   .0001352   .0001632     0.83   0.407    -.0001846     .000455
             |
       _cons |   -1.19407   1.398493    -0.85   0.393    -3.935066    1.546925
------------------------------------------------------------------------------



Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -893.02712  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(0)        =      -0.00
                                                Prob > chi2       =          .
Log likelihood = -893.02712                     Pseudo R2         =    -0.0000

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       _cons |  -1.059822   .0578034   -18.33   0.000    -1.173114    -.946529
------------------------------------------------------------------------------


(1,566 missing values generated)

(403 real changes made)

(1,163 real changes made)

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        sw_a |      1,566    .9988444    .2882233   .3312489   4.297662


Logistic regression                             Number of obs     =      1,566
                                                Wald chi2(1)      =       0.04
                                                Prob > chi2       =     0.8482
Log pseudolikelihood = -749.11596               Pseudo R2         =     0.0000

                               (Std. Err. adjusted for 1,566 clusters in seqn)
------------------------------------------------------------------------------
             |               Robust
       death | Odds Ratio   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        qsmk |   1.030578   .1621842     0.19   0.848     .7570517    1.402931
       _cons |   .2252711   .0177882   -18.88   0.000     .1929707    .2629781
------------------------------------------------------------------------------
Note: _cons estimates baseline odds.
```

## Program 12.6

- Assessing effect modification by sex using a marginal structural mean model
- Data from NHEFS
- Section 12.5


```stata
use ./data/nhefs, clear

* drop pd_qsmk pn_qsmk sw_a

/*Check distribution of sex*/
tab sex

/*Fit logistc model for the denominator of IP weights, as in PROGRAM 12.3 */
logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 
predict pd_qsmk, pr

/*Fit logistic model for the numerator of IP weights, no including sex */
logit qsmk sex
predict pn_qsmk, pr

/*Generate IP weights as before*/
gen sw_a=.
replace sw_a=pn_qsmk/pd_qsmk if qsmk==1
replace sw_a=(1-pn_qsmk)/(1-pd_qsmk) if qsmk==0

summarize sw_a

/*Fit marginal structural model in the pseudopopulation, including interaction term between quitting smoking and sex*/
regress wt82_71 qsmk##sex [pw=sw_a], cluster(seqn)
```

```
        sex |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |        799       49.05       49.05
          1 |        830       50.95      100.00
------------+-----------------------------------
      Total |      1,629      100.00


Iteration 0:   log likelihood = -938.14308  
Iteration 1:   log likelihood = -884.53806  
Iteration 2:   log likelihood = -883.35064  
Iteration 3:   log likelihood = -883.34876  
Iteration 4:   log likelihood = -883.34876  

Logistic regression                             Number of obs     =      1,629
                                                LR chi2(18)       =     109.59
                                                Prob > chi2       =     0.0000
Log likelihood = -883.34876                     Pseudo R2         =     0.0584

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         sex |  -.5075218   .1482316    -3.42   0.001    -.7980505   -.2169932
        race |  -.8502312   .2058722    -4.13   0.000    -1.253733   -.4467292
         age |   .1030132   .0488996     2.11   0.035     .0071718    .1988547
             |
 c.age#c.age |  -.0006052   .0005074    -1.19   0.233    -.0015998    .0003893
             |
   education |
          1  |  -.3796632   .2203948    -1.72   0.085     -.811629    .0523026
          2  |  -.4779835   .2141771    -2.23   0.026    -.8977629   -.0582041
          3  |  -.3639645   .1885776    -1.93   0.054    -.7335698    .0056409
          4  |  -.4221892   .2717235    -1.55   0.120    -.9547574     .110379
             |
smokeinten~y |  -.0651561   .0147589    -4.41   0.000    -.0940831   -.0362292
             |
          c. |
smokeinten~y#|
          c. |
smokeinten~y |   .0008461   .0002758     3.07   0.002     .0003054    .0013867
             |
    smokeyrs |  -.0733708   .0269958    -2.72   0.007    -.1262816     -.02046
             |
  c.smokeyrs#|
  c.smokeyrs |   .0008384   .0004435     1.89   0.059    -.0000307    .0017076
             |
    exercise |
          0  |  -.3550517   .1799293    -1.97   0.048    -.7077067   -.0023967
          1  |    -.06364   .1351256    -0.47   0.638    -.3284812    .2012013
             |
      active |
          0  |  -.0683123   .2087269    -0.33   0.743    -.4774095    .3407849
          1  |   -.057437   .2039967    -0.28   0.778    -.4572632    .3423892
             |
        wt71 |  -.0128478   .0222829    -0.58   0.564    -.0565214    .0308258
             |
      c.wt71#|
      c.wt71 |   .0001209   .0001352     0.89   0.371     -.000144    .0003859
             |
       _cons |  -1.185875   1.263142    -0.94   0.348    -3.661588    1.289838
------------------------------------------------------------------------------



Iteration 0:   log likelihood = -938.14308  
Iteration 1:   log likelihood = -933.49896  
Iteration 2:   log likelihood = -933.49126  
Iteration 3:   log likelihood = -933.49126  

Logistic regression                             Number of obs     =      1,629
                                                LR chi2(1)        =       9.30
                                                Prob > chi2       =     0.0023
Log likelihood = -933.49126                     Pseudo R2         =     0.0050

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         sex |  -.3441893   .1131341    -3.04   0.002     -.565928   -.1224506
       _cons |  -.8634417   .0774517   -11.15   0.000    -1.015244   -.7116391
------------------------------------------------------------------------------


(1,629 missing values generated)

(428 real changes made)

(1,201 real changes made)

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        sw_a |      1,629    .9991318    .2636164   .2901148   3.683352

(sum of wgt is 1,562.01032829285)

Linear regression                               Number of obs     =      1,566
                                                F(3, 1565)        =      16.31
                                                Prob > F          =     0.0000
                                                R-squared         =     0.0379
                                                Root MSE          =     7.8024

                               (Std. Err. adjusted for 1,566 clusters in seqn)
------------------------------------------------------------------------------
             |               Robust
     wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
             |
    qsmk#sex |
        1 1  |   -.161224   1.036143    -0.16   0.876      -2.1936    1.871152
             |
       _cons |   1.759045   .3102511     5.67   0.000     1.150494    2.367597
------------------------------------------------------------------------------
```

## Program 12.7

- Estimating IP weights to adjust for selection bias due to censoring
- Data from NHEFS
- Section 12.6


```stata
use ./data/nhefs, clear

/*Analysis including all individuals regardless of missing wt82 status: N=1629*/
/*Generate censoring indicator: C = 1 if wt82 missing*/
gen byte cens = (wt82 == .)

/*Check distribution of censoring by quitting smoking and baseline weight*/
tab cens qsmk, column
bys cens: summarize wt71

/*Fit logistic regression model for the  denominator of IP weight for A*/
logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 
predict pd_qsmk, pr

/*Fit logistic regression model for the  numerator of IP weights for A*/
logit qsmk
predict pn_qsmk, pr

/*Fit logistic regression model for the  denominator of IP weights for C, including quitting smoking*/
logit cens qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 
predict pd_cens, pr

/*Fit logistic regression model for the  numerator of IP weights for C, including quitting smoking */
logit cens qsmk
predict pn_cens, pr

/*Generate the stabilized weights for A (sw_a)*/
gen sw_a=.
replace sw_a=pn_qsmk/pd_qsmk if qsmk==1
replace sw_a=(1-pn_qsmk)/(1-pd_qsmk) if qsmk==0

/*Generate the stabilized weights for C (sw_c)*/
/*NOTE: the conditional probability estimates generated by our logistic models for C represent the conditional probability of being censored (C=1)*/
/*We want weights for the conditional probability of bing uncensored, Pr(C=0|A,L)*/
gen sw_c=.
replace sw_c=(1-pn_cens)/(1-pd_cens) if cens==0

/*Generate the final stabilized weights and check distribution*/
gen sw=sw_a*sw_c
summarize sw

/*Fit marginal structural model in the pseudopopulation*/
regress wt82_71 qsmk [pw=sw], cluster(seqn)
```

```
| Key               |
|-------------------|
|     frequency     |
| column percentage |
+-------------------+

           | quit smoking between
           |   baseline and 1982
      cens |         0          1 |     Total
-----------+----------------------+----------
         0 |     1,163        403 |     1,566 
           |     96.84      94.16 |     96.13 
-----------+----------------------+----------
         1 |        38         25 |        63 
           |      3.16       5.84 |      3.87 
-----------+----------------------+----------
     Total |     1,201        428 |     1,629 
           |    100.00     100.00 |    100.00 


-------------------------------------------------------------------------------
-> cens = 0

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        wt71 |      1,566    70.83092     15.3149      39.58     151.73

-------------------------------------------------------------------------------
-> cens = 1

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        wt71 |         63    76.55079     23.3326      36.17     169.19



Iteration 0:   log likelihood = -938.14308  
Iteration 1:   log likelihood = -884.53806  
Iteration 2:   log likelihood = -883.35064  
Iteration 3:   log likelihood = -883.34876  
Iteration 4:   log likelihood = -883.34876  

Logistic regression                             Number of obs     =      1,629
                                                LR chi2(18)       =     109.59
                                                Prob > chi2       =     0.0000
Log likelihood = -883.34876                     Pseudo R2         =     0.0584

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         sex |  -.5075218   .1482316    -3.42   0.001    -.7980505   -.2169932
        race |  -.8502312   .2058722    -4.13   0.000    -1.253733   -.4467292
         age |   .1030132   .0488996     2.11   0.035     .0071718    .1988547
             |
 c.age#c.age |  -.0006052   .0005074    -1.19   0.233    -.0015998    .0003893
             |
   education |
          1  |  -.3796632   .2203948    -1.72   0.085     -.811629    .0523026
          2  |  -.4779835   .2141771    -2.23   0.026    -.8977629   -.0582041
          3  |  -.3639645   .1885776    -1.93   0.054    -.7335698    .0056409
          4  |  -.4221892   .2717235    -1.55   0.120    -.9547574     .110379
             |
smokeinten~y |  -.0651561   .0147589    -4.41   0.000    -.0940831   -.0362292
             |
          c. |
smokeinten~y#|
          c. |
smokeinten~y |   .0008461   .0002758     3.07   0.002     .0003054    .0013867
             |
    smokeyrs |  -.0733708   .0269958    -2.72   0.007    -.1262816     -.02046
             |
  c.smokeyrs#|
  c.smokeyrs |   .0008384   .0004435     1.89   0.059    -.0000307    .0017076
             |
    exercise |
          0  |  -.3550517   .1799293    -1.97   0.048    -.7077067   -.0023967
          1  |    -.06364   .1351256    -0.47   0.638    -.3284812    .2012013
             |
      active |
          0  |  -.0683123   .2087269    -0.33   0.743    -.4774095    .3407849
          1  |   -.057437   .2039967    -0.28   0.778    -.4572632    .3423892
             |
        wt71 |  -.0128478   .0222829    -0.58   0.564    -.0565214    .0308258
             |
      c.wt71#|
      c.wt71 |   .0001209   .0001352     0.89   0.371     -.000144    .0003859
             |
       _cons |  -1.185875   1.263142    -0.94   0.348    -3.661588    1.289838
------------------------------------------------------------------------------



Iteration 0:   log likelihood = -938.14308  
Iteration 1:   log likelihood = -938.14308  

Logistic regression                             Number of obs     =      1,629
                                                LR chi2(0)        =       0.00
                                                Prob > chi2       =          .
Log likelihood = -938.14308                     Pseudo R2         =     0.0000

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       _cons |  -1.031787   .0562947   -18.33   0.000    -1.142122   -.9214511
------------------------------------------------------------------------------



Iteration 0:   log likelihood = -266.67873  
Iteration 1:   log likelihood = -238.48654  
Iteration 2:   log likelihood = -232.82848  
Iteration 3:   log likelihood = -232.68043  
Iteration 4:   log likelihood = -232.67999  
Iteration 5:   log likelihood = -232.67999  

Logistic regression                             Number of obs     =      1,629
                                                LR chi2(19)       =      68.00
                                                Prob > chi2       =     0.0000
Log likelihood = -232.67999                     Pseudo R2         =     0.1275

------------------------------------------------------------------------------
        cens |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        qsmk |   .5168674   .2877162     1.80   0.072    -.0470459    1.080781
         sex |   .0573131   .3302775     0.17   0.862     -.590019    .7046452
        race |  -.0122715   .4524888    -0.03   0.978    -.8991332    .8745902
         age |  -.2697293   .1174647    -2.30   0.022    -.4999559   -.0395027
             |
 c.age#c.age |   .0028837   .0011135     2.59   0.010     .0007012    .0050661
             |
   education |
          1  |   .3823818   .5601808     0.68   0.495    -.7155523    1.480316
          2  |  -.0584066   .5749586    -0.10   0.919    -1.185305    1.068491
          3  |   .2176937   .5225008     0.42   0.677    -.8063891    1.241776
          4  |   .5208288   .6678735     0.78   0.435    -.7881792    1.829837
             |
smokeinten~y |   .0157119   .0347319     0.45   0.651    -.0523614    .0837851
             |
          c. |
smokeinten~y#|
          c. |
smokeinten~y |  -.0001133   .0006058    -0.19   0.852    -.0013007    .0010742
             |
    smokeyrs |   .0785973   .0749576     1.05   0.294    -.0683169    .2255116
             |
  c.smokeyrs#|
  c.smokeyrs |  -.0005569   .0010318    -0.54   0.589    -.0025791    .0014653
             |
    exercise |
          0  |    .583989   .3723133     1.57   0.117    -.1457317     1.31371
          1  |  -.3874824   .3439133    -1.13   0.260     -1.06154    .2865754
             |
      active |
          0  |  -.7065829   .3964577    -1.78   0.075    -1.483626    .0704599
          1  |  -.9540614   .3893181    -2.45   0.014    -1.717111   -.1910119
             |
        wt71 |  -.0878871   .0400115    -2.20   0.028    -.1663082   -.0094659
             |
      c.wt71#|
      c.wt71 |   .0006351   .0002257     2.81   0.005     .0001927    .0010775
             |
       _cons |   3.754678   2.651222     1.42   0.157    -1.441622    8.950978
------------------------------------------------------------------------------



Iteration 0:   log likelihood = -266.67873  
Iteration 1:   log likelihood = -264.00252  
Iteration 2:   log likelihood = -263.88028  
Iteration 3:   log likelihood = -263.88009  
Iteration 4:   log likelihood = -263.88009  

Logistic regression                             Number of obs     =      1,629
                                                LR chi2(1)        =       5.60
                                                Prob > chi2       =     0.0180
Log likelihood = -263.88009                     Pseudo R2         =     0.0105

------------------------------------------------------------------------------
        cens |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        qsmk |   .6411113   .2639262     2.43   0.015     .1238255    1.158397
       _cons |  -3.421172   .1648503   -20.75   0.000    -3.744273   -3.098071
------------------------------------------------------------------------------


(1,629 missing values generated)

(428 real changes made)

(1,201 real changes made)

(1,629 missing values generated)

(1,566 real changes made)

(63 missing values generated)

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
          sw |      1,566    .9962351    .2819583   .3546469   4.093113

(sum of wgt is 1,560.10419079661)

Linear regression                               Number of obs     =      1,566
                                                F(1, 1565)        =      44.19
                                                Prob > F          =     0.0000
                                                R-squared         =     0.0363
                                                Root MSE          =     7.8652

                               (Std. Err. adjusted for 1,566 clusters in seqn)
------------------------------------------------------------------------------
             |               Robust
     wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        qsmk |   3.496493   .5259796     6.65   0.000     2.464794    4.528192
       _cons |    1.66199   .2328986     7.14   0.000     1.205164    2.118816
------------------------------------------------------------------------------
```
