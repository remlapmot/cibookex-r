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
```

```
end of do-file
```


```stata
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

-------------------------------------------------------------------------------
         qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
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
smokeintens~y |  -.0772704   .0152499    -5.07   0.000    -.1071596   -.0473812
              |
           c. |
smokeintens~y#|
           c. |
smokeintens~y |   .0010451   .0002866     3.65   0.000     .0004835    .0016068
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
c.wt71#c.wt71 |   .0001352   .0001632     0.83   0.407    -.0001846     .000455
              |
        _cons |   -1.19407   1.398493    -0.85   0.393    -3.935066    1.546925
-------------------------------------------------------------------------------


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
> onditional probability of smoking*/ 

Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -839.70016  
Iteration 2:   log likelihood = -838.45045  
Iteration 3:   log likelihood = -838.44842  
Iteration 4:   log likelihood = -838.44842  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(18)       =     109.16
                                                Prob > chi2       =     0.0000
Log likelihood = -838.44842                     Pseudo R2         =     0.0611

-------------------------------------------------------------------------------
         qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
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
smokeintens~y |  -.0772704   .0152499    -5.07   0.000    -.1071596   -.0473812
              |
           c. |
smokeintens~y#|
           c. |
smokeintens~y |   .0010451   .0002866     3.65   0.000     .0004835    .0016068
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
c.wt71#c.wt71 |   .0001352   .0001632     0.83   0.407    -.0001846     .000455
              |
        _cons |   -1.19407   1.398493    -0.85   0.393    -3.935066    1.546925
-------------------------------------------------------------------------------



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

-------------------------------------------------------------------------------
smkintensi~71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
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
smokeintens~y |  -.3589684   .2246653    -1.60   0.110     -.799771    .0818342
              |
           c. |
smokeintens~y#|
           c. |
smokeintens~y |   .0019582   .0085753     0.23   0.819    -.0148668    .0187832
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
c.wt71#c.wt71 |   .0005711    .000877     0.65   0.515    -.0011496    .0022918
              |
        _cons |   16.86761   7.109189     2.37   0.018      2.91909    30.81614
-------------------------------------------------------------------------------

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
-------------------------------------------------------------------------------
              |               Robust
      wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
smkintensi~71 |  -.1089889   .0315762    -3.45   0.001    -.1709417   -.0470361
              |
           c. |
smkintensi~71#|
           c. |
smkintensi~71 |   .0026949   .0024203     1.11   0.266    -.0020537    .0074436
              |
        _cons |   2.004525    .295502     6.78   0.000     1.424747    2.584302
-------------------------------------------------------------------------------


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
*use ./data/nhefs, clear

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
(0 observations deleted)


Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -839.70016  
Iteration 2:   log likelihood = -838.45045  
Iteration 3:   log likelihood = -838.44842  
Iteration 4:   log likelihood = -838.44842  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(18)       =     109.16
                                                Prob > chi2       =     0.0000
Log likelihood = -838.44842                     Pseudo R2         =     0.0611

-------------------------------------------------------------------------------
         qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
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
smokeintens~y |  -.0772704   .0152499    -5.07   0.000    -.1071596   -.0473812
              |
           c. |
smokeintens~y#|
           c. |
smokeintens~y |   .0010451   .0002866     3.65   0.000     .0004835    .0016068
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
c.wt71#c.wt71 |   .0001352   .0001632     0.83   0.407    -.0001846     .000455
              |
        _cons |   -1.19407   1.398493    -0.85   0.393    -3.935066    1.546925
-------------------------------------------------------------------------------



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
          0 |        762       48.66       48.66
          1 |        804       51.34      100.00
------------+-----------------------------------
      Total |      1,566      100.00


Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -839.70016  
Iteration 2:   log likelihood = -838.45045  
Iteration 3:   log likelihood = -838.44842  
Iteration 4:   log likelihood = -838.44842  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(18)       =     109.16
                                                Prob > chi2       =     0.0000
Log likelihood = -838.44842                     Pseudo R2         =     0.0611

-------------------------------------------------------------------------------
         qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
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
smokeintens~y |  -.0772704   .0152499    -5.07   0.000    -.1071596   -.0473812
              |
           c. |
smokeintens~y#|
           c. |
smokeintens~y |   .0010451   .0002866     3.65   0.000     .0004835    .0016068
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
c.wt71#c.wt71 |   .0001352   .0001632     0.83   0.407    -.0001846     .000455
              |
        _cons |   -1.19407   1.398493    -0.85   0.393    -3.935066    1.546925
-------------------------------------------------------------------------------



Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -889.21002  
Iteration 2:   log likelihood = -889.20429  
Iteration 3:   log likelihood = -889.20429  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(1)        =       7.65
                                                Prob > chi2       =     0.0057
Log likelihood = -889.20429                     Pseudo R2         =     0.0043

------------------------------------------------------------------------------
        qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         sex |  -.3202065   .1160399    -2.76   0.006    -.5476405   -.0927724
       _cons |  -.9016385   .0799404   -11.28   0.000    -1.058319   -.7449581
------------------------------------------------------------------------------


(1,566 missing values generated)

(403 real changes made)

(1,163 real changes made)

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        sw_a |      1,566     .998931    .2705899   .2929788   3.801141

(sum of wgt is 1,564.32597082853)

Linear regression                               Number of obs     =      1,566
                                                F(3, 1565)        =      15.36
                                                Prob > F          =     0.0000
                                                R-squared         =     0.0361
                                                Root MSE          =     7.8072

                                (Std. Err. adjusted for 1,566 clusters in seqn)
-------------------------------------------------------------------------------
              |               Robust
      wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
         qsmk |
Smoking ce..  |   3.521978   .6579102     5.35   0.000     2.231499    4.812456
              |
     qsmk#sex |
Smoking ce.. #|
           1  |  -.1594785   1.047419    -0.15   0.879    -2.213971    1.895014
              |
        _cons |   1.784447    .310238     5.75   0.000     1.175921    2.392973
-------------------------------------------------------------------------------
```

## Program 12.7
- Estimating IP weights to adjust for selection bias due to censoring
- Data from NHEFS
- Section 12.6


```stata
* use ./data/nhefs-formatted, clear

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
      cens | No smokin  Smoking c |     Total
-----------+----------------------+----------
         0 |     1,163        403 |     1,566 
           |    100.00     100.00 |    100.00 
-----------+----------------------+----------
     Total |     1,163        403 |     1,566 
           |    100.00     100.00 |    100.00 


--------------------------------------------------------------------------------
-> cens = 0

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        wt71 |      1,566    70.83092     15.3149      39.58     151.73



Iteration 0:   log likelihood = -893.02712  
Iteration 1:   log likelihood = -839.70016  
Iteration 2:   log likelihood = -838.45045  
Iteration 3:   log likelihood = -838.44842  
Iteration 4:   log likelihood = -838.44842  

Logistic regression                             Number of obs     =      1,566
                                                LR chi2(18)       =     109.16
                                                Prob > chi2       =     0.0000
Log likelihood = -838.44842                     Pseudo R2         =     0.0611

-------------------------------------------------------------------------------
         qsmk |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
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
smokeintens~y |  -.0772704   .0152499    -5.07   0.000    -.1071596   -.0473812
              |
           c. |
smokeintens~y#|
           c. |
smokeintens~y |   .0010451   .0002866     3.65   0.000     .0004835    .0016068
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
c.wt71#c.wt71 |   .0001352   .0001632     0.83   0.407    -.0001846     .000455
              |
        _cons |   -1.19407   1.398493    -0.85   0.393    -3.935066    1.546925
-------------------------------------------------------------------------------



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



outcome does not vary; remember:
                                  0 = negative outcome,
        all other nonmissing values = positive outcome
r(2000);

end of do-file
r(2000);
```
