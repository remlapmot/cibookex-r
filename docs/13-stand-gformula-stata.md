# 13. Standardization and the parametric G-formula: Stata{-}


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

## Program 13.1

- Estimating the mean outcome within levels of treatment and confounders: Data from NHEFS
- Section 13.2


```stata
use ./data/nhefs-formatted, clear

/* Estimate the the conditional mean outcome within strata of quitting 
smoking and covariates, among the uncensored */
glm wt82_71 qsmk sex race c.age##c.age ib(last).education ///
  c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ///
  ib(last).exercise ib(last).active c.wt71##c.wt71 ///
  qsmk##c.smokeintensity
predict meanY
summarize meanY

/*Look at the predicted value for subject ID = 24770*/
list meanY if seqn == 24770

/*Observed mean outcome for comparison */
summarize wt82_71
```

```
note: 1.qsmk omitted because of collinearity.
note: smokeintensity omitted because of collinearity.

Iteration 0:  Log likelihood = -5328.5765  

Generalized linear models                         Number of obs   =      1,566
Optimization     : ML                             Residual df     =      1,545
                                                  Scale parameter =    53.5683
Deviance         =  82763.02862                   (1/df) Deviance =    53.5683
Pearson          =  82763.02862                   (1/df) Pearson  =    53.5683

Variance function: V(u) = 1                       [Gaussian]
Link function    : g(u) = u                       [Identity]

                                                  AIC             =   6.832154
Log likelihood   = -5328.576456                   BIC             =   71397.58

------------------------------------------------------------------------------------
                   |                 OIM
           wt82_71 | Coefficient  std. err.      z    P>|z|     [95% conf. interval]
-------------------+----------------------------------------------------------------
              qsmk |   2.559594   .8091486     3.16   0.002      .973692    4.145496
               sex |  -1.430272   .4689576    -3.05   0.002    -2.349412   -.5111317
              race |   .5601096   .5818888     0.96   0.336    -.5803714    1.700591
               age |   .3596353   .1633188     2.20   0.028     .0395364    .6797342
                   |
       c.age#c.age |   -.006101   .0017261    -3.53   0.000    -.0094841   -.0027178
                   |
         education |
                1  |    .194977   .7413692     0.26   0.793     -1.25808    1.648034
                2  |   .9854211   .7012116     1.41   0.160    -.3889285    2.359771
                3  |   .7512894   .6339153     1.19   0.236    -.4911617    1.993741
                4  |   1.686547   .8716593     1.93   0.053    -.0218744    3.394967
                   |
    smokeintensity |   .0491365   .0517254     0.95   0.342    -.0522435    .1505165
                   |
  c.smokeintensity#|
  c.smokeintensity |  -.0009907    .000938    -1.06   0.291    -.0028292    .0008479
                   |
          smokeyrs |   .1343686   .0917122     1.47   0.143     -.045384    .3141212
                   |
        c.smokeyrs#|
        c.smokeyrs |  -.0018664   .0015437    -1.21   0.227    -.0048921    .0011592
                   |
          exercise |
                0  |  -.3539128   .5588587    -0.63   0.527    -1.449256    .7414301
                1  |  -.0579374   .4316468    -0.13   0.893    -.9039497    .7880749
                   |
            active |
                0  |   .2613779   .6845577     0.38   0.703     -1.08033    1.603086
                1  |  -.6861916   .6739131    -1.02   0.309    -2.007037    .6346539
                   |
              wt71 |   .0455018   .0833709     0.55   0.585    -.1179022    .2089058
                   |
     c.wt71#c.wt71 |  -.0009653   .0005247    -1.84   0.066    -.0019937    .0000631
                   |
              qsmk |
Smoking cessation  |          0  (omitted)
    smokeintensity |          0  (omitted)
                   |
              qsmk#|
  c.smokeintensity |
Smoking cessation  |   .0466628   .0351448     1.33   0.184    -.0222197    .1155453
                   |
             _cons |  -1.690608   4.388883    -0.39   0.700    -10.29266    6.911444
------------------------------------------------------------------------------------

(option mu assumed; predicted mean wt82_71)

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       meanY |      1,566      2.6383    3.034683  -10.87582   9.876489

      +----------+
      |    meanY |
      |----------|
 960. | .3421569 |
      +----------+

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
     wt82_71 |      1,566      2.6383    7.879913  -41.28047   48.53839
```

## Program 13.2

- Standardizing the mean outcome to the baseline confounders
- Data from Table 2.2
- Section 13.3


```stata
clear
input str10 ID L A Y
"Rheia" 	0 0 0 
"Kronos" 	0 0 1 
"Demeter" 	0 0 0 
"Hades" 	0 0 0 
"Hestia" 	0 1 0 
"Poseidon" 	0 1 0 
"Hera"  	0 1 0 
"Zeus" 		0 1 1 
"Artemis" 	1 0 1
"Apollo"	1 0 1
"Leto"		1 0 0
"Ares"		1 1 1
"Athena"	1 1 1
"Hephaestus" 1 1 1
"Aphrodite" 1 1 1
"Cyclope"	1 1 1
"Persephone" 1 1 1
"Hermes"	1 1 0
"Hebe"		1 1 0
"Dionysus"	1 1	0 
end

/* i. Data set up for standardization: 
 - create 3 copies of each subject first, 
 - duplicate the dataset and create a variable `interv` which indicates
which copy is the duplicate (interv =1) */
expand 2, generate(interv)

/* Next, duplicate the original copy (interv = 0) again, and create
another variable 'interv2' to indicate the copy */
expand 2 if interv == 0, generate(interv2)

/* Now, change the value of 'interv' to -1 in one of the copies so that
there are unique values of interv for each copy */
replace interv = -1  if interv2 ==1
drop interv2

/* Check that the data has the structure you want: 
 - there should be 1566 people in each of the 3 levels of interv*/
tab interv

/* Two of the copies will be for computing the standardized result
for these two copies (interv = 0 and interv = 1), set the outcome to
missing and force qsmk to either 0 or 1, respectively.
You may need to edit this part of the code for your outcome and exposure variables */
replace Y = . if interv != -1
replace A = 0 if interv == 0
replace A = 1 if interv == 1

/* Check that the data has the structure you want: 
for interv = -1, some people quit and some do not; 
for interv = 0 or 1, noone quits or everyone quits, respectively */
by interv, sort: summarize A

*ii.Estimation in original sample*
*Now, we do a parametric regression with the covariates we want to adjust for*
*You may need to edit this part of the code for the variables you want.*
*Because the copies have missing Y, this will only run the regression in the
*original copy.*
*The double hash between A & L creates a regression model with A and L and a 
* product term between A and L*
regress Y A##L

*Ask Stata for expected values - Stata will give you expected values for all 
* copies, not just the original ones*
predict predY, xb

*Now ask for a summary of these values by intervention*
*These are the standardized outcome estimates: you can subtract them to get the
* standardized difference*
by interv, sort: summarize predY

*iii.OPTIONAL: Output standardized point estimates and difference*
*The summary from the last command gives you the standardized estimates*
*We can stop there, or we can ask Stata to calculate the standardized difference
* and display all the results in a simple table*
*The code below can be used as-is without changing any variable names*
*The option "quietly" asks Stata not to display the output of some intermediate
* calculations*
*You can delete this option if you want to see what is happening step-by-step*
quietly summarize predY if(interv == -1)
matrix input observe = (-1,`r(mean)')
quietly summarize predY if(interv == 0)
matrix observe = (observe \0,`r(mean)')
quietly summarize predY if(interv == 1)
matrix observe = (observe \1,`r(mean)')
matrix observe = (observe \., observe[3,2]-observe[2,2]) 

*Add some row/column descriptions and print results to screen*
matrix rownames observe = observed E(Y(a=0)) E(Y(a=1)) difference
matrix colnames observe = interv value
matrix list observe 

*to interpret these results:*
*row 1, column 2, is the observed mean outcome value in our original sample*
*row 2, column 2, is the mean outcome value if everyone had not quit smoking*
*row 3, column 2, is the mean outcome value if everyone had quit smoking*
*row 4, column 2, is the mean difference outcome value if everyone had quit 
* smoking compared to if everyone had not quit smoking*
```

```
             ID          L          A          Y
  1. "Rheia"         0 0 0 
  2. "Kronos"        0 0 1 
  3. "Demeter"       0 0 0 
  4. "Hades"         0 0 0 
  5. "Hestia"        0 1 0 
  6. "Poseidon"      0 1 0 
  7. "Hera"          0 1 0 
  8. "Zeus"          0 1 1 
  9. "Artemis"       1 0 1
 10. "Apollo"        1 0 1
 11. "Leto"          1 0 0
 12. "Ares"          1 1 1
 13. "Athena"        1 1 1
 14. "Hephaestus" 1 1 1
 15. "Aphrodite" 1 1 1
 16. "Cyclope"       1 1 1
 17. "Persephone" 1 1 1
 18. "Hermes"        1 1 0
 19. "Hebe"          1 1 0
 20. "Dionysus"      1 1     0 
 21. end

(20 observations created)

(20 observations created)

(20 real changes made)



  Expanded observation |
                  type |      Freq.     Percent        Cum.
-----------------------+-----------------------------------
                    -1 |         20       33.33       33.33
  Original observation |         20       33.33       66.67
Duplicated observation |         20       33.33      100.00
-----------------------+-----------------------------------
                 Total |         60      100.00

(40 real changes made, 40 to missing)

(13 real changes made)

(7 real changes made)


--------------------------------------------------------------------------------------
-> interv = -1

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
           A |         20         .65    .4893605          0          1

--------------------------------------------------------------------------------------
-> interv = Original

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
           A |         20           0           0          0          0

--------------------------------------------------------------------------------------
-> interv = Duplicat

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
           A |         20           1           0          1          1

      Source |       SS           df       MS      Number of obs   =        20
-------------+----------------------------------   F(3, 16)        =      1.07
       Model |  .833333333         3  .277777778   Prob > F        =    0.3909
    Residual |  4.16666667        16  .260416667   R-squared       =    0.1667
-------------+----------------------------------   Adj R-squared   =    0.0104
       Total |           5        19  .263157895   Root MSE        =    .51031

------------------------------------------------------------------------------
           Y | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
-------------+----------------------------------------------------------------
         1.A |   1.05e-16   .3608439     0.00   1.000    -.7649549    .7649549
         1.L |   .4166667    .389756     1.07   0.301    -.4095791    1.242912
             |
         A#L |
        1 1  |  -5.83e-17   .4959325    -0.00   1.000     -1.05133     1.05133
             |
       _cons |        .25   .2551552     0.98   0.342    -.2909048    .7909048
------------------------------------------------------------------------------



--------------------------------------------------------------------------------------
-> interv = -1

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       predY |         20          .5     .209427        .25   .6666667

--------------------------------------------------------------------------------------
-> interv = Original

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       predY |         20          .5     .209427        .25   .6666667

--------------------------------------------------------------------------------------
-> interv = Duplicat

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       predY |         20          .5     .209427        .25   .6666667












observe[4,2]
               interv      value
  observed         -1  .50000001
 E(Y(a=0))          0  .50000001
 E(Y(a=1))          1  .50000001
difference          .          0
```

## Program 13.3

- Standardizing the mean outcome to the baseline confounders:
- Data from NHEFS
- Section 13.3


```stata
use ./data/nhefs-formatted, clear

*i.Data set up for standardization: create 3 copies of each subject*
*first, duplicate the dataset and create a variable 'interv' which indicates
* which copy is the duplicate (interv =1)
expand 2, generate(interv)

*next, duplicate the original copy (interv = 0) again, and create another
* variable 'interv2' to indicate the copy
expand 2 if interv == 0, generate(interv2)

*now, change the value of 'interv' to -1 in one of the copies so that there are
* unique values of interv for each copy*
replace interv = -1  if interv2 ==1
drop interv2 

*check that the data has the structure you want: there should be 1566 people in
* each of the 3 levels of interv*
tab interv

*two of the copies will be for computing the standardized result*
*for these two copies (interv = 0 and interv = 1), set the outcome to missing
* and force qsmk to either 0 or 1, respectively*
*you may need to edit this part of the code for your outcome and exposure variables*
replace wt82_71 = . if interv != -1
replace qsmk = 0 if interv == 0
replace qsmk = 1 if interv == 1

*check that the data has the structure you want: for interv = -1, some people
* quit and some do not; for interv = 0 or 1, noone quits or everyone quits, respectively*
by interv, sort: summarize qsmk

*ii.Estimation in original sample*
*Now, we do a parametric regression with the covariates we want to adjust for*
*You may need to edit this part of the code for the variables you want.*
*Because the copies have missing wt82_71, this will only run the regression in 
* the original copy*
regress wt82_71 qsmk sex race c.age##c.age ib(last).education ///
c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ///
ib(last).exercise ib(last).active c.wt71##c.wt71 qsmk#c.smokeintensity

*Ask Stata for expected values - Stata will give you expected values for all 
* copies, not just the original ones*
predict predY, xb

*Now ask for a summary of these values by intervention*
*These are the standardized outcome estimates: you can subtract them to get the
* standardized difference*
by interv, sort: summarize predY

/* iii.OPTIONAL: Output standardized point estimates and difference
- The summary from the last command gives you the 
standardized estimates
- We can stop there, or we can ask Stata to calculate the 
standardized difference and display all the results 
in a simple table
- The code below can be used as-is without changing any
variable names
- The option `quietly` asks Stata not to display the output of 
some intermediate calculations
- You can delete this option if you want to see what is 
happening step-by-step */
quietly summarize predY if(interv == -1)
matrix input observe = (-1,`r(mean)')
quietly summarize predY if(interv == 0)
matrix observe = (observe \0,`r(mean)')
quietly summarize predY if(interv == 1)
matrix observe = (observe \1,`r(mean)')
matrix observe = (observe \., observe[3,2]-observe[2,2]) 

* Add some row/column descriptions and print results to screen
matrix rownames observe = observed E(Y(a=0)) E(Y(a=1)) difference
matrix colnames observe = interv value
matrix list observe 

/* To interpret these results:
- row 1, column 2, is the observed mean outcome value 
in our original sample
- row 2, column 2, is the mean outcome value 
if everyone had not quit smoking
- row 3, column 2, is the mean outcome value 
if everyone had quit smoking
- row 4, column 2, is the mean difference outcome value 
if everyone had quit smoking compared to if everyone 
had not quit smoking */

/* Addition due to way Statamarkdown works 
i.e. each code chunk is a separate Stata session */
mata observe = st_matrix("observe")
mata mata matsave ./data/observe observe, replace

*drop the copies*
drop if interv != -1
gen meanY_b =.
qui save ./data/nhefs_std, replace
```

```
(1,566 observations created)

(1,566 observations created)

(1,566 real changes made)



  Expanded observation |
                  type |      Freq.     Percent        Cum.
-----------------------+-----------------------------------
                    -1 |      1,566       33.33       33.33
  Original observation |      1,566       33.33       66.67
Duplicated observation |      1,566       33.33      100.00
-----------------------+-----------------------------------
                 Total |      4,698      100.00

(3,132 real changes made, 3,132 to missing)

(403 real changes made)

(1,163 real changes made)


--------------------------------------------------------------------------------------
-> interv = -1

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
        qsmk |      1,566    .2573436    .4373099          0          1

--------------------------------------------------------------------------------------
-> interv = Original

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
        qsmk |      1,566           0           0          0          0

--------------------------------------------------------------------------------------
-> interv = Duplicat

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
        qsmk |      1,566           1           0          1          1

      Source |       SS           df       MS      Number of obs   =     1,566
-------------+----------------------------------   F(20, 1545)     =     13.45
       Model |   14412.558        20    720.6279   Prob > F        =    0.0000
    Residual |  82763.0286     1,545  53.5683033   R-squared       =    0.1483
-------------+----------------------------------   Adj R-squared   =    0.1373
       Total |  97175.5866     1,565  62.0930266   Root MSE        =     7.319

------------------------------------------------------------------------------------
           wt82_71 | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
-------------------+----------------------------------------------------------------
              qsmk |   2.559594   .8091486     3.16   0.002     .9724486     4.14674
               sex |  -1.430272   .4689576    -3.05   0.002    -2.350132   -.5104111
              race |   .5601096   .5818888     0.96   0.336    -.5812656    1.701485
               age |   .3596353   .1633188     2.20   0.028     .0392854    .6799851
                   |
       c.age#c.age |   -.006101   .0017261    -3.53   0.000    -.0094868   -.0027151
                   |
         education |
                1  |    .194977   .7413692     0.26   0.793    -1.259219    1.649173
                2  |   .9854211   .7012116     1.41   0.160     -.390006    2.360848
                3  |   .7512894   .6339153     1.19   0.236    -.4921358    1.994715
                4  |   1.686547   .8716593     1.93   0.053    -.0232138    3.396307
                   |
    smokeintensity |   .0491365   .0517254     0.95   0.342     -.052323    .1505959
                   |
  c.smokeintensity#|
  c.smokeintensity |  -.0009907    .000938    -1.06   0.291    -.0028306    .0008493
                   |
          smokeyrs |   .1343686   .0917122     1.47   0.143     -.045525    .3142621
                   |
        c.smokeyrs#|
        c.smokeyrs |  -.0018664   .0015437    -1.21   0.227    -.0048944    .0011616
                   |
          exercise |
                0  |  -.3539128   .5588587    -0.63   0.527    -1.450114    .7422889
                1  |  -.0579374   .4316468    -0.13   0.893     -.904613    .7887381
                   |
            active |
                0  |   .2613779   .6845577     0.38   0.703    -1.081382    1.604138
                1  |  -.6861916   .6739131    -1.02   0.309    -2.008073    .6356894
                   |
              wt71 |   .0455018   .0833709     0.55   0.585    -.1180303    .2090339
                   |
     c.wt71#c.wt71 |  -.0009653   .0005247    -1.84   0.066    -.0019945    .0000639
                   |
              qsmk#|
  c.smokeintensity |
Smoking cessation  |   .0466628   .0351448     1.33   0.184    -.0222737    .1155993
                   |
             _cons |  -1.690608   4.388883    -0.39   0.700     -10.2994    6.918188
------------------------------------------------------------------------------------



--------------------------------------------------------------------------------------
-> interv = -1

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       predY |      1,566      2.6383    3.034683  -10.87582   9.876489

--------------------------------------------------------------------------------------
-> interv = Original

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       predY |      1,566    1.756213    2.826271  -11.83737   6.733498

--------------------------------------------------------------------------------------
-> interv = Duplicat

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       predY |      1,566    5.273587    2.920532  -9.091126    11.0506












observe[4,2]
               interv      value
  observed         -1  2.6382998
 E(Y(a=0))          0  1.7562131
 E(Y(a=1))          1  5.2735873
difference          .  3.5173742


(saving observe[4,2])
file ./data/observe.mmat saved

(3,132 observations deleted)

(1,566 missing values generated)
```

## Program 13.4

- Computing the 95% confidence interval of the standardized means and their difference: Data from NHEFS
- Section 13.3


```stata
*Run program 13.3 to obtain point estimates, and then the code below*

capture program drop bootstdz

program define bootstdz, rclass
use ./data/nhefs_std, clear

preserve

* Draw bootstrap sample from original observations
bsample 
		
/* Create copies with each value of qsmk in bootstrap sample.
First, duplicate the dataset and create a variable `interv` which
indicates which copy is the duplicate (interv =1)*/
expand 2, generate(interv_b)

/* Next, duplicate the original copy (interv = 0) again, and create
another variable `interv2` to indicate the copy*/
expand 2 if interv_b == 0, generate(interv2_b)

/* Now, change the value of interv to -1 in one of the copies so that
there are unique values of interv for each copy*/
replace interv_b = -1  if interv2_b ==1
drop interv2_b

/* Two of the copies will be for computing the standardized result.
For these two copies (interv = 0 and interv = 1), set the outcome to
missing and force qsmk to either 0 or 1, respectively*/
replace wt82_71 = . if interv_b != -1
replace qsmk = 0 if interv_b == 0
replace qsmk = 1 if interv_b == 1

* Run regression
regress wt82_71 qsmk sex race c.age##c.age ib(last).education ///
  c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ///
  ib(last).exercise ib(last).active c.wt71##c.wt71 ///
  qsmk#c.smokeintensity

/* Ask Stata for expected values.
Stata will give you expected values for all copies, not just the
original ones*/
predict predY_b, xb
summarize predY_b if interv_b == 0
return scalar boot_0 = r(mean)
summarize predY_b if interv_b == 1
return scalar boot_1 = r(mean)
return scalar boot_diff = return(boot_1) - return(boot_0)
drop meanY_b

restore

end

/* Then we use the `simulate` command to run the bootstraps as many
times as we want.
Start with reps(10) to make sure your code runs, and then change to
reps(1000) to generate your final CIs.*/
simulate EY_a0=r(boot_0) EY_a1 = r(boot_1) ///
  difference = r(boot_diff), reps(10) seed(1): bootstdz

/* Next, format the point estimate to allow Stata to calculate our
standard errors and confidence intervals*/
  
* Addition: read back in the observe matrix  
mata mata matuse ./data/observe, replace
mata st_matrix("observe", observe)

matrix pe = observe[2..4, 2]'
matrix list pe

/* Finally, the bstat command generates valid 95% confidence intervals
under the normal approximation using our bootstrap results.
The default results use a normal approximation to calcutlate the
confidence intervals.
Note, n contains the original sample size of your data before censoring*/
bstat, stat(pe) n(1629) 
```

```
 12. 

      Command: bootstdz
        EY_a0: r(boot_0)
        EY_a1: r(boot_1)
   difference: r(boot_diff)

Simulations (10): .........10 done

(loading observe[4,2])




pe[1,3]
           r2         r3         r4
c2  1.7562131  5.2735873  3.5173742


Bootstrap results                                        Number of obs = 1,629
                                                         Replications  =    10

------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             | coefficient  std. err.      z    P>|z|     [95% conf. interval]
-------------+----------------------------------------------------------------
       EY_a0 |   1.756213   .2157234     8.14   0.000     1.333403    2.179023
       EY_a1 |   5.273587   .4999001    10.55   0.000     4.293801    6.253374
  difference |   3.517374    .538932     6.53   0.000     2.461087    4.573662
------------------------------------------------------------------------------
```
