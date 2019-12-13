# 15. Outcome regression and propensity scores: Stata{-}


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

## Program 15.1
- Estimating the average causal effect within levels of confounders under the assumption of effect-measure modification by smoking intensity ONLY 
- Data from NHEFS
- Section 15.1


```stata
use ./data/nhefs-formatted, clear
```

```
end of do-file
```


```stata
/*generate smoking intensity among smokers product term*/
gen qsmkintensity = qsmk*smokeintensity

* regression on covariates, allowing for some effect modfication
regress wt82_71 qsmk qsmkintensity c.smokeintensity##c.smokeintensity sex race c.age##c.age ///
ib(last).education c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 

*Output the estimated mean difference between quitting and not quitting value when smoke intensity = 5 cigarettes/ day*
lincom 1*_b[qsmk] + 5*1*_b[qsmkintensity] 

*Output the estimated mean difference between quitting and not quitting value when smoke intensity = 40 cigarettes/ day*
lincom 1*_b[qsmk] + 40*1*_b[qsmkintensity]

/* regression on covariates, with no product terms*/
regress wt82_71 qsmk c.smokeintensity##c.smokeintensity sex race c.age##c.age ///
ib(last).education c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 
```

```
      Source |       SS           df       MS      Number of obs   =     1,566
-------------+----------------------------------   F(20, 1545)     =     13.45
       Model |   14412.558        20    720.6279   Prob > F        =    0.0000
    Residual |  82763.0286     1,545  53.5683033   R-squared       =    0.1483
-------------+----------------------------------   Adj R-squared   =    0.1373
       Total |  97175.5866     1,565  62.0930266   Root MSE        =     7.319

-------------------------------------------------------------------------------
      wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
         qsmk |   2.559594   .8091486     3.16   0.002     .9724486     4.14674
qsmkintensity |   .0466628   .0351448     1.33   0.184    -.0222737    .1155993
smokeintens~y |   .0491365   .0517254     0.95   0.342     -.052323    .1505959
              |
           c. |
smokeintens~y#|
           c. |
smokeintens~y |  -.0009907    .000938    -1.06   0.291    -.0028306    .0008493
              |
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
        _cons |  -1.690608   4.388883    -0.39   0.700     -10.2994    6.918188
-------------------------------------------------------------------------------


 ( 1)  qsmk + 5*qsmkintensity = 0

------------------------------------------------------------------------------
     wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         (1) |   2.792908   .6682596     4.18   0.000     1.482117      4.1037
------------------------------------------------------------------------------


 ( 1)  qsmk + 40*qsmkintensity = 0

------------------------------------------------------------------------------
     wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         (1) |   4.426108   .8477818     5.22   0.000     2.763183    6.089032
------------------------------------------------------------------------------

      Source |       SS           df       MS      Number of obs   =     1,566
-------------+----------------------------------   F(19, 1546)     =     14.06
       Model |  14318.1239        19   753.58547   Prob > F        =    0.0000
    Residual |  82857.4627     1,546  53.5947365   R-squared       =    0.1473
-------------+----------------------------------   Adj R-squared   =    0.1369
       Total |  97175.5866     1,565  62.0930266   Root MSE        =    7.3208

-------------------------------------------------------------------------------
      wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
         qsmk |   3.462622   .4384543     7.90   0.000     2.602594     4.32265
smokeintens~y |   .0651533   .0503115     1.29   0.196    -.0335327    .1638392
              |
           c. |
smokeintens~y#|
           c. |
smokeintens~y |  -.0010468   .0009373    -1.12   0.264    -.0028853    .0007918
              |
          sex |   -1.46505    .468341    -3.13   0.002      -2.3837   -.5463989
         race |   .5864117   .5816949     1.01   0.314    -.5545827    1.727406
          age |   .3626624   .1633431     2.22   0.027     .0422649    .6830599
              |
  c.age#c.age |  -.0061377   .0017263    -3.56   0.000    -.0095239   -.0027515
              |
    education |
           1  |   .1708264   .7413289     0.23   0.818     -1.28329    1.624943
           2  |   .9893527   .7013784     1.41   0.159    -.3864007    2.365106
           3  |   .7423268   .6340357     1.17   0.242     -.501334    1.985988
           4  |   1.679344   .8718575     1.93   0.054    -.0308044    3.389492
              |
     smokeyrs |   .1333931   .0917319     1.45   0.146    -.0465389    .3133252
              |
   c.smokeyrs#|
   c.smokeyrs |   -.001827   .0015438    -1.18   0.237    -.0048552    .0012012
              |
     exercise |
           0  |  -.3628786   .5589557    -0.65   0.516     -1.45927    .7335129
           1  |  -.0421962   .4315904    -0.10   0.922    -.8887606    .8043683
              |
       active |
           0  |   .2580374   .6847219     0.38   0.706    -1.085044    1.601119
           1  |    -.68492   .6740787    -1.02   0.310    -2.007125    .6372851
              |
         wt71 |   .0373642   .0831658     0.45   0.653    -.1257655     .200494
              |
c.wt71#c.wt71 |  -.0009158   .0005235    -1.75   0.080    -.0019427    .0001111
              |
        _cons |  -1.724603   4.389891    -0.39   0.694    -10.33537    6.886166
-------------------------------------------------------------------------------
```

## Prorgam 15.2
- Estimating and plotting the propensity score
- Data from NHEFS
- Section 15.2


```stata
/*Fit a model for the exposure, quitting smoking*/
logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 

/*Estimate the propensity score, P(Qsmk|Covariates)*/
predict ps, pr

/*Check the distribution of the propensity score*/
bys qsmk: summarize ps 

/*Return extreme values of propensity score: note, for Stata versions 15 and above, start by installing extremes*/
/*
ssc install extremes 
*/
extremes ps seqn
bys qsmk: extremes ps seqn

/*Plotting the estimated propensity score*/
histogram ps, width(0.05) start(0.025) frequency fcolor(none) lcolor(black) lpattern(solid) addlabel addlabopts(mlabcolor(black) mlabposition(12) mlabangle(zero)) ///
ytitle(No. Subjects) ylabel(#4) xtitle(Estimated Propensity Score) xlabel(#15)  ///
by(, title(Estimated Propensity Score Distribution) subtitle(By Quit Smoking Status)) ///
by(, legend(off)) by(qsmk, style(compact)  colfirst) subtitle(, size(small) box bexpand)
qui gr export ./figs/stata-fig-15-2.png, replace
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



--------------------------------------------------------------------------------
-> qsmk = No smoking cessation

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
          ps |      1,163    .2392928    .1056545   .0510008   .6814955

--------------------------------------------------------------------------------
-> qsmk = Smoking cessation

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
          ps |        403    .3094353    .1290642   .0598799   .7768887



  +--------------------------+
  |  obs:         ps    seqn |
  |--------------------------|
  |   65.   .0510008   22941 |
  |  364.   .0527126    1769 |
  |  427.   .0558418   21140 |
  |  578.   .0558752    2522 |
  | 1139.   .0567372   12639 |
  +--------------------------+

  +--------------------------+
  | 1365.   .6659576   22272 |
  |   13.   .6814955   22773 |
  | 1346.   .7166381   14983 |
  | 1289.   .7200644   24817 |
  | 1495.   .7768887   24949 |
  +--------------------------+


--------------------------------------------------------------------------------
-> qsmk = No smoking cessation

  +--------------------------+
  |  obs:         ps    seqn |
  |--------------------------|
  |   65.   .0510008   22941 |
  |  364.   .0527126    1769 |
  |  427.   .0558418   21140 |
  |  578.   .0558752    2522 |
  | 1139.   .0567372   12639 |
  +--------------------------+

  +--------------------------+
  | 1161.   .6337243   17096 |
  |  220.   .6345721   17768 |
  | 1084.   .6440308   19147 |
  |  267.   .6566707   21983 |
  |   13.   .6814955   22773 |
  +--------------------------+

--------------------------------------------------------------------------------
-> qsmk = Smoking cessation

  +--------------------------+
  |  obs:         ps    seqn |
  |--------------------------|
  | 1390.   .0598799    4289 |
  | 1545.   .0600822   23550 |
  | 1367.   .0806089   24306 |
  | 1263.   .0821677   22904 |
  | 1511.   .1021875   24584 |
  +--------------------------+

  +--------------------------+
  | 1215.    .635695   17738 |
  | 1365.   .6659576   22272 |
  | 1346.   .7166381   14983 |
  | 1289.   .7200644   24817 |
  | 1495.   .7768887   24949 |
  +--------------------------+

```

![](stata-fig-15-2.png)<!-- -->

## Program 15.3
- Stratification and outcome regression using deciles of the propensity score
- Data from NHEFS
- Section 15.3
- NOTE: Stata decides borderline cutpoints differently from SAS, so, despite identically distributed propensity scores, the results of regression using deciles are not an exact match with the book.


```stata
/*Calculation of deciles of ps*/
xtile ps_dec = ps, nq(10)
by ps_dec, sort: summarize ps

/*Stratification on PS deciles, allowing for effect modification*/
/*Note: stata compares qsmk 0 vs qsmk 1, so the coefficients are reversed relative to the book*/
by ps_dec: ttest wt82_71, by(qsmk)

/*Regression on PS deciles, with no product terms*/
regress wt82_71 qsmk ib(last).ps_dec
```

```
ps not found
r(111);

end of do-file
r(111);
```

## Program 15.4
- Standardization and outcome regression using the propensity score
- Data from NHEFS
- Section 15.3


```stata
/*Estimate the propensity score*/
logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 
predict ps, pr

/*Expand the dataset for standardization*/
expand 2, generate(interv)
expand 2 if interv == 0, generate(interv2)
replace interv = -1  if interv2 ==1
drop interv2 
tab interv
replace wt82_71 = . if interv != -1
replace qsmk = 0 if interv == 0
replace qsmk = 1 if interv == 1
by interv, sort: summarize qsmk

/*Regression on the propensity score, allowing for effect modification*/
regress wt82_71 qsmk##c.ps
predict predY, xb
by interv, sort: summarize predY

quietly summarize predY if(interv == -1)
matrix input observe = (-1,`r(mean)')
quietly summarize predY if(interv == 0)
matrix observe = (observe \0,`r(mean)')
quietly summarize predY if(interv == 1)
matrix observe = (observe \1,`r(mean)')
matrix observe = (observe \., observe[3,2]-observe[2,2]) 
matrix rownames observe = observed E(Y(a=0)) E(Y(a=1)) difference
matrix colnames observe = interv value
matrix list observe 

/*bootstrap program*/
drop if interv != -1
gen meanY_b =.
qui save ./data/nhefs_std, replace
capture program drop bootstdz
program define bootstdz, rclass
	use ./data/nhefs_std, clear
		preserve
		bsample 
		/*Create 2 new copies of the data. Set the outcome AND the exposure to missing in the copies*/
		expand 2, generate(interv_b)
		expand 2 if interv_b == 0, generate(interv2_b)
		replace interv_b = -1  if interv2_b ==1
		drop interv2_b
		replace wt82_71 = . if interv_b != -1
		replace qsmk = . if interv_b != -1
		/*Fit the propensity score in the original data (where qsmk is not missing) and generate predictions for everyone*/
		logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity ///
		c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 
		predict ps_b, pr
		/*Set the exposure to 0 for everyone in copy 0, and 1 to everyone for copy 1*/
		replace qsmk = 0 if interv_b == 0
		replace qsmk = 1 if interv_b == 1
		/*Fit the outcome regression in the original data (where wt82_71 is not missing) and generate predictions for everyone*/
		regress wt82_71 qsmk##c.ps
		predict predY_b, xb
		/*Summarize the predictions in each set of copies*/
		summarize predY_b if interv_b == 0
		return scalar boot_0 = r(mean)
		summarize predY_b if interv_b == 1
		return scalar boot_1 = r(mean)
		return scalar boot_diff = return(boot_1) - return(boot_0)
	drop meanY_b
	restore
end

*Then we use the 'simulate' command to run the bootstraps as many times as we want*
*Start with reps(10) to make sure your code runs, and then change to reps(1000) to generate your final CIs*
simulate EY_a0=r(boot_0) EY_a1 = r(boot_1) difference = r(boot_diff), reps(500) seed(1): bootstdz /

matrix pe = observe[2..4, 2]'
matrix list pe
bstat, stat(pe) n(1629) 
estat bootstrap, p
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


(1,566 observations created)

(1,566 observations created)

(1,566 real changes made)

     interv |      Freq.     Percent        Cum.
------------+-----------------------------------
         -1 |      1,566       33.33       33.33
          0 |      1,566       33.33       66.67
          1 |      1,566       33.33      100.00
------------+-----------------------------------
      Total |      4,698      100.00

(3,132 real changes made, 3,132 to missing)

(403 real changes made)

(1,163 real changes made)


--------------------------------------------------------------------------------
-> interv = -1

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        qsmk |      1,566    .2573436    .4373099          0          1

--------------------------------------------------------------------------------
-> interv = 0

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        qsmk |      1,566           0           0          0          0

--------------------------------------------------------------------------------
-> interv = 1

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
        qsmk |      1,566           1           0          1          1

      Source |       SS           df       MS      Number of obs   =     1,566
-------------+----------------------------------   F(3, 1562)      =     29.96
       Model |  5287.31428         3  1762.43809   Prob > F        =    0.0000
    Residual |  91888.2723     1,562   58.827319   R-squared       =    0.0544
-------------+----------------------------------   Adj R-squared   =    0.0526
       Total |  97175.5866     1,565  62.0930266   Root MSE        =    7.6699

-------------------------------------------------------------------------------
      wt82_71 |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
--------------+----------------------------------------------------------------
         qsmk |
Smoking ce..  |   4.036457    1.13904     3.54   0.000      1.80225    6.270665
           ps |   -12.3319   2.129602    -5.79   0.000    -16.50908   -8.154716
              |
    qsmk#c.ps |
Smoking ce..  |  -2.038829   3.649684    -0.56   0.576    -9.197625    5.119967
              |
        _cons |   4.935432   .5570216     8.86   0.000     3.842843    6.028021
-------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-> interv = -1

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
       predY |      1,566      2.6383    1.838063    -3.4687   8.111371

--------------------------------------------------------------------------------
-> interv = 0

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
       predY |      1,566    1.761898    1.433264  -4.645079   4.306496

--------------------------------------------------------------------------------
-> interv = 1

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
       predY |      1,566    5.273676    1.670225  -2.192565   8.238971












observe[4,2]
               interv      value
  observed         -1  2.6382998
 E(Y(a=0))          0  1.7618979
 E(Y(a=1))          1  5.2736757
difference          .  3.5117778

(3,132 observations deleted)

(1,566 missing values generated)

      command:  bootstdz /
        EY_a0:  r(boot_0)
        EY_a1:  r(boot_1)
   difference:  r(boot_diff)

Simulations (500)
----+--- 1 ---+--- 2 ---+--- 3 ---+--- 4 ---+--- 5 
..................................................    50
..................................................   100
..................................................   150
..................................................   200
..................................................   250
..................................................   300
..................................................   350
..................................................   400
..................................................   450
..................................................   500



pe[1,3]
        E(Y(a=0))   E(Y(a=1))  difference
value   1.7618979   5.2736757   3.5117778


Bootstrap results                               Number of obs     =      1,629
                                                Replications      =        500

------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       EY_a0 |   1.761898   .2273005     7.75   0.000     1.316397    2.207399
       EY_a1 |   5.273676   .4636993    11.37   0.000     4.364842     6.18251
  difference |   3.511778   .5247816     6.69   0.000     2.483225    4.540331
------------------------------------------------------------------------------


Bootstrap results                               Number of obs     =      1,629
                                                Replications      =        500

------------------------------------------------------------------------------
             |    Observed               Bootstrap
             |       Coef.       Bias    Std. Err.  [95% Conf. Interval]
-------------+----------------------------------------------------------------
       EY_a0 |   1.7618979  -.0079171   .22730051    1.307729   2.191437   (P)
       EY_a1 |   5.2736757  -.0001726   .46369929    4.340613   6.210879   (P)
  difference |   3.5117778   .0077445    .5247816    2.531495   4.523983   (P)
------------------------------------------------------------------------------
(P)    percentile confidence interval
```
