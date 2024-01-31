# 14. G-estimation of Structural Nested Models: Stata{-}


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

## Program 14.1

- Ranks of extreme observations
- Data from NHEFS
- Section 14.4


```stata
/*For Stata 15 or later, first install the extremes function using this code:*/
* ssc install extremes 

*Data preprocessing***

use ./data/nhefs, clear
gen byte cens = (wt82 == .)

/*Ranking of extreme observations*/
extremes wt82_71 seqn

/*Estimate unstabilized censoring weights for use in g-estimation models*/
glm cens qsmk sex race c.age##c.age ib(last).education ///
  c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ///
  ib(last).exercise ib(last).active c.wt71##c.wt71 ///
  , family(binomial)
predict pr_cens
gen w_cens = 1/(1-pr_cens)
replace w_cens = . if cens == 1 
/*observations with cens = 1 contribute to censoring models but not outcome model*/
summarize w_cens

/*Analyses restricted to N=1566*/
drop if wt82 == .
summarize wt82_71

save ./data/nhefs-wcens, replace
```

```
  |  obs:        wt82_71    seqn |
  |------------------------------|
  | 1329.   -41.28046982   23321 |
  |  527.   -30.50192161   13593 |
  | 1515.   -30.05007421   24363 |
  |  204.   -29.02579305    5412 |
  | 1067.   -25.97055814   21897 |
  +------------------------------+

  +-----------------------------+
  |  205.   34.01779932    5415 |
  | 1145.   36.96925111   22342 |
  |   64.   37.65051215    1769 |
  |  260.   47.51130337    6928 |
  | 1367.   48.53838568   23522 |
  +-----------------------------+


Iteration 0:  Log likelihood = -292.45812  
Iteration 1:  Log likelihood =  -233.5099  
Iteration 2:  Log likelihood = -232.68635  
Iteration 3:  Log likelihood =    -232.68  
Iteration 4:  Log likelihood = -232.67999  

Generalized linear models                         Number of obs   =      1,629
Optimization     : ML                             Residual df     =      1,609
                                                  Scale parameter =          1
Deviance         =  465.3599898                   (1/df) Deviance =   .2892231
Pearson          =  1654.648193                   (1/df) Pearson  =   1.028371

Variance function: V(u) = u*(1-u)                 [Bernoulli]
Link function    : g(u) = ln(u/(1-u))             [Logit]

                                                  AIC             =   .3102271
Log likelihood   = -232.6799949                   BIC             =  -11434.36

-----------------------------------------------------------------------------------
                  |                 OIM
             cens | Coefficient  std. err.      z    P>|z|     [95% conf. interval]
------------------+----------------------------------------------------------------
             qsmk |   .5168674   .2877162     1.80   0.072    -.0470459    1.080781
              sex |   .0573131   .3302775     0.17   0.862     -.590019    .7046452
             race |  -.0122715   .4524888    -0.03   0.978    -.8991332    .8745902
              age |  -.2697293   .1174647    -2.30   0.022    -.4999558   -.0395027
                  |
      c.age#c.age |   .0028837   .0011135     2.59   0.010     .0007012    .0050661
                  |
        education |
               1  |   .3823818   .5601808     0.68   0.495    -.7155523    1.480316
               2  |  -.0584066   .5749586    -0.10   0.919    -1.185305    1.068491
               3  |   .2176937   .5225008     0.42   0.677    -.8063891    1.241776
               4  |   .5208288   .6678735     0.78   0.435    -.7881792    1.829837
                  |
   smokeintensity |   .0157119   .0347319     0.45   0.651    -.0523614    .0837851
                  |
 c.smokeintensity#|
 c.smokeintensity |  -.0001133   .0006058    -0.19   0.852    -.0013007    .0010742
                  |
         smokeyrs |   .0785973   .0749576     1.05   0.294     -.068317    .2255116
                  |
       c.smokeyrs#|
       c.smokeyrs |  -.0005569   .0010318    -0.54   0.589    -.0025791    .0014653
                  |
         exercise |
               0  |    .583989   .3723133     1.57   0.117    -.1457317     1.31371
               1  |  -.3874824   .3439133    -1.13   0.260     -1.06154    .2865753
                  |
           active |
               0  |  -.7065829   .3964577    -1.78   0.075    -1.483626    .0704599
               1  |  -.9540614   .3893181    -2.45   0.014    -1.717111   -.1910119
                  |
             wt71 |  -.0878871   .0400115    -2.20   0.028    -.1663082   -.0094659
                  |
    c.wt71#c.wt71 |   .0006351   .0002257     2.81   0.005     .0001927    .0010775
                  |
            _cons |   3.754678   2.651222     1.42   0.157    -1.441622    8.950978
-----------------------------------------------------------------------------------

(option mu assumed; predicted mean cens)


(63 real changes made, 63 to missing)

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
      w_cens |      1,566    1.039197      .05646   1.001814   1.824624

(63 observations deleted)

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
     wt82_71 |      1,566      2.6383    7.879913  -41.28047   48.53839

file ./data/nhefs-wcens.dta saved
```

## Program 14.2

- G-estimation of a 1-parameter structural nested mean model
- Brute force search
- Data from NHEFS
- Section 14.5


```stata
use ./data/nhefs-wcens, clear

/*Generate test value of Psi = 3.446*/
gen psi = 3.446

/*Generate H(Psi) for each individual using test value of Psi and
their own values of weight change and smoking status*/
gen Hpsi = wt82_71 - psi * qsmk 

/*Fit a model for smoking status, given confounders and H(Psi) value, 
with censoring weights and display H(Psi) coefficient*/
logit qsmk sex race c.age##c.age ib(last).education ///
  c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ///
  ib(last).exercise ib(last).active c.wt71##c.wt71 Hpsi ///
  [pw = w_cens], cluster(seqn)
di _b[Hpsi]

/*G-estimation*/
/*Checking multiple possible values of psi*/
cap noi drop psi Hpsi

local seq_start = 2
local seq_end = 5
local seq_by = 0.1 // Setting seq_by = 0.01 will yield the result 3.46
local seq_len = (`seq_end'-`seq_start')/`seq_by' + 1
                 
matrix results = J(`seq_len', 4, 0)

qui gen psi = .
qui gen Hpsi = .

local j = 0

forvalues i =  `seq_start'(`seq_by')`seq_end' {
	local j = `j' + 1
	qui replace psi = `i'
	qui replace Hpsi = wt82_71 - psi * qsmk 
	quietly logit qsmk sex race c.age##c.age ///
	  ib(last).education c.smokeintensity##c.smokeintensity ///
	  c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active ///
	  c.wt71##c.wt71 Hpsi ///
	  [pw = w_cens], cluster(seqn)
	matrix p_mat = r(table)
	matrix p_mat = p_mat["pvalue","qsmk:Hpsi"]
	local p = p_mat[1,1]
	local b = _b[Hpsi]
	di "coeff", %6.3f `b', "is generated from psi", %4.1f `i'
	matrix results[`j',1]= `i'
	matrix results[`j',2]= `b'
	matrix results[`j',3]= abs(`b')
	matrix results[`j',4]= `p'
}
matrix colnames results = "psi" "B(Hpsi)" "AbsB(Hpsi)" "pvalue"
mat li results 

mata
res = st_matrix("results")
for(i=1; i<= rows(res); i++) { 
  if (res[i,3] == colmin(res[,3])) res[i,1]
}
end
* Setting seq_by = 0.01 will yield the result 3.46
```

```
Iteration 0:  Log pseudolikelihood = -936.10067  
Iteration 1:  Log pseudolikelihood = -879.13942  
Iteration 2:  Log pseudolikelihood = -877.82647  
Iteration 3:  Log pseudolikelihood = -877.82423  
Iteration 4:  Log pseudolikelihood = -877.82423  

Logistic regression                                     Number of obs =  1,566
                                                        Wald chi2(19) = 106.13
                                                        Prob > chi2   = 0.0000
Log pseudolikelihood = -877.82423                       Pseudo R2     = 0.0623

                                    (Std. err. adjusted for 1,566 clusters in seqn)
-----------------------------------------------------------------------------------
                  |               Robust
             qsmk | Coefficient  std. err.      z    P>|z|     [95% conf. interval]
------------------+----------------------------------------------------------------
              sex |  -.5137324   .1536024    -3.34   0.001    -.8147876   -.2126772
             race |  -.8608912   .2099415    -4.10   0.000    -1.272369   -.4494133
              age |   .1151589   .0502116     2.29   0.022      .016746    .2135718
                  |
      c.age#c.age |  -.0007593   .0005297    -1.43   0.152    -.0017976     .000279
                  |
        education |
               1  |  -.4710855   .2247701    -2.10   0.036    -.9116268   -.0305441
               2  |  -.5000231   .2208583    -2.26   0.024    -.9328974   -.0671487
               3  |  -.3833788    .195914    -1.96   0.050    -.7673632    .0006056
               4  |  -.4047116   .2836068    -1.43   0.154    -.9605707    .1511476
                  |
   smokeintensity |  -.0783425    .014645    -5.35   0.000    -.1070461   -.0496389
                  |
 c.smokeintensity#|
 c.smokeintensity |   .0010722   .0002651     4.04   0.000     .0005526    .0015917
                  |
         smokeyrs |  -.0711097    .026398    -2.69   0.007    -.1228488   -.0193705
                  |
       c.smokeyrs#|
       c.smokeyrs |   .0008153   .0004491     1.82   0.069     -.000065    .0016955
                  |
         exercise |
               0  |  -.3800465   .1889205    -2.01   0.044    -.7503238   -.0097692
               1  |  -.0437043   .1372725    -0.32   0.750    -.3127534    .2253447
                  |
           active |
               0  |  -.2134552   .2122025    -1.01   0.314    -.6293645    .2024541
               1  |  -.1793327    .207151    -0.87   0.387    -.5853412    .2266758
                  |
             wt71 |  -.0076607   .0256319    -0.30   0.765    -.0578983    .0425769
                  |
    c.wt71#c.wt71 |   .0000866   .0001582     0.55   0.584    -.0002236    .0003967
                  |
             Hpsi |  -1.90e-06   .0088414    -0.00   1.000    -.0173307    .0173269
            _cons |  -1.338367   1.359613    -0.98   0.325     -4.00316    1.326426
-----------------------------------------------------------------------------------

-1.905e-06










  6.         matrix p_mat = r(table)
  7.         matrix p_mat = p_mat["pvalue","qsmk:Hpsi"]
  8.         local p = p_mat[1,1]
  9.         local b = _b[Hpsi]
 10.         di "coeff", %6.3f `b', "is generated from psi", %4.1f `i'
 11.         matrix results[`j',1]= `i'
 12.         matrix results[`j',2]= `b'
 13.         matrix results[`j',3]= abs(`b')
 14.         matrix results[`j',4]= `p'
 15. }
coeff  0.027 is generated from psi  2.0
coeff  0.025 is generated from psi  2.1
coeff  0.023 is generated from psi  2.2
coeff  0.021 is generated from psi  2.3
coeff  0.019 is generated from psi  2.4
coeff  0.018 is generated from psi  2.5
coeff  0.016 is generated from psi  2.6
coeff  0.014 is generated from psi  2.7
coeff  0.012 is generated from psi  2.8
coeff  0.010 is generated from psi  2.9
coeff  0.008 is generated from psi  3.0
coeff  0.006 is generated from psi  3.1
coeff  0.005 is generated from psi  3.2
coeff  0.003 is generated from psi  3.3
coeff  0.001 is generated from psi  3.4
coeff -0.001 is generated from psi  3.5
coeff -0.003 is generated from psi  3.6
coeff -0.005 is generated from psi  3.7
coeff -0.007 is generated from psi  3.8
coeff -0.009 is generated from psi  3.9
coeff -0.011 is generated from psi  4.0
coeff -0.012 is generated from psi  4.1
coeff -0.014 is generated from psi  4.2
coeff -0.016 is generated from psi  4.3
coeff -0.018 is generated from psi  4.4
coeff -0.020 is generated from psi  4.5
coeff -0.022 is generated from psi  4.6
coeff -0.024 is generated from psi  4.7
coeff -0.026 is generated from psi  4.8
coeff -0.028 is generated from psi  4.9
coeff -0.030 is generated from psi  5.0



results[31,4]
            psi     B(Hpsi)  AbsB(Hpsi)      pvalue
 r1           2   .02672188   .02672188   .00177849
 r2         2.1   .02489456   .02489456   .00359089
 r3         2.2   .02306552   .02306552   .00698119
 r4         2.3   .02123444   .02123444   .01305479
 r5         2.4   .01940095   .01940095   .02346121
 r6         2.5   .01756472   .01756472   .04049437
 r7         2.6    .0157254    .0157254   .06710192
 r8         2.7   .01388267   .01388267   .10673812
 r9         2.8    .0120362    .0120362   .16301154
r10         2.9   .01018567   .01018567   .23912864
r11           3   .00833081   .00833081   .33720241
r12         3.1   .00647131   .00647131   .45757692
r13         3.2    .0046069    .0046069   .59835195
r14         3.3   .00273736   .00273736   .75528009
r15         3.4   .00086243   .00086243   .92212566
r16         3.5  -.00101809   .00101809   .90856559
r17         3.6  -.00290439   .00290439    .7444406
r18         3.7  -.00479666   .00479666   .59230593
r19         3.8  -.00669505   .00669505   .45731304
r20         3.9  -.00859969   .00859969    .3425138
r21           4  -.01051072   .01051072    .2488326
r22         4.1  -.01242824   .01242824   .17537691
r23         4.2  -.01435235   .01435235    .1199593
r24         4.3  -.01628313   .01628313   .07967563
r25         4.4  -.01822063   .01822063   .05142147
r26         4.5  -.02016492   .02016492   .03227271
r27         4.6  -.02211603   .02211603   .01971433
r28         4.7  -.02407401   .02407401   .01173271
r29         4.8  -.02603888   .02603888   .00680955
r30         4.9  -.02801063   .02801063   .00385828
r31           5  -.02998926   .02998926   .00213639

------------------------------------------------- mata (type end to exit) ------------
: res = st_matrix("results")

: for(i=1; i<= rows(res); i++) { 
>   if (res[i,3] == colmin(res[,3])) res[i,1]
> }
  3.4

: end
--------------------------------------------------------------------------------------
```

## Program 14.3

- G-estimation for 2-parameter structural nested mean model
- Closed form estimator
- Data from NHEFS
- Section 14.6


```stata
use ./data/nhefs-wcens, clear

/*create weights*/
logit qsmk sex race c.age##c.age ib(last).education ///
  c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ///
  ib(last).exercise ib(last).active c.wt71##c.wt71 ///
  [pw = w_cens], cluster(seqn)
predict pr_qsmk
summarize pr_qsmk

/* Closed form estimator linear mean models  **/
* ssc install tomata
putmata *, replace
mata: diff = qsmk - pr_qsmk
mata: part1 = w_cens :* wt82_71 :* diff
mata: part2 = w_cens :* qsmk :* diff
mata: psi = sum(part1)/sum(part2)

/*** Closed form estimator for 2-parameter model **/
mata
diff = qsmk - pr_qsmk
diff2 = w_cens :* diff

lhs = J(2,2, 0)
lhs[1,1] = sum( qsmk :* diff2)
lhs[1,2] = sum( qsmk :* smokeintensity :* diff2 )
lhs[2,1] = sum( qsmk :* smokeintensity :* diff2)
lhs[2,2] = sum( qsmk :* smokeintensity :* smokeintensity :* diff2 )
                                                                
rhs = J(2,1,0)
rhs[1] = sum(wt82_71 :* diff2 )
rhs[2] = sum(wt82_71 :* smokeintensity :* diff2 )

psi = (lusolve(lhs, rhs))'
psi
psi = (invsym(lhs'lhs)*lhs'rhs)'
psi
end
```

```
Iteration 0:  Log pseudolikelihood = -936.10067  
Iteration 1:  Log pseudolikelihood = -879.13943  
Iteration 2:  Log pseudolikelihood = -877.82647  
Iteration 3:  Log pseudolikelihood = -877.82423  
Iteration 4:  Log pseudolikelihood = -877.82423  

Logistic regression                                     Number of obs =  1,566
                                                        Wald chi2(18) = 106.13
                                                        Prob > chi2   = 0.0000
Log pseudolikelihood = -877.82423                       Pseudo R2     = 0.0623

                                    (Std. err. adjusted for 1,566 clusters in seqn)
-----------------------------------------------------------------------------------
                  |               Robust
             qsmk | Coefficient  std. err.      z    P>|z|     [95% conf. interval]
------------------+----------------------------------------------------------------
              sex |  -.5137295   .1533507    -3.35   0.001    -.8142913   -.2131677
             race |  -.8608919   .2099555    -4.10   0.000    -1.272397   -.4493867
              age |   .1151581   .0503079     2.29   0.022     .0165564    .2137598
                  |
      c.age#c.age |  -.0007593     .00053    -1.43   0.152    -.0017981    .0002795
                  |
        education |
               1  |  -.4710854   .2247796    -2.10   0.036    -.9116454   -.0305255
               2  |  -.5000247    .220776    -2.26   0.024    -.9327378   -.0673116
               3  |  -.3833802   .1954991    -1.96   0.050    -.7665515   -.0002089
               4  |  -.4047148   .2833093    -1.43   0.153    -.9599908    .1505613
                  |
   smokeintensity |  -.0783426   .0146634    -5.34   0.000    -.1070824   -.0496029
                  |
 c.smokeintensity#|
 c.smokeintensity |   .0010722   .0002655     4.04   0.000     .0005518    .0015925
                  |
         smokeyrs |  -.0711099   .0263523    -2.70   0.007    -.1227596   -.0194602
                  |
       c.smokeyrs#|
       c.smokeyrs |   .0008153   .0004486     1.82   0.069    -.0000639    .0016945
                  |
         exercise |
               0  |  -.3800461   .1890123    -2.01   0.044    -.7505034   -.0095887
               1  |  -.0437044    .137269    -0.32   0.750    -.3127467     .225338
                  |
           active |
               0  |  -.2134564   .2121759    -1.01   0.314    -.6293135    .2024007
               1  |  -.1793322   .2070848    -0.87   0.386    -.5852109    .2265466
                  |
             wt71 |  -.0076609   .0255841    -0.30   0.765    -.0578048     .042483
                  |
    c.wt71#c.wt71 |   .0000866   .0001572     0.55   0.582    -.0002216    .0003947
                  |
            _cons |  -1.338358   1.359289    -0.98   0.325    -4.002516      1.3258
-----------------------------------------------------------------------------------

(option pr assumed; Pr(qsmk))

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
     pr_qsmk |      1,566    .2607709    .1177584   .0514466   .7891403

(68 vectors posted)





------------------------------------------------- mata (type end to exit) ------------
: diff = qsmk - pr_qsmk

: diff2 = w_cens :* diff

: 
: lhs = J(2,2, 0)

: lhs[1,1] = sum( qsmk :* diff2)

: lhs[1,2] = sum( qsmk :* smokeintensity :* diff2 )

: lhs[2,1] = sum( qsmk :* smokeintensity :* diff2)

: lhs[2,2] = sum( qsmk :* smokeintensity :* smokeintensity :* diff2 )

:                                                                 
: rhs = J(2,1,0)

: rhs[1] = sum(wt82_71 :* diff2 )

: rhs[2] = sum(wt82_71 :* smokeintensity :* diff2 )

: 
: psi = (lusolve(lhs, rhs))'

: psi
                 1             2
    +-----------------------------+
  1 |  2.859470362   .0300412816  |
    +-----------------------------+

: psi = (invsym(lhs'lhs)*lhs'rhs)'

: psi
                 1             2
    +-----------------------------+
  1 |  2.859470362   .0300412816  |
    +-----------------------------+

: end
--------------------------------------------------------------------------------------
```
