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


```stata
*Data preprocessing***

use ./data/nhefs-formatted, clear
gen byte cens = (wt82 == .)
```

```

```

## Program 14.1
- Ranks of extreme observations
- Data from NHEFS
- Section 14.4


```stata
/*For Stata 15 or later, first install the extremes function using this code:*/
ssc install extremes 

/*Ranking of extreme observations*/
extremes wt82_71 seqn

/*Estimate unstabilized censoring weights for use in g-estimation models*/
glm cens qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71, family(binomial)
predict pr_cens
gen w_cens = 1/(1-pr_cens)
replace w_cens = . if cens == 1 /*observations with cens = 1 contribute to censoring models but not outcome model*/
summarize w_cens

/*Analyses restricted to N=1566*/
drop if wt82 == .
summarize wt82_71
```

```
checking extremes consistency and verifying not already installed...
all files already exist and are up to date.


  +------------------------------+
  |  obs:        wt82_71    seqn |
  |------------------------------|
  | 1101.   -41.28046982   23321 |
  |  490.   -30.50192161   13593 |
  | 1027.   -30.05007421   24363 |
  |  421.   -29.02579305    5412 |
  |  145.   -25.97055814   21897 |
  +------------------------------+

  +-----------------------------+
  |  576.   34.01779932    5415 |
  |  577.   36.96925111   22342 |
  |  364.   37.65051215    1769 |
  | 1164.   47.51130337    6928 |
  |  939.   48.53838568   23522 |
  +-----------------------------+

outcome does not vary; remember:
                                  0 = negative outcome,
        all other nonmissing values = positive outcome
r(2000);

end of do-file
r(2000);
```

## Program 14.2
- G-estimation of a 1-parameter structural nested mean model
- Brute force search
- Data from NHEFS
- Section 14.5


```stata
/*Generate test value of Psi = 3.446*/
gen psi = 3.446

/*Generate H(Psi) for each individual using test value of Psi and their own values of weight change and smoking status*/
gen Hpsi = wt82_71 - psi * qsmk 

/*Fit a model for smoking status, given confounders and H(Psi) value, with censoring weights and display H(Psi) coefficient*/
logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 Hpsi [pw = w_cens], cluster(seqn)
di _b[Hpsi]

/*G-estimation*/
/*Checking multiple possible values of psi*/
drop psi Hpsi

local seq_start = 2
local seq_end = 5
local seq_by = 0.1 // Setting seq_by = 0.01 will yield the result 3.46
local seq_len = (`seq_end'-`seq_start')/`seq_by' + 1
matrix results = J(`seq_len',4,0)
matrix list results
gen psi = .
gen Hpsi = .
local j = 0
forvalues i =  `seq_start'(`seq_by')`seq_end' {
	local j = `j'+1
	replace psi = `i'
	replace Hpsi = wt82_71 - psi * qsmk 
	quietly logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 Hpsi [pw = w_cens], cluster(seqn)
	matrix p_mat = r(table)["pvalue","qsmk:Hpsi"]
	local p = p_mat[1,1]
	local b = _b[Hpsi]
	di "coeff `b' is generated from psi `i'"
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
w_cens not found
r(111);

end of do-file
r(111);
```

## Program 14.3
- G-estimation for 2-parameter structural nested mean model
- Closed form estimator
- Data from NHEFS
- Section 14.6


```stata
/*create weights*/
logit qsmk sex race c.age##c.age ib(last).education c.smokeintensity##c.smokeintensity c.smokeyrs##c.smokeyrs ib(last).exercise ib(last).active c.wt71##c.wt71 [pw = w_cens], cluster(seqn)  
predict pr_qsmk
summarize pr_qsmk

/* Closed form estimator linear mean models  **/
ssc install tomata
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
w_cens not found
r(111);

end of do-file
r(111);
```
