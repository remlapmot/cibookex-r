# (PART\*) Stata code {-}

# 11. Why model?{-}


```r
library(Statamarkdown)
```



## Program 11.1

- Sample averages by treatment level
- Data from Figures 11.1 and 11.2


```stata
qui input A Y
1 200
1 150
1 220
1 110
1 50
1 180
1 90
1 170
0 170
0 30
0 70
0 110
0 80
0 50
0 10
0 20
end

scatter Y A, ylab(0(50)250) xlab(0 1) xscale(range(-0.5 1.5))
qui gr export ./figs/fig11-1.png, replace

bysort A: sum Y
```

<img src="./figs/fig11-1.png" width="85%" style="display: block; margin: auto;" />


```stata
qui input A Y
1 110
1 80
1 50
1 40
2 170
2 30
2 70
2 50
3 110
3 50
3 180
3 130
4 200
4 150
4 220
4 210
end

scatter Y A, ylab(0(50)250) xlab(0(1)4) xscale(range(0 4.5))
qui gr export ./figs/fig11-2.png, replace

bysort A: sum Y
```

<img src="./figs/fig11-2.png" width="85%" style="display: block; margin: auto;" />

## Program 11.2

- 2-parameter linear model
- Data from Figures 11.3 and 11.1


```stata
input A Y
3   21	
11	54
17	33
23	101
29	85
37	65
41	157
53	120
67	111
79	200
83	140
97	220
60	230
71	217
15	11
45  190
end
```


```stata
* Fit the regression model
regress Y A, cformat(%5.2f)

* Output the estimated mean Y value when A = 90
lincom _b[_cons] + 90*_b[A]

* Plot the data with the regression line: Fig 11.4
twoway ///
  lfitci Y A ///
  || scatter Y A ///
  , ylab(0(50)250) xlab(0(10)100) xscale(range(0 100))
qui gr export ./figs/fig11-4.png, replace
```

<img src="figs/fig11-4.png" width="85%" style="display: block; margin: auto;" />

## Program 11.3

- 3-parameter linear model
- Data from Figure 11.3


```stata
* Create the product term
gen Asq = A*A

* Fit the regression model
regress Y A Asq, cformat(%5.2f)

* Output the estimated mean Y value when A = 90
lincom _b[_cons] + 90*_b[A] + 90*90*_b[Asq]

* Plot the data with the regression line: Fig 11.5
twoway ///
  qfitci Y A ///
  || scatter Y A ///
  , ylab(0(50)250) xlab(0(10)100) xscale(range(0 100))
qui gr export ./figs/fig11-5.png, replace
```

<img src="figs/fig11-5.png" width="85%" style="display: block; margin: auto;" />
