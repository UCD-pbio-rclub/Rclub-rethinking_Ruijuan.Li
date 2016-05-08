# Chapter-06-part1-assignment
Ruijuan Li  
May 8, 2016  

# 6E1

```r
# State the three motivating criteria that define information entropy. Try to express each in your own words. 
# 1) The measure of uncertainty should be continuous: small change in any of the possibility among all possibilities should result in a corresponding measurable small change in the uncertainty.   
# 2) the measure of uncertainty should increase as the number of possible events increases: as more possibilities esxist, the certainty of the model prediction becomes less for any particular possibility. 
# 3) the measure of uncertainty should be additive: the uncertainty over all possibilities should be additive.   
```

# 6E2

```r
# Suppose a coin is weighted such that, when it is tossed and lands on a table, it comes up heads 70% of the time. What is the entropy of this coin? 

p <- c(0.3, 0.7)
-sum(p*log(p)) # see R code 6.9 
```

```
## [1] 0.6108643
```

# 6E3

```r
# Suppose a four-sided die is loaded such that, when tossed onto a table, it shows “1” 20%, “2” 25%, “3” 25%, and “4” 30% of the time. What is the entropy of this die?

p <- c(0.2, 0.25, 0.25, 0.3)
-sum(p*log(p))
```

```
## [1] 1.376227
```

# 6E4 

```r
# Suppose another four-sided die is loaded such that it never shows “4”. The other three sides show equally often. What is the entropy of this die? 
p <- rep(1/3, 3) # see overthinking "more on entropy". events that never happen drop out. 
-sum(p*log(p)) 
```

```
## [1] 1.098612
```
