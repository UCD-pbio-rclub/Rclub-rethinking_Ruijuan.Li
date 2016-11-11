# Chapter-12-assignment-02
Ruijuan Li  
11/10/2016  
# using results='hide' to hide text result 

# 12M3

```r
# Re-estimate the basic Reed frog varying intercept model, but now using a Cauchy distribution in place of the Gaussian distribution for the varying intercepts. 

library(rethinking)
```

```
## Loading required package: rstan
```

```
## Warning: package 'rstan' was built under R version 3.2.5
```

```
## Loading required package: ggplot2
```

```
## Loading required package: StanHeaders
```

```
## Warning: package 'StanHeaders' was built under R version 3.2.5
```

```
## rstan (Version 2.10.1, packaged: 2016-06-24 13:22:16 UTC, GitRev: 85f7a56811da)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## rstan_options(auto_write = TRUE)
## options(mc.cores = parallel::detectCores())
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.59)
```

```r
data("reedfrogs")
d <- reedfrogs
str(d)
head(d)
d$tank <- 1:nrow(d)
head(d)
d$density 

m12M3.chapter <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1), 
    sigma ~ dcauchy(0, 1)  
  ), data = d, iter = 4000, chains = 4) 
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used
```

```
## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## The following numerical problems occured the indicated number of times after warmup on chain 1
```

```
##                                                                                 count
## Exception thrown at line 17: normal_log: Scale parameter is 0, but must be > 0!     1
```

```
## When a numerical problem occurs, the Metropolis proposal gets rejected.
```

```
## However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
```

```
## Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## The following numerical problems occured the indicated number of times after warmup on chain 2
```

```
##                                                                                 count
## Exception thrown at line 17: normal_log: Scale parameter is 0, but must be > 0!     2
```

```
## When a numerical problem occurs, the Metropolis proposal gets rejected.
```

```
## However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
```

```
## Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## The following numerical problems occured the indicated number of times after warmup on chain 3
```

```
##                                                                                 count
## Exception thrown at line 17: normal_log: Scale parameter is 0, but must be > 0!     2
```

```
## When a numerical problem occurs, the Metropolis proposal gets rejected.
```

```
## However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
```

```
## Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```

```r
m12M3.here <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dcauchy(a, sigma),
    a ~ dnorm(0, 1), 
    sigma ~ dcauchy(0, 1)  
  ), data = d, iter = 4000, chains = 4)
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## The following numerical problems occured the indicated number of times after warmup on chain 1
```

```
##                                                                                      count
## Exception thrown at line 17: cauchy_log: Scale parameter is inf, but must be finite!     1
```

```
## When a numerical problem occurs, the Metropolis proposal gets rejected.
```

```
## However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
```

```
## Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## The following numerical problems occured the indicated number of times after warmup on chain 4
```

```
##                                                                                 count
## Exception thrown at line 17: cauchy_log: Scale parameter is 0, but must be > 0!     1
```

```
## When a numerical problem occurs, the Metropolis proposal gets rejected.
```

```
## However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
```

```
## Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## Warning in FUN(X[[i]], ...): data with name pred is not numeric and not
## used

## Warning in FUN(X[[i]], ...): data with name size is not numeric and not
## used
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```
## Aggregated binomial counts detected. Splitting to 0/1 outcome for WAIC calculation.
```

```r
?Cauchy
?dcauchy
```

# 12M3 continue 

```r
# Compare the posterior means of the intercepts, αTANK, to the posterior means produced in the chapter, using the customary Gaussian prior. Can you explain the pattern of differences? 
precis(m12M3.chapter, depth = 2)
```

```
##             Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a_tank[1]   2.14   0.89       0.70       3.46  8000    1
## a_tank[2]   3.07   1.13       1.24       4.73  8000    1
## a_tank[3]   1.00   0.68      -0.02       2.15  8000    1
## a_tank[4]   3.08   1.12       1.32       4.77  8000    1
## a_tank[5]   2.16   0.88       0.67       3.46  8000    1
## a_tank[6]   2.13   0.88       0.77       3.50  8000    1
## a_tank[7]   3.09   1.13       1.44       4.95  8000    1
## a_tank[8]   2.13   0.89       0.72       3.49  8000    1
## a_tank[9]  -0.19   0.64      -1.25       0.80  8000    1
## a_tank[10]  2.14   0.88       0.78       3.56  8000    1
## a_tank[11]  1.00   0.68      -0.07       2.07  8000    1
## a_tank[12]  0.58   0.62      -0.46       1.54  8000    1
## a_tank[13]  1.00   0.67      -0.08       2.05  8000    1
## a_tank[14]  0.19   0.63      -0.78       1.22  8000    1
## a_tank[15]  2.14   0.88       0.71       3.44  8000    1
## a_tank[16]  2.14   0.90       0.64       3.44  8000    1
## a_tank[17]  2.91   0.79       1.57       4.02  8000    1
## a_tank[18]  2.40   0.67       1.36       3.46  8000    1
## a_tank[19]  2.01   0.58       1.10       2.94  8000    1
## a_tank[20]  3.67   1.00       2.09       5.15  8000    1
## a_tank[21]  2.40   0.64       1.40       3.41  8000    1
## a_tank[22]  2.39   0.67       1.31       3.39  8000    1
## a_tank[23]  2.41   0.66       1.39       3.44  8000    1
## a_tank[24]  1.70   0.52       0.83       2.48  8000    1
## a_tank[25] -1.01   0.44      -1.69      -0.30  8000    1
## a_tank[26]  0.15   0.40      -0.50       0.77  8000    1
## a_tank[27] -1.46   0.51      -2.24      -0.64  8000    1
## a_tank[28] -0.48   0.41      -1.13       0.17  8000    1
## a_tank[29]  0.16   0.39      -0.47       0.78  8000    1
## a_tank[30]  1.44   0.50       0.67       2.27  8000    1
## a_tank[31] -0.65   0.41      -1.30       0.01  8000    1
## a_tank[32] -0.32   0.40      -0.94       0.33  8000    1
## a_tank[33]  3.21   0.78       1.93       4.36  8000    1
## a_tank[34]  2.73   0.66       1.65       3.71  8000    1
## a_tank[35]  2.72   0.67       1.69       3.78  8000    1
## a_tank[36]  2.06   0.51       1.21       2.80  8000    1
## a_tank[37]  2.06   0.52       1.20       2.83  8000    1
## a_tank[38]  3.92   0.99       2.37       5.38  8000    1
## a_tank[39]  2.71   0.65       1.70       3.75  8000    1
## a_tank[40]  2.35   0.56       1.43       3.21  8000    1
## a_tank[41] -1.82   0.48      -2.63      -1.10  8000    1
## a_tank[42] -0.58   0.35      -1.12      -0.02  8000    1
## a_tank[43] -0.46   0.34      -1.00       0.09  8000    1
## a_tank[44] -0.34   0.35      -0.90       0.20  8000    1
## a_tank[45]  0.58   0.35       0.02       1.15  8000    1
## a_tank[46] -0.58   0.36      -1.13       0.03  8000    1
## a_tank[47]  2.07   0.51       1.28       2.89  8000    1
## a_tank[48]  0.00   0.33      -0.56       0.50  8000    1
## a           1.30   0.25       0.94       1.74  8000    1
## sigma       1.64   0.22       1.29       1.97  5650    1
```

```r
precis(m12M3.here, depth = 2) 
```

```
##             Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a_tank[1]   2.02   0.86       0.72       3.30  5693 1.00
## a_tank[2]  10.52  26.47       0.67      15.01    50 1.09
## a_tank[3]   1.09   0.62       0.10       2.05  8000 1.00
## a_tank[4]  12.74  38.39       0.45      15.90    37 1.09
## a_tank[5]   1.99   0.84       0.71       3.17  5176 1.00
## a_tank[6]   2.01   0.86       0.76       3.26  3452 1.00
## a_tank[7]   6.72  13.92       0.69      11.24   233 1.01
## a_tank[8]   2.01   0.86       0.76       3.24  5077 1.00
## a_tank[9]  -0.08   0.67      -1.14       0.97  8000 1.00
## a_tank[10]  2.00   0.83       0.77       3.19  8000 1.00
## a_tank[11]  1.09   0.61       0.13       2.07  8000 1.00
## a_tank[12]  0.73   0.62      -0.28       1.68  8000 1.00
## a_tank[13]  1.10   0.61       0.09       2.01  8000 1.00
## a_tank[14]  0.34   0.64      -0.68       1.36  8000 1.00
## a_tank[15]  2.00   0.87       0.67       3.22  4048 1.00
## a_tank[16]  2.01   0.85       0.75       3.31  8000 1.00
## a_tank[17]  2.88   0.97       1.39       4.21  5425 1.00
## a_tank[18]  2.27   0.67       1.21       3.24  8000 1.00
## a_tank[19]  1.92   0.55       1.06       2.76  8000 1.00
## a_tank[20] 29.42 135.18       1.33      22.52    42 1.07
## a_tank[21]  2.26   0.65       1.26       3.23  8000 1.00
## a_tank[22]  2.25   0.65       1.19       3.18  8000 1.00
## a_tank[23]  2.26   0.66       1.23       3.25  8000 1.00
## a_tank[24]  1.66   0.49       0.88       2.42  8000 1.00
## a_tank[25] -1.05   0.47      -1.75      -0.28  8000 1.00
## a_tank[26]  0.22   0.41      -0.44       0.85  8000 1.00
## a_tank[27] -1.57   0.55      -2.47      -0.73  8000 1.00
## a_tank[28] -0.46   0.43      -1.17       0.21  8000 1.00
## a_tank[29]  0.23   0.41      -0.39       0.91  8000 1.00
## a_tank[30]  1.44   0.44       0.72       2.12  8000 1.00
## a_tank[31] -0.65   0.44      -1.37       0.03  8000 1.00
## a_tank[32] -0.28   0.43      -1.00       0.37  8000 1.00
## a_tank[33]  3.22   0.95       1.81       4.58  5629 1.00
## a_tank[34]  2.62   0.68       1.54       3.65  8000 1.00
## a_tank[35]  2.61   0.70       1.58       3.68  8000 1.00
## a_tank[36]  1.98   0.49       1.17       2.71  8000 1.00
## a_tank[37]  1.97   0.48       1.19       2.69  8000 1.00
## a_tank[38] 13.49  26.89       1.57      24.61   597 1.00
## a_tank[39]  2.60   0.67       1.58       3.62  8000 1.00
## a_tank[40]  2.24   0.56       1.37       3.08  8000 1.00
## a_tank[41] -2.01   0.55      -2.82      -1.11  8000 1.00
## a_tank[42] -0.57   0.36      -1.16      -0.01  8000 1.00
## a_tank[43] -0.44   0.36      -1.03       0.12  8000 1.00
## a_tank[44] -0.31   0.34      -0.85       0.22  8000 1.00
## a_tank[45]  0.65   0.35       0.08       1.21  8000 1.00
## a_tank[46] -0.57   0.37      -1.16       0.02  8000 1.00
## a_tank[47]  1.97   0.49       1.20       2.71  8000 1.00
## a_tank[48]  0.05   0.35      -0.49       0.61  8000 1.00
## a           1.41   0.30       0.93       1.87  5051 1.00
## sigma       1.03   0.23       0.64       1.36  4513 1.00
```

```r
compare(m12M3.chapter, m12M3.here)
```

```
##                 WAIC pWAIC dWAIC weight    SE  dSE
## m12M3.chapter 1010.8  38.5     0   0.62 38.17   NA
## m12M3.here    1011.8  39.1     1   0.38 37.72 2.21
```

```r
post.chapter <- extract.samples(m12M3.chapter)
post.here <- extract.samples(m12M3.here)

# compute posterior mean for each tank 
# also transform to probability with logistic 
d$propsurv.est.chapter <- logistic(apply(post.chapter$a_tank, 2, mean))
d$propsurv.est.here <- logistic(apply(post.here$a_tank, 2, mean))

d.plot <- data.frame(row.names = c(1: 96),
                     tank = rep(c(1:48), 2),
                     propsurv.est = c(d$propsurv.est.here, d$propsurv.est.chapter),
                     class = c(rep("here", 48), rep("chapter", 48)),
                     size=rep(d$density,2))


p.1 <- ggplot(data = d.plot)
p.1 <- p.1 + geom_point(aes(x=tank, y = propsurv.est, color=factor(class)))
p.1 <- p.1 + facet_grid(~size)
p.1
```

![](Chapter-12-assignment-02_files/figure-html/unnamed-chunk-2-1.png)

```r
# as the sample size increases, the difference between the two models decreases, but which one is closer to the truth?  

# I don't quite understand the estimate got from precis, what if plot all of them together? 
estimated.chapter <- logistic(as.numeric(coef(m12M3.chapter)[1:48]))  
estimated.here <- logistic(as.numeric(coef(m12M3.here)[1:48]))

d.plot.2 <- data.frame(row.names = c(1: 192),
                     tank = rep(c(1:48), 4),
                     propsurv.est = c(d$propsurv.est.here, estimated.here, d$propsurv.est.chapter, estimated.chapter),
                     class = c(rep("here", 96), rep("chapter", 96)),
                     type=c(rep("posterior", 48), rep("estimate", 48), rep("posterior", 48), rep("estimate", 48)),
                     size=rep(d$density,4))

p.2 <- ggplot(data = d.plot.2)
p.2 <- p.2 + geom_point(aes(x=tank, y = propsurv.est, color=factor(class)))
p.2 <- p.2 + facet_grid(type~size)
p.2
```

![](Chapter-12-assignment-02_files/figure-html/unnamed-chunk-2-2.png)

```r
# precis result & posterior are the same thing? 
```


# 12H1

```r
# get data, 1934 women's fertility data 
data("bangladesh")
d.fertility <- bangladesh
head(d.fertility) # 1) district: where they are from 2) use.contraception: 0/1 indicate yes or no 3) urban: from city or rural area 
```

```
##   woman district use.contraception living.children age.centered urban
## 1     1        1                 0               4      18.4400     1
## 2     2        1                 0               1      -5.5599     1
## 3     3        1                 0               3       1.4400     1
## 4     4        1                 0               4       8.4400     1
## 5     5        1                 0               1     -13.5590     1
## 6     6        1                 0               1     -11.5600     1
```

```r
colnames(d.fertility) <- gsub("\\.", "_", colnames(d.fertility)) # remove dots 
head(d.fertility)
```

```
##   woman district use_contraception living_children age_centered urban
## 1     1        1                 0               4      18.4400     1
## 2     2        1                 0               1      -5.5599     1
## 3     3        1                 0               3       1.4400     1
## 4     4        1                 0               4       8.4400     1
## 5     5        1                 0               1     -13.5590     1
## 6     6        1                 0               1     -11.5600     1
```

```r
dim(d.fertility) # 1934    6 
```

```
## [1] 1934    6
```

```r
str(d.fertility)
```

```
## 'data.frame':	1934 obs. of  6 variables:
##  $ woman            : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ district         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ use_contraception: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ living_children  : int  4 1 3 4 1 1 4 4 2 4 ...
##  $ age_centered     : num  18.44 -5.56 1.44 8.44 -13.56 ...
##  $ urban            : int  1 1 1 1 1 1 1 1 1 1 ...
```

```r
# The first thing to do is ensure that the cluster variable, district, is a contiguous set of integers. Recall that these values will be index values inside the model. If there are gaps, you'll have parameters for which there is no data to inform them. Worse, the model probably won't run. Look at the unique values of the district variable: 

sort(unique(d.fertility$district))
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
## [24] 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
## [47] 47 48 49 50 51 52 53 55 56 57 58 59 60 61
```

```r
# District 54 is absent. So district isn't yet a good index variable, because it's not contiguous. This is easy to fix. Just make a new variable that is contiguous. This is enough to do it: 

d.fertility$district_id <- as.integer(as.factor(d.fertility$district))
sort(unique(d.fertility$district_id))
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
## [24] 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
## [47] 47 48 49 50 51 52 53 54 55 56 57 58 59 60
```


```r
# Now, focus on predicting use.contraception, clustered by district_id. Do not include urban just yet.  
# fit a traditional fixed-effects model that uses dummy variables for district 
# create dummy variable, how?  

# fit the model with dummy variables for each? 
m12H1.1 <- map2stan(
  alist(
    use_contraception ~ dbinom(1, p),
    logit(p) <- a_district[district_id],
    a_district[district_id] ~ dnorm(0, 5)
  ), data = d.fertility, iter = 4000, chains = 4) 
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```

```r
# fit a multilevel model with varying intercepts for district.  
m12H1.2 <- map2stan(
  alist(
    use_contraception ~ dbinom(1, p),
    logit(p) <- a_district[district_id],
    a_district[district_id] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1), 
    sigma ~ dcauchy(0, 1)  
  ), data = d.fertility, iter = 4000, chains = 4) 
```

```
## The following numerical problems occured the indicated number of times after warmup on chain 1
```

```
##                                                                                 count
## Exception thrown at line 16: normal_log: Scale parameter is 0, but must be > 0!     1
```

```
## When a numerical problem occurs, the Metropolis proposal gets rejected.
```

```
## However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
```

```
## Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## The following numerical problems occured the indicated number of times after warmup on chain 2
```

```
##                                                                                 count
## Exception thrown at line 16: normal_log: Scale parameter is 0, but must be > 0!     1
```

```
## When a numerical problem occurs, the Metropolis proposal gets rejected.
```

```
## However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
```

```
## Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## The following numerical problems occured the indicated number of times after warmup on chain 3
```

```
##                                                                                 count
## Exception thrown at line 16: normal_log: Scale parameter is 0, but must be > 0!     1
```

```
## When a numerical problem occurs, the Metropolis proposal gets rejected.
```

```
## However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
```

```
## Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## The following numerical problems occured the indicated number of times after warmup on chain 4
```

```
##                                                                                 count
## Exception thrown at line 16: normal_log: Scale parameter is 0, but must be > 0!     1
```

```
## When a numerical problem occurs, the Metropolis proposal gets rejected.
```

```
## However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
```

```
## Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
```

```
## Computing WAIC
```

```
## Constructing posterior predictions
```


```r
precis(m12H1.2, depth = 2)
```

```
##                 Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
## a_district[1]  -1.00   0.20      -1.31      -0.68  8000    1
## a_district[2]  -0.60   0.36      -1.17      -0.03  8000    1
## a_district[3]  -0.22   0.52      -1.07       0.56  8000    1
## a_district[4]  -0.18   0.31      -0.68       0.31  8000    1
## a_district[5]  -0.58   0.28      -1.03      -0.12  8000    1
## a_district[6]  -0.82   0.25      -1.19      -0.42  8000    1
## a_district[7]  -0.76   0.37      -1.33      -0.14  8000    1
## a_district[8]  -0.51   0.30      -0.98      -0.04  8000    1
## a_district[9]  -0.72   0.35      -1.26      -0.16  8000    1
## a_district[10] -1.16   0.44      -1.88      -0.48  8000    1
## a_district[11] -1.58   0.44      -2.26      -0.90  8000    1
## a_district[12] -0.61   0.31      -1.11      -0.10  8000    1
## a_district[13] -0.42   0.33      -0.92       0.14  8000    1
## a_district[14]  0.39   0.18       0.10       0.67  8000    1
## a_district[15] -0.56   0.35      -1.11       0.00  8000    1
## a_district[16] -0.11   0.35      -0.66       0.44  8000    1
## a_district[17] -0.76   0.35      -1.33      -0.23  8000    1
## a_district[18] -0.64   0.27      -1.06      -0.20  8000    1
## a_district[19] -0.50   0.32      -1.03       0.00  8000    1
## a_district[20] -0.48   0.39      -1.13       0.11  8000    1
## a_district[21] -0.50   0.36      -1.07       0.08  8000    1
## a_district[22] -0.97   0.38      -1.56      -0.36  8000    1
## a_district[23] -0.77   0.40      -1.41      -0.14  8000    1
## a_district[24] -1.20   0.44      -1.84      -0.45  8000    1
## a_district[25] -0.27   0.23      -0.65       0.08  8000    1
## a_district[26] -0.52   0.40      -1.15       0.11  8000    1
## a_district[27] -1.19   0.31      -1.67      -0.68  8000    1
## a_district[28] -0.97   0.28      -1.41      -0.53  8000    1
## a_district[29] -0.81   0.32      -1.32      -0.30  8000    1
## a_district[30] -0.14   0.23      -0.52       0.22  8000    1
## a_district[31] -0.30   0.29      -0.78       0.15  8000    1
## a_district[32] -0.99   0.36      -1.55      -0.42  8000    1
## a_district[33] -0.42   0.39      -1.05       0.18  8000    1
## a_district[34]  0.29   0.30      -0.22       0.75  8000    1
## a_district[35] -0.13   0.26      -0.55       0.28  8000    1
## a_district[36] -0.58   0.37      -1.17       0.01  8000    1
## a_district[37] -0.21   0.40      -0.86       0.42  8000    1
## a_district[38] -0.71   0.40      -1.35      -0.08  8000    1
## a_district[39] -0.20   0.31      -0.70       0.29  8000    1
## a_district[40] -0.26   0.27      -0.70       0.16  8000    1
## a_district[41] -0.20   0.32      -0.72       0.30  8000    1
## a_district[42] -0.23   0.41      -0.92       0.39  8000    1
## a_district[43] -0.03   0.27      -0.46       0.39  8000    1
## a_district[44] -0.96   0.35      -1.49      -0.40  8000    1
## a_district[45] -0.66   0.29      -1.12      -0.20  8000    1
## a_district[46]  0.00   0.21      -0.33       0.32  8000    1
## a_district[47] -0.34   0.38      -0.93       0.27  8000    1
## a_district[48] -0.08   0.27      -0.50       0.37  8000    1
## a_district[49] -0.87   0.51      -1.71      -0.07  8000    1
## a_district[50] -0.29   0.35      -0.85       0.26  8000    1
## a_district[51] -0.27   0.28      -0.70       0.20  8000    1
## a_district[52] -0.30   0.23      -0.66       0.09  8000    1
## a_district[53] -0.43   0.35      -0.98       0.13  8000    1
## a_district[54] -0.80   0.46      -1.49      -0.03  8000    1
## a_district[55]  0.10   0.27      -0.33       0.51  8000    1
## a_district[56] -1.08   0.35      -1.63      -0.54  8000    1
## a_district[57] -0.30   0.30      -0.78       0.17  8000    1
## a_district[58] -1.01   0.45      -1.71      -0.28  8000    1
## a_district[59] -1.00   0.32      -1.50      -0.49  8000    1
## a_district[60] -1.06   0.30      -1.53      -0.59  8000    1
## a              -0.54   0.09      -0.68      -0.40  5941    1
## sigma           0.53   0.08       0.39       0.66  2715    1
```














