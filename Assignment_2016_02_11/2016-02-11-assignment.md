# Statistical Rethinking Chapter 2, sections 2.1 - 2.3

Name: Ruijuan Li        

## 2E1 (2)

## 2E2 (3)

## 2E3 (1)

## 2E4  "The probability of water is 0.7" indicates that out of 10 times toss, the chance that you get the water side is 7 times. However, which side you get is not necessirily subjected to probability, instead, it is subjected to the gravity, the way you do the toss, whether there is wind when tossing, etc. 
## 2M3 Pr(earth|land) = Pr(land|earth)*Pr(earth)/Pr(land) = 0.3 * 0.5 / 130/200 = 0.23

## 2M4 conditonal on seeing black on one side, # of ways that a card could produce
    white/white = 0
    white/black = 1
    black/black =2
    Total ways to produce black =3. So the probability to produce black/black = 2/3

## 2M1

WWW 
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 3, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# R code 2.4, display the posterior distribution
plot(p_grid, posterior, type = "b", 
xlab="probability of water", ylab="posterior probabilty")
mtext("WWW")

WWWL
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 4, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# R code 2.4, display the posterior distribution
plot(p_grid, posterior, type = "b", 
xlab="probability of water", ylab="posterior probabilty")
mtext("WWWL")

LWWLWWW
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(5, size = 7, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# R code 2.4, display the posterior distribution
plot(p_grid, posterior, type = "b", 
xlab="probability of water", ylab="posterior probabilty")
mtext("LWWLWWW")

## 2M2
WWW
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(p_grid<0.5, 0, 1)

# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 3, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# R code 2.4, display the posterior distribution
plot(p_grid, posterior, type = "b", 
xlab="probability of water", ylab="posterior probabilty")
mtext("WWW")


WWWL
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(p_grid<0.5, 0, 1)

# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 4, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# R code 2.4, display the posterior distribution
plot(p_grid, posterior, type = "b", 
xlab="probability of water", ylab="posterior probabilty")
mtext("WWWL")

LWWLWWW
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define prior
prior <- rep(p_grid<0.5, 0, 1)

# compute likelihood at each value in grid
likelihood <- dbinom(5, size = 7, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# R code 2.4, display the posterior distribution
plot(p_grid, posterior, type = "b", 
xlab="probability of water", ylab="posterior probabilty")
mtext("LWWLWWW")

## 2M5
conditional on seeing black, number of ways that a card could produce:
white/white = 0
white/black =1
black/black = 4
total ways to have black = 5, so the probability to produce black * black = 4/5

## 2M6
continuing on 2M4, 
conditonal on seeing black on one side, # of ways that a card could produce black
white/white = 0
white/black = 1
black/black = 2

# of ways to pull from the bag
white/white = 3
white/black = 2
black/black = 1

# of ways to produce black for each card 
white/white = 0*3
white/black = 1*2
black/black = 2*1

So the probability that the other side is also black is 2/4 = 0.5

## 2M7

## 2H1 
10% X 1/2 + 20 X 1/2 = 15% ???

## 2H2 -- 2H3 
should grid approximation be used here??? 




































