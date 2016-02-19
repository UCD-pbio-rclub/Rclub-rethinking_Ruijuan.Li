# Statistical Rethinking Chapter 2, sections 2.1 - 2.3

Name: Ruijuan Li        

2E1 
>(2)

2E2 
>(3)

2E3 
>(1)

2E4 
>"The probability of water is 0.7" indicates that out of 10 times toss, the chance that you get the water side is 7 times. However, which side you get is not necessirily subjected to probability, instead, it is subjected to the gravity, the way you do the toss, whether there is wind when tossing, etc. 
2M3 
>Pr(earth|land) = Pr(land|earth)*Pr(earth)/Pr(land) = 0.3 * 0.5 / 130/200 = 0.23

2M4 
>conditonal on seeing black on one side, # of ways that a card could produce
    white/white = 0

    white/black = 1

    black/black =2

    Total ways to produce black =3. So the probability to produce black/black = 2/3

2M1

>WWW 

    p_grid <- seq(from = 0, to = 1, length.out = 20)

    prior <- rep(1, 20)

    likelihood <- dbinom(3, size = 3, prob = p_grid)

    unstd.posterior <- likelihood * prior

    posterior <- unstd.posterior / sum(unstd.posterior)

    plot(p_grid, posterior, type = "b", 

    xlab="probability of water", ylab="posterior probabilty")

    mtext("WWW")

>WWWL

    p_grid <- seq(from = 0, to = 1, length.out = 20)

    prior <- rep(1, 20)

    likelihood <- dbinom(3, size = 4, prob = p_grid)

    unstd.posterior <- likelihood * prior
    
    posterior <- unstd.posterior / sum(unstd.posterior)

    plot(p_grid, posterior, type = "b", 

    xlab="probability of water", ylab="posterior probabilty")

    mtext("WWWL")

>LWWLWWW

    p_grid <- seq(from = 0, to = 1, length.out = 20)

    prior <- rep(1, 20)

    likelihood <- dbinom(5, size = 7, prob = p_grid)

    unstd.posterior <- likelihood * prior

    posterior <- unstd.posterior / sum(unstd.posterior)

    plot(p_grid, posterior, type = "b", 

    xlab="probability of water", ylab="posterior probabilty")

    mtext("LWWLWWW")

>2M2

    p_grid <- seq(from = 0, to = 1, length.out = 20)

    prior <- rep(p_grid<0.5, 0, 1)

    likelihood <- dbinom(3, size = 3, prob = p_grid)

    unstd.posterior <- likelihood * prior

    posterior <- unstd.posterior / sum(unstd.posterior)

    plot(p_grid, posterior, type = "b", 

    xlab="probability of water", ylab="posterior probabilty")

    mtext("WWW")


>WWWL

    p_grid <- seq(from = 0, to = 1, length.out = 20)

    prior <- rep(p_grid<0.5, 0, 1)

    likelihood <- dbinom(3, size = 4, prob = p_grid)

    unstd.posterior <- likelihood * prior

    posterior <- unstd.posterior / sum(unstd.posterior)

    plot(p_grid, posterior, type = "b", 

    xlab="probability of water", ylab="posterior probabilty")

    mtext("WWWL")

>LWWLWWW

    p_grid <- seq(from = 0, to = 1, length.out = 20)

    prior <- rep(p_grid<0.5, 0, 1)

    likelihood <- dbinom(5, size = 7, prob = p_grid)

    unstd.posterior <- likelihood * prior

    posterior <- unstd.posterior / sum(unstd.posterior)

    plot(p_grid, posterior, type = "b", 

    xlab="probability of water", ylab="posterior probabilty")

    mtext("LWWLWWW")

>2M5

conditional on seeing black, number of ways that a card could produce:

white/white = 0

white/black =1

black/black = 4

total ways to have black = 5, so the probability to produce black * black = 4/5

>2M6

continuing on 2M4, 

conditonal on seeing black on one side, # of ways that a card could produce black

white/white = 0

white/black = 1

black/black = 2

number of ways to pull from the bag

white/white = 3

white/black = 2

black/black = 1

number of ways to produce black for each card 

white/white = 0*3

white/black = 1*2

black/black = 2*1

So the probability that the other side is also black is 2/4 = 0.5

>2M7

couting the ways each B/N & W/N could be drew

B/B = 2 for first draw, W/W = 2 or W/B = 1 for second draw; 2*(2+1)=6 ways to produce B/

B and W/N

B/W = 1 for first draw, W/W = 2 for second draw; 1*2=2 ways to produce B/W and W/W

W/W = 0 for first draw, 0 ways to produce other results.

So the probability to get B/B and W/N is 6/8=0.75

>2H1 

The question is Pr(twin_next)

Pr(twin_next) = Pr(A)XPr(A|twins) + Pr(B)XPr(B|twins), so need to know Pr(A|twin): the 

probability that this twin 

is from A

Pr(A|twin)=Pr(twin|A)X Pr(A) / Pr(twin) = 0.1X05/0.1X0.5+0.2X0.5 = 1/3 

Pr(B|twin)=1-1/3=2/3

so Pr(twin_next)= 0.1 X 1/3 + 0.2 X 2/3 = 0.167 

>2H2

Based on the last Q, the probability thi is A is 

Pr(A|twin)=Pr(twin|A)X Pr(A) / Pr(twin) = 0.1X05/0.1X0.5+0.2X0.5 = 1/3 

>2H3 

use the prior info that Pr(A) = 1/3

so the Q is Pr(A|single) 

Pr(A|single) = Pr(single|A)*Pr(A)/Pr(single)

Pr(single|A) = 0.9

Pr(A) = 1/3

Pr(single) = Pr(single|A)*Pr(A) + Pr(single|B)*Pr(B) = 0.9 * 1/3 + 0.8 * 2/3 = 0.833 
 
So Pr(A|single) = 0.9 * 1/3 / 0.833 = 0.36

>2H4

The Q is Pr(positive A|A)

Pr(positive A|A) = Pr(A|postive A) * Pr(A) / Pr(positive A) 

Pr(A|positive A) = 0.8

Pr(A) = 0.5

Pr(positive A): probability that you get postive A when its A and the 

probability that you get A when 

Pr(positive A) = Pr (postive A|A)*Pr(A) + Pr(postive A|B)*Pr(B) = 0.8*0.5 + 

(1-0.65)*0.5= 0.575

so Pr(postive A|A) = (0.8)(0.5)/(0.575)=0.69




