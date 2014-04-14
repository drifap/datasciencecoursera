Quiz 4
========================================================

Question 1
--------------------
What is produced at the end of this snippet of R code?

```r
set.seed(1)
rpois(5, 2)
```

```
## [1] 1 1 2 4 1
```


 1. A vector with the numbers 1, 4, 1, 1, 5
 2. __A vector with the numbers 1, 1, 2, 4, 1__
 3. It is impossible to tell because the result is random
 4. A vector with the numbers 3.3, 2.5, 0.5, 1.1, 1.7

Question 2
-----------------------
What R function can be used to generate standard Normal random variables?

```r
# rnorm is for random deviates
args(rnorm)  ## this one
```

```
## function (n, mean = 0, sd = 1) 
## NULL
```

```r
# qnorm is for quantile function
args(qnorm)
```

```
## function (p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
## NULL
```

```r
# dnorm is for density
args(dnorm)
```

```
## function (x, mean = 0, sd = 1, log = FALSE) 
## NULL
```

```r
# pnorm is for the distribution function
args(pnorm)
```

```
## function (q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
## NULL
```



Question 3
-------------------------
When simulating data, why is using the set.seed() function important?

 1. It ensures that the sequence of random numbers is truly random.
 2. It ensures that the random numbers generated are within specified boundaries.
 3. It can be used to generate non-uniform random numbers.
 4. __It ensures that the sequence of random numbers is reproducible.__


Question 4
-------------------------
Which function can be used to evaluate the inverse cumulative distribution function for the Poisson distribution?


```r
# dpois gives the (log) density
args(dpois)
```

```
## function (x, lambda, log = FALSE) 
## NULL
```

```r
# qpois gives the quantile function
args(qpois)  ## this one?
```

```
## function (p, lambda, lower.tail = TRUE, log.p = FALSE) 
## NULL
```

```r
# ppois gives the (log) distribution function
args(ppois)
```

```
## function (q, lambda, lower.tail = TRUE, log.p = FALSE) 
## NULL
```

```r
# rpois generates random deviates
args(rpois)
```

```
## function (n, lambda) 
## NULL
```



Question 5
------------------------------
What does the following code do?


```r
set.seed(10)
x <- rbinom(10, 10, 0.5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
```


 1. Generate random exponentially distributed data
 2. Generate data from a Poisson generalized linear model
 3. __Generate data from a Normal linear model__
 4. Generate uniformly distributed random data

Question 6
------------------------------
What R function can be used to generate Binomial random variables?


```r
# dbinom gives the density
args(dbinom)
```

```
## function (x, size, prob, log = FALSE) 
## NULL
```

```r
# rbinom generates random deviates
args(rbinom)  ## this one?
```

```
## function (n, size, prob) 
## NULL
```

```r
# pbinom gives the distribution function
args(pbinom)
```

```
## function (q, size, prob, lower.tail = TRUE, log.p = FALSE) 
## NULL
```

```r
# qbinom gives the quantile function
args(qbinom)
```

```
## function (p, size, prob, lower.tail = TRUE, log.p = FALSE) 
## NULL
```



Question 7
-----------------------------
What aspect of the R runtime does the profiler keep track of when an R expression is evaluated?
 1. __the function call stack__ (see lecture notes)
 2. the working directory
 3. the package search list
 4. the global environment

Question 8
-----------------------------
Consider the following R code


```r
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
```

```
## Error: object 'x1' not found
```

```r
Rprof(NULL)
```


(Assume that y, x1, and x2 are present in the workspace.) Without running the code, what percentage of the run time is spent in the 'lm' function, based on the 'by.total' method of normalization shown in 'summaryRprof()'?
 1. __100%__
 2. 50%
 3. 23%
 4. It is not possible to tell

Question 9
----------------------------
When using 'system.time()', what is the user time?
 1. __It is the time spent by the CPU evaluating an expression__
 2. It is the "wall-clock" time it takes to evaluate an expression
 3. It is a measure of network latency
 4. It is the time spent by the CPU waiting for other tasks to finish

Question 10
-------------------------------
If a computer has more than one available processor and R is able to take advantage of that, then which of the following is true when using 'system.time()'?
 1. __elapsed time may be smaller than user time__
 2. elapsed time is 0
 3. user time is always smaller than elapsed time
 4. user time is 0
