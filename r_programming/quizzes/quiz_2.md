Quiz 2
========================================================

Quesion 1
----------------------
Suppose I define the following function in R:

```r
cube <- function(x, n) {
    x^3
}
```


What is the result of running:

```r
cube(3)
```

```
## [1] 27
```



Question 2
--------------------------
The following code will produce a warning in R.


```r
x <- 1:10
if (x > 5) {
    x <- 0
}
```

```
## Warning: the condition has length > 1 and only the first element will be
## used
```

Why? 

Question 3
---------------------------
Consider the following function


```r
ff <- function(x) {
    g <- function(y) {
        y + z
    }
    z <- 4
    x + g(x)
}
# changed f to ff so it wouldn't confuse the code later on!
```


then run:

```r
z <- 10
f(3)
```

```
## Error: could not find function "f"
```

What value is returned?

Question 4
-------------------------
Consider the following expression:


```r
x <- 5
y <- if (x < 3) {
    NA
} else {
    10
}
```


What is the value of 'y' after evaluating this expression?

```r
y
```

```
## [1] 10
```


Question 5
---------------------------------
Consider the following R function

```r
h <- function(x, y = NULL, d = 3L) {
    z <- cbind(x, d)
    if (!is.null(y)) 
        z <- z + y else z <- z + f
    g <- x + y/z
    if (d == 3L) 
        return(g)
    g <- g + 10
    g
}
```


Which symbol in the above function is a free variable?
 
 * see notes on free variables [here](http://www.stat.berkeley.edu/~statcur/Workshop2/Presentations/functions.pdf)
 * The scoping rules determine how a value is associated with a free variable in a function
 * The scoping rules of a language determine how values are assigned to free variables. 
 * Free variables are not formal arguments and are not local variables (assigned insided the function body).
 * __options are: f,z,d,L,g__


```r
x <- 1:10
h(x)
```

```
## Error: object 'f' not found
```


### Questions 6-10

__answers are all found [here](http://www.stat.berkeley.edu/~statcur/Workshop2/Presentations/functions.pdf)__
