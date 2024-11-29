
## Exercise 3

### Part 1.

df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

df


### Part 2.


nll_lm <- function(data, par) {
  
  x1 <- data$x1
  x2 <- data$x2
  x3 <- data$x3
  y <- data$y
  
  beta0 <- par[1]
  beta1 <- par[2]
  beta2 <- par[3]
  beta3 <- par[4]
  
  yhat <- beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
  e <- y - yhat
  sigma2 <- var(e)
  
  
  nll <- (length(y) / 2) * log(2 * pi * sigma2) + (1 / (2 * sigma2)) * sum(e^2)
  
  return(nll)
}

par <- c(0, 1, 1, 1)

nll <- nll_lm(df, par)
print(nll)


### Part 3.


nll_lm <- function(data, par) {
  x1 <- data$x1
  x2 <- data$x2
  x3 <- data$x3
  y <- data$y
  
  beta0 <- par[1]
  beta1 <- par[2]
  beta2 <- par[3]
  beta3 <- par[4]
  sigma <- par[5]
  
  yhat <- beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
  
  e <- y - yhat
  
  nll <- (length(y) / 2) * log(2 * pi * sigma^2) + (1 / (2 * sigma^2)) * sum(e^2)
  
  return(nll)
}

guess <- c(mean(df$y), 0, 0, 0, sd(df$y)) 

result <- optim(par = guess, 
                fn = nll_lm, 
                data = df, 
                method = "L-BFGS-B",
                lower = c(-Inf, -Inf, -Inf, -Inf, 0.000001), 
                upper = c(Inf, Inf, Inf, Inf, Inf))  


params <- result$par


sprintf("Beta0: %f", params[1])
sprintf("Beta1: %f", params[2])
sprintf("Beta2: %f", params[3])
sprintf("Beta3: %f", params[4])
sprintf("Sigma: %f", params[5])



### Part 4.

##fill

### Part 5.

X <- cbind(1, df$x1, df$x2, df$x3)
y <- df$y

ols <- solve(t(X) %*% X) %*% t(X) %*% y


sprintf("Beta0: %f", ols[1])
sprintf("Beta1: %f", ols[2])
sprintf("Beta2: %f", ols[3])
sprintf("Beta3: %f", ols[4])

### Part 6.

X <- cbind(1, df$x1, df$x2, df$x3) 
y <- df$y
betaH <- solve(t(X) %*% X) %*% t(X) %*% y

e <- y - X %*% betaH

n <- length(y)  
p <- ncol(X)    
sigma2 <- sum(e^2) / (n - p)

sigmaH <- sqrt(sigma2)

sprintf("New sigma hat: %f", sigmaH)
sprintf("Previous: %f", params[5])

### Part 7.

## why is max likelihood SD different from estimate of least squares SD

