## This is the code to reproduce the "Post selection inference" simulation
## Copyright: ZDS

######
### loading necessary packages
library("glmnet")
library("ggplot2")

######
### simulation

## setting seed for reproducibility
set.seed(26)
## number of observations
n <- 100
## number of predictors
p <- 80
## values generated from a Normal(0,1)
random.noise <- rnorm(n * p, mean = 0, sd = 1)
## allocate the random noise into n rows
X <- matrix(random.noise, nrow = n)
## create an independent response variable, also Normal(0,1) 
Y <- rnorm(n, mean = 0, sd = 1)

## fitting the linear model
lm.full <- lm(Y ~ X)
summary(lm.full)


### model selection
## alpha = 1 performs lasso
cvfit <- cv.glmnet(x = X, y = Y, alpha = 1)
## the coefficients of the cross-validated lasso are
coef.lasso <- coef(cvfit, s = "lambda.min")
## store the coefficients of the model, except for the intercept
coefs.selected.model <- coef.lasso[- 1]

## we store the data X as a data frame, just so that when we select the variables
## in the next step, we do not loose the variable names
X <- as.data.frame(X)
## select the variables obtained by the lasso
X.sel <- X[, which(coefs.selected.model != 0)]
## back transforming the data frame X.sel into a matrix
X.sel <- as.matrix(X.sel)

## fitting the linear model based on only the selected predictors
lm.selected <- lm(Y ~ X.sel)
summary(lm.selected)



### simulation inspecting the distribution of the estimated coefficients

######
### First, without model selection

## empty vector to store coefficients
beta1.vec <- c() 
set.seed(3)

while (length(beta1.vec) < 1000){
  n <- 100  ## number of observations
  p <- 3    ## number of predictors
  ## values generated from a Normal(0,1)
  random.noise <- rnorm(n * p, mean = 0, sd = 1)
  ## allocate the random noise into n rows
  X <- matrix(random.noise, nrow = n)
  ## create an independent response variable, also Normal(0,1) 
  Y <- rnorm(n, mean = 0, sd = 1)
  ## fit the linear model
  lm.fit <- lm(Y ~ X)
  ## store the coefficient of X1
  ## the first coefficient is the intercept
  beta1.vec <- c(beta1.vec, coef(lm.fit)[2])
}

## histogram
ggplot(data = as.data.frame(beta1.vec),
       aes(x = beta1.vec)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "steelblue2",
                 binwidth = 0.005) +
  geom_density(lwd = 1.2, colour = 2) +
  xlim(c(-0.4, 0.4)) + 
  ggtitle(label = expression(paste("Distribution of ", beta[1] ,
                                   " without model selection"))) +
  xlab(expression(paste("Values of ", beta[1])))

## QQ-plot
qqnorm(beta1.vec)
qqline(beta1.vec)


### Then, with data-driven model selection

# empty vector to store coefficients
beta1.vec.sel <- c()
set.seed(3)

## this takes a few minutes to run!!
while (length(beta1.vec.sel) < 1000){
  n <- 100  ## number of observations
  p <- 3    ## number of predictors
  random.noise <- rnorm(n * p, mean = 0, sd = 1)
  X <- matrix(random.noise, nrow = n)
  Y <- rnorm(n, mean = 0, sd = 1)
  ## apply the lasso for model selection
  cvfit <- cv.glmnet(x = X, y = Y, alpha = 1)
  ## save the coefficients of the cv lasso (without intercept)
  coefs.selected.model <- coef(cvfit, s = "lambda.min")[- 1]
  ## if X1 is selected, we store the coef of the refitted model
  if(coefs.selected.model[1] != 0){
    ## refitting linear model with selected variables
    X.sel <- X[, which(coefs.selected.model != 0)]
    lm.selected <- lm(Y ~ X.sel)
    beta1.vec.sel <- c(beta1.vec.sel, coef(lm.selected)[2])
  }
}

## histogram
ggplot(data = as.data.frame(beta1.vec.sel),
       aes(x = beta1.vec.sel)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "steelblue2",
                 binwidth = 0.005) +
  geom_density(lwd = 1.2, colour = 2) +
  xlim(c(-0.4, 0.4)) + 
  ggtitle(label = expression(paste("Distribution of ", beta[1] ,
                                   " with model selection"))) +
  xlab(expression(paste("Values of ", beta[1])))


## QQ-plot
qqnorm(beta1.vec.sel)
qqline(beta1.vec.sel)

