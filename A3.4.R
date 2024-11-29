df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

nll_lm.2 <- function(x,par = 1:1) {
  sigma_2 <- var(x[,1])
  y <- x[,1]
  if(min(par) == 0) {
    x.matrix <- as.matrix(rep(1, length(x[,1])))
  } else if(max(par) > ncol(x)){
    stop("Wrong parameters")
  }
  else {
    x.matrix <- cbind(rep(1, length(x[,1])), as.matrix(x[,par+1]))
  }
  beta <- as.vector(as.matrix(solve(crossprod(x.matrix)) %*% crossprod(x.matrix,y)))
  
  resid <- as.vector((y - x.matrix%*%beta))
  #l_theta <- length(x[,1])/2 * log(1/(2*pi*sigma_2)) + 
  #    1/(2*sigma_2)*(sum(resid^2))
  l_theta <- -log(prod(dnorm(resid, mean= 0, sd = sqrt(sigma_2))))
  
  nll_lm_list <- list(residuals = resid, log_theta <- l_theta, betas = list(
    intercept = beta[1],
    x_s = beta[-1L]),
    sd =sqrt(sigma_2))
  class(nll_lm_list) <- "nll_lm"
  
  return(nll_lm_list)
  #return(l_theta)
}

nll_lm.3 <- function(params, y, ...) {
  
  sigma_2 <- var(y[,1])
  beta <- as.matrix(params)
  
  x.matrix <- cbind(rep(1, length(y[,1])), as.matrix(y[,-1L]))
  
  resid <- as.vector((y[,1] - x.matrix%*%beta))
  
  l_theta <- -log(prod(dnorm(resid, mean = 0, sd = sqrt(sigma_2))))
  #l_theta <- length(y[,1])/2 * log(1/(2*pi*sigma_2)) + 1/(2*sigma_2)*(sum(resid^2))
  
  return(l_theta)
}

nll_lm.6 <- function(params, y, ...) {
  
  sigma_2 <- var(y[,1])
  beta <- as.matrix(params)
  
  x.matrix <- cbind(rep(1, length(y[,1])), as.matrix(y[,-1L]))
  
  resid <- as.vector((y[,1] - x.matrix%*%beta))
  
  l_theta <- -log(prod(dnorm(resid, mean = 0, sd = sqrt(sigma_2))))
  
  return(list(
    log_theta = l_theta,
    residuals = resid, 
    beta = params,
    var = sqrt(sigma_2),
    hat_var = var(resid)
  ))
}

### Writing the estimated coefficients from lm()
model <- lm(y ~ x1 + x2 + x3, df)
model$coefficients
var(model$residuals)
