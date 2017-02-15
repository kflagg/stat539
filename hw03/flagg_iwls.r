# Kenny's IWLS function for a binomial GLM with n = 1 and logit link.
# Not very general, but starting from scratch is fun!
# Arguments:
#   y      response
#   X      model matrix
#   beta   vector of initial beta values
#   maxit  maximum number of iterations
#   tol    tolerance level for individual coefficients
iwlsLogit <- function(y, X, beta = rep(0, ncol(X)),
                      maxit = 100, tol = 0.00001){
  # Some error checking.
  if(length(y) != nrow(X)){
    stop('X and y have different lengths.')
  }
  if(length(beta) != ncol(X)){
    stop(sprintf('Initial beta must have length %d.', ncol(X)))
  }

  # Initialize iteration counter.
  i <- 0

  # Loop until convergence or maximum number of iterations.
  repeat{
    # Update mean estimate.
    mu <- exp(X %*% beta) / (1 + exp(X %*% beta))

    # Inverse of matrix of derivatives of inverse logit as a function of mu.
    D_inv <- diag(1 / as.vector(mu * (1 - mu)))

    # The weight matrix is W = D V^-1 D. Note that var(y_i) = mu_i (1 - mu_i)
    # so V^-1 = D^-1 and thus W = D.
    W <- diag(as.vector(mu * (1 - mu)))

    # Working response.
    z <- X %*% beta + D_inv %*% (y - mu)

    # Update beta.
    beta_old <- beta
    beta <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z

    # Check for convergence and break the loop if we're done.
    i <- i + 1
    if(i > maxit){
      warning('Maximum number of iterations exceeded.')
      break
    }
    if(max(abs(beta - beta_old)) < tol){
      break
    }
  }

  # Return coefficient estimates and number of iterations.
  colnames(beta) <- 'Estimate'
  return(structure(beta, iterations = i))
}

# Read data and create a female indicator.
fram <- read.table('Framingham.txt')
fram$female <- as.numeric(fram$sex == 2)

# Compute the estimates.
beta_hat <- iwlsLogit(fram$chdfate, model.matrix(~ female + age, data = fram))

