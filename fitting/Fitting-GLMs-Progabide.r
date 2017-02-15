###
##### Lecture 6: Algorithms for Fitting GLMs
###

### Example: Epileptic seizure data (described in Homework 1)
dat <- read.table('http://www.math.montana.edu/shancock/courses/stat539/data/epilepsy.txt', header = TRUE)
# Check that data set was read in correctly:
head(dat)
tail(dat)
summary(dat)  ## n = 59

### Correlated data --> Use sum of seizure counts as response
### - Model as Poisson counts
dat$y <- with(dat, Week2 + Week4 + Week6 + Week8)

### Create indicator variable for Progabide
dat$x <- as.numeric(dat$trt == 'Progabide')


### Fisher scoring algorithm via IWLS:

# Use y as starting values for fitted values (plus small positive value since zero counts):

# Function to calculate quantities related to log-link (canonical link for Poisson)
loglink <- function(a){
	n <- length(a)
	y1 <- rep(0,n)  # To calculate g(a)
	y2 <- rep(0,n)  # To calculate g^(-1)(a)
	y3 <- rep(0,n)  # To calculate g'(a)
	for(i in 1:n){
		if(a[i] == 0){
			y1[i] <- -10
			y2[i] <- 1
			y3[i] <- 1000
		} else {
			y1[i] <- log(a[i])
			y2[i] <- exp(a[i])
			y3[i] <- 1/(a[i])
		}
	}
	return(list(g = y1, ginv = y2, gder = y3))
}

# Function to run IWLS algorithm
	# X is the design matrix
	# y is the vector of responses
	# link is the function used to calculate quantities related to link function
iwls <- function(X, y, link, maxit=20, tol=.001){
	n <- length(y)
	## Find initial estimates
		z0 <- link(y)$g
		w0 <- diag(rep(1,n))
		beta0 <- solve(t(X)%*%w0%*%X, t(X)%*%w0%*%z0)
	## Update
		crit <- 1
		it <- 0
		beta <- beta0
		while(crit > tol && it <= maxit){
			it <- it+1
			betaprev <- beta
			nu <- X%*%betaprev
			mu <- link(nu)$ginv
			gmu <- link(mu)$gder
			z <- nu + gmu*(y - mu)
			w <- diag(1/(mu*gmu^2))
			beta <- solve(t(X)%*%w%*%X, t(X)%*%w%*%z)
			print(paste("Iteration = ", it))
			print(beta)
			crit <- sum((beta-betaprev)^2/sum(beta^2))
		}
		# Calculate information matrix
		info <- t(X)%*%w%*%X
		finalbeta <- beta
		nu <- X%*%finalbeta
		mu <- link(nu)$ginv
		return(list(finalbeta = finalbeta, info = info, fits = mu))
}	


y <- matrix(dat$y)
X <- matrix(c(rep(1,59),dat$x), nrow=59, ncol=2, byrow=FALSE)
iwls(X, y, loglink)

### Use glm function
mod <- glm(y ~ x, family=poisson(link = "log"), data=dat)

### Generic call: glm(formula, family, data)
# formula: specifies mean model, e.g., y ~ x1 + x2 + x1:x2
# family: specifies probability model and link, e.g.
	# binomial(link = "logit") 
	# gaussian(link = "identity")
	# Gamma(link = "inverse")
	# poisson(link = "log")
	# Each of the above links are the default if you only specify name of distn.
# data: specifies data set containing response and predictor variables


## LRT of beta1 = 0
mod0 <- update(mod, .~.-x)
anova(mod0, mod, test="LRT")

## Interpret coefficients and write conclusions.

# Help function
# ?anova.glm
