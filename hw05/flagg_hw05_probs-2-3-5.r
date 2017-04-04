# Kenny's script for HW 5 problems 2, 3, and 5.

library(nnet)
source('GillenRFunctions.R')

# The original summ.mfit doesn't seem to work with the multinom objects that
# I get when using a multiple-column response. The outcome labels are in a
# variable called lab, not lev.
summ.mfit2 <- function(model){
  s <- summary(model)
  for(i in 1:nrow(s$coefficients)){ # Go down rows, not columns.
    cat('\nLevel ', model$lab[i+1],  ' vs. Level ', model$lab[1], '\n' )
    coef <- s$coefficients[i,]
    rrr <- exp(coef)
    se <- s$standard.errors[i,]
    zStat <- coef / se
    pVal <- 2*pnorm(abs(zStat), lower.tail = FALSE)
    ci95.lo <- exp(coef + qnorm(0.025)*se)
    ci95.hi <- exp(coef + qnorm(0.975)*se)
    rslt <- cbind(rrr, se, zStat, pVal, ci95.lo, ci95.hi)
    print(round(rslt, 3))
  }
}


# Enter the Hadleyverse.
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())


###############
## PROBLEM 2 ##
###############

# Load the data for problem 2.
# My makefile wgets the file from
#   http://www.math.montana.edu/shancock/courses/stat539/data/Alligator.csv
# As an aside, I keep data files locally because I've had too many experiences
# where I used Mark's code to download something from his Box and it turned
# out Box changed the URL. Also, my home internet connection cuts out
#  frequently. So now I download things once and keep them.
gator2 <- read.csv('Alligator.csv')

# Make a new data frame with nice names for plot output.
# There needs to be a version of switch that works on vectors...
# I'm specifying all the levels so they stay in the order of the numerical
# coding instead of getting alphabetized.
gator2_out <- gator2 %>%
  mutate(Lake = factor(sapply(lake, switch,
                              `1` = 'Hancock', `2` = 'Oklawaha',
                              `3` = 'Trafford', `4` = 'George', NA),
                       levels = c('Hancock', 'Oklawaha', 'Trafford', 'George')),
         Food = factor(sapply(as.character(food),
                              FUN = switch, # Stupid partial matching
                             `F` = 'Fish', `I` = 'Invertebrate',
                             `R` = 'Reptile', `B` = 'Bird', `O` = 'Other', NA),
                       levels = c('Fish', 'Invertebrate', 'Reptile',
                                        'Bird', 'Other')),
         Gender = factor(sapply(gender, switch, `1` = 'Male', `2` = 'Female'),
                         levels = c('Male', 'Female')),
         Size = factor(sapply(size, switch, `1` = 'Small', `2` = 'Large'),
                       levels = c('Small', 'Large')))

# 2(a)
plot2a1 <- ggplot(gator2_out, aes(fill = Food, x = Lake, weight = count)) +
             geom_bar(position = 'fill') +
             ylab('Proportion') +
             ggtitle('Primary Food Type by Lake') +
             scale_fill_grey()
plot2a1
plot2a2 <- ggplot(gator2_out %>% mutate(
                    `Size and Gender` = interaction(Size, Gender, sep = ' '
                  )),
                  aes(fill = Food, x = `Size and Gender`, weight = count)) +
             geom_bar(position = 'fill') +
             ylab('Proportion') +
             ggtitle('Primary Food Type by Size and Gender') +
             scale_fill_grey()
plot2a2

# 2(b)
# Spread the counts into wide format and set up factors and indicators.
gator2_wide <- gator2 %>%
  spread(food, count) %>%
  mutate(
    s = sapply(size, switch, `1` = 1, `2` = 0, NA),
    L = factor(lake, levels = c(4, 1:3), labels = c('G', 'H', 'O', 'T'))
  )
gator2_fit <- multinom(cbind(F, B, I, O, R) ~ s + L, data = gator2_wide)
summary(gator2_fit)
summ.mfit2(gator2_fit)

# 2(c)
LinContr.mfit(contr.names = c('I:s', 'R:s'), contr.coef = c(1, -1), gator2_fit)

# 2(d)
smallO <- c(1, 1, 0, 1, 0)
eta_hat <- coef(gator2_fit) %*% smallO
pI_smallO <- exp(eta_hat['I',]) / (1 + sum(exp(eta_hat)))
pI_smallO

# 2(e)
gator2_IF <- glm(cbind(I, F) ~ s + L, family = binomial, data = gator2_wide)
summary(gator2_IF) # The estimates and SEs agree.


###############
## PROBLEM 3 ##
###############

# Load the data for problem 3.
# I pull this from http://www.stat.ufl.edu/~aa/glm/data/Alligators3.dat
gator3 <- read.table('Alligators3.dat', header = TRUE)

# 3(a)
# Fish is baseline because F is alphebetically first.
gator3_fit <- multinom(food ~ length, data = gator3)
summ.mfit2(gator3_fit)

# 3(b)
# Here's a function to compute the fitted probability for length x
# and food type t.
gator3_pi_hat <- function(x, t){
  coefs <- coef(gator3_fit)
  return(sapply(x, function(x){
    return((if(t == 'F') 1 else exp(coefs[t,] %*% c(1, x))) /
             (1 + sum(exp(coefs %*% c(1, x)))))
  }))
}
# Make a ggplot with a rug and a curve for each food type.
# Only plot the curves in the range of the observed lengths.
plot3b <- ggplot(gator3, aes(x = length, lty = food)) +
  geom_rug() +
  stat_function(fun = gator3_pi_hat, args = list(t = 'F'),
                n = 501, lty = 1, xlim = range(gator3$length)) +
  stat_function(fun = gator3_pi_hat, args = list(t = 'I'),
                n = 501, lty = 2, xlim = range(gator3$length)) +
  stat_function(fun = gator3_pi_hat, args = list(t = 'O'),
                n = 501, lty = 3, xlim = range(gator3$length)) +
  xlim(1, 4) +
  ylim(0, 1) +
  xlab('Alligator Length (meters)') +
  ylab('Estimated Probability') +
  ggtitle('Estimated Probabilities of Each Primary Food Type')
plot3b


###############
## PROBLEM 5 ##
###############

library(VGAM)
race <- c(1, 1, 0, 0)
gender <- c(1, 0, 1, 0)
y1 <- c(88, 54, 397, 235)
y2 <- c(16, 7, 141, 189)
y3 <- c(2, 5, 24, 39)

# 5(a)
# Set parallel = FALSE to get the general model with coefficients that differ
# by response category.
prob5_fit <- vglm(cbind(y1, y2, y3) ~ gender + race,
                  family = multinomial(parallel = FALSE))
summary(prob5_fit)

# 5(b)
# Yes is 1 and no is 3 so we're interested in the gender:1 row.
exp(confint(prob5_fit))

# 5(c)
# Null model only includes race.
prob5_nogender <- vglm(cbind(y1, y2, y3) ~ race,
                       family = multinomial(parallel = FALSE))
# There's no ANOVA method for vglm objects, but the summary output includes
# the deviance and the loglihood (as Mark calls it).
summary(prob5_nogender)
pchisq(-2 * (prob5_nogender@criterion$loglikelihood -
               prob5_fit@criterion$loglikelihood),
       prob5_nogender@df.residual - prob5_fit@df.residual,
       lower.tail = FALSE)
