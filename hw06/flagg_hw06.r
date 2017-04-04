# Kenny Flagg
# Stat 539 Homework 6

library(xtable)
library(tidyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# I'm going to be daring and try downloading things on the fly this time.
# Charter, please don't fail me!!
source('http://www.math.montana.edu/shancock/courses/stat539/r/GillenRFunctions.R')


###############
## PROBLEM 1 ##
###############


## Part (a)

prob1 <- data.frame(
  Defendant = factor(c('W', 'W', 'W', 'W', 'B', 'B', 'B', 'B'), levels = c('B', 'W')),
  Victim = factor(c('W', 'W', 'B', 'B', 'W', 'W', 'B', 'B'), levels = c('B', 'W')),
  Penalty = factor(c('Y', 'N', 'Y', 'N', 'Y', 'N', 'Y', 'N'), levels = c('N', 'Y')),
  Freq = c(19, 132, 0, 9, 11, 52, 6, 97)
)

print(prob1)


## Part (b)

print(xtabs(Freq ~ Defendant + Penalty + Victim, data = prob1))


## Part (c)

print(xtabs(Freq ~ Defendant + Penalty, data = prob1))

## Part (d)

# Use tidyr and dplyr to put the data in wide format.
prob1_wide <- prob1 %>% spread(Penalty, Freq)

prob1_marginal <- glm(cbind(Y, N) ~ Defendant, family = binomial,
                      data = prob1_wide)
print(xtable(summary(prob1_marginal)$coefficients, digits = 4))

## Part (e)

prob1_conditional <- glm(cbind(Y, N) ~ Defendant + Victim, family = binomial,
                         data = prob1_wide)
print(xtable(summary(prob1_conditional)$coefficients, digits = 4))

## Parts (f) and (g)

`prob1_(D,V,P)` <- glm(Freq ~ Defendant + Victim + Penalty,
                       family = poisson, data = prob1)
`prob1_(DV,VP)` <- glm(Freq ~ Defendant * Victim + Victim * Penalty,
                       family = poisson, data = prob1)
`prob1_(DV,VP,DP)` <- glm(Freq ~ (Defendant + Victim + Penalty)^2,
                       family = poisson, data = prob1)
`prob1_(DVP)` <- glm(Freq ~ Defendant * Victim * Penalty,
                     family = poisson, data = prob1)

summary(`prob1_(D,V,P)`)
summary(`prob1_(DV,VP)`)
summary(`prob1_(DV,VP,DP)`)
summary(`prob1_(DVP)`)

## Part (h)

# It really bugs me how there's no easy way to have xtable pass p-values
# through format.pval.
print(xtable(summary(`prob1_(DV,VP)`)$coefficients, digits = 4))

sqrt(c(0, 0, 0, 0, 1, 1) %*% vcov(`prob1_(DV,VP)`) %*% c(0, 0, 0, 0, 1, 1))


###############
## PROBLEM 3 ##
###############

## Part (c)

prob3 <- read.table('http://www.math.montana.edu/shancock/courses/stat539/data/stdgrp.txt',
                    header = TRUE)

prob3_rate <- glm(n.reinfect ~ white + edugrp + factor(inftype) + condom,
                  offset = log(yrsfu), family = poisson, data = prob3)
print(xtable(summary(prob3_rate)$coefficients, digits = 4))

## Part (d)

ggplot(prob3, aes(x = edugrp)) +
  geom_jitter(aes(y = n.reinfect / yrsfu),
              height = 0, width = 0.1, shape = 16, col = '#00000040') +
  geom_jitter(aes(y = fitted(prob3_rate)),
              height = 0, width = 0.1, shape = 4, col = '#ff000080') +
  scale_y_sqrt() + # Square root scale to remove some of the skew.
  ggtitle('Observed and Fitted Reinfection Counts vs Age Group')

prob3_quasi <- glm(n.reinfect ~ white + edugrp + factor(inftype) + condom,
                   offset = log(yrsfu), family = quasipoisson, data = prob3)
print(xtable(summary(prob3_quasi)$coefficients, digits = 4,
      caption = sprintf('Dispersion parameter = %.2f',
                        summary(prob3_quasi)$dispersion)))

## Part (f)

prob3_reduced <- glm(n.reinfect ~ white + edugrp + factor(inftype),
                     offset = log(yrsfu), family = quasipoisson, data = prob3)
print(xtable(anova(prob3_reduced, prob3_quasi, test = 'LRT'),
             digits = c(0, 0, 2, 0, 4, 4)), include.rownames = FALSE)
