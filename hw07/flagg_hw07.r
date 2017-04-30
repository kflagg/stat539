# Kenny Flagg
# Stat 539 Homework 7

library(geepack)
library(lme4)


## PROBLEM 2

# Blank plot.
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xaxt = 'n', yaxt = 'n', bty = 'n',
     main = 'Voting Behavior and Wealth for Four States',
     xlab = 'Wealth', ylab = 'Probability of Voting Republican')
axis(1, at = c(0, 1), labels = c('Low', 'High'))
axis(2, at = c(0, 1), labels = c('Low', 'High'))
# Marginal trend.
segments(x0 = 0, y0 = 0.9, x1 = 1, y1 = 0.1, lwd = 3)
# Within-state trends for four states.
segments(x0 = c(0, 0.25, 0.5, 0.75), y0 = c(0.75, 0.55, 0.35, 0.15),
         x1 = c(0.25, 0.5, 0.75, 1), y1 = c(0.85, 0.65, 0.45, 0.25), lwd = 1)
legend('topright', lwd = c(1, 3), legend = c('Within-State', 'Among-States'))


## PROBLEM 3

# Part (a)

rats <- read.table('http://www.stat.ufl.edu/~aa/glm/data/Rats.dat', header = TRUE)
rats$z <- as.integer(rats$group == 1) # Placebo indicator.

# Make an ungrouped data frame so the response is Bernoulli instead of binomial.
# Do this by repeating each row index i n_i times...
rats_ungrouped <- rats[
    unlist(lapply(seq_len(nrow(rats)), function(i){
      return(rep(i, rats$n[i]))
    })),
  ]
# ...then, in each litter, repeat 1 s_i times and 0 n_i - s_i times
rats_ungrouped$s <- unlist(lapply(seq_len(nrow(rats)), function(i){
    return(c(rep(1, rats$s[i]), rep(0, rats$n[i] - rats$s[i])))
  }))

# Naive GLM.
rats_glm <- glm(s ~ z + h, data = rats_ungrouped, family = binomial)
summary(rats_glm)

# GEE with robust sandwich estimator.
rats_gee <- geeglm(s ~ z + h, data = rats_ungrouped, family = binomial,
                   id = litter, corstr = 'exchangeable', std.err = 'san.se')
summary(rats_gee)

# Part (b)

# glmer can use up to 100 quadrature points.
rats_glmm <- glmer(s ~ z + h + (1|litter), data = rats_ungrouped,
                   family = binomial, nAGQ = 100)
summary(rats_glmm)
exp(fixef(rats_glmm))


## PROBLEM 4

# Part (a)

skin <- read.table('http://www.math.montana.edu/shancock/courses/stat539/data/skin.txt', header = TRUE)

skin_glmm <- glmer(Y ~ Trt * Year + (1|ID), data = skin,
                   family = poisson, nAGQ = 100)
summary(skin_glmm)

# Part (b)

exp(fixef(skin_glmm))

# Part (c)

# Exponentiated empirical rule.
exp(qnorm(c(0.025, 0.975)) * attr(VarCorr(skin_glmm)$ID, 'stddev'))

# Part (d)

skin_glmm_reduced <- glmer(Y ~ Year + (1|ID), data = skin,
                     family = poisson, nAGQ = 100)
anova(skin_glmm_reduced, skin_glmm, test = 'LRT')

# Part (e)

# The first 10 BLUPs.
head(ranef(skin_glmm)$ID, n = 10)

# Part i
var(ranef(skin_glmm)$ID$`(Intercept)`)

# Part ii

# Get the observations where each ID first appears.
skin_first <- skin[!duplicated(skin$ID),]

# ranef(skin_glmm)$ID is a data.frame with the IDs as row names.
par(mfrow = c(1, 2), cex = 1)
plot(ranef(skin_glmm)$ID[as.character(skin_first$ID),'(Intercept)'] ~ Age,
     data = skin_first, main = 'EBLUPs versus Age\nat First Observation',
     ylab = expression(hat(u)[i]), col = '#00000040')
plot(ranef(skin_glmm)$ID[as.character(skin$ID),'(Intercept)'] ~ Exposure,
     data = skin, main = 'EBLUPs versus Number\n of Previous Cancers',
     ylab = expression(hat(u)[i]), col = '#00000040')

# Part (f)

skin_gee <- geeglm(Y ~ Trt * Year, data = skin, family = poisson,
                   id = ID, corstr = 'exchangeable')
summary(skin_gee)

# Part (h)

skin_gee_full <- geeglm(Y ~ Skin + Age + Exposure + Trt * Year, data = skin,
                        family = poisson, id = ID, corstr = 'exchangeable')
summary(skin_gee_full)

skin_gee_reduced <- geeglm(Y ~ Skin + Age + Exposure + Year, data = skin,
                           family = poisson, id = ID, corstr = 'exchangeable')
anova(skin_gee_reduced, skin_gee_full)
