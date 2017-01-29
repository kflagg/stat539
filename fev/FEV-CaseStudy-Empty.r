##
#####	Read in functions defined especially for Stat 211
##
source('http://www.math.montana.edu/shancock/courses/stat539/r/Stat211Functions.R')
##
#####	FEV example
##
#####	Preliminary data description and management
##
fev <- read.table('http://www.math.montana.edu/shancock/courses/stat539/data/fev.raw', header = TRUE)
summary(fev)
fev$male <- as.numeric(fev$sex) - 1
table(fev$sex, fev$male)
fev <- fev[,!is.element(names(fev), 'sex')]  ## Remove sex variable
summary(fev)

with(fev, (table(smoke, age))

###
### Case Study: Data Analysis - do in class
###

## Exploratory plots
boxplot(fev ~ smoke, data = fev, ylab = 'FEV (L/s)')
plot(fev ~ jitter(age), col = ifelse(smoke == 'smoker', '#ff000080', '#00ffff40'), pch = 16, data = fev)
fev$`age group` <- cut(fev$age, breaks = c(0, 9, 14, 19))
boxplot(fev ~ `age group` + smoke, col = c('cyan', 'red'), data = fev)

## Analysis
fit.unadjusted <- lm(fev ~ smoke, subset = age>=9, data = fev)
summary(fit.unadjusted)
# We estimate that the mean FEV for smokers is 0.304 L/s (SE = 0.102) higher
# than the mean FEV for nonsmokers, for children aged 9 to 19.

fit.ageadjusted <- lm(fev ~ smoke + age, subset = age>=9, data = fev)
summary(fit.ageadjusted)
# We estimate that the mean FEV for smokers of a given age is 0.176 L/s
# (SE = 0.093) lower than the mean FEV for nonsmokers of the same age, for
# children aged 9 to 19.

fit.ageheight <- lm(fev ~ smoke + age + height, subset = age>=9, data = fev)
summary(fit.ageheight)
# Note SE(smoke) and MSE decreased. smoke coef estimate didn't change much.

fit.ageheightsex <- lm(fev ~ smoke + age + height + male, subset = age>=9, data = fev)
summary(fit.ageheightsex)
# Sex doesn't add much info about smoking.

fit.agemodifier <- lm(fev ~ smoke * age, subset = age>=9, data = fev)
summary(fit.agemodifier)

## Check assumptions

par(mfrow = c(2, 2))
plot(fit.ageheight, add.smooth = FALSE)
# Increasing variance

# Robust sandwich!
robust.se.lm(fit.ageheight)

