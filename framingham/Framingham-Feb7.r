## Framingham Heart Study Description:
## http://www.ics.uci.edu/~staceyah/111-202/data/framingham.html

# sex      : Sex (1 = male; 2 = female)
# sbp      : Systolic Blood Pressure
# dbp      : Diastolic Blood Pressure
# scl      : Serum Cholesterol
# chdfate  : Coronary Heart Disease
# followup : Follow-up in Days
# age      : Age in Years
# bmi      : Body Mass Index (wt (kg) / h^2 (m)
# month    : Study Month of Baseline Exam
# id       : Subject ID

## Read in data:
fram <- read.table('http://www.math.montana.edu/shancock/courses/stat539/data/Framingham.txt', header = TRUE)

dim(fram)
head(fram)
summary(fram)

is.numeric(fram$sex)  # R is treating sex as quantitative
# Either change class to factor:
fram$sex <- factor(fram$sex, levels=c(1,2), labels=c("M","F")) 
# or create indicator variable:
fram$female <- as.numeric(fram$sex == "F")


## Analysis goals:
# (1) Does systolic blood pressure have an effect on the probability of CHD?
# (2) If so, does this effect differ for men and women?

## Steps:
# 1. Think about model selection prior to looking at the data. Set up hypotheses.
# 2. Visualize and summarize data.
# 3. Conduct appropriate inference.
# 4. Check model assumptions. If violated, return to 3.
# 5. Further exploratory analysis if appropriate.
# 6. Conclusions.

# TODO: Visualize chdfate vs sbp by binning sbp.

## R functions for glms:
#?glm
#?anova.glm  # the anova R function applied to a glm object
#?summary.glm  # the summary R function applied to a glm object
#?residuals.glm
#?predict.glm

fram.glm <- glm(chdfate ~ sbp * female + age, data = fram,
                family = binomial)
summary(fram.glm)

