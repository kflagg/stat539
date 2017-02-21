# Kenny's script for HW 4 problem 4.

# Enter the Hadleyverse.
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# Read in Gillen's functions.
source('GillenRFunctions.R')

# Load the data and remove the unusual surgery time.
SoreThroat <- read.table('SoreThroat.dat', header = TRUE, skip = 5) %>%
  filter(D < 135)

# Set a random seed so jittering is the same every time.
set.seed(876235)

# Make a snazzy ggplot with loess smoothers and slightly jittered points.
p <- ggplot(SoreThroat %>%
              mutate(
                Device = ifelse(T == 1, 'Tracheal Tube', 'Mask'),
                `Duration (Minutes)` = D,
                `Sore Throat` = Y
              ),
            aes(x = `Duration (Minutes)`, y = `Sore Throat`, col = Device)) +
     geom_smooth(method = 'loess', se = FALSE) +
     geom_jitter(height = 0.01, width = 0.5, alpha = 0.5) +
     scale_color_manual(values = c('black', 'grey')) +
     ylim(-0.01, 1.01) +
     ggtitle('Sore Throat Outcomes by Duration and Device')
print(p)

# Fit the no-interaction model assuming log-odds are linear in duration.
fit_initial <- glm(Y ~ D + T, data = SoreThroat, family = binomial)
print(summary(fit_initial))

# Hosmer-Lemeshow GoF test. Groups 2, 2, 4 have > 4.6 expected sore throats.
hl_gof <- binary.gof(fit_initial, 4)
print(hl_gof)

