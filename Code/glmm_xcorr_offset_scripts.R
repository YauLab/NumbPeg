library(lme4)
library(readxl)
library(car)
library(emmeans)


#### exp 1 ####
data1 <- read_excel('/path/to/data/lmm_table_xcorr_offset_exp1.xlsx')

glmm1 <- lmer(Offset_lags ~ Block*Hand*Phase + (1|Subject),
              data = data1)
car::Anova(glmm1)
hist(residuals(glmm1), main = "Histogram of Residuals pc", xlab = "Residuals", breaks = 20)
contrast(emmeans(glmm1, pairwise ~ Block*Hand*Phase), interaction=c('revpairwise','revpairwise'), by='Phase', adjust='mvt')

#### exp 2 ####
data2 <- read_excel('/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare/lmm_table_xcorr_offset_exp2.xlsx')
# data2 <- read_excel('/path/to/data/lmm_table_xcorr_offset_exp2.xlsx')


glmm2 <- lmer(Offset_lags ~ Block*Hand*Phase + (1|Subject),
              data = data2)
car::Anova(glmm2)
hist(residuals(glmm2), main = "Histogram of Residuals pc", xlab = "Residuals", breaks = 20)
contrast(emmeans(glmm2, pairwise ~ Block*Hand*Phase), interaction=c('revpairwise','pairwise'), by='Phase', adjust='mvt')

