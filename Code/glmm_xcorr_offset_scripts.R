require(lme4)
require(readxl)
require(car)
require(emmeans)


#### exp 1 ####
data1 <- read_excel('/path/to/data/lmm_table_xcorr_offset_exp1.xlsx')


data1$Drug <- relevel(factor(data1$Drug, levels = c('None','Sham','Anesthesia')), ref = "None")

glmm1 <- lmer(Offset_lags ~ Block*Hand*Phase + (1|Subject),
              data = data1)
car::Anova(glmm1)
hist(residuals(glmm1), main = "Histogram of Residuals pc", xlab = "Residuals", breaks = 20)
contrast(emmeans(glmm1, pairwise ~ Block*Hand*Phase), interaction=c('revpairwise','revpairwise'), by='Phase')

#### exp 2 ####
data2 <- read_excel('/Users/kevin/Desktop/Kevin/Per data/New Data/lmm_table_xcorr_offset_exp2.xlsx')
# data2 <- read_excel('/path/to/data/lmm_table_xcorr_offset_exp2.xlsx')

data2$Drug <- relevel(factor(data2$Drug, levels = c('None','Sham','Anesthesia')), ref = "None")

glmm2 <- lmer(Offset_lags ~ Block*Hand*Phase + (1|Subject),
              data = data2)
car::Anova(glmm2)
hist(residuals(glmm2), main = "Histogram of Residuals pc", xlab = "Residuals", breaks = 20)
contrast(emmeans(glmm2, pairwise ~ Block*Hand*Phase), interaction=c('revpairwise','pairwise'), by='Phase')


#### plots ####
# apply function over list (list in this case being subjects from dataset gisi_during)
# function shows summary statistics: mean of mmDifference from Drug*Session interaction
lapply(split(data, data$Subject), function(gg) {
  aggregate(
    Offset_lags ~ Drug, mean, data=gg,
    # subset=Drug=='Anesthesia'
  )
})

# see if there is a trend across sessions within each treatment condition
# emtrends(
#   glmm, pairwise ~ Drug, var='Session'
# )

ggplot(data, aes( x = Offset_lags, color = Drug)) + 
  geom_density() + 
  xlim(-500,500) +
  theme(legend.position='top') +
  labs(x = 'observed offset lag')

ggplot(data1, aes( x = Offset_lags, color = Hand)) + 
  geom_density() + 
  xlim(-500,500) +
  theme(legend.position='top') +
  labs(x = 'observed offset lag')

ggplot(data, aes( x = Offset_lags, color = Block)) + 
  geom_density() + 
  xlim(-500,500) +
  theme(legend.position='top') +
  labs(x = 'observed offset lag')

ggplot(data, aes( x = Offset_lags, color = Phase)) + 
  geom_density() + 
  xlim(-500,500) +
  theme(legend.position='top') +
  labs(x = 'observed offset lag')

ggplot(data, aes( x = Offset_lags, color = interaction(Hand,Block))) + 
  geom_density() + 
  xlim(-500,500) +
  theme(legend.position='top') +
  labs(x = 'observed offset lag')

ggplot(data, aes( x = Offset_lags, color = interaction(Hand,Drug))) + 
  geom_density() + 
  xlim(-500,500) +
  theme(legend.position='top') +
  labs(x = 'observed offset lag')

summary_table <- tidy(glmm)

summary_table %>%
  kable %>%
  kable_styling('striped', full_width=F)