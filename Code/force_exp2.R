require(readxl)
require(emmeans)

data <- read_excel("/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare/force_table_exp2.xlsx")
# data <- read_excel("/path/to/data/force_table_exp2.xlsx")



Place_FXLRForceDuration <- lmer(Place_FXLRForceDuration ~ Block*Hand + (1|Subject/Session),
                                data=data)
car::Anova(Place_FXLRForceDuration)
contrast(
  emmeans(Place_FXLRForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Place_FXLRForcePeak <- lmer(Place_FXLRForcePeak ~ Block*Hand + (1|Subject/Session),
                            data=data)
car::Anova(Place_FXLRForcePeak)
contrast(
  emmeans(Place_FXLRForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Place_FX2ForceDuration <- lmer(Place_FX2ForceDuration ~ Block*Hand + (1|Subject),
                               data=data)
car::Anova(Place_FX2ForceDuration)
contrast(
  emmeans(Place_FX2ForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Place_FX2ForcePeak <- lmer(Place_FX2ForcePeak ~ Block*Hand + (1|Subject),
                           data=data)
car::Anova(Place_FX2ForcePeak)
contrast(
  emmeans(Place_FX2ForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Place_FXLRtoFX2 <- lmer(Place_FXLRtoFX2 ~ Block*Hand + (1|Subject),
                        data=data)
car::Anova(Place_FXLRtoFX2)
contrast(
  emmeans(Place_FXLRtoFX2, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Place_FX2toFXLR <- lmer(Place_FX2toFXLR ~ Block*Hand + (1|Subject/Session),
                        data=data)
car::Anova(Place_FX2toFXLR)
contrast(
  emmeans(Place_FX2toFXLR, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Place_TY2tot <- lmer(Place_TY2tot ~ Block*Hand + (1|Subject),
                     data=data)
car::Anova(Place_TY2tot)
contrast(
  emmeans(Place_TY2tot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Place_TYLRtot <- lmer(Place_TYLRtot ~ Block*Hand + (1|Subject),
                      data=data)
car::Anova(Place_TYLRtot)
contrast(
  emmeans(Place_TYLRtot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Retrieve_FXLRForceDuration <- lmer(Retrieve_FXLRForceDuration ~ Block*Hand + (1|Subject),
                                   data=data)
car::Anova(Retrieve_FXLRForceDuration)
contrast(
  emmeans(Retrieve_FXLRForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Retrieve_FXLRForcePeak <- lmer(Retrieve_FXLRForcePeak ~ Block*Hand + (1|Subject),
                               data=data)
car::Anova(Retrieve_FXLRForcePeak)
contrast(
  emmeans(Retrieve_FXLRForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Retrieve_FX2ForceDuration <- lmer(Retrieve_FX2ForceDuration ~ Block*Hand + (1|Subject/Session),
                                  data=data)
car::Anova(Retrieve_FX2ForceDuration)
contrast(
  emmeans(Retrieve_FX2ForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Retrieve_FX2ForcePeak <- lmer(Retrieve_FX2ForcePeak ~ Block*Hand + (1|Subject/Session),
                              data=data)
car::Anova(Retrieve_FX2ForcePeak)
contrast(
  emmeans(Retrieve_FX2ForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Retrieve_FXLRtoFX2 <- lmer(Retrieve_FXLRtoFX2 ~ Block*Hand + (1|Subject),
                           data=data)
car::Anova(Retrieve_FXLRtoFX2)
contrast(
  emmeans(Retrieve_FXLRtoFX2, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Retrieve_FX2toFXLR <- lmer(Retrieve_FX2toFXLR ~ Block*Hand + (1|Subject),
                           data=data)
car::Anova(Retrieve_FX2toFXLR)
contrast(
  emmeans(Retrieve_FX2toFXLR, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Retrieve_TY2tot <- lmer(Retrieve_TY2tot ~ Block*Hand + (1|Subject),
                        data=data)
car::Anova(Retrieve_TY2tot)
contrast(
  emmeans(Retrieve_TY2tot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)

Retrieve_TYLRtot <- lmer(Retrieve_TYLRtot ~ Block*Hand + (1|Subject/Session),
                         data=data)
car::Anova(Retrieve_TYLRtot)
contrast(
  emmeans(Retrieve_TYLRtot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
)
