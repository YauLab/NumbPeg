library(readxl)
library(lme4)
library(car)
library(emmeans)
library(broom.mixed)
library(dplyr)
library(ggplot2)
library(lmerTest)

data <- read_excel("/path/to/data/force_table_exp2.xlsx")

data$Hand <- relevel(factor(data$Hand), ref="Non-dominant")


Place_FXLRForceDuration <- lmerTest::lmer(Place_FXLRForceDuration ~ Block*Hand + (1|Subject/Round),
                                data=data)
car::Anova(Place_FXLRForceDuration)
contrast(
  emmeans(Place_FXLRForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FXLRForcePeak <- lmerTest::lmer(Place_FXLRForcePeak ~ Block*Hand + (1|Subject/Round),
                            data=data)
car::Anova(Place_FXLRForcePeak)
contrast(
  emmeans(Place_FXLRForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FX2ForceDuration <- lmerTest::lmer(Place_FX2ForceDuration ~ Block*Hand + (1|Subject),
                               data=data)
car::Anova(Place_FX2ForceDuration)
contrast(
  emmeans(Place_FX2ForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FX2ForcePeak <- lmerTest::lmer(Place_FX2ForcePeak ~ Block*Hand + (1|Subject),
                           data=data)
car::Anova(Place_FX2ForcePeak)
contrast(
  emmeans(Place_FX2ForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FXLRtoFX2 <- lmerTest::lmer(Place_FXLRtoFX2 ~ Block*Hand + (1|Subject),
                        data=data)
car::Anova(Place_FXLRtoFX2)
contrast(
  emmeans(Place_FXLRtoFX2, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FX2toFXLR <- lmerTest::lmer(Place_FX2toFXLR ~ Block*Hand + (1|Subject/Round),
                        data=data)
car::Anova(Place_FX2toFXLR)
contrast(
  emmeans(Place_FX2toFXLR, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_TY2tot <- lmerTest::lmer(Place_TY2tot ~ Block*Hand + (1|Subject),
                     data=data)
car::Anova(Place_TY2tot)
contrast(
  emmeans(Place_TY2tot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_TYLRtot <- lmerTest::lmer(Place_TYLRtot ~ Block*Hand + (1|Subject),
                      data=data)
car::Anova(Place_TYLRtot)
contrast(
  emmeans(Place_TYLRtot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FXLRForceDuration <- lmerTest::lmer(Retrieve_FXLRForceDuration ~ Block*Hand + (1|Subject),
                                   data=data)
car::Anova(Retrieve_FXLRForceDuration)
contrast(
  emmeans(Retrieve_FXLRForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FXLRForcePeak <- lmerTest::lmer(Retrieve_FXLRForcePeak ~ Block*Hand + (1|Subject),
                               data=data)
car::Anova(Retrieve_FXLRForcePeak)
contrast(
  emmeans(Retrieve_FXLRForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FX2ForceDuration <- lmerTest::lmer(Retrieve_FX2ForceDuration ~ Block*Hand + (1|Subject/Round),
                                  data=data)
car::Anova(Retrieve_FX2ForceDuration)
contrast(
  emmeans(Retrieve_FX2ForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FX2ForcePeak <- lmerTest::lmer(Retrieve_FX2ForcePeak ~ Block*Hand + (1|Subject/Round),
                              data=data)
car::Anova(Retrieve_FX2ForcePeak)
contrast(
  emmeans(Retrieve_FX2ForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FXLRtoFX2 <- lmerTest::lmer(Retrieve_FXLRtoFX2 ~ Block*Hand + (1|Subject),
                           data=data)
car::Anova(Retrieve_FXLRtoFX2)
contrast(
  emmeans(Retrieve_FXLRtoFX2, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FX2toFXLR <- lmerTest::lmer(Retrieve_FX2toFXLR ~ Block*Hand + (1|Subject),
                           data=data)
car::Anova(Retrieve_FX2toFXLR)
contrast(
  emmeans(Retrieve_FX2toFXLR, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_TY2tot <- lmerTest::lmer(Retrieve_TY2tot ~ Block*Hand + (1|Subject),
                        data=data)
car::Anova(Retrieve_TY2tot)
contrast(
  emmeans(Retrieve_TY2tot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_TYLRtot <- lmerTest::lmer(Retrieve_TYLRtot ~ Block*Hand + (1|Subject/Round),
                         data=data)
car::Anova(Retrieve_TYLRtot)
contrast(
  emmeans(Retrieve_TYLRtot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

# Table -------------------------------------------------------------------
model_list <- list(
  Coarse_collect_ForceDuration = Place_FXLRForceDuration,
  Coarse_collect_ForcePeak = Place_FXLRForcePeak,
  Precise_deliver_ForceDuration = Place_FX2ForceDuration,
  Precise_deliver_ForcePeak = Place_FX2ForcePeak,
  Place_transport = Place_FXLRtoFX2,
  Place_pegFree = Place_FX2toFXLR,
  Precise_deliver_totTorque = Place_TY2tot,
  Coarse_collect_totTorque = Place_TYLRtot,
  Coarse_deliver_ForceDuration = Retrieve_FXLRForceDuration,
  Coarse_deliver_ForcePeak = Retrieve_FXLRForcePeak,
  Precise_collect_ForceDuration = Retrieve_FX2ForceDuration,
  Precise_collect_ForcePeak = Retrieve_FX2ForcePeak,
  Retrieve_pegFree = Retrieve_FXLRtoFX2,
  Retrieve_transport = Retrieve_FX2toFXLR,
  Precise_collect_totTorque = Retrieve_TY2tot,
  Coarse_deliver_totTorque = Retrieve_TYLRtot
)

for (i in seq_along(model_list)) {
  model <- model_list[[i]]
  model_name <- names(model_list)[i]
  model_summary <- summary(model)
  coefs <- as.data.frame(model_summary$coefficients)
  
  # Round for readability
  coefs_rounded <- round(coefs,3)
  
  # Add row names as a column
  coefs_rounded$Term <- rownames(coefs)
  coefs_rounded <- coefs_rounded[, c("Term", colnames(coefs)[1:5])] # Reorder columns
  
  # Make the table figure
  table_plot <- ggtexttable(coefs_rounded, rows = NULL, theme = ttheme("light"))
  
  # Add a title to each plot
  table_plot <- annotate_figure(table_plot,
                                top = text_grob(paste(model_name, "Summary"),
                                                face = "bold", size = 14))
  
  # Plot it
  print(table_plot)
}
