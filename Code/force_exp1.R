library(readxl)
library(lme4)
library(car)
library(emmeans)
library(broom.mixed)
library(dplyr)
library(ggplot2)

data <- read_excel("/path/to/data/force_table_exp1.xlsx")


Place_FXLRForceDuration <- lmer(Place_FXLRForceDuration ~ Block*Hand + (1|Subject/Session),
                                data=data)
hist(residuals(Place_FXLRForceDuration), main = "Histogram of Residuals r free", xlab = "Residuals")
car::Anova(Place_FXLRForceDuration)
contrast(
  emmeans(Place_FXLRForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FXLRForcePeak <- lmer(Place_FXLRForcePeak ~ Block*Hand + (1|Subject/Session),
                            data=data)
car::Anova(Place_FXLRForcePeak)
contrast(
  emmeans(Place_FXLRForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FX2ForceDuration <- lmer(Place_FX2ForceDuration ~ Block*Hand + (1|Subject),
                               data=data)
car::Anova(Place_FX2ForceDuration)
contrast(
  emmeans(Place_FX2ForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FX2ForcePeak <- lmer(Place_FX2ForcePeak ~ Block*Hand + (1|Subject),
                           data=data)
car::Anova(Place_FX2ForcePeak)
contrast(
  emmeans(Place_FX2ForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FXLRtoFX2 <- lmer(Place_FXLRtoFX2 ~ Block*Hand + (1|Subject),
                        data=data)
car::Anova(Place_FXLRtoFX2)
contrast(
  emmeans(Place_FXLRtoFX2, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_FX2toFXLR <- lmer(Place_FX2toFXLR ~ Block*Hand + (1|Subject),
                        data=data)
car::Anova(Place_FX2toFXLR)
contrast(
  emmeans(Place_FX2toFXLR, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_TY2tot <- lmer(Place_TY2tot ~ Block*Hand + (1|Subject),
                     data=data)
car::Anova(Place_TY2tot)
contrast(
  emmeans(Place_TY2tot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Place_TYLRtot <- lmer(Place_TYLRtot ~ Block*Hand + (1|Subject),
                      data=data)
car::Anova(Place_TYLRtot)
contrast(
  emmeans(Place_TYLRtot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FXLRForceDuration <- lmer(Retrieve_FXLRForceDuration ~ Block*Hand + (1|Subject),
                                   data=data)
car::Anova(Retrieve_FXLRForceDuration)
contrast(
  emmeans(Retrieve_FXLRForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FXLRForcePeak <- lmer(Retrieve_FXLRForcePeak ~ Block*Hand + (1|Subject),
                               data=data)
car::Anova(Retrieve_FXLRForcePeak)
contrast(
  emmeans(Retrieve_FXLRForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FX2ForceDuration <- lmer(Retrieve_FX2ForceDuration ~ Block*Hand + (1|Subject/Session),
                                  data=data)
car::Anova(Retrieve_FX2ForceDuration)
contrast(
  emmeans(Retrieve_FX2ForceDuration, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FX2ForcePeak <- lmer(Retrieve_FX2ForcePeak ~ Block*Hand + (1|Subject/Session),
                              data=data)
car::Anova(Retrieve_FX2ForcePeak)
contrast(
  emmeans(Retrieve_FX2ForcePeak, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FXLRtoFX2 <- lmer(Retrieve_FXLRtoFX2 ~ Block*Hand + (1|Subject),
                           data=data)
car::Anova(Retrieve_FXLRtoFX2)
contrast(
  emmeans(Retrieve_FXLRtoFX2, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_FX2toFXLR <- lmer(Retrieve_FX2toFXLR ~ Block*Hand + (1|Subject),
                           data=data)
car::Anova(Retrieve_FX2toFXLR)
contrast(
  emmeans(Retrieve_FX2toFXLR, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_TY2tot <- lmer(Retrieve_TY2tot ~ Block*Hand + (1|Subject),
                        data=data)
car::Anova(Retrieve_TY2tot)
contrast(
  emmeans(Retrieve_TY2tot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)

Retrieve_TYLRtot <- lmer(Retrieve_TYLRtot ~ Block*Hand + (1|Subject/Session),
                         data=data)
car::Anova(Retrieve_TYLRtot)
contrast(
  emmeans(Retrieve_TYLRtot, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise', adjust='mvt')
)


# Table -------------------------------------------------------------------

# List of model names and corresponding model objects
model_list <- list(
  Place_FXLRForceDuration = Place_FXLRForceDuration,
  Place_FXLRForcePeak = Place_FXLRForcePeak,
  Place_FX2ForceDuration = Place_FX2ForceDuration,
  Place_FX2ForcePeak = Place_FX2ForcePeak,
  Place_FXLRtoFX2 = Place_FXLRtoFX2,
  Place_FX2toFXLR = Place_FX2toFXLR,
  Place_TY2tot = Place_TY2tot,
  Place_TYLRtot = Place_TYLRtot,
  Retrieve_FXLRForceDuration = Retrieve_FXLRForceDuration,
  Retrieve_FXLRForcePeak = Retrieve_FXLRForcePeak,
  Retrieve_FX2ForceDuration = Retrieve_FX2ForceDuration,
  Retrieve_FX2ForcePeak = Retrieve_FX2ForcePeak,
  Retrieve_FXLRtoFX2 = Retrieve_FXLRtoFX2,
  Retrieve_FX2toFXLR = Retrieve_FX2toFXLR,
  Retrieve_TY2tot = Retrieve_TY2tot,
  Retrieve_TYLRtot = Retrieve_TYLRtot
)

# Create summary table from Anova
anova_summary <- lapply(names(model_list), function(name) {
  model <- model_list[[name]]
  aov_result <- car::Anova(model)
  aov_df <- as.data.frame(aov_result)
  aov_df$Term <- rownames(aov_df)
  aov_df$Model <- name
  return(aov_df)
}) %>% bind_rows() %>%
  select(Model, Term, everything()) %>%
  arrange(Model, Term)

# Print summary
print(anova_summary)

# Clean up the data
anova_plot_data <- anova_summary %>%
  filter(!is.na(`Pr(>Chisq)`)) %>%
  mutate(Significance = case_when(
    `Pr(>Chisq)` < 0.001 ~ "***",
    `Pr(>Chisq)` < 0.01 ~ "**",
    `Pr(>Chisq)` < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Plot
ggplot(anova_plot_data, aes(x = Term, y = Model, fill = `Pr(>Chisq)`)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Significance), color = "black", size = 4) +
  scale_fill_gradient(low = "#fef0d9", high = "#d7301f", name = "p-value") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "ANOVA Results by Model",
    x = "Term",
    y = "Model"
  )

posthoc_summary <- lapply(names(model_list), function(name) {
  model <- model_list[[name]]
  emms <- emmeans(model, pairwise ~ Block*Hand)
  contr <- summary(contrast(emms, interaction = c("revpairwise", "revpairwise")))
  contr_df <- as.data.frame(contr)
  contr_df$Model <- name
  return(contr_df)
}) %>% bind_rows()

# Print pairwise comparisons
print(posthoc_summary)


posthoc_plot_data <- posthoc_summary %>%
  filter(grepl("B", Block_revpairwise)) %>%
  mutate(Significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Plot
ggplot(posthoc_plot_data, aes(x = Block_revpairwise, y = Model, fill = p.value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Significance), size = 4) +
  scale_fill_gradient(low = "#e0ecf4", high = "#8856a7", name = "p-value") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Post-Hoc Pairwise Comparisons",
    x = "Contrast",
    y = "Model"
  )
