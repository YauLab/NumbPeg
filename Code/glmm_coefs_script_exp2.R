require(lme4)
# require(ggplot2)
# require(broom.mixed) # for tidy
# require(kableExtra)
require(emmeans)
require(readxl)

# choose the dataset
data <- read_excel('/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare/lmm_coefs_exp_decay_exp2.xlsx')
# data <- read_excel('/path/to/data/lmm_coefs_exp_decay_exp2.xlsx')


# generate the model and model summary 
# to do the log link function, data can't be < 0
data_omit_nan <- data[which(data$b_1 > 0),]
data_omit_nan <- data_omit_nan[which(data_omit_nan$Subject != 1),] # omiting subject 1 due to noisy data


# data meeting the specific conditions ----
data_place_free <- subset(x=data_omit_nan, 
                             subset=Phase=='Place' & Action=='Collect' & When=='before')
data_place_transport <- subset(x=data_omit_nan, 
                             subset=Phase=='Place' & Action=='Deliver' & When=='before')
data_retrieve_free <- subset(x=data_omit_nan, 
                             subset=Phase=='Retrieve' & Action=='Collect' & When=='before')
data_retrieve_transport <- subset(x=data_omit_nan, 
                             subset=Phase=='Retrieve' & Action=='Deliver' & When=='before')

# data meeting the specific conditions
data_place_collect <- subset(x=data_omit_nan, 
                             subset=Phase=='Place' & Action=='Collect' & When=='during')
data_place_deliver <- subset(x=data_omit_nan, 
                             subset=Phase=='Place' & Action=='Deliver' & When=='during')
data_retrieve_collect <- subset(x=data_omit_nan, 
                                subset=Phase=='Retrieve' & Action=='Collect' & When=='during')
data_retrieve_deliver <- subset(x=data_omit_nan, 
                                subset=Phase=='Retrieve' & Action=='Deliver' & When=='during')



# place peg-free hand movement
# removed (1|Subject/Hand) random effect for before so it's not singular
model_p_free <- glmer(b_1 ~ Block*Hand*Session + (1+Session|Subject),
                        family = Gamma(link = "log"),
                        data = data_place_free,
                        control = glmerControl(optimizer = 'bobyqa')
                        )

hist(residuals(model_p_free), main = "Histogram of Residuals p free", xlab = "Residuals")

car::Anova(model_p_free)
emmeans(
  model_p_free, pairwise ~ Block*Hand
)
contrast(
  regrid(emmeans(model_p_free, pairwise ~ Block*Hand)),
  interaction=c('revpairwise','pairwise')
)

# place collect
model_pc <- glmer(b_1 ~ Block*Hand*Session + (1+Session|Subject),
                    family = Gamma(link = "log"),
                    data = data_place_collect,
                    control = glmerControl(optimizer = 'bobyqa')
                    )

hist(residuals(model_pc), main = "Histogram of Residuals pc", xlab = "Residuals")


car::Anova(model_pc)
emmeans(model_pc, pairwise ~ Block*Hand)
contrast(
  emmeans(model_pc, pairwise ~ Block*Hand),
  interaction=c('revpairwise','pairwise')
)

# place transport
# removed (1|Subject/Hand) random effect for before so it's not singular
model_p_transport <- glmer(b_1 ~ Block*Hand*Session + (1+Session|Subject),
                    family = Gamma(link = "log"),
                    data = data_place_transport,
                    control = glmerControl(optimizer = 'bobyqa')
                    )

hist(residuals(model_p_transport), main = "Histogram of Residuals p transport", xlab = "Residuals")


car::Anova(model_p_transport)
emmeans(
  model_p_transport, pairwise ~ Block*Hand
  )
contrast(
  regrid(emmeans(model_p_transport, pairwise ~ Block*Hand)),
  interaction=c('revpairwise','pairwise')
  )


# place deliver
model_pd <- glmer(b_1 ~ Block*Hand*Session + (1|Subject),
                    family = Gamma(link = "log"),
                    data = data_place_deliver,
                    control = glmerControl(optimizer = 'bobyqa')
                    )

hist(residuals(model_pd), main = "Histogram of Residuals pd", xlab = "Residuals")


car::Anova(model_pd)
emmeans(model_pd, pairwise ~ Block*Hand | Session, type='response')
contrast(
  regrid(emmeans(model_pd, pairwise ~ Block*Hand)),
  interaction=c('revpairwise','pairwise')
)

# retrieve peg-free hand movement
# removed (1|Subject/Hand) random effect for before so it's not singular
model_r_free <- glmer(b_1 ~ Block*Hand*Session + (1+Session|Subject),
                        family = Gamma(link = "log"),
                        data = data_retrieve_free,
                        control = glmerControl(optimizer = 'bobyqa')
                        )

hist(residuals(model_r_free), main = "Histogram of Residuals r free", xlab = "Residuals")

car::Anova(model_r_free)
emmeans(
  model_r_free, pairwise ~ Block*Hand
)
contrast(
  regrid(emmeans(model_r_free, pairwise ~ Block*Hand)),
  interaction=c('revpairwise','pairwise')
)


# retrieve collect
model_rc <- glmer(b_1 ~ Block*Hand*Session + (1+Session|Subject),
                    family = Gamma(link = "log"),
                    data = data_retrieve_collect,
                    control = glmerControl(optimizer = 'bobyqa')
                    )

hist(residuals(model_rc), main = "Histogram of Residuals rc", xlab = "Residuals")


car::Anova(model_rc)
emmeans(
  model_rc, pairwise ~ Block*Hand
)
contrast(
  regrid(emmeans(model_rc, pairwise ~ Block*Hand)),
  interaction=c('revpairwise','pairwise')
)


# retrieve transport
model_r_transport <- glmer(b_1 ~ Block*Hand*Session + (1|Subject),
                              family = Gamma(link = "log"),
                              data = data_retrieve_transport,
                              control = glmerControl(optimizer = 'bobyqa')
                              )

hist(residuals(model_r_transport), main = "Histogram of Residuals r transport", xlab = "Residuals")


car::Anova(model_r_transport)
emmeans(
  model_r_transport, pairwise ~ Block*Hand
)
contrast(
  regrid(emmeans(model_r_transport, pairwise ~ Block*Hand)),
  interaction=c('revpairwise','pairwise')
)


# retrieve deliver
model_rd <- glmer(b_1 ~ Block*Hand*Session + (1+Session|Subject),
                    family = Gamma(link = "log"),
                    data = data_retrieve_deliver,
                    control = glmerControl(optimizer = 'bobyqa')
                    )

hist(residuals(model_rd), main = "Histogram of Residuals rd", xlab = "Residuals")

car::Anova(model_rd)
emmeans(
  model_rd, pairwise ~ Block*Hand
)
contrast(
  regrid(emmeans(model_rd, pairwise ~ Block*Hand)),
  interaction=c('revpairwise','pairwise')
)
