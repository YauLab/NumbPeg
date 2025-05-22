require(readxl)
require(lme4)
require(car)
require(emmeans)



# choose the dataset
data <- readxl::read_excel('/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare/lmm_coefs_exp_decay_exp1.xlsx')
# data <- readxl::read_excel('/path/to/data/lmm_coefs_exp_decay_exp1.xlsx')


# remove b_1 <= 0 due to model fit errors 
data_omit_nan <- data[which(data$b_1 > 0),]

# subset the data to specific actions ----
data_place_free <- subset(x=data_omit_nan, 
                          subset=Phase=='Place' & Action=='Collect' & When=='before')
data_place_transport <- subset(x=data_omit_nan, 
                               subset=Phase=='Place' & Action=='Deliver' & When=='before')
data_retrieve_free <- subset(x=data_omit_nan, 
                             subset=Phase=='Retrieve' & Action=='Collect' & When=='before')
data_retrieve_transport <- subset(x=data_omit_nan, 
                                  subset=Phase=='Retrieve' & Action=='Deliver' & When=='before')

# subset the data to specific actions
data_place_collect <- subset(x=data_omit_nan, 
                             subset=Phase=='Place' & Action=='Collect' & When=='during')
data_place_deliver <- subset(x=data_omit_nan, 
                             subset=Phase=='Place' & Action=='Deliver' & When=='during')
data_retrieve_collect <- subset(x=data_omit_nan, 
                                subset=Phase=='Retrieve' & Action=='Collect' & When=='during')
data_retrieve_deliver <- subset(x=data_omit_nan, 
                                subset=Phase=='Retrieve' & Action=='Deliver' & When=='during')

# data_place <- subset(x=data_omit_nan, subset=Phase=='Place')
# data_retrieve <- subset(x=data_omit_nan, subset=Phase=='Retrieve')
# 
# mean(data_place$rsq, na.rm = TRUE)
# sd(data_place$rsq, na.rm = TRUE) / sqrt(sum(!is.na(data_place$rsq)))

# place peg-free hand movement
model_p_free <- glmer(b_1 ~ Block*Hand*Session + (1 + Session|Subject),
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
  emmeans(model_p_free, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
  )


# place collect
model_pc <- glmer(b_1 ~ Block*Hand*Session + (1+Block*Hand|Subject),
                    family = Gamma(link = "log"),
                    data = data_place_collect,
                    control = glmerControl(optimizer = 'bobyqa')
                  )

hist(residuals(model_pc), main = "Histogram of Residuals pc", xlab = "Residuals")


car::Anova(model_pc)
emmeans(
  model_pc, 
  pairwise ~ Block*Hand
  )
contrast(
  emmeans(model_pc, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
  )

# place transport
model_p_transport <- glmer(b_1 ~ Block*Hand*Session + (1+Block*Hand|Subject),
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
  emmeans(model_p_transport, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
  )


# place deliver
model_pd <- glmer(b_1 ~ Block*Hand*Session + (1+Block*Hand|Subject),
                    family = Gamma(link = "log"),
                    data = data_place_deliver,
                    control = glmerControl(optimizer = 'bobyqa')
                  )

hist(residuals(model_pd), main = "Histogram of Residuals pd", xlab = "Residuals")

car::Anova(model_pd)
emmeans(
  model_pd, 
  pairwise ~ Block*Hand
  )
contrast(
  emmeans(model_pd, pairwise ~ Block*Hand), 
  interaction=c('revpairwise','revpairwise')
  )

# retrieve peg-free hand movement
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
  emmeans(model_r_free, pairwise ~ Block*Hand),
  interaction=c('revpairwise','revpairwise')
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
  emmeans(model_rc, pairwise ~ Block*Hand),
  interaction=c('revpairwise','revpairwise')
)

# retrieve transport
model_r_transport <- glmer(b_1 ~ Block*Hand*Session + (1+Block*Hand|Subject),
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
  emmeans(model_r_transport, pairwise ~ Block*Hand),
  interaction=c('revpairwise','revpairwise')
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
  emmeans(model_rd, pairwise ~ Block*Hand),
  interacation=c('revpairwise','revpairwise')
)
