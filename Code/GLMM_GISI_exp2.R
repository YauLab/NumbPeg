require(readxl)
require(lme4)
require(car)
require(emmeans)



setwd('/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare')
# setwd('/path/to/data')

# import the data
place_collect <- readxl::read_excel('GazeIndex_LMM_table_ProgressDuring_PlaceCollect_exp2.xlsx')
place_deliver <- readxl::read_excel('GazeIndex_LMM_table_ProgressDuring_PlaceDeliver_exp2.xlsx')
retrieve_collect <- readxl::read_excel('GazeIndex_LMM_table_ProgressDuring_RetrieveCollect_exp2.xlsx')
retrieve_deliver <- readxl::read_excel('GazeIndex_LMM_table_ProgressDuring_RetrieveDeliver_exp2.xlsx')

place_free <- readxl::read_excel('GazeIndex_LMM_table_ProgressBefore_PlaceCollect_exp2.xlsx')
place_transport <- readxl::read_excel('GazeIndex_LMM_table_ProgressBefore_PlaceDeliver_exp2.xlsx')
retrieve_free <- readxl::read_excel('GazeIndex_LMM_table_ProgressBefore_RetrieveCollect_exp2.xlsx')
retrieve_transport <- readxl::read_excel('GazeIndex_LMM_table_ProgressBefore_RetrieveDeliver_exp2.xlsx')

# concatenate the dataframes by row
combined <- rbind(place_free, place_collect, place_transport, place_deliver, 
                   retrieve_free, retrieve_collect, retrieve_transport, retrieve_deliver)

data <- combined[which(combined$Subject != 1),] # omits subject 1 because of unreliability of gaze tracking
data$Progress <- factor(data$Progress)

# split the data according to action
place_free <- subset(x = data,
                      subset = Phase == 'Place' & Action == 'Collect' & When == 'before')
place_collect <- subset(x = data,
                        subset = Phase == 'Place' & Action == 'Collect' & When == 'during')
place_transport <- subset(x = data,
                          subset = Phase == 'Place' & Action == 'Deliver' & When == 'before')
place_deliver <- subset(x = data,
                        subset = Phase == 'Place' & Action == 'Deliver' & When == 'during')

retrieve_free <- subset(x = data,
                      subset = Phase == 'Retrieve' & Action == 'Collect' & When == 'before')
retrieve_collect <- subset(x = data,
                        subset = Phase == 'Retrieve' & Action == 'Collect' & When == 'during')
retrieve_transport <- subset(x = data,
                          subset = Phase == 'Retrieve' & Action == 'Deliver' & When == 'before')
retrieve_deliver <- subset(x = data,
                        subset = Phase == 'Retrieve' & Action == 'Deliver' & When == 'during')


model_p_free <- lmer(mmDifference ~ Block*Hand*Progress + (1+Session|Subject),
                     data = place_free,
                    )

hist(residuals(model_p_free), main = "Histogram of Residuals p_free", xlab = "Residuals")

car::Anova(model_p_free)
contrast(emmeans(model_p_free, ~Block),'revpairwise')
contrast(emmeans(model_p_free, ~ Block*Hand), interaction=c('revpairwise','pairwise'))
emres_p_free <- emmeans(model_p_free, ~ Block*Hand*Progress)
contrast(emres_p_free, interaction=c('revpairwise','pairwise'), Block*Hand, by='Progress')
emmeans(model_p_free, pairwise ~ Block*Hand)


model_pc <- lmer(mmDifference ~ Block*Hand*Progress + (1+Session|Subject),
                        data = place_collect,
                        )

hist(residuals(model_pc), main = "Histogram of Residuals pc", xlab = "Residuals")


car::Anova(model_pc)
contrast(emmeans(model_pc, ~Block),'revpairwise')
contrast(emmeans(model_pc, ~ Block*Hand), interaction=c('revpairwise','pairwise'))
emres_pc <- emmeans(model_pc, pairwise ~ Block*Hand*Progress)
contrast(emres_pc, interaction=c('revpairwise','pairwise'), Block*Hand, by='Progress')

model_p_transport <- lmer(mmDifference ~ Block*Hand*Progress + (1+Session|Subject),
                          data = place_transport,
                          control = lmerControl(optimizer = 'bobyqa')
                        )

hist(residuals(model_p_transport), main = "Histogram of Residuals p_transport", xlab = "Residuals")

car::Anova(model_p_transport)
contrast(emmeans(model_p_transport, ~Block), 'revpairwise')
contrast(emmeans(model_p_transport, ~Block*Hand), interaction=c('revpairwise','pairwise'))
emres_p_transport <- emmeans(model_p_transport, pairwise ~ Block*Hand*Progress)
contrast(emres_p_transport, interaction=c('revpairwise','pairwise'), Block*Hand, by='Progress')


model_pd <- lmer(mmDifference ~ Block*Hand*Progress + (1+Session|Subject),
                        data = place_deliver,
                        )

hist(residuals(model_pd), main = "Histogram of Residuals pd", xlab = "Residuals")

car::Anova(model_pd)
contrast(emmeans(model_pd, ~Block), 'revpairwise')
contrast(emmeans(model_pd, ~Block*Hand), interaction=c('revpairwise','pairwise'))
emres_pd <- emmeans(model_pd, pairwise ~ Block*Hand*Progress)
contrast(emres_pd, interaction=c('revpairwise','pairwise'), Block*Hand, by='Progress')
emmeans(model_pd, pairwise ~ Block*Hand)

model_r_free <- lmer(mmDifference ~ Block*Hand*Progress + (1+Session|Subject),
                     data = retrieve_free,
                     control = lmerControl(optimizer = 'bobyqa')
                    )

hist(residuals(model_r_free), main = "Histogram of Residuals r_free", xlab = "Residuals")

car::Anova(model_r_free)
contrast(emmeans(model_r_free, ~Block), 'revpairwise')
contrast(emmeans(model_r_free, ~Block*Hand), interaction=c('revpairwise','pairwise'))
emres_r_free <- emmeans(model_r_free, pairwise ~ Block*Hand*Progress)
contrast(emres_r_free, interaction =c('revpairwise','pairwise'), Block*Hand, by='Progress')
emmeans(model_r_free, pairwise ~ Block*Hand)


model_rc <- lmer(mmDifference ~ Block*Hand*Progress + (1+Session|Subject),
                        data = retrieve_collect
                        )

hist(residuals(model_rc), main = "Histogram of Residuals rc", xlab = "Residuals")

car::Anova(model_rc)
contrast(emmeans(model_rc, ~Block), 'revpairwise')
contrast(emmeans(model_rc, ~Block*Hand), interaction=c('revpairwise','pairwise'))
emres_rc <- emmeans(model_rc, pairwise~Block*Hand*Progress)
contrast(emres_rc, interaction=c('revpairwise','pairwise'),Block*Hand,by='Progress')
emmeans(model_rc, pairwise ~ Block*Hand)

model_r_transport <- lmer(mmDifference ~ Block*Hand*Progress + (1+Session|Subject),
                          data = retrieve_transport,
                          control = lmerControl(optimizer = 'bobyqa')
                          )

hist(residuals(model_r_transport), main = "Histogram of Residuals r_transport", xlab = "Residuals")

car::Anova(model_r_transport)
contrast(emmeans(model_r_transport, ~Block),'revpairwise')
contrast(emmeans(model_r_transport, ~Block*Hand), interaction=c('revpairwise','pairwise'))
emres_r_transport <- emmeans(model_r_transport, pairwise~Block*Hand*Progress)
contrast(emres_r_transport, interaction=c('revpairwise','pairwise'), Block*Hand, by='Progress')
emmeans(model_r_transport, pairwise ~ Block*Hand)

model_rd <- lmer(mmDifference ~ Block*Hand*Progress + (1+Session|Subject),
                        data = retrieve_deliver,
                        control = lmerControl(optimizer = 'bobyqa')
                        )

hist(residuals(model_rd), main = "Histogram of Residuals rd", xlab = "Residuals")

car::Anova(model_rd)
contrast(emmeans(model_rd, ~Block),'revpairwise')
contrast(emmeans(model_rd, ~Block*Hand), interaction=c('revpairwise','pairwise'))
emres_rd <- emmeans(model_rd, pairwise~Block*Hand*Progress)
contrast(emres_rd, interaction=c('revpairwise','pairwise'), Block*Hand, by='Progress')
emmeans(model_rd, pairwise ~ Block*Hand)


