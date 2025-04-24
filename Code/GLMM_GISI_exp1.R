require(lme4)
require(ggplot2)
require(emmeans)
require(dplyr)
# require(broom.mixed)
# require(kableExtra)

setwd('/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare')
# setwd('/path/to/data')

place_collect <- readxl::read_excel('GazeIndex_LMM_table_ProgressDuring_PlaceCollect_exp1.xlsx')
place_deliver <- readxl::read_excel('GazeIndex_LMM_table_ProgressDuring_PlaceDeliver_exp1.xlsx')
retrieve_collect <- readxl::read_excel('GazeIndex_LMM_table_ProgressDuring_RetrieveCollect_exp1.xlsx')
retrieve_deliver <- readxl::read_excel('GazeIndex_LMM_table_ProgressDuring_RetrieveDeliver_exp1.xlsx')

place_free <- readxl::read_excel('GazeIndex_LMM_table_ProgressBefore_PlaceCollect_exp1.xlsx')
place_transport <- readxl::read_excel('GazeIndex_LMM_table_ProgressBefore_PlaceDeliver_exp1.xlsx')
retrieve_free <- readxl::read_excel('GazeIndex_LMM_table_ProgressBefore_RetrieveCollect_exp1.xlsx')
retrieve_transport <- readxl::read_excel('GazeIndex_LMM_table_ProgressBefore_RetrieveDeliver_exp1.xlsx')

place_free$Progress <- factor(place_free$Progress)
place_collect$Progress <- factor(place_collect$Progress)
place_transport$Progress <- factor(place_transport$Progress)
place_deliver$Progress <- factor(place_deliver$Progress)

retrieve_free$Progress <- factor(retrieve_free$Progress)
retrieve_collect$Progress <- factor(retrieve_collect$Progress)
retrieve_transport$Progress <- factor(retrieve_transport$Progress)
retrieve_deliver$Progress <- factor(retrieve_deliver$Progress)



model_p_free <- lmer(mmDifference ~ Block*Hand*Progress + (1+Session|Subject),
                     data = place_free,
                     control = lmerControl(optimizer='bobyqa')
)

hist(residuals(model_p_free))
car::Anova(model_p_free)
emres_p_free <- emmeans(model_p_free, ~ Block*Hand*Progress)
contrast(emres_p_free, interaction=c('pairwise','pairwise'), Block*Hand, by='Progress')
emmeans(model_p_free, pairwise ~ Block*Hand)

model_pc <- lmer(mmDifference ~ Block*Hand*Progress + (1+Block*Hand|Subject) + (0 + Session|Subject),
                 data = place_collect,
                 control = lmerControl(optimizer = 'bobyqa'),
)

hist(residuals(model_pc))
car::Anova(model_pc)
emres_pc <- emmeans(model_pc, ~ Block*Hand*Progress)
contrast(emres_pc, interaction=c('pairwise','pairwise'), Block*Hand, by='Progress')
contrast(emres_pc, 'pairwise', by=c('Hand','Progress'))
emmeans(model_pc, pairwise ~ Block*Hand)


model_p_transport <- lmer(mmDifference ~ Block*Hand*Progress + (1+Block*Hand|Subject) + (0 + Session|Subject),
                          data = place_transport,
                          control = lmerControl(optimizer = 'bobyqa')
)

car::Anova(model_p_transport)
emres_p_transport <- emmeans(model_p_transport, ~ Block*Hand*Progress)
contrast(emres_p_transport, interaction=c('pairwise','pairwise'), Block*Hand, by='Progress')
emmeans(model_p_transport, pairwise ~ Block*Hand)

model_pd <- lmer(mmDifference ~ Block*Hand*Progress + (1 + Block*Hand|Subject) + (0+Session|Subject),
                 data = place_deliver,
                 control = lmerControl(optimizer = 'bobyqa')
)

car::Anova(model_pd)
emres_pd <- emmeans(model_pd, ~ Block*Hand*Progress)
contrast(emres_pd, interaction=c('pairwise','pairwise'), Block*Hand, by='Progress')
emmeans(model_pd, pairwise ~ Block*Hand)

model_r_free <- lmer(mmDifference ~ Block*Hand*Progress + (1 + Block*Hand|Subject),
                     data = retrieve_free,
                     control=lmerControl(optimizer = 'bobyqa')
)

car::Anova(model_r_free)
emres_r_free <- emmeans(model_r_free, ~Block*Hand*Progress)
contrast(emres_r_free, interaction=c('pairwise','pairwise'), by='Progress')
emmeans(model_r_free, pairwise ~ Block*Hand)

model_rc <- lmer(mmDifference ~ Block*Hand*Progress + (1+Block*Hand|Subject) + (0 + Session|Subject),
                 data = retrieve_collect,
                 control = lmerControl(optimizer = 'bobyqa')
)


car::Anova(model_rc)
emres_rc <- emmeans(model_rc, ~Block*Hand*Progress)
contrast(emres_rc, interaction=c('pairwise','pairwise'), by='Progress')
emmeans(model_rc, pairwise ~ Block*Hand)

model_r_transport <- lmer(mmDifference ~ Block*Hand*Progress + (1+Block*Hand|Subject) + (0 + Session|Subject),
                          data = retrieve_transport,
                          control = lmerControl(optimizer = 'bobyqa')
)

car::Anova(model_r_transport)
emres_r_transport <- emmeans(model_r_transport, ~Block*Hand*Progress)
contrast(emres_r_transport, interaction=c('pairwise','pairwise'), by='Progress')
emmeans(model_r_transport, pairwise ~ Block*Hand)

model_rd <- lmer(mmDifference ~ Block*Hand*Progress + (1 + Session|Subject),
                 data = retrieve_deliver,
                 control = lmerControl(optimizer = 'bobyqa')
)

car::Anova(model_rd)
emres_rd <- emmeans(model_rd, ~Block*Hand*Progress)
contrast(emres_rd, interaction=c('pairwise','pairwise'), by='Progress')
emmeans(model_rd, pairwise ~ Block*Hand)

# Looking at the mean GISI between coarse collect and precise collect
place_collect_smry <- place_collect %>% 
  group_by(Block,Hand,Progress) %>% 
  dplyr::summarise(
    mean_response = mean(mmDifference,na.rm=TRUE)
    )

retrieve_collect_smry <- retrieve_collect %>% 
  group_by(Block,Hand,Progress) %>% 
  dplyr::summarise(
    mean_response = mean(mmDifference,na.rm=TRUE)
    )

t.test(place_collect_smry$mean_response,retrieve_collect_smry$mean_response)
place_collect_smry_se <- sd(place_collect_smry$mean_response) / sqrt(sum(!is.na(place_collect_smry$mean_response)))
retrieve_collect_smry_se <- sd(retrieve_collect_smry$mean_response) / sqrt(sum(!is.na(retrieve_collect_smry$mean_response)))
