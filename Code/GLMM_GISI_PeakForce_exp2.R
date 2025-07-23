library(lme4)
library(readxl)

PeakForce_PlaceCollect <- read_excel("/path/to/data/GISI_PeakForce_PlaceCollect_exp2.xlsx")
PeakForce_PlaceDeliver <- read_excel("/path/to/data/GISI_PeakForce_PlaceDeliver_exp2.xlsx")
PeakForce_RetrieveCollect <- read_excel("/path/to/data/GISI_PeakForce_RetrieveCollect_exp2.xlsx")
PeakForce_RetrieveDeliver <- read_excel("/path/to/data/GISI_PeakForce_RetrieveDeliver_exp2.xlsx")

PlaceCollect_mmDifference <- lmer(mmDifference ~ Block*Hand + (1|Subject/Round),
                                     data=PeakForce_PlaceCollect)
car::Anova(PlaceCollect_mmDifference)
emres_pc_mmDiff <- emmeans(PlaceCollect_mmDifference, ~ Block*Hand)
contrast(emres_pc_mmDiff, interaction=c('revpairwise','pairwise'), Block*Hand, adjust='mvt')

PlaceDeliver_mmDifference <- lmer(mmDifference ~ Block*Hand + (1|Subject/Round),
                                     data=PeakForce_PlaceDeliver)
car::Anova(PlaceDeliver_mmDifference)
emres_pd_mmDiff <- emmeans(PlaceDeliver_mmDifference, ~Block*Hand)
contrast(emres_pd_mmDiff, interaction=c('revpairwise','pairwise'), Block*Hand, adjust='mvt')

RetrieveCollect_mmDifference <- lmer(mmDifference ~ Block*Hand + (1|Subject/Round),
                                        data=PeakForce_RetrieveCollect)
car::Anova(RetrieveCollect_mmDifference)
emres_rc_mmDiff <- emmeans(RetrieveCollect_mmDifference, ~Block*Hand)
contrast(emres_rc_mmDiff, interaction=c('revpairwise','pairwise'), Block*Hand, adjust='mvt')

RetrieveDeliver_mmDifference <- lmer(mmDifference ~ Block*Hand + (1|Subject/Round),
                                        data=PeakForce_RetrieveDeliver)
car::Anova(RetrieveDeliver_mmDifference)
emres_rd_mmDiff <- emmeans(RetrieveDeliver_mmDifference, ~Block*Hand)
contrast(emres_rd_mmDiff, interaction=c('revpairwise','pairwise'), Block*Hand, adjust='mvt')

