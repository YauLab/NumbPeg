library(lme4)
library(readxl)
library(car)
library(emmeans)

PeakForce_PlaceCollect <- read_excel("/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare/GISI_PeakForce_PlaceCollect_exp1.xlsx")
PeakForce_PlaceDeliver <- read_excel("/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare/GISI_PeakForce_PlaceDeliver_exp1.xlsx")
PeakForce_RetrieveCollect <- read_excel("/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare/GISI_PeakForce_RetrieveCollect_exp1.xlsx")
PeakForce_RetrieveDeliver <- read_excel("/Users/kevin/Desktop/Kevin/NumbPeg/Manuscript/Revisions/DataShare/GISI_PeakForce_RetrieveDeliver_exp1.xlsx")
#glme_table <- read_excel("/Volumes/bcm-neuro-yau/data/DATA/PerAnesthesiaPeg/Final data/glme_table.xlsx")
#View(TimeDuring_PlaceCollect)

PlaceCollect_mmDifference <- lmer(mmDifference ~ Time*Hand + (1|Subject/Session),
                                     data=PeakForce_PlaceCollect)
car::Anova(PlaceCollect_mmDifference)
emres_pc_mmDiff <- emmeans(PlaceCollect_mmDifference, ~ Time*Hand)
contrast(emres_pc_mmDiff, interaction=c('revpairwise'), Time*Hand, by="Hand", adjust='mvt')
contrast(emres_pc_mmDiff, interaction=c('revpairwise','revpairwise'), Time*Hand, adjust='mvt')

PlaceDeliver_mmDifference <- lmer(mmDifference ~ Time*Hand + (1|Subject/Session),
                                     data=PeakForce_PlaceDeliver)
car::Anova(PlaceDeliver_mmDifference)
emres_pd_mmDiff <- emmeans(PlaceDeliver_mmDifference, ~Time*Hand)
contrast(emres_pd_mmDiff, interaction=c('revpairwise','revpairwise'), Time*Hand, adjust='mvt')

RetrieveCollect_mmDifference <- lmer(mmDifference ~ Time*Hand + (1|Subject/Session),
                                        data=PeakForce_RetrieveCollect)
car::Anova(RetrieveCollect_mmDifference)
emres_rc_mmDiff <- emmeans(RetrieveCollect_mmDifference, ~Time*Hand)
contrast(emres_rc_mmDiff, interaction=c('revpairwise','revpairwise'), Time*Hand, adjust='mvt')

RetrieveDeliver_mmDifference <- lmer(mmDifference ~ Time*Hand + (1|Subject/Session),
                                        data=PeakForce_RetrieveDeliver)
car::Anova(RetrieveDeliver_mmDifference)
emres_rd_mmDiff <- emmeans(RetrieveDeliver_mmDifference, ~Time*Hand)
contrast(emres_rd_mmDiff, interaction=c('revpairwise','revpairwise'), Time*Hand, adjust='mvt')
