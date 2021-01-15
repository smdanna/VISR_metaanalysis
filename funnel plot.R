
# Loading data
visr <- read.csv("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/visr_meta/ORs 20210111.csv")

library(dplyr)
library(metafor)

View(visr)

joint.funnel <- filter(visr, is.na(rep_joint) & generated==0)

joint.funnel

meta.joint.funnel <- metagen(OR_ln,
                      SE,
                      data=joint.funnel,
                      studlab=paste(study),
                      comb.fixed = FALSE,
                      comb.random = TRUE,
                      method.tau = "DL",
                      hakn = FALSE,
                      prediction=FALSE,
                      sm="OR")

meta.joint




funnel(meta.joint)
