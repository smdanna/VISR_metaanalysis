
# packages
library(meta)
library(metafor)
library(dplyr)

# loading data
visr <- read.csv("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/visr_meta/ORs 20210125.csv")

# inspecting dataframe
visr
str(visr)


#### EMPLOYMENT ####

emp <- filter(visr, emp==1 & is.na(rep_emp))

emp

meta.emp <- metagen(OR_ln,
                    SE,
                    data=emp,
                    studlab=paste(study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.emp

forest(meta.emp)

forest(meta.emp, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
     #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)
  
#### EDUCATION ####
  
edu <- filter(visr, edu==1 & is.na(rep_edu))

edu

meta.edu <- metagen(OR_ln,
                    SE,
                    data=edu,
                    studlab=paste(study),
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    method.tau = "DL",
                    hakn = FALSE,
                    prediction=FALSE,
                    sm="OR")

meta.edu

forest(meta.edu)

forest(meta.edu, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)
  
#### JOINT ####

joint <- filter(visr, is.na(rep_joint))

joint

meta.joint <- metagen(OR_ln,
                      SE,
                      data=joint,
                      studlab=paste(study),
                      comb.fixed = FALSE,
                      comb.random = TRUE,
                      method.tau = "DL",
                      hakn = FALSE,
                      prediction=FALSE,
                      sm="OR")

meta.joint

forest(meta.joint)

forest(meta.joint, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)

#### JOINT - interactions ####

joint.interaction <- filter(visr, interaction==1)

joint.interaction

meta.joint.interaction <- metagen(OR_ln,
                                  SE,
                                  data=joint.interaction,
                                  studlab=paste(study),
                                  comb.fixed = FALSE,
                                  comb.random = TRUE,
                                  method.tau = "DL",
                                  hakn = FALSE,
                                  prediction=FALSE,
                                  sm="OR")

meta.joint.interaction

forest(meta.joint.interaction)

forest(meta.joint.interaction, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)

#### JOINT - design ####

joint.des <- filter(visr, is.na(rep_joint.des))

joint.des

meta.joint.des <- metagen(OR_ln,
                          SE,
                          data=joint.des,
                          studlab=paste(study),
                          comb.fixed = FALSE,
                          comb.random = TRUE,
                          method.tau = "DL",
                          hakn = FALSE,
                          prediction=FALSE,
                          sm="OR")

meta.joint.des<-update.meta(meta.joint.des,
                            byvar=design, 
                            comb.random = T, 
                            comb.fixed = F)

meta.joint.des

forest(meta.joint.des)

forest(meta.joint.des, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)

#### JOINT - intervention ####

joint.inter <- filter(visr, is.na(rep_joint.inter))

joint.inter

meta.joint.inter <- metagen(OR_ln,
                            SE,
                            data=joint.inter,
                            studlab=paste(study),
                            comb.fixed = FALSE,
                            comb.random = TRUE,
                            method.tau = "DL",
                            hakn = FALSE,
                            prediction=FALSE,
                            sm="OR")

meta.joint.inter<-update.meta(meta.joint.inter, 
                              byvar=intervention, 
                              comb.random = T, 
                              comb.fixed = F)

meta.joint.inter

forest(meta.joint.inter)

forest(meta.joint.inter, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)

#### JOINT - mental health problem ####

joint.mh <- filter(visr, is.na(rep_joint.mh))

joint.mh

meta.joint.mh <- metagen(OR_ln,
                         SE,
                         data=joint.mh,
                         studlab=paste(study),
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         method.tau = "DL",
                         hakn = FALSE,
                         prediction=FALSE,
                         sm="OR")

meta.joint.mh<-update.meta(meta.joint.mh, 
                           byvar=mhp, 
                           comb.random = T, 
                           comb.fixed = F)

meta.joint.mh

forest(meta.joint.mh)

forest(meta.joint.mh, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)

#### JOINT - control ####

joint.con <- filter(visr, is.na(rep_joint.con))

joint.con

meta.joint.con <- metagen(OR_ln,
                          SE,
                          data=joint.con,
                          studlab=paste(study),
                          comb.fixed = FALSE,
                          comb.random = TRUE,
                          method.tau = "DL",
                          hakn = FALSE,
                          prediction=FALSE,
                          sm="OR")

meta.joint.con<-update.meta(meta.joint.con, 
                            byvar=control, 
                            comb.random = T, 
                            comb.fixed = F)

meta.joint.con

forest(meta.joint.con)

forest(meta.joint.con, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)

#### JOINT - time ####

joint.time <- filter(visr, is.na(rep_joint.time))

joint.time

meta.joint.time <- metagen(OR_ln,
                           SE,
                           data=joint.time,
                           studlab=paste(study),
                           comb.fixed = FALSE,
                           comb.random = TRUE,
                           method.tau = "DL",
                           hakn = FALSE,
                           prediction=FALSE,
                           sm="OR")

meta.joint.time<-update.meta(meta.joint.time, 
                             byvar=time, 
                             comb.random = T, 
                             comb.fixed = F)

meta.joint.time

forest(meta.joint.time)

forest(meta.joint.time, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)

#### JOINT - risk of bias ####

joint.rob <- filter(visr, is.na(rep_joint.rob))

joint.rob

meta.joint.rob <- metagen(OR_ln,
                          SE,
                          data=joint.rob,
                          studlab=paste(study),
                          comb.fixed = FALSE,
                          comb.random = TRUE,
                          method.tau = "DL",
                          hakn = FALSE,
                          prediction=FALSE,
                          sm="OR")

meta.joint.rob<-update.meta(meta.joint.rob, 
                            byvar=rob, 
                            comb.random = T, 
                            comb.fixed = F)

meta.joint.rob

forest(meta.joint.rob)

forest(meta.joint.rob, 
       studlab = T,
       comb.random=TRUE,
       overall = T,
       #  xlim = c(0.5,14.0),                    # set x axis limits
       text.random = "Random effects model",  # change text
       col.random = "black",
       type.random = "diamond",
       leftcols=c("studlab"),
       leftlabs = c("Study"),
       smlab = "",                            # remove "Odds Ratio" text above plot
       rightcols = c("effect.ci"),
       rightlabs = c("Odds Ratio [95% CI]"),
       just = "left", just.addcols = "left", just.studlab = "left",
       col.study = "black",
       col.square = "grey",
       col.square.lines = "grey",
       col.inside = "black",
       col.diamond = "white",
       hetstat = TRUE,
       overall.hetstat=T, 
       print.I2 = T,
       print.tau2=F)
  