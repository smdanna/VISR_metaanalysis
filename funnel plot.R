# https://rdrr.io/cran/metafor/man/funnel.html
# https://toptipbio.com/funnel-plot/
# https://handbook-5-1.cochrane.org/chapter_10/10_4_1_funnel_plots.htm

library(dplyr)
library(meta)
library(metafor)


#### funnel plot ####
visr <- read.csv("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/visr_meta/ORs 20210125.csv")

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

meta.joint.funnel

forest(meta.joint.funnel)

# basic plot
funnel(meta.joint.funnel)

# changing the x limits
funnel(meta.joint.funnel, xlim = c(.05,50))
funnel(meta.joint.funnel, xlim = c(.025,200))

# with study labels
funnel(meta.joint.funnel, studlab = TRUE)

# details of estimates being used
select(joint.funnel, study, OR, SE)

#### egger's test ####
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/smallstudyeffects.html
# https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/eggers.test.R

eggers.test = function(x) {
  
  # Validate
  x = x
  
  if (x$k < 10) {
    
    warning(paste("Your meta-analysis contains k =",
                  x$k, "studies. Egger's test may lack the statistical power to detect bias when the number of studies is small (i.e., k<10)."))
    
  }
  
  if (class(x)[1] %in% c("meta", "metabin", "metagen", "metacont", "metacor", "metainc", "metaprop")) {
    
    # Conduct metabias
    eggers = meta::metabias(x, k.min = 3, method = "linreg")
    
    # Get Intercept
    intercept = as.numeric(eggers$estimate[1])
    
    # Get SE
    se = as.numeric(eggers$estimate[2])
    
    # Calculate 95CI
    llci = intercept - qnorm(0.975) * se
    ulci = intercept + qnorm(0.975) * se
    
    # Get t
    t = as.numeric(eggers$statistic)
    
    # Get df
    df = as.numeric(eggers$parameters)
    
    # Get p
    p = as.numeric(eggers$p.value)
    
    # Make df
    returnlist = list(intercept = intercept,
                      llci = llci,
                      ulci = ulci,
                      t = t,
                      p = p,
                      meta.obj = x)
    
  } else {
    
    stop("x must be of type 'metabin', 'metagen', 'metacont', 'metainc' or 'metaprop'")
    
  }
  
  class(returnlist) = "eggers.test"
  
  return(returnlist)
  
}

## running test
eggers.test(x = meta.joint.funnel)
