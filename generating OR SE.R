#### cleaning generated ORs ####

# reading in rudimentary new OR data
library(readxl)
new_ORs <- read_excel("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/new_ORs.xlsx")

View(new_ORs)

# calculating ORs from 2x2 cells
new_ORs$OR <- (new_ORs$a_case_int*new_ORs$d_noncase_cntl)/(new_ORs$b_noncase_int*new_ORs$c_case_cntl)

# calculating natural log of OR
new_ORs$OR_ln <- log(new_ORs$OR)

# calculating standard error from 2x2 cells
new_ORs$SE <- sqrt((1/new_ORs$a_case_int)+(1/new_ORs$b_noncase_int)+(1/new_ORs$c_case_cntl)+
                     (1/new_ORs$d_noncase_cntl))

# new/generated ORs now have lnOR and SE for meta-analyses

# writing this dataframe to file
write.csv(new_ORs,'new ORs 20201231.csv')

#### cleaning extants ORs ####

# reading in extant OR data
extant_ORs <- read.csv("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/visr_meta/extant ORs 20210105 before if.csv")

View(extant_ORs) 

summary(extant_ORs$p)
str(extant_ORs)

extant_ORs$OR <- as.numeric(extant_ORs$OR)
extant_ORs$CI.up <- as.numeric(extant_ORs$CI.up)
extant_ORs$CI.lo <- as.numeric(extant_ORs$CI.lo)
extant_ORs$p <- as.numeric(extant_ORs$p)

library(dplyr)

View(extant_ORs)

# calculating natural log of OR
extant_ORs$OR_ln <- log(extant_ORs$OR)

## generating standard errors from confidence intervals
# https://handbook-5-1.cochrane.org/chapter_7/7_7_7_3_obtaining_standard_errors_from_confidence_intervals_and.htm
# https://handbook-5-1.cochrane.org/chapter_7/7_7_7_2_obtaining_standard_errors_from_confidence_intervals_and.htm

# generating natural log of CI limits
extant_ORs$CI.lo_ln <- log(extant_ORs$CI.lo)
extant_ORs$CI.up_ln <- log(extant_ORs$CI.up)

# generating SEs from ln CI limits
extant_ORs$SE <- ((extant_ORs$CI.up_ln-extant_ORs$CI.lo_ln)/3.92)

## generating standard errors from p-values
# https://www.bmj.com/content/343/bmj.d2090.extract?casa_token=lHzXy8TFChoAAAAA:ttcY6DoDAUgArmKMQWhmXT-k1DME10aWiPQAmx1m7hnSresAnayDc867GshI08acrLJwurtg8ygXTw

# generating z-score corresponding to p-value
extant_ORs$z <- qnorm(extant_ORs$p)

# generating SEs from p-values ONLY when z is available (and not NA)
mutate(extant_ORs,SE=ifelse(!is.na(z),(OR_ln/z),SE))

extant_ORs <- mutate(extant_ORs,SE=ifelse(!is.na(z),(OR_ln/z),SE))

extant_ORs <- mutate(extant_ORs,SE=abs(SE))

View(extant_ORs)

write.csv(extant_ORs,'extant ORs 20210106.csv')

#### binding new and extant ORs into one dataset ####

# reading in cleaned new OR data
new_ORs <- read.csv("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/visr_meta/new ORs 20201231.csv")

View(new_ORs)

ORs <- bind_rows(new_ORs, extant_ORs)

View(ORs)

str(ORs)
summary(ORs)

ORs <- ORs %>%
  mutate(
    X = NULL,
    X.1 = NULL
    )

ORs <- arrange(ORs, study)

write.csv(ORs,'ORs 20210106.csv') 

# subsequently, I manually added rep_ columns for removing overlapping estimates and 
# named the file 'ORs 20210108.csv'

ORs <- read.csv("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/visr_meta/ORs 20210108.csv")

View(ORs)

ORs <- arrange(ORs, study)

write.csv(ORs,'ORs 20210111.csv')
