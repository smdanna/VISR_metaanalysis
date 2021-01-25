# cleaning data, generating ORs

library(dplyr)

# reading in rudimentary new OR data
new_ORs <- read.csv("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/visr_meta/new ORs 20210125.csv")

View(new_ORs)

#### calculating ORs and SEs from 2x2 data ####

# calculating ORs from 2x2 cells
#new_ORs$OR <- (new_ORs$a_case_int*new_ORs$d_noncase_cntl)/(new_ORs$b_noncase_int*new_ORs$c_case_cntl) # this eliminates the OR that is already there

# calculating ORs from 2x2 tables only when OR is NA
new_ORs <- mutate(new_ORs,OR=ifelse(is.na(OR),((new_ORs$a_case_int*new_ORs$d_noncase_cntl)/(new_ORs$b_noncase_int*new_ORs$c_case_cntl)),OR))

# calculating natural log of OR
new_ORs <- mutate(new_ORs, OR_ln = log(new_ORs$OR))

# calculating standard error from 2x2 cells
new_ORs <- mutate(new_ORs, SE = sqrt((1/new_ORs$a_case_int)+(1/new_ORs$b_noncase_int)+(1/new_ORs$c_case_cntl)+
                                       (1/new_ORs$d_noncase_cntl)))

# generated ORs now have lnOR and SE for meta-analyses

#### calculating SEs from CIs and p-values ####

## generating standard errors from confidence intervals
# https://handbook-5-1.cochrane.org/chapter_7/7_7_7_3_obtaining_standard_errors_from_confidence_intervals_and.htm
# https://handbook-5-1.cochrane.org/chapter_7/7_7_7_2_obtaining_standard_errors_from_confidence_intervals_and.htm

# calculating standard error from confidence intervals

str(new_ORs)

View(new_ORs)

new_ORs$CI.lo_ln <- as.numeric(new_ORs$CI.lo_ln)
new_ORs$CI.up_ln <- as.numeric(new_ORs$CI.up_ln)
new_ORs$p <- as.numeric(new_ORs$p)
new_ORs$z <- as.numeric(new_ORs$z)

# generating natural log of CI limits
new_ORs$CI.lo_ln <- log(new_ORs$CI.lo)
new_ORs$CI.up_ln <- log(new_ORs$CI.up)

# generating SEs from ln CI limits only if SE is NA
new_ORs <- mutate(new_ORs,SE=ifelse(is.na(SE),((new_ORs$CI.up_ln-new_ORs$CI.lo_ln)/3.92),SE))

## generating standard errors from p-values
# https://www.bmj.com/content/343/bmj.d2090.extract?casa_token=lHzXy8TFChoAAAAA:ttcY6DoDAUgArmKMQWhmXT-k1DME10aWiPQAmx1m7hnSresAnayDc867GshI08acrLJwurtg8ygXTw

# generating z-score corresponding to p-value
new_ORs$z <- qnorm(new_ORs$p)

# generating SEs from p-values ONLY when z is available (and not NA)
mutate(new_ORs,SE=ifelse(!is.na(z),(OR_ln/z),SE))

new_ORs <- mutate(new_ORs,SE=ifelse(!is.na(z),(OR_ln/z),SE))

# making SEs into absolute values
new_ORs <- mutate(new_ORs,SE=abs(SE))


# writing this dataframe to file
write.csv(new_ORs,'new ORs 20210125.csv')


#### binding new and existing data into one dataset ####

# reading in new and existing datasets
new_ORs <- read.csv("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/visr_meta/new ORs 20210125.csv")

View(new_ORs)

extant_ORs <- read.csv("~/Dropbox/PEPP/vocational interventions SR - VISR/6 Analysis/VISR meta-analysis/visr_meta/ORs 20210119.csv")

View(extant_ORs)

# binding together
ORs <- bind_rows(new_ORs, extant_ORs)

View(ORs)

# alphabetize by study author
ORs <- arrange(ORs, study)

str(ORs)
summary(ORs)

# remove pesky X columns that R creates
ORs <- ORs %>%
  mutate(
    X = NULL,
    X.1 = NULL
    )

write.csv(ORs,'ORs 20210125.csv') 

# rep_ columns for removing overlapping estimates are added and populated manually
