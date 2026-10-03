#Yellow cells in dataset are modified or filled in by me. 6.25 clutch max rounded down to 6.25

library(readxl)
df2=read_excel("Opt02_GraceEdit.xlsx")

df2$TNT <- ifelse(
  df2$`2024 IUCN Red List category` %in% c("VU", "EN", "CR"), 1,
  ifelse(df2$`2024 IUCN Red List category` %in% c("LC", "NT"), 0, NA)
)


#GLB with binomial errors/binomial logistic regression
mod1 = glm(TNT~Clutch_Max,family=binomial,data=df2)
summary(mod1)
plot(mod1)

#Clutch_Max is significant (and so is the intercept>)
#Check for overdispersion: 97.17/73=1.33 which is less than 1.5 so NO overdispersion. 75 datapoints anyway

#Checking using DHARMa to make it easier to check GLM w binomial errors (cause of binary outcomes)

install.packages("DHARMa")
library(DHARMa)

testDispersion(mod1)
# dispersion = 1.0174, p-value = 0.784
testZeroInflation(mod1)
#ratioObsSim = 1.0026, p-value = 1
#alternative hypothesis: two.sided
testOutliers(mod1)
#ratioObsSim = 1.0026, p-value = 1
#alternative hypothesis: two.sided
testResiduals(mod1)
#ratioObsSim = 1.0026, p-value = 1
#alternative hypothesis: two.sided

#Passed every single one cleanly yay!

plot(simulateResiduals(mod1)) #OK not yay re: the right one

########

#OK I try to account for taxonomy.

library(lme4)

mod2Fam <- glmer(
  TNT ~ Clutch_Max + (1 | df2$`Family HBW/BirdLife v9.1 (2024)`),
  family = binomial,
  data = df2
)
summary(mod2Fam)
plot(mod2Fam)

mod3Fam <- glmer(
  TNT ~ Clutch_Max + (1 | df2$`Average Mass`),
  family = binomial,
  data = df2
)
summary(mod3Fam)
plot(mod3Fam)


#Need to check for diagnostics but ya the simpler GLM has slightly better AIC than either the model accounting for mass or the one accounting for Family.

#Clutch_Max coefficient is so stable, almost no effect on the estimated effect size.

#Each one-unit increase in maximum clutch size was associated with approximately 26% lower odds of being threatened (OR ≈ 0.74).

# So, if Family was specified a priori because you wanted to account for taxonomic non-independence, I'd lean toward reporting the GLMM as the primary model, with the ordinary GLM as a sensitivity comparison. The fact that both give essentially the same effect size is useful supporting evidence.


#TESTING TO SEE IF FAMILY IS A WORTHY THING TO INCLUDE OR NOT DESPITE THE SMOL DIFFERENCE

#False: the model isn't showing evidence that the random-effect structure is too complex for the data.
isSingular(mod2Fam)

#How many families only contain 1 species. A random effect cannot learn much within-family clustering from singleton families.
table(df2$`Family HBW/BirdLife v9.1 (2024)`)

#Lean toward using GLMM as the main model bc family should be factored.
