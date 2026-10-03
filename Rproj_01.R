#Ex-q me since it's not quasibinomial you NEED to check all your assumptions thank u
#Check with DHARMa!!!!
# 
# install.packages("DHARMa")
# library(DHARMa)
# 
# testDispersion(mod1)
# testZeroInflation(mod1)
# testOutliers(mod1)
# testResiduals(mod1)


library(readxl)
df=read_excel("Opt03_GraceEdit.xlsx")
str(df)

# Dietary and habitat breadth were mean-centred prior to analysis. 

df$TNT <- ifelse(
  df$`2024 IUCN Red List category` %in% c("VU", "EN", "CR"), 1,
  ifelse(df$`2024 IUCN Red List category` %in% c("LC", "NT"), 0, NA)
)

#model with centering
mod1 = glm(TNT~DBC*HBC,family=binomial,data=df)
summary(mod1)
plot(mod1)


testDispersion(mod1)
# dispersion = 0.99775, p-value = 0.936
# alternative hypothesis: two.sided
testZeroInflation(mod1)
# ratioObsSim = 1.0002, p-value = 1
# alternative hypothesis: two.sided
testOutliers(mod1)
# outliers at both margin(s) = 16, observations = 1376, p-value =
#   0.1276
# alternative hypothesis: true probability of success is not equal to 0.007968127
# 95 percent confidence interval:
#   0.006660556 0.018814473
# sample estimates:
#   frequency of outliers (expected: 0.00796812749003984 ) 
# 0.01162791 
testResiduals(mod1)
#ratioObsSim = 1.0026, p-value = 1
#alternative hypothesis: two.sided






#trial model without centering
mod2 = glm(TNT~DB*HB,family=binomial,data=df)
summary(mod2)
# wow ok it's actually easier to rule out db and db:hb
#definitely no overdispersion here
mod2.1=update(mod2,~.-DB:HB)
summary(mod2.1)
#wait wat. after removing DB:HB DB becomes significant. wAT ok nvm we just stick with centering


# Results for mod1


# Call:
#   glm(formula = TNT ~ DBC * HBC, family = binomial, data = df)
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -1.79601    0.08315 -21.599  < 2e-16 ***
#   DBC          0.19942    0.07384   2.701  0.00692 ** 
#   HBC         -0.38630    0.06892  -5.605 2.08e-08 ***
#   DBC:HBC      0.06053    0.05953   1.017  0.30924    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1189.3  on 1375  degrees of freedom
# Residual deviance: 1149.8  on 1372  degrees of freedom
# AIC: 1157.8
# 
# Number of Fisher Scoring iterations: 5


#Checking for overdispersion
1149.8/1372 #= 0.8380466 so NO overdispersion because less than 1.5

#Removing non-significant DBC:HBC
mod1.1=update(mod1,~.-DBC:HBC)
summary(mod1.1) #this is the minimum model!

#OK. Now to interpret. Whoo.

# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -1.77861    0.08046 -22.104  < 2e-16 ***
#   DBC          0.18441    0.07246   2.545   0.0109 *  
#   HBC         -0.37263    0.06686  -5.573 2.51e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1189.3  on 1375  degrees of freedom
# Residual deviance: 1150.8  on 1373  degrees of freedom
# AIC: 1156.8
# 
# Number of Fisher Scoring iterations: 5

#### Need to convert to probability of the binary outcome (threatened/non-threatened) using 1/(1+1/exp(the estimate value)) 

#for the intercept estimate
1/(1+1/exp(-1.77861)) #0.1444749 - 14.5% chance of being classified as threatened for a species with mean dietary breadth and mean habitat breadth (baseline probability)

#for DBC estimate (intercept PLUS estimate)
1/(1+1/exp(-1.77861+0.18441)) #0.1687938 - If DB increases by one unit while HB stays constant, probability of being classified as threatened increases from 14.5% (the mean) to 16.9%

#for HBC estimate (intercept PLUS estimate)
1/(1+1/exp(-1.77861-0.37263)) #0.1042154 - If HB increases by one unit while DB stays constant, probability of being classified as threatened decreases from 14.5% (the mean) to 10.4%.


#Trying out odds ratios (basically a proportion, which is your y, in this case proportion of NT:T)
#so going backwards for log e X is e^x so

#Odds ratio (using just the estimate)
exp(0.18441) # 1.202509
#For every one-unit increase in dietary breadth, the odds of being threatened are multiplied by approximately 1.20, while holding habitat breadth constant.
(1.20-1) x 100 = 20%
#So each one-unit increase in dietary breadth was associated with approximately 20% higher odds of being threatened, after accounting for habitat breadth (OR = 1.20, p = 0.011).


#Odds for HBC (using just the estimate)
exp(-0.37263) # 0.6889201
#For every one-unit increase in habitat breadth, the odds of being threatened are multiplied by approximately 0.689, while holding dietary breadth constant.

(1-0.689)*100 #31.1
#So each one-unit increase in habitat breadth was associated with approximately 31.1% LOWER (because we 1-x) odds of being threatened, after accounting for dietary breadth (OR = 0.69, p < 0.001).


#OK what. How to explain these weird results. Time to try accounting for Family by turning binomial GLM into binomial GLMM (generalised linear mixed model) with family as random intercept. WHICH WE HAVE NOT LEARNT YET

library(lme4)

modFam <- glmer(
  TNT ~ DBC + HBC + (1 | `Family HBW/BirdLife v9.1 (2024)`),
  family = binomial,
  data = df
)

summary(modFam)

#Family changes conclusion about dietary breadth, but not habitat breadth.
#Once family is accounted for, there is no clear association (p=0.261 >0.05)
#Only HBC remains significant
#For each one-unit increase in habitat breadth, the odds of a species being threatened were approximately 30% lower, after accounting for dietary breadth and variation among families.

#Family random effect
Variance = 0.83
SD       = 0.911
# Baseline threatened status varied among taxonomic families (random-intercept variance = 0.83, SD = 0.91).
# This indicates substantial variation among families in their baseline log-odds of threatened status, even after accounting for DB and HB.

#Need to compare GLM and GLMM
# But after accounting for Family, the DB coefficient shrinks from 0.184 → 0.107 and is no longer statistically significant.
# That suggests that the apparent dietary-breadth association in the simpler model was at least partly related to taxonomic clustering. Certain families may simultaneously differ in dietary breadth and baseline extinction risk.


#RESULTS OF SUMMARY MODFAM BELOW

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: binomial  ( logit )
# Formula: TNT ~ DBC + HBC + (1 | `Family HBW/BirdLife v9.1 (2024)`)
# Data: df
# 
# AIC       BIC    logLik -2*log(L)  df.resid 
# 1115.3    1136.2    -553.7    1107.3      1372 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.3908 -0.4264 -0.3286 -0.2283  4.5500 
# 
# Random effects:
#   Groups                          Name        Variance Std.Dev.
# Family HBW/BirdLife v9.1 (2024) (Intercept) 0.83     0.911   
# Number of obs: 1376, groups:  Family HBW/BirdLife v9.1 (2024), 94
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.93997    0.16005 -12.121  < 2e-16 ***
#   DBC          0.10727    0.09553   1.123    0.261    
# HBC         -0.35086    0.07244  -4.844 1.28e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) DBC   
# DBC  0.049       
# HBC  0.144 -0.140
