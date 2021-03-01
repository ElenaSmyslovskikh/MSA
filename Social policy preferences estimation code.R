rm(list=ls())
options(digits=2)

#___________________________________________________________________________________
# PREPROCESS THE DATA

# Upload World Values Survey 6 wave
# df <- readRDS(file.choose())

# Merge the dataset with country-level characteristics: 
# Income Group (World Bank estimation) and HDI 
# (other indicators can be added here, matched by country index in the codebook)

# countries <- read.csv("HDI IncGroup column.csv")
# df <- merge(cbind(data, X=rownames(data)), cbind(countries, variable=rownames(countries)))
# saveRDS(df, file="data.Rda")

df <- readRDS(file.choose()) # read the prepared data
#rm(data, countries)



# Negative values are to ve recoded as NA-s due to the specifics of the WVS data. 
df[df < 0] <- NA


# Choosing the variables to work with
library(tidyverse)
df <- df %>% select(HDI, IncGroup, SustComp, NatCap, SocCap, IntelCap, Governance, ResIntens, 
              V2, V242, V238, V248, V230, V25:V35, V198, V199, V201,
              V101, V98, V96, V95, 
              V224, V226, V227, V229, V239, V253, V213, V214, V137)

#___________________________________________________________________________________
# PART 1. Constructing variables

df$social.policy.pref <- rowSums(subset(df, select= c(V101, V98, V96, V95), na.rm = TRUE))/4
hist(df$social.policy.pref, breaks = 50)


# calculate the sum 'points' a respondent gets for active membership in associations
subset <- subset(df, select=c(V25:V35))
df$membership <- rowSums(subset, na.rm = TRUE)


# calculate the relative involvement of a respondent in associations as a mean of all rows
library(plyr)

membership_mean <- aggregate(df$membership, list(df$V2), mean, na.rm = TRUE)
membership_mean <- rename(membership_mean, c("Group.1" = "V2"))

df <- plyr::join_all(list(df, membership_mean), 
                     by = "V2", type = "left", match = "all")
df$membership <- df$membership/df$mean_assoc_country

# now our variable shows the degree to which respondent's activity exceeds the mean activity rate for the country


# calculate mean 'freerider' value
df$freerider <- rowSums(subset(df, select= c(V198, V199, V201), na.rm = TRUE))/3
df$freerider <- (df$freerider - 10)*(-1) 
# the higher the value, the higher is respondent's approval of 'freerider' actions

# recode variables, so that higher values correspond with higher indicators
df$V226 <- (df$V226 - 3)*(-1)
df$V227 <- (df$V227 - 3)*(-1)
df$V213 <- (df$V213 - 5)*(-1)
df$V214 <- (df$V214 - 5)*(-1)
df$V238 <- (df$V238 - 6)*(-1)
df$V224 <- (df$V224 - 6)*(-1)

df <- df %>% select(HDI, IncGroup, SustComp, NatCap, SocCap, IntelCap, Governance, ResIntens, 
                    V2, V242, V238, V248, V230, membership, freerider,
                    social.policy.pref, 
                    V239, V253, V137)

# Rename individual-level variables
df <- df %>% 
  rename(
    country = V2,
    #vote.local = V226,
    #vote.national = V227,
    #empl.status = V229,
    scale.income = V239,
    size.town = V253,
    age = V242,
    class = V238,
    education = V248,
    empl.sector = V230,
    #attitude.wealth.accum = V101,
    #attitude.gov.respons = V98,
    #attitude.income.equality = V96,
    #self.pos.polscale = V95,
    #justif.fare.avoid = V199,
    #justif.gov.benefits = V198,
    #justif.cheating.taxes = V201,
    #self.part.local = V213,
    #self.part.national = V214
  )

# Recode variables to reorder them according to World Bank estimations
# Now "1" stands for low" and "4" stands for "high"
df$IncGroup <- as.factor(df$IncGroup)

library(plyr)
df$IncGroup <- as.numeric(revalue(as.character(df$IncGroup), 
                             c('L' = '1', 'LM' = '2', 'UM' = '3', 'H' = '4')))

# Let us see the distribution of observations by country. 
# The mean number of observations is between 1000 and 2500, but there are outliers:
# India (4078), South Africa (3531), New Zealand (841)
table(df$country)
hist(table(df$country), breaks = 20)

# We want the name of country as a factor, recode it
df$country <- as.factor(df$country)

# Descriptive statistics
library(stargazer)
stargazer(df, digits = 2)

#___________________________________________________________________________________
# PART 2. Pre-analysis

# Let us see mean values of the dependent variable (constructed mean value of social policy preference)
# by country.
soc.pol.pref <- aggregate(df$social.policy.pref, list(df$country), mean, na.rm = TRUE)
hist(soc.pol.pref$x, breaks = 20)

library(psych)
describe(soc.pol.pref$x)

# Haiti is an outlier with the mean value 3.0
# In other countries, values range from 4.3 to 6.8

# How much variation is there at the country-level? Calculate the ICC
library(multilevel)
anovaicc <- aov(social.policy.pref ~ country, df)
summary(anovaicc)
ICC1(anovaicc)
# ICC = 0.19
# Almost 20% of variation lies at the country-level, so ME-model is relevant

# See the differences in starting points in different countries for the 
# individual-level variables

ggplot(df, aes(x=membership, y=social.policy.pref, color=country)) +
  geom_smooth(method=lm, se=FALSE)

ggplot(df, aes(x=freerider, y=social.policy.pref, color=country)) +
  geom_smooth(method=lm, se=FALSE)

# We see different starting conditions, as well as different relatioship

#___________________________________________________________________________________
# PART 3. Modeling

dat <- na.omit(df)

# Step 1. Test the null-model
library(lme4)
null <- lmer(social.policy.pref ~ 1 + (1|country), REML = FALSE, data = dat)
summary(null)

# Step 2. Add fixed-effect for the individual-level predictor
model1 <- lmer(social.policy.pref ~ freerider + (1|country), REML = FALSE, 
               data = dat)
summary(model1)

# Calculate random effects
ranef(model1)
library(lattice)
dotplot(ranef(model1, condVar=TRUE))

# Confidence interaval covers zero only in a few countries, 
# almost all othre countries show a significant random effect

anova(null, model1)
# Anova-test votes for extended model

# Step 3. Add the rest of the predictors and control variables

library(nlme)
null <- lme(social.policy.pref ~ 1, random = ~1|country, method = "ML", data = dat)

lmm1 <- lme(social.policy.pref ~ freerider + membership + 
              scale.income +
              size.town +
              age + 
              class +
              education +
              empl.sector,
            random = ~1|country, method = "ML", data = dat)

lmm2 <- lme(social.policy.pref ~ freerider + membership + 
              scale.income +
              size.town +
              age + 
              class +
              education +
              empl.sector,
            random = ~1|HDI, method = "ML", data = dat)

lmm3 <- lme(social.policy.pref ~ freerider + membership + 
              scale.income +
              size.town +
              age + 
              class +
              education +
              empl.sector,
            random = ~1|SocCap, method = "ML", data = dat)

lmm4 <- lme(social.policy.pref ~ freerider + membership + 
              scale.income +
              size.town +
              age + 
              class +
              education +
              empl.sector,
            random = ~1|IncGroup, method = "ML", data = dat)

anova(null, lmm1)
anova(lmm1, lmm2)
anova(lmm1, lmm3) # RE for country and SocCap are best predictors
anova(lmm3, lmm4)

stargazer(lmm3)

# adding interaction terms improves the model insignificantly, 
# so we won't spend degrees of freedom with no need
lmm.5 <- lme(social.policy.pref ~ freerider + membership + 
              scale.income +
              size.town +
              age + 
              class +
              education +
              empl.sector + membership*SocCap,
            random = ~1|SocCap, method = "ML", data = dat)

# here the effect is significant
lmm.6 <- lme(social.policy.pref ~ freerider + membership + 
               scale.income +
               size.town +
               age + 
               class +
               education +
               empl.sector + freerider*SocCap,
             random = ~1|SocCap, method = "ML", data = dat)

anova(lmm3, lmm.6)

stargazer(lmm.6)

altern <- lme(V137 ~ freerider + membership + 
                         scale.income +
                         size.town +
                         age + 
                         class +
                         education +
                         empl.sector + freerider*SocCap,
                       random = ~1|SocCap, method = "ML", data = dat)
stargazer(altern)


