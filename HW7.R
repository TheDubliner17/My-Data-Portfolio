# Import dataset
HW7Data <- read.csv("C:/Users/themi/Downloads/HW7Data.csv")

# Null model
library(lmerTest)
null <- lmer(popularity ~ 1+(1|teacher_experience), HW7Data, REML=F)
summary(null)

# Calculate ICC
ICC <- .2887/(.2887+1.2148)
ICC

# Random effects ANCOVA model
PE_grand <- HW7Data$pupil_extraversion - mean(HW7Data$pupil_extraversion)
PE_grand
RE_model <- lmer(popularity ~ 1 + PE_grand + (1|schoolID), HW7Data,
                 REML=F)
summary(RE_model)

# Random coefficients model
RC_model <- lmer(popularity ~ 1 + PE_grand + (PE_grand|schoolID), 
                 HW7Data, REML=F)
summary(RC_model)

# Slopes and intercepts as outcomes mdoel
SIO_model <- lmer(popularity ~ 1 + PE_grand + teacher_experience +
                  PE_grand*teacher_experience + (PE_grand|schoolID),
                  HW7Data, REML=F)
summary(SIO_model)
