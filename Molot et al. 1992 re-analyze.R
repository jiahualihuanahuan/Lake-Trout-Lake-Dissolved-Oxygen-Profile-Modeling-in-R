setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Molot et al. 1992 CJFAS paper follow up analysis/data")

# Load long term mean data for lake morphometry, TP, Chlorophyll a, DOC, Secchi etc. for predicting Zox
# lakes <- read.csv("Molot et al. 1992 lakes.csv", header=T)
# colnames(lakes)
# str(lakes)

# correlation test
# cor(lakes[c(1:8),c(3:15)], use="pairwise.complete.obs")

# reciprocal transformation
# lakes.recip <- 1/lakes[,c(3:15)]
# write.csv(lakes.recip, file="my.file.csv")

# log transformation
# lakes.log <- log(lakes[,c(3:15)])
# write.csv(lakes.log, file="my.file.csv")

# square transformation
# lakes.square <- lakes[,c(3:15)]^2
# write.csv(lakes.square, file="my.file.csv")

# square root transformation
# lakes.sqrt <- sqrt(lakes[,c(3:15)])
# write.csv(lakes.sqrt, file="my.file.csv")

# Molot et al. 1992 lakes with transformations
lakes.trans <- read.csv("Molot et al. 1992 lakes with transformations.csv", header=T)
colnames(lakes.trans)
head(lakes.trans, 8)

# Check normality

# colnames(lakes.trans)
# for (i in 1:65){
	# test <- shapiro.test(lakes.trans[,i+2])
# print(test)
# }

# shapiro.test(lakes.trans[,20])


# hist(lakes.trans[,42])


# Model development using A lakes

# # # # # # model in Molot et al. 1992
Zox.l <- lm(lakes.trans[c(1:8),]$Zox ~ DOC.recip, data=lakes.trans[c(1:8),])
summary(Zox.l)

# # # # # # Multiple Linear Regression with Zox (the upper most depth exhibiting some DO depletion)
colnames(lakes.trans)
library(packfor)
forward.sel(lakes.trans[c(1:8),]$Zox, lakes.trans[c(1:8),-c(1:2,12:14,25:27,38:41,51:53,64:66,11,24,37,50,63)],alpha=0.05, nperm=99999) 

# 19 is TP_epi_recip; ,11,24,37,50,63 are Zox;1:2,12:15,25:28,38:41,51:54,64:67 are NA and lake names/class

# # # # # # # new model
Zox.l <- lm(lakes.trans[c(1:8),]$Zox ~ TP_so.recip, data= lakes.trans[c(1:8),])
summary(Zox.l)

# parameters in final model
coefficients(Zox.l)
confint(Zox.l, level=0.95)
fitted(Zox.l)
residuals(Zox.l)
anova(Zox.l)
vcov(Zox.l)
influence(Zox.l)
# Diagnostic plots
layout(matrix(c(1,2,3,4),2,2))
plot(Zox.l)

#--------------------------------------------------------------------
# Graphic check of new model of Zox
lakes.trans <- read.csv("Molot et al. 1992 lakes with transformations.csv", header=T)
colnames(lakes.trans)

Zox.pred <- c()
Zox.pred <- -2.501 + 90.124*lakes.trans[1:8,]$TP_so.recip

plot(Zox.pred, lakes.trans[1:8,11], xlab="Zox predicted (m)", ylab="Zox observed (m)")
obs.vs.pred <- lm(lakes.trans[1:8,11] ~ Zox.pred)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(10,12,label="1:1 line", col="red")

# Validation of Zox using B lakes (new model)
lakes.trans <- read.csv("Molot et al. 1992 lakes with transformations.csv", header=T)
colnames(lakes.trans)
lakes.trans[9:16,] # B lakes

Zox.pred <- c()
Zox.pred <- -2.501 + 90.124*lakes.trans[9:16,]$TP_so.recip 

# plot(Zox.pred, lakes.trans[9:16,11], xlab="Zox predicted (m)", ylab="Zox observed (m)")
# obs.vs.pred <- lm(lakes.trans[9:16,11] ~ Zox.pred)
# abline(obs.vs.pred)
# abline(0,1, col="red")
# summary(obs.vs.pred)
# text(11,8,label="1:1 line", col="red")
cor(lakes.trans[9:16,11], Zox.pred)
#--------------------------------------------------------------------


#---------------------------------------------------------------------
# Spring DO model
# Load profile data
# Molot et al. 1992 profile data with transformations
profiles.trans <- read.csv("Molot et al. 1992 profiles with transformations.csv", header=T)
colnames(profiles.trans)
head(profiles.trans, 2)

# # # # # # model in Molot et al. 1992
Spring.DO.l <- lm(profiles.trans[1:53,]$DOi ~ A0.recip + Depth, data=profiles.trans[1:53,])
summary(Spring.DO.l)

########### Develop new DOi model
library(packfor)
colnames(profiles.trans)
forward.sel(profiles.trans[1:53,]$DOi, profiles.trans[1:53,-c(1,3,7,11,15,19,4,8,12,16,20)], alpha=0.05, nperm=9999) # 1 is lake, 3,6,9,12 are DOi


# Final Spring DO model by Molot et al. 1992
Spring.DO.l <- lm(profiles.trans[1:53,]$DOi~A0.recip + Depth, data=profiles.trans[1:53,])
summary(Spring.DO.l)

# Final model by forward selection
Spring.DO.l <- lm(profiles.trans[1:53,]$DOi ~ A0.recip + Depth + TP.epi.square, data=profiles.trans[1:53,])
summary(Spring.DO.l)
#---------------------------------------------------------------------------
# Validate Spring DO model within A lakes
profiles.trans <- read.csv("Molot et al. 1992 profiles with transformations.csv", header=T)
colnames(profiles.trans)
profiles.trans[1:53,] # B lakes

DOi.pred <- 12.501802-80.969690* profiles.trans[1:53,]$A0.recip - 0.119693*profiles.trans[1:53,]$Depth - 0.012763*profiles.trans[1:53,]$TP.epi.square

DOi.obs <- profiles.trans[1:53,3]

plot(DOi.pred, DOi.obs, xlab="DOi predicted (mg/L)", ylab="DOi observed (mg/L)")
obs.vs.pred <- lm(DOi.obs ~ DOi.pred)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(7,8,label="1:1 line", col="red")


#---------------------------------------------------------------------------
# New spring DO model validation on B lakes
profiles.trans <- read.csv("Molot et al. 1992 profiles with transformations.csv", header=T)
colnames(profiles.trans)
rownames(profiles.trans)
head(profiles.trans)
profiles.trans[,c(1,2)]

profiles.trans[54:106,] # B lakes

DOi.pred <- 12.501802-80.969690* profiles.trans[54:106,]$A0.recip - 0.119693*profiles.trans[54:106,]$Depth - 0.012763*profiles.trans[54:106,]$TP.epi.square

DOi.obs <- profiles.trans[54:106,3]
cor(DOi.pred, DOi.obs)

plot(DOi.pred, DOi.obs, xlab="DOi predicted (mg/L)", ylab="DOi observed (mg/L)")
obs.vs.pred <- lm(DOi.obs ~ DOi.pred)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(7,8,label="1:1 line", col="red")


#---------------------------------------------------------------------------
# Spring DO model validation on B lakes Molot 1992???

profiles.trans <- read.csv("Molot et al. 1992 profiles with transformations.csv", header=T)
colnames(profiles.trans)
profiles.trans[54:106,] # B lakes

DOi.pred <- 8.823320-47.279906* profiles.trans[54:106,29] - 0.071018*profiles.trans[54:106,2] + 1.897017*profiles.trans[54:106,10] + 3.155465*profiles.trans[54:106,8]

DOi.obs <- profiles.trans[54:106,3]

plot(DOi.pred, DOi.obs, xlab="DOi predicted (mg/L)", ylab="DOi observed (mg/L)")
obs.vs.pred <- lm(DOi.obs ~ DOi.pred)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(7,6,label="1:1 line", col="red")
#---------------------------------------------------------------------------

# Model end of summer DO profile

# Molot et al. 1992 profile data with transformations
profiles.trans <- read.csv("Molot et al. 1992 profiles with transformations.csv", header=T)
colnames(profiles.trans)
head(profiles.trans, 8)


# All included
DO.summer.l <- lm(profiles.trans$DOf ~., profiles.trans[,-c(1,5,9,13,17)])
summary(DO.summer.l)


#---------------------------------------------------------------------------
# Model 1 from Molot: logDOf ~ 1/VSA +1/DOi + TP^2 (epi)
DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf.log ~ VSA.recip + DOi.recip + TP.epi.square, profiles.trans[c(1:53),])
summary(DO.summer.l)

#---------------------------------------------------------------------------
# Model 2 from Molot: logDOf ~ 1/VSA +1/DOi + TP_so^2
DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf.log ~ VSA.recip + DOi.recip + TP_so.square, profiles.trans[c(1:53),])
summary(DO.summer.l)

#---------------------------------------------------------------------------
# Validation using Model 1 from Molot et al. 1992 
profiles.trans <- read.csv("Molot et al. 1992 profiles with transformations.csv", header=T)
colnames(profiles.trans[c(54:106),])

profiles.trans[c(54:106),] # B lakes

DOf.pred <- 10^(1.8305336-(1.9055824*profiles.trans[c(54:106),8])-(7.0614672*profiles.trans[c(54:106),6])-(0.0012680*profiles.trans[c(54:106),25]))

DOf.obs <- profiles.trans[c(54:106),4]

plot(DOf.pred, DOf.obs, xlab="DOf predicted (mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,9,label="1:1 line", col="red")

#---------------------------------------------------------------------------
# Validation using Model 2 from Molot et al. 1992 
profiles.trans <- read.csv("Molot et al. 1992 profiles with transformations.csv", header=T)
colnames(profiles.trans[c(54:106),])

profiles.trans[c(54:106),] # B lakes

DOf.pred <- 10^(1.8847238-(1.8835974*profiles.trans[c(54:106),8])-(7.2939025*profiles.trans[c(54:106),6])-(0.0026820*profiles.trans[c(54:106),24]))

DOf.obs <- profiles.trans[c(54:106),4]

plot(DOf.pred, DOf.obs, xlab="DOf predicted (mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,9,label="1:1 line", col="red")

#---------------------------------------------------------------------------
colnames(profiles.trans)
library(packfor)

# Develop new model for Fall DO profile
# final model
# 1 DOf
forward.sel(profiles.trans[c(1:53),]$DOf, profiles.trans[c(1:53),-c(1,4,8,12,16,20, 5,9,17,21, 3,7,11,19, 2,10,14,18)], alpha=0.05, nperm=9999)

DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf ~ VSA.log + DOi.square + A0 + TP.epi.sqrt + Depth.recip, profiles.trans[c(1:53),])
summary(DO.summer.l)

# 2 DOf.log
forward.sel(profiles.trans[c(1:53),]$DOf.log, profiles.trans[c(1:53),-c(1,2,4,8,12,16,20, 13,5,17,21, 11,22:26,32:36)], alpha=0.05, nperm=9999)

colnames(profiles.trans)

# # stepwise regression
# library(MASS)
# fit <- lm(profiles.trans[c(1:53),]$DOf.log ~.,data=profiles.trans[c(1:53),-c(1,2,4,8,12,16,20)])
# step <- stepAIC(fit, direction="forward")

# step$anova

# forward
# backward
# both

# ?stepAIC

# select 1
DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf.log ~ VSA.log + DOi.log, profiles.trans[c(1:53),])
summary(DO.summer.l)

# select 2: epi TP
DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf.log ~ VSA.recip + DOi.sqrt + TP.epi.square, profiles.trans[c(1:53),])
summary(DO.summer.l)

# select 3
DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf.log ~ VSA.recip + DOi.recip + TP.epi.square, profiles.trans[c(1:53),])
summary(DO.summer.l)

# select 4 spring overturn TP
DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf.log ~ VSA.recip + DOi.sqrt + TP_so.log, profiles.trans[c(1:53),])
summary(DO.summer.l)

3 DOf.recip
colnames(profiles.trans)
forward.sel(profiles.trans[c(1:53),]$DOf.recip, profiles.trans[c(1:53),-c(1,2,4,8,12,16,20, 5, 13, 17,21,3,11,15,19,32:36,6,10,14,18)], alpha=0.1, nperm=9999)

DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf.recip ~ VSA.recip + DOi.recip, profiles.trans[c(1:53),])
summary(DO.summer.l)

#4 DOf.square xxxxxx
colnames(profiles.trans)
forward.sel(profiles.trans[c(1:53),]$DOf.square, profiles.trans[c(1:53),-c(1,2,4,8,12,16,20)], alpha=0.05, nperm=9999)

DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf.square ~ VSA.sqrt + DOi.square + A0.log, profiles.trans[c(1:53),])
summary(DO.summer.l)

5 DOf.sqrt
colnames(profiles.trans)
forward.sel(profiles.trans[c(1:53),]$DOf.sqrt, profiles.trans[c(1:53),-c(1,2,4,8,12,16,20 , 5,9,17,21)], alpha=0.05, nperm=9999)

DO.summer.l <- lm(profiles.trans[c(1:53),]$DOf.sqrt ~ VSA.log + DOi + A0.square, profiles.trans[c(1:53),])
summary(DO.summer.l)

#---------------
# Validate on A lakes
profiles.trans[c(1:53),] # A lakes
colnames(profiles.trans)
#---------------
# 1 DOf~VSA+DOi+TPepi
DOf.pred <- 10^(-2.1766274-1.8019688*profiles.trans[c(1:53),9] + 1.1129920*profiles.trans[c(1:53),19]-0.0034081*profiles.trans[c(1:53),24])

DOf.obs <- profiles.trans[c(1:53),4]
cor(DOf.obs, DOf.pred)

plot(DOf.pred, DOf.obs, xlab="DOf predicted (mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(4,2,label="1:1 line", col="red")
#---------------
# 2 DOf~VSA+DOi+TPso
DOf.pred <- 10^(-1.60688-1.85064*profiles.trans[c(1:53),9] + 1.04838*profiles.trans[c(1:53),19]-0.59656*profiles.trans[c(1:53),30])

DOf.obs <- profiles.trans[c(1:53),4]
cor(DOf.obs, DOf.pred)

plot(DOf.pred, DOf.obs, xlab="DOf predicted (mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(4,2,label="1:1 line", col="red")
#---------------
# Validate on B lakes
profiles.trans[c(54:106),] # B lakes
#---------------
# 1 DOf~VSA+DOi+TPepi
DOf.pred <- 10^(-2.1766274-1.8019688*profiles.trans[c(54:106),9] + 1.1129920*profiles.trans[c(54:106),19]-0.0034081*profiles.trans[c(54:106),24])

DOf.obs <- profiles.trans[c(54:106),4]
cor(DOf.obs, DOf.pred)

plot(DOf.pred, DOf.obs, xlab="DOf predicted (mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,9,label="1:1 line", col="red")
#---------------
# 2 DOf~VSA+DOi+TPso
DOf.pred <- 10^(-1.60688-1.85064*profiles.trans[c(54:106),9] + 1.04838*profiles.trans[c(54:106),19]-0.59656*profiles.trans[c(54:106),30])

DOf.obs <- profiles.trans[c(54:106),4]
cor(DOf.obs, DOf.pred)

plot(DOf.pred, DOf.obs, xlab="DOf predicted (mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,9,label="1:1 line", col="red")
#---------------







# coefficients(DO.summer.l)
# confint(DO.summer.l, level=0.95)
# fitted(DO.summer.l)
# residuals(DO.summer.l)
# anova(DO.summer.l)
# vcov(DO.summer.l)
# influence(DO.summer.l)

# # Diagnostic plots
# layout(matrix(c(1,2,3,4),2,2))
# plot(DO.summer.l)

# # Cross validation: k-fold cross-validation ?????
# library(DAAG)
# cv.lm(df=profiles.trans[c(1:53),], DO.summer.l, m=3)

# # Calculate relative importance for each predictor
# library(relaimpo)
# calc.relimp(DO.summer.l, type=c("lmg", "last", "first", "pratt"), rela=T)
# # Bootstrap measures of relative importance 
# boot <- boot.relimp(DO.summer.l, b=1000, type=c("lmg", "last", "first", "pratt"), rank = T, diff=T, rela=T)
# booteval.relimp(boot)
# plot(booteval.relimp(boot, sort=T))


# AHOD <- DO[complete.cases(DO[,c(7)]),c(1,7)] 
# V <- DO[complete.cases(DO[,c(6)]),c(1,6)][c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31),]

# #cor(AHOD$AHOD, lakes$Secchi[c(1:16)], method="pearson")
# library(Hmisc)

# cor(lakes[complete.cases(lakes[,c(11)]),c(3:12)])
# rcorr(Lakes, type="pearson")
# Lakes <- cbind(lakes[c(1:16),c(3:12)])

# # Multiple Linear Regression
# fit <- lm(DO$DOf ~ DO$DOi + DO$VSA)
# summary(fit)



# library(Hmisc)






