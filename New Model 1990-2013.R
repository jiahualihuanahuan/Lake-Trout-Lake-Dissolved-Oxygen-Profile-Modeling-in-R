# Develop new models based on data from 1990-2013
#------------------------------------------------
# 1. Zox
# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")

LAKE <- read.csv("Zox model development 1990-2013.csv")
colnames(LAKE)
LAKE[,1]
A <- LAKE[1:8,]
B <- LAKE[9:16,]
AB <- LAKE[1:16,]
C <- LAKE[17:33,]
# forward selection
library(packfor)
colnames(A)
# ?forward.sel

# Check old model by Molot et al. 1992
# A lakes
Zox.pred <- 1.52 + 28.1*(1/A$DOC)
Zox.obs <- A$Zox
cor(Zox.obs, Zox.pred) # r= 0.9359297
# B lakes
Zox.pred <- 1.52 + 28.1*(1/B$DOC)
Zox.obs <- B$Zox
cor(Zox.obs, Zox.pred) # r= 0.8069274

# A and B lakes
Zox.pred <- 1.52 + 28.1*(1/AB$DOC)
Zox.obs <- AB$Zox
cor(Zox.obs, Zox.pred) # r= 0.8717854

plot(Zox.obs, Zox.pred, xlim=c(0,16), ylim=c(0,16),xlab="observed Zox", ylab="predicted Zox")
abline(0,1, col="red")
text(8,6,label="1:1 line", col="red")

# Build similar model but different coefficient
Zox.l <- lm(A$Zox ~ DOC_recip, data=A)
summary(Zox.l)

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.3513     1.3454   0.261 0.802740    
# DOC_recip    26.8944     4.1316   6.509 0.000626 ***
# R2adj=0.8553, p= 0.0006263

#---------
# Zox model validation on B and C lakes
pred.Zox.A <- 0.3513 + 26.8944*A$DOC_recip
pred.Zox.B <- 0.3513 + 26.8944*B$DOC_recip
pred.Zox.AB <- 0.3513 + 26.8944*AB$DOC_recip
obs.Zox.A <- A$Zox
obs.Zox.B <-B$Zox
obs.Zox.AB <-AB$Zox

cor(pred.Zox.A, obs.Zox.A) # r= 0.9359297
cor(pred.Zox.B, obs.Zox.B) # r= 0.8069274
cor(pred.Zox.AB, obs.Zox.AB) # r= 0.8717854

# Check predicted and observed end of Zox: A lakes
plot(pred.Zox.A, obs.Zox.A, xlab="Zox predicted (m)", ylab="Zox observed (m)")
obs.vs.pred <- lm(obs.Zox.A ~ pred.Zox.A)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,6,label="1:1 line", col="red")


# Check predicted and observed Zox: B lakes
plot(pred.Zox.B, obs.Zox.B, xlab="Zox predicted (m)", ylab="Zox observed (m)")
obs.vs.pred <- lm(obs.Zox.B ~ pred.Zox.B)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,6,label="1:1 line", col="red")

# Check predicted and observed Zox: A and B lakes
plot(pred.Zox.AB, obs.Zox.AB, xlab="Zox predicted (m)", ylab="Zox observed (m)")
obs.vs.pred <- lm(obs.Zox.AB ~ pred.Zox.AB)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,6,label="1:1 line", col="red")
#---------
# Build new model based on data A lakes 1990-2013; Forward Selection
colnames(A)
forward.sel(A$Zox, A[,-c(1,2,7, 18,19,21,22,24,25,27,28,30,31,33,34,36,37,39,40,42,43,45,46,48,49,51,52,54,55,57,58,60,61,63,64,66,67,69,70,72,73,75,76,78,79,81,82,84,85,87,88,90,91)], alpha=0.05, nperm=9999) # Hypolimnion
# hypo N only
Zox.l <- lm(A$Zox ~ DOC_recip + NH4.H.recip, data=A)
summary(Zox.l)

# Zox=-1.15+28.5/DOC+28.7/NH4_H
# R2adj= 0.9924, p= 3.49e-05

# all strata N data
forward.sel(A$Zox, A[,-c(1,2,7)], alpha=0.05, nperm=9999) # all N data
Zox.l <- lm(A$Zox ~ DOC_recip + NH4.H.recip + TN.E.sqr, data=A)
summary(Zox.l)

# Zox=-13.52+47.69/DOC+35.65/NH4_H + 0.0001*TN_E
# R2adj= 0.9924, p= 3.49e-05
#------------------------------------------------
#----------------------------------------------
#-----------------------------------------------
#------------------------------------------------
#------------------------------------------------
# Spring DO (DOi)
# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")
LAKE <- read.csv("DO profiles model development 1990-2013.csv")
colnames(LAKE)
rownames(LAKE)
LAKE[,1]
A <- LAKE[1:53,]
B <- LAKE[54:106,]
AB <- LAKE[1:106,]
colnames(A)
rownames(A)

# ### exam the model from Molot et al. 1992: DOi = 11.2 - 70.9/A0 - 0.092*z
# # A lakes
DOi.predicted <- 11.16414 - 71.22101/A$A0 - 0.09177*A$Depth
DOi.observed <- A$DOi
cor(DOi.observed, DOi.predicted) # r= 0.8372859
# B lakes
DOi.predicted <- 11.16414 - 71.22101/B$A0 - 0.09177*B$Depth
DOi.observed <- B$DOi
cor(DOi.observed, DOi.predicted) # r= 0.9136791
# # A and B lakes
DOi.predicted <- 11.16414 - 71.22101/AB$A0 - 0.09177*AB$Depth
DOi.observed <- AB$DOi
cor(DOi.observed, DOi.predicted) # r= 0.8909483


plot(DOi.observed, DOi.predicted, xlim=c(0,12), ylim=c(0,12),xlab="observed DOi", ylab="predicted DOi")
abline(0,1, col="red")
text(8,6,label="1:1 line", col="red")

# ### exam the model from Molot et al. 1992: log10(DOi) (MD<1.4km) = 0.99 - 5.74/A0 +0.64/z
# # A lakes
# DOi.predicted <- 10^(0.99 - 5.74/A[-c(25:36),]$A0 - 0.64/A[-c(25:36),]$Depth)
# DOi.observed <- A[-c(25:36),]$DOi
# cor(DOi.observed, DOi.predicted) # r= 0.6590213

# # B lakes: without Bigwind
# B[,1]
# DOi.predicted <- 10^(0.99 - 5.74/B[-c(10:21),]$A0 - 0.64/B[-c(10:21),]$Depth)
# DOi.observed <- B[-c(10:21),]$DOi
# cor(DOi.observed, DOi.predicted) # r= 0.86535

# # A and B lakes
# AB[,1]
# DOi.predicted <- 10^(0.99 - 5.74/AB[-c(25:36,63:74),]$A0 - 0.64/AB[-c(25:36,63:74),]$Depth)
# DOi.observed <- AB[-c(25:36,63:74),]$DOi
# cor(DOi.observed, DOi.predicted) # r= 0.8150268

# # Use the same predictive variables and build new Spring DO model using data 1990-2013
# DOi.l <- lm(A$DOi ~ A0_recip + Depth, data=A)
# summary(DOi.l)
# # R2adj= 0.7304, p= 2.207e-15
#--------------------
# Forward Selection using forward.sel function
colnames(A)
# forward selection
library(packfor)
############ 1.1 DOi：GOOOD
colnames(A)
forward.sel(A$DOi, A[,-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10,32,33, 7,13,19,24,29,40:42,49:51,58:60,67:69,76:78)], alpha=0.05, nperm=9999)
# A0_recip, Depth, NH4.H.log, TON.E.recip
# R2adj= 0.8858065

# model by Molot et al. 1992
DOi = 11.2 - 70.9/A0 - 0.092*z

# 1.1.1 only A0 and Z
DOi.l <- lm(A$DOi ~ A0_recip + Depth, data=A)
summary(DOi.l)

DOi = 10.74298 - 86.07214/A0 - 0.06166*z

# Coefficients:
             # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  10.74298    0.36990  29.043  < 2e-16 ***
# A0_recip    -86.07214    7.26684 -11.845    4e-16 ***
# Depth        -0.06166    0.01687  -3.655 0.000617 ***

# Residual standard error: 0.8305 on 50 degrees of freedom
# Multiple R-squared:  0.7407,	Adjusted R-squared:  0.7304 
# F-statistic: 71.42 on 2 and 50 DF,  p-value: 2.207e-15


# 1.1.1 all A0, Z, N variables
DOi.l <- lm(A$DOi ~ A0_recip + Depth + NH4.H.log + TON.E.recip, data=A)
summary(DOi.l)

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  12.2041     0.5024  24.293  < 2e-16 ***
# A0_recip    -63.3881     5.9120 -10.722 2.46e-14 ***
# Depth        -0.1280     0.0136  -9.410 1.79e-12 ***
# NH4.H.log    -1.7389     0.2180  -7.976 2.40e-10 ***
# TON.E.recip 349.1631    92.6996   3.767 0.000452 ***

# Residual standard error: 0.5405 on 48 degrees of freedom
# Multiple R-squared:  0.8946,	Adjusted R-squared:  0.8858 
# F-statistic: 101.8 on 4 and 48 DF,  p-value: < 2.2e-16
#--------------------
# Validation DOi model on B lakes: test above model on B lakes (only the first model applies because we do not have N data for B lakes)
# Model 1: DOi=?+?/A0+?z
# intercept 10.74298
# A0_recip -86.07214
# Depth -0.06166

B[,1]

DOi.predicted <- 10.74298 - 86.07214/B[,]$A0 - 0.06166/B[,]$Depth
DOi.observed <- B[,]$DOi
cor(DOi.observed, DOi.predicted) # r= 0.9087272

plot(DOi.observed, DOi.predicted)
abline(0,1,col="red")
hist(DOi.observed)
hist(DOi.predicted)

# Very good validation correlation coefficient r
#--------------------
# 1.2 DOi: without Harp Lake
colnames(LAKE)
LAKE[,1]
forward.sel(A[-c(25:36),]$DOi, A[-c(25:36),-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10,32,33, 7,13,19,24,29,40:42,49:51,58:60,67:69,76:78)], alpha=0.05, nperm=9999)
# A0_recip, Depth, NH4.H.log, TON.E.recip
# R2adj= 0.8973975

DOi.l <- lm(A[-c(25:36),]$DOi ~ A0_recip + Depth + NH4.H.log + TON.E.sqrt, data=A[-c(25:36),])
summary(DOi.l)
# DOi model build by A lakes but without Harp Lake
# DOi = 31.70212 -49.48666/A0 - 0.29586* sqrt(VSA) - 0.19050*Z + 7.84350*log(TKN)????
# R2adj= 0.8974, p=< 2.2e-16

# Validation on B lakes
DOi.B.pred <- 19.11943 - 63.54606*B$A0_recip - 0.16240*B$Depth - 2.00670*B$NH4.H.log - 0.29025*B$TON.E.sqrt
DOi.B.obs <- B$DOi 
cor(DOi.B.obs, DOi.B.pred) 
# r= 0.9393865

# DOi.B.pred <- 8.73993 - 83.20614*B[-c(10:21),]$A0_recip + 0.48584*B[-c(10:21),]$VSA_sqrt - 0.09081*B[-c(10:21),]$Depth + 6.68843*B[-c(10:21),]$TP_recip
# DOi.B.obs <- B[-c(10:21),]$DOi 
# cor(DOi.B.obs, DOi.B.pred) 
# # r= 0.876316
#--------------------
# 2.1 DOi_recip:
colnames(A) 
forward.sel(A$DOi_recip, A[,-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10)], alpha=0.05, nperm=9999)
# R2adj= 0.5395754
#--------------------
# 2.2 DOi: without Harp Lake: not very good
colnames(LAKE) 
LAKE[,1]
forward.sel(A[-c(25:36),]$DOi_recip, A[-c(25:36),-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10,32,33)], alpha=0.05, nperm=9999)
# A0_recip
# R2adj= 0.5357143

DOi.l <- lm(A[-c(25:36),]$DOi_log ~ A0_recip + VSA_sqrt, data=A[-c(25:36),])
summary(DOi.l)
# DOi model build by A lakes but without Harp Lake
# R2adj= 0.6862, p=1.031e-10
#--------------------	
# 3.1 DOi_log 
forward.sel(A$DOi_log, A[,-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10,32,33)], alpha=0.05, nperm=9999)
# R2adj= 0.7201487

# 3.2 DOi: without Harp Lake: not very good
colnames(LAKE) 
LAKE[,1]
forward.sel(A[-c(25:36),]$DOi_log, A[-c(25:36),-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10,32,33)], alpha=0.05, nperm=9999)
# A0_recip, VSA sqrt, Depth, TP_recip
# R2adj= 0.8024623

DOi.l <- lm(A[-c(25:36),]$DOi_log ~ A0_recip + VSA_sqrt, data=A[-c(25:36),])
summary(DOi.l)
# DOi model build by A lakes but without Harp Lake
# R2adj= 0.6862, p=1.031e-10
#--------------------
####### 4.1 DOi_square: GOOD
forward.sel(A$DOi_square, A[,-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10,29)], alpha=0.05, nperm=9999)
# R2adj= 0.8504881

DOi.l <- lm(A$DOi_square ~ A0_recip + VSA_log + Depth + TP_recip, data=A)
summary(DOi.l)
# R2adj= 0.8505, p< 2.2e-16

# 4.2 DOi_square: without Harp Lake:: GOOD
colnames(LAKE) 
LAKE[,1]
forward.sel(A[-c(25:36),]$DOi_square, A[-c(25:36),-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10,32,33)], alpha=0.05, nperm=9999)
# A0_recip, VSA sqrt, Depth, TP_recip
# R2adj= 0.8115048

DOi.l <- lm(A[-c(25:36),]$DOi_square ~ A0_recip + VSA_sqrt + Depth + TP_recip, data=A[-c(25:36),])
summary(DOi.l)
# DOi model build by A lakes but without Harp Lake
# R2adj= 0.8115, p=2.16e-13

# Validation on B lakes
DOi.B.pred <- sqrt(68.1647 - 925.1529*B$A0_recip + 22.9699*B$VSA_sqrt - 1.2346*B$Depth + 100.0945*B$TP_recip)
DOi.B.obs <- B$DOi 
cor(DOi.B.obs, DOi.B.pred) 
# r= 0.7880665

DOi.B.pred <- (69.6260 - 903.6227*B[-c(10:21),]$A0_recip + 7.2591*B[-c(10:21),]$VSA_sqrt - 1.3620*B[-c(10:21),]$Depth + 90.6916*B[-c(10:21),]$TP_recip)
DOi.B.obs <- B[-c(10:21),]$DOi 
cor(DOi.B.obs, DOi.B.pred, rm.na=T) 
# r= xxx
#--------------------
# 5.1 DOi_sqrt
forward.sel(A$DOi_sqrt, A[,-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10,32,33)], alpha=0.05, nperm=9999)
# A0_recip, VSA sqrt, Depth
# R2adj= 0.7776570

DOi.l <- lm(A$DOi_sqrt ~ A0_recip + VSA_sqrt + Depth, data=A)
summary(DOi.l)

# 5.2 DOi_sqrt: without Harp Lake
colnames(LAKE) 
LAKE[,1]
forward.sel(A[-c(25:36),]$DOi_square, A[-c(25:36),-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10,32,33)], alpha=0.05, nperm=9999)
# A0_recip, VSA sqrt, Depth, TP_recip
# R2adj= 0.8115048

DOi.l <- lm(A[-c(25:36),]$DOi_square ~ A0_recip + VSA_sqrt + Depth + TP_recip, data=A[-c(25:36),])
summary(DOi.l)
# DOi model build by A lakes but without Harp Lake
# R2adj= 0.8115, p=2.16e-13

# Validation on B lakes
DOi.B.pred <- (3.058911 - 17.443754*B$A0_recip + 0.099474*B$VSA_sqrt - 0.010454*B$Depth)^2
DOi.B.obs <- B$DOi 
cor(DOi.B.obs, DOi.B.pred) 
# r= 0.8020307

DOi.B.pred <- (69.6260 - 903.6227*B[-c(10:21),]$A0_recip + 7.2591*B[-c(10:21),]$VSA_sqrt - 1.3620*B[-c(10:21),]$Depth + 90.6916*B[-c(10:21),]$TP_recip)^2
DOi.B.obs <- B[-c(10:21),]$DOi 
cor(DOi.B.obs, DOi.B.pred) 
# r= 0.4840863
#---------------------------
#---------------------------
#---------------------------
# Compare predicted DOi in B lakes using model developed by data from A lakes
# new model 1
DOi.B.pred <- 8.69279 - 82.72090*B$A0_recip + 1.44704*B$VSA_log - 0.08206*B$Depth + 6.82540*B$TP_recip
DOi.B.obs <- B$DOi 
cor(DOi.B.obs, DOi.B.pred) # r= 0.9393865

# plot(DOi.B.pred, DOi.B.obs)
# abline(0,1,col="red")

# new model 2
DOi.B.pred <- sqrt(68.1647 - 925.1529*B$A0_recip + 22.9699*B$VSA_log - 1.2346*B$Depth + 100.0945*B$TP_recip)
DOi.B.obs <- B$DOi 
cor(DOi.B.obs[-c(38:42)], DOi.B.pred[-c(38:42)]) # r= 0.8352158

#######################################

# DOf model development
# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")
LAKE <- read.csv("DO profiles model development 1990-2013.csv")
colnames(LAKE)
rownames(LAKE)
LAKE[,1]
A <- LAKE[1:53,]
B <- LAKE[54:106,]
colnames(A)
rownames(A)

# use only when we exclude Harp Lake, Plastic and 5m of Crosson, Dickie & 13 m of RCM
LAKE.new <- LAKE[-c(14,22,25:40),]
A.new <- LAKE.new[1:35,]
B.new <- LAKE.new[36:85,]

# Check the old model
# Old model looks like this: DOf_log = 1.8305336 - 1.9055824*VSA_recip - 7.0614672*DOi_recip - 0.001268*TP_so_squre

# A lakes
DOf.pred <- 10^(1.8305336 - 1.9055824* A$VSA_recip - 7.0614672* A$DOi_recip - 0.001268* A$TP_square)
DOf.obs <- A$DOf
cor(DOf.obs, DOf.pred) # r= 0.9148744

# B lakes
DOf.pred <- 10^(1.8305336 - 1.9055824* B$VSA_recip - 7.0614672* B$DOi_recip - 0.001268* B$TP_square)
DOf.obs <- B$DOf
cor(DOf.obs, DOf.pred) # r= 0.808292

# A and B lakes
DOf.pred <- 10^(1.8305336 - 1.9055824* AB$VSA_recip - 7.0614672* AB$DOi_recip - 0.001268* AB$TP_square)
DOf.obs <- AB$DOf
cor(DOf.obs, DOf.pred) # r= 0.8365969

plot(DOf.obs, DOf.pred, xlim=c(0,11), ylim=c(0,11),xlab="observed DOf", ylab="predicted DOf")
abline(0,1, col="red")
text(8,6,label="1:1 line", col="red")

# Try the same kind of model but different estimate (i.e. DOf_log = ? + ?*VSA_recip - ?*DOi_recip - ?*TP_so_square)
# R2adj= 0.7665, p= 3.898e-16
DO.f.l <- lm(A$DOf_log ~ VSA_recip + DOi_recip +TP_square, data=A)
summary(DO.f.l)

###################
# New Model Development
#---------------------------------
# Forward Selection using forward.sel function
# 1 DOf
colnames(A)
forward.sel(A$DOf, A[,-c(1,2,6,12,18,23,28)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf ~ VSA_log + DOi_square + A0_sqrt + NO3.E.log, data=A)
summary(DO.f.l)
# DOf=-3.219549 + 2.796364*VSA_log + 0.091050*DOi_square - 0.562046*A0_sqrt + 2.857698*NO3.E.log
# R2adj= 0.9114, p< 2.2e-16

#---------------------------------
# 2 DOf_recip
colnames(A)
forward.sel(A$DOf_recip, A[,-c(1,2,6,12,18,23,28, 5,11,17,22, 7,13,24,29)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf_recip ~ DOi_sqrt + VSA_recip + A0_square + Depth + TKN.H.log, data=A)
summary(DO.f.l)
# DOf=19.07 - 3.951*DOi_sqrt + 3.337*VSA_recip + 0.0001966* A0_square - 0.07776*Depth + 3.06*TKN.H.log
# R2adj= 0.9305, p< 2.2e-16
#---------------------------------
#---------------------------------
# 3 DOf_log
colnames(A)
forward.sel(A$DOf_log, A[,-c(1,2,6,12,18,23,28, 72)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + NH4.H.sqrt + A0_sqrt + Depth + DOC, data=A)
summary(DO.f.l)
# log10(DOf)=-0.813729 + 0.026336*DOi_square - 0.838259*VSA_recip + 0.010159*NH4.H.sqrt + 0.125087* A0_sqrt + 0.026181*Depth + 0.054004*DOC
# R2adj= 0.9545, p< 2.2e-16
#---------------------------------
# DOf_log-----no A0
colnames(A)
forward.sel(A$DOf_log, A[,-c(1,2,6,12,18,23,28,3,5,11,17,27, 4,10,16,32,33, 7,13,24,29)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + NH4.H.sqrt + NO3.M.sqr + NH4.M.sqr, data=A)
summary(DO.f.l)
# log10(DOf)=0.1922 + 0.01571*DOi_square - 0.8125*VSA_recip - 0.07264* NH4.H.sqrt - 0.0000349* NO3.M.sqr + 0.00009586*NH4.M.sqr
# R2adj= 0.9283, p< 2.2e-16
#---------------------------------
# 4 DOf_sqrt:
colnames(A)
forward.sel(A$DOf_sqrt, A[,-c(1,2,6,12,18,23,28,4,33,10,16,29,7,24,19)], alpha= 0.07)
DO.f.l <- lm(A$DOf_sqrt ~ DOi_square + VSA_log + A0_square + NO3.E.recip + Depth + TKN.H.sqrt, data=A)
summary(DO.f.l)
# R2adj= 0.9299, p< 2.2e-16
#---------------------------------
# 5 DOf_square: 
forward.sel(A$DOf_square, A[,-c(1,2,6,12,18,23,28,5,27,11)], alpha= 0.1)
DO.f.l <- lm(A$DOf_square ~ VSA_sqrt + DOi_square + A0_log + TP_recip, data=A)
summary(DO.f.l)
# R2adj= 0.8564, p< 2.2e-16
# intercept not significant