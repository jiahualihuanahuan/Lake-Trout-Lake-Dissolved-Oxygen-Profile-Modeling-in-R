# Develop new models based on data from 1993-2013
#------------------------------------------------
# 1. Zox
# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")

LAKE <- read.csv("Zox model development 1993-2013.csv")
colnames(LAKE)
A <- LAKE[1:8,]
B <- LAKE[9:16,]
AB <- LAKE[1:16,]
# forward selection
library(packfor)
colnames(LAKE)
LAKE[,1]

# Build similar model but different coefficient
Zox.l <- lm(A$Zox ~ DOC_recip, data=A)
summary(Zox.l)
A$DOC_recip
# R2adj= 0.8587, p= 0.0005822

# Build new model based on data A lakes 1990-2013; Forward Selection
colnames(A)
forward.sel(A$Zox, A[,-c(1,2,7,14,21,28,35)], alpha=0.05, nperm=9999)
# DOC_recip, NH4.H.recip, TKN.E.sqr
# R2adj= 0.9896970

Zox.l <- lm(A$Zox ~ DOC_recip + NH4.H.recip + TKN.E.sqr, data=A)
summary(Zox.l)
# R2adj= 0.9897, p= 6.486e-05


#------------------------------------------------
# Spring DO (DOi)
# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")
LAKE <- read.csv("DO profiles model development 1993-2013.csv")
colnames(LAKE)
rownames(LAKE)
LAKE[,1]
A <- LAKE[1:53,]
B <- LAKE[54:106,]
AB <- LAKE[1:106,]
str(A)
colnames(A)
rownames(A)

# Use the same predictive variables and build new Spring DO model using data 1990-2013
DOi.l <- lm(A$DOi ~ A0_recip + Depth, data=A)
summary(DOi.l)
# R2adj= 0.7234, p= 4.192e-15
#--------------------
# Forward Selection using forward.sel function
library(packfor)
#1.1 DOi：
colnames(A)
forward.sel(A$DOi, A[,-c(1,2,3,13,23,33,43, 4,14,24,34,44, 19,29,39,49)], alpha=0.05, nperm=9999)
# A0_recip, VSA_sqrt, Depth_log, TKN.H.sqrt
# R2adj= 0.8934892

DOi.l <- lm(A$DOi ~ A0_recip + VSA_sqrt + Depth_log + TKN.H.sqrt, data=A)
summary(DOi.l)
# R2adj= 0.8935, p< 2.2e-16

# 1.2 DOi: without Harp Lake
colnames(LAKE)
LAKE[,1]
forward.sel(A[-c(25:36),]$DOi, A[-c(25:36),-c(1,2,3,13,23,33,43, 4,14,24,34,44)], alpha=0.05, nperm=9999)
# A0_recip, VSA sqrt, Depth, TP_recip
# R2adj= 0.8024623

DOi.l <- lm(A[-c(25:36),]$DOi ~ A0_recip + VSA_sqrt + Depth + TP_recip, data=A[-c(25:36),])
summary(DOi.l)
# DOi model build by A lakes but without Harp Lake
# DOi = 8.73993 -83.20614/A0 + 0.48584* sqrt(VSA) - 0.09081*Z + 6.68843/TP
# R2adj= 0.8025, p=4.974e-13

# Validation on B lakes
DOi.B.pred <- 8.69279 - 82.72090*B$A0_recip + 1.44704*B$VSA_log - 0.08206*B$Depth + 6.82540*B$TP_recip
DOi.B.obs <- B$DOi 
cor(DOi.B.obs, DOi.B.pred) 
# r= 0.9393865

DOi.B.pred <- 8.73993 - 83.20614*B[-c(10:21),]$A0_recip + 0.48584*B[-c(10:21),]$VSA_sqrt - 0.09081*B[-c(10:21),]$Depth + 6.68843*B[-c(10:21),]$TP_recip
DOi.B.obs <- B[-c(10:21),]$DOi 
cor(DOi.B.obs, DOi.B.pred) 
# r= 0.876316

#--------------------
# 2.1 DOi_recip:
colnames(A) 
forward.sel(A$DOi_recip, A[,-c(1,2,5,6,11,12,17,18,22,23,27,28, 4,10)], alpha=0.05, nperm=9999)
# R2adj= 0.5395754

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
LAKE <- read.csv("DO profiles model development 1993-2013.csv")
colnames(LAKE)
rownames(LAKE)
LAKE[,1]
A <- LAKE[1:53,]
B <- LAKE[54:106,]
colnames(A)
rownames(A)

# Check the old model
# Old model looks like this: DOf_log = 1.8305336 - 1.9055824*VSA_recip - 7.0614672*DOi_recip - 0.001268*TP_so_squre

# A lakes
DOf.pred <- 10^(1.8305336 - 1.9055824* A$VSA_recip - 7.0614672* A$DOi_recip - 0.001268* A$TP_so_square)
DOf.obs <- A$DOf
cor(DOf.obs, DOf.pred) # r= 0.9143472

# Try the same kind of model but different estimate (i.e. DOf_log = ? + ?*VSA_recip - ?*DOi_recip - ?*TP_so_square)
# R2adj= 0.7491, p= 2.258e-15
DO.f.l <- lm(A$DOf_log ~ VSA_recip + DOi_recip, data=A)
summary(DO.f.l)

###################
# New Model Development
#---------------------------------
# Forward Selection using forward.sel function
# 1 DOf
colnames(A)
forward.sel(A$DOf, A[,-c(1,2,4,14,24,34,44)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf ~ VSA_log + DOi_square + A0_sqrt + NO3.E.recip, data=A)
summary(DO.f.l)
# DOf=1.3 + 2.9*VSA_log + 0.09*DOi^2 - 0.5*A0_sqrt + 10.630975*TP_recip - 16.8/NO3_E
# R2adj= 0.9083, p< 2.2e-16
# intercept not significant
#---------------------------------
# 2 DOf_recip
colnames(A)
forward.sel(A$DOf_recip, A[,-c(1,2,4,14,24,34,44)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf_recip ~ DOi_sqrt + VSA_recip + A0_square + Depth_square + TKN.H.log + NO3.H.sqr, data=A)
summary(DO.f.l)
# 1/DOf=18.8 - 3.6*sqrt(DOi) + 2.7/VSA + 0.00023*A0^2 - 0.002*Z^2 - 3.4*log10(TKN_H) - 0.00002*NO3_H^2
# R2adj= 0.9229, p< 2.2e-16
# OK, all variables and intercept are signiciant
#---------------------------------
#---------------------------------
# 3 DOf_log
colnames(A)


forward.sel(A$DOf_log, A[,-c(1,4,14,24,34,44)], alpha= 0.05, nperm=9999)
#---------------------------------
# all variables
colnames(A)
forward.sel(A$DOf_log, A[,-c(1,4,14,24,34,44,3,13,23,43,5,25,35,45,12,22,32,42)], alpha= 0.05, nperm=9999)

If include depth, NH4_H is not significant
#---------------------------------
# has A0, no depth: GOODGOOGGOOGOGOOGOGOGOGOGOGO
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + NH4.H.sqrt + A0_log, data=A)
summary(DO.f.l)
# R2adj= 0.9245, p< 2.2e-16
# GOOD, all variables and intecept are significant
#---------------------------------
# has A0, has depth: NONONONONONONONO
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + NH4.H.sqrt + A0_log + Depth + DOC_square, data=A)
summary(DO.f.l)
# R2adj= 0.9544, p< 2.2e-16
# VSA variable not significant

#---------------------------------
# no A0: NONOONO
colnames(A)
forward.sel(A$DOf_log, A[,-c(1,4,14,24,34,44, 6,16,26,36,46)], alpha= 0.05, nperm=9999)

DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + NH4.H.sqrt + MD_recip, data=A)
summary(DO.f.l)
# log10(DOf)=-0.245990 + 0.013930*DOi_square - 0.785604*VSA_recip - 0.035186*sqrt(NH4_H) + 0.215263/MD
# R2adj= 0.9135, p< 2.2e-16
# intercept not significant
#---------------------------------
# 4 DOf_sqrt:NONONONONON
colnames(A)
forward.sel(A$DOf_sqrt, A[,-c(1,2,4,14,24,34,44, 5,15,35,45)], alpha= 0.05)
DO.f.l <- lm(A$DOf_sqrt ~ DOi_square + VSA_log + A0_square + NO3.E.recip, data=A)
summary(DO.f.l)
# R2adj= 0.9193, p< 2.2e-16
# intercept not significant
#---------------------------------
# 5 DOf_square: NONONONON
forward.sel(A$DOf_square, A[,-c(1,2,4,14,24,34,44)], alpha= 0.05)
DO.f.l <- lm(A$DOf_square ~ VSA_sqrt + DOi_square + A0_log + TP_so_recip, data=A)
summary(DO.f.l)
# R2adj= 0.8544498, p< 2.2e-16
# intercept not significant



