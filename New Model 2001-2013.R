# Develop new models based on data from 1993-2013
#------------------------------------------------
# 1. Zox
# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")

LAKE <- read.csv("Zox model development 2001-2013.csv")
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
# R2adj= 0.854, p= 0.0006435
# intercept not significant

# Build new model based on data A lakes 1990-2013; Forward Selection
colnames(A)
forward.sel(A$Zox, A[,-c(1,2,7,14,21,28,35)], alpha=0.05, nperm=9999)
# DOC_recip, Max_depth_square, and A0_sqrt
# R2adj= 0.9785784

Zox.l <- lm(A$Zox ~ DOC_recip, data=A)
summary(Zox.l)


Zox.l <- lm(A$Zox ~ DOC_recip + Max_depth_square + A0_sqrt, data=A)
summary(Zox.l)
# R2adj= 0.9785784, p= 0.0002798

# Varipart for here not really good: suggest use 1/DOC as only explainatory variable.
# ### Variation partitioning with two sets of explanatory variables: chemistry and lake morphology
# colnames(A)
# Chem <- A[,16]
# Morph <- A[,c(25,33)]
# library(vegan)
# Zox.part <- varpart(A$Zox, Chem, Morph)
# plot(Zox.part)
# Zox.part


#------------------------------------------------
# Spring DO (DOi)
# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")
LAKE <- read.csv("DO profiles model development 2001-2013.csv")
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
# R2adj= 0.7162, p= 7.935e-15
#--------------------
# Forward Selection using forward.sel function
library(packfor)
#1.1 DOi：
colnames(LAKE)
forward.sel(A$DOi, A[,-c(1,2,3,13,23,33,43, 4,14,24,34,44, 19,29,39,49, 6,26,36,46)], alpha=0.05, nperm=9999)
# A0_recip, VSA_sqrt, Depth_recip
# R2adj= 0.8198721

DOi.l <- lm(A$DOi ~ A0_recip + VSA_sqrt + Depth_recip, data=A)
summary(DOi.l)
# R2adj= 0.8199, p< 2.2e-16

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
LAKE <- read.csv("DO profiles model development 2001-2013.csv")
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
DO.f.l <- lm(A$DOf_log ~ VSA_recip + DOi_recip + TP_so_square, data=A)
summary(DO.f.l)

###################
# New Model Development
#---------------------------------
# Forward Selection using forward.sel function
# 1 DOf
colnames(A)
forward.sel(A$DOf, A[,-c(1,2,4,14,24,34,44)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf ~ VSA_log + DOi_square + A0_sqrt + Max_depth_sqaure, data=A)
summary(DO.f.l)
# DOf=-1.779371 + 3.071782*VSA_log + 0.086294*DOi_square - 0.393523*A0_sqrt + 10.630975*TP_recip - 3.196827*DOC_recip
# R2adj= 0.9045, p< 2.2e-16

#---------------------------------
# 2 DOf_recip
# R2adj= 0.8161
colnames(A)
forward.sel(A$DOf_recip, A[,-c(1,2,4,14,24,34,44)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf_recip ~ DOi_sqrt + VSA_recip + A0_square + Depth_square + Mean_depth_recip, data=A)
summary(DO.f.l)
# DOf=8.103 - 2.942*DOi_sqrt + 2.647*VSA_recip + 0.0001417* A0_square - 0.04136*Depth + 3.048*TP_recip
# R2adj= 0.9113, p< 2.2e-16
#---------------------------------
#---------------------------------
# 3 DOf_log
colnames(A)
forward.sel(A$DOf_log, A[,-c(1,2,4,14,24,34,44)], alpha= 0.05, nperm=9999)
#---------------------------------
# A0
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + A0_square + Depth_square + DOC_recip, data=A)
summary(DO.f.l)
# R2adj= 0.9436, p< 2.2e-16
#---------------------------------
# no A0: GOOD
colnames(A)
forward.sel(A$DOf_log, A[,-c(1,2,4,14,24,34,44, 6,16,26,36,46, 9,19,29,39,49, 10,20,30,40,50, 2,12,22,32,42, 3,13,23,43)], alpha= 0.07, nperm=9999)

DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip, data=A)
summary(DO.f.l)
# log10(DOf)=-0.182825 + 0.013989*DOi_square - 1.060077*VSA_recip
# R2adj= 0.878, p< 2.2e-16

#---------------------------------
# no DOC
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip, data=A)
summary(DO.f.l)
# log10(DOf)=-0.467752 + 0.016222*DOi_square - 1.169331*VSA_recip + 1.006533*TP_recip
# R2adj= 0.878, p< 2.2e-16
#---------------------------------
# no TP and DOC
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip, data=A)
summary(DO.f.l)
# log10(DOf)=-0.310913 + 0.016174*DOi_square - 1.158817*VSA_recip + 2.826594*TP_recip + 0.009798*DOC_square
# R2adj= 0.8545, p< 2.2e-16
#---------------------------------
# 4 DOf_sqrt:
colnames(A)
forward.sel(A$DOf_sqrt, A[,-c(1,2,4,14,24,34,44)], alpha= 0.05)
DO.f.l <- lm(A$DOf_sqrt ~ DOi_square + VSA_log + A0_square + Depth_square, data=A)
summary(DO.f.l)
# R2adj= 0.9146, p< 2.2e-16
#---------------------------------
# 5 DOf_square: xxx
forward.sel(A$DOf_square, A[,-c(1,2,4,14,24,34,44)], alpha= 0.05)
DO.f.l <- lm(A$DOf_square ~ VSA_sqrt + DOi_square + A0_log + TP_so_recip, data=A)
summary(DO.f.l)
# R2adj= 0.8544498, p< 2.2e-16
# intercept not significant



