# Model DOf

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")
LAKE <- read.csv("DO profiles model development 1990-2013.csv")
colnames(LAKE)
rownames(LAKE)
LAKE[,1]
A <- LAKE[1:53,]

#---------------------------------

# Check the old model
# Old model looks like this: DOf_log = 1.83 - 1.91*VSA_recip - 7.06*DOi_recip - 0.0013*TP_so_squre

DOf.pred <- 10^(1.83 - 1.91* A$VSA_recip - 7.06* A$DOi_recip - 0.0013* A$TP_square)
DOf.obs <- A$DOf

cor(DOf.obs, DOf.pred) # r= 0.9141923
# we cannot check the old model becuase our VSA data are not the same. Molot et al. 1992 used 2-m interval VSA and Jiahua used 1-m interval VSA.

# Check predicted and observed end of summer DO (DOf)
plot(DOf.pred, DOf.obs, xlab="DOf predicted (mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(6,2,label="1:1 line", col="red")

# For this section above, I suspect that we may not be able to do it because VSAs are in different value for 1-m interval than 2-m's.
#---------------------------------

# Try the same kind of model but different estimate (i.e. DOf_log = ? + ?*VSA_recip - ?*DOi_recip - ?*TP_so_square)
DO.f.l <- lm(A$DOf_log ~ VSA_recip + DOi_recip + TP_so_square, data=A)
summary(DO.f.l)

# R2adj= 0.5733; not as good
#---------------------------------
# What if add DOC (try all transformation and select the best one)--the following one is the best one
# R2adj= 0.3221 , p=4.087e-11
colnames(A)
forward.sel(A$DOf, A[,c(7,9,33,28,30,32,34)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ VSA_recip + DOi_recip + TP_so_square + DOC_recip, data=A)
summary(DO.f.l)
# R2adj= 0.6231
#---------------------------------


###################
# New Model Development

library(packfor)

colnames(A)

# 1.1 DOf (all variables except morphology like Zmean,Zmax,MD)
# GOOD
colnames(A)
forward.sel(A$DOf, A[,-c(1,2,6,12,18,23,28)], alpha=0.05, nperm=9999)

DO.f.l <- lm(A$DOf ~ VSA_log + DOi_square + A0_sqrt + NO3.E.log, data=A)
summary(DO.f.l)

Residual standard error: 0.6609 on 48 degrees of freedom
Multiple R-squared:  0.9182,	Adjusted R-squared:  0.9114 
F-statistic: 134.6 on 4 and 48 DF,  p-value: < 2.2e-16
#---------------------------------
# 1.2 DOf (only VSA, DOi, TP)
colnames(A)
forward.sel(A$DOf, A[,-c(1:4,6,12,18,23,28,10,16,32:33)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf ~ VSA_log + DOi_square + TP_recip, data=A)
summary(DO.f.l)

Residual standard error: 0.7949 on 49 degrees of freedom
Multiple R-squared:  0.8791,	Adjusted R-squared:  0.8717 
F-statistic: 118.8 on 3 and 49 DF,  p-value: < 2.2e-16
#---------------------------------
#---------------------------------
# 2.1 DOf_log (only DOi and VSA)
colnames(A)
forward.sel(A$DOf_log, A[,-c(1:4,6,8:10,12,14:16,18,20,21,23,28,30:113, 5,11,17,27)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip, data=A)
summary(DO.f.l)

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.310913   0.100526  -3.093  0.00324 ** 
DOi_square   0.016174   0.001365  11.849 3.95e-16 ***
VSA_recip   -1.158817   0.160929  -7.201 2.91e-09 ***

Residual standard error: 0.1726 on 50 degrees of freedom
Multiple R-squared:  0.8637,	Adjusted R-squared:  0.8583 
F-statistic: 158.5 on 2 and 50 DF,  p-value: < 2.2e-16

#---------------------------------
# 2.2 DOf_log (DOi, VSA, N)
colnames(A)
forward.sel(A$DOf_log, A[,c(22,19,10,34:108)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + NH4.H.sqrt + A0_log, data=A)
summary(DO.f.l)

Procedure stopped (alpha criteria): pvalue for variable 5 is 0.246500 (superior to 0.050000)
   variables order         R2     R2Cum  AdjR2Cum         F  pval
1 DOi_square     1 0.72244835 0.7224483 0.7170062 132.74958 1e-04
2  VSA_recip     2 0.14129830 0.8637467 0.8582965  51.85131 1e-04
3 NH4.H.sqrt    42 0.04632539 0.9100720 0.9045662  25.24181 1e-04
4     A0_log     3 0.02460938 0.9346814 0.9292382  18.08444 2e-04

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.679489   0.171525   3.961 0.000246 ***
DOi_square   0.015885   0.001452  10.937 1.24e-14 ***
VSA_recip   -0.879607   0.120125  -7.322 2.36e-09 ***
NH4.H.sqrt  -0.030390   0.004843  -6.275 9.51e-08 ***
A0_log      -0.493097   0.115952  -4.253 9.71e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.122 on 48 degrees of freedom
Multiple R-squared:  0.9347,	Adjusted R-squared:  0.9292 
F-statistic: 171.7 on 4 and 48 DF,  p-value: < 2.2e-16

# Very Good


#---------------------------------
# 2.3 DOf_log (DOi, VSA, TP, DOC)
colnames(A)
forward.sel(A$DOf_log, A[,c(22,19,20,26)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + TP_recip + DOC_square, data=A)
summary(DO.f.l)





par(mfrow=c(2,3))
hist(A$DOf_log)
hist(A$DOi_square)
hist(A$VSA_square)
hist(A$TP_so_recip)
hist(A$DOC_recip)

shapiro.test(A$DOf_log)
shapiro.test(A$DOi_square)
shapiro.test(A$VSA_square)
shapiro.test(A$TP_so_recip)
shapiro.test(A$DOC_recip)


#---------------------------------
#---------------------------------
# 3.1 DOf_recip (all variables)
forward.sel(A$DOf_recip, A[,-c(1,4,8,12,16,20,3,7,11,15,38,44,47,50,22,24,23,26,2,6,10,18,27,31,29,35,5,9,13,21)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_recip ~ DOi_sqrt + Max_depth_recip + A0_log + Depth_square + TP_so_square + VSA_square, data=A)
summary(DO.f.l)

#Residual standard error: 0.454 on 121 degrees of freedom
#Multiple R-squared:  0.8964,	Adjusted R-squared:  0.8913 
#F-statistic: 174.5 on 6 and 121 DF,  p-value: < 2.2e-16
#---------------------------------
# 3.2 DOf_recip (only DOi TP DOC and VSA)
forward.sel(A$DOf_recip, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,15,5,9,13,17)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_recip ~ DOi_sqrt + VSA_sqrt, data=A)
summary(DO.f.l)

#Residual standard error: 0.7746 on 125 degrees of freedom
#Multiple R-squared:  0.6884,	Adjusted R-squared:  0.6834 
#F-statistic: 138.1 on 2 and 125 DF,  p-value: < 2.2e-16

#---------------------------------
#---------------------------------
# 4.1 DOf_sqrt (all variables)
forward.sel(A$DOf_sqrt, A[,-c(1,4,8,12,16,20,3,7,11,19,38,44,47,50,22,24,25,26,37,40,43,49,28,32,30,36)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_sqrt ~ DOi_square + Max_depth_recip + A0_recip + Mean_depth_sqaure + DOC_square, data=A)
summary(DO.f.l)

# Residual standard error: 0.1925 on 122 degrees of freedom
# Multiple R-squared:  0.9323,	Adjusted R-squared:  0.9295 
# F-statistic: 335.9 on 5 and 122 DF,  p-value: < 2.2e-16
# new
#---------------------------------
# 4.2 DOf_recip (only DOi TP DOC and VSA)
forward.sel(A$DOf_sqrt, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,19,5,9,13,21,27,29,33,35,28,34,30,36)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_sqrt ~ DOi_square + VSA_square + TP_so_recip + DOC_recip, data=A)
summary(DO.f.l)

    # variables order         R2     R2Cum  AdjR2Cum         F  pval
# 1  DOi_square     1 0.75926878 0.7592688 0.7573582 397.40532 1e-04
# 2  VSA_square     2 0.04888532 0.8081541 0.8050846  31.85195 1e-04
# 3 TP_so_recip     3 0.02124734 0.8294014 0.8252741  15.44368 1e-04
# 4   DOC_recip     4 0.02103386 0.8504353 0.8455714  17.29796 5e-04

# Coefficients:
             # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.801125   0.171454  -4.673 7.68e-06 ***
# DOi_square   0.024145   0.001573  15.348  < 2e-16 ***
# VSA_square   1.118237   0.222213   5.032 1.68e-06 ***
# TP_so_recip  4.670658   0.792077   5.897 3.34e-08 ***
# DOC_recip   -1.453362   0.349443  -4.159 5.95e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2849 on 123 degrees of freedom
# Multiple R-squared:  0.8504,	Adjusted R-squared:  0.8456 
# F-statistic: 174.8 on 4 and 123 DF,  p-value: < 2.2e-16

par(mfrow=c(2,3))
hist(A$DOf_sqrt)
hist(A$DOi_square)
hist(A$VSA_square)
hist(A$TP_so_recip)
hist(A$DOC_recip)

shapiro.test(A$DOf_sqrt)
shapiro.test(A$DOi_square)
shapiro.test(A$VSA_square)
shapiro.test(A$TP_so_recip)
shapiro.test(A$DOC_recip)
#---------------------------------
#---------------------------------
# 5.1 DOf_square (all variables)
forward.sel(A$DOf_square, A[,-c(1,4,8,12,16,20,3,7,11,19,22,24,23,26,37,40,43,49,6,10,14,18,38,41,44,50,9,13,17,21)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_square ~ DOi_square + A0_log + Mean_depth_sqaure + Depth + Max_depth_sqaure + VSA, data=A)
summary(DO.f.l)

# Residual standard error: 6.263 on 121 degrees of freedom
# Multiple R-squared:  0.8683,	Adjusted R-squared:  0.8617 
# F-statistic: 132.9 on 6 and 121 DF,  p-value: < 2.2e-16
#---------------------------------
# 5.2 DOf_square (only DOi TP DOC and VSA)
forward.sel(A$DOf_square, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,19,27,29,33,35,5,9,13,21,28,32,30,36)], alpha= 0.06, nperm=9999)
DO.f.l <- lm(A$DOf_square ~ DOi_square + VSA_square + TP_so_recip, data=A)
summary(DO.f.l)

# Residual standard error: 9.301 on 124 degrees of freedom
# Multiple R-squared:  0.7023,	Adjusted R-squared:  0.6951 
# F-statistic: 97.49 on 3 and 124 DF,  p-value: < 2.2e-16