# Model DOf

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")
LAKE <- read.csv("DO profiles model development 1993-2013.csv")
colnames(LAKE)
rownames(LAKE)
LAKE[,1]
A <- LAKE[1:53,]
~
#---------------------------------

# Check the old model
# Old model looks like this: DOf_log = 1.83 - 1.91*VSA_recip - 7.06*DOi_recip - 0.0013*TP_so_squre

DOf.pred <- 10^(1.83 - 1.91* A$VSA_recip - 7.06* A$DOi_recip - 0.0013* A$TP_so_square)
DOf.obs <- A$DOf

cor(DOf.obs, DOf.pred) # r= 0.9271338

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
# R2adj= 0.2684, p=1.223e-09
DO.f.l <- lm(A$DOf_log ~ VSA_recip + TP_so_square, data=A)
summary(DO.f.l)


# not good
#---------------------------------
# What if add DOC (try all transformation and select the best one)--the following one is the best one
# R2adj= 0.3221 , p=4.087e-11
DO.f.l <- lm(A$DOf_log ~ VSA_recip + TP_so_square + DOC_recip, data=A)
summary(DO.f.l)

# not good
#---------------------------------


###################
# New Model Development

library(packfor)

colnames(A)
??
# 1 DOf (all variables)
forward.sel(A$DOf, A[,-c(1,4,8,12,16,20)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf ~ DOi_square + A0 + Max_depth_sqaure + Depth + DOC_recip + MD_recip, data=A)
summary(DO.f.l)

?

#---------------------------------
# 1.1 DOf (only VSA, TP, DOC, DOi)
forward.sel(A$DOf, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,19,27,29,33,35,5,9,13,21,32,34,30,36)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf ~ DOi_square + TP_so_recip + VSA_square + DOC_1993_2013, data=A)
summary(DO.f.l)

# Residual standard error: 0.7746 on 125 degrees of freedom
# Multiple R-squared:  0.6884,	Adjusted R-squared:  0.6834 
# F-statistic: 138.1 on 2 and 125 DF,  p-value: < 2.2e-16
#---------------------------------
#---------------------------------
# 2.1 DOf_log (all variables)
colnames(A)
forward.sel(A$DOf_log, A[,-c(1,4,14,24,34,44, 3,13,23,43, 26,46,6,16,36)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + NH4.H.sqrt, data=A)
summary(DO.f.l)

??
#---------------------------------
# 2.2 DOf_log (only DOi TP DOC and VSA)
forward.sel(A$DOf_log, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,19,5,9,13,21,27,29,33,35,28,32,34,30)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_square + TP_so_recip + DOC_sqrt, data=A)
summary(DO.f.l)

par(mfrow=c(2,3))
hist(A$DOf_log)
hist(A$DOi_square)
hist(A$VSA_square)
hist(A$TP_so_recip)
hist(A$DOC_sqrt)

shapiro.test(A$DOf_log)
shapiro.test(A$DOi_square)
shapiro.test(A$VSA_square)
shapiro.test(A$TP_so_recip)
shapiro.test(A$DOC_sqrt)

# Residual standard error: 0.2017 on 123 degrees of freedom
# Multiple R-squared:  0.8262,	Adjusted R-squared:  0.8205 
# F-statistic: 146.2 on 4 and 123 DF,  p-value: < 2.2e-16


#---------------------------------
#---------------------------------
# 3.1 DOf_recip (all variables)
forward.sel(A$DOf_recip, A[,-c(1,4,8,12,16,20,3,7,11,15,38,44,47,50,22,24,23,26,2,6,10,18,27,31,29,35,5,9,13,21)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_recip ~ DOi_sqrt + Max_depth_recip + A0_log + Depth_square + TP_so_square + VSA_square, data=A)
summary(DO.f.l)

??
#---------------------------------
# 3.2 DOf_recip (only DOi TP DOC and VSA)
forward.sel(A$DOf_recip, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,15,9,13,17,21)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_recip ~ DOi_sqrt + VSA, data=A)
summary(DO.f.l)

# Residual standard error: 0.7232 on 125 degrees of freedom
# Multiple R-squared:  0.6845,	Adjusted R-squared:  0.6795 
# F-statistic: 135.6 on 2 and 125 DF,  p-value: < 2.2e-16


#---------------------------------
# sqrt(DOf) (only VSA, TP, DOC, DOi)
forward.sel(A$DOf_sqrt, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,19,5,9,13,21,27,29,33,35,28,32,34,30)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf_sqrt ~ DOi_square + VSA_square + TP_so_recip + DOC_sqrt, data=A)
summary(DO.f.l)

par(mfrow=c(2,3))
hist(A$DOf_sqrt)
hist(A$DOi_square)
hist(A$VSA_square)
hist(A$TP_so_recip)
hist(A$DOC_sqrt)

shapiro.test(A$DOf_sqrt)
shapiro.test(A$DOi_square)
shapiro.test(A$VSA_square)
shapiro.test(A$TP_so_recip)
shapiro.test(A$DOC_sqrt)
#---------------------------------
# square(DOf) (only VSA, TP, DOC, DOi)
forward.sel(A$DOf_square, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,19,27,29,33,35,5,9,13,21 ,28,32,30,36)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf_square ~ DOi_square + TP_so_recip + VSA_square + DOC_square, data=A)
summary(DO.f.l)

