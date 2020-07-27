# Model DOf

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/the Lake Trout Strategic Project/csv data for model development")
LAKE <- read.csv("data for new models development 6 1993-2006.csv")
colnames(LAKE)
rownames(LAKE)
LAKE[,1]
A <- LAKE[1:128,]

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
forward.sel(A$DOf, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,19,27,31,29,35,5,9,13,21,28,34,30,36)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf ~ DOi_square + TP_so_square + VSA_square + DOC_recip, data=A)
summary(DO.f.l)

# Radj2= 0.8146
# p-value: < 2.2e-16

# OK

#---------------------------------
#---------------------------------
# 2.1 DOf_log (all variables)
forward.sel(A$DOf_log, A[,-c(1,4,8,12,16,20)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + Max_depth_recip + A0_log + Depth_square + DOC_sqrt + MD_log, data=A)
summary(DO.f.l)

??
#---------------------------------
# 2.2 DOf_log (only DOi TP DOC and VSA)
forward.sel(A$DOf_log, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_square + TP_so_recip + DOC_recip, data=A)
summary(DO.f.l)

par(mfrow=c(2,3))
hist(A$DOf_log)
hist(A$DOi_square)
hist(A$VSA_square)
hist(A$TP_so_recip)
hist(A$DOC_recip)

# Residual standard error: 0.1993 on 123 degrees of freedom
# Multiple R-squared:  0.8373,	Adjusted R-squared:  0.832 
# F-statistic: 158.3 on 4 and 123 DF,  p-value: < 2.2e-16

# OK good

#---------------------------------
#---------------------------------
# 3.1 DOf_recip (all variables)
forward.sel(A$DOf_recip, A[,-c(1,4,8,12,16,20,3,7,11,15,38,44,47,50,22,24,23,26,2,6,10,18,27,31,29,35,5,9,13,21)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_recip ~ DOi_sqrt + Max_depth_recip + A0_log + Depth_square + TP_so_square + VSA_square, data=A)
summary(DO.f.l)

??
#---------------------------------
# 3.2 DOf_recip (only DOi TP DOC and VSA)
forward.sel(A$DOf_recip, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,7,11,15,19,5,9,17,21,28,34,30,36,27,29,33,35)], alpha= 0.07, nperm=9999)
DO.f.l <- lm(A$DOf_recip ~ DOi + VSA_log + DOC_recip + TP_so_recip, data=A)
summary(DO.f.l)

# Residual standard error: 0.8369 on 125 degrees of freedom
# Multiple R-squared:  0.6636,	Adjusted R-squared:  0.6582 
# F-statistic: 123.3 on 2 and 125 DF,  p-value: < 2.2e-16

# bad model
#---------------------------------
# sqrt(DOf) (only VSA, TP, DOC, DOi)
forward.sel(A$DOf_sqrt, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,19,5,9,13,21,27,29,33,35,28,34,30,36)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf_sqrt ~ DOi_square + TP_so_sqrt + VSA_square + DOC_square, data=A)
summary(DO.f.l)

par(mfrow=c(2,3))
hist(A$DOf_sqrt)
hist(A$DOi_square)
hist(A$TP_so_sqrt)
hist(A$VSA_square)
hist(A$DOC_square)

#---------------------------------
# square(DOf) (only VSA, TP, DOC, DOi)
forward.sel(A$DOf_square, A[,-c(1,4,8,12,16,20,22,23,24,25,26,39,42,45,48,51,37,40,43,46,49,38,41,44,47,50,2,6,10,14,18,3,7,11,19,27,31,29,33,5,9,13,21,28,32,30,36)], alpha=0.05, nperm=9999)
DO.f.l <- lm(A$DOf_square ~ DOi_square + TP_so_recip + VSA_square + DOC_square, data=A)
summary(DO.f.l)