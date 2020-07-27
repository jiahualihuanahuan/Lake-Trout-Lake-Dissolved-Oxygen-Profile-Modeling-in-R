# Part 3 DO profile model chapter
# some figures


# Figure 1

# set working directory
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for model development")
LAKE <- read.csv("DO profiles model development 1990-2013.csv")
colnames(LAKE)
rownames(LAKE)
LAKE[,1]
A <- LAKE[1:53,]


#-------------------------
# model DOf~DOi+VSA+NH4hypo
colnames(A)
forward.sel(A$DOf_log, A[,c(22,19,10,34:108)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + NH4.H.sqrt + A0_log, data=A)
summary(DO.f.l)

# examine the impact of doubling (higher) hypolimnetic NH4 on DOf at ocnstant DOi and constant TPso
DOf.pred.1xNH4 <- 10^(0.679489 + 0.015885*A$DOi_square - 0.879607*A$VSA_recip - 0.030390*A$NH4.H.sqrt - 0.493097*A$A0_log)
DOf.pred.2xNH4 <- 10^(0.679489 + 0.015885*A$DOi_square - 0.879607*A$VSA_recip - 0.030390*A$NH4.H.sqrt - 2*0.493097*A$A0_log)
DOf.obs <- A$DOf

# plot
par(mfrow=c(3,1))
# 2.2.1 Check predicted 1xNH4 and observed end of summer DO (DOf)
plot(DOf.pred.1xNH4, DOf.obs, xlab="DOf predicted 1xNH4(mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred.1xNH4)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(5,2,label="1:1 line", col="red")

# 2.2.2 Check predicted 2xNH4 and observed end of summer DO (DOf)
plot(DOf.pred.2xNH4, DOf.obs, xlab="DOf predicted 2xNH4 (mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred.2xNH4)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(6,2,label="1:1 line", col="red")

# 2.2.3 Check predicted 1xNH4 and predicted 1xNH4 end of summer DO (DOf)
plot(DOf.pred.2xNH4, DOf.pred.1xNH4, xlab="DOf predicted 2xNH4 (mg/L)", ylab="DOf predicted 1xNH4 (mg/L)")
obs.vs.pred <- lm(DOf.pred.1xNH4 ~ DOf.pred.2xNH4)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(2,6,label="1:1 line", col="red")

#-------------------------
# Figure 2
# model DOf~DOi+VSA+NH4hypo
forward.sel(A$DOf_log, A[,c(22,19,20,26)], alpha= 0.05, nperm=9999)
DO.f.l <- lm(A$DOf_log ~ DOi_square + VSA_recip + TP_recip + DOC_square, data=A)
summary(DO.f.l)
# Impact of Brownification on DOf
# examine the impact of a doubling of DOC on DOf at constant DOi and constant TPso
A[,c(12,22,19,20,26)]
DOf.pred.1xDOC <- 10^(-0.905250 + 0.015922*A$DOi_square - 1.129529*A$VSA_recip + 2.826594*A$TP_recip + 0.009798*A$DOC_square)
DOf.pred.2xDOC <- 10^(-0.905250 + 0.015922*A$DOi_square - 1.129529*A$VSA_recip + 2.826594*A$TP_recip + 2*0.009798*A$DOC_square)
DOf.obs <- A$DOf

par(mfrow=c(3,1))
# 2.3.1 Check predicted 1xDOC and observed end of summer DO (DOf)
plot(DOf.pred.1xDOC, DOf.obs, xlab="DOf predicted 1xDOC(mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred.1xDOC)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(5,2,label="1:1 line", col="red")

# 2.3.2 Check predicted 2xDOC and observed end of summer DO (DOf)
plot(DOf.pred.2xDOC, DOf.obs, xlab="DOf predicted 2xDOC (mg/L)", ylab="DOf observed (mg/L)")
obs.vs.pred <- lm(DOf.obs ~ DOf.pred.2xDOC)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(6,2,label="1:1 line", col="red")

# 2.3.3 Check predicted 1xDOC and predicted 1xDOC end of summer DO (DOf)
plot(DOf.pred.2xDOC, DOf.pred.1xDOC, xlab="DOf predicted 2xDOC (mg/L)", ylab="DOf predicted 1xDOC (mg/L)")
obs.vs.pred <- lm(DOf.pred.1xDOC ~ DOf.pred.2xDOC)
abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(2,6,label="1:1 line", col="red")
#-------------------------
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 3 DO profile modeling/Data/csv data for comparison between two study periods")
DO <- read.csv("DOi and DOf profiles comparison between two periods.csv")
CHEM <- read.csv("Lake chemistry data comparison between two periods.csv")
head(DO)
head(CHEM)
DO[,1]
CHEM[,1]

par(mfrow=c(2,1))
par(mar=c(4,4,1,1))
#-------------------------
# Figure 3
# Comparison of mean spring DO in 1976-1989 to 1990-2013 with 1:1 line
plot(DO$DOi_1976_1989, DO$DOi_1990_2013, xlab="DOi 1976-1989 (mg/L)", ylab="DOi 1990-2013 (mg/L)", xlim=c(0,10), ylim=c(0,10))
obs.vs.pred <- lm(DO$DOi_1990_2013 ~ DO$DOi_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,5,label="1:1 line", col="red")


#-------------------------
# Figure 4
# Comparison of mean end-of-summer DO in 1976-1989 to 1990-2013 with 1:1 line
plot(DO$DOf_1976_1989, DO$DOf_1990_2013, xlab="DOf 1976-1989 (mg/L)", ylab="DOf 1990-2013 (mg/L)", xlim=c(0,10), ylim=c(0,10))
obs.vs.pred <- lm(DO$DOf_1990_2013 ~ DO$DOf_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,5,label="1:1 line", col="red")

par(mfrow=c(2,1))
par(mar=c(4,4,1,1))
#-------------------------
# Figure 5 Comparison of mean TPso in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$TP_so_1976_1989, CHEM$TP_so_1990_2013, xlab="TP_so 1976-1989 (mg/L)", ylab="TP_so 1990-2013 (mg/L)",xlim=c(0,28),ylim=c(0,28))
obs.vs.pred <- lm(CHEM$TP_so_1990_2013 ~ CHEM$TP_so_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(8,15,label="1:1 line", col="red")


head(CHEM)
CHEM[,1]
CHEM[,c(4,5)]
delta <- CHEM[,5]-CHEM[,4]
percentage <- delta/CHEM[,5]

max(percentage[-1], na.rm=T)
min(percentage[-12], na.rm=T)
mean(percentage[-c(1,12)], na.rm=T)
#-------------------------
# Figure 6 Comparison of mean DOC in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$DOC_1976_1989, CHEM$DOC_1990_2013, xlab="DOC 1976-1989 (mg/L)", ylab="DOC 1990-2013 (mg/L)",xlim=c(0,7),ylim=c(0,7))
obs.vs.pred <- lm(CHEM$DOC_1990_2013 ~ CHEM$DOC_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(5,2,label="1:1 line", col="red")


head(CHEM)
CHEM[,1]
CHEM[,c(6,7)]
delta <- CHEM[,7]-CHEM[,6]
percentage <- delta/CHEM[,7]
hist(percentage)
max(percentage, na.rm=T)
min(percentage[-c(6,9,11,12,14,22)], na.rm=T)



max(percentage[c(11,14,22)], na.rm=T)
min(percentage[c(6,9,11,12,14)], na.rm=T)

mean(percentage, na.rm=T)
#-------------------------
#-------------------------




#-------------------------

#-------------------------
#-------------------------
#-------------------------
#-------------------------
# give the size of the outer margins some space for  text
par(oma=c(2,2,0,0))
par(mfrow=c(3,1))
par(mar=c(0,0,0,0))
#-------------------------
# Figure 7 Comparison of mean NH4 epi in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$NH4_E_1976_1989, CHEM$NH4_E_1990_2013, xlab="NH4_E 1976-1989 (mg/L)", ylab="NH4_E 1990-2013 (mg/L)",xlim=c(6,18),ylim=c(6,18))


obs.vs.pred <- lm(CHEM$NH4_E_1990_2013 ~ CHEM$NH4_E_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(13,12,label="1:1 line", col="red")


#-------------------------
# Figure 8 Comparison of mean NH4 meta in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$NH4_M_1976_1989, CHEM$NH4_M_1990_2013, xlab="NH4_M 1976-1989 (mg/L)", ylab="NH4_M 1990-2013 (mg/L)",xlim=c(10,110),ylim=c(10,110))

obs.vs.pred <- lm(CHEM$NH4_M_1990_2013 ~ CHEM$NH4_M_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(60,40,label="1:1 line", col="red")



#-------------------------
# Figure 9 Comparison of mean NH4 hypo in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$NH4_H_1976_1989, CHEM$NH4_H_1990_2013, xlab="NH4_H 1976-1989 (mg/L)", ylab="NH4_H 1990-2013 (mg/L)",xlim=c(10,430),ylim=c(10,430))


obs.vs.pred <- lm(CHEM$NH4_H_1990_2013 ~ CHEM$NH4_H_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(100,40,label="1:1 line", col="red")

#-------------------------
# Figure 10 Comparison of mean NO3 epi in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$NO3_E_1976_1989, CHEM$NO3_E_1990_2013, xlab="NO3_E 1976-1989 (mg/L)", ylab="NO3_E 1990-2013 (mg/L)",xlim=c(0,35),ylim=c(0,35))


obs.vs.pred <- lm(CHEM$NO3_E_1990_2013 ~ CHEM$NO3_E_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(5,12,label="1:1 line", col="red")


#-------------------------
# Figure 11 Comparison of mean NO3 meta in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$NO3_M_1976_1989, CHEM$NO3_M_1990_2013, xlab="NO3_M 1976-1989 (mg/L)", ylab="NO3_M 1990-2013 (mg/L)",xlim=c(10,110),ylim=c(10,110))

obs.vs.pred <- lm(CHEM$NO3_M_1990_2013 ~ CHEM$NO3_M_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(60,40,label="1:1 line", col="red")



#-------------------------
# Figure 12 Comparison of mean NO3 hypo in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$NO3_H_1976_1989, CHEM$NO3_H_1990_2013, xlab="NO3_H 1976-1989 (mg/L)", ylab="NO3_H 1990-2013 (mg/L)",xlim=c(10,170),ylim=c(10,170))


obs.vs.pred <- lm(CHEM$NO3_H_1990_2013 ~ CHEM$NO3_H_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(100,40,label="1:1 line", col="red")



#-------------------------
# Figure 13 Comparison of mean TN epi in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$TN_E_1976_1989, CHEM$TN_E_1990_2013, xlab="TN_E 1976-1989 (mg/L)", ylab="TN_E 1990-2013 (mg/L)",xlim=c(180,300),ylim=c(180,300))


obs.vs.pred <- lm(CHEM$TN_E_1990_2013 ~ CHEM$TN_E_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(220,200,label="1:1 line", col="red")


#-------------------------
# Figure 14 Comparison of mean TN meta in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$TN_M_1976_1989, CHEM$TN_M_1990_2013, xlab="TN_M 1976-1989 (mg/L)", ylab="TN_M 1990-2013 (mg/L)",xlim=c(190,410),ylim=c(190,410))

obs.vs.pred <- lm(CHEM$TN_M_1990_2013 ~ CHEM$TN_M_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(300,270, label="1:1 line", col="red")



#-------------------------
# Figure 15 Comparison of mean TN hypo in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$TN_H_1976_1989, CHEM$TN_H_1990_2013, xlab="TN_H 1976-1989 (mg/L)", ylab="TN_H 1990-2013 (mg/L)",xlim=c(320,750),ylim=c(320,750))


obs.vs.pred <- lm(CHEM$TN_H_1990_2013 ~ CHEM$TN_H_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(600,500,label="1:1 line", col="red")

#-------------------------
# Figure 16 Comparison of mean TON epi in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$TON_E_1976_1989, CHEM$TON_E_1990_2013, xlab="TON_E 1976-1989 (mg/L)", ylab="TON_E 1990-2013 (mg/L)",xlim=c(150,300),ylim=c(150,300))

obs.vs.pred <- lm(CHEM$TON_E_1990_2013 ~ CHEM$TON_E_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(220,200,label="1:1 line", col="red")


#-------------------------
# Figure 17 Comparison of mean TON meta in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$TON_M_1976_1989, CHEM$TON_M_1990_2013, xlab="TON_M 1976-1989 (mg/L)", ylab="TON_M 1990-2013 (mg/L)",xlim=c(160,300),ylim=c(160,300))

obs.vs.pred <- lm(CHEM$TON_M_1990_2013 ~ CHEM$TON_M_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(240,220, label="1:1 line", col="red")
#-------------------------
# Figure 18 Comparison of mean TON hypo in 1976-1989 to 1990-2013 with 1:1 line
plot(CHEM$TON_H_1976_1989, CHEM$TON_H_1990_2013, xlab="TON_H 1976-1989 (mg/L)", ylab="TON_H 1990-2013 (mg/L)",xlim=c(150,350),ylim=c(150,350))

obs.vs.pred <- lm(CHEM$TON_H_1990_2013 ~ CHEM$TON_H_1976_1989)
# abline(obs.vs.pred)
abline(0,1, col="red")
summary(obs.vs.pred)
text(240,220,label="1:1 line", col="red")











