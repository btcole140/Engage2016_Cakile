setwd("/Users/mac/Google Drive/NSERC Engage/Methods and Data/Cakile/data analysis")

EngC <- read.csv("Engage2016_Cakile.csv")

library("car", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("lme4", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")


str(EngC)

EngC$SID <- as.factor(EngC$SID)
EngC$Rep <- as.factor(EngC$Rep)
EngC$Trtmt <- as.factor(EngC$Trtmt)
EngC$Pres_Abs <- as.factor(EngC$Pres_Abs)
EngC$DateFlw <- as.Date(EngC$DateFlw, "%d-%b-%y")
EngC$DateFrt <- as.Date(EngC$DateFrt, "%d-%b-%y")
write.table(EngC, file = "Engage2016_Cakile_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)


#Subset data
EngC1 <- subset(EngC, Rep == "1")
EngC2 <- subset(EngC, Rep == "2")
EngC3 <- subset(EngC, Rep == "3")
EngC4 <- subset(EngC, Rep == "4")
write.table(EngC1, file = "Engage2016_CakileRep1_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(EngC2, file = "Engage2016_CakileRep2_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(EngC3, file = "Engage2016_CakileRep3_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(EngC4, file = "Engage2016_CakileRep4_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)


EngC234 <- subset(EngC, Rep != "1")
write.table(EngC234, file = "Engage2016_CakileRep234_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)


# Set Fuction to Summarize data
# Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#   data: a data frame.
#   measurevar: the name of a column that contains the variable to be summariezed
#   groupvars: a vector containing names of columns that contain grouping variables
#   na.rm: a boolean that indicates whether to ignore NA's
#   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}


#*********************************All Reps*****************************************
#Distribution of Data
#SizeH
par(mfrow = c(1,1))
hist(EngC$SizeH) #good shape, slight skew right *
EngC$LogSizeH <- log10(EngC$SizeH+1)
hist(EngC$LogSizeH) #not better
EngC$sqrtSizeH <- sqrt(EngC$SizeH+0.5)
hist(EngC$sqrtSizeH) #same shape as raw, and also skewed right
EngC$rankSizeH <- rank(EngC$SizeH)

#BranchesH
par(mfrow = c(1,1))
hist(EngC$BranchesH) #poor shape, one tall column at left and small columns trail to right
EngC$LogBranchesH <- log10(EngC$BranchesH+1)
hist(EngC$LogBranchesH) #not better than raw
EngC$sqrtBranchesH <- sqrt(EngC$BranchesH+0.5)
hist(EngC$sqrtBranchesH) #not better than raw
EngC$rankBranchesH <- rank(EngC$BranchesH) #*

#LTFrt
par(mfrow = c(1,1))
hist(EngC$LTFrt) #okay shape, slight skew left
EngC$LogLTFrt <- log10(EngC$LTFrt+1)
hist(EngC$LogLTFrt) #not better than raw, skew right
EngC$sqrtLTFrt <- sqrt(EngC$LTFrt+0.5)
hist(EngC$sqrtLTFrt) #better than raw, more centered and more columns *
EngC$rankLTFrt <- rank(EngC$LTFrt)

#WetWtAll
par(mfrow = c(1,1))
hist(EngC$WetWtAll) #okay shape, tallest columns are not much taller than surrounding *
EngC$LogWetWtAll <- log10(EngC$WetWtAll+1)
hist(EngC$LogWetWtAll) #better shape, but skew right
EngC$sqrtWetWtAll <- sqrt(EngC$WetWtAll+0.5)
hist(EngC$sqrtWetWtAll) #fewer columns
EngC$rankWetWtAll <- rank(EngC$WetWtAll)

#DryWtAll
par(mfrow = c(1,1))
hist(EngC$DryWtAll) #okay shape, tallest columns are not much taller than surrounding *
EngC$LogDryWtAll<- log10(EngC$DryWtAll+1)
hist(EngC$LogDryWtAll) #slightly better shape
EngC$sqrtDryWtAll <- sqrt(EngC$DryWtAll+0.5)
hist(EngC$sqrtDryWtAll) #more centered than raw, but not much better shape
EngC$rankDryWtAll <- rank(EngC$DryWtAll)
 
#WetWtFrt
par(mfrow = c(1,1))
hist(EngC$WetWtFrt) #okay shape, middle column lower than surrounding columns
EngC$LogWetWtFrt <- log10(EngC$WetWtFrt+1)
hist(EngC$LogWetWtFrt) #not better shape than raw
EngC$sqrtWetWtFrt <- sqrt(EngC$WetWtFrt+0.5)
hist(EngC$sqrtWetWtFrt) #more centered than raw and much better shape *
EngC$rankWetWtFrt <- rank(EngC$WetWtFrt)

#DryWtFrt
par(mfrow = c(1,1))
hist(EngC$DryWtFrt) #okay shape, skew left
EngC$LogDryWtFrt <- log10(EngC$DryWtFrt+1)
hist(EngC$LogDryWtFrt) #not better shape than raw
EngC$sqrtDryWtFrt <- sqrt(EngC$DryWtFrt+0.5)
hist(EngC$sqrtDryWtFrt) #more centered than raw *
EngC$rankDryWtFrt <- rank(EngC$DryWtFrt)

#WCAll
par(mfrow = c(1,1))
hist(EngC$WCAll) #okay shape, middle column is not much taller than surrounding
EngC$LogWCAll <- log10(EngC$WCAll+1)
hist(EngC$LogWCAll) #not better shape than raw, skew right
EngC$sqrtWCAll <- sqrt(EngC$WCAll+0.5)
hist(EngC$sqrtWCAll) #more centered than raw, and a bit better shape *
EngC$rankWCAll <- rank(EngC$WCAll)

#WCFrt
par(mfrow = c(1,1))
hist(EngC$WCFrt) #okay shape *
EngC$LogWCFrt <- log10(EngC$WCFrt+1)
hist(EngC$LogWCFrt) #not much better than raw
EngC$sqrtWCFrt <- sqrt(EngC$WCFrt+0.5)
hist(EngC$sqrtWCFrt) #not much better than raw
EngC$rankWCFrt <- rank(EngC$WCFrt)

write.table(EngC, file = "Engage2016_Cakile_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#********************************
#Analysis of variation between trtmts while considering rep and tray
#*******************************
#SizeH
SumEngCH<- summarySE(EngC, measurevar="SizeH", groupvars=c("Rep", "Trtmt")) 
GGEngCH <- ggplot(data=SumEngCH, aes(x=Trtmt, y=SizeH, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=SizeH-se, ymax=SizeH+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab("Stem Height at Harvest (cm)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Stem Height\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCHTSR <- lmer(SizeH~Trtmt+(1|SSID)+(1|Rep), data=EngC)
summary(lmeEngCHTSR)
lmeEngCHTS <- lmer(SizeH~Trtmt+(1|SSID), data=EngC)
anova(lmeEngCHTS, lmeEngCHTSR) #the removal of Rep was significant (p=0.00057 Chisq=11.87)
lmeEngCHTR <- lmer(SizeH~Trtmt+(1|Rep), data=EngC)
anova(lmeEngCHTR, lmeEngCHTSR) #the removal of SSID was significant (p=0.011 Chisq=6.38)
lmEngCHT <- lm(SizeH~Trtmt, data=EngC)
x <- -2*logLik(lmEngCHT, REML=T) +2*logLik(lmeEngCHTSR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=27.35, p=<0.0001, random effects of SSID and Rep are sig
AIC(lmEngCHT) #=1611.74
AIC(lmeEngCHTSR) #=1580.701
#Therefore SSID and Rep need to be included in the model as random effects
lmeEngCHSR <- update(lmeEngCHTSR,~.-Trtmt)
anova(lmeEngCHSR, lmeEngCHTSR) #the effect of trtmt is significant after considering
#the variation explained by SSID and Rep (p=0.0016 chisq=15.18)

#check assumptions of best model
RlmeEngCHTSR <- resid(lmeEngCHTSR) 
FlmeEngCHTSR <- fitted(lmeEngCHTSR)
plot(FlmeEngCHTSR, RlmeEngCHTSR) #okay scatter
abline(h=0, col=c("red"))
hist(RlmeEngCHTSR) #not bad, few columns and a slight skew right
qqnorm(RlmeEngCHTSR, main="Q-Q plot for residuals") 
qqline(RlmeEngCHTSR) #lg tail at bottom end

#outliers
RlmeEngCHTSR <- resid(lmeEngCHTSR)
SDRlmeEngCHTSR <- 3*sd(RlmeEngCHTSR)
ORlmeEngCHTSR <- ifelse(abs(RlmeEngCHTSR)>SDRlmeEngCHTSR, 1, 0)
plot(RlmeEngCHTSR, col=ORlmeEngCHTSR+1, pch=16, ylim=c(-30,30))
EngCH <- EngC[!ORlmeEngCHTSR,]
nrow(EngCH) #251 from 258

SumEngCHx<- summarySE(EngCH, measurevar="SizeH", groupvars=c("Rep", "Trtmt")) 
GGEngCHx <- ggplot(data=SumEngCHx, aes(x=Trtmt, y=SizeH, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=SizeH-se, ymax=SizeH+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8)) +
  xlab("Treatment (%)") + ylab("Stem Height at Harvest (cm)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Stem Height\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCHTSR2 <- lmer(SizeH~Trtmt+(1|SSID)+(1|Rep), data=EngCH)
summary(lmeEngCHTSR2)
lmeEngCHTS2 <- lmer(SizeH~Trtmt+(1|SSID), data=EngCH)
anova(lmeEngCHTS2, lmeEngCHTSR2) #the removal of Rep was significant (p=0.00036 Chisq=12.73)
lmeEngCHTR2 <- lmer(SizeH~Trtmt+(1|Rep), data=EngCH)
anova(lmeEngCHTR2, lmeEngCHTSR2) #the removal of SSID was significant (p=0.0086 Chisq=6.899)
lmEngCHT2 <- lm(SizeH~Trtmt, data=EngCH)
x <- -2*logLik(lmEngCHT2, REML=T) +2*logLik(lmeEngCHTSR2, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=29.14, p=<0.0001, random effects of SSID and Rep are sig
AIC(lmEngCHT2) #=1555.66
AIC(lmeEngCHTSR2) #=1522.84
#Therefore SSID and Rep need to be included in the model as random effects
lmeEngCHSR2 <- update(lmeEngCHTSR2,~.-Trtmt)
anova(lmeEngCHSR2, lmeEngCHTSR2) #the effect of trtmt is significant after considering
#the variation explained by SSID and Rep (p=0.0015 chisq=15.38)

#check assumptions of best model
RlmeEngCHTSR2 <- resid(lmeEngCHTSR2) 
FlmeEngCHTSR2 <- fitted(lmeEngCHTSR2)
plot(FlmeEngCHTSR2, RlmeEngCHTSR2) #okay scatter
abline(h=0, col=c("red"))
hist(RlmeEngCHTSR2) #not bad, not many columns and a slight skew right
qqnorm(RlmeEngCHTSR2, main="Q-Q plot for residuals") 
qqline(RlmeEngCHTSR2) # still tail at bottom end

#*********************
#BranchesH
SumEngCBH<- summarySE(EngC, measurevar="rankBranchesH", groupvars=c("Rep", "Trtmt")) 
GGEngCBH <- ggplot(data=SumEngCBH, aes(x=Trtmt, y=rankBranchesH, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=rankBranchesH-se, ymax=rankBranchesH+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab("Ranked No. of Branches at Harvest (cm)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile No.Branches\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCBHTSR <- lmer(rankBranchesH~Trtmt+(1|SSID)+(1|Rep), data=EngC)
summary(lmeEngCBHTSR)
lmeEngCBHTS <- lmer(rankBranchesH~Trtmt+(1|SSID), data=EngC)
anova(lmeEngCBHTS, lmeEngCBHTSR) #the removal of Rep was significant (p=0.001 Chisq=10.82)
lmeEngCBHTR <- lmer(rankBranchesH~Trtmt+(1|Rep), data=EngC)
anova(lmeEngCBHTR, lmeEngCBHTSR) #the removal of SSID was not significant (p=0.486 Chisq=0.486)
lmEngCBHT <- lm(rankBranchesH~Trtmt, data=EngC)
x <- -2*logLik(lmEngCBHT, REML=T) +2*logLik(lmeEngCBHTR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=16.35, p=0.00096, random effect of Rep was sig
AIC(lmEngCBHT) #=2957.64
AIC(lmeEngCBHTR) #=2918.23
#Therefore Rep needs to be included in the model as random effect
lmeEngCBHR <- update(lmeEngCBHTR,~.-Trtmt)
anova(lmeEngCBHR, lmeEngCBHTR) #the effect of trtmt is not significant after considering
#the variation explained by SSID and Rep (p=0.112 chisq=5.998)

#check assumptions of best model
RlmeEngCBHTR <- resid(lmeEngCBHTR) 
FlmeEngCBHTR <- fitted(lmeEngCBHTR)
plot(FlmeEngCBHTR, RlmeEngCBHTR) #okay scatter, but shows trend from left to right
abline(h=0, col=c("red"))
hist(RlmeEngCBHTR) #okay.. few columns and the middle columns are all the same height
qqnorm(RlmeEngCBHTR, main="Q-Q plot for residuals") 
qqline(RlmeEngCBHTR) #tails at either end

#outliers
RlmeEngCBHTR <- resid(lmeEngCBHTR)
SDRlmeEngCBHTR <- 3*sd(RlmeEngCBHTR)
ORlmeEngCBHTR <- ifelse(abs(RlmeEngCBHTR)>SDRlmeEngCBHTR, 1, 0)
plot(RlmeEngCBHTR, col=ORlmeEngCBHTR+1, pch=16, ylim=c(-200,200))
EngCBH <- EngC[!ORlmeEngCBHTR,]
nrow(EngCBH) #258 from 258... no outliers

#*********************
#LTFrt
SumEngCFr<- summarySE(EngC, measurevar="sqrtLTFrt", groupvars=c("Rep", "Trtmt")) 
GGEngCFr <- ggplot(data=SumEngCFr, aes(x=Trtmt, y=sqrtLTFrt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtLTFrt-se, ymax=sqrtLTFrt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Lifetime~Fruit)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Lifetime Fruit\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCFrTSR <- lmer(sqrtLTFrt~Trtmt+(1|SSID)+(1|Rep), data=EngC)
summary(lmeEngCFrTSR)
lmeEngCFrTS <- lmer(sqrtLTFrt~Trtmt+(1|SSID), data=EngC)
anova(lmeEngCFrTS, lmeEngCFrTSR) #the removal of Rep was significant (p=<0.0001 Chisq=15.71)
lmeEngCFrTR <- lmer(sqrtLTFrt~Trtmt+(1|Rep), data=EngC)
anova(lmeEngCFrTR, lmeEngCFrTSR) #the removal of SSID was not significant (p=1 Chisq=0)
lmEngCFrT <- lm(sqrtLTFrt~Trtmt, data=EngC)
x <- -2*logLik(lmEngCFrT, REML=T) +2*logLik(lmeEngCFrTR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=20.91, p=0.00011, random effect of Rep was sig
AIC(lmEngCFrT) #=721.98
AIC(lmeEngCFrTR) #=710.86
#Therefore Rep needs to be included in the model as random effect
lmeEngCFrR <- update(lmeEngCFrTR,~.-Trtmt)
anova(lmeEngCFrR, lmeEngCFrTR) #the effect of trtmt is marginally non significant after considering
#the variation explained by SSID and Rep (p=0.0703 chisq=7.051)

#check assumptions of best model
RlmeEngCFrTR <- resid(lmeEngCFrTR) 
FlmeEngCFrTR <- fitted(lmeEngCFrTR)
plot(FlmeEngCFrTR, RlmeEngCFrTR) #okay scatter, small gap just left of middle
abline(h=0, col=c("red"))
hist(RlmeEngCFrTR) #okay.. few columns
qqnorm(RlmeEngCFrTR, main="Q-Q plot for residuals") 
qqline(RlmeEngCFrTR) #tails at either end

#outliers
RlmeEngCFrTR <- resid(lmeEngCFrTR)
SDRlmeEngCFrTR <- 3*sd(RlmeEngCFrTR)
ORlmeEngCFrTR <- ifelse(abs(RlmeEngCFrTR)>SDRlmeEngCFrTR, 1, 0)
plot(RlmeEngCFrTR, col=ORlmeEngCFrTR+1, pch=16, ylim=c(-10,10))
EngCFr <- EngC[!ORlmeEngCFrTR,]
nrow(EngCFr) #253 from 258

SumEngCFr2<- summarySE(EngCFr, measurevar="sqrtLTFrt", groupvars=c("Rep", "Trtmt")) 
GGEngCFr2 <- ggplot(data=SumEngCFr2, aes(x=Trtmt, y=sqrtLTFrt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtLTFrt-se, ymax=sqrtLTFrt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Lifetime~Fruit)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Lifetime Fruit\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCFrTSR2 <- lmer(sqrtLTFrt~Trtmt+(1|SSID)+(1|Rep), data=EngCFr)
summary(lmeEngCFrTSR2)
lmeEngCFrTS2 <- lmer(sqrtLTFrt~Trtmt+(1|SSID), data=EngCFr)
anova(lmeEngCFrTS2, lmeEngCFrTSR2) #the removal of Rep was significant (p=0.00012 Chisq=14.79)
lmeEngCFrTR2 <- lmer(sqrtLTFrt~Trtmt+(1|Rep), data=EngCFr)
anova(lmeEngCFrTR2, lmeEngCFrTSR2) #the removal of SSID was not significant (p=1 Chisq=0)
lmEngCFrT2 <- lm(sqrtLTFrt~Trtmt, data=EngCFr)
x <- -2*logLik(lmEngCFrT2, REML=T) +2*logLik(lmeEngCFrTR2, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=19.81, p=0.00019, random effect of Rep was sig
AIC(lmEngCFrT2) #=705.705
AIC(lmeEngCFrTR2) #=695.61
#Therefore Rep needs to be included in the model as random effect
lmeEngCFrR2 <- update(lmeEngCFrTR2,~.-Trtmt)
anova(lmeEngCFrR2, lmeEngCFrTR2) #the effect of trtmt is marginally non significant after considering
#the variation explained by SSID and Rep (p=0.072 chisq=7.002)

#check assumptions of best model
RlmeEngCFrTR2 <- resid(lmeEngCFrTR2) 
FlmeEngCFrTR2 <- fitted(lmeEngCFrTR2)
plot(FlmeEngCFrTR2, RlmeEngCFrTR2) #okay scatter, small gap just left of middle
abline(h=0, col=c("red"))
hist(RlmeEngCFrTR2) #okay.. few columns
qqnorm(RlmeEngCFrTR2, main="Q-Q plot for residuals") 
qqline(RlmeEngCFrTR2) #tails at either end... no change from when outliers included.


#***************************
#WetWtAll
SumEngCWWA<- summarySE(EngC, measurevar="WetWtAll", groupvars=c("Rep", "Trtmt")) 
GGEngCWWA <- ggplot(data=SumEngCWWA, aes(x=Trtmt, y=WetWtAll, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=WetWtAll-se, ymax=WetWtAll+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab("Plant Weight\nat Harvest (g)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Whole Wet Weight\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCWWATSR <- lmer(WetWtAll~Trtmt+(1|SSID)+(1|Rep), data=EngC)
summary(lmeEngCWWATSR)
lmeEngCWWATS <- lmer(WetWtAll~Trtmt+(1|SSID), data=EngC)
anova(lmeEngCWWATS, lmeEngCWWATSR) #the removal of Rep was significant (p=<0.0001 Chisq=23.027)
lmeEngCWWATR <- lmer(WetWtAll~Trtmt+(1|Rep), data=EngC)
anova(lmeEngCWWATR, lmeEngCWWATSR) #the removal of SSID was not significant (p=0.135 Chisq=2.23)
lmEngCWWAT <- lm(WetWtAll~Trtmt, data=EngC)
x <- -2*logLik(lmEngCWWAT, REML=T) +2*logLik(lmeEngCWWATR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=37.039, p=<0.0001, random effect of Rep was sig
AIC(lmEngCWWAT) #=1016.735
AIC(lmeEngCWWATR) #=984.359
#Therefore Rep needs to be included in the model as random effect
lmeEngCWWAR <- update(lmeEngCWWATR,~.-Trtmt)
anova(lmeEngCWWAR, lmeEngCWWATR) #the effect of trtmt is non significant after considering
#the variation explained by SSID and Rep (p=0.1326 chisq=5.6037)

#check assumptions of best model
RlmeEngCWWATR <- resid(lmeEngCWWATR) 
FlmeEngCWWATR <- fitted(lmeEngCWWATR)
plot(FlmeEngCWWATR, RlmeEngCWWATR) #okay scatter, small gap just left of middle
abline(h=0, col=c("red"))
hist(RlmeEngCWWATR) #good
qqnorm(RlmeEngCWWATR, main="Q-Q plot for residuals") 
qqline(RlmeEngCWWATR) #slight tail at either end, but good

#outliers
RlmeEngCWWATR <- resid(lmeEngCWWATR)
SDRlmeEngCWWATR <- 3*sd(RlmeEngCWWATR)
ORlmeEngCWWATR <- ifelse(abs(RlmeEngCWWATR)>SDRlmeEngCWWATR, 1, 0)
plot(RlmeEngCWWATR, col=ORlmeEngCWWATR+1, pch=16, ylim=c(-10,10))
EngCWWA <- EngC[!ORlmeEngCWWATR,]
nrow(EngCWWA) #258 from 258... no outliers


#***************************
#DryWtAll
SumEngCDWA<- summarySE(EngC, measurevar="DryWtAll", groupvars=c("Rep", "Trtmt")) 
GGEngCDWA <- ggplot(data=SumEngCDWA, aes(x=Trtmt, y=DryWtAll, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=DryWtAll-se, ymax=DryWtAll+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab("Plant Dry Weight (g)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Whole Dry Weight\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCDWATSR <- lmer(DryWtAll~Trtmt+(1|SSID)+(1|Rep), data=EngC)
summary(lmeEngCDWATSR)
lmeEngCDWATS <- lmer(DryWtAll~Trtmt+(1|SSID), data=EngC)
anova(lmeEngCDWATS, lmeEngCDWATSR) #the removal of Rep was significant (p=<0.0001 Chisq=25.88)
lmeEngCDWATR <- lmer(DryWtAll~Trtmt+(1|Rep), data=EngC)
anova(lmeEngCDWATR, lmeEngCDWATSR) #the removal of SSID was not significant (p=0.795 Chisq=0.067)
lmEngCDWAT <- lm(DryWtAll~Trtmt, data=EngC)
x <- -2*logLik(lmEngCDWAT, REML=T) +2*logLik(lmeEngCDWATR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=38.72, p=<0.0001, random effect of Rep was sig
AIC(lmEngCDWAT) #=179.37
AIC(lmeEngCDWATR) #=159.88
#Therefore Rep needs to be included in the model as random effect
lmeEngCDWAR <- update(lmeEngCDWATR,~.-Trtmt)
anova(lmeEngCDWAR, lmeEngCDWATR) #the effect of trtmt is non significant after considering
#the variation explained by SSID and Rep (p=0.127 chisq=5.67)

#check assumptions of best model
RlmeEngCDWATR <- resid(lmeEngCDWATR) 
FlmeEngCDWATR <- fitted(lmeEngCDWATR)
plot(FlmeEngCDWATR, RlmeEngCDWATR) #okay scatter
abline(h=0, col=c("red"))
hist(RlmeEngCDWATR) #good
qqnorm(RlmeEngCDWATR, main="Q-Q plot for residuals") 
qqline(RlmeEngCDWATR) #slight tail at bottom end, but good

#outliers
RlmeEngCDWATR <- resid(lmeEngCDWATR)
SDRlmeEngCDWATR <- 3*sd(RlmeEngCDWATR)
ORlmeEngCDWATR <- ifelse(abs(RlmeEngCDWATR)>SDRlmeEngCDWATR, 1, 0)
plot(RlmeEngCDWATR, col=ORlmeEngCDWATR+1, pch=16, ylim=c(-1,1))
EngCDWA <- EngC[!ORlmeEngCDWATR,]
nrow(EngCDWA) #258 from 258... no outliers


#***************************
#WetWtFrt
SumEngCWWF<- summarySE(EngC, measurevar="sqrtWetWtFrt", groupvars=c("Rep", "Trtmt")) 
GGEngCWWF <- ggplot(data=SumEngCWWF, aes(x=Trtmt, y=sqrtWetWtFrt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtWetWtFrt-se, ymax=sqrtWetWtFrt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Fruit~Harvest~Weight~(g))))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Fruit Wet Weight\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCWWFTSR <- lmer(sqrtWetWtFrt~Trtmt+(1|SSID)+(1|Rep), data=EngC)
summary(lmeEngCWWFTSR)
lmeEngCWWFTS <- lmer(sqrtWetWtFrt~Trtmt+(1|SSID), data=EngC)
anova(lmeEngCWWFTS, lmeEngCWWFTSR) #the removal of Rep was significant (p=<0.0001 Chisq=23.66)
lmeEngCWWFTR <- lmer(sqrtWetWtFrt~Trtmt+(1|Rep), data=EngC)
anova(lmeEngCWWFTR, lmeEngCWWFTSR) #the removal of SSID was not significant (p=0.1247 Chisq=2.36)
lmEngCWWFT <- lm(sqrtWetWtFrt~Trtmt, data=EngC)
x <- -2*logLik(lmEngCWWFT, REML=T) +2*logLik(lmeEngCWWFTR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=40.44, p=<0.0001, random effect of Rep was sig
AIC(lmEngCWWFT) #=199.99
AIC(lmeEngCWWFTR) #=178.41
#Therefore Rep needs to be included in the model as random effect
lmeEngCWWFR <- update(lmeEngCWWFTR,~.-Trtmt)
anova(lmeEngCWWFR, lmeEngCWWFTR) #the effect of trtmt is non significant after considering
#the variation explained by SSID and Rep (p=0.42 chisq=2.81)

#check assumptions of best model
RlmeEngCWWFTR <- resid(lmeEngCWWFTR) 
FlmeEngCWWFTR <- fitted(lmeEngCWWFTR)
plot(FlmeEngCWWFTR, RlmeEngCWWFTR) #okay scatter, small gap just left of middle
abline(h=0, col=c("red"))
hist(RlmeEngCWWFTR) #good
qqnorm(RlmeEngCWWFTR, main="Q-Q plot for residuals") 
qqline(RlmeEngCWWFTR) #tail at bottom end

#outliers
RlmeEngCWWFTR <- resid(lmeEngCWWFTR)
SDRlmeEngCWWFTR <- 3*sd(RlmeEngCWWFTR)
ORlmeEngCWWFTR <- ifelse(abs(RlmeEngCWWFTR)>SDRlmeEngCWWFTR, 1, 0)
plot(RlmeEngCWWFTR, col=ORlmeEngCWWFTR+1, pch=16, ylim=c(-1,1))
EngCWWF <- EngC[!ORlmeEngCWWFTR,]
nrow(EngCWWF) #258 from 258... no outliers

#***************************
#DryWtFrt
SumEngCDWF<- summarySE(EngC, measurevar="sqrtDryWtFrt", groupvars=c("Rep", "Trtmt")) 
GGEngCDWF <- ggplot(data=SumEngCDWF, aes(x=Trtmt, y=sqrtDryWtFrt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtDryWtFrt-se, ymax=sqrtDryWtFrt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Fruit~Dry~Weight~(g))))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Fruit Dry Weight\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCDWFTSR <- lmer(sqrtDryWtFrt~Trtmt+(1|SSID)+(1|Rep), data=EngC)
summary(lmeEngCDWFTSR)
lmeEngCDWFTS <- lmer(sqrtDryWtFrt~Trtmt+(1|SSID), data=EngC)
anova(lmeEngCDWFTS, lmeEngCDWFTSR) #the removal of Rep was significant (p=<0.0001 Chisq=30.79)
lmeEngCDWFTR <- lmer(sqrtDryWtFrt~Trtmt+(1|Rep), data=EngC)
anova(lmeEngCDWFTR, lmeEngCDWFTSR) #the removal of SSID was not significant (p=0.701 Chisq=0.147)
lmEngCDWFT <- lm(sqrtDryWtFrt~Trtmt, data=EngC)
x <- -2*logLik(lmEngCDWFT, REML=T) +2*logLik(lmeEngCDWFTR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=50.65, p=<0.0001, random effect of Rep was sig
AIC(lmEngCDWFT) #=-425.786
AIC(lmeEngCDWFTR) #=-446.687
#Therefore Rep needs to be included in the model as random effect
lmeEngCDWFR <- update(lmeEngCDWFTR,~.-Trtmt)
anova(lmeEngCDWFR, lmeEngCDWFTR) #the effect of trtmt is non significant after considering
#the variation explained by SSID and Rep (p=0.37 chisq=3.14)

#check assumptions of best model
RlmeEngCDWFTR <- resid(lmeEngCDWFTR) 
FlmeEngCDWFTR <- fitted(lmeEngCDWFTR)
plot(FlmeEngCDWFTR, RlmeEngCDWFTR) #okay scatter
abline(h=0, col=c("red"))
hist(RlmeEngCDWFTR) #good
qqnorm(RlmeEngCDWFTR, main="Q-Q plot for residuals") 
qqline(RlmeEngCDWFTR) #slight tail at either end, but good

#outliers
RlmeEngCDWFTR <- resid(lmeEngCDWFTR)
SDRlmeEngCDWFTR <- 3*sd(RlmeEngCDWFTR)
ORlmeEngCDWFTR <- ifelse(abs(RlmeEngCDWFTR)>SDRlmeEngCDWFTR, 1, 0)
plot(RlmeEngCDWFTR, col=ORlmeEngCDWFTR+1, pch=16, ylim=c(-1,1))
EngCDWF <- EngC[!ORlmeEngCDWFTR,]
nrow(EngCDWF) #257 from 258... one outlier... likely not worth running models again without this outlier

#***************************
#WCAll
EngC$WCAll2 <- ((EngC$WCAll/EngC$WetWtAll)*100)
hist(EngC$WCAll2) #good as raw
write.table(EngC, file = "Engage2016_Cakile_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)


SumEngCWCA<- summarySE(EngC, measurevar="sqrtWCAll", groupvars=c("Rep", "Trtmt")) 
GGEngCWCA <- ggplot(data=SumEngCWCA, aes(x=Trtmt, y=sqrtWCAll, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtWCAll-se, ymax=sqrtWCAll+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(sqrt(Water~Content~(g))))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Water Content\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

SumEngCWCA2<- summarySE(EngC, measurevar="WCAll2", groupvars=c("Rep", "Trtmt")) 
GGEngCWCA2 <- ggplot(data=SumEngCWCA2, aes(x=Trtmt, y=WCAll2, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=WCAll2-se, ymax=WCAll2+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab("Water Content (%)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile % Water Content\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCWCA2TSR <- lmer(WCAll2~Trtmt+(1|SSID)+(1|Rep), data=EngC)
summary(lmeEngCWCA2TSR)
lmeEngCWCA2TS <- lmer(WCAll2~Trtmt+(1|SSID), data=EngC)
anova(lmeEngCWCA2TS, lmeEngCWCA2TSR) #the removal of Rep was marginally non significant (p=0.056 Chisq=3.65)
lmeEngCWCA2TR <- lmer(WCAll2~Trtmt+(1|Rep), data=EngC)
anova(lmeEngCWCA2TR, lmeEngCWCA2TSR) #the removal of SSID was significant (p=0.0021 Chisq=9.45)
lmEngCWCA2T <- lm(WCAll2~Trtmt, data=EngC)
x <- -2*logLik(lmEngCWCA2T, REML=T) +2*logLik(lmeEngCWCA2TS, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=14.31, p=0.0025, random effect of SSID was sig
AIC(lmEngCWCA2T) #=1105.407
AIC(lmeEngCWCA2TS) #=1094.215
#Therefore SSID needs to be included in the model as random effect
lmeEngCWCA2S <- update(lmeEngCWCA2TS,~.-Trtmt)
anova(lmeEngCWCA2S, lmeEngCWCA2TS) #the effect of trtmt is significant after considering
#the variation explained by SSID and Rep (p=0.0078 chisq=11.89)

#check assumptions of best model
RlmeEngCWCA2TS <- resid(lmeEngCWCA2TS) 
FlmeEngCWCA2TS <- fitted(lmeEngCWCA2TS)
plot(FlmeEngCWCA2TS, RlmeEngCWCA2TS) #okay scatter, more centred along x axis
abline(h=0, col=c("red"))
hist(RlmeEngCWCA2TS) #good
qqnorm(RlmeEngCWCA2TS, main="Q-Q plot for residuals") 
qqline(RlmeEngCWCA2TS) #slight tail at either end, but good

#outliers
RlmeEngCWCA2TS <- resid(lmeEngCWCA2TS)
SDRlmeEngCWCA2TS <- 3*sd(RlmeEngCWCA2TS)
ORlmeEngCWCA2TS <- ifelse(abs(RlmeEngCWCA2TS)>SDRlmeEngCWCA2TS, 1, 0)
plot(RlmeEngCWCA2TS, col=ORlmeEngCWCA2TS+1, pch=16, ylim=c(-10,10))
EngCWCA2 <- EngC[!ORlmeEngCWCA2TS,]
nrow(EngCWCA2) #252 from 258

SumEngCWCA22<- summarySE(EngCWCA2, measurevar="WCAll2", groupvars=c("Rep", "Trtmt")) 
GGEngCWCA22 <- ggplot(data=SumEngCWCA22, aes(x=Trtmt, y=WCAll2, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=WCAll2-se, ymax=WCAll2+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab("Water Content (%)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile % Water Content\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCWCA2TSR2 <- lmer(WCAll2~Trtmt+(1|SSID)+(1|Rep), data=EngCWCA2)
summary(lmeEngCWCA2TSR2)
lmeEngCWCA2TS2 <- lmer(WCAll2~Trtmt+(1|SSID), data=EngCWCA2)
anova(lmeEngCWCA2TS2, lmeEngCWCA2TSR2) #the removal of Rep was marginally non significant (p=0.059 Chisq=3.57)
lmeEngCWCA2TR2 <- lmer(WCAll2~Trtmt+(1|Rep), data=EngCWCA2)
anova(lmeEngCWCA2TR2, lmeEngCWCA2TSR2) #the removal of SSID was significant (p=0.0053 Chisq=7.76)
lmEngCWCA2T2 <- lm(WCAll2~Trtmt, data=EngCWCA2)
x <- -2*logLik(lmEngCWCA2T2, REML=T) +2*logLik(lmeEngCWCA2TS2, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=11.21, p=0.0106, random effect of SSID was sig
AIC(lmEngCWCA2T2) #=1077.98
AIC(lmeEngCWCA2TS2) #=1069.866
#Therefore SSID needs to be included in the model as random effect
lmeEngCWCA2S2 <- update(lmeEngCWCA2TS2,~.-Trtmt)
anova(lmeEngCWCA2S2, lmeEngCWCA2TS2) #the effect of trtmt is significant after considering
#the variation explained by SSID and Rep (p=0.0108 chisq=11.18)

#check assumptions of best model
RlmeEngCWCA2TS2 <- resid(lmeEngCWCA2TS2) 
FlmeEngCWCA2TS2 <- fitted(lmeEngCWCA2TS2)
plot(FlmeEngCWCA2TS2, RlmeEngCWCA2TS2) #okay scatter, scatter better than with outliers
abline(h=0, col=c("red"))
hist(RlmeEngCWCA2TS2) #good
qqnorm(RlmeEngCWCA2TS2, main="Q-Q plot for residuals") 
qqline(RlmeEngCWCA2TS2) #slight tail at either end, but good


#***************************
#WCFrt
EngC$WCFrt2 <- ((EngC$WCFrt/EngC$WetWtFrt)*100)
hist(EngC$WCFrt2) #good as raw
write.table(EngC, file = "Engage2016_Cakile_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)


SumEngCWCF<- summarySE(EngC, measurevar="WCFrt", groupvars=c("Rep", "Trtmt")) 
GGEngCWCF <- ggplot(data=SumEngCWCF, aes(x=Trtmt, y=WCFrt, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=WCFrt-se, ymax=WCFrt+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab("Water Content in Fruit (g)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Fruit Water Content\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

SumEngCWCF2<- summarySE(EngC, measurevar="WCFrt2", groupvars=c("Rep", "Trtmt")) 
GGEngCWCF2 <- ggplot(data=SumEngCWCF2, aes(x=Trtmt, y=WCFrt2, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=WCFrt2-se, ymax=WCFrt2+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab("Water Content in Fruit (%)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile % Water Content in Fruit\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCWCF2TSR <- lmer(WCFrt2~Trtmt+(1|SSID)+(1|Rep), data=EngC)
summary(lmeEngCWCF2TSR)
lmeEngCWCF2TS <- lmer(WCFrt2~Trtmt+(1|SSID), data=EngC)
anova(lmeEngCWCF2TS, lmeEngCWCF2TSR) #the removal of Rep was non significant (p=0.3125 Chisq=1.02)
lmeEngCWCF2TR <- lmer(WCFrt2~Trtmt+(1|Rep), data=EngC)
anova(lmeEngCWCF2TR, lmeEngCWCF2TSR) #the removal of SSID was significant (p=<0.0001 Chisq=19.554)
lmEngCWCF2T <- lm(WCFrt2~Trtmt, data=EngC)
x <- -2*logLik(lmEngCWCF2T, REML=T) +2*logLik(lmeEngCWCF2TS, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=23.45, p=<0.0001, random effect of SSID was sig
AIC(lmEngCWCF2T) #=1293.38
AIC(lmeEngCWCF2TS) #=1268.58
#Therefore SSID needs to be included in the model as random effect
lmeEngCWCF2S <- update(lmeEngCWCF2TS,~.-Trtmt)
anova(lmeEngCWCF2S, lmeEngCWCF2TS) #the effect of trtmt is non significant after considering
#the variation explained by SSID and Rep (p=0.21 chisq=4.51)

#check assumptions of best model
RlmeEngCWCF2TS <- resid(lmeEngCWCF2TS) 
FlmeEngCWCF2TS <- fitted(lmeEngCWCF2TS)
plot(FlmeEngCWCF2TS, RlmeEngCWCF2TS) #okay scatter, slight skew right
abline(h=0, col=c("red"))
hist(RlmeEngCWCF2TS) #good
qqnorm(RlmeEngCWCF2TS, main="Q-Q plot for residuals") 
qqline(RlmeEngCWCF2TS) #slight tail at either end, but good

#outliers
RlmeEngCWCF2TS <- resid(lmeEngCWCF2TS)
SDRlmeEngCWCF2TS <- 3*sd(RlmeEngCWCF2TS)
ORlmeEngCWCF2TS <- ifelse(abs(RlmeEngCWCF2TS)>SDRlmeEngCWCF2TS, 1, 0)
plot(RlmeEngCWCF2TS, col=ORlmeEngCWCF2TS+1, pch=16, ylim=c(-10,10))
EngCWCF2 <- EngC[!ORlmeEngCWCF2TS,]
nrow(EngCWCF2) #255 from 258

SumEngCWCF22<- summarySE(EngCWCF2, measurevar="WCFrt2", groupvars=c("Rep", "Trtmt")) 
GGEngCWCF22 <- ggplot(data=SumEngCWCF22, aes(x=Trtmt, y=WCFrt2, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=WCFrt2-se, ymax=WCFrt2+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab("Water Content in Fruit (%)") +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile % Water Content in Fruit\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngCWCF2TSR2 <- lmer(WCFrt2~Trtmt+(1|SSID)+(1|Rep), data=EngCWCF2)
summary(lmeEngCWCF2TSR2)
lmeEngCWCF2TS2 <- lmer(WCFrt2~Trtmt+(1|SSID), data=EngCWCF2)
anova(lmeEngCWCF2TS2, lmeEngCWCF2TSR2) #the removal of Rep was non significant (p=0.3006 Chisq=1.071)
lmeEngCWCF2TR2 <- lmer(WCFrt2~Trtmt+(1|Rep), data=EngCWCF2)
anova(lmeEngCWCF2TR2, lmeEngCWCF2TSR2) #the removal of SSID was significant (p=<0.0001 Chisq=18.15)
lmEngCWCF2T2 <- lm(WCFrt2~Trtmt, data=EngCWCF2)
x <- -2*logLik(lmEngCWCF2T2, REML=T) +2*logLik(lmeEngCWCF2TS2, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=21.801, p=<0.0001, random effect of SSID was sig
AIC(lmEngCWCF2T2) #=1274.66
AIC(lmeEngCWCF2TS2) #=1251.48
#Therefore SSID needs to be included in the model as random effect
lmeEngCWCF2S2 <- update(lmeEngCWCF2TS2,~.-Trtmt)
anova(lmeEngCWCF2S2, lmeEngCWCF2TS2) #the effect of trtmt is non significant after considering
#the variation explained by SSID and Rep (p=0.2312 chisq=4.2961)

#check assumptions of best model
RlmeEngCWCF2TS2 <- resid(lmeEngCWCF2TS2) 
FlmeEngCWCF2TS2 <- fitted(lmeEngCWCF2TS2)
plot(FlmeEngCWCF2TS2, RlmeEngCWCF2TS2) #okay scatter, slight skew right
abline(h=0, col=c("red"))
hist(RlmeEngCWCF2TS2) #good
qqnorm(RlmeEngCWCF2TS2, main="Q-Q plot for residuals") 
qqline(RlmeEngCWCF2TS2) #slight tail at either end, but good

#**************************Only Reps 2, 3, 4 ****************************
#Distribution
#SizeFlw
par(mfrow = c(1,1))
hist(EngC234$SizeFlw) #okay shape, few columns
EngC234$LogSizeFlw <- log10(EngC234$SizeFlw+1)
hist(EngC234$LogSizeFlw) #better than raw *
EngC234$sqrtSizeFlw <- sqrt(EngC234$SizeFlw+0.5)
hist(EngC234$sqrtSizeFlw) #not much better than raw, more centred
EngC234$rankSizeFlw <- rank(EngC234$SizeFlw)

hist(EngC234SFlwx$SizeFlw) #okay shape, few columns
EngC234SFlwx$LogSizeFlw <- log10(EngC234SFlwx$SizeFlw+1)
hist(EngC234SFlwx$LogSizeFlw) #better than raw *
EngC234SFlwx$sqrtSizeFlw <- sqrt(EngC234SFlwx$SizeFlw+0.5)
hist(EngC234SFlwx$sqrtSizeFlw) #not much better than raw, more centred
EngC234SFlwx$rankSizeFlw <- rank(EngC234SFlwx$SizeFlw)

#DFlw
par(mfrow = c(1,1))
hist(EngC234$DFlw) #not great shape, few columns
EngC234$LogDFlw <- log10(EngC234$DFlw+1)
hist(EngC234$LogDFlw) #bit better than raw
EngC234$sqrtDFlw <- sqrt(EngC234$DFlw+0.5)
hist(EngC234$sqrtDFlw) #more columns and okay shape *
EngC234$rankDFlw <- rank(EngC234$DFlw)

hist(EngC234DFlwx$DFlw) #okay shape, few columns
EngC234DFlwx$LogDFlw <- log10(EngC234DFlwx$DFlw+1)
hist(EngC234DFlwx$LogDFlw) #bit better than raw
EngC234DFlwx$sqrtDFlw <- sqrt(EngC234DFlwx$DFlw+0.5)
hist(EngC234DFlwx$sqrtDFlw) #more columns and okay shape *
EngC234DFlwx$rankDFlw <- rank(EngC234DFlwx$DFlw)

#FlwDuration
par(mfrow = c(1,1))
hist(EngC234$FlwDuration) #not great shape, few columns
EngC234$LogFlwDuration <- log10(EngC234$FlwDuration+1)
hist(EngC234$LogFlwDuration) #not better than raw
EngC234$sqrtFlwDuration <- sqrt(EngC234$FlwDuration+0.5)
hist(EngC234$sqrtFlwDuration) #more columns but not much shape
EngC234$rankFlwDuration <- rank(EngC234$FlwDuration) #*

hist(EngC234FlwDx$FlwDuration) #not great shape, few columns
EngC234FlwDx$LogFlwDuration <- log10(EngC234FlwDx$FlwDuration+1)
hist(EngC234FlwDx$LogFlwDuration) #not better than raw
EngC234FlwDx$sqrtFlwDuration <- sqrt(EngC234FlwDx$FlwDuration+0.5)
hist(EngC234FlwDx$sqrtFlwDuration) #more columns but not much shape
EngC234FlwDx$rankFlwDuration <- rank(EngC234FlwDx$FlwDuration) #*

write.table(EngC234, file = "Engage2016_CakileRep234_set.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#*************************************
#Analysis of variation between trtmts while considering rep and tray
#*******************************
#SizeFlw
EngC234SFlwx <- EngC234[!rowSums(is.na(EngC234["SizeFlw"])),]
                          
SumEngC234HFlw<- summarySE(EngC234SFlwx, measurevar="LogSizeFlw", groupvars=c("Rep", "Trtmt")) 
GGEngC234HFlw <- ggplot(data=SumEngC234HFlw, aes(x=Trtmt, y=LogSizeFlw, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=LogSizeFlw-se, ymax=LogSizeFlw+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(Log[10]~Height~at~Flowering~(cm)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Height at Flowering\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngC234HFlwTSR <- lmer(LogSizeFlw~Trtmt+(1|SSID)+(1|Rep), data=EngC234SFlwx)
summary(lmeEngC234HFlwTSR)
lmeEngC234HFlwTS <- lmer(LogSizeFlw~Trtmt+(1|SSID), data=EngC234SFlwx)
anova(lmeEngC234HFlwTS, lmeEngC234HFlwTSR) #the removal of Rep was significant (p=<0.0001 Chisq=16.79)
lmeEngC234HFlwTR <- lmer(LogSizeFlw~Trtmt+(1|Rep), data=EngC234SFlwx)
anova(lmeEngC234HFlwTR, lmeEngC234HFlwTSR) #the removal of SSID was non significant (p=<0.1641 Chisq=1.94)
lmEngC234HFlwT <- lm(LogSizeFlw~Trtmt, data=EngC234SFlwx)
x <- -2*logLik(lmEngC234HFlwT, REML=T) +2*logLik(lmeEngC234HFlwTR, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=14.66, p=0.0021, random effect of Rep was sig
AIC(lmEngC234HFlwT) #=-366.76
AIC(lmeEngC234HFlwTR) #=-369.32
#Therefore Rep needs to be included in the model as random effect
lmeEngC234HFlwR <- update(lmeEngC234HFlwTR,~.-Trtmt)
anova(lmeEngC234HFlwR, lmeEngC234HFlwTR) #the effect of trtmt is significant after considering
#the variation explained by SSID and Rep (p=0.0056 chisq=12.59)

#check assumptions of best model
RlmeEngC234HFlwTR <- resid(lmeEngC234HFlwTR) 
FlmeEngC234HFlwTR <- fitted(lmeEngC234HFlwTR)
plot(FlmeEngC234HFlwTR, RlmeEngC234HFlwTR) #okay scatter
abline(h=0, col=c("red"))
hist(RlmeEngC234HFlwTR) #good
qqnorm(RlmeEngC234HFlwTR, main="Q-Q plot for residuals") 
qqline(RlmeEngC234HFlwTR) #slight tail at either end, but good

#outliers
RlmeEngC234HFlwTR <- resid(lmeEngC234HFlwTR)
SDRlmeEngC234HFlwTR <- 3*sd(RlmeEngC234HFlwTR)
ORlmeEngC234HFlwTR <- ifelse(abs(RlmeEngC234HFlwTR)>SDRlmeEngC234HFlwTR, 1, 0)
plot(RlmeEngC234HFlwTR, col=ORlmeEngC234HFlwTR+1, pch=16, ylim=c(-1,1))
EngC234HFlw <- EngC234SFlwx[!ORlmeEngC234HFlwTR,]
nrow(EngC234HFlw) #187 from 188... one less outlier will unlikely change the results

#*******************************
#DFlw
EngC234DFlwx <- EngC234[!rowSums(is.na(EngC234["DFlw"])),]

SumEngC234DFlw<- summarySE(EngC234DFlwx, measurevar="sqrtDFlw", groupvars=c("Rep", "Trtmt")) 
GGEngC234DFlw <- ggplot(data=SumEngC234DFlw, aes(x=Trtmt, y=sqrtDFlw, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtDFlw-se, ymax=sqrtDFlw+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(Log[10]~Height~at~Flowering~(cm)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Height at Flowering\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngC234DFlwTSR <- lmer(sqrtDFlw~Trtmt+(1|SSID)+(1|Rep), data=EngC234DFlwx)
summary(lmeEngC234DFlwTSR)
lmeEngC234DFlwTS <- lmer(sqrtDFlw~Trtmt+(1|SSID), data=EngC234DFlwx)
anova(lmeEngC234DFlwTS, lmeEngC234DFlwTSR) #the removal of Rep was non significant (p=0.725 Chisq=0.124)
lmeEngC234DFlwTR <- lmer(sqrtDFlw~Trtmt+(1|Rep), data=EngC234DFlwx)
anova(lmeEngC234DFlwTR, lmeEngC234DFlwTSR) #the removal of SSID was significant (p=<0.0001 Chisq=21.198)
lmEngC234DFlwT <- lm(sqrtDFlw~Trtmt, data=EngC234DFlwx)
x <- -2*logLik(lmEngC234DFlwT, REML=T) +2*logLik(lmeEngC234DFlwTS, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=23.87, p=<0.0001, random effect of SSID was sig
AIC(lmEngC234DFlwT) #=240.94
AIC(lmeEngC234DFlwTS) #=-233.52
#Therefore SSID needs to be included in the model as random effect
lmeEngC234DFlwS <- update(lmeEngC234DFlwTS,~.-Trtmt)
anova(lmeEngC234DFlwS, lmeEngC234DFlwTS) #the effect of trtmt is non significant after considering
#the variation explained by SSID and Rep (p=0.305 chisq=3.63)

#check assumptions of best model
RlmeEngC234DFlwTS <- resid(lmeEngC234DFlwTS) 
FlmeEngC234DFlwTS <- fitted(lmeEngC234DFlwTS)
plot(FlmeEngC234DFlwTS, RlmeEngC234DFlwTS) #okay scatter, definitely more condensed in the middle
abline(h=0, col=c("red"))
hist(RlmeEngC234DFlwTS) #good
qqnorm(RlmeEngC234DFlwTS, main="Q-Q plot for residuals") 
qqline(RlmeEngC234DFlwTS) #tails at either end, but okay

#outliers
RlmeEngC234DFlwTS <- resid(lmeEngC234DFlwTS)
SDRlmeEngC234DFlwTS <- 3*sd(RlmeEngC234DFlwTS)
ORlmeEngC234DFlwTS <- ifelse(abs(RlmeEngC234DFlwTS)>SDRlmeEngC234DFlwTS, 1, 0)
plot(RlmeEngC234DFlwTS, col=ORlmeEngC234DFlwTS+1, pch=16, ylim=c(-1,1))
EngC234DFlw <- EngC234DFlwx[!ORlmeEngC234DFlwTS,]
nrow(EngC234DFlw) #185 from 188

EngC234DFlw$sqrtDFlw <- sqrt(EngC234DFlw$DFlw+0.5)

SumEngC234DFlw2<- summarySE(EngC234DFlw, measurevar="sqrtDFlw", groupvars=c("Rep", "Trtmt")) 
GGEngC234DFlw2 <- ggplot(data=SumEngC234DFlw2, aes(x=Trtmt, y=sqrtDFlw, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=sqrtDFlw-se, ymax=sqrtDFlw+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(Log[10]~Height~at~Flowering~(cm)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Height at Flowering\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngC234DFlwTSR2 <- lmer(sqrtDFlw~Trtmt+(1|SSID)+(1|Rep), data=EngC234DFlw)
summary(lmeEngC234DFlwTSR2)
lmeEngC234DFlwTS2 <- lmer(sqrtDFlw~Trtmt+(1|SSID), data=EngC234DFlw)
anova(lmeEngC234DFlwTS2, lmeEngC234DFlwTSR2) #the removal of Rep was non significant (p=1 Chisq=0)
lmeEngC234DFlwTR2 <- lmer(sqrtDFlw~Trtmt+(1|Rep), data=EngC234DFlw)
anova(lmeEngC234DFlwTR2, lmeEngC234DFlwTSR2) #the removal of SSID was significant (p=<0.0001 Chisq=34.403)
lmEngC234DFlwT2 <- lm(sqrtDFlw~Trtmt, data=EngC234DFlw)
x <- -2*logLik(lmEngC234DFlwT2, REML=T) +2*logLik(lmeEngC234DFlwTS2, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=34.96, p=<0.0001, random effect of SSID was sig
AIC(lmEngC234DFlwT2) #=214.054
AIC(lmeEngC234DFlwTS2) #=195.97
#Therefore SSID needs to be included in the model as random effect
lmeEngC234DFlwS2 <- update(lmeEngC234DFlwTS2,~.-Trtmt)
anova(lmeEngC234DFlwS2, lmeEngC234DFlwTS2) #the effect of trtmt is non significant after considering
#the variation explained by SSID and Rep (p=0.2698 chisq=3.92)

#check assumptions of best model
RlmeEngC234DFlwTS2 <- resid(lmeEngC234DFlwTS2) 
FlmeEngC234DFlwTS2 <- fitted(lmeEngC234DFlwTS2)
plot(FlmeEngC234DFlwTS2, RlmeEngC234DFlwTS2) #okay scatter, more condensed in the middle
abline(h=0, col=c("red"))
hist(RlmeEngC234DFlwTS2) #good
qqnorm(RlmeEngC234DFlwTS2, main="Q-Q plot for residuals") 
qqline(RlmeEngC234DFlwTS2) #tails at either end, but okay.. better than before removing outliers


#*******************************
#FlwDuration
EngC234FlwDx <- EngC234[!rowSums(is.na(EngC234["FlwDuration"])),]

SumEngC234FlwD<- summarySE(EngC234FlwDx, measurevar="rankFlwDuration", groupvars=c("Rep", "Trtmt")) 
GGEngC234FlwD <- ggplot(data=SumEngC234FlwD, aes(x=Trtmt, y=rankFlwDuration, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=rankFlwDuration-se, ymax=rankFlwDuration+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(Log[10]~Height~at~Flowering~(cm)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Height at Flowering\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngC234FlwDTSR <- lmer(rankFlwDuration~Trtmt+(1|SSID)+(1|Rep), data=EngC234FlwDx)
summary(lmeEngC234FlwDTSR)
lmeEngC234FlwDTS <- lmer(rankFlwDuration~Trtmt+(1|SSID), data=EngC234FlwDx)
anova(lmeEngC234FlwDTS, lmeEngC234FlwDTSR) #the removal of Rep was non significant (p=1 Chisq=0)
lmeEngC234FlwDTR <- lmer(rankFlwDuration~Trtmt+(1|Rep), data=EngC234FlwDx)
anova(lmeEngC234FlwDTR, lmeEngC234FlwDTSR) #the removal of SSID was marginally non significant (p=0.062 Chisq=3.48)
lmEngC234FlwDT <- lm(rankFlwDuration~Trtmt, data=EngC234FlwDx)
x <- -2*logLik(lmEngC234FlwDT, REML=T) +2*logLik(lmeEngC234FlwDTS, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=3.44, p=0.33, random effect of SSID was non sig
AIC(lmEngC234FlwDT) #=1969.76
AIC(lmeEngC234FlwDTS) #=1944.991
#Therefore SSID needs to be included in the model as random effect
lmeEngC234FlwDS <- update(lmeEngC234FlwDTS,~.-Trtmt)
anova(lmeEngC234FlwDS, lmeEngC234FlwDTS) #the effect of trtmt is significant after considering
#the variation explained by SSID and Rep (p=0.027 chisq=9.15)

#check assumptions of best model
RlmeEngC234FlwDTS <- resid(lmeEngC234FlwDTS) 
FlmeEngC234FlwDTS <- fitted(lmeEngC234FlwDTS)
plot(FlmeEngC234FlwDTS, RlmeEngC234FlwDTS) #scatter shows trend
abline(h=0, col=c("red"))
hist(RlmeEngC234FlwDTS) #poor
qqnorm(RlmeEngC234FlwDTS, main="Q-Q plot for residuals") 
qqline(RlmeEngC234FlwDTS) #large tails

#outliers
RlmeEngC234FlwDTS <- resid(lmeEngC234FlwDTS)
SDRlmeEngC234FlwDTS <- 3*sd(RlmeEngC234FlwDTS)
ORlmeEngC234FlwDTS <- ifelse(abs(RlmeEngC234FlwDTS)>SDRlmeEngC234FlwDTS, 1, 0)
plot(RlmeEngC234FlwDTS, col=ORlmeEngC234FlwDTS+1, pch=16, ylim=c(-100,100))
EngC234FlwD <- EngC234FlwDx[!ORlmeEngC234FlwDTS,]
nrow(EngC234FlwD) #184 from 188

SumEngC234FlwD2<- summarySE(EngC234FlwD, measurevar="rankFlwDuration", groupvars=c("Rep", "Trtmt")) 
GGEngC234FlwD2 <- ggplot(data=SumEngC234FlwD2, aes(x=Trtmt, y=rankFlwDuration, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=rankFlwDuration-se, ymax=rankFlwDuration+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+#can change size of data points
  scale_shape_manual(values=c(15, 16, 17, 8))+
  xlab("Treatment (%)") + ylab(expression(bold(Log[10]~Height~at~Flowering~(cm)))) +
  scale_colour_hue(name="Replicate", l=40) + ggtitle("Cakile Height at Flowering\nbetween Treatments") + #name=sets the legend titel
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0.5))+ #legend.position is set to top right
  theme(axis.title.x = element_text(face="bold", size=20), # can also add colour with "colour="#x"" where x is colour number
        axis.text.x  = element_text(vjust=0.5, size=16))+ #vjust repositions the x axis text, can change angle of text with "angle=90"
  theme(axis.title.y = element_text(face="bold", size=20),
        axis.text.y  = element_text(size=16))

lmeEngC234FlwDTSR2 <- lmer(rankFlwDuration~Trtmt+(1|SSID)+(1|Rep), data=EngC234FlwD)
summary(lmeEngC234FlwDTSR2)
lmeEngC234FlwDTS2 <- lmer(rankFlwDuration~Trtmt+(1|SSID), data=EngC234FlwD)
anova(lmeEngC234FlwDTS2, lmeEngC234FlwDTSR2) #the removal of Rep was non significant (p=1 Chisq=0)
lmeEngC234FlwDTR2 <- lmer(rankFlwDuration~Trtmt+(1|Rep), data=EngC234FlwD)
anova(lmeEngC234FlwDTR2, lmeEngC234FlwDTSR2) #the removal of SSID was marginally non significant (p=0.062 Chisq=3.48)
lmEngC234FlwDT2 <- lm(rankFlwDuration~Trtmt, data=EngC234FlwD)
x <- -2*logLik(lmEngC234FlwDT2, REML=T) +2*logLik(lmeEngC234FlwDTS2, REML=T)
x
pchisq(x, df=3, lower.tail=F)
#logLik=3.44, p=0.33, random effect of SSID was non sig
AIC(lmEngC234FlwDT2) #=1969.76
AIC(lmeEngC234FlwDTS2) #=1944.991
#Therefore SSID needs to be included in the model as random effect
lmeEngC234FlwDS2 <- update(lmeEngC234FlwDTS2,~.-Trtmt)
anova(lmeEngC234FlwDS2, lmeEngC234FlwDTS2) #the effect of trtmt is significant after considering
#the variation explained by SSID and Rep (p=0.027 chisq=9.15)

#check assumptions of best model
RlmeEngC234FlwDTS2 <- resid(lmeEngC234FlwDTS2) 
FlmeEngC234FlwDTS2 <- fitted(lmeEngC234FlwDTS2)
plot(FlmeEngC234FlwDTS2, RlmeEngC234FlwDTS2) #scatter shows trend
abline(h=0, col=c("red"))
hist(RlmeEngC234FlwDTS2) #poor
qqnorm(RlmeEngC234FlwDTS2, main="Q-Q plot for residuals") 
qqline(RlmeEngC234FlwDTS2) #large tails

