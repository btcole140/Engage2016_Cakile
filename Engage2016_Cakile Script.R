setwd("/Users/mac/Google Drive/NSERC Engage/Methods and Data/Cakile/data analysis")

EngC <- read.csv("Engage2016_Cakile.csv")

str(EngC)

EngC$SID <- as.factor(EngC$SID)
EngC$Rep <- as.factor(EngC$Rep)
EngC$Trtmt <- as.factor(EngC$Trtmt)
EngC$Pres_Abs <- as.factor(EngC$Pres_Abs)
EngC$DateFlw <- as.Date(EngC$DateFlw, "%d-%b-%y")
EngC$DateFrt <- as.Date(EngC$DateFrt, "%d-%b-%y")

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

#********************************
#Analysis of variation between trtmts while considering rep and tray
#*******************************
#SizeH
SumEngCH<- summarySE(EngC, measurevar="SizeH", groupvars=c("Rep", "Trtmt")) 
GGEngCH <- ggplot(data=SumEngCH, aes(x=Trtmt, y=SizeH, group=Rep, shape=Rep)) +
  geom_errorbar(aes(ymin=SizeH-se, ymax=SizeH+se), width=0.1) + #set error bars
  geom_line() + geom_point(size=3)+ #can change size of data points
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
plot(FlmeEngCHTSR, RlmeEngCHTSR) #okay skatter
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

#DFlw
par(mfrow = c(1,1))
hist(EngC234$DFlw) #not great shape, few columns
EngC234$LogDFlw <- log10(EngC234$DFlw+1)
hist(EngC234$LogDFlw) #bit better than raw
EngC234$sqrtDFlw <- sqrt(EngC234$DFlw+0.5)
hist(EngC234$sqrtDFlw) #more columns and okay shape *
EngC234$rankDFlw <- rank(EngC234$DFlw)

#FlwDuration
par(mfrow = c(1,1))
hist(EngC234$FlwDuration) #not great shape, few columns
EngC234$LogFlwDuration <- log10(EngC234$FlwDuration+1)
hist(EngC234$LogFlwDuration) #not better than raw
EngC234$sqrtFlwDuration <- sqrt(EngC234$FlwDuration+0.5)
hist(EngC234$sqrtFlwDuration) #more columns but not much shape
EngC234$rankFlwDuration <- rank(EngC234$FlwDuration) #*

