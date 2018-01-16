library(ggpubr)
library(gridExtra)
library(tidyr)
library(alr4)
library(GGally)
library(lmtest)

f2.orig <- read.csv("Microbiome swap F2's - Sheet1.csv")

f2.fixed <- separate(data = f2.orig, col = Mother.Individual, into = c("mother", "individual"), sep = "\\.")

f2.fixed <- f2.fixed[-(35:40),]

f2.fixed <- f2.fixed[complete.cases(f2.fixed[,1]),]

f2.fixed$Maternal.size <- as.factor(f2.fixed$Maternal.size)

f2.removed <- subset(f2.fixed, Dead. == "")







ggpairs(data = f2.orig, columns = c(4,11))

ggpairs(data = f2.orig, columns = c(7,11))

ggpairs(data = f2.fixed, columns = c(5,8))

# Looking at mortality
dead <- f2.fixed[f2.fixed$Dead. == "y",]
alive <- f2.fixed[f2.fixed$Dead. == "",]

gg <- f2.fixed[f2.fixed$Treatment.ID == "gg",]

gs <- f2.fixed[f2.fixed$Treatment.ID == "gs",]

gg.survival <- (nrow(gg) - (sum(gg$Dead. == "y"))) / nrow(gg)

gs.survival <- (nrow(gs) - (sum(gs$Dead. == "y"))) / nrow(gs)


ggvgs.surv <- prop.test(x = c((nrow(gg) - (sum(gg$Dead. == "y"))), (nrow(gs) - (sum(gs$Dead. == "y")))), n = c(nrow(gg), nrow(gs)))

ggvgs.surv

survivals <- c(gg.survival,gs.survival)
survivals <- as.data.frame(survivals)
survivals$treatment <- c("gg", "gs")


ggplot(survivals, aes(x = treatment, y = survivals)) + geom_point() + expand_limits(y = 0)


# So, no difference in survival rate between the treatment F2's



ggplot(f2.fixed, aes(x = mother, y = Time.to.adulthood, fill = Treatment.ID)) + geom_boxplot()

ggplot(f2.fixed, aes(x = mother, y = Time.to.adulthood, fill = Treatment.ID)) + geom_boxplot() + facet_wrap(~Sex)

ggplot(f2.fixed, aes(x = mother, y = Adult.size, fill = Treatment.ID)) + geom_boxplot()

ggplot(f2.fixed, aes(x = Maternal.size, y = Adult.size, fill = Treatment.ID)) + geom_boxplot()

ggplot(f2.fixed, aes(x = Treatment.ID, y = Adult.size, fill = Sex)) + geom_boxplot()

ggplot(f2.fixed, aes(x = Treatment.ID, y = Adult.size)) + geom_boxplot()


fun_length <- function(x){
  return(data.frame(y=median(x),label= paste0("n=", length(x))))
}

ggplot(f2.fixed, aes(x = Treatment.ID, y = Adult.size)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.5, size = 4)

ggplot(f2.fixed, aes(x = Sex, y = Adult.size)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("m","f"))) +
  stat_summary(aes(x=factor(Sex)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.5, size = 4)

ggplot(f2.fixed, aes(x = mother, y = Adult.size, fill = Treatment.ID)) + geom_boxplot() + facet_wrap(~Sex)

ggplot(f2.fixed, aes(x= Treatment.ID, y= Adult.size)) + geom_boxplot()

ggplot(f2.fixed, aes(x= Maternal.size, y= BB.weight)) + geom_boxplot()

scatterplot(Adult.size ~ BB.weight | Maternal.size, data = f2.fixed)

scatterplot(Adult.size ~ Maternal.size, data = f2.orig, boxplots = FALSE, smoother = FALSE)


ggplot(f2.fixed, aes(x = mother, y = Dead., fill = Treatment.ID)) + geom_point()


mfull.time <- lm(Time.to.adulthood ~ Treatment.ID + Sex + BB.weight*Maternal.size, data = f2.fixed)
summary(mfull.time)

Anova(mfull.time)

mred.time <- lm(Time.to.adulthood ~ Treatment.ID + Sex, data = f2.fixed)
summary(mred.time)

Anova(mred.time)

``````````````````````````````

mfull.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight*Maternal.size, data = f2.fixed)
summary(mfull.size)

Anova(mfull.size)


mfullfac.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight*Maternal.size, data = f2.removed)
summary(mfullfac.size)

Anova(mfullfac.size)

anova(mfull.size,mfullfac.size)


mred.size <- lm(Adult.size ~ Treatment.ID + Sex, data = f2.fixed)
summary(mred.size)

Anova(mred.size)

mbb.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight, data = f2.fixed)
summary(mbb.size)

mmom.size <- lm(Adult.size ~ Treatment.ID + Sex + Maternal.size, data = f2.fixed)
summary(mmom.size)

Anova(mred.size)

anova(mred.size, mfull.size)

mmid.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight:Maternal.size, data = f2.fixed)
summary(mmid.size)

Anova(mmid.size)

anova(mred.size,mmid.size)

anova(mmid.size, mfull.size)

anova(mmom.size, mfull.size)

anova(mbb.size, mfull.size)



plot(Effect(c("Treatment.ID","Sex","BB.weight"),mmid.size))

plot(Effect(c("Treatment.ID","Sex","Maternal.size"),mmid.size))

plot(Effect(c("Treatment.ID","Sex"),mfull.size))


plot(Effect(c("Treatment.ID"),mred.size))

plot(Effect(c("Sex"),mred.size))


plot(Effect(c("Treatment.ID"),mfull.size))

plot(Effect(c("Sex"),mfull.size))

plot(Effect(c("BB.weight"),mfull.size))

plot(Effect(c("Maternal.size"),mfull.size))


plot(Effect(c("Treatment.ID"),mmid.size))

plot(Effect(c("Sex"),mmid.size))

plot(Effect(c("BB.weight"),mmid.size))

plot(Effect(c("Maternal.size"),mmid.size))




mtest.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight:(Maternal.size)^2, data = f2.fixed)
summary(mtest.size)

#Testing variance

ncvTest(mmid.size)
# Non-constant variance, but just barely

#looking for which regressor can best explain the non-constant variance
Z1=with(f2.fixed,ncvTest(mmid.size, ~ Treatment.ID))
Z2=with(f2.fixed,ncvTest(mmid.size, ~ Sex))
Z3=with(f2.fixed,ncvTest(mmid.size, ~ Treatment.ID+Sex))
Z4=with(f2.fixed,ncvTest(mmid.size, ~ BB.weight:Maternal.size))
Z5=with(f2.fixed,ncvTest(mmid.size))
table1=rbind(with(Z1,c(Df,ChiSquare,p)),with(Z2,c(Df,ChiSquare,p)),with(Z3,c(Df,ChiSquare,p)),with(Z4,c(Df,ChiSquare,p)),with(Z5,c(Df,ChiSquare,p)))
row.names(table1)=c("Treatment","Sex","Treatment,Sex","Interaction","Fitted values")
colnames(table1)=c("df","Test statistic","p-Value")
table1

mtest.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight:Maternal.size, data = f2.fixed, weights = Treatment.ID)

coeftest(mmid.size, vcov=hccm)
# So, looks like all regressors except for treatment are well explained by constant variance, but treatment is very close to non-significant, and is a factor (so can't use it as the weight)
# So it might be okay just the assume all the regressors have constant variance.


summary(powerTransform(cbind(BB.weight,Maternal.size) ~ Treatment.ID, f2.fixed))


vif(mmid.size)
vif(mfull.size)


# So, best model seems to include treatment, sex and interaction between brood ball weight and maternal size (mmid.size)


influenceIndexPlot(mfull.size, id.n = 3)
# Should look at removing observations 79 and 98 at least, 79 because it has a really low bonferroni value, and 98 for a low bonferroni and high influence (Di)
outlierTest(mfull.size)

mfull.size.removed <- update(mfull.size, subset=-c(79,98))

compareCoefs(mfull.size,mfull.size.removed)
# So, removing the points reduces the estimated value for treatment very slightly.  Not a large change.

Anova(mfull.size.removed)

residualPlots(mfull.size)

residualPlots(mfull.size.removed)
# Also slightly improves the residuals.
# But still looks like BB weight needs a transformation

bc1 = powerTransform(cbind(BB.weight) ~ Maternal.size, f2.fixed)
summary(bc1)
# Maybe the sqrt (0.5) power?

mfull.size.sqrt <- lm(Adult.size ~ Treatment.ID + Sex + sqrt(BB.weight)*Maternal.size, data = f2.fixed)
summary(mfull.size.sqrt)
Anova(mfull.size.sqrt)
residualPlots(mfull.size.sqrt)
outlierTest(mfull.size.sqrt)
mfull.size.removed.sqrt <- update(mfull.size.sqrt, subset=-c(79,98))
summary(mfull.size.removed.sqrt)
residualPlots(mfull.size.removed.sqrt)
# So removing the outlier after the square root transformation gets as close as possible to normal residuals.
# But does it really matter?

plot(Effect(c("Treatment.ID","Sex","BB.weight"),mfull.size.removed.sqrt))
