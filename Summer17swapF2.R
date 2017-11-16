library(ggpubr)
library(gridExtra)
library(tidyr)
library(alr4)
library(GGally)
library(lmtest)

f2.orig <- read.csv("Microbiome swap F2's - Sheet1.csv")

f2.fixed <- separate(data = f2.orig, col = Mother.Individual, into = c("mother", "individual"), sep = "\\.")

f2.fixed <- f2.fixed[-(35:40),]

f2.fixed$Maternal.size <- as.factor(f2.fixed$Maternal.size)

ggpairs(data = f2.orig, columns = c(4,11))

ggpairs(data = f2.orig, columns = c(7,11))

ggpairs(data = f2.fixed, columns = c(5,8))


ggplot(f2.fixed, aes(x = mother, y = Time.to.adulthood, fill = Treatment.ID)) + geom_boxplot()

ggplot(f2.fixed, aes(x = mother, y = Time.to.adulthood, fill = Treatment.ID)) + geom_boxplot() + facet_wrap(~Sex)

ggplot(f2.fixed, aes(x = mother, y = Adult.size, fill = Treatment.ID)) + geom_boxplot()

ggplot(f2.fixed, aes(x = Treatment.ID, y = Adult.size, fill = Sex)) + geom_boxplot()

fun_length <- function(x){
  return(data.frame(y=median(x),label= paste0("n=", length(x))))
}

ggplot(f2.fixed, aes(x = Treatment.ID, y = Adult.size)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.5, size = 4)

ggplot(f2.fixed, aes(x = mother, y = Adult.size, fill = Treatment.ID)) + geom_boxplot() + facet_wrap(~Sex)

ggplot(f2.fixed, aes(x= Treatment.ID, y= Adult.size)) + geom_boxplot()

ggplot(f2.fixed, aes(x= Maternal.size, y= BB.weight)) + geom_boxplot()

scatterplot(Adult.size ~ BB.weight | Maternal.size, data = f2.fixed)

scatterplot(BB.weight ~ Maternal.size, data = f2.orig)

ggplot(f2.fixed, aes(x = mother, y = Dead., fill = Treatment.ID)) + geom_point()


mfull.time <- lm(Time.to.adulthood ~ Treatment.ID + Sex + BB.weight*Maternal.size, data = f2.fixed)
summary(mfull.time)

Anova(mfull.time)

mred.time <- lm(Time.to.adulthood ~ Treatment.ID + Maternal.size, data = f2.fixed)
summary(mred.time)

Anova(mfull.time)

``````````````````````````````

mfull.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight*Maternal.size, data = f2.fixed)
summary(mfull.size)

Anova(mfull.size)

mred.size <- lm(Adult.size ~ Treatment.ID + Sex, data = f2.fixed)
summary(mred.size)

Anova(mred.size)

anova(mred.size, mfull.size)

mmid.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight:Maternal.size, data = f2.fixed)
summary(mmid.size)

Anova(mmid.size)

plot(Effect(c("Treatment.ID","Sex","BB.weight"),mmid.size))

plot(Effect(c("Treatment.ID","Sex","Maternal.size"),mmid.size))

anova(mred.size,mmid.size)

anova(mmid.size, mfull.size)

mtest.size <- lm(Adult.size^2 ~ Treatment.ID + Sex + log(BB.weight):Maternal.size, data = f2.fixed)
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

e=resid(mmid.size)
op=par(mfrow=c(2,2)) 
with(f2.fixed,plot(fitted(mmid.size),e))
with(f2.fixed,plot(Sex,e))
with(f2.fixed,plot(Treatment.ID,e))
with(f2.fixed,plot(BB.weight,e))
par(op)


vif(mmid.size)
vif(mfull.size)


# So, best model seems to include treatment, sex and interaction between brood ball weight and maternal size (mmid.size)
