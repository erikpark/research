library(ggpubr)
library(gridExtra)
library(tidyr)
library(alr4)
library(GGally)

f2.orig <- read.csv("Microbiome swap F2's - Sheet1.csv")

f2.fixed <- separate(data = f2.orig, col = Mother.Individual, into = c("mother", "individual"), sep = "\\.")

f2.fixed <- f2.fixed[-(35:40),]

f2.fixed$Maternal.size <- as.factor(f2.fixed$Maternal.size)

ggpairs(data = f2.fixed, columns = c(5,8))

ggplot(f2.fixed, aes(x = mother, y = Time.to.adulthood, fill = Treatment.ID)) + geom_boxplot()

ggplot(f2.fixed, aes(x = mother, y = Time.to.adulthood, fill = Treatment.ID)) + geom_boxplot() + facet_wrap(~Sex)

ggplot(f2.fixed, aes(x = mother, y = Adult.size, fill = Treatment.ID)) + geom_boxplot()

ggplot(f2.fixed, aes(x = Treatment.ID, y = Adult.size, fill = Sex)) + geom_boxplot()

ggplot(f2.fixed, aes(x = mother, y = Adult.size, fill = Treatment.ID)) + geom_boxplot() + facet_wrap(~Sex)

scatterplot(Adult.size ~  | Treatment.ID, data = f2.fixed)

ggplot(f2.fixed, aes(x = mother, y = Dead., fill = Treatment.ID)) + geom_point()


mfull.time <- lm(Time.to.adulthood ~ Treatment.ID + Sex + BB.weight*Maternal.size, data = f2.fixed)
summary(mfull.time)

Anova(mfull.time)

mred.time <- lm(Time.to.adulthood ~ Treatment.ID + Maternal.size, data = f2.fixed)
summary(mred.time)


mfull.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight*Maternal.size, data = f2.fixed)
summary(mfull.size)

Anova(mfull.size)

mred.size <- lm(Adult.size ~ Treatment.ID + Sex, data = f2.fixed)
summary(mred.size)

Anova(mred.size)

anova(mred.size, mfull.size)

mmid.size <- lm(Adult.size ~ Treatment.ID + Sex + BB.weight:Maternal.size, data = f2.fixed)
summary(mmid.size)

anova(mred.size,mmid.size)

anova(mmid.size, mfull.size)

mtest.size <- lm(Adult.size ~ Treatment.ID*Sex + BB.weight:Maternal.size, data = f2.fixed)
summary(mtest.size)

Anova(mtest.size)
