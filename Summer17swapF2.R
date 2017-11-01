library(ggpubr)
library(gridExtra)
library(tidyr)
library(alr4)

f2.orig <- read.csv("Microbiome swap F2's - Sheet1.csv")

f2.fixed <- separate(data = f2.orig, col = Mother.Individual, into = c("mother", "individual"), sep = "\\.")

f2.fixed <- f2.fixed[-(35:40),]

ggplot(f2.fixed, aes(x = mother, y = Time.to.adulthood, fill = Treatment.ID)) + geom_boxplot()

ggplot(f2.fixed, aes(x = mother, y = Adult.size, fill = Treatment.ID)) + geom_boxplot()

scatterplot(Adult.size ~  | Treatment.ID, data = f2.fixed)

ggplot(f2.fixed, aes(x = mother, y = Dead., fill = Treatment.ID)) + geom_point()


mfull.time <- lm(Time.to.adulthood ~ Treatment.ID + Sex + BB.weight*Maternal.size, data = f2.fixed)
summary(mfull.time)

Anova(mfull.time)

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
