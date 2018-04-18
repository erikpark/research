swap <- read.table("validation.csv", sep = ",", header = TRUE)

swap.validate <- swap[complete.cases(swap[,11]),]

library(alr4)
library(ggpubr)
library(ggsignif)

ggplot(swap.validate, aes(x = Treatment.ID, y = Scope.size)) + geom_boxplot()+ geom_signif(comparisons = list(c("gg","gs")), test = "wilcox.test", map_signif_level = FALSE, textsize = 7, vjust = +0.1)

ggplot(swap.validate, aes(x = Treatment.ID, y = ATW)) + geom_boxplot()+ geom_signif(comparisons = list(c("gg","gs")), test = "wilcox.test", map_signif_level = FALSE, textsize = 7, vjust = +0.1)


f2 <- read.table("f2validation.csv", sep = ",", header = TRUE)

f2.validate <- f2[complete.cases(f2[,8]),]

ggplot(f2.validate, aes(x = Sex, y = Scope.size)) + geom_boxplot()+ geom_signif(comparisons = list(c("m","f")), test = "wilcox.test", map_signif_level = FALSE, textsize = 7, vjust = +0.1)

ggplot(f2.validate, aes(x = Sex, y = Adult.size)) + geom_boxplot()+ geom_signif(comparisons = list(c("m","f")), test = "wilcox.test", map_signif_level = FALSE, textsize = 7, vjust = +0.1)

ggplot(f2.validate, aes(x = BB.weight, y = Scope.size, color = Sex)) + geom_point()

m1 <- lm(Scope.size ~ Sex*Treatment.ID*BB.weight*Maternal.size, data = f2)

Anova(m1)

m2 <- lm(Scope.size ~ Sex*Treatment.ID*BB.weight, data = f2)

Anova(m2)

m3 <- lm(Scope.size ~ Sex + Treatment.ID + BB.weight + Treatment.ID:BB.weight + Sex:Treatment.ID, data = f2.validate)

Anova(m3)

plot(cooks.distance(m3), pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooks.distance(m3), na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooks.distance(m3))+1, 
     y=cooks.distance(m3), labels=ifelse(cooks.distance(m3)>4*mean(cooks.distance(m3), na.rm=T),
                                         names(cooks.distance(m3)),""), col="red")  # add labels
# So, should take out observation 98 probably, based on above dot plot, also want to take out observation 4, the large outlier.

f2.outliers <- f2.validate[-c(4,74),]

m4 <- lm(Scope.size ~ Sex + Treatment.ID + BB.weight + Treatment.ID:BB.weight + Sex:Treatment.ID, data = f2.outliers)

Anova(m4)

m5 <- lm(Scope.size ~ Sex*Treatment.ID, data = f2.outliers)

Anova(m5)

m6 <- lm(Scope.size ~ Sex + Treatment.ID, data = f2.outliers)
anova(m5,m4)
anova(m6,m4)
anova(m6,m5)
# So, model 6 with just main effects of sex and treatment ID is the best.