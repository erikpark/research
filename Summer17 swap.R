library(ggpubr)
library(gridExtra)

setwd("/media/Datas/Research/")

swap.orig <- read.csv("Microbiome swap data - Sheet1.csv")


treatments <- swap.orig$Treatment.ID

dead <- swap.orig[swap.orig$Dead. == "y",]
alive <- swap.orig[swap.orig$Dead. == "",]

alive$Treatment.ID <- as.factor(alive$Treatment.ID)
alive$LT <- as.numeric(alive$LT)


gg <- swap.orig[swap.orig$Treatment.ID == "gg",]

gs <- swap.orig[swap.orig$Treatment.ID == "gs",]

ss <- swap.orig[swap.orig$Treatment.ID == "ss",]

sg <- swap.orig[swap.orig$Treatment.ID == "sg",]


gg.survival <- (nrow(gg) - (sum(gg$Dead. == "y"))) / nrow(gg)

gs.survival <- (nrow(gs) - (sum(gs$Dead. == "y"))) / nrow(gs)

ss.survival <- (nrow(ss) - (sum(ss$Dead. == "y"))) / nrow(ss)

sg.survival <- (nrow(sg) - (sum(sg$Dead. == "y"))) / nrow(sg)

survivals <- c(gg.survival,gs.survival,ss.survival,sg.survival)
survivals <- as.data.frame(survivals)
survivals$treatment <- c("gg", "gs", "ss", "sg")
survivals



fun_length <- function(x){
  return(data.frame(y=median(x),label= paste0("n=", length(x))))
}


# Analyses done with "living" individuals only

a <- ggplot(alive, aes(x = Treatment.ID, y = LT)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.5, size = 4) + labs(title = "Time to L3 for survivors only")

c <- ggplot(alive, aes(x = Treatment.ID, y = LW)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.1, size = 4) + labs(title = "Weight at L3 day 3 for survivors only")

e <- ggplot(alive, aes(x = Treatment.ID, y = PT)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.5, size = 4) + labs(title = "Time to pupation for survivors only")

g <- ggplot(alive, aes(x = Treatment.ID, y = PW)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.1, size = 4) + labs(title = "Weight at P day 3 for survivors only")

ggplot(alive, aes(x = Treatment.ID, y = AT)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.5, size = 4) + labs(title = "Time to adult eclosion")

ggplot(alive, aes(x = Treatment.ID, y = ATW)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.5, size = 4) + labs(title = "Adult thorax width")

ggplot(survivals, aes(x = treatment, y = survivals)) + geom_point() 


# two-proportions z-test, tests for significance in difference between porpotions for two populations that are:
# independent, large enough (>=5) (sg?), randomly selected. 
ggvgs.surv <- prop.test(x = c((nrow(gg) - (sum(gg$Dead. == "y"))), (nrow(gs) - (sum(gs$Dead. == "y")))), n = c(nrow(gg), nrow(gs)))

ggvgs.surv

ssvsg.surv <- prop.test(x = c((nrow(ss) - (sum(ss$Dead. == "y"))), (nrow(sg) - (sum(sg$Dead. == "y")))), n = c(nrow(ss), nrow(sg)))

ssvsg.surv




# Analyses done with individuals who died before adulthood


b <- ggplot(swap.orig, aes(x = Treatment.ID, y = LT)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.5, size = 4) + labs(title = "Time to L3 for all individuals")

d <- ggplot(swap.orig, aes(x = Treatment.ID, y = LW)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.1, size = 4) + labs(title = "Weight at L3 day 3 for all individuals")

f <- ggplot(swap.orig, aes(x = Treatment.ID, y = PT)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.5, size = 4) + labs(title = "Time to pupation for all individuals")

h <- ggplot(swap.orig, aes(x = Treatment.ID, y = PW)) + geom_boxplot() + stat_compare_means(method = "wilcox.test", comparisons = list(c("gg","gs"), c("sg","ss"))) +
  stat_summary(aes(x=factor(Treatment.ID)), position=position_dodge(.9), fun.data = fun_length,  geom = "text",
               vjust = +1.1, size = 4) + labs(title = "Weight at P day 3 for all individuals")




grid.arrange(a,b)
grid.arrange(c,d)
grid.arrange(e,f)
grid.arrange(g,h)

