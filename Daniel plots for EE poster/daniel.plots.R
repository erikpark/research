library(ggpubr)
library(gridExtra)
library(ggsignif)
library(ggthemes)
library(lme4)
library(alr4)
library(visreg)



# Custom theme for plots

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(size = 16),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA, size = 5),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size= unit(0.7, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}



# data for Daniel plots

daniel.data.1 <- read.table("external/Research/Daniel plots for EE poster/daniel.experiment1.csv", sep = ",", header = T)

daniel.data.tempstress <- read.table("external/Research/Daniel plots for EE poster/daniel.experiment2tempstress.csv", sep = ",", header = T)

daniel.data.desstress <- read.table("external/Research/Daniel plots for EE poster/daniel.experiment2dessstress.csv", sep = ",", header = T)


# Code for daniel figure 1A: larval mass, time to adulthood, and adult body size

lm <- ggplot(daniel.data.1, aes(x = Treatment, y = Day6Mass, fill = Treatment)) + geom_boxplot() + 
  geom_signif(comparisons = list(c("M(+)","M(-)")), test = "t.test", map_signif_level = TRUE)+
  scale_x_discrete(labels = c("", "")) + labs(x = "", y = "") + 
  theme_Publication() + 
  scale_fill_grey(start=1, end = 0.8, name = "", breaks=c("M(+)","M(-)"), labels=c("Microbes", "No Microbes"))

ttah <- ggplot(daniel.data.1, aes(x = Treatment, y = TimeToAdult, fill = Treatment)) + geom_boxplot() + 
  geom_signif(comparisons = list(c("M(+)","M(-)")), test = "t.test", map_signif_level = TRUE)+
  scale_x_discrete(labels = c("", "")) + labs(x = "", y = "") + 
  theme_Publication() + 
  scale_fill_grey(start=1, end = 0.8, name = "", breaks=c("M(+)","M(-)"), labels=c("Microbes", "No Microbes"))

abs <- ggplot(daniel.data.1, aes(x = Treatment, y = AdultSize, fill = Treatment)) + geom_boxplot() + 
  geom_signif(comparisons = list(c("M(+)","M(-)")), test = "t.test", map_signif_level = TRUE)+
  scale_x_discrete(labels = c("", "")) + labs(x = "", y = "") + 
  theme_Publication() + 
  scale_fill_grey(start=1, end = 0.8, name = "", breaks=c("M(+)","M(-)"), labels=c("Microbes", "No Microbes"))

ggarrange(lm,ttah,abs, ncol = 3, common.legend = TRUE, legend = "none")


# code for dessication stress figure - maybe leave out? 

dessication <- ggplot(daniel.data.desstress, aes(x = Treatment, y = ))

# code for temp stress figure - based on linear model

temp.lm <- lm(TimeToAdult ~ TemperatureTreatment * MicrobeTreatment, data = daniel.data.tempstress)

visreg(temp.lm, "TemperatureTreatment", gg = TRUE, by = "MicrobeTreatment", overlay = TRUE) +
  theme_Publication() + labs(x = "Temp", y = "Time to adult")
