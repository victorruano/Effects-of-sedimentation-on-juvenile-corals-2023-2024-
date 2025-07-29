library(nlme)
library(lme4)
library(tidyverse)
library(ggpubr)
library(Matrix)
library(grid)
library(performance)

rm(list=ls())

setwd("/Users/victorruano/Library/CloudStorage/OneDrive-UniversityofMiami/CIMAS/Settlement Rates")

##################### Top trials ##########################
sedtop3<-read.csv("Filtered_settled_top_bi_n_V3.csv")

PCLIn3<-subset(sedtop3, sedtop3$species=="Pcli")
CNATn3<-subset(sedtop3, sedtop3$species=="Cnat")
PSTRn3<-subset(sedtop3, sedtop3$species=="Pstr")
OFAVn3<-subset(sedtop3, sedtop3$species=="Ofav")
DLABn3<-subset(sedtop3, sedtop3$species=="Dlab")

#### CNAT 
cnat.settlement.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|sed_load_mgcm2:replicate),
                               family=binomial, data=CNATn3)

summary(cnat.settlement.glme.v3)
r2(cnat.settlement.glme.v3)

cnat.plot.v3<-ggplot(CNATn3,aes(x=sed_load_mgcm2,y=Binary))+
  geom_point(shape = 16, 
             color = "violet", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "violet", 
              fill = "violet",  
              size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Cnat",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

cnat.plot.v3

### OFAV
ofav.settlement.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|sed_load_mgcm2:replicate),
                             family=binomial, data=OFAVn3)
summary(ofav.settlement.glme.v3)
r2(ofav.settlement.glme.v3)

ofav.plot.v3<-ggplot(OFAVn3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "seagreen3", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "seagreen3",
              fill = "seagreen3",
              size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Ofav",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

ofav.plot.v3

### PCLI
pcli.settlement.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|sed_load_mgcm2:replicate),
                             family=binomial, data=PCLIn3)
summary(pcli.settlement.glme.v3)
r2(pcli.settlement.glme.v3)

# Plot
pcli.plot.v3<-ggplot(PCLIn3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "olivedrab", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "olivedrab",
              fill = "olivedrab",
              size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Pcli",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

pcli.plot.v3

### PSTR
pstr.settlement.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|sed_load_mgcm2:replicate),
                             family=binomial, data=PSTRn3)
summary(pstr.settlement.glme.v3)
r2(pstr.settlement.glme.v3)

pstr.plot.v3<-ggplot(PSTRn3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "goldenrod1",
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "goldenrod1",  
              fill = "goldenrod1",
              size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Pstr",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

pstr.plot.v3

### DLAB
dlab.settlement.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|sed_load_mgcm2:replicate),
                               family=binomial, data=DLABn3)
summary(dlab.settlement.glme.v3)
r2(dlab.settlement.glme.v3)

dlab.plot.v3<-ggplot(DLABn3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "maroon", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "maroon",
              fill = "maroon",
              size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Dlab",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

dlab.plot.v3

######################## Bottom trials ###########################

sedbot3<-read.csv("set_treat_bot_V3.csv")

PCLInb3<-subset(sedbot3, sedbot3$species=="Pcli")
CNATnb3<-subset(sedbot3, sedbot3$species=="Cnat")
PSTRnb3<-subset(sedbot3, sedbot3$species=="Pstr")
OFAVnb3<-subset(sedbot3, sedbot3$species=="Ofav")
DLABnb3<-subset(sedbot3, sedbot3$species=="Dlab")


### CNAT
cnat.settlement.bot.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|sed_load_mgcm2:replicate),
                                 family=binomial, data=CNATnb3)
summary(cnat.settlement.bot.glme.v3)
r2(cnat.settlement.bot.glme.v3)

cnat.bot.plot.v3<-ggplot(CNATnb3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "violet", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "violet",
              fill = "violet",
              size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = "Probability of settlement")+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Cnat",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

cnat.bot.plot.v3

### OFAV

ofav.settlement.bot.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|sed_load_mgcm2:replicate),
                                 family=binomial, data=OFAVnb3)
summary(ofav.settlement.bot.glme.v3)
r2(ofav.settlement.bot.glme.v3)

ofav.bot.plot.v3<-ggplot(OFAVnb3,
                      aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "seagreen3", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "seagreen3",
              fill = "seagreen3",
              size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = "Probability of settlement")+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Ofav",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

ofav.bot.plot.v3

### PCLI

pcli.settlement.bot.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|sed_load_mgcm2:replicate),
                                   family=binomial, data=PCLInb3)
summary(pcli.settlement.bot.glme.v3)
r2(pcli.settlement.bot.glme.v3)

pcli.bot.plot.v3<-ggplot(PCLInb3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "olivedrab", 
             size = 3, 
             alpha = 0.5)+
  #geom_smooth(method="glm", 
              #method.args = list(family = "binomial"),
              #color = "black",  
              #size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.5, 
    #       label="Pcli",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

pcli.bot.plot.v3

### PSTR

pstr.settlement.bot.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|replicate),
                                   family=binomial, data=PSTRnb3)
summary(pstr.settlement.bot.glme.v3)
r2(pstr.settlement.bot.glme.v3)

pstr.bot.plot.v3<-ggplot(PSTRnb3, aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "goldenrod1", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "goldenrod1",  
              fill = "goldenrod1",
              size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.5, 
    #       label="Pstr",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

pstr.bot.plot.v3

### DLAB

dlab.settlement.bot.glme.v3<-glmer(Binary~sed_load_mgcm2+(1|sed_load_mgcm2:replicate),
                                   family=binomial, data=DLABnb3)
summary(dlab.settlement.bot.glme.v3)
r2(dlab.settlement.bot.glme.v3)

dlab.bot.plot.v3<-ggplot(DLABnb3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "maroon", 
             size = 3, 
             alpha = 0.5)+
  #geom_smooth(method="glm", 
              #method.args = list(family = "binomial"),
              #color = "black",  
              #size = 1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.5, 
    #       label="Dlab",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

dlab.bot.plot.v3


ggarrange(cnat.bot.plot.v3, ofav.bot.plot.v3, pstr.bot.plot.v3, pcli.bot.plot.v3,
          labels = c("A", "B", "C", "D"), common.legend = T,
          ncol = 2, nrow = 2)



##################### Corrected curves #######################

# CNAT

# Bottom estimates
# Intercept)   1.5718 
# treatment_n  -0.6978 

# Top estimates (first set is raw prop I think, double check)
# Intercept   1.4922
# treatment_n -12.9142

# Intercept)   1.4922
# treatment_n -25.8285 

cnatlog.v3 <- function(x) { exp(((0.009304*x)+(-0.2583*x)) + (1.4922))/
    (1+exp(((0.009304*x)+(-0.2583*x)) + (1.4922))) }

cnatlog.v3(150)
cnatlog.v3(300)

# Predict probability values
# top
cnatlog.top <- function(x) { exp(((-25.8285*x)) + (1.4922))/
    (1+exp(((-25.8285*x)) + (1.4922))) }

cnatlog.top(0.1)

#gather# Predict probability values
# bottom
cnatlog.bottom <- function(x) { exp(((-0.6978*x)) + (1.5718))/
    (1+exp(((-0.6978*x)) + (1.5718))) }

cnatlog.bottom(0)

cnat.plot.correct.v3<-ggplot(CNATn3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "violet", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "violet",
              fill = "violet",
              size = 1)+
  geom_line(stat='function', 
            fun=cnatlog.v3, 
            color='firebrick', 
            size=1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Cnat",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

cnat.plot.correct.v3

# OFAV

# Bottom estimates
# Intercept  0.06687
# treatment_n -0.67630

# Top estimates
# Intercept -0.04276
# treatment_n -1.51639 

# (Intercept)  -0.1186
# treatment_n  -1.4843  

ofavlog.v3 <- function(x) { exp(((0.009017*x)+(-0.019964*x)) + (-0.093048))/
    (1+exp(((0.009017*x)+(-0.019964*x)) + (-0.093048))) }

ofavlog.v3(150)
ofavlog.v3(300)

# Predict probability values
# top
ofavlog.top <- function(x) { exp(((-1.4843*x)) + (-0.1186))/
    (1+exp(((-1.4843*x)) + (-0.1186))) }

ofavlog.top(0)
ofavlog.top(2)
ofavlog.top(4)

# Predict probability values
# bottom
ofavlog.bottom <- function(x) { exp(((-0.67630*x)) + (0.06687))/
    (1+exp(((-0.67630*x)) + (0.06687))) }

ofavlog.bottom(4)

ofav.plot.correct.v3<-ggplot(OFAVn3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "seagreen3", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
  method.args = list(family = "binomial"),
  color = "seagreen3",
  fill = "seagreen3",
  size = 1)+
  geom_line(stat='function', 
            fun=ofavlog.v3, 
            color='firebrick', 
            size=1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Ofav",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

ofav.plot.correct.v3


# PSTR

# Bottom estimates
# (Intercept)   1.7652
# treatment_n  -0.4027

# Top estimates
# (Intercept)   1.1610
# treatment_n  -1.6094  

# (Intercept)   1.1101
# treatment_n  -1.5785

pstrlog.v3 <- function(x) { exp(((0.005370*x)+(-0.021184*x)) + (1.126272))/
    (1+exp(((0.005370*x)+(-0.021184*x)) + (1.126272))) }

pstrlog.v3(0)
pstrlog.v3(150)
pstrlog.v3(300)

# Predict probability values
# top
pstrlog.top <- function(x) { exp(((-1.5785*x)) + (1.1101))/
    (1+exp(((-1.5785*x)) + (1.1101))) }

pstrlog.top(0)
pstrlog.top(2)
pstrlog.top(4)

# Predict probability values
# bottom
pstrlog.bottom <- function(x) { exp(((-0.4027*x)) + (1.7652))/
    (1+exp(((-0.4027*x)) + (1.7652))) }

pstrlog.bottom(4)

pstr.plot.correct.v3<-ggplot(PSTRn3,aes(x=sed_load_mgcm2, y=Binary))+
  geom_point(shape = 16, 
             color = "goldenrod1", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "goldenrod1",
              fill = "goldenrod1",
              size = 1)+
  geom_line(stat='function', 
            fun=pstrlog.v3, 
            color='firebrick', 
            size=1)+
  labs(x = expression(Sediment~load~(mg~cm^{"-2"})), 
       y = expression("Probability of settlement"))+
  #annotate(geom="text", 
   #        x=150, y=0.8, 
    #       label="Pstr",
     #      color="black",
      #     size = 6)+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))

pstr.plot.correct.v3

ggarrange(cnat.plot.correct.v3, ofav.plot.correct.v3,
          pcli.plot.v3, pstr.plot.correct.v3,
          labels = c("A", "B", "C", "D"), common.legend = T,
          ncol = 2, nrow = 2)

################## Final publication figure ##################

set.fig<-ggarrange(cnat.plot.correct.v3 + rremove("ylab") + rremove("xlab"), 
                   dlab.plot.v3 + rremove("ylab") + rremove("xlab"), 
                   ofav.plot.correct.v3 + rremove("ylab") + rremove("xlab"),
                   pcli.plot.v3 + rremove("ylab") + rremove("xlab"), 
                   pstr.plot.correct.v3 + rremove("ylab") + rremove("xlab"),
                   cnat.bot.plot.v3 + rremove("ylab") + rremove("xlab"), 
                   dlab.bot.plot.v3 + rremove("ylab") + rremove("xlab"), 
                   ofav.bot.plot.v3 + rremove("ylab") + rremove("xlab"), 
                   pcli.bot.plot.v3 + rremove("ylab") + rremove("xlab"), 
                   pstr.bot.plot.v3 + rremove("ylab") + rremove("xlab"),
                   labels = c("", "", "", "", "", "","",""), 
                   common.legend = T,
                   ncol = 5, nrow = 2)

par(mar = c(4,4,4,4))

annotate_figure(set.fig, left = text_grob("Probability of settlement", 
                                         rot = 90, vjust = 1, size = 18),
                bottom = text_grob(expression(Sediment~load~(mg~cm^{"-2"})), size = 18))

?annotate_figure

# PCLI
pclilog.top <- function(x) { exp(((-2.6467*x)) + (3.0872))/
    (1+exp(((-2.6467*x)) + (3.0872))) }

pclilog.top(0)
pclilog.top(2)
pclilog.top(4)


