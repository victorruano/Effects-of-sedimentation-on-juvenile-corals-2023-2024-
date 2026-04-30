library(nlme)
library(lme4)
library(tidyverse)
library(ggpubr)
library(Matrix)
library(grid)
library(performance)
library(ggeffects)
library(emmeans)

rm(list=ls())

setwd("/Users/victorruano/Library/CloudStorage/OneDrive-UniversityofMiami/CIMAS/Settlement Rates")

##################### Top trials ##########################
sedtop3<-read.csv("Filtered_settled_top_bi_n_V3.csv")

# Test different models
spp.glm <- glmer(
  cbind(settled, total_larvae - settled) ~ species + sed_load_mgcm2 + 
    (1 | run),
  data = sedtop3,
  family = binomial
)
summary(spp.glm)

# Need to scale the values of sediment load
sedtop3$load_scaled <- scale(sedtop3$sed_load_mgcm2) 

# Test GLMM without interactions
spp.glm2 <- glmer(cbind(settled, total_larvae - settled) ~ species + 
                    load_scaled + (1 | run), data = sedtop3, family = binomial )
summary(spp.glm2)

# Model with interactions has the lowest AIC
spp.glm_int <- glmer(cbind(settled, total_larvae - settled) ~ species * 
                       load_scaled + (1 | run), data = sedtop3, family = binomial )
summary(spp.glm_int)

# Testing whether sensitivity differs among species
# Significant result → at least one species responds differently to sediment load
# Non-significant → all species are similarly sensitive
anova(spp.glm2, spp.glm_int, test = "Chisq")

# Extract species-specific slopes
emtrends(spp.glm_int, ~ species, var = "load_scaled")

# Pairwise comparisons of sensitivity
pairs(emtrends(spp.glm_int, ~ species, var = "load_scaled"))

# Get predictions across the range of sediment load for each species
pred <- ggpredict(spp.glm_int, terms = c("load_scaled [all]", "species"))

sedtop3$group <- sedtop3$species

center <- attr(sedtop3$load_scaled, "scaled:center")
scale  <- attr(sedtop3$load_scaled, "scaled:scale")

pred$x_original <- pred$x * scale + center

# Plot
top_plot<-ggplot(pred, aes(x = x_original, y = predicted, color = group)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  geom_point(data = sedtop3,
             aes(x = sed_load_mgcm2, y = settled / total_larvae, color = group),
             inherit.aes = FALSE,
             alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.7)+
  scale_y_continuous(limits = c(0,1))+
  facet_wrap(~ group, nrow = 1, ncol = 5) +
  scale_fill_manual(values = c("violet", "maroon", "seagreen3", "olivedrab", "goldenrod1"))+
  scale_color_manual(values = c("violet", "maroon", "seagreen3", "olivedrab", "goldenrod1"))+
  labs(x = "Sediment load (mg/cm²)",
       y = "Settlement probability",
       color = "Species",
       fill = "Species") +
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none",
        axis.ticks.length = unit(0.3, "cm"),
        legend.text = element_blank(),
        legend.title = element_blank())


# Build separate models for each species
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

cnat.glm2 <- glmer(cbind(settled, total_larvae - settled) ~ 
                     load_scaled + (1 | run), data = CNATn3, family = binomial )
summary(cnat.glm2)

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

ofav.glm2 <- glmer(cbind(settled, total_larvae - settled) ~ 
                      load_scaled + (1 | run), data = OFAVn3, family = binomial )
summary(ofav.glm2)

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

pcli.glm2 <- glmer(cbind(settled, total_larvae - settled) ~ 
                     load_scaled + (1 | run), data = PCLIn3, family = binomial )
summary(pcli.glm2)

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

sedbot3<-read.csv("set_treat_bot_V4.csv")

spp.b.glm <- glmer(
  cbind(settled, total_larvae - settled) ~ species + sed_load_mgcm2 + 
    (1 | run),
  data = sedbot3,
  family = binomial
)

summary(spp.glm)

sedbot3$load_scaled <- scale(sedbot3$sed_load_mgcm2) 

spp.b.glm2 <- glmer(cbind(settled, total_larvae - settled) ~ species + 
                    load_scaled + (1 | run), data = sedbot3, family = binomial )
summary(spp.b.glm2)

spp.b.glm_int <- glmer(cbind(settled, total_larvae - settled) ~ species * 
                       load_scaled + (1 | run), data = sedbot3, family = binomial )
summary(spp.b.glm_int)

# Testing whether sensitivity differs among species
# Significant result → at least one species responds differently to sediment load
# Non-significant → all species are similarly sensitive
anova(spp.b.glm2, spp.b.glm_int, test = "Chisq")

# Extract species-specific slopes
emtrends(spp.b.glm_int, ~ species, var = "load_scaled")

# Pairwise comparisons of sensitivity
pairs(emtrends(spp.b.glm_int, ~ species, var = "load_scaled"))

# Get predictions across the range of sediment load for each species
pred_bot <- ggpredict(spp.b.glm_int, terms = c("load_scaled [all]", "species"))

sedbot3$group <- sedbot3$species

center_bot <- attr(sedbot3$load_scaled, "scaled:center")
scale_bot  <- attr(sedbot3$load_scaled, "scaled:scale")

pred_bot$x_original <- pred_bot$x * scale_bot + center_bot

# Plot
bottom_plot<-ggplot(pred_bot, aes(x = x_original, y = predicted, color = group)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  geom_point(data = sedbot3,
             aes(x = sed_load_mgcm2, y = settled / total_larvae, color = group),
             inherit.aes = FALSE,
             alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.7)+
  scale_y_continuous(limits = c(0,1))+
  facet_wrap(~ group, nrow = 1, ncol = 5) +
  scale_fill_manual(values = c("violet", "maroon", "seagreen3", "olivedrab", "goldenrod1"))+
  scale_color_manual(values = c("violet", "maroon", "seagreen3", "olivedrab", "goldenrod1"))+
  labs(x = "Sediment load (mg/cm²)",
       y = "Settlement probability",
       color = "Species",
       fill = "Species") +
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none",
        axis.ticks.length = unit(0.3, "cm"),
        legend.text = element_blank(),
        legend.title = element_blank())

################ Final publication figure ###############################
set.fig.prop<-ggarrange(top_plot + rremove("ylab") + rremove("xlab"), 
                        bottom_plot + rremove("ylab") + rremove("xlab"), 
                        labels = c("", ""),
                        ncol = 1, nrow = 2)

set.fig.prop<-annotate_figure(set.fig.prop, left = text_grob("Settlement probability", 
                                          rot = 90, vjust = 1, size = 16),
                bottom = text_grob(expression(Sediment~load~(mg~cm^{"-2"})), size = 16))

ggsave("Fig1_correct.tiff", plot = set.fig.prop, width = 8, height = 5, units = "in", dpi = 300, bg = "white")
?ggsave


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

################### Sediment source analysis ###################

source<-read.csv("Sed source.csv")

source$treatment<-factor(source$treatment, 
                            levels = c("control","sprinkle","2mm", "4mm"))

gg.source<-ggplot(source) +
  aes(x = interaction(treatment, source), y = prop_settled, fill = source) +
  geom_violin(width = 1, position = position_dodge(0.9), show.legend = TRUE) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), fill = "white")+
  facet_wrap(vars(species))+
  scale_fill_manual(values=c("steelblue", "goldenrod", "firebrick"))+
  theme_classic() +  
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top") +
  labs(y = "Proportion settled")+
  scale_y_continuous(limits = c(0, 0.25))
gg.source

source.glme<-glmer(binary~species*source+load_mgcm2+(1|load_mgcm2:replicate),
                               family=binomial, data=source)

m1 <- glmer(
  cbind(settled, total_larvae - settled) ~ source + load_mgcm2 + species + 
    (1 | run),
  data = source,
  family = binomial
)

m2 <- glmer(
  cbind(settled, total_larvae - settled) ~ source * load_mgcm2 + species + 
    (1 | run),
  data = source,
  family = binomial
)

anova(m1, m2, test = "Chisq")


df_sub <- subset(source, source %in% c("KL", "PEV"))

m <- glmer(
  cbind(settled, total_larvae - settled) ~ source + load_mgcm2 + species + 
    (1 | run),
  data = df_sub,
  family = binomial
)

summary(m)

library(emmeans)
emmeans(m, pairwise ~ source)
