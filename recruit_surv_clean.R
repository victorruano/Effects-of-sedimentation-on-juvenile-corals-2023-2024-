library(nlme)
library(lme4)
library(tidyverse)
library(survival)
library(survminer)
library(ggpubr)

rm(list = ls())

setwd("/Users/victorruano/Library/CloudStorage/OneDrive-UniversityofMiami/CIMAS/Recruits Sed Exposures")

################## Survival analyses for 1 mo recruits ##################

month1.d<-read.csv("Recruits_dead_m1_v2.csv")

CNAT.m1.d<-subset(month1.d, month1.d$Species=="Cnat")
PSTR.m1.d<-subset(month1.d, month1.d$Species=="Pstr")
OFAV.m1.d<-subset(month1.d, month1.d$Species=="Ofav")

### CNAT
CNAT.m1.d$Treatment<-factor(CNAT.m1.d$Treatment, 
                            levels = c("control","2mm","4mm"))

sfit.cnat = survfit(Surv(Day, Binary)~Treatment, data=CNAT.m1.d)
summary(sfit.cnat)

# survival_test <- survdiff(Surv(time, status) ~ treatment, data = recruit_data)
# summary(survival_test)

kp.cnat1<-ggsurvplot(sfit.cnat,
                    conf.int=TRUE,
                    legend.labs=c("control", "2 mm", "4 mm"), # change group labels
                    legend.title="Treatment",  # add legend title
                    palette=c("steelblue", "goldenrod", "firebrick"),
                    font.x = 18,
                    font.y = 18,
                    font.tickslab = 16,
                    font.legend = 18)

kp.cnat1$plot <- kp.cnat1$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")
kp.cnat1

surv.cnat.m1 <- coxph(Surv(Day, Binary) ~
                        Treatment, data=CNAT.m1.d)
summary(surv.cnat.m1)

res.cnat.1<-read.csv("CNAT_coxph_out_m1.csv")

p_mid.cnat.1 <-ggplot(res.cnat.1, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size= 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.05,100))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", size = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))
p_mid.cnat.1

test.ph.cnat = cox.zph(surv.cnat.m1)
ggcoxzph(test.ph.cnat)

ggcoxdiagnostics(surv.cnat.m1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

### PSTR
PSTR.m1.d$Treatment<-factor(PSTR.m1.d$Treatment, 
                            levels = c("control","2mm","4mm"))

sfit.pstr = survfit(Surv(Day, Binary)~Treatment, data=PSTR.m1.d)
summary(sfit.pstr)

kp.pstr1<-ggsurvplot(sfit.pstr,
                    conf.int=TRUE,
                    legend.labs=c("control", "2 mm", "4 mm"), # change group labels
                    legend.title="Treatment",  # add legend title
                    palette=c("steelblue", "goldenrod", "firebrick"),
                    font.x = 18,
                    font.y = 18,
                    font.tickslab = 16,
                    font.legend = 18)

kp.pstr1$plot <- kp.pstr1$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")
kp.pstr1

surv.pstr.m1 <- coxph(Surv(Day, Binary) ~
                        Treatment, data=PSTR.m1.d)
summary(surv.pstr.m1)

res.pstr.1<-read.csv("PSTR_coxph_out_m1.csv")

p_mid.pstr.1 <-ggplot(res.pstr.1, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size= 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.05,100))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", size = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))

p_mid.pstr.1

test.ph.pstr = cox.zph(surv.pstr.m1)
ggcoxzph(test.ph.pstr)

ggcoxdiagnostics(surv.pstr.m1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

###OFAV
OFAV.m1.d$Treatment<-factor(OFAV.m1.d$Treatment, 
                            levels = c("control","2mm","4mm"))

sfit.ofav = survfit(Surv(Day, Binary)~Treatment, data=OFAV.m1.d)
summary(sfit.ofav)

kp.ofav1<-ggsurvplot(sfit.ofav,
                    conf.int=TRUE,
                    legend.labs=c("control", "2 mm", "4 mm"), # change group labels
                    legend.title="Treatment",  # add legend title
                    palette=c("steelblue", "goldenrod", "firebrick"),
                    font.x = 18,
                    font.y = 18,
                    font.tickslab = 16,
                    font.legend = 18)

kp.ofav1$plot <- kp.ofav1$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")
kp.ofav1

surv.ofav.m1 <- coxph(Surv(Day, Binary) ~
                        Treatment, data=OFAV.m1.d)
summary(surv.ofav.m1)

res.ofav.1<-read.csv("OFAV_coxph_out_m1.csv")

p_mid.ofav.1 <-ggplot(res.ofav.1, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size= 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.05,200))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", size = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))

p_mid.ofav.1

test.ph.ofav = cox.zph(surv.ofav.m1)
ggcoxzph(test.ph.ofav)

ggcoxdiagnostics(surv.ofav.m1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

legend.kp <- get_legend(
  kp.cnat1$plot + theme(legend.position = "top")
)

legend.forest <- get_legend(
  p_mid.cnat.1 + theme(legend.position = "top")
)

combined_plots.1<-ggarrange(kp.cnat1$plot + rremove("ylab") + rremove("xlab") + rremove("legend"),
                          p_mid.cnat.1 + rremove("ylab") + rremove("xlab") + rremove("legend"),
                          kp.ofav1$plot + rremove("ylab") + rremove("xlab") + rremove("legend"),
                          p_mid.ofav.1 + rremove("ylab") + rremove("xlab") + rremove("legend"),
                          kp.pstr1$plot + rremove("ylab") + rremove("legend"),
                          p_mid.pstr.1 + rremove("ylab") + rremove("legend"),
                          #widths = c(0.6, 0.4, 0.6, 0.4),
                          ncol = 2, nrow = 3#,
                          #labels = c("a", "", "b", "", "c"),
                          #vjust = -0.05
)


########################## Survival analyses for 3 mo recruits #################################

month3.d<-read.csv("Recruits_dead_m3.csv")
month3.d<-read.csv("Recruits_dead_m3_noalgae.csv")

ACER.m3.d<-subset(month3.d, month3.d$Species=="Acer")
CNAT.m3.d<-subset(month3.d, month3.d$Species=="Cnat")
PSTR.m3.d<-subset(month3.d, month3.d$Species=="Pstr")
OFAV.m3.d<-subset(month3.d, month3.d$Species=="Ofav")

### ACER
ACER.m3.d$Treatment<-factor(ACER.m3.d$Treatment, 
                            levels = c("control","2mm","4mm"))

sfit.acer3 = survfit(Surv(Day, Binary)~Treatment, data=ACER.m3.d)
summary(sfit.acer3)

kp.acer3<-ggsurvplot(sfit.acer3,
           conf.int=TRUE,
           legend.labs=c("control", "2 mm", "4 mm"), # change group labels
           legend.title="Treatment",  # add legend title
           palette=c("steelblue", "goldenrod", "firebrick"),
           font.x = 18,
           font.y = 18,
           font.tickslab = 16,
           font.legend = 18)

kp.acer3$plot <- kp.acer3$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")
kp.acer3

surv.acer.m3 <- coxph(Surv(Day, Binary) ~
                        Treatment, data=ACER.m3.d)
summary(surv.acer.m3)

test.ph.acer3 = cox.zph(surv.acer.m3)
ggcoxzph(test.ph.acer3)

ggcoxdiagnostics(surv.acer.m3, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

res.acer<-read.csv("ACER_coxph_out.csv")

# create forest plot on log scale (middle section of figure)
p_mid.acer3 <-ggplot(res.acer, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size = 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(1,800))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", size = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))

p_mid.acer3

### CNAT
CNAT.m3.d$Treatment<-factor(CNAT.m3.d$Treatment, 
                            levels = c("control","2mm","4mm"))

sfit.cnat3 = survfit(Surv(Day, Binary)~Treatment, data=CNAT.m3.d)
summary(sfit.cnat3)

kp.cnat3<-ggsurvplot(sfit.cnat3,
           conf.int=TRUE,
           legend.labs=c("control", "2 mm", "4 mm"), # change group labels
           legend.title="Treatment",  # add legend title
           palette=c("steelblue", "goldenrod", "firebrick"),
           font.x = 18,
           font.y = 18,
           font.tickslab = 16,
           font.legend = 18)

kp.cnat3$plot <- kp.cnat3$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")
kp.cnat3

surv.cnat.m3 <- coxph(Surv(Day, Binary) ~
                        Treatment, data=CNAT.m3.d)
summary(surv.cnat.m3)

res.cnat<-read.csv("CNAT_coxph_out.csv")

# create forest plot on log scale (middle section of figure)
p_mid.cnat3 <-ggplot(res.cnat, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size = 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.2,15))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", size = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))
p_mid.cnat3

test.ph.cnat3 = cox.zph(surv.cnat.m3)
ggcoxzph(test.ph.cnat3)
ggcoxdiagnostics(surv.cnat.m1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

### OFAV
OFAV.m3.d$Treatment<-factor(OFAV.m3.d$Treatment, 
                            levels = c("control","2mm","4mm"))

sfit.ofav3 = survfit(Surv(Day, Binary)~Treatment, data=OFAV.m3.d)
summary(sfit.ofav3)

# survival_test <- survdiff(Surv(time, status) ~ treatment, data = recruit_data)
# summary(survival_test)

kp.ofav3<-ggsurvplot(sfit.ofav3,
           conf.int=TRUE,
           legend.labs=c("control", "2 mm", "4 mm"), # change group labels
           legend.title="Treatment",  # add legend title
           palette=c("steelblue", "goldenrod", "firebrick"),
           font.x = 18,
           font.y = 18,
           font.tickslab = 16,
           font.legend = 18)

kp.ofav3$plot <- kp.ofav3$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")
kp.ofav3

surv.ofav.m3 <- coxph(Surv(Day, Binary) ~
                        Treatment, data=OFAV.m3.d)
summary(surv.ofav.m3)

res.ofav<-read.csv("OFAV_coxph_out.csv")

# create forest plot on log scale (middle section of figure)
p_mid.ofav3 <-ggplot(res.ofav, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size= 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.3,30))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", size = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))
p_mid.ofav3

test.ph.ofav3 = cox.zph(surv.ofav.m3)
# From the output above, the test is not statistically significant for each of the covariates, 
# and the global test is also not statistically significant. Therefore, 
# our proportional hazards assumption is reasonable.

ggcoxzph(test.ph.ofav3)
ggcoxdiagnostics(surv.ofav.m3, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# PSTR
PSTR.m3.d$Treatment<-factor(PSTR.m3.d$Treatment, 
                            levels = c("control","2mm","4mm"))

sfit.pstr3 = survfit(Surv(Day, Binary)~Treatment, data=PSTR.m3.d)
summary(sfit.pstr3)

# survival_test <- survdiff(Surv(time, status) ~ treatment, data = recruit_data)
# summary(survival_test)

kp.pstr3<-ggsurvplot(sfit.pstr3,
           conf.int=TRUE,
           legend.labs=c("control", "2 mm", "4 mm"), # change group labels
           legend.title="Treatment",  # add legend title
           palette=c("steelblue", "goldenrod", "firebrick"),
           font.x = 18,
           font.y = 18,
           font.tickslab = 16,
           font.legend = 18)

kp.pstr3$plot <- kp.pstr3$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")
kp.pstr3

surv.pstr.m3 <- coxph(Surv(Day, Binary) ~
                        Treatment, data=PSTR.m3.d)
summary(surv.pstr.m3)

res.pstr<-read.csv("PSTR_coxph_out.csv")

# create forest plot on log scale (middle section of figure)
p_mid.pstr3 <-ggplot(res.pstr, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size= 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.1,100))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", size = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))
p_mid.pstr3


test.ph.pstr3 = cox.zph(surv.pstr.m3)
ggcoxzph(test.ph.pstr3)
ggcoxdiagnostics(surv.pstr.m3, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

combined_plots.3<-ggarrange(kp.acer3$plot + rremove("ylab") + rremove("xlab") + rremove("legend"),
                            p_mid.acer3 + rremove("ylab") + rremove("xlab") + rremove("legend"),
                            kp.cnat3$plot + rremove("ylab") + rremove("xlab") + rremove("legend"),
                            p_mid.cnat3 + rremove("ylab") + rremove("xlab") + rremove("legend"),
                            kp.ofav3$plot + rremove("ylab") + rremove("xlab") + rremove("legend"),
                            p_mid.ofav3 + rremove("ylab") + rremove("xlab") + rremove("legend"),
                            kp.pstr3$plot + rremove("ylab") + rremove("legend"),
                            p_mid.pstr3 + rremove("ylab") + rremove("legend"),
                            #widths = c(0.6, 0.4, 0.6, 0.4),
                            ncol = 2, nrow = 4#,
                            #labels = c("d", "", "e", "", "f", "", "g", ""),
                            #vjust = -0.02
)


mo1.3_surv_plot <- ggarrange(
  combined_plots.1,
  combined_plots.3,
  ncol = 2,
  heights = c(1,1)
)

# Display the final plot
mo1.3_surv_plot
annotate_figure(mo1.3_surv_plot, left = text_grob("Survival probability", 
                                                      rot = 90, vjust = 0.75, size = 19))


ggsave("Fig 2_new_V1.tiff", units="in", bg = "white", width=13, height=8, dpi=300)


################## Survival analyses for 6 mo DLAB ##################
month6.d<-read.csv("Recruits_dead_m6.csv")

month6.d$Treatment<-factor(month6.d$Treatment, 
                           levels = c("control","2mm","4mm"))

sfit.dlab6 = survfit(Surv(Day, Binary)~Treatment, data=month6.d)
summary(sfit.dlab6)

kp.dlab<-ggsurvplot(sfit.dlab6,
           conf.int=TRUE,
           legend.labs=c("control", "2 mm", "4 mm"), # change group labels
           legend.title="Treatment",  # add legend title
           palette=c("steelblue", "goldenrod", "firebrick"),
           font.x = 18,
           font.y = 18,
           font.tickslab = 16,
           font.legend = 18)

kp.dlab$plot <- kp.dlab$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")
kp.dlab


surv.dlab.m6 <- coxph(Surv(Day, Binary) ~
                        Treatment, data=month6.d)
summary(surv.dlab.m6)

test.ph.dlab6 = cox.zph(surv.dlab.m6)
ggcoxzph(test.ph.dlab6)

ggcoxdiagnostics(surv.dlab.m6, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

res.dlab<-read.csv("DLAB_coxph_out.csv")

p_mid.dlab <-ggplot(res.dlab, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size = 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.1,200))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", size = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))

p_mid.dlab




################ Survival analyses for OFAV fragments #################

setwd("/Users/victorruano/Library/CloudStorage/OneDrive-UniversityofMiami/CIMAS/Recruits Sed Exposures")

### 1 cm (6 month olds)

ofav.frag.cox<-read.csv("OFAV_frag_survival_cox.csv")

ofav.frag.cox$Treatment<-factor(ofav.frag.cox$Treatment, 
                                levels = c("control","2mm","4mm"))

ofav.frag.cox.1cm <- filter(ofav.frag.cox, Size_cm == "1")

set.seed(123)
num_pseudo_events <- 1  # Number of pseudo-events to add

# Find the indices of control samples
control_indices.1 <- which(ofav.frag.cox.1cm$Treatment == "control")

# Randomly select indices for pseudo-events
pseudo_event_indices.1 <- sample(control_indices.1, num_pseudo_events)

# Add pseudo-events
ofav.frag.cox.1cm$Status_bi[pseudo_event_indices.1] <- 1

# Fit the Cox proportional hazards model
cox_model.1cm <- coxph(Surv(Day, Status_bi) ~ Treatment, data = ofav.frag.cox.1cm)
summary(cox_model.1cm)

test.ph.ofav1 = cox.zph(cox_model.1cm)
ggcoxzph(test.ph.ofav1)

ggcoxdiagnostics(cox_model.1cm, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# Kaplan-Meier curve
sfit.ofav.1cm = survfit(Surv(Day, Status_bi)~Treatment, data=ofav.frag.cox.1cm)
summary(sfit.ofav.1cm)

kp.ofav.1<-ggsurvplot(sfit.ofav.1cm,
           conf.int=TRUE,
           legend.labs=c("Control" ,"2 mm", "4 mm"), # change group labels
           legend.title="Treatment",  # add legend title
           palette=c("steelblue", "goldenrod", "firebrick"),
           font.x = 18,
           font.y = 18,
           font.tickslab = 16,
           font.legend = 18)

kp.ofav.1$plot <- kp.ofav.1$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")

kp.ofav.1

res.ofav1<-read.csv("OFAV_1cm_cox_output.csv")

# create forest plot on log scale (middle section of figure)
p_mid.ofav1 <-ggplot(res.ofav1, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size = 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.2,100))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", linewidth = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))

p_mid.ofav1


### 1.5 cm (12 month olds)
ofav.frag.cox.1.5cm <- filter(ofav.frag.cox, Size_cm == "1.5")

set.seed(456)
num_pseudo_events <- 1  # Number of pseudo-events to add

# Find the indices of control samples
control_indices.1.5 <- which(ofav.frag.cox.1.5cm$Treatment == "control")

# Randomly select indices for pseudo-events
pseudo_event_indices.1.5 <- sample(control_indices.1.5, num_pseudo_events)

# Add pseudo-events
ofav.frag.cox.1.5cm$Status_bi[pseudo_event_indices.1.5] <- 1

# Fit the Cox proportional hazards model
cox_model.1.5cm <- coxph(Surv(Day, Status_bi) ~ Treatment, data = ofav.frag.cox.1.5cm)
summary(cox_model.1.5cm)

test.ph.ofav1.5 = cox.zph(cox_model.1.5cm)
ggcoxzph(test.ph.ofav1.5)

ggcoxdiagnostics(cox_model.1.5cm, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# Kaplan-Meier curve
sfit.ofav.1.5cm = survfit(Surv(Day, Status_bi)~Treatment, data=ofav.frag.cox.1.5cm)
summary(sfit.ofav.1.5cm)

kp.ofav.1.5<-ggsurvplot(sfit.ofav.1.5cm,
           conf.int=TRUE,
           legend.labs=c("Control" ,"2 mm", "4 mm"), # change group labels
           legend.title="Treatment",  # add legend title
           palette=c("steelblue", "goldenrod", "firebrick"),
           font.x = 18,
           font.y = 18,
           font.tickslab = 16,
           font.legend = 18)

kp.ofav.1.5$plot <- kp.ofav.1.5$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")

kp.ofav.1.5

res.ofav1.5<-read.csv("OFAV_1.5cm_cox_output.csv")

# create forest plot on log scale (middle section of figure)
p_mid.ofav1.5 <-ggplot(res.ofav1.5, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size = 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.2,110))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", linewidth = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))

p_mid.ofav1.5


### 2 cm (18 month olds)
ofav.frag.cox.2cm <- filter(ofav.frag.cox, Size_cm == "2")

set.seed(100)
num_pseudo_events <- 1  # Number of pseudo-events to add

# Find the indices of control samples
control_indices.2 <- which(ofav.frag.cox.2cm$Treatment == "control")

# Randomly select indices for pseudo-events
pseudo_event_indices.2 <- sample(control_indices.2, num_pseudo_events)

# Add pseudo-events
ofav.frag.cox.2cm$Status_bi[pseudo_event_indices.2] <- 1

# Fit the Cox proportional hazards model
cox_model.2cm <- coxph(Surv(Day, Status_bi) ~ Treatment, data = ofav.frag.cox.2cm)
summary(cox_model.2cm)

test.ph.ofav2 = cox.zph(cox_model.2cm)
ggcoxzph(test.ph.ofav2)

ggcoxdiagnostics(cox_model.2cm, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# Kaplan-Meier curve
sfit.ofav.2cm = survfit(Surv(Day, Status_bi)~Treatment, data=ofav.frag.cox.2cm)
summary(sfit.ofav.2cm)

kp.ofav.2<-ggsurvplot(sfit.ofav.2cm,
           conf.int=TRUE,
           legend.labs=c("Control" ,"2 mm", "4 mm"), # change group labels
           legend.title="Treatment",  # add legend title
           palette=c("steelblue", "goldenrod", "firebrick"),
           font.x = 18,
           font.y = 18,
           font.tickslab = 16,
           font.legend = 18)

kp.ofav.2$plot <- kp.ofav.2$plot + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme(axis.ticks.length = unit(0.3, "cm")) +
  xlab("Time (days)")

kp.ofav.2

res.ofav2<-read.csv("OFAV_2cm_cox_output.csv")

# create forest plot on log scale (middle section of figure)
p_mid.ofav2 <-ggplot(res.ofav2, aes(y = fct_rev(Treatment), color = Treatment)) +
  theme_classic() +
  geom_point(aes(x=estimate), size = 5) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high), size = 1.5) +
  scale_x_log10(limits = c(0.2,150))+
  labs(x="Hazard Ratio") +
  geom_vline(linetype="dashed",xintercept = 1, colour = "steelblue", linewidth = 1.5) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_color_manual(values = c("goldenrod", "firebrick"))

p_mid.ofav2


######################## All plots combined ######################

legend.kp <- get_legend(
  kp.dlab$plot + theme(legend.position = "top")
)

legend.forest <- get_legend(
  p_mid.dlab + theme(legend.position = "top")
)


combined_plots<-ggarrange(kp.dlab$plot + rremove("ylab") + rremove("xlab") + rremove("legend"),
                               p_mid.dlab + rremove("ylab") + rremove("xlab") + rremove("legend"),
                               kp.ofav.1$plot + rremove("ylab") + rremove("xlab") + rremove("legend"),
                               p_mid.ofav1 + rremove("ylab") + rremove("xlab") + rremove("legend"),
                               kp.ofav.1.5$plot + rremove("ylab") + rremove("legend"),
                               p_mid.ofav1.5 + rremove("ylab") + rremove("legend"),
                               kp.ofav.2$plot + rremove("ylab") + rremove("legend"),
                               p_mid.ofav2 + rremove("ylab") + rremove("legend"),
                               #widths = c(0.6, 0.4, 0.6, 0.4),
                               ncol = 4, nrow = 2,
                               labels = c("a", "", "b", "", "c", "", "d"),
                          vjust = -0.05
                          )

OFAV.DLAB_surv_plot <- ggarrange(
  legend.kp,
  legend.forest,
  combined_plots,
  ncol = 1,
  heights = c(0.1, 0.1, 1)
)

# Display the final plot
OFAV.DLAB_surv_plot
annotate_figure(OFAV.DLAB_surv_plot, left = text_grob("Survival probability", 
                                        rot = 90, vjust = 0.75, size = 19))


ggsave("Fig 3_new_V1.tiff", units="in", bg = "white", width=12, height=7, dpi=300)
