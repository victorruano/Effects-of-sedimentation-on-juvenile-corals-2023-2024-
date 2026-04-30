# Recruits and LSAT Analysis
library(nlme)
library(lme4)
library(tidyverse)
library(ggbreak)

update.packages(ask = FALSE, checkBuilt = TRUE)

rm(list = ls())

setwd("/Users/victorruano/Library/CloudStorage/OneDrive-UniversityofMiami/CIMAS/Florida Sediments/R scripts")

lsat.comp<-read.csv("LSAT_comp_S23.csv")

lsat.comp$Site<-factor(lsat.comp$Site, levels = c("Ft Lauderdale","Dania Beach","South Canyon", 
                                                  "Emerald Reef", "Carysfort Reef", "Conch Reef"))

# Recruits and LSAT data
rec.lsat<-read.csv("Recruits_LSAT.csv")


# Reorganize order of sites
rec.lsat$Site<-factor(rec.lsat$Site, levels = c("Ft Lauderdale","Dania Beach","South Canyon", 
                                                "Emerald Reef", "Carysfort Reef", "Conch Reef"))

# Subset by season
LSAT.S23<-subset(rec.lsat, rec.lsat$Season == "Summer")
LSAT.F23<-subset(rec.lsat, rec.lsat$Season == "Fall")

# Sediment trap data (summer)
seds.s23<-read.csv("Sed_traps_S23.csv")

seds.s23$Site<-factor(seds.s23$Site, levels = c("Ft Lauderdale","Dania Beach","South Canyon", 
                                                "Emerald Reef", "Carysfort Reef", "Conch Reef"))

#################### Sediment traps analysis ######################

# One-way ANOVA summer
trans.seds<-log(seds.s23$per_m2)

shapiro.test(trans.seds)
hist(trans.seds)

bartlett.test(trans.seds~seds.s23$Site)

summary(aov(trans.seds~seds.s23$Site))
TukeyHSD(aov(trans.seds~seds.s23$Site))

# Standard Error function
se<-function(x){sd(x,na.rm=T)/sqrt(sum(!is.na(x)))}

seds.s23.summary.m2 <- seds.s23 %>%
  group_by(Site) %>%
  summarise(
    ct = mean(per_m2),
    se = se(per_m2)
  )


# Dot plots
s <- ggplot(
  seds.s23.summary.m2, 
  aes(x = Site, y = ct, ymin = ct-se, ymax = ct+se)) + 
  geom_errorbar(width = 0.15) +
  geom_crossbar(
    aes(y = ct, ymin = ct, ymax = ct),
    width = 0.5,        # <-- controls how wide the mean line is
    middle.linewidth = 0,         # removes the default thick middle bar behavior
    color = "black"
  ) +
  geom_point(
    data = seds.s23,
    aes(x = Site, y = per_m2),  # <-- replace with your raw variable name
    position = position_jitter(width = 0.15),
    alpha = 0.6,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(y = "Sedimentation rate")+
  ylab(expression(Sedimentation~rate~(g~m^{"-2"}~day^{"-1"})))+
  scale_y_continuous(limits = c(0, 15), breaks = seq(0,15, by = 5))+
  theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"),
        axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y=element_text(angle = 90, hjust = 0.5, vjust = 1))

s

ggsave("sedimentation_cross.tiff", plot = s, width = 4, height = 5, units = "in", dpi = 300)
ggsave("sedimentation_cross.jpeg", plot = s, width = 4, height = 5, units = "in", dpi = 300)


############ Analysis of LSAT depth and coral recruits (GLMM) ##############
# Summer
LSAT.s23.glme<-glmer(Presence_juv_corals~LSAT_Sediment_depth_mm + (1|Site),
                     family = binomial, data=LSAT.S23)
summary(LSAT.s23.glme)

lsat.depth.summer<-ggplot(LSAT.S23, 
                          aes(x=LSAT_Sediment_depth_mm,y=Presence_juv_corals))+
  geom_point(shape = 16, 
             color = "steelblue4", 
             size = 3, 
             alpha = 0.5)+
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"),
              color = "steelblue4",
              fill = "steelblue4",
              linewidth = 1)+
  labs(x = "LSAT sediment depth (mm)", 
       y = expression("Presence of juvenile corals"))+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"))


#################### Analysis of LSAT thickness among sites ####################

# From 25x25 plots
# Does not meet ANOVA assumptions
shapiro.test(LSAT.S23$LSAT_Sediment_depth_mm)
hist(LSAT.S23$LSAT_Sediment_depth_mm)

# Trasformations unsuccessful
trans.lsat.d<-log(LSAT.S23$LSAT_Sediment_depth_mm+1)
shapiro.test(trans.lsat.d)

# Use K-W and Dunn tests
kruskal.test(LSAT.S23$LSAT_Sediment_depth_mm~LSAT.S23$Site)
dunn.test(LSAT.S23$LSAT_Sediment_depth_mm, LSAT.S23$Site)

# Summary of medians and MADs
seds.s23.summary.depth <- LSAT.S23 %>%
  group_by(Site) %>%
  summarise(
    med = median(LSAT_Sediment_depth_mm),
    mad = mad(LSAT_Sediment_depth_mm)
  )

### From 5x5 plots
shapiro.test(lsat.comp$Sed_depth_mm)
hist(lsat.comp$Sed_depth_mm)

kruskal.test(lsat.comp$Sed_depth_mm~lsat.comp$Site)
dunn.test(lsat.comp$Sed_depth_mm, lsat.comp$Site)

################### Analysis of LSAT cover among sites ####################
shapiro.test(LSAT.S23$LSAT_.)
bartlett.test(LSAT.S23$LSAT_.~LSAT.S23$Site)

summary(aov(LSAT.S23$LSAT_.~LSAT.S23$Site))
TukeyHSD(aov(LSAT.S23$LSAT_.~LSAT.S23$Site))
#                                 diff         lwr        upr     p adj
# Conch Reef-Ft Lauderdale      21.68   7.3708882 35.9891118 0.0004947
# Emerald Reef-Dania Beach     -15.24 -29.5491118 -0.9308882 0.0307431
# Carysfort Reef-Emerald Reef   15.16   0.8508882 29.4691118 0.0320947
# Conch Reef-Emerald Reef       27.44  13.1308882 41.7491118 0.0000050

seds.s23.summary.cover <- LSAT.S23 %>%
  group_by(Site) %>%
  summarise(
    ct = mean(LSAT_.),
    se = se(LSAT_.)
  )

## violin plots 
gg.lsat.cover.s<-ggplot(LSAT.S23) +
  aes(x = Site, y = LSAT_.) +
  theme_classic() +
  geom_violin(show.legend = FALSE, fill = "olivedrab") +
  geom_boxplot(width=0.1, fill="white")+
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top") +
  labs(y = "LSAT cover (%)")+
  scale_y_continuous(limits = c(0, 100))
gg.lsat.cover.s


gg.lsat.depth.s<-ggplot(LSAT.S23) +
  aes(x = Site, y = LSAT_Sediment_depth_mm) +
  theme_classic() +
  geom_violin(show.legend = FALSE, fill = "olivedrab") +
  geom_boxplot(width=0.1, fill="white")+
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top") +
  labs(y = "LSAT sediment depth (mm)")+
  scale_y_continuous(limits = c(0, 8))
gg.lsat.depth.s

gg.lsat.depth.s.5<-ggplot(lsat.comp) +
  aes(x = Site, y = Sed_depth_mm) +
  theme_classic() +
  geom_violin(show.legend = FALSE, fill = "olivedrab") +
  geom_boxplot(width=0.1, fill="white")+
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top") +
  labs(y = "LSAT sediment depth (mm)")+
  scale_y_continuous(limits = c(0, 15))
gg.lsat.depth.s.5


######################### Turf length vs sed depth ###########################
SedimentDTurfLGraph<- ggplot(data = lsat.comp, aes(Turf_length_mm, Sed_depth_mm)) + 
  stat_summary (geom= "point", fun.y = "mean", size=4, show.legend=F) +
  geom_smooth(method = "lm", se = T, color = "black", size=1, fill="lightgray") +
  ylab("LSAT sediment depth (mm)") + xlab ("Turf algae length (mm)") +
  scale_y_continuous(breaks = seq(0, 6, by = 1))+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  theme (
    axis.text = element_text(color='black', size=14),
    axis.title = element_text(color='black', size=16),
    axis.ticks = element_line(color='black'),
    axis.ticks.length=unit(.25, "cm"),
    legend.title = element_text (size=18),
    legend.position =c(0.15,0.75),
    panel.background = element_rect (fill=NA, color='black'),
    panel.grid = element_blank()
  )
SedimentDTurfLGraph

lsat.turf.lme<-lme(Sed_depth_mm~Turf_length_mm, random = ~ 1 | Site, data = lsat.comp)
summary(lsat.turf.lme)

# Use a placeholder for Site, e.g., the first level of Site from the original data
unique_sites <- unique(lsat.comp$Site)
new_data_FLL <- data.frame(Site = unique_sites[1], Turf_length_mm = seq(0, 10, by = 0.5))
new_data_DB <- data.frame(Site = unique_sites[2], Turf_length_mm = seq(0, 10, by = 0.5))
new_data_ER <- data.frame(Site = unique_sites[3], Turf_length_mm = seq(0, 10, by = 0.5))
new_data_CR <- data.frame(Site = unique_sites[4], Turf_length_mm = seq(0, 10, by = 0.5))
new_data_CH <- data.frame(Site = unique_sites[5], Turf_length_mm = seq(0, 10, by = 0.5))
new_data_SC <- data.frame(Site = unique_sites[6], Turf_length_mm = seq(0, 10, by = 0.5))

new_data_site<-rbind(new_data_FLL, new_data_DB, new_data_ER, new_data_CR, new_data_CH, new_data_SC)

# Make predictions
predictions <- as.data.frame(predict(lsat.turf.lme, new_data_site))
p.turf.lme<-cbind(new_data_site, predictions)

lsat.turf.lm<-lm(Sed_depth_mm~Turf_length_mm, data = lsat.comp)
summary(lsat.turf.lm)

# Create a new data frame for prediction
new_data <- data.frame(Turf_length_mm = seq(0, 10, by = 0.5))
# Make predictions
p.lsat.depth <- predict(lsat.turf.lm, new_data)

pred.lsat.depth<-cbind(new_data, p.lsat.depth)



########################### Sediment load vs sediment depth #########################
library(tidyverse)
lsat.comp$Sed_load <- c((lsat.comp$Sediment_w/25)*10000)

SedimentLoad<- ggplot(data = lsat.comp, aes(Sed_depth_mm, Sed_load)) + 
  stat_summary (geom= "point", fun.y = "mean", size=4, show.legend=F) +
  geom_smooth(method = "lm", se = T, color = "black", linewidth=1, fill="lightgray") +
  ylab(expression(Sediment~load~(g~m^{"-2"}))) + 
  xlab ("LSAT sediment depth (mm)") +
  scale_y_continuous(breaks = seq(0, 4000, by = 500))+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  theme (
    axis.text = element_text(color='black', size=14),
    axis.title = element_text(color='black', size=16),
    axis.ticks = element_line(color='black'),
    axis.ticks.length=unit(.25, "cm"),
    legend.title = element_text (size=18),
    legend.position =c(0.15,0.75),
    panel.background = element_rect (fill=NA, color='black'),
    panel.grid = element_blank()
  )
SedimentLoad

lsat.load.lme<-lme(Sed_load~Sed_depth_mm, random = ~ 1 | Site, data = lsat.comp)
summary(lsat.load.lme)

lsat.load.lm<-lm(Sed_load~Sed_depth_mm, data = lsat.comp)
summary(lsat.load.lm)

# Create a new data frame for prediction
new_data_load <- data.frame(Sed_depth_mm = seq(0, 6, by = 0.5))
# Make predictions
p.lsat.load <- predict(lsat.load.lm, new_data_load)

pred.lsat.load<-cbind(new_data_load, p.lsat.load)


#############################  Map with LSAT thickness ############################

setwd("/Users/victorruano/Library/CloudStorage/OneDrive-UniversityofMiami/CIMAS/Florida Sediments/R scripts")

rec.lsat<-read.csv("Recruits_LSAT.csv")

rec.lsat$Site<-factor(rec.lsat$Site, levels = c("Ft Lauderdale","Dania Beach","South Canyon", 
                                                "Emerald Reef", "Carysfort Reef", "Conch Reef"))

# Load your sediment data (replace with your actual data file)
Sediment_Data <- read.csv("Book3.csv")

# Extract unique latitude and longitude values from your sediment data
Latitude <- unique(Sediment_Data$Latitude)
Longitude <- unique(Sediment_Data$Longitude)

# Create a data frame with the extracted latitude and longitude
Locations <- data.frame("Latitude" = Latitude, "Longitude" = Longitude)

# Add LSAT index
rec.lsat <- rec.lsat %>%
  mutate(LSAT_index = LSAT_Sediment_depth_mm * LSAT_.)

rec.lsat$Season<-factor(rec.lsat$Season, levels = c("Summer", "Fall", "Winter"))

LSAT.S23<-subset(rec.lsat, rec.lsat$Season=="Summer")

seds.s23.summary.depth <- LSAT.S23 %>%
  group_by(Site) %>%
  summarise(
    ct = median(LSAT_Sediment_depth_mm),
    mad = mad(LSAT_Sediment_depth_mm)
  )

seds.s23.summary.depth$Site<-factor(seds.s23.summary.depth$Site, 
                                    levels = c("Ft Lauderdale","Dania Beach","South Canyon", 
                                               "Emerald Reef", "Carysfort Reef", "Conch Reef"))

Extnt <- data.frame("lat" = c(24.7, 26.5, 24.7, 26.5), 
                    "lon" = c(-81.0, -79.5, -79.5, -81.0))

map <- basemap(data = Extnt, bathymetry = FALSE, 
               land.col = "antiquewhite") +
  geom_point(data = seds.s23.summary.depth, 
             aes(x = Longitude, y = Latitude, size = ct))
# Add labels to the points on the map
map +
  geom_text(data = Sediment_Data, 
            aes(x = Longitude, y = Latitude, label = Site), 
            size = 5, 
            color = "black", 
            hjust = -0.1)+
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering)+
  annotation_scale()+
  annotate(geom="text", 
           x = -80.6, 
           y = 25.8, 
           label = "Florida", 
           fontface = "bold", 
           color = "black", 
           size = 8)+
  theme_classic()+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.position = "top")

ggsave("Map_LSAT_thicc.tiff", units="in", bg = "white", width=6, height=9, dpi=300)
ggsave("Map_LSAT_thicc.jpg", units="in", bg = "white", width=6, height=9, dpi=300)

###################### LSAT Sediment grain size ##########################

grain.lsat<-read.csv("Grain_size_LSAT.csv", encoding = "UTF-8")

grain.lsat$Site<-factor(grain.lsat$Site, levels = c("Ft Lauderdale","Dania Beach","South Canyon", 
                                                    "Emerald Reef", "Carysfort Reef", "Conch Reef"))

grain.lsat$Sed_size<-factor(grain.lsat$Sed_size, levels = c("<125 µm", "125-249 µm", "250-499 µm", "≥500 µm"))

legend_title <- "LSAT sediment grain size"

ggplot(grain.lsat, aes(x = Site, y = Prop, fill = Sed_size)) +
  scale_fill_manual(legend_title, values = c("firebrick", "goldenrod", "steelblue", "purple3"))+
  geom_col() +
  ylab("Relative abundance")+
  theme_classic()+
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size=12), 
        strip.text.x = element_text(size = 14),
        axis.ticks.length=unit(.25, "cm"),
        axis.text.y=element_text(angle = 90, hjust = 0.5, vjust = 1),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
