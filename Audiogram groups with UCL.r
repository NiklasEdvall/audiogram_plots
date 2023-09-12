# Load libraries ====================================================
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(svglite)

# Load aud data as data frame "dat" =================================
dat <- read_csv("data/aud_data_UCL.csv")

# Specify "Group" as factor variable and label levels
dat$Group <- factor(dat$Group, levels = c(1,2,3), labels = c("No tinnitus", "Mild tinnitus", "Severe tinnitus"))

# Specify function for SEM (standard error of mean) =================
std_error <- function(x) sd(x)/sqrt(length(x))

# Specify columns in data to use for Pure Tone Thresholds (PTT) and Uncomfortable Loudness Levels (UCL) ======
ptt.labs <- c("L125", "L250", "L500", "L1000", "L2000", "L3000", "L4000", "L6000", "L8000",
              "R125", "R250", "R500", "R1000", "R2000", "R3000", "R4000", "R6000", "R8000")

ucl.labs <- c("UCLL500", "UCLL1000", "UCLL2000", "UCLL4000",
              "UCLR500", "UCLR1000", "UCLR2000", "UCLR4000")

# Create separate data frames for PTT and UCL ================================================================
ptt <- dat[,c("ID", "Group", ptt.labs)]
ucl <- dat[,c("ID", "Group", ucl.labs)]

# Make data to long format (need tidyverse package) ==========================================================
ptt.long <- gather(ptt, key = "ear-freq", value = "dB", -ID, -Group)
ucl.long <- gather(ucl, key = "ear-freq", value = "dB", -ID, -Group)

# Split and rename variables
ptt.long <- ptt.long %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (1)) %>% #sep = 1 is n characters before frequency in col name for ptt
  mutate(freq = (type.convert(freq, as.is = TRUE))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("R", "L"))) %>%
  mutate(ear = recode(ear, "R" = "Right", "L" = "Left"))

ucl.long <- ucl.long %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (4)) %>% #sep = 4 is n characters before frequency in col name ucl
  mutate(freq = (type.convert(freq, as.is = TRUE))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("UCLR", "UCLL"))) %>%
  mutate(ear = recode(ear, "UCLR" = "Right", "UCLL" = "Left"))

# Specify shared plot options to make it easy to modify all plots ============================================
titlesz = 16 #Title text size
axlabsz = 14 #Axis label text size
axtitlesz = 16 #Axis title size
linew = 0.5 #Linewith
pointsz = 1 #Point size
pointa = 0.75 #Point alpha (opacity)
ymax = 90 #Y-scale maximum
ymin = -10 #Y-scale minimum
pointscat = 0.3 #point scatter/jitter, useful if overlapping data

# Create plot of LEFT EAR audiogram (note subsest of data = ) only using data frame "ptt.long" ===============
ptt.plot.L <- ggplot(data = ptt.long[ptt.long$ear == "Left",], aes(y = dB, x = freqLabels, group = Group, color = Group, shape = Group))+
  
  # Aud layers
  stat_summary(fun = mean, #plot mean audiogram
               fun.min = function(x) mean(x) - std_error(x), # Negative error bar (can use sd() for std deviation)
               fun.max = function(x) mean(x) + std_error(x), # Positive error bar (can use sd() for std deviation)
               geom = "pointrange", size = pointsz, alpha = pointa, position=position_dodge(width=pointscat)) + # Point options
  stat_summary(fun = mean, geom = "line", linewidth = linew) + # Line options
  
  scale_y_reverse(limits = c(ymax,ymin), breaks = seq(ymin, ymax, by=10))+ #Y-scale options
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", linewidth=0.5)+ # Highlight line at 25dB
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+ # Axis labels
  ggtitle("Audiogram Left") + # Title of plot
  theme_bw()+ # Specify general theme of plot
  theme(plot.title = element_text(size = titlesz), axis.text = element_text(size = axlabsz), axis.title=element_text(size=axtitlesz)) # Adjust text sizes of plot

# Create plot of RIGHT EAR audiogram (note subsest of data = ) only using data frame "ptt.long" ==============
ptt.plot.R <- ggplot(data = ptt.long[ptt.long$ear == "Right",], aes(y = dB, x = freqLabels, group = Group, color = Group, shape = Group))+
  
  # Aud layers
  stat_summary(fun = mean, #plot mean audiogram
               fun.min = function(x) mean(x) - std_error(x), # Negative error bar (can use sd() for std deviation)
               fun.max = function(x) mean(x) + std_error(x), # Positive error bar (can use sd() for std deviation)
               geom = "pointrange", size = pointsz, alpha = pointa, position=position_dodge(width=pointscat)) + # Point options
  stat_summary(fun = mean, geom = "line", linewidth = linew) + # Line options
  
  scale_y_reverse(limits = c(ymax,ymin), breaks = seq(ymin, ymax, by=10))+ #Y-scale options
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", linewidth=0.5)+ # Highlight line at 25dB
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+ # Axis labels
  ggtitle("Audiogram Right") + # Title of plot
  theme_bw()+ # Specify general theme of plot
  theme(plot.title = element_text(size = titlesz), axis.text = element_text(size = axlabsz), axis.title=element_text(size=axtitlesz)) # Adjust text sizes of plot

# Create plot of LEFT EAR PTT and UCL combining data frames ptt.long and ucl.long =====================================
# Note subsest of data in both ptt and ucl layers
ptt.ucl.plot.L <- ggplot(data = ptt.long[ptt.long$ear == "Left",], aes(y = dB, x = freqLabels, group = Group, color = Group, shape = Group))+
  
  # Aud layers
  stat_summary(fun = mean, #plot mean audiogram
               fun.min = function(x) mean(x) - std_error(x), # Negative error bar (can use sd() for std deviation)
               fun.max = function(x) mean(x) + std_error(x), # Positive error bar (can use sd() for std deviation)
               geom = "pointrange", size = pointsz, alpha = pointa, position=position_dodge(width=pointscat)) + # Point options
  stat_summary(fun = mean, geom = "line", linewidth = linew) + # Line options
  
  #UCL layers
  stat_summary(data = ucl.long[ucl.long$ear == "Left",], fun = mean, geom = "line", size = linew) + #Plot mean line for UCL
  stat_summary(data = ucl.long[ucl.long$ear == "Left",], fun = mean,
               fun.min = function(x) mean(x) - std_error(x), # Negative error bar (can use sd() for std deviation)
               fun.max = function(x) mean(x) + std_error(x), # Positive error bar (can use sd() for std deviation)
               geom = "pointrange", size = pointsz, alpha = pointa, position=position_dodge(width=pointscat)) + # Point options
  
  scale_y_reverse(limits = c(ymax,ymin), breaks = seq(ymin, ymax, by=10))+ #Y-scale options
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", linewidth=0.5)+ # Highlight line at 25dB
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+ # Axis labels
  ggtitle("Audiogram with UCL - Left") + # Title of plot
  theme_bw()+ # Specify general theme of plot
  theme(plot.title = element_text(size = titlesz), axis.text = element_text(size = axlabsz), axis.title=element_text(size=axtitlesz)) # Adjust text sizes of plot

# Create plot of RIGHT EAR PTT and UCL combining data frames ptt.long and ucl.long =====================================
# Note subsest of data in both ptt and ucl layers
ptt.ucl.plot.R <- ggplot(data = ptt.long[ptt.long$ear == "Right",], aes(y = dB, x = freqLabels, group = Group, color = Group, shape = Group))+
  
  # Aud layers
  stat_summary(fun = mean, #plot mean audiogram
               fun.min = function(x) mean(x) - std_error(x), # Negative error bar (can use sd() for std deviation)
               fun.max = function(x) mean(x) + std_error(x), # Positive error bar (can use sd() for std deviation)
               geom = "pointrange", size = pointsz, alpha = pointa, position=position_dodge(width=pointscat)) + # Point options
  stat_summary(fun = mean, geom = "line", linewidth = linew) + # Line options
  
  #UCL layers
  stat_summary(data = ucl.long[ucl.long$ear == "Right",], fun = mean, geom = "line", size = linew) + #Plot mean line for UCL
  stat_summary(data = ucl.long[ucl.long$ear == "Right",], fun = mean,
               fun.min = function(x) mean(x) - std_error(x), # Negative error bar (can use sd() for std deviation)
               fun.max = function(x) mean(x) + std_error(x), # Positive error bar (can use sd() for std deviation)
               geom = "pointrange", size = pointsz, alpha = pointa, position=position_dodge(width=pointscat)) + # Point options
  
  scale_y_reverse(limits = c(ymax,ymin), breaks = seq(ymin, ymax, by=10))+ #Y-scale options
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", linewidth=0.5)+ # Highlight line at 25dB
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+ # Axis labels
  ggtitle("Audiogram with UCL - Right") + # Title of plot
  theme_bw()+ # Specify general theme of plot
  theme(plot.title = element_text(size = titlesz), axis.text = element_text(size = axlabsz), axis.title=element_text(size=axtitlesz)) # Adjust text sizes of plot

# Arrange plots with ggpubr package and save ==========================================================================

final.plot <- ggarrange(ptt.plot.L, ptt.plot.R,
                        ptt.ucl.plot.L, ptt.ucl.plot.R, ncol = 2, nrow = 2)

# Save as png
png(file="output/audiogram2.png",
    width=1040, height=900)
final.plot
dev.off()

# Save as svg
ggsave(file="output/audiogram2.svg", plot=final.plot, units = "cm", width=30, height=25)