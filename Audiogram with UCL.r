# Load libraries ====================================================
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Load aud data as data frame "dat" =================================
dat <- read_csv("data/aud_data_UCL.csv")

# Specify function for SEM (standard error of mean) =================
std_error <- function(x) sd(x)/sqrt(length(x))

# Specify columns in data to use for Pure Tone Thresholds (PTT) and Uncomfortable Loudness Levels (UCL) ======
ptt.labs <- c("L125", "L250", "L500", "L1000", "L2000", "L3000", "L4000", "L6000", "L8000",
              "R125", "R250", "R500", "R1000", "R2000", "R3000", "R4000", "R6000", "R8000")

ucl.labs <- c("UCLL500", "UCLL1000", "UCLL2000", "UCLL4000",
              "UCLR500", "UCLR1000", "UCLR2000", "UCLR4000")

# Create separate data frames for PTT and UCL ================================================================
# Note - dropping variable "Group"
ptt <- dat[,c("ID", ptt.labs)]
ucl <- dat[,c("ID", ucl.labs)]

# Make data to long format (need tidyverse package) ==========================================================
ptt.long <- gather(ptt, key = "ear-freq", value = "dB", -ID)
ucl.long <- gather(ucl, key = "ear-freq", value = "dB", -ID)

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
axlabsz = 12 #Axis label text size
linew = 0.5 #Linewith
pointsz = 1 #Point size
pointa = 0.75 #Point alpha (opacity)
ymax = 90 #Y-scale maximum
ymin = -10 #Y-scale minimum
pointscat = 0.3 #point scatter/jitter, useful if overlapping data

# Create plot of audiogram only using data frame "ptt.long" ==================================================
ptt.plot <- ggplot(data = ptt.long, aes(y = dB, x = freqLabels, group = ear, color = ear, shape = ear))+
  
  # Aud layers
  stat_summary(fun = mean, #plot mean audiogram
               fun.min = function(x) mean(x) - std_error(x), # Negative error bar (can use sd() for std deviation)
               fun.max = function(x) mean(x) + std_error(x), # Positive error bar (can use sd() for std deviation)
               geom = "pointrange", size = 1, alpha = pointa, position=position_dodge(width=pointscat)) + # Point options
  stat_summary(fun = mean, geom = "line", linewidth = linew) + # Line options
  
  scale_y_reverse(limits = c(ymax,ymin), breaks = seq(ymin, ymax, by=10))+ #Y-scale options
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", linewidth=0.5)+ # Highlight line at 25dB
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+ # Axis labels
  ggtitle("Audiogram") + # Title of plot
  theme_bw()+ # Specify general theme of plot
  theme(plot.title = element_text(size = titlesz), axis.text = element_text(size = axlabsz)) # Adjust text sizes of plot

# Create plot of PTT and UCL combining data frames ptt.long and ucl.long =====================================
ptt.ucl.plot <- ggplot(data = ptt.long, aes(y = dB, x = freqLabels, group = ear, color = ear, shape = ear))+
  
  # Aud layers
  stat_summary(fun = mean, #plot mean PTTs
               fun.min = function(x) mean(x) - std_error(x), # Negative error bar (can use sd() for std deviation)
               fun.max = function(x) mean(x) + std_error(x), # Positive error bar (can use sd() for std deviation)
               geom = "pointrange", size = pointsz, alpha = pointa, position=position_dodge(width=pointscat)) + # Point options
  stat_summary(fun = mean, geom = "line") + # Line options
  
  #UCL layers
  stat_summary(data = ucl.long, fun = mean, geom = "line", size = linew) + #Plot mean line for UCL
  stat_summary(data = ucl.long, fun = mean,
               fun.min = function(x) mean(x) - std_error(x), # Negative error bar (can use sd() for std deviation)
               fun.max = function(x) mean(x) + std_error(x), # Positive error bar (can use sd() for std deviation)
               geom = "pointrange", size = pointsz, alpha = pointa, position=position_dodge(width=pointscat)) + # Point options
  
  scale_y_reverse(limits = c(ymax, ymin), breaks = seq(ymin, ymax, by=10))+ #Y-scale options
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", linewidth=0.5)+ # Highlight line at 25dB
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+ # Axis labels
  ggtitle("Audiogram") + # Title of plot
  theme_bw()+ # Specify general theme of plot
  theme(plot.title = element_text(size = titlesz), axis.text = element_text(size = axlabsz)) # Adjust text sizes of plot

# Arrange plots with ggpubr package ==========================================================================
