# Script for making nice-looking oscillogram and spectrogram figures
# for publications.

# ___________________________
#
# 0) Load the modified 'prep_static_ggspectro' function to create spectrograms of stereo files. ----
#
#___________________________
source("modif_spectro_function.R")

#___________________________
#
# 1) Load libraries ----
#___________________________
library(seewave) # for audio manupulation
library(tuneR) # for reading audio in R
library(dynaSpec) # for spectrograms using ggplot

library(viridis) # for color palettes.
library(poisonfrogs) # for frog color palettes.
library(ggplot2) # for general plotting
library(cowplot) # for plot_grid function
library(ggpubr) # for as_ggplot function
library(extrafont) # AnimBehav likes Times New Roman
loadfonts()
# fonts() #uncomment if you want to see available fonts


# Define font family for all figures (e.g., Arial or Times New Roman)
font.family <- 'Arial'
#font.family <- 'Times New Roman'



#___________________________
#
# 2) Load audiofile ----
#
#___________________________
Hyla_call <- readWave("Hyla_stereo.wav")
Hyla_call


#___________________________
#
# 3) Extract parameters from audiofile ----
# # These values will be used to plot the oscillogram later
#___________________________
sample.sequence <- seq(1:length(Hyla_call@left)) # sequence of samples
time.s <- sample.sequence/Hyla_call@samp.rate # transform samples to time in seconds.
samples.Left <- as.vector(cbind(Hyla_call@left)) # vector of amplitudes, unitless
samples.Right <- as.vector(cbind(Hyla_call@right)) # vector of amplitudes, unitless

df_Hyla <- data.frame(sample.sequence, time.s, samples.Left, samples.Right)
head(df_Hyla)

plot(df_Hyla$time.s, df_Hyla$samples.Left, type = "l")
plot(df_Hyla$time.s, df_Hyla$samples.Right, type = "l")


#___________________________
#
# 4) Oscillograms ----
#
#___________________________

#Oscillogram LEFT
ggoscilo_synth_L <- ggplot(df_Hyla)+
  geom_line(mapping = aes(x=time.s, y=samples.Left/1000), color="#3f9dcf")+
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
  labs(x = "Time (s)", y = "")+
  theme_bw()+
  theme(plot.margin = unit(c(0.1, 0.1, 0, 0.1), "lines"),  # top, right, bottom, left
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = NA),
        text = element_text(family = font.family),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggoscilo_synth_L


#Oscillogram RIGHT
ggoscilo_synth_R <- ggplot(df_Hyla)+
  geom_line(mapping = aes(x=time.s, y=samples.Right/1000), color="#f9bc3e")+
 # scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
  labs(x = "Time (s)", y = "")+
  theme_bw()+
  theme(plot.margin = unit(c(0, 0.1, 0.1, 0.1), "lines"),  # top, right, bottom, left
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        # axis.title.x=element_blank(),
        # axis.text.x=element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        text = element_text(family = font.family),
        axis.ticks.length = unit(-0.2, "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggoscilo_synth_R

# Combine L-R oscillograms:
oscillo_LR <- plot_grid(ggoscilo_synth_L, ggoscilo_synth_R, 
          align = "v", 
          axis    = "lr",
          labels = c("A", ""),
          #labels = c("Left", "Right"),
          label_size = 25,
          label_fontface = "plain",
          label_fontfamily = font.family,
          ncol = 1,
          rel_heights = c(1, 1.25))
oscillo_LR



#___________________________
#
# 5) Create spectrogram using 'dynaSpec'. ----
#
#___________________________
# Define some basic spectrogram parameters:
palette <- "turbo"      # color palette. Can try any viridis palette.
freq.lim <- c(0, 5)     # frequency limits of spectrogram, in kHz.
win.len <- 512          # window length, in samples.
ovlp <-  90             # overlap between windows, in percent.
min.dB <- -30           # minimum dB values. E.g., will go from 0 to -30 dB.
colbins <- abs(min.dB)  # color bins, one color bin per dB.

# Make preliminar spectrograms. These will be used below:
# Left channel:
params_L <- prep_static_ggspectro_ch("Hyla_stereo.wav" ,
                                     channel = "left", # left channel.
                                     colPal = palette,
                                     #colPal = c("#0a1e3f", "#f6e58d"),
                                     yLim = freq.lim, 
                                     wl = win.len,
                                     ovlp = ovlp,
                                     min_dB = min.dB,
                                     colbins = colbins,
                                     specHeight = 5,
                                     specWidth = 7,
                                     plotLegend = FALSE,
                                     savePNG=FALSE,
                                     onlyPlotSpec = FALSE)


# Right channel:
params_R <- prep_static_ggspectro_ch("Hyla_stereo.wav" ,
                                     channel = "right", # right channel.
                                     colPal = palette,
                                     yLim = freq.lim, 
                                     wl = win.len,
                                     ovlp = ovlp,
                                     min_dB = min.dB,
                                     colbins = colbins,
                                     specHeight = 5,
                                     specWidth = 7,
                                     plotLegend = FALSE,
                                     savePNG=FALSE,
                                     onlyPlotSpec = FALSE)


#___________________________
#
# 6) Spectrograms ----
#
#___________________________

#Spectrogram LEFT (top plot)
ggspectro_synth_L <- params_L$spec[[1]]+
  scale_x_continuous(expand = c(0,0), limits = c(0, max(time.s)))+
  scale_y_continuous(expand = c(0,0), limits = freq.lim,
                     breaks = seq(1,4,1), labels = seq(1,4,1))+
  labs(x = "", y = "Frequency (kHz)")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18, face="plain"),
        text = element_text(family = font.family),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color="white"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "lines"))  # top, right, bottom, left
ggspectro_synth_L

#Spectrogram RIGHT (bottom plot)
ggspectro_synth_R <- params_R$spec[[1]]+
  scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
  scale_y_continuous(expand = c(0,0), limits = freq.lim,
                     breaks = seq(1,4,1), labels = seq(1,4,1))+
  labs(x = "Time (s)", y = "Frequency (kHz)")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18, face="plain"),
        text = element_text(family = font.family),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color="white"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "lines"))  # top, right, bottom, left
ggspectro_synth_R

# Combine L-R spectrograms:
spectro_LR <- plot_grid(ggspectro_synth_L, ggspectro_synth_R,
                        ncol = 1, 
                        rel_heights = c(1, 1.25),
                        labels = c("A", ""),
                        label_size = 25,
                        label_fontface = "plain",
                        label_fontfamily = font.family,
                        axis    = "lr",
                        align = "v"
)
spectro_LR

#___________________________
#
# 7) Combined ----
#
#___________________________
plot_grid(oscillo_LR, spectro_LR,
          align = "hv", 
          axis    = "tb",
          ncol = 2,
          rel_heights = c(1, 1))
