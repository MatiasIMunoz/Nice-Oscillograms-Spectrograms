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
library(ggplot2) # for general plotting
library(dynaSpec) # for spectrograms using ggplot
library(cowplot) # for plot_grid function
library(ggpubr) # for as_ggplot function
library(extrafont) # AnimBehav likes Times New Roman
loadfonts()
# fonts() #uncomment if you want to see available fonts

#___________________________
#
# 2) Load audiofile ----
Hyla_call <- readWave("Hyla_stereo.wav")
Hyla_call
#
#___________________________

#___________________________
#
# 3) Extract parameters from audiofile ----
#
#___________________________
sample.sequence <- seq(1:length(Hyla_call@left)) # sequence of samples
time.s <- sample.sequence/Hyla_call@samp.rate # transform samples to time in seconds.
samples.Left <- as.vector(cbind(Hyla_call@left)) # vector of amplitudes, unitless
samples.Right <- as.vector(cbind(Hyla_call@right)) # vector of amplitudes, unitless

df_Hyla <- data.frame(sample.sequence, time.s, samples.Left, samples.Right)
head(df_Hyla)

#___________________________
#
# 4) Create spectrogram using 'dynaSpec'.
#
#___________________________

palette <- "turbo"  # color palette try any viridis palette.
freq.lim <- c(0, 5) # frequency limits of spectrogram, in kHz.
win.len <- 512 # window length, in samples.
ovlp <-  90 # overlap between windows, in percent.
min.dB <- -30 # minimum dB values.
colbins <- abs(min.dB) # color bins

# Left channel:
params_L <- prep_static_ggspectro_ch("Hyla_stereo.wav" ,
                               channel = "left",
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

# Right channel:
params_R <- prep_static_ggspectro_ch("Hyla_stereo.wav" ,
                                     channel = "right",
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

#plot(df_Hyla$time.s, df_Hyla$samples.Left, type = "l")

#___________________________
#
# 4) Spectrograms ----
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
        text = element_text(family = 'Times New Roman'),
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
        text = element_text(family = 'Times New Roman'),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color="white"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "lines"))  # top, right, bottom, left
ggspectro_synth_R


spectrox2 <- plot_grid(ggspectro_synth_L, ggspectro_synth_R,
          ncol = 1, 
          rel_heights = c(1, 1.25),
          axis    = "lr",
          align = "v"
          )
spectrox2

#___________________________
#
# 5) Oscillograms ----
#
#___________________________

#Oscillogram LEFT
ggoscilo_synth_L <- ggplot(df_Hyla)+
  geom_line(mapping = aes(x=time.s, y=samples.Left/1000), color="black")+
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
  labs(x = "Time (s)", y = "")+
  theme_bw()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"),  # top, right, bottom, left
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        text = element_text(family = 'Times New Roman'),
        axis.ticks.length = unit(-0.2, "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggoscilo_synth_L


#Oscillogram RIGHT
ggoscilo_synth_R <- ggplot(df_Hyla)+
  geom_line(mapping = aes(x=time.s, y=samples.Right/1000), color="black")+
 # scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
  labs(x = "Time (s)", y = "")+
  theme_bw()+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"),  # top, right, bottom, left
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        text = element_text(family = 'Times New Roman'),
        axis.ticks.length = unit(-0.2, "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggoscilo_synth_R

# 
# #Oscillogram LEFT
# ggoscilo_synth_L <- ggplot(df_Hyla)+
#   geom_line(mapping = aes(x=time.s, y=samples.Left/1000), color="black")+
#  # scale_x_continuous(expand = c(0,0))+
#   scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
#   labs(x = "Time (s)", y = "")+
#   theme_bw()+
#   theme(plot.margin = unit(c(0,0,1,0), "lines"), # top, right, bottom, left
#         axis.title=element_text(size=18),
#         axis.text=element_text(size=14),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         text = element_text(family = 'Times New Roman'),
#         axis.ticks.length = unit(-0.2, "cm"),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# ggoscilo_synth_L
# 

oscillox2 <- plot_grid(ggoscilo_synth_L, ggoscilo_synth_R, 
          align = "v", 
          axis    = "lr",
          ncol = 1,
          rel_heights = c(1, 1.25))
oscillox2


#___________________________
#
# 6) Combined ----
#
#___________________________
plot_grid(oscillox2, spectrox2,
          align = "hv", 
          axis    = "lr",
          ncol = 2,
          rel_heights = c(1, 1))
