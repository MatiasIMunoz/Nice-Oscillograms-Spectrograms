# Script for ABS 2026 Cincinnati - spectral bandwidth of crow.



# ___________________________
#
# 0) Load the modified 'prep_static_ggspectro' function to create spectrograms of stereo files. ----
# This .R script should be stored in the working directory for this to work.
#___________________________
source("modif_spectro_function.R")


#___________________________
#
# 1) Load libraries ----
#___________________________
library(seewave) # for audio manupulation
library(tuneR) # for reading audio in R
library(dynaSpec) # for spectrograms using ggplot

library(scico) # for color palettes. (batlow).
library(wesanderson) # for color palette (Zissou1).
library(viridisLite) # for color palettes (turbo).
library(poisonfrogs) # for frog color palettes (Ramazonica).
library(pals) # for color palettes (parula).

library(Cairo) # for better pdf export.
library(ggplot2) # for general plotting
library(cowplot) # for plot_grid function
library(ggpubr) # for as_ggplot function
library(extrafont) # AnimBehav likes Times New Roman
library(scales) # for alpha() function
loadfonts()
#fonts() #uncomment if you want to see available fonts


# Define font family for all figures (e.g., Arial or Times New Roman)
font.family <- 'Arial'
#font.family <- 'Times New Roman'

# Some color palettes
BuYe <- rev(c("#f6e58d", "#f9d56e", "#f9bc3e", "#66bfbf", "#3f9dcf", "#2d6a9b", "#1c3f5a", "#0a1e3f"))


#___________________________
#
# 2) Load audiofile ----
#
#___________________________
Crow_call <- readWave("wavs/XC803389-Hooded_crow_Corvus cornix_CUT.wav")
Crow_call



#___________________________
#
# 3) Extract parameters from audiofile ----
# # These values will be used to plot the oscillogram later
#___________________________
sample.sequence <- seq(1:length(Crow_call@left)) # sequence of samples
time.s <- sample.sequence/Crow_call@samp.rate # transform samples to time in seconds.
samples.Left <- as.vector(cbind(Crow_call@left)) # vector of amplitudes, unitless
samples.Right <- as.vector(cbind(Crow_call@right)) # vector of amplitudes, unitless

df_Crow <- data.frame(sample.sequence, time.s, samples.Left, samples.Right)
head(df_Crow)

plot(df_Crow$time.s, df_Crow$samples.Left, type = "l")
plot(df_Crow$time.s, df_Crow$samples.Right, type = "l")



#___________________________
#
# 4) Oscillograms ----
#
#___________________________
ylim <- max(abs(c(samples.Left, samples.Right)))/1000


#Oscillogram LEFT
oscillo <- ggplot(df_Crow)+
  geom_line(mapping = aes(x=time.s, y=samples.Left/1000), color="#3f9dcf", linewidth = 0.4)+
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
  scale_y_continuous(limits = c(-ylim, ylim))+
  labs(x = "Time (s)", y = "")+
  #theme_bw()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"),  # top, right, bottom, left
        axis.title=element_blank(),
        panel.background = element_blank(),
        axis.text=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = NA),
        text = element_text(family = font.family),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
oscillo



#___________________________
#
# 5) Create spectrogram using 'dynaSpec'. ----
#
#___________________________
# Define some basic spectrogram parameters:
#palette <- "turbo"      # color palette. Can try any viridis palette.
#palette <-  scico(100, palette = "batlow")

freq.lim <- c(0, 9)     # frequency limits of spectrogram, in kHz.
win.len <- 512        # window length, in samples.
ovlp <-  90             # overlap between windows, in percent.
min.dB <- -50          # minimum dB values. E.g., will go from 0 to -30 dB.
colbins <- abs(min.dB)  # color bins, one color bin per dB.


# Freq. parameters
# Compute bandwidth at -12 dB
Q_result <- Q(spectrum_df, level = -12)

peak.freq <- Q_result$dfreq
f.min <- Q_result$fmin
f.max <- Q_result$fmax


# Make preliminar spectrograms. These will be used below:
# Left channel:
params_L <- prep_static_ggspectro_pal("wavs/XC803389-Hooded_crow_Corvus cornix_CUT.wav" ,
                                      channel = "right", # left channel.
                                      #colPal = BuYe,
                                      #colPal = c("#0a1e3f", "#f6e58d")
                                      #colPal = wes_palette("Zissou1", 100, type = "continuous"),
                                      #colPal = scico(100, palette = "batlowK"),
                                      colPal = parula(256),
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

#Spectrogram LEFT (bottom plot)
spectrogram<- params_L$spec[[1]]+
  scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
  scale_y_continuous(expand = c(0,0), limits = freq.lim, breaks = seq(2,9,2), labels = seq(2,9,2))+
  labs(x = "Time (s)", y = "Frequency (kHz)")+
  
   annotate("rect", ymin = f.min, ymax = f.max, xmin = -Inf, xmax = Inf, fill = "red", alpha = 0.2, color = NA)+
   annotate("rect", ymin = 2.75, ymax = 2.85, xmin = -Inf, xmax = Inf, fill = "red", alpha = 0.2, color = NA)+

  #geom_hline(yintercept = c(f.min, f.max), col = "darkred", linewidth = 1.5, linetype = "dashed")+
  #geom_hline(yintercept = peak.freq, col = "red", linewidth = 1.5, linetype = "dashed")+
  
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18, face="plain"),
        text = element_text(family = font.family),
         axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color="white"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "lines"))  # top, right, bottom, left
spectrogram



#___________________________
#
# 5) Create power spectrum using 'seewave'. ----
#
#___________________________
spectrum_df <- meanspec(Crow_call, channel = 1, wl = win.len, ovlp = ovlp, dB = "max0")


spectrum <- 
ggplot(spectrum_df, aes(x = x, y=y))+
  
  annotate("rect", xmin = 2.75, xmax = 2.85, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2, color = NA)+
  annotate("rect", xmin = f.min, xmax = f.max, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2, color = NA)+
  #geom_vline(xintercept = c(f.min, f.max), col = "darkred", linewidth = 1.5, linetype = "dashed")+
  
  geom_hline(yintercept = -12, col = "grey70", linetype = "dashed", linewidth = 0.8)+
  
  geom_line(color="black", linewidth = 1.5)+
  
  #annotate("point", x = peak.freq, y = 0, colour = "red", size = 4)+
  #geom_segment(aes(x = peak.freq, xend = peak.freq, y = 0, yend = -Inf), color = "red", linewidth = 1.5, linetype = "dashed")+
  
  scale_x_continuous(expand = c(0,0), limits = freq.lim, breaks = seq(2,9,2), labels = seq(2,9,2))+
  scale_y_continuous(limits = c(-40, 0), breaks = c(-40, -20 , -12, 0), labels = c(-40, -20 , -12, 0))+
  coord_flip()+
#  theme_bw()+
  xlab("") + ylab("Amplitude (dB)")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18, face="plain"),
        text = element_text(family = font.family),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color="black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "lines"))  # top, right, bottom, left
spectrum


#___________________________
#
# 6) Plot audiogram. ----
#
#___________________________
# crow audiogram
Freqs <- c(0.25, 0.35, 0.5, 0.7, 1, 1.4, 2, 2.5, 2.8, 4, 5.6, 8)
Thrs <- c(22.333333, 14.000000, 8.166667, 3.000000, 0.666667, 2.000000, 3.166667, 3.000000, 1.333333, 7.166667, 12.166667, 61.166667)
Thrs_scaled <- Thrs- min(Thrs) 
Audiogram_df <- cbind.data.frame(Freqs, Thrs, Thrs_scaled)

coef <- max(Thrs)


#Audiogram
audiogram <- ggplot(Audiogram_df, aes(x = Freqs))+
 
   #threshold
  geom_hline(yintercept = 12, col = "grey70", linetype = "dashed", linewidth = 0.8)+

 # annotate("rect",xmin = c(0.3842844), xmax = c(5.624491),  ymin = -2.5, ymax = -1,  fill = "darkorange", alpha= 1)+

   # annotate("point", x = 1, y = -1-0.75, shape = 21, fill = "white", col = "darkorange3", size = 4)+
  # #Vocal
  # annotate("rect",
  #          xmin = c(f.min), xmax = c(f.max), ymin = -4, ymax = -2.5,  fill = "#3f9dcf", alpha= 1)+
  # annotate("point", x = peak.freq, y = -2.5-0.75, shape = 21, fill = "white", col = "3f9dcf", size = 4)+
  # 
  
  # Vocal bandwidth
  #annotate("rect", xmin = 2.75, xmax = 2.85, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2, color = NA)+
  #annotate("rect", xmin = f.min, xmax = f.max, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2, color = NA)+
  
  # Auditory bandwidth
  annotate("rect",xmin = c(0.3842844), xmax = c(5.624491),  ymin = -Inf, ymax = Inf,  fill = "darkorange", alpha= 0.2)+
  
  #Audiogram data
  geom_line(data = Audiogram_df, aes(x = Freqs, y = Thrs_scaled), col = "darkorange", linewidth = 1.5)+
  geom_point(data = Audiogram_df, aes(x = Freqs, y = Thrs_scaled),col = "darkorange3", fill = "darkorange", size = 2.5, shape = 21)+
  
  xlab("Frequency (kHz)")+ylab("Threshold (dB)")+
  scale_x_continuous(expand = c(0,0), limits = freq.lim, breaks = seq(2,9,2), labels = seq(2,9,2))+
  scale_y_continuous(limits = c(-2.5, 61), breaks = c(0, 12, 30, 60), labels = c(0, 12, 30, 60))+
  
  coord_flip()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18, face="plain"),
        text = element_text(family = font.family),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color="black"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "lines"))  # top, right, bottom, left
audiogram

#___________________________
#
# 7) Combine plots. ----
#
#___________________________
placeholder <- 
  ggplot() +
  theme_void()+
  theme(axis.title=element_blank(), 
        axis.text=element_blank(),
        aspect.ratio = NULL,
        panel.background = element_blank())


# Combine spectrograms and power spectrum:
combined_plot <- plot_grid(spectrogram, spectrum, audiogram,
                           ncol = 3, 
                           rel_widths = c(1, 0.5, 0.5),
                           #labels = c("A", ""),
                           #label_size = 25,
                           #label_fontface = "plain",
                           #label_fontfamily = font.family,
                           axis    = "tb",
                           align = "h"
)
combined_plot


ggsave("figures/Crow_call_ABS26_fig4.png", plot = combined_plot, width = 7, height = 5, units = "in", dpi = 600)






full_plot <- plot_grid(
  oscillo,     placeholder,
  spectrogram, spectrum,
  ncol = 2, nrow = 2,
  rel_widths  = c(1, 0.5),   # match combined_plot's rel_widths
  rel_heights = c(0.3, 1),   # tweak so oscillogram isn't too tall
  align = "hv",              # align both horizontally and vertically
  axis  = "tblr"             # align top/bottom/left/right axes across the whole grid
)
full_plot




