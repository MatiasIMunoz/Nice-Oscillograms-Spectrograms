# Script for making nice-looking oscillogram and spectrogram figures
# for publications.

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
ylim <- max(abs(c(samples.Left, samples.Right)))/1000


#Oscillogram LEFT
ggoscilo_synth_L <- ggplot(df_Hyla)+
  geom_line(mapping = aes(x=time.s, y=samples.Left/1000), color="#3f9dcf", linewidth = 0.4)+
  #scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
  scale_y_continuous(limits = c(-ylim, ylim))+
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
#ggoscilo_synth_L


#Oscillogram RIGHT
ggoscilo_synth_R <- ggplot(df_Hyla)+
  geom_line(mapping = aes(x=time.s, y=samples.Right/1000), color="#f9bc3e", linewidth = 0.4)+
 # scale_x_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),  limits = c(0, max(time.s)))+
  scale_y_continuous(limits = c(-ylim, ylim))+
  labs(x = "Time (s)", y = "")+
  theme_bw()+
  annotate("segment", x = 0.05, xend = 0.15, y = -0.8*ylim, yend = -0.8*ylim, linewidth = 1.5) + # Add axis
  annotate("text", x = 0.10, y = -0.95*ylim, label = "100 ms", size = 5) + # Add axis

  theme(plot.margin = unit(c(0, 0.1, 0.1, 0.1), "lines"),  # top, right, bottom, left
        axis.title=element_text(size=18),
        axis.text=element_text(size=14),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
        panel.border = element_blank(),
       # axis.line.x = element_line(colour = "black"),
        text = element_text(family = font.family),
        axis.ticks.length = unit(0, "cm"), # replaced -0.2 for 0 for new time axis.
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#ggoscilo_synth_R


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
          rel_heights = c(1, 1))
oscillo_LR

# Export figure
#ggsave("Hyla_stereo_oscillo.pdf", plot = oscillo_LR, device = cairo_pdf, width = 9, height = 4)
#ggsave("Hyla_stereo_oscillo.png", plot = oscillo_LR, width = 9, height = 4, units = "in", dpi = 600)

#___________________________
#
# 5) Create spectrogram using 'dynaSpec'. ----
#
#___________________________
# Define some basic spectrogram parameters:
palette <- "turbo"      # color palette. Can try any viridis palette.
palette <-  scico(100, palette = "batlow")

freq.lim <- c(0, 5)     # frequency limits of spectrogram, in kHz.
win.len <- 512          # window length, in samples.
ovlp <-  90             # overlap between windows, in percent.
min.dB <- -30           # minimum dB values. E.g., will go from 0 to -30 dB.
colbins <- abs(min.dB)  # color bins, one color bin per dB.

# Make preliminar spectrograms. These will be used below:
# Left channel:
params_L <- prep_static_ggspectro_pal("Hyla_stereo.wav" ,
                                     channel = "left", # left channel.
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


# Right channel:
params_R <- prep_static_ggspectro_pal("Hyla_stereo.wav" ,
                                     channel = "right", # right channel.
                                     #colPal = palette,
                                     #colPal = poison_palette("Ramazonica", return = "vector"),
                                     #colPal = wes_palette("Zissou1", 100, type = "continuous"),
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


#___________________________
#
# 6) Spectrograms ----
#
#___________________________

#Spectrogram LEFT (top plot)
ggspectro_synth_L <- params_L$spec[[1]]+
  scale_x_continuous(expand = c(0,0), limits = c(0, max(time.s)))+
  #scale_y_continuous(expand = c(0,0), limits = freq.lim, breaks = seq(1,4,1), labels = seq(1,4,1))+
  scale_y_log10(expand = c(0,0), breaks = seq(1,4,1), labels = seq(1,4,1), limits = c(0.75, 5))+
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
  #scale_y_continuous(expand = c(0,0), limits = freq.lim,breaks = seq(1,4,1), labels = seq(1,4,1))+
  scale_y_log10(expand = c(0,0), breaks = seq(1,4,1), labels = seq(1,4,1), limits = c(0.75, 5))+
  labs(x = "Time (s)", y = "Frequency (kHz)")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18, face="plain"),
        text = element_text(family = font.family),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.ticks = element_line(color="white"),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "lines"))  # top, right, bottom, left
#ggspectro_synth_R

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

# Export figure
#ggsave("Hyla_stereo_spectro.pdf", plot = spectro_LR, device = cairo_pdf, width = 9, height = 6)
#ggsave("Hyla_stereo_spectro.png", plot = spectro_LR, width = 9, height = 6, units = "in", dpi = 600)

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


# END OF SCRIPT ----

 
# > sessionInfo()
# R version 4.3.1 (2023-06-16)
# Platform: x86_64-apple-darwin20 (64-bit)
# Running under: macOS 15.4.1
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: America/Chicago
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] Cairo_1.6-2       extrafont_0.19    ggpubr_0.6.0      cowplot_1.1.1     ggplot2_3.5.2     poisonfrogs_1.0.2 viridis_0.6.3    
# [8] viridisLite_0.4.2 tuneR_1.4.7       seewave_2.2.3     dynaSpec_1.0.1   
# 
# loaded via a namespace (and not attached):
#   [1] generics_0.1.4     tidyr_1.3.0        rstatix_0.7.2      extrafontdb_1.0    digest_0.6.37      magrittr_2.0.4     evaluate_1.0.3    
# [8] grid_4.3.1         RColorBrewer_1.1-3 pkgload_1.4.0      fastmap_1.2.0      backports_1.5.0    gridExtra_2.3      purrr_1.0.2       
# [15] scales_1.4.0       textshaping_0.3.6  isoband_0.2.7      abind_1.4-5        cli_3.6.5          rlang_1.1.6        crayon_1.5.3      
# [22] yaml_2.3.10        withr_3.0.2        tools_4.3.1        ggsignif_0.6.4     dplyr_1.1.4        broom_1.0.5        vctrs_0.6.5       
# [29] R6_2.5.1           lifecycle_1.0.4    car_3.1-2          MASS_7.3-60        ragg_1.2.5         pkgconfig_2.0.3    pillar_1.10.1     
# [36] gtable_0.3.6       rsconnect_1.0.1    glue_1.8.0         systemfonts_1.2.3  xfun_0.50          tibble_3.2.1       tidyselect_1.2.0  
# [43] knitr_1.49         rstudioapi_0.15.0  dichromat_2.0-0.1  farver_2.1.2       htmltools_0.5.8.1  rmarkdown_2.29     carData_3.0-5     
# [50] labeling_0.4.3     Rttf2pt1_1.3.12    signal_1.8-1       compiler_4.3.1    