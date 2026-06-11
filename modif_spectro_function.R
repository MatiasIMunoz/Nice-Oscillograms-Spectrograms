# Modified dynaSpec:::prep_static_ggspectro function to:
# a) Handle stereo audio files.

# Maybe modify to allow any color palette (not only viridis....)

# Load library
library(dynaSpec)



ggSpec_pal <- function(wav, soundFile, resampleRate = NULL, segLens, savePNG,
                       specWidth, specHeight, destFolder, title = NULL,
                       ovlp, wl, wn, yLim, xLim = NULL, colPal, colbins, ampTrans,
                       plotLegend, onlyPlotSpec, isViridis, bg, fontAndAxisCol,
                       min_dB, bgFlood, optim, timeAdj = NULL, ...) {
  
  nSegs <- if (missing(segLens)) 1L else length(segLens) - 1L
  
  # Auto-pick font color for contrast against bg (unchanged)
  if (is.null(fontAndAxisCol)) {
    autoFontCol <- TRUE
    bgRGB <- grDevices::col2rgb(bg)
    fontAndAxisCol <- if (bgRGB["red",1]*0.299 + bgRGB["green",1]*0.587 +
                          bgRGB["blue",1]*0.114 > 149) "#000000" else "#ffffff"
  } else {
    autoFontCol <- FALSE
  }
  
  if (!is.null(resampleRate)) {
    message("Resampling from ", wav@samp.rate, " to ", resampleRate, " Hz")
    wav <- tuneR::downsample(wav, samp.rate = resampleRate)
  }
  
  spectrogram <- seewave::spectro(wav, plot = FALSE, ovlp = ovlp, wl = wl, wn = wn, ...)
  df <- data.frame(
    time      = rep(spectrogram$time, each  = nrow(spectrogram$amp)),
    freq      = rep(spectrogram$freq, times = ncol(spectrogram$amp)),
    amplitude = as.vector(spectrogram$amp)
  )
  
  Glist <- list()
  for (i in seq_len(nSegs)) {
    df_i <- if (nSegs > 1) {
      if (i == 1 && nSegs > 1) cat("\nSpectrogram ggplots of segmented WAVs being generated\n")
      if (i == nSegs)
        subset(df, time >= segLens[i] & time <= segLens[i + 1])
      else
        subset(df, time >= segLens[i] & time <  segLens[i + 1])
    } else df
    
    level <- NULL  # suppress R CMD check NOTE
    
    Glist[[i]] <-
      ggplot2::ggplot(df_i, ggplot2::aes(x = time, y = freq, z = amplitude)) +
      ggplot2::xlim(segLens[i], segLens[i + 1]) +
      ggplot2::ylim(yLim) +
      ggplot2::labs(x = "Time (s)", y = "Frequency (kHz)",
                    fill = "Amplitude\n(dB)\n", title = title) +
      {
        if (isViridis) {
          # Original viridis path — unchanged
          viridis::scale_fill_viridis(
            limits   = c(min_dB, 0),
            na.value = "transparent",
            option   = colPal,
            trans    = scales::modulus_trans(p = ampTrans)
          )
        } else {
          # *** THE FIX ***
          # scale_fill_gradient()  → only uses colPal[1] and colPal[2]
          # scale_fill_gradientn() → uses ALL colors in colPal
          ggplot2::scale_fill_gradientn(
            limits   = c(min_dB, 0),
            na.value = "transparent",
            colours  = colPal,
            trans    = scales::modulus_trans(p = ampTrans)
          )
        }
      } +
      ggplot2::stat_contour(
        geom = "polygon",
        ggplot2::aes(fill = ggplot2::after_stat(level)),
        bins  = colbins,
        na.rm = TRUE
      ) +
      dynaSpec:::mytheme(bg) +
      {
        if (!autoFontCol)
          ggplot2::theme(
            axis.text  = ggplot2::element_text(colour = fontAndAxisCol),
            text       = ggplot2::element_text(colour = fontAndAxisCol),
            axis.line  = ggplot2::element_line(colour = fontAndAxisCol),
            axis.ticks = ggplot2::element_line(colour = fontAndAxisCol)
          )
      } +
      {
        if (onlyPlotSpec)
          ggplot2::theme_void() +
          ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg),
                         text = ggplot2::element_text(colour = fontAndAxisCol))
        else if (bgFlood)
          ggplot2::theme(
            plot.background   = ggplot2::element_rect(fill = bg),
            axis.text         = ggplot2::element_text(colour = fontAndAxisCol),
            text              = ggplot2::element_text(colour = fontAndAxisCol),
            axis.line         = ggplot2::element_line(colour = fontAndAxisCol),
            axis.ticks        = ggplot2::element_line(colour = fontAndAxisCol),
            legend.background = ggplot2::element_rect(fill = bg)
          )
      } +
      {
        if (!plotLegend) ggplot2::theme(legend.position = "none")
        else             ggplot2::theme(legend.position = "right")
      }
    
    if (savePNG) {
      if (i == 1) {
        baseNom <- basename(tools::file_path_sans_ext(soundFile))
        subDest <- fs::path(destFolder, paste0(baseNom, "_static_specs"))
        dir.create(subDest, showWarnings = FALSE)
      }
      fn_i <- fs::path(subDest, paste0(baseNom, "_", i), ext = "png")
      ggplot2::ggsave(fn_i, width = specWidth, height = specHeight, units = "in")
      cat(paste0("\nStatic spec saved @", fn_i))
    }
  }
  
  rm(spectrogram)
  list(specList = Glist, fontAndAxisCol = fontAndAxisCol, autoFontCol = autoFontCol)
}





# ── Updated prep_static_ggspectro_pal: calls ggSpec_pal instead of ggSpec ─────
prep_static_ggspectro_pal <- function(soundFile, destFolder, outFilename, savePNG = FALSE,
                                      colPal = "inferno", crop = NULL, bg = NULL, filter = NULL,
                                      xLim = NULL, yLim = c(0, 10), plotLegend = FALSE,
                                      onlyPlotSpec = TRUE, ampTrans = 1, min_dB = -30,
                                      wl = 512, ovlp = 90, wn = "blackman", specWidth = 9,
                                      specHeight = 3, colbins = 30, ampThresh = 0,
                                      bgFlood = FALSE, fontAndAxisCol = NULL, optim = NULL,
                                      channel = "left",   # <-- (1) new argument
                                      ...) {
  
  # path / file handling (unchanged)
  if (missing(destFolder)) {
    destFolder <- if (dynaSpec:::is.url(soundFile)) getwd()
    else dirname(tools::file_path_as_absolute(soundFile))
  }
  if (destFolder == "wd") destFolder <- getwd()
  if (!grepl("/$", destFolder)) destFolder <- paste0(destFolder, "/")
  if (dynaSpec:::is.url(soundFile)) {
    utils::download.file(soundFile, paste0(destFolder, basename(soundFile)))
    soundFile <- paste0(destFolder, basename(soundFile))
  }
  if (missing(outFilename))
    outFilename <- paste0(tools::file_path_sans_ext(basename(soundFile)), ".PNG")
  if (!grepl(".png|PNG", outFilename))
    outFilename <- paste0(outFilename, ".png")
  
  # palette resolution (unchanged)
  viridis_pals <- c("magma","inferno","plasma","viridis","cividis","rocket","mako","turbo")
  
  if (is.function(colPal)) {
    isViridis <- FALSE
    colPal    <- colPal(colbins)
  } else if (length(colPal) == 1 && colPal %in% viridis_pals) {
    isViridis <- TRUE
  } else if (length(colPal) == 1) {
    stop(paste0("'", colPal, "' is not a viridis palette name. ",
                "Provide a palette function or a character vector of hex colors."))
  } else {
    isViridis <- FALSE
  }
  
  if (is.null(bg))
    bg <- if (isViridis) eval(parse(text = paste0("viridis::", colPal)))(1) else colPal[1]
  
  # audio loading
  if (tools::file_ext(soundFile) == "mp3") {
    print("***Converting mp3 to wav***")
    wav0 <- tuneR::readMP3(soundFile)
  } else {
    wav0 <- tuneR::readWave(soundFile)
  }
  
  # <-- (2) channel selection: validate then extract
  if (wav0@stereo) {
    channel <- match.arg(channel, c("left", "right"))
    wav0    <- tuneR::mono(wav0, which = channel)
  }
  
  prepped <- dynaSpec:::processSound(wav0, crop = crop, xLim = xLim,
                                     filter = filter, ampThresh)
  if (length(yLim) == 1) yLim <- c(0, yLim)
  
  specOutList <- ggSpec_pal(
    wav = prepped$newWav, soundFile = soundFile,
    segLens = prepped$segLens, savePNG = savePNG, specWidth = specWidth,
    specHeight = specHeight, destFolder = destFolder, colPal = colPal,
    isViridis = isViridis, bg = bg, xLim = prepped$xLim, yLim = yLim,
    plotLegend = plotLegend, onlyPlotSpec = onlyPlotSpec, ampTrans = ampTrans,
    min_dB = min_dB, wl = wl, ovlp = ovlp, wn = wn, colbins = colbins,
    bgFlood = bgFlood, fontAndAxisCol = fontAndAxisCol, optim = optim, ...)
  
  plot(specOutList$specList[[1]])
  if (length(prepped$segWavs) > 1) cat("\nFor segmented spectrogram, only segment 1 shown\n")
  
  list(soundFile = soundFile, destFolder = destFolder, outFilename = outFilename,
       crop = crop, colPal = colPal, isViridis = isViridis, xLim = prepped$xLim,
       yLim = yLim, plotLegend = plotLegend, onlyPlotSpec = onlyPlotSpec,
       ampTrans = ampTrans, ampThresh = ampThresh, min_dB = min_dB, bg = bg,
       wl = wl, ovlp = ovlp, wn = wn, specWidth = specWidth, specHeight = specHeight,
       colbins = colbins, bgFlood = bgFlood, autoFontCol = specOutList$autoFontCol,
       fontAndAxisCol = specOutList$fontAndAxisCol, spec = specOutList$specList,
       newWav = prepped$newWav, segWavs = prepped$segWavs,
       channel = channel)   # <-- (3) stored so paged_spectro or your code can inspect it
}