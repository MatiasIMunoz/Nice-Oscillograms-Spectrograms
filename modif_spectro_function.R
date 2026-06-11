# Modified dynaSpec:::prep_static_ggspectro function to:
# a) Handle stereo audio files.

# Maybe modify to allow any color palette (not only viridis....)

# Load library
library(dynaSpec)

# Modify original function. 
prep_static_ggspectro_ch <- function (soundFile, destFolder, outFilename, savePNG = FALSE, 
          colPal = "inferno", crop = NULL, bg = NULL, filter = NULL, 
          xLim = NULL, yLim = c(0, 10), plotLegend = FALSE, onlyPlotSpec = TRUE, 
          ampTrans = 1, min_dB = -30, wl = 512, ovlp = 90, wn = "blackman", 
          specWidth = 9, specHeight = 3, colbins = 30, ampThresh = 0, 
          bgFlood = FALSE, fontAndAxisCol = NULL, optim = NULL,
          channel = "left", ...) 
{
  if (missing(destFolder)) {
    if (dynaSpec:::is.url(soundFile)) {
      destFolder = getwd()
    }
    else {
      destFolder = dirname(tools::file_path_as_absolute(soundFile))
    }
  }
  if (destFolder == "wd") {
    destFolder <- getwd()
  }
  if (!grepl("/$", destFolder)) {
    destFolder = paste0(destFolder, "/")
  }
  if (dynaSpec:::is.url(soundFile)) {
    utils::download.file(soundFile, paste0(destFolder, basename(soundFile)))
    soundFile = paste0(destFolder, basename(soundFile))
  }
  if (missing(outFilename)) {
    outFilename = paste0(tools::file_path_sans_ext(basename(soundFile)), 
                         ".PNG")
  }
  if (!grepl(".png|PNG", outFilename)) {
    outFilename = paste0(outFilename, ".png")
  }

   if (length(colPal) == 1) {
    isViridis <- T
  }
  else {
    isViridis <- F
  }
  if (is.null(bg)) {
    if (isViridis) {
      pal = eval(parse(text = paste0("viridis::", colPal)))
      bg = pal(1)
    }
    else {
      bg = colPal[1]
    }
  }

  if (tools::file_ext(soundFile) == "mp3") {
    print("***Converting mp3 to wav***")
    wav0 <- tuneR::readMP3(soundFile)
  }
  else {
    wav0 <- tuneR::readWave(soundFile)
  }
  
  # if (wav0@stereo) {
  #   wav0 <- tuneR::mono(wav0, which = "left")
  # }
  
  # b) Added to make it possible to choose the channel to plot. ----
  if (wav0@stereo) {
    channel <- match.arg(channel, c("left", "right"))
    wav0 <- tuneR::mono(wav0, which = channel)
  }
  # b) End modification ----
  
  prepped <- dynaSpec:::processSound(wav0, crop = crop, xLim = xLim, filter = filter, 
                          ampThresh)
  if (length(yLim) == 1) {
    yLim = c(0, yLim)
  }
  specOutList <- dynaSpec:::ggSpec(wav = prepped$newWav, soundFile = soundFile, 
                        segLens = prepped$segLens, savePNG = savePNG, specWidth = specWidth, 
                        specHeight = specHeight, destFolder = destFolder, colPal = colPal, 
                        isViridis = isViridis, crop = crop, bg = bg, filter = filter, 
                        xLim = prepped$xLim, yLim = yLim, plotLegend = plotLegend, 
                        onlyPlotSpec = onlyPlotSpec, ampTrans = ampTrans, ampThresh = ampThresh, 
                        min_dB = min_dB, wl = wl, ovlp = ovlp, wn = wn, colbins = colbins, 
                        bgFlood = bgFlood, fontAndAxisCol = fontAndAxisCol, optim = optim, 
                        ...)
  plot(specOutList$specList[[1]])
  if (length(prepped$segWavs) > 1) {
    cat("\nFor segmented spectrogram, only segment 1 shown\n")
  }
  specParams = list(soundFile = soundFile, destFolder = destFolder, 
                    outFilename = outFilename, crop = crop, colPal = colPal, 
                    isViridis = isViridis, xLim = prepped$xLim, yLim = yLim, 
                    plotLegend = plotLegend, onlyPlotSpec = onlyPlotSpec, 
                    ampTrans = ampTrans, ampThresh = ampThresh, min_dB = min_dB, 
                    bg = bg, wl = wl, ovlp = ovlp, wn = wn, specWidth = specWidth, 
                    specHeight = specHeight, colbins = colbins, bgFlood = bgFlood, 
                    autoFontCol = specOutList$autoFontCol, fontAndAxisCol = specOutList$fontAndAxisCol, 
                    spec = specOutList$specList, newWav = prepped$newWav, 
                    segWavs = prepped$segWavs)
  return(specParams)
}
