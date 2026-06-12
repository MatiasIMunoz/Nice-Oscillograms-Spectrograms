Nice-looking bioacoustic figures
================

<!-- Short Description  -->

Code for making oscillograms and spectrograms of stereo files using
ggplot2.

**Last updates:**

- Modified to allow using any color palette on spectrogram (e.g.,
  parula). Before the function wouldo only accept `viridis`.

- Added log-transformed frequency axis on spectrogram following [Cardoso
  (2025)](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.70145).

- Added time bar to oscillogram and removed time as a line axis.

- Modified the `prep_static_ggspectro` function from the
  [dynaSpec](https://marce10.github.io/dynaSpec/) library to be able to
  manage stereo audio files.

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- ## Status -->
<!-- Project is: _in progress_ -->
<h2>
Examples
</h2>
<p align="center">
<img src="Hyla_stereo_oscillo.png" width="750"/> <br><br>
<img src="Hyla_stereo_spectro.png" width="750"/>
</p>
<h2>
Contact
</h2>

Created by [Matías I.
Muñoz](https://sites.google.com/view/matiasmunozsandoval/contact?authuser=0)
(<ma.munozsandoval@gmail.com>)
