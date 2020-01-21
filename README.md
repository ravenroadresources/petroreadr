---
title: "README"
author: "RavenRoad S.A.S."
date: "2017-09-16"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

[![Build Status](https://travis-ci.org/ravenroadresources/petroreadr.svg?branch=master)](https://travis-ci.org/ravenroadresources/petroreadr)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)



### petroreadr

This package is currently work-in-progress.

To install the package, run `devtools::install_github("ravenroadresources/petroreadr")`

At today, it includes 4 functions to read and write file formats commonly used in the oil industry.

 * `read_las()`
 * `write_las()`
 * `read_asciigrid()`
 * `read_gslib()`


Additionally, a modified version of the `base::summary` function is included, called `summary_mod`.
This modified version adds p90, p10 and st.dev statistics to the output.

&nbsp;


