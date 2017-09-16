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

### petroreadr

This package is currently work-in-progress.

To install the package, run `devtools::install_github("ravenroadresources/petroreadr")`

At today, it includes 3 functions to read file formats commonly used in the oil
industry. 

 * read_las()
 * read_asciigrid()
 * read_gslib()


Additionally, a modified version of the `base::summary` function is included.
This modified version adds p90, p10 and st.dev.

&nbsp;

Developed by: [RavenRoad S.A.S.](http://www.ravenroadresources.com)

[![Build Status](https://travis-ci.org/ravenroadresources/petroreadr.svg?branch=master)](https://travis-ci.org/ravenroadresources/petroreadr)
