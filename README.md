# madoc.utils

This repository contains an R package for extracting data from Madoc.

>> 'Madoc' is an 'Omeka S' based platform for the display, enrichment, and curation of digital objects in  'IIIF' format. The platform can be used for all kinds of crowdsourcing activities in the domain of digital humanities.

### Installation

- For installing the development version of this package: `remotes::install_github("DIGI-VUB/madoc.utils")`

### Example

- Get transcriptions

```
library(madoc.udils)
site         <- "https://www.madoc.ugent.be/s/brugse-vrije"

## Get all projects on that madoc site
projects     <- madoc_projects(site)
projects     <- subset(projects, slug == "brugse-vrije-gebruikerstest")
projects

## Get all manifests and canvasses of a collection
manifests    <- madoc_collection(site = site, id = projects$collection_id)
head(manifests, n = 3)
canvasses    <- madoc_manifest(site = site,   id = manifests$manifest_id)
str(canvasses)

## Get annotations
annotations  <- madoc_canvas_model(site = site, id = canvasses$canvas_id)
head(subset(annotations, nchar(value) > 0))
```

- See the image of a canvas

```
library(magick)
canvasses_urls <- madoc_canvas_image(site, id = sample(canvasses$canvas_id, size = 10))
x              <- merge(canvasses, canvasses_urls, by = "canvas_id")
url <- unlist(x$image_url[[1]])
image_read(url)
```

### DIGI

By DIGI: Brussels Platform for Digital Humanities: https://digi.research.vub.be

![](vignettes/logo.png)
