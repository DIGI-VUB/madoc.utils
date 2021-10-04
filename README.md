# madoc.utils

This repository contains an R package for extracting data from Madoc.

> ‘Madoc’ is an ‘Omeka S’ based platform for the display, enrichment,
> and curation of digital objects in ‘IIIF’ format. The platform can be
> used for all kinds of crowdsourcing activities in the domain of
> digital humanities.

### Installation

-   For installing the development version of this package:
    `remotes::install_github("DIGI-VUB/madoc.utils")`

### Example

-   Get transcriptions

``` r
library(madoc.utils)
site         <- "https://www.madoc.ugent.be/s/brugse-vrije"

## Get all projects on that madoc site
projects     <- madoc_projects(site)
projects     <- subset(projects, slug == "brugse-vrije-gebruikerstest")
projects
```

    #>   project_id collection_id                        slug        label summary
    #> 1         12          2746 brugse-vrije-gebruikerstest Brugse Vrije

``` r
## Get all manifests and canvasses of a collection
manifests <- list()
manifests <- madoc_collection(site = site, id = projects$collection_id, tidy_metadata = TRUE)
head(manifests)
```

    #>   manifest_id collection_id manifest_type
    #> 1         398          2746    collection
    #> 2         399          2746      manifest
    #> 3         400          2746      manifest
    #> 4         401          2746      manifest
    #> 5         402          2746      manifest
    #> 6         403          2746      manifest
    #>                                           manifest_label manifest_canvasCount
    #> 1 Collectie Brugse Vrije - Gebruikerstest - Brugse Vrije                  193
    #> 2                                              Subject 2                    5
    #> 3                                              Subject 1                    6
    #> 4                                              Subject 3                    3
    #> 5                                              Subject 4                    8
    #> 6                                              Subject 5                    7
    #>     archief   datum verhoor      eeuw      familienaam geslacht
    #> 1      <NA>            <NA>      <NA>             <NA>     <NA>
    #> 2 RA Brugge     14 mei 1708 18de eeuw             <NA>     <NA>
    #> 3 RA Brugge     12 mei 1708 18de eeuw            Clist        M
    #> 4 RA Brugge      3 mei 1708 18de eeuw De Schoolmeester        M
    #> 5 RA Brugge     4 juni 1708 18de eeuw        Van Damme        M
    #> 6 RA Brugge 1 augustus 1708 18de eeuw       De Deckere        M
    #>                       id inventaris inventaris nummer pagina's  rol
    #> 1                   <NA>       <NA>              <NA>     <NA> <NA>
    #> 2 RABrugge_I15_16999_V02        I15             16999        5    V
    #> 3 RABrugge_I15_16999_V01        I15             16999        6    V
    #> 4 RABrugge_I15_16999_V03        I15             16999        3    V
    #> 5 RABrugge_I15_16999_V04        I15             16999        8    V
    #> 6 RABrugge_I15_16999_V05        I15             16999        7    V
    #>   stad-platteland taal   voornaam
    #> 1            <NA> <NA>       <NA>
    #> 2      Platteland   NL       <NA>
    #> 3      Platteland   NL     Thomas
    #> 4      Platteland   NL     Pieter
    #> 5      Platteland   NL Jo(h)annes
    #> 6      Platteland   NL     Lieven

``` r
canvasses <- madoc_manifest(site = site,   id = manifests$manifest_id)
head(manifests)
```

    #>   manifest_id collection_id manifest_type
    #> 1         398          2746    collection
    #> 2         399          2746      manifest
    #> 3         400          2746      manifest
    #> 4         401          2746      manifest
    #> 5         402          2746      manifest
    #> 6         403          2746      manifest
    #>                                           manifest_label manifest_canvasCount
    #> 1 Collectie Brugse Vrije - Gebruikerstest - Brugse Vrije                  193
    #> 2                                              Subject 2                    5
    #> 3                                              Subject 1                    6
    #> 4                                              Subject 3                    3
    #> 5                                              Subject 4                    8
    #> 6                                              Subject 5                    7
    #>     archief   datum verhoor      eeuw      familienaam geslacht
    #> 1      <NA>            <NA>      <NA>             <NA>     <NA>
    #> 2 RA Brugge     14 mei 1708 18de eeuw             <NA>     <NA>
    #> 3 RA Brugge     12 mei 1708 18de eeuw            Clist        M
    #> 4 RA Brugge      3 mei 1708 18de eeuw De Schoolmeester        M
    #> 5 RA Brugge     4 juni 1708 18de eeuw        Van Damme        M
    #> 6 RA Brugge 1 augustus 1708 18de eeuw       De Deckere        M
    #>                       id inventaris inventaris nummer pagina's  rol
    #> 1                   <NA>       <NA>              <NA>     <NA> <NA>
    #> 2 RABrugge_I15_16999_V02        I15             16999        5    V
    #> 3 RABrugge_I15_16999_V01        I15             16999        6    V
    #> 4 RABrugge_I15_16999_V03        I15             16999        3    V
    #> 5 RABrugge_I15_16999_V04        I15             16999        8    V
    #> 6 RABrugge_I15_16999_V05        I15             16999        7    V
    #>   stad-platteland taal   voornaam
    #> 1            <NA> <NA>       <NA>
    #> 2      Platteland   NL       <NA>
    #> 3      Platteland   NL     Thomas
    #> 4      Platteland   NL     Pieter
    #> 5      Platteland   NL Jo(h)annes
    #> 6      Platteland   NL     Lieven

``` r
## Get annotations on a canvas or several canvasses
annotations  <- madoc_canvas_model(site = site, id = canvasses$canvas_id)
anno         <- subset(annotations, nchar(value) > 0)
str(anno)
```

    #> 'data.frame':    12 obs. of  16 variables:
    #>  $ document_id   : chr  "429b1cd7-d6ea-49a0-84fb-8eece8fa37fa" "429b1cd7-d6ea-49a0-84fb-8eece8fa37fa" "f89bb738-adfc-42b5-88f6-d2b4eb47c7c5" "f89bb738-adfc-42b5-88f6-d2b4eb47c7c5" ...
    #>  $ document_type : chr  "entity" "entity" "entity" "entity" ...
    #>  $ document_label: chr  "Brugse Vrije - gebruikerstest" "Brugse Vrije - gebruikerstest" "Brugse Vrije - gebruikerstest" "Brugse Vrije - gebruikerstest" ...
    #>  $ model_id      : chr  "e4254224-47b8-41e4-bdcc-8c8fb249dcfd" "e4254224-47b8-41e4-bdcc-8c8fb249dcfd" "e4254224-47b8-41e4-bdcc-8c8fb249dcfd" "e4254224-47b8-41e4-bdcc-8c8fb249dcfd" ...
    #>  $ status        : chr  "submitted" "draft" "submitted" "draft" ...
    #>  $ authors       : chr  "urn:madoc:user:88" "urn:madoc:user:86" "urn:madoc:user:88" "urn:madoc:user:86" ...
    #>  $ id            : chr  "82ce97cf-20da-40da-bf90-a7b32752a14e" "7f0f0c77-5605-494d-9708-e1af53b9ba51" "f762d622-dd99-4861-b69a-0dd2ae28cf33" "001ffbcc-fa16-4c76-a7d7-0dfba07f4134" ...
    #>  $ type          : chr  "text-field" "text-field" "text-field" "text-field" ...
    #>  $ value         : chr  "Actum 14 meije 1708 cepirage binnen\nBrugghe present d'heeren schepenen Willaeijs ende\nde Berge\n\nGevraeght d"| __truncated__ "Actum 14 meye1708 uitte cepirage binnen Brugge en present d'heeren schepenen Willareijs & de Lange,\n\ngevraegh"| __truncated__ "Dat sy gevangenen inde dreve vanden\nvoorst(en?) hof(stede?) hebben gearretteert oste help en\narresteren seker"| __truncated__ "Dat gij gevangenen inde dreve vande voors.(eijde) hof(stede) hebben gearresteert ofte helpen arresteren zeker m"| __truncated__ ...
    #>  $ label         : chr  "transcriptie" "transcriptie" "transcriptie" "transcriptie" ...
    #>  $ id_revision   : chr  "1fb15dd3-5d05-4734-a745-e2ab6223c407" "120feb12-4541-43df-9e05-e76645ba325b" "e5623b5b-e6d2-4979-916c-9a46c6fc934f" "06fd49d7-dedb-4440-8ef8-fd8551844c64" ...
    #>  $ id_revises    : chr  "8b409b3d-bb73-453d-b2a4-02888b9eb8b5" "8b409b3d-bb73-453d-b2a4-02888b9eb8b5" "a0a69c4d-94ad-4dae-a990-b63c02845219" "a0a69c4d-94ad-4dae-a990-b63c02845219" ...
    #>  $ selector_state: chr  NA NA NA NA ...
    #>  $ selector_type : chr  NA NA NA NA ...
    #>  $ selector_id   : chr  NA NA NA NA ...
    #>  $ canvas_id     : int  590 590 592 592 594 594 595 595 596 596 ...

``` r
## Get URL of canvas for which volunteers performed an annotation
images <- madoc_canvas_image(site = site, id = sort(unique(anno$canvas_id)))
images <- merge(images, canvasses, by = "canvas_id")
images <- images[, c("manifest_id", "canvas_id", "height", "width", "image_url")]
head(images)
```

    #>   manifest_id canvas_id height width
    #> 1         399       590   4117  2677
    #> 2         399       592   4126  2761
    #> 3         399       594   4215  2689
    #> 4         399       595   4059  2641
    #> 5         399       596   1142  1545
    #> 6         403       614   4107  2679
    #>                                                                                                                                        image_url
    #> 1 https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999_V02_01/full/full/0/default.jpg
    #> 2 https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999_V02_02/full/full/0/default.jpg
    #> 3 https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999_V02_03/full/full/0/default.jpg
    #> 4 https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999_V02_04/full/full/0/default.jpg
    #> 5 https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999_V02_05/full/full/0/default.jpg
    #> 6 https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V05:RABrugge_I15_16999_V05_01/full/full/0/default.jpg

-   Combine annotations with image url and manifest metadata

``` r
anno <- merge(images, anno, by = "canvas_id", all.x = TRUE, all.y = FALSE)
anno <- merge(anno, manifests, by = "manifest_id", all.x = TRUE, all.y = FALSE, suffixes = c("", "_manifest"))
anno <- subset(anno, !is.na(value) & nchar(value) > 0)
str(anno)
```

    #> 'data.frame':    12 obs. of  37 variables:
    #>  $ manifest_id         : int  399 399 399 399 399 399 399 399 399 399 ...
    #>  $ canvas_id           : int  590 590 592 592 594 594 595 595 596 596 ...
    #>  $ height              : int  4117 4117 4126 4126 4215 4215 4059 4059 1142 1142 ...
    #>  $ width               : int  2677 2677 2761 2761 2689 2689 2641 2641 1545 1545 ...
    #>  $ image_url           : chr  "https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999"| __truncated__ "https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999"| __truncated__ "https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999"| __truncated__ "https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999"| __truncated__ ...
    #>  $ document_id         : chr  "429b1cd7-d6ea-49a0-84fb-8eece8fa37fa" "429b1cd7-d6ea-49a0-84fb-8eece8fa37fa" "f89bb738-adfc-42b5-88f6-d2b4eb47c7c5" "f89bb738-adfc-42b5-88f6-d2b4eb47c7c5" ...
    #>  $ document_type       : chr  "entity" "entity" "entity" "entity" ...
    #>  $ document_label      : chr  "Brugse Vrije - gebruikerstest" "Brugse Vrije - gebruikerstest" "Brugse Vrije - gebruikerstest" "Brugse Vrije - gebruikerstest" ...
    #>  $ model_id            : chr  "e4254224-47b8-41e4-bdcc-8c8fb249dcfd" "e4254224-47b8-41e4-bdcc-8c8fb249dcfd" "e4254224-47b8-41e4-bdcc-8c8fb249dcfd" "e4254224-47b8-41e4-bdcc-8c8fb249dcfd" ...
    #>  $ status              : chr  "submitted" "draft" "submitted" "draft" ...
    #>  $ authors             : chr  "urn:madoc:user:88" "urn:madoc:user:86" "urn:madoc:user:88" "urn:madoc:user:86" ...
    #>  $ id                  : chr  "82ce97cf-20da-40da-bf90-a7b32752a14e" "7f0f0c77-5605-494d-9708-e1af53b9ba51" "f762d622-dd99-4861-b69a-0dd2ae28cf33" "001ffbcc-fa16-4c76-a7d7-0dfba07f4134" ...
    #>  $ type                : chr  "text-field" "text-field" "text-field" "text-field" ...
    #>  $ value               : chr  "Actum 14 meije 1708 cepirage binnen\nBrugghe present d'heeren schepenen Willaeijs ende\nde Berge\n\nGevraeght d"| __truncated__ "Actum 14 meye1708 uitte cepirage binnen Brugge en present d'heeren schepenen Willareijs & de Lange,\n\ngevraegh"| __truncated__ "Dat sy gevangenen inde dreve vanden\nvoorst(en?) hof(stede?) hebben gearretteert oste help en\narresteren seker"| __truncated__ "Dat gij gevangenen inde dreve vande voors.(eijde) hof(stede) hebben gearresteert ofte helpen arresteren zeker m"| __truncated__ ...
    #>  $ label               : chr  "transcriptie" "transcriptie" "transcriptie" "transcriptie" ...
    #>  $ id_revision         : chr  "1fb15dd3-5d05-4734-a745-e2ab6223c407" "120feb12-4541-43df-9e05-e76645ba325b" "e5623b5b-e6d2-4979-916c-9a46c6fc934f" "06fd49d7-dedb-4440-8ef8-fd8551844c64" ...
    #>  $ id_revises          : chr  "8b409b3d-bb73-453d-b2a4-02888b9eb8b5" "8b409b3d-bb73-453d-b2a4-02888b9eb8b5" "a0a69c4d-94ad-4dae-a990-b63c02845219" "a0a69c4d-94ad-4dae-a990-b63c02845219" ...
    #>  $ selector_state      : chr  NA NA NA NA ...
    #>  $ selector_type       : chr  NA NA NA NA ...
    #>  $ selector_id         : chr  NA NA NA NA ...
    #>  $ collection_id       : int  2746 2746 2746 2746 2746 2746 2746 2746 2746 2746 ...
    #>  $ manifest_type       : chr  "manifest" "manifest" "manifest" "manifest" ...
    #>  $ manifest_label      : chr  "Subject 2" "Subject 2" "Subject 2" "Subject 2" ...
    #>  $ manifest_canvasCount: int  5 5 5 5 5 5 5 5 5 5 ...
    #>  $ archief             : chr  "RA Brugge" "RA Brugge" "RA Brugge" "RA Brugge" ...
    #>  $ datum verhoor       : chr  "14 mei 1708" "14 mei 1708" "14 mei 1708" "14 mei 1708" ...
    #>  $ eeuw                : chr  "18de eeuw" "18de eeuw" "18de eeuw" "18de eeuw" ...
    #>  $ familienaam         : chr  NA NA NA NA ...
    #>  $ geslacht            : chr  NA NA NA NA ...
    #>  $ id_manifest         : chr  "RABrugge_I15_16999_V02" "RABrugge_I15_16999_V02" "RABrugge_I15_16999_V02" "RABrugge_I15_16999_V02" ...
    #>  $ inventaris          : chr  "I15" "I15" "I15" "I15" ...
    #>  $ inventaris nummer   : chr  "16999" "16999" "16999" "16999" ...
    #>  $ pagina's            : chr  "5" "5" "5" "5" ...
    #>  $ rol                 : chr  "V" "V" "V" "V" ...
    #>  $ stad-platteland     : chr  "Platteland" "Platteland" "Platteland" "Platteland" ...
    #>  $ taal                : chr  "NL" "NL" "NL" "NL" ...
    #>  $ voornaam            : chr  NA NA NA NA ...

``` r
library(writexl)
```

    #> Warning: package 'writexl' was built under R version 4.1.1

``` r
write_xlsx(anno, "brugse-vrije.xlsx")
```

-   See the image of a canvas

``` r
library(magick)
```

    #> Warning: package 'magick' was built under R version 4.1.1

    #> Linking to ImageMagick 6.9.12.3
    #> Enabled features: cairo, freetype, fftw, ghostscript, heic, lcms, pango, raw, rsvg, webp
    #> Disabled features: fontconfig, x11

``` r
url  <- anno$image_url[[1]]
url
```

    #> [1] "https://iiif.ghentcdh.ugent.be/iiif/images/getuigenissen:brugse_vrije:RABrugge_I15_16999_V02:RABrugge_I15_16999_V02_01/full/full/0/default.jpg"

``` r
img  <- image_read(url)
img  <- image_resize(img, "x800")
txt  <- anno$value[[1]]

trans <- image_blank(width = image_info(img)$width, height = image_info(img)$height)
trans <- image_annotate(trans, txt, size = 10, color = "green")
image_append(c(img, trans))
```

<img src="tools/img-example-1.png" width="1040" />

### DIGI

By DIGI: Brussels Platform for Digital Humanities:
<https://digi.research.vub.be>

![](tools/logo.png)
