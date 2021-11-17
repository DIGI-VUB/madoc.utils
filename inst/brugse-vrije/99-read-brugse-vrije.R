library(madoc.utils)
library(udpipe)
library(data.table)
site         <- "https://www.madoc.ugent.be/s/brugse-vrije"

## Get all projects on that madoc site
projects     <- madoc_projects(site)
projects     <- subset(projects, slug == "brugse-vrije-gebruikerstest")
projects

## Get all manifests and canvasses of a collection
manifests <- list()
manifests <- madoc_collection(site = site, id = projects$collection_id, tidy_metadata = TRUE)
canvasses <- madoc_manifest(site = site,   id = manifests$manifest_id)

## Get tasks
tasks <- list()
tasks$canvas   <- madoc_tasks(site = site, project = "brugse-vrije-gebruikerstest", type = "canvas", id = canvasses$canvas_id)
tasks$canvas   <- subset(tasks$canvas, !is.na(id))
tasks$manifest <- madoc_tasks(site = site, project = "brugse-vrije-gebruikerstest", type = "manifest", id = manifests$manifest_id)
tasks$manifest <- subset(tasks$manifest, !is.na(id))
madoc_login("https://www.madoc.ugent.be/s/brugse-vrije", 
            email = "jan.wijffels@vub.be", password = Sys.getenv("MADOC_PWD"))
tasks$details_canvas   <- madoc_taskdetails(site = "https://www.madoc.ugent.be", id = tasks$canvas$id)
tasks$details_manifest <- madoc_taskdetails(site = "https://www.madoc.ugent.be", id = tasks$manifest$id)
canvasses      <- merge(canvasses, tasks$canvas, by. = "canvas_id", by.y = "canvas_id", all.x = TRUE, all.y = FALSE, suffixes = c("", ".task"))

## Get detailed information about the revision tasks
revisions        <- setNames(tasks$details_canvas$subtasks, tasks$details_canvas$task_id)
revisions        <- rbindlist(revisions, idcol = "canvas_task_id", fill = TRUE)
revisions_detail <- madoc_taskdetails(site = "https://www.madoc.ugent.be", id = revisions$id)
revisions <- merge(revisions[, c("canvas_task_id", 
                                 "id", "id_revision", "id_task_review", "id_task_manifest", 
                                 "type", "name", "status_text", "subject", "state")], 
                   revisions_detail[, c("task_id", "created_at", "modified_at", "status_text", "creator", "assignee", 
                                        "name", "description", "type", "subject", 
                                        "state", "parameters")], 
                   by.x = "id", by.y = "task_id", all.x = TRUE, all.y = FALSE, suffixes = c("", "_detail"))

## Get users
users <- madoc_users()

## Get annotations
annotations  <- madoc_canvas_model(site = site, id = canvasses$canvas_id)
anno         <- subset(annotations, nchar(value) > 0)
str(anno)

## Get URL of canvas for which volunteers performed an annotation
images <- madoc_canvas_image(site = site, id = sort(unique(anno$canvas_id)))
images <- merge(images, canvasses, by = "canvas_id")
images <- images[, c("manifest_id", "canvas_id", "height", "width", "image_url")]
head(images)

## Combine images url with the annotations and the manifest metadata
anno <- merge(images, anno, by = "canvas_id", all.x = TRUE, all.y = FALSE)
anno <- merge(anno, manifests, by = "manifest_id", all.x = TRUE, all.y = FALSE, suffixes = c("", "_manifest"))
anno <- subset(anno, !is.na(value) & nchar(value) > 0)


x <- merge(anno, 
           revisions[, c("id_revision", "created_at", "modified_at", "assignee")], 
           by.x = "id_revision", by.y = "id_revision", all.x = TRUE, all.y = FALSE)
x <- x[, c("manifest_id", "canvas_id", "id_revision", "created_at", "modified_at", 
           "assignee", "height", "width", 
           "image_url", "document_id", "document_type", "document_label", 
           "model_id", "status", "authors", "id", "type", "value", "label", 
           "id_revises", "selector_state", "selector_type", "selector_id", 
           "collection_id", "manifest_type", "manifest_label", "manifest_canvasCount", 
           "archief", "datum verhoor", "eeuw", "familienaam", "geslacht", 
           "id_manifest", "inventaris", "inventaris nummer", "pagina's", 
           "rol", "stad-platteland", "taal", "voornaam")]
x$assignee <- txt_recode(x$assignee, from = users$madoc_id, to = users$email)
x <- x[order(x$modified_at, decreasing = TRUE), ]

save.image(file = "anno.RData")
library(writexl)
write_xlsx(x, "brugse-vrije.xlsx")
