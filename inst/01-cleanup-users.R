library(madoc.utils)
madoc_login("https://www.madoc.ugent.be/s/brugse-vrije", email = "jan.wijffels@vub.be", password = Sys.getenv("MADOC_PWD"))
x <- madoc_users(type = "global")
x <- subset(x, role %in% "researcher" & is_active == FALSE)
## delete all users which have no ampersand in email
out <- subset(x, !grepl(email, pattern = "@"))
View(out)
for(id in out$id){
  madoc_delete_user(id)
}
## delete all users which have no dot after the ampersand
out <- x
out$extension <- sapply(strsplit(out$email, split = "@"), function(x) tail(x, n = 1))
out <- subset(out, !grepl(extension, pattern = "\\."))
View(out)
for(id in out$id){
  madoc_delete_user(id)
}
