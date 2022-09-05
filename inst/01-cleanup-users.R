library(madoc.utils)
library(writexl)
madoc_login("https://www.madoc.ugent.be/s/brugse-vrije", email = "jan.wijffels@vub.be", password = Sys.getenv("MADOC_PWD"))
x <- madoc_users(type = "global")
write_xlsx(x, path = "madoc_users.xlsx")
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


library(madoc.utils)
library(writexl)
madoc_login("https://www.madoc.ugent.be/s/getuigenissen-2-0", email = "jan.wijffels@vub.be", password = Sys.getenv("MADOC_PWD"))
users_global <- madoc_users(type = "global")
users_site   <- madoc_users(type = "site")
x <- merge(users_site, users_global, by.x = "id", by.y = "id", suffixes = c("", "_global"), all.x = TRUE, all.y = TRUE)
x <- x[order(x$created, decreasing = TRUE), ]
write_xlsx(x = x[, c("id", "name_global", "email", "created", "is_active", "modified", "site_role", "role_global")], path = "madoc_users.xlsx")
