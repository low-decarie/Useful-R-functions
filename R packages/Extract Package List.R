package.list<-rownames(installed.packages())
save(package.list, file="~/Documents/Useful-R-functions/R packages/package_list.RData")
write.csv(package.list, file="~/Documents/Useful-R-functions/R packages/package_list.csv")