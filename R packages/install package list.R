if(!require(plyr))install.packages("plyr")

load("~/Documents/Useful-R-functions/R packages/package_list.RData")

l_ply(package.list, install.packages)