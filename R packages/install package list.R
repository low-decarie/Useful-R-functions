if(!require(plyr))install.packages("plyr")

load("./R/package_list.RData")

l_ply(package.list, install.packages)