datacite.search<-function(doi){
  
  doc<-xmlTreeParse(readLines(paste("http://search.datacite.org/api?q=",
                                    gsub("/", "%2F", doi),
                                    "&fl=doi&wt=xml&indent=true",
                                    sep="")),
                    useInternalNodes = TRUE)
  
  root <- xmlRoot(doc)
  
  doi.list<-xpathSApply(root, "response", xmlValue)
  
  
  return()
}