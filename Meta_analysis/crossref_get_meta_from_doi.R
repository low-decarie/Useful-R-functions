meta_from_doi<-function(username,
                        doi="10.1006/jmbi.2000.4282"){
  
  
  doc<-xmlTreeParse(readLines(paste("http://www.crossref.org/openurl/?pid=",
                                    username,
                                    "&id=", doi,
                                    "&noredirect=true",
                                    sep="")),
                    useInternalNodes = TRUE)
  
  root <- xmlRoot(doc)
  
  doi.list<-xpathSApply(root, "//x:doi", xmlValue, namespaces = "x")
  
  journal_title<-xpathSApply(root, "//x:journal_title", xmlValue, namespaces = "x")
  
  surname<-xpathSApply(root, "//x:surname", xmlValue, namespaces = "x")
  
  given_name<-xpathSApply(root, "//x:given_name", xmlValue, namespaces = "x")
  
  year<-xpathSApply(root, "//x:year", xmlValue, namespaces = "x")
  
  article_title<-xpathSApply(root, "//x:article_title", xmlValue, namespaces = "x")
  
  meta<-data.frame(doi=doi.list,
                   journal=journal_title,
                   first.author=paste(given_name[1], surname[1]),
                   last.author=paste(given_name[length(given_name)], surname[length(surname)]),
                   article.title=article_title
                   year=year)
  
  return(meta)
  
}
            