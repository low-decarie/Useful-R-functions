meta_from_doi<-function(username,
                        doi="10.1006/jmbi.2000.4282"){
  
  try.test<-try({
  
  
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
  
  volume<-xpathSApply(root, "//x:volume", xmlValue, namespaces = "x")
  issue<-xpathSApply(root, "//x:issue", xmlValue, namespaces = "x")
  first_page<-xpathSApply(root, "//x:first_page", xmlValue, namespaces = "x")
  last_page<-xpathSApply(root, "//x:last_page", xmlValue, namespaces = "x")
  
  for(i in c("doi.list",
             "journal_title",
             "first.author",
             "last.author",
             "article_title",
             "volume",
             "issue",
             "first_page",
             "last_page",
             "year")){
    if(class(get(i))!="character"){assign(i, NA)}
  }
  
  meta<-data.frame(doi=doi.list,
                   journal=journal_title,
                   first.author=paste(given_name[1], surname[1]),
                   last.author=paste(given_name[length(given_name)], surname[length(surname)]),
                   article.title=article_title,
                   volume=volume,
                   issue=issue,
                   first_page=first_page,
                   last_page=last_page,
                   year=year)
  })
  
  if(class(try.test)=="try-error"){
  
  meta<-data.frame(doi=doi,
                   journal=NA,
                   first.author=NA,
                   last.author=NA,
                   article.title=NA,
                   volume=NA,
                   issue=NA,
                   first_page=NA,
                   last_page=NA,
                   year=NA)
  }
  
  return(meta)
  
}
            