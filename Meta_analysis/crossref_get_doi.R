#This function recovers a list of Digital Object Identifiers (DOI) from CrossRef


get_doi<-function(username,
                  journal="",
                  author.name="",
                  volume="",
                  issue="",
                  spage="",
                  year=""){
  
  require(XML)
  
  try.test<-try({
  
  title<-""
  if(journal!="") title<-paste("title=", journal, sep="")
  
  aulast<-""
  if(author.name!="") aulast<-paste("&aulast=",author.name, sep="")
  
  date<-""
  if(year!="") date<-paste("&date=", year, sep="")
  

  if(volume!="") {volume<-paste("&volume=", volume, sep="")}
  
  if(issue!="") {issue<-paste("&issue=", issue, sep="")}

  if(spage!="") {spage<-paste("&spage=", spage, sep="")}
  
  
  doc<-xmlTreeParse(readLines(paste("http://www.crossref.org/openurl?",
                                    title,
                                    aulast,
                                    date,
                                    issue,
                                    volume,
                                    spage,
                                    "&multihit=true&pid=",
                                    username, sep="")),
                    useInternalNodes = TRUE)

  root <- xmlRoot(doc)
  
  doi.list<-xpathSApply(root, "//x:doi", xmlValue, namespaces = "x")
  
  })
  
  if(class(try.test)=="try-error"){doi.list<-NA}
  
  return(unlist(doi.list)[1])
  
}


