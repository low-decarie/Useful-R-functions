#This function recovers a list of Digital Object Identifiers (DOI) from CrossRef


get_doi<-function(username,
                  journal="",
                  author.name="",
                  year=2011){
  
  require(XML)
  
  title<-""
  if(journal!="") title<-paste("title=", journal,"&", sep="")
  
  aulast<-""
  if(author.name!="") aulast<-paste("aulast=",author.name, "&", sep="")
  
  date<-""
  if(year!="") date<-paste("date=", year, sep="")
  
  doc<-xmlTreeParse(readLines(paste("http://www.crossref.org/openurl?",
                                    title,
                                    aulast,
                                    date,
                                    "&multihit=true&pid=",
                                    username, sep="")),
                    useInternalNodes = TRUE)

  root <- xmlRoot(doc)
  
  doi.list<-xpathSApply(root, "//x:doi", xmlValue, namespaces = "x")
  
  return(doi.list)
  
}

test<-get_doi(username="etienne.low-decarie@mail.mcgill.ca")

