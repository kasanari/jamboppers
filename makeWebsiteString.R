makeWebsiteString = function(sentList){
  
  websiteString <- rep(0, times = 1, length.out = length(sentList), each = 1)
  websiteString[sentList == "like"] <- 1
  
  websitestring = paste(websiteString, "")
  
  return(websiteString)
}