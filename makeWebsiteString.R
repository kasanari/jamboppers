makeWebsiteString = function(sentList){
  
  websiteString <- rep(0, times = 1, length.out = length(sentList), each = 1)
  websiteString[sentList == "like"] <- 1
  
  txt = ""
  for (answer in websiteString) {
    txt = paste(txt, sprintf("%d", answer), sep = "")
  }
  return(txt)
}