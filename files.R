scaleData = function(data) {
  names = names(data)
  data.scaled = data
  for (name in names) {
    tryCatch({
      #Catch the error that pops up when it tries to scale a non-numeric column
      data.scaled[name] = scale(data[[name]])
    },
    error = function(e) {
      cat(name, "is not a numeric column and can not be scaled\n")
    })
  }
  return(data.scaled)
}

setQualitative = function(data, names){
  for (name in names) {
    data[name] = as.factor(data[[name]]) 
  }
  return(data)
}

write_to_file = function(name, title, predicts){
  txt <- makeWebsiteString(predicts)
  txt = paste(title, txt)
  write(txt, file=name, append = TRUE)
}

makeWebsiteString = function(sentList){
  
  websiteString <- rep(0, times = 1, length.out = length(sentList), each = 1)
  websiteString[sentList == "like"] <- 1
  
  txt = ""
  for (answer in websiteString) {
    txt = paste(txt, sprintf("%d", answer), sep = "")
  }
  return(txt)
}
