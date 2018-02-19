library(combinat)

source("files.R")


songs <- read.csv('training_data.csv', header = T)

training_indices <- sample(nrow(songs), size = 400, replace = FALSE)

qualitative_vars = list("label","key", "time_signature", "mode")
songs = setQualitative(songs, qualitative_vars)

songs.train = songs[training_indices,]
songs.test = songs[-training_indices,]

vars = names(songs)
vars = vars[vars != "label"]

coltostr = function(col) {
  txt = ""
  for (name in col) {
    txt = paste(txt, name, sep = "+")
  }
  return(paste("label", txt, sep = "~"))
}

for (i in 1:3) {
  comb = combn(names, i)
  if (i != 1) {
    for (col in combs) {
      f = as.formula(coltostr(col))
      print(f)
    }
  } else {
    for (name in col) {
      
    }
  }
  
}


