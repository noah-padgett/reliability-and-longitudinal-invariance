# pdf to png

library(pdftools)
w.d <- getwd()

files.all <- list.files(paste0(w.d, '/diagrams/'))

files <- paste0(paste0(w.d, '/diagrams/'), grep(".pdf", files.all, value=T))
files.out <- paste0(unlist(strsplit(files, ".pdf")), ".png")
dpi <- rep(400,length(files))
for(i in 1:length(files)){
  pdf_convert(files[i], format = "png", dpi = dpi[i],
              filenames = files.out[i])
}



