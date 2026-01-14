## Convert PDF plots to PNG (for GitHub)

library(pdftools)

setwd("C:/Projects-Code-Certificates/Credit-Risk-Mortgages")

pdfs <- list.files("figures", pattern="\\.pdf$", full.names=TRUE)

for (pdf in pdfs) {
  
  base <- tools::file_path_sans_ext(basename(pdf))
  
  pdf_convert(
    pdf, 
    format="png", 
    dpi=200)
    
}


