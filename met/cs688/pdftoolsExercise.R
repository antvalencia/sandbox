# Extracting the content from a pdf file
# Clear Workspace and Console
rm(list=ls()); cat("\014")
library(pdftools)

# folder "PDF Files" with PDFs
pdf.loc <- file.path(getwd(),"PDF Files")
# Get the path (chr-vector) of PDF file names
myPDFfiles <- normalizePath(list.files(path = pdf.loc, pattern = "pdf",  full.names = TRUE))

# Get the text content from the PDF file
my.text <- pdf_text(myPDFfiles[1])
# Save as txt file
write.table(my.text, file=paste0(pdf.loc,"/text.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " " )


# Convert to text several pdf files that are contained in a single folder.
convert.PDF <- function(myPDFfiles) {
  for (ff in 1:length(myPDFfiles)) {
    pdf.file <- myPDFfiles[ff]
    # Get the text content from the PDF file
    my.text <- pdf_text(pdf.file)
    File.Name <- sub(".pdf",".txt",pdf.file)
    # Save as txt file
    write.table(my.text, file=File.Name, quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " " )
  }
}

convert.PDF(myPDFfiles)

# Use lapply with in line function to convert each PDF file indexed by "i" into a text file 
lapply(
  1:length(myPDFfiles),
  function(ff, myPDFfiles) {
    my.text = pdf_text(myPDFfiles[ff]);
    write.table(
      my.text,
      file=sub(".pdf", ".txt", myPDFfiles[ff]),
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE,
      eol = " "
      )
    },
  myPDFfiles
  )
