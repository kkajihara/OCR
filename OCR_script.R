#Tesseract
library(tesseract)
library(dplyr)
library(magick)



### This script is for reading barcode images, extracting the barcode information, and writing out a csv for labelprinting

# photo locations
dest <- "~/Desktop/OCRPics"

# make a vector of PDF file names
myFiles <- list.files(path = dest, pattern = "jpg",  full.names = TRUE, ignore.case = TRUE)

# have the computer read photos and extract text
barcodeList <- lapply(myFiles, function(x) tesseract::ocr(x))

# clean up the barcode name to only include the 5 digit barcode
cleanBarcodeList <- sub(".*(0\\d{4}).*","\\1", barcodeList)

# make a list of numbers as long as there are barcodes
listNumbers      <- 1:length(cleanBarcodeList)

# create a 2 column table for barcodes and numbers
barcodeTable     <- data.frame(SampleBarcode = cleanBarcodeList, number = listNumbers, stringsAsFactors = F)

# create a logical list for every time the barcodes follow the 5 digit ("00...") format
barcodeCheck <- grepl("0\\d{4}",cleanBarcodeList)

# subset the initial barcodeTable containing only barcodes that answered true
goodBarcodes <- barcodeTable[barcodeCheck,]
View(goodBarcodes)

# subset the initial barcodeTable containing only barcodes that answered false
badBarcodes  <- barcodeTable[!barcodeCheck,]
View(badBarcodes)

# function to prompt user with each barcode one-by-one and either confirm correct or overwrite when wrong
humanCheck <- function(barcode){
  
  ### Just for testing
  #barcode <- barcodeTable
  # x = 1
  # for every barcode
  for(x in 1:nrow(barcode)) {
    # print barcode to stdout
    barcode[x,]
    # ask if barcode is okay
    answer <- "null"
    
    # take arguments yes, no, or quit to exit program
    while (answer != 'y' & answer != 'n' & answer != 'quit') {
      answer <- readline(prompt = paste( "Is the barcode", lapply(barcode[x,], function(x) as.character(x)), "okay? Type quit or ", "y/n:  "))
    }
    # if barcode is not okay ask for update 
    if (answer == "n") {
      newBarcode   <-  readline(prompt = "Enter new barcode:    ")
      barcode[x,1] <- newBarcode
    }
    # if barcode is okay skip to the next item in the table
    if (answer == "y") {
      next
    }
    # if done, quit program
    if (answer == "quit") {
      break
    }
  }
  # stores the results of the humanCheck function
  barcodeChecked <- humanCheck(barcodeTable)
  
  # returns the updated table with overwritten barcodes (if applicable)
  return(barcode)
  
}

# stores the results of the humanCheck function
barcodeChecked <- humanCheck(barcodeTable)



# subset sample csv by barcode list
samples    <- read.csv("~/Desktop/C-MAIKI_Sample_Collection.csv", stringsAsFactors = F, colClasses = "character")


# join C-MAIKI samples table to the barcodeChecked table wherever values for "SampleBarcode" are the same
# run dplyr if opening for the first time to avoid error "could not find function inner_join"
labelSheet <- inner_join(barcodeChecked, samples, by = "SampleBarcode")

# create a csv for the joined table
write.table(labelSheet, file = "waimea_freezedried_tough.csv", sep = ",", row.names = FALSE, col.names = TRUE )