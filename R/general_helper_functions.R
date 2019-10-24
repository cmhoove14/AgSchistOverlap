#' Function to insert latex equations into .md dox
#'
#' Function copied directly from here:
#' https://github.com/STAT545-UBC/Discussion/issues/102
#` This is a workaround for the fact that markdown doesn't read latex so
#' equations don't render properly
#'
#' @param latex latex code to generate equation
#' @export
#'

latexImg = function(latex){

    link = paste0('http://latex.codecogs.com/gif.latex?',
           gsub('\\=','%3D',URLencode(latex)))

    link = gsub("(%..)","\\U\\1",link,perl=TRUE)
    return(paste0('![](',link,')'))
}

#' Function to generate log transformed sequence evenly distributed across broad orders of magnitude
#'
#' Functions just like `seq` but evenly distributes values across the full range
#' rather than for instance `seq(0.000001, 10000, length.out = 100)` returning values that are all >100
#'
#' @param min minimum value in the sequence
#' @param max maximum value in the sequence
#' @param seq.length length of the sequence
#'
#' @return numeric vector spanning min and max with n = seq.length entries
#' @export
#'
#'
exp_seq <- function(min, max, seq.length){
  exp(seq(log(min), log(max), length.out = seq.length))
}

#' Load a csv as R object from a googledrive id
#'
#' You can find file ids by going to the sharing settings of a particular csv and
#' copy/pasting the alphanumeric string between "https://drive.google.com/file/d/" and "/view?usp=sharing"
#'
#' @param id googledrive id
#'
#' @return R object from csv in googledrive
#' @export

load_csv_from_googledrive <- function(id){

  temp <- tempfile(fileext = ".csv")
  dl <- googledrive::drive_download(as_id(id), path = temp, overwrite = TRUE)

  out <- readr::read_csv(temp)

  file.remove(temp)

  return(out)
}

#' Load a stata as R object from a googledrive id
#'
#' You can find file ids by going to the sharing settings of a particular csv and
#' copy/pasting the alphanumeric string between "https://drive.google.com/file/d/" and "/view?usp=sharing"
#'
#' @param id googledrive id
#'
#' @return R object from dta file in googledrive
#' @export

load_dta_from_googledrive <- function(id){

  temp <- tempfile(fileext = ".dta")
  dl <- googledrive::drive_download(as_id(id), path = temp, overwrite = TRUE)

  out <- foreign::read.dta(temp, convert.factors=FALSE)

  file.remove(temp)

  return(out)
}

#'  Load a shapefile contained in a googledrive folder as an R sf object
#'  
#` You can find folder ids by going to the sharing settings of a particular folder and 
#'  copy/pasting the alphanumeric string between "https://drive.google.com/file/d/" and "/view?usp=sharing"
#'  
#' @param id googledrive folder id
#'
#' @return R sf object from shapefile file in googledrive folder
#' @export
 
load_sf_from_googledrive <- function(id){

#Get all googledrive ids in the folder containing the shapefile
  file_info <- drive_ls(as_id(id))
  file_ids <- file_info$id
  file_names <- file_info$name

#Create folder to place spatial files in    
  temp <- tempdir()
  
#Get all files in the google drive folder and put them in the temp folder  
  for(i in 1:length(file_ids)){
    drive_download(as_id(file_ids[i]), path = paste0(temp, "//", file_names[i]), overwrite = TRUE)
  }
  
# load as sf object 
  out <- st_read(temp, layer = strsplit(file_names[which(grepl(".shp", file_names))], ".shp")[[1]])
  
  return(out)
}