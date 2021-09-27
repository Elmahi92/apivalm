library(xml2)
library(tidyverse)
library(dplyr)

# Make a temporary file (tf) and a temporary folder (tdir)
tf <- tempfile(tmpdir = tdir <- tempdir())
#enter year
year <- 2018

url<-paste0("https://data.val.se/val/val",year,"/slutresultat/slutresultat.zip")

url
# Download the zip file
download.file(url, tf)

# Unzip it in the temp folder
xml_files <- unzip(tf, exdir = tdir)


# note that K>> Municipality || L>> County Council || R>> Municipality note that letters in CAPS
file<- "K"
pat<- paste0("slutresultat_\\d{2}",file,".xml$")

# Get the filenames of the files to import
# They have 4 digits in the file name, and ends with the letter K
files_to_import <- fs::dir_ls(tdir) %>%
  str_subset(pattern = pat)

# Create a function to read a file and get the information wanted
val<-as.character(c("//KOMMUN","//VALDISTRIKT"))
read_dist <- . %>%
  read_xml() %>%
  xml_find_all(.,val) %>%
  map_dfr(~ {
    # extract the attributes from the parent tag as a data.frame
    parent <- xml_attrs(.x) %>% enframe() %>% spread(name, value)
    # make a data.frame out of the attributes of the kids
    kids <- xml_children(.x) %>% map_dfr(~ as.list(xml_attrs(.x)))
    # combine them (bind_cols does not repeat parent rows)
    cbind.data.frame(parent, kids) %>% set_tidy_names() %>% as_tibble()
  })
# Map over all the files
KOMMUN <- map_df(files_to_import, read_dist)

#select dataframe
KOMMUN2 <- KOMMUN[,c(7,14:16)]




