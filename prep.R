library(readxl)
library(rjson)
library(httr)
library(stringr)
library(rvest)
library(pracma)
library(dplyr)

# +---------------------------------------------------------------------------+
# First, download the data from the BFI website
# https://www.bfi.org.uk/industry-data-insights/weekend-box-office-figures
# +---------------------------------------------------------------------------+

# Create a data.frame of download urls
downloads <- data.frame(
  year = c('2001-2006/', '2007/', '2008/', '2009/', '2010/', '2011/', '2012/', '2013/', '2014/', '2015/', '2016/', '2017/', '2018/'),
  urls = c(
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-5',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-3',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-1',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-0',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-4',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-2',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-6',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-2013',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-2014',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-2015',
    'https://www2.bfi.org.uk/publications/corporate-documents-publications/film-industry-statistics-research/box-office-reports-2016',
    'https://www.bfi.org.uk/industry-data-insights/weekend-box-office-figures/uk-weekend-box-office-reports-2017',
    'https://www.bfi.org.uk/industry-data-insights/weekend-box-office-figures/uk-weekend-box-office-reports-2018'
  )
)

# Now loop through and download each year batch to a local set of folders
for (year in downloads$year){
  row <- which(year == downloads$year)
  page <- downloads$url[row]
  page <- xml2::read_html(page)  
  # Get the url for the download xls files
  files <- page %>%
    rvest::html_nodes("a") %>%     # find all links
    rvest::html_attr("href") %>%   # get the url
    stringr::str_subset("\\.xls")  # find those that end in xls
  
  # if not given a filename search for 'download'
  if(pracma::isempty(files)){
    files <- page %>%
      rvest::html_nodes("a") %>%     # find all links
      rvest::html_attr("href") %>%   # get the url
      stringr::str_subset("download")  # find those that end in xls
  }
  
  # replace ww2 if it exists as this will throw error
  files <- gsub('www2', 'www', files)
  
  # Loop through and download each file
  for (file in files){
    # Use the alternate way to "download.file" as we want to preserve filename
    file_Location <- paste0('data/raw/', year)
    write_fn <- paste0(file_Location, "tmp.fil")
    fil <- GET(file, write_disk(write_fn, overwrite = T))
    
    # If filename not given in content-disposition
    if(is.null(headers(fil)$`content-disposition`)){
      # It will be the filename
      fname_rename <- paste0(file_Location, basename(file))
    } else {
      # Get what name the site suggests it should be
      fname_rename <- paste0(file_Location, str_match(headers(fil)$`content-disposition`, "\"(.*)\"")[2])  
    }
    # Now rename the file to the correct name
    file.rename(write_fn, fname_rename)
  }
}

# +---------------------------------------------------------------------------+
# Now clean up the files and combine...
# +---------------------------------------------------------------------------+

year <- '2019-2021'
files <- list.files(paste0('data/raw/', year, '/'), full.names = T)

df_all <- data.frame()
for (fn in files){
  fn_num <- which(fn == files)
  print(paste0('Processing file ', fn_num, ' of ', length(files)))
  print(paste0('File ref.: ', basename(fn)))
  df <- readxl::read_xls(fn, sheet = 1, trim_ws = T)
  df <- df[, 1:10] 
  
  # remove any columns that are all NA
  df <- Filter(function(x) !all(is.na(x)), df)
  
  # Remove rows that contain NAs.
  # TODO --- check if this is too aggressive.
  df <- df %>% na.omit()
  
  bfn <- gsub('.xls', '', basename(fn))
  fn_date <- as.Date(substr(bfn, nchar(bfn)-12, nchar(bfn)-3))+2
  names(df) <- gsub('%', 'percent', gsub(' ', '_', tolower(df[1, ])))
  df$weekend_sunday <- fn_date
  df <- df[-1, ]
  
  if ('percent_change_on_last_week' %in% names(df)){
    df$percent_change_on_last_week <- as.numeric(df$percent_change_on_last_week)  
  } else {
    df$percent_change_on_last_week <- NA
  }
  
  num_fields <- c('rank', 'weeks_on_release', 'number_of_cinemas', 'site_average', 'total_gross_to_date')
  
  for (field in num_fields){
    ind <- which(names(df) == field)
    col <- data.frame(lapply(df[, ind], function(x) as.integer(x)))  
    df[, ind] <- col
  }
  
  df <- df %>% arrange(rank)
  df_all <- rbind(df_all, df)
}

df_all <- df_all %>% arrange(weekend_sunday)
write.csv(df_all, 'data/weekendbf.csv', row.names = F)

