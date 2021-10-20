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

month_dict <- data.frame(
  nums = rep(sprintf("%02d", sequence(12)), 2),
  mons = c(
    "january",
    "february", 
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december",
    "jan",
    "feb", 
    "mar",
    "apr",
    "may",
    "jun",
    "jul",
    "aug",
    "sep",
    "oct",
    "nov",
    "dec"
  )
)

years <- data.frame(
  yrs = c('2001-2006', '2007-monthly', '2007', '2008', '2009', '2010', '2011', '2012', '2013', 
          '2014', '2015', '2016', '2017', '2018', '2019-2021'),
  typ = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
)

#year <- '2008'
for (year in years$yrs[2:length(years$yrs)]){
 print(year) 
}  
  files <- list.files(paste0('data/raw/', year, '/'), full.names = T)
  
  if (years$typ[years$yrs == year] == 1){
    files_tmp <- basename(files)
    files_tmp <- gsub('bfi-weekend-box-office-report-', '', files_tmp)
    files_tmp <- gsub('bfi-uk-box-office-', '', files_tmp) 
    files_tmp <- gsub('uk-film-council-box-office-report-', '', files_tmp)
    files_tmp <- gsub('.xls', '', files_tmp)
    files_tmp
    
    files_lis <- strsplit(files_tmp, '-')
    length(files_lis)
    parsed_df <- data.frame()
    for (f in sequence(length(files_lis))){
      parsed_str <- files_lis[[f]]
      chk_len <- length(parsed_str)
      if (chk_len == 5){
        p_add <- data.frame(
          file = files[f],
          day1 = sprintf("%02d", as.integer(parsed_str[2])),
          day2 = sprintf("%02d", as.integer(parsed_str[4])),
          mont = tolower(parsed_str[3]),
          monn = month_dict$nums[tolower(parsed_str[3]) == month_dict$mons],
          year = as.integer(parsed_str[5])
        )
      } else {
        p_add <- data.frame(
          file = files[f],
          day1 = sprintf("%02d", as.integer(parsed_str[1])),
          day2 = sprintf("%02d", as.integer(parsed_str[2])),
          mont = tolower(parsed_str[3]),
          monn = month_dict$nums[tolower(parsed_str[3]) == month_dict$mons],
          year = substr(parsed_str[4], 1, 4)
        )
      }
      p_add$weekend_sunday <- as.Date(paste0(p_add$year, '-', p_add$monn, '-', p_add$day2))
      parsed_df <- rbind(parsed_df, p_add)
    }
  } else {
    files_tmp <- basename(files)
    files_tmp <- gsub('bfi-weekend-box-office-report-', '', files_tmp)
    files_tmp <- gsub('.xls', '', files_tmp)
    files_tmp
    
    files_lis <- strsplit(files_tmp, '-')
    length(files_lis)
    parsed_df <- data.frame()
    for (f in sequence(length(files_lis))){
      p_add <- data.frame(
        file = files[f],
        day1 = sprintf("%02d", as.integer(files_lis[[f]][3])),
        day2 = sprintf("%02d", as.integer(files_lis[[f]][4])),
        monn = sprintf("%02d", as.integer(files_lis[[f]][2])),
        year = substr(files_lis[[f]][1], 1, 4)
      )
      p_add$weekend_sunday <- as.Date(paste0(p_add$year, '-', p_add$monn, '-', p_add$day2))
      parsed_df <- rbind(parsed_df, p_add) 
    }
  }
  
  parsed_df <- parsed_df %>% unique()
  
  df_all <- data.frame()
  files <- parsed_df$file
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
    
    if (tolower(names(df)[1]) == 'rank'){
      names(df) <- gsub('%', 'percent', gsub(' ', '_', tolower(names(df))))
    } else {
      names(df) <- gsub('%', 'percent', gsub(' ', '_', tolower(df[1, ])))  
    }
    
    df$weekend_sunday <- parsed_df$weekend_sunday[parsed_df$file == fn]
    
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
    
    # In some years they used 'title' in place of 'film', change this to 'film'
    if ('title' %in% names(df)){
      names(df)[names(df) %in% 'title'] <- 'film'
    } else if ('#' %in% names(df)){
      names(df)[names(df) %in% '#'] <- 'film'
    }
    
    if ('gross' %in% names(df)){
      names(df)[names(df) %in% 'gross'] <- 'weekend_gross'
    } 
    
    if ('total_to_date' %in% names(df)){
      names(df)[names(df) %in% 'total_to_date'] <- 'total_gross_to_date'
    } 
    
    df <- df %>% arrange(rank)
    df_all <- rbind(df_all, df)
  }
  
  # Arrange by the date of the Sunday of the weekend
  df_all <- df_all %>% arrange(weekend_sunday)
  
  # Write the yearly file to a csv
  write.csv(df_all, paste0('data/weekendbf_', year, '.csv'), row.names = F)
}

# Combine each separate yearly file into a single file
files <- list.files(paste0('data/'), full.names = T)[grepl('csv', files)]
all_data <- data.frame()
for (file in files){
  data <- read.csv(file, stringsAsFactors = F)
  all_data <- rbind(all_data, data)
}
all_data <- all_data %>% arrange(weekend_sunday)
all_data$year <- lubridate::year(all_data$weekend_sunday)
all_data$week <- lubridate::isoweek(all_data$weekend_sunday)
all_data$year_week <- paste0(all_data$year, '-', all_data$week)
write.csv(all_data, 'data/allWeekendBf.csv', row.names = F)

# Create a list of unique film names
films <- data.frame(
  films = sort(unique(all_data$film))
)

films$films <- gsub("<.\\+", " ", films$films)
films$films <- gsub(">", " ", films$films)
films$films <- gsub("FEFF", " ", films$films)
films$films <- trimws(films$films)
write.csv(films, 'data/allFilms.csv', row.names = F)

