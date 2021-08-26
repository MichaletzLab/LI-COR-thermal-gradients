read_licor6400_jcg = function(file, debug = FALSE){
  
  if(debug){
    print(file)
  }
  
  is_XLS <-grepl("\\.xls$", file) 
  #check if excel file
  if(is_XLS){
    tmpdir <- tempdir()
    newfile <- file.path(
      tmpdir, 
      "licor", 
      gsub("xls$", "csv", basename(file))
    )
    system(paste("code/unoconv -f csv", "-o", newfile, file))
    file <- newfile
  } 
  
  ## reading in raw licor data file
  f <- read_lines(file = file) #min_N = 15
  
  #find header_row
  header_row <- grep("^.{0,1}Obs", f)
  #remove extra header rows
  if(length(header_row) > 1){ 
    f <- f[-header_row[-1]]
    header_row <- header_row[1]
  }
  #check if header row
  if(length(header_row) == 0){
    warning("No header row in ", file)
    return(data_frame())
  }
  
  #remove everything above header row
  f <- f[-(1:(header_row - 1))]  
  
  #removing junk  - where licore restarts
  if(!is_XLS & any(grepl("\\$", f))){
    remove <- grep("\\$", f)
    f <- f[-c(remove, remove - 1)]
    remove <- grep("OPEN", f)
    f <- f[-c(remove, remove + 1)]
    remove <- grep("<", f)
    f <- f[-remove]
  }  
  
  
  #remove in/out line from excel file
  in_out <- grep("^in", f)
  if(length(in_out) > 0){
    f <- f[-in_out]  
  }
  
  ##finding rows of metadata
  meta_rows <- grep("^\"|Remark", f)
  if(!is_XLS){
    meta_rows <- meta_rows[-1] #keep header row
  }
  
  
  meta <- f[meta_rows]
  dat <- f[-meta_rows]
  
  if(length(meta_rows) == 0){
    #    warning(paste("exit no meta on",file))
    #    return(data_frame())
    dat = f
  }
  
  
  if(length(dat) <= 1){
    warning(paste("exit no data on", file))
    return(data_frame())
  }
  
  meta <- gsub("\"|Remark=,", "", meta)
  ## replace multiple spaces with a single space
  meta <- gsub(" +", " ", meta)
  ## grouping multiple worded parameters 
  meta <- gsub("CO2 Mixer", "CO2_Mixer", meta)
  #meta must have arrow
  meta <- grep("->", meta, value = TRUE)#some rows only have a timestamp
  #CO2_Mixer -> OFF upsets things remove
  meta <- grep("CO2_Mixer -> OFF", meta, invert = TRUE, value = TRUE)  
  
  
  #remove comma from csv
  meta <- gsub(",", "", meta)
  dat <- gsub(",", "\t", dat)
  
  
  dat <- read_delim(paste(dat, collapse = "\n"), delim = "\t")
  
  if (length(meta)>0) {
    meta <- read_delim(file = paste(meta, collapse = "\n"), delim = " ", col_names = c("HHMMSS", "variable", "parameter", "arrow", "level", "unit")) %>% 
      select(-arrow)
  } else {
    meta <- NULL#data.frame(HHMMSS = NA, variable = NA, parameter = NA, level = NA, unit = NA)
  }
  
  
  #meta <- read_delim(paste(meta, collapse = "\n"), delim = " ", col_names = c("HHMMSS", "variable", "parameter", "arrow", "level", "unit")) %>% 
  #    select(-arrow)
  
  
  if(is.null(meta)) {
    
    dat <- dat %>% 
      arrange(HHMMSS) %>% # sort data by time logged
      filter(!is.na(Obs)) %>%
      mutate(n = n())
    
  } else {

    dat <- bind_rows(meta %>% # bind logged data and metadata
                       filter(parameter == "Tblock"), dat) %>% # discard parameter changes except block temp
      arrange(HHMMSS) %>% # sort data by time logged
      fill(variable, parameter, level, unit) %>%  # fill block temp parameter in for logged data
      filter(!is.na(Obs)) %>%
      group_by(level) %>% # group by block temperature
      mutate(n = n())
  }
    
  return(dat)
}
