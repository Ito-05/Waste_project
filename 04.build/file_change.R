main <- function(){
  
  colname_list <- make_name_list()
  folder_name <- "frequency"
  
  
}

check_data <- readxl::read_xls(here::here('02.raw', prefecture, folder_name, file_name), sheet = 4)


check_data <- dplyr::mutate(check_data,...15=str_replace(...15,pattern="○",replacement = "1"))


make_name_list <- function(){
  
  colname_list <- c("prefecture_name",
                    "city_id",
                    "city_name",
                    "burnable",
                    "non_burnable",
                    "paper",
                    "paper_pack",
                    "cardboard",
                    "metals",
                    "glass",
                    "plastic_bottles")
  
  return(colname_list)
}


file_name <- "22.xls"
prefecture <- "hokkaido"


check_data <- readxl::read_xls(here::here('02.raw', prefecture, folder_name[1], file_name))


open_file <- function(prefecture, colname_list, folder_name_list){
  
  base_data <- data.frame(matrix(ncol = 12)[0, ])
  
  for(base_year in 17:31){
    
    if(base_year <= 18){
      extension <- ".xls"
      file_name <- paste0(base_year, extension)
      new_data <- readxl::read_xls(here::here('02.raw', prefecture, folder_name_list[1], file_name)) %>% 
        dplyr::select(1,2,3,16,24,40,48,52)
      
      col_specific <- colname_list[-6:-8]
      
      colnames(new_data) <- col_specific
      
      new_data <- mutate(new_data, paper = 0, .after = non_burnable)
      new_data <- mutate(new_data, paper_pack = 0, .after = paper)
      new_data <- mutate(new_data, cardboard = 0, .after = paper_pack)
      
      
      colnames(new_data) <- colname_list
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
      
    } else if(base_year <= 20){
      extension <- ".xls"
      file_name <- paste0(base_year, extension)
      new_data <- readxl::read_xls(here::here('02.raw', prefecture, folder_name[1], file_name)) %>% 
        dplyr::select(1,2,3,20,30,40,50,60,70,80,90)
      
      colnames(new_data) <- colname_list
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
    
    
    } else if(base_year <= 27){
      extension <- ".xls"
      file_name <- paste0(base_year, extension)
      new_data <- readxl::read_xls(here::here('02.raw', prefecture, folder_name[1], file_name)) %>% 
        dplyr::select(1,2,3,19,28,37,46,55,64,73,82)
      
      colnames(new_data) <- colname_list
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
      
      
    } else if(base_year >=28){
      extension <- ".xlsx"
      file_name <- paste0(base_year, extension)
      new_data <- readxl::read_xlsx(here::here('02.raw', prefecture, folder_name[1], file_name)) %>% 
        dplyr::select(1,2,3,18,26,34,42,50,58,67,74)
      
      colnames(new_data) <- colname_list
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
      
    }
    
    
    
    new_data <- slice_tail(new_data, n = nrow(new_data) - 6)
    
    base_data <- rbind(base_data, new_data)
    
    
    
  }
  
  
  return(base_data)
  
  
}

iwate_frequency <- open_file("iwate", colname_list, folder_name)

city_name_list <- dplyr::distinct(complete_tiba, city_name)



modify_data <- function(data, colname_list){
   
  new_data <- data
  
  for (num in 4:11){
    
    new_column <- stringr::str_replace(unlist(new_data[num]), pattern="回", replacement="")
    new_column <- stringi::stri_trans_general(new_column, "Fullwidth-Halfwidth")
    df <- data.frame(new_column)
    colnames(df) <- colname_list[num - 1]
    
    new_data[num] <- df 
    
  }

  return(new_data)
}

count_unique <- function(data, kind){
  
  kind <- rlang::enquo(kind)
  
  city_name_list <- data[4]
  
  city_name_count <- c()
  
  for(i in 1:nrow(data)){
    
    new_frame <- data %>% 
      filter(city_name == as.character(city_name_list[i,]))
    
    name_num <-  distinct(new_frame, !!kind)
    
    if(nrow(name_num) != 1){
      # print(as.character(city_name_list[i,]))
      
      city_name_count <- c(city_name_count, as.character(city_name_list[i,]))
      
    }
    
  }
  return(city_name_count)
  
}


compile_pop <- function(){
  
  base_data <- data.frame(matrix(ncol = 4)[0, ])
  
  for(base_year in 2005:2019){
    
    file_name <- paste0(base_year,'.xls')
    new_data <-  readxl::read_xls(here::here('02.raw','population',file_name)) %>% 
      dplyr::select(c(1,3,6))
    
    colnames(new_data) <- c("city_id",
                            "city_name_diff",
                            "pop")
    
    new_data <- new_data %>% 
      dplyr::mutate(year = base_year - 1988, .after = city_id)
    
    base_data <- rbind(base_data, new_data)
    
  }
  
  base_data$city_id <- stringr::str_sub(base_data$city_id, start = 1, end = 5)
  
  return(base_data)
  
}




