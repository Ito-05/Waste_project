make_name_list <- function(){
  
  colname_price <- c("prefecture_name",
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

folder_name

price_hokkaido <- open_price("hokkaido", colname_price, folder_name = "frequency") %>% 
  arrange(city_name)

check_hokkaido <- price_hokkaido

check_hokkaido <- na.omit(check_hokkaido)

kind_list <- count_unique(check_hokkaido,burnable)

narrow_list <-  c(unique(kind_list))

hhh <- check_hokkaido %>% 
  filter(city_name %in% narrow_list)

price_tokyo <- open_price("tokyo", colname_price, folder_name = "frequency") %>% 
  arrange(city_name)

check_tokyo <- price_tokyo
check_tokyo<- na.omit(check_tokyo)
kind_list <- count_unique(check_tokyo,burnable)
narrow_list <-  c(unique(kind_list))
 
View(filter(check_tokyo, city_name %in% narrow_list))





open_price <- function(prefecture, colname_price, folder_name){
  
  base_data <- data.frame(matrix(ncol = 12)[0, ])
  
  for(base_year in 17:31){
    
    if(base_year <= 17){
      extension <- ".xls"
      file_name <- paste0(base_year, extension)
      new_data <- readxl::read_xls(here::here('02.raw', prefecture, folder_name, file_name), sheet = 4) %>% 
        dplyr::select(1,2,3,20,21,36,37,68,69,84,85,100,101)
      
      
      
      price_name_list <- c("prefecture_name",
                           "city_id",
                           "city_name",
                           "burnable",
                           "burnable_free",
                           "non_burnable",
                           "non_burnable_free",
                           "metals",
                           "metals_free",
                           "glass",
                           "glass_free",
                           "plastic_bottles",
                           "plastic_bottles_fee")

      colnames(new_data) <- price_name_list
      
      new_data <- new_data %>% 
        dplyr::slice_tail(n = nrow(new_data) - 5)
      
      for (i in 4:ncol(new_data)){
        
        change_one <- tibble(city_name = new_data[3],
                             stringr::str_replace(unlist(new_data[i]), pattern = "○", replacement = "1"))
        
        new_data[i] <- change_one[2]
        
        
      }
      
      new_data[is.na(new_data)] <- "100"

      for (i in seq(4,ncol(new_data), by = 2)) {
        
        
        for (j in 1:nrow(new_data)) {
          
          
          if(unlist(new_data[j,i+1]) == "1"){
            
            new_data[j,i] <- "0" 
            
            }
            
          }
      }
      
      new_data <- new_data %>% 
        select(c("prefecture_name",
                 "city_id",
                 "city_name",
                 "burnable",
                 "non_burnable",
                 "metals",
                 "glass",
                 "plastic_bottles"))
      
      new_data[new_data=="100"] <- NA
      
      new_data <- mutate(new_data, paper = NA, .after = non_burnable)
      new_data <- mutate(new_data, paper_pack = NA, .after = paper)
      new_data <- mutate(new_data, cardboard = NA, .after = paper_pack)
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
        
      
      } else if(base_year <= 18){
        extension <- ".xls"
        file_name <- paste0(base_year, extension)
        new_data <- readxl::read_xls(here::here('02.raw', prefecture, folder_name, file_name), sheet = 4) %>% 
          dplyr::select(1,2,3,16,17,28,29,52,53,64,65,76,77)
        
        
        
        price_name_list <- c("prefecture_name",
                             "city_id",
                             "city_name",
                             "burnable",
                             "burnable_free",
                             "non_burnable",
                             "non_burnable_free",
                             "metals",
                             "metals_free",
                             "glass",
                             "glass_free",
                             "plastic_bottles",
                             "plastic_bottles_fee")
        
        colnames(new_data) <- price_name_list
        
        new_data <- new_data %>% 
          dplyr::slice_tail(n = nrow(new_data) - 6)
        
        for (i in 4:ncol(new_data)){
          
          change_one <- tibble(city_name = new_data[3],
                               stringr::str_replace(unlist(new_data[i]), pattern = "○", replacement = "1"))
          
          new_data[i] <- change_one[2]
          
          
        }
        
        new_data[is.na(new_data)] <- "100"
        
        for (i in seq(4,ncol(new_data), by = 2)) {
          
          
          for (j in 1:nrow(new_data)) {
            
            
            if(unlist(new_data[j,i+1]) == "1"){
              
              new_data[j,i] <- "0" 
              
            }
            
          }
        }
        
        new_data <- new_data %>% 
          select(c("prefecture_name",
                   "city_id",
                   "city_name",
                   "burnable",
                   "non_burnable",
                   "metals",
                   "glass",
                   "plastic_bottles"))
        
        new_data[new_data=="100"] <- NA
        
        new_data <- mutate(new_data, paper = NA, .after = non_burnable)
        new_data <- mutate(new_data, paper_pack = NA, .after = paper)
        new_data <- mutate(new_data, cardboard = NA, .after = paper_pack)
        
        new_data <- new_data %>% 
          dplyr::mutate(year = base_year, .before = prefecture_name)
        
        
      
    } else if(base_year <= 20){
      extension <- ".xls"
      file_name <- paste0(base_year, extension)
      new_data <- readxl::read_xls(here::here('02.raw', prefecture, folder_name, file_name), sheet = 4) %>% 
        dplyr::select(1,2,3,16,17,28,29,40,41,52,53,64,65,76,77,88,89,100,101)
      
      
      
      price_name_list <- c("prefecture_name","city_id","city_name",
                           "burnable","burnable_free",
                           "non_burnable","non_burnable_free",
                           "paper","paper_free",
                           "paper_pack","paper_pack_free",
                           "cardboard","cardboard_free",
                           "metals","metals_free",
                           "glass","glass_free",
                           "plastic_bottles","plastic_bottles_fee")
      
      colnames(new_data) <- price_name_list
      
      new_data <- new_data %>% 
        dplyr::slice_tail(n = nrow(new_data) - 6)
      
      for (i in 4:ncol(new_data)){
        
        change_one <- tibble(city_name = new_data[3],
                             stringr::str_replace(unlist(new_data[i]), pattern = "○", replacement = "1"))
        
        new_data[i] <- change_one[2]
        
        
      }
      
      new_data[is.na(new_data)] <- "100"
      
      for (i in seq(4,ncol(new_data), by = 2)) {
        
        
        for (j in 1:nrow(new_data)) {
          
          
          if(unlist(new_data[j,i+1]) == "1"){
            
            new_data[j,i] <- "0" 
            
          }
          
        }
      }
      
      new_data <- new_data %>% 
        select(colname_price)
      
      new_data[new_data=="100"] <- NA
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
      
    } else if(base_year <=27){
      extension <- ".xls"
      file_name <- paste0(base_year, extension)
      new_data <- readxl::read_xls(here::here('02.raw', prefecture, folder_name, file_name), sheet = 4) %>% 
        dplyr::select(1,2,3,15,16,26,27,37,38,48,49,59,60,70,71,81,82,92,93)
      
      
      price_name_list <- c("prefecture_name","city_id","city_name",
                           "burnable","burnable_free",
                           "non_burnable","non_burnable_free",
                           "paper","paper_free",
                           "paper_pack","paper_pack_free",
                           "cardboard","cardboard_free",
                           "metals","metals_free",
                           "glass","glass_free",
                           "plastic_bottles","plastic_bottles_fee")
      
      colnames(new_data) <- price_name_list
      
      new_data <- new_data %>% 
        dplyr::slice_tail(n = nrow(new_data) - 6)
      
      for (i in 4:ncol(new_data)){
        
        change_one <- tibble(city_name = new_data[3],
                             stringr::str_replace(unlist(new_data[i]), pattern = "○", replacement = "1"))
        
        new_data[i] <- change_one[2]
        
      }
      
      new_data[is.na(new_data)] <- "100"
      
      for (i in seq(4,ncol(new_data), by = 2)) {
        
        
        for (j in 1:nrow(new_data)) {
          
          
          if(unlist(new_data[j,i+1]) == "1"){
            
            new_data[j,i] <- "0" 
            
          }
          
        }
      }
      
      new_data <- new_data %>% 
        select(colname_price)
      
      new_data[new_data=="100"] <- NA
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
      
      
      
    } else if(base_year >=28){
      extension <- ".xlsx"
      file_name <- paste0(base_year, extension)
      new_data <- readxl::read_xlsx(here::here('02.raw', prefecture, folder_name, file_name), sheet = 4) %>% 
        dplyr::select(1,2,3,15,16,26,27,37,38,48,49,59,60,70,71,81,82,92,93)
      
      
      price_name_list <- c("prefecture_name","city_id","city_name",
                           "burnable","burnable_free",
                           "non_burnable","non_burnable_free",
                           "paper","paper_free",
                           "paper_pack","paper_pack_free",
                           "cardboard","cardboard_free",
                           "metals","metals_free",
                           "glass","glass_free",
                           "plastic_bottles","plastic_bottles_fee")
      
      colnames(new_data) <- price_name_list
      
      new_data <- new_data %>% 
        dplyr::slice_tail(n = nrow(new_data) - 6)
      
      for (i in 4:ncol(new_data)){
        
        change_one <- tibble(city_name = new_data[3],
                             stringr::str_replace(unlist(new_data[i]), pattern = "○", replacement = "1"))
        
        new_data[i] <- change_one[2]
        
      }
      
      new_data[is.na(new_data)] <- "100"
      
      for (i in seq(4,ncol(new_data), by = 2)) {
        
        
        for (j in 1:nrow(new_data)) {
          
          
          if(unlist(new_data[j,i+1]) == "1"){
            
            new_data[j,i] <- "0" 
            
          }
          
        }
      }
      
      new_data <- new_data %>% 
        select(colname_price)
      
      new_data[new_data=="100"] <- NA
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
      
      
      
    }
    
    
    base_data <- rbind(base_data, new_data)
    
    
  }
  
  
  return(base_data)
  
}
