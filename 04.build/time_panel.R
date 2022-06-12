tt <- readxl::read_xls(here::here('02.raw', "volume", file_name), sheet = 5) 
ile_name <- '21.xls'


library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(rlang)

vv <- readxl::read_xls(here::here('02.raw', "volume", file_name)) 


colname_volume_list <- make_volume_list()


make_volume_list <- function(){
  
  colname_volume_list <- c("prefecture_name",
                            "city_id",
                            "city_name",
                            "burnable_volume",
                            "non_burnable_volume",
                            "paper_volume",
                            "paper_pack_volume",
                            "cardboard_volume",
                            "metals_volume",
                            "glass_volume",
                            "plastic_bottles_volume")
  
  return(colname_volume_list)
            
}



open_volume <- function(colname_volume_list){
  
  base_data <- data.frame(matrix(ncol = 12)[0, ])
  
  for(base_year in 19:31){
    
    if(base_year <= 20){
      extension <- ".xls"
      file_name <- paste0(base_year, extension)
      garbage_data <- readxl::read_xls(here::here('02.raw',"volume", file_name), sheet = 2) %>% 
        dplyr::select(c(1,2,3,79,80))
        
      resource_data <- readxl::read_xls(here::here('02.raw', "volume", file_name), sheet = 5) %>% 
        dplyr::select(2,65,66,67,68,69,70)
      
      new_data <- left_join(garbage_data, resource_data, by = "...2")
      
      colnames(new_data) <- colname_volume_list
      
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
      
      
    } else if(base_year <= 27){
      extension <- ".xls"
      file_name <- paste0(base_year, extension)
      garbage_data <- readxl::read_xls(here::here('02.raw',"volume", file_name), sheet = 2) %>% 
        dplyr::select(c(1,2,3,79,80))
      
      resource_data <- readxl::read_xls(here::here('02.raw', "volume", file_name), sheet = 5) %>% 
        dplyr::select(2,68,69,70,71,72,73)
      
      new_data <- left_join(garbage_data, resource_data, by = "...2")
      
      colnames(new_data) <- colname_volume_list
      
      
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
      
      
    } else if(base_year >=28){
      extension <- ".xlsx"
      file_name <- paste0(base_year, extension)
      garbage_data <- readxl::read_xlsx(here::here('02.raw', "volume", file_name), sheet = 2) %>% 
        dplyr::select(c(1,2,3,79,80))
      
      resource_data <- readxl::read_xlsx(here::here('02.raw', "volume", file_name), sheet = 5) %>% 
        dplyr::select(2,68,69,70,71,72,73)
      
      new_data <- left_join(garbage_data, resource_data, by = "...2")
      
      colnames(new_data) <- colname_volume_list
      
      new_data <- new_data %>% 
        dplyr::mutate(year = base_year, .before = prefecture_name)
      
    }
    
    
    nrow(new_data)
    
    
    new_data <- slice_tail(new_data, n = nrow(new_data) - 17)
    
    base_data <- rbind(base_data, new_data)
    
    
    
  }
  
  
  return(base_data)
  
  
}


volume_city <- open_volume(colname_volume_list) 

openxlsx::write.xlsx(volume_city, "/Users/ito_hiroki/02.Lecture/field_lecture/2022.Spring/volume.xlsx")

prefecture_name <- "kanagawa"

all_prefecture <- function(prefecture_name,volume_city){
  
  frequency_city <- open_file(prefecture_name, colname_list, folder_name_list)
  frequency_city <- modify_data(frequency_city, colname_list)
  
  base_data <- frequency_city
  
  # population_data
  
  all_prefecture <- left_join(base_data, volume_city, by = c("city_id", "year", "city_name"))  
  all_prefecture <- left_join(all_prefecture, population_data, by = c("city_id", "year"))
  
  for(i in 14:23){
    if(i == 22){
       
    } else if(i != 22){
      
      all_prefecture[i] <- as.numeric(unlist(all_prefecture[i]))

    }
    
  }
  

  all_prefecture <- all_prefecture %>% 
    mutate(per_burnable = burnable_volume/pop,
           per_paper = paper_volume/pop)

  return(all_prefecture)
  
}


small_prefecture <- function(prefecture_name,volume_city){
  
  frequency_city <- open_file(prefecture_name, colname_list, folder_name_list)
  frequency_city <- modify_data(frequency_city, colname_list)
  
  base_data <- frequency_city
  
  # population_data
  
  all_prefecture <- left_join(base_data, volume_city, by = c("city_id", "year", "city_name"))  
  all_prefecture <- left_join(all_prefecture, population_data, by = c("city_id", "year"))
  
  for(i in 14:23){
    if(i == 22){
      
    } else if(i != 22){
      
      all_prefecture[i] <- as.numeric(unlist(all_prefecture[i]))
      
    }
    
  }
  
  
  all_prefecture <- all_prefecture %>% 
    mutate(per_burnable = burnable_volume/pop,
           per_paper = paper_volume/pop)
  
  kind_list <- c(count_unique(frequency_city, paper))

  small_prefecture_data <- filter(all_prefecture, city_name %in% kind_list) %>%
    arrange(city_name)

  return(small_prefecture_data)
  
}




small_kanagawa <- small_prefecture("kanagawa", volume_city)
small_tokyo <- small_prefecture("tokyo", volume_city)
small_saitama <- small_prefecture("saitama", volume_city)
small_hokkaido <- small_prefecture("hokkaido", volume_city)
small_osaka <- small_prefecture("osaka", volume_city)
small_kyoto <- small_prefecture("kyoto", volume_city)
small_iwate <- small_prefecture("iwate", volume_city)
small_yamanashi <- small_prefecture("yamanashi", volume_city)
small_toyama <- small_prefecture("toyama", volume_city)
small_ishikawa <- small_prefecture("ishikawa", volume_city)
small_nagano <- small_prefecture("nagano", volume_city)


city_plot <- function(plot_data, variable){
  
  variable <- rlang::enquo(variable)
  
  waste_plot <- ggplot(data = plot_data,
                       aes(x = year, y = !!variable, colour = city_name))+
    geom_point() + 
    geom_line() +
    theme_gray (base_family = "HiraKakuPro-W3")  
  
 return(waste_plot)
  
}



tt_list <- c("南足柄市","厚木市","小田原市","座間市",
             "湯河原町")

variable <- rlang::enquo(variable)

tt_data <- small_kanagawa  %>% 
  filter(city_name %in% tt_list)  

plot(city_plot(tt_data, per_paper))

View(new_data)

city_plot(small_kanagawa, per_paper)


small_plot(small_kanagawa, tt_lists, per_paper)

small_plot(small_tokyo, tokyo_lists, per_paper)

small_plot(small_kyoto,kyoto_lists)
small_plot(small_saitama, saitama_lists)
small_plot(small_hokkaido, hokkaido_lists)
small_plot(small_nagano, nagano_lists)
small_plot(small_yamanashi, yamanashi_lists)


all_hokkaido <- all_prefecture("hokkaido", volume_city)

city_plot(all_hokkaido)


hokkaido_lists <- c("01210","01453","01438",
                    "01403","01212","01215"
                    ,"01347","01452")
kanagawa_lists <- c("14210","14214","14217",
                    "14212","14401")
tokyo_lists <- c("13382","13305","13119",
                 "13218","13121")
saitama_lists <- c("11210","11206")
osaka_lists <- c("27208")
kyoto_lists <- c("26201")
iwate_lists <- c("03206")
yamanashi_lists <- c("19364","19365","19201")
toyama_lists <- c("16209")
ishikawa_lists <- c()
nagano_lists <- c("20422","20211","20583",
                  "20305","20588","20541",
                  "20486","20409","20590")



View(small_nagano)




small_plot <- function(small_data, small_lists, variable){

  
  variable <- rlang::enquo(variable)
  
  new_data <- small_data  %>% 
    filter(city_id %in% small_lists)  
  
  plot(city_plot(new_data, !!variable))
  
  View(new_data)
  
}


small_plot(small_nagano, nagano_lists)

