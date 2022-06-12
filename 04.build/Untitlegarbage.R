library(openxlsx)
library(dplyr)
library(stringr)
library(stringi)

data_20 <- readxl::read_xlsx("")

tiba_21 <- readxl::read_xls("/Users/ito_hiroki/01.Research/Waste_project/02.raw/tiba/frequency/21.xls")

tiba_21_x <- readxl::read_xlsx("/Users/ito_hiroki/01.Research/Waste_project/02.rawdata/tiba/frequency/21.xlsx")

data_kanagawa <- read.xlsx("/Users/ito_hiroki/01.Research/garbage/01.rawdata/kanagawa/garbage_kanagawa.xlsx") %>% 
  arrange(city_name) %>% 
  mutate(fire = str_replace(data_kanagawa$fire, pattern="回", replacement="")) %>% 
  mutate(not_fire = str_replace(data_kanagawa$not_fire, pattern="回", replacement="")) %>% 
  mutate(resource = str_replace(data_kanagawa$resource, pattern="回", replacement="")) %>% 
  mutate(resource = str_replace(data_kanagawa$resource, pattern="回", replacement=""))

colnames(complete_tiba)

stri_trans_general(data_kanagawa$fire, "Fullwidth-Halfwidth")
stri_trans_general(data_kanagawa$not_fire, "Fullwidth-Halfwidth")
stri_trans_general(data_kanagawa$resource, "Fullwidth-Halfwidth")

data_garbage <- read.xlsx("/Users/ito_hiroki/01.Research/garbage/01.rawdata/frequency_garbage.xlsx") %>% 
  arrange(X1)

str_replace(data_kanagawa$fire, pattern="回", replacement="")



for(i in 1:30){
  
  new_frame <- omit_kanagawa %>% 
    filter(city_name == char_list[i])
  
  name_num <-  distinct(new_frame, resource)
  
  if(nrow(name_num) != 1){
    print(i)
  }
  
}

city_name_list[1]

new_frame <- omit_kanagawa %>% 
  filter(city_name == char_list[1])

name_num <-  distinct(new_frame, fire)

nrow(name_num)


volume_data <- readxl::read_xls("/Users/ito_hiroki/Downloads/29_volume.xls", sheet = 2) %>% 
  dplyr::rename(city_id = ...2) 

volume_data <- dplyr::select(volume_data, c(1,2,3,79,80))

resource_data <- readxl::read_xls("/Users/ito_hiroki/Downloads/29_volume.xls", sheet = 5) %>% 
  dplyr::rename(city_id = ...2) 




resource_data <- dplyr::select(resource_data, c(1,2,3,68,69,70,71,72,73))




