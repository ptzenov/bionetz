if(!require(pacman)){
  install.packages("pacman")
}


pacman::p_load("readr","data.table","magrittr","stringr")
# library(data.table)
measurement_records <- read_csv(
  "measurement_records.csv")
# View(measurement_records)
# library(readr)
dataset <- read_csv("dataset.csv", col_types = cols(id = col_character(),
                                                    session_id = col_character(), timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                                    user_id = col_character()))
setDT(dataset)
# View(dataset)
# 
# library(dataMaid)
# clean(dataset)
# 
# library(dataMaid)
# clean(measurement_records,output = "html",
#       file="cleaned_measurement_records.html")

library(magrittr)
dataset %>% unique(by="timestamp")->dataset
vars_to_diff <- c("throttle","pc_brake",'pc_steering',"pc_speed","pc_pos_x","pc_pos_y",
                  "pc_lap_distance","vr_pos_x","vr_pos_y","vr_pos_z","vr_rotation_x","vr_rotation_y",
                  "vr_rotation_z") %>% intersect(colnames(dataset))

library(stringr)
dataset[,str_c(vars_to_diff,"_diff"):=lapply(.SD,function(x)jitter(c(0,diff(x)))),.SDcols=vars_to_diff,
        by=.(session_id,user_id)]
diffed_cols <- grep(x=colnames(dataset),pattern = "_diff$",value = T)


setDT(measurement_records)
setDT(dataset)