install.packages("dplyr")    # install the dplyr package
library(dplyr)     # load the package in your work environment

#Reading in the data from CSV
data_since_2020 <- read.csv("statcast_since_2020.csv")

#Filtering out unneccessary columns
data_since_2020 <- data_since_2020[c("batter", "pitcher", "balls", "strikes", "outs_when_up", 
                  "pitch_type", "plate_x", "plate_z", "zone", "type", "events",
                  "description", "bb_type", "inning", "inning_topbot", 
                  "stand", "p_throws", "pitch_number", "bat_score", "fld_score", 
                  "on_3b", "on_2b", "on_1b", "launch_speed_angle", "babip_value", "release_spin_rate", "release_speed")]

#Creating the whiff column
data_since_2020$whiff <- ifelse(grepl("swinging_strike", data_since_2020$description), 1, 0)

#Creating the success column
data_since_2020$success <- ifelse((data_since_2020$whiff | data_since_2020$launch_speed_angle < 3) , 1, 0)
#Creating the bases_occupied column
data_since_2020$bases_occupied <- ""
data_since_2020$bases_occupied <- paste(data_since_2020$bases_occupied, ifelse(is.na(data_since_2020$on_1b), "", "1"), sep = "")
data_since_2020$bases_occupied <- paste(data_since_2020$bases_occupied, ifelse(is.na(data_since_2020$on_2b), "", "2"), sep = "")
data_since_2020$bases_occupied <- paste(data_since_2020$bases_occupied, ifelse(is.na(data_since_2020$on_3b), "", "3"), sep = "")

#Converting p_throws to binary
data_since_2020$p_throws <- ifelse(data_since_2020$p_throws == "R", 1, 0)

#Converting stand to binary
data_since_2020$stand <- ifelse(data_since_2020$stand == 'R', 1, 0)

write.csv(data_since_2020, "statcast_since_2020_mod.csv")

df <- data_since_2020 %>%
  group_by(outs_when_up, balls, strikes, bases_occupied, p_throws, stand) %>%
    group_split()

keys <- data_since_2020 %>%
  group_by(outs_when_up, balls, strikes, bases_occupied, p_throws, stand) %>%
  group_keys()

library(ggplot2)

write.csv(keys, "Pitch Effectiveness Data (Split)/keys")

file_num <- 1
for(situation in df) {
  filename <- paste("Pitch Effectiveness Data (Split)/", as.character(file_num), sep = "")
  filename <- paste(filename, ".csv", sep= "")
  write.csv(df[[file_num]], filename)
  file_num <- file_num + 1
}

test <- df[[100]]
test <- test[test$success == 1, ]

df <- read.csv("Pitch Effectiveness Data (Split)/1.csv")
df_split <- df %>%
  group_by(pitch_type) %>%
    group_split()

df_keys <- df %>%
  group_by(pitch_type) %>%
  group_keys()

df_1 <- df_split[[2]]

keys$success_rate <- 0

keys$success_rate <- success_rate

success_rate <- c()

print

for (i in 1:nrow(keys)){
  temp1 <- df[[i]]$success
  temp2 <- temp1[!is.na(temp1)]
  success_rate[i] <- sum(temp2) / length(temp1)
}
