setwd("~/Developer/parks-and-rec-dialog")

#==========================================================
# Read data
#==========================================================

# Get filenames of all files in /scripts
filenames = list.files(
  path = "~/Developer/parks-and-rec-dialog/scripts", 
  pattern = ".csv"
)

# Initialize dataframe
data = data.frame(
  Season = integer(),
  Episode = integer(),
  Character = character(),
  Line = character()
)

# Read data from each CSV file and append to dataframe
for (file_name in filenames) {
  file_data = read.csv(paste("scripts/", file_name, sep = ""))
  
  file_data$Season = as.numeric(strsplit(
    strsplit(file_name, 's')[[1]][2], 'e')[[1]][1])
  
  file_data$Episode = as.numeric(strsplit(
    strsplit(file_name, 'e')[[1]][2], '.csv')[[1]][1])
  
  data = rbind(data, file_data)
}

#==========================================================
# Read data
#==========================================================
