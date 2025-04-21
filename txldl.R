#Read data from "All_GPUs.csv"
GPUs <- read.csv("C:/HK242/XSTK/AssignmentXSTK/All_GPUs.csv")

#Create a dataset containing the key variables and take the 6 first rows of the dataset                                                     
final_df <- (GPU[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Pixel_Rate", "TMUs", "Max_Power", "Manufacturer", "Memory_Type")])
head(final_df)

#Fill blank cells with NA and check for missing data.
final_df[final_df == ""] <- NA
final_df[final_df == "-"] <- NA
library(questionr)
freq.na(final_df)

#Remove missing data and check for missing data again
final_df <- final_df[!is.na(final_df$Memory_Type), ]
freq.na(final_df)

#Remove the units from the quantitative variables.
final_df$Max_Power <- as.numeric(gsub("[^0-9]", "", final_df$Max_Power))
final_df$Pixel_Rate <- as.numeric(gsub("[^0-9]", "", final_df$Pixel_Rate))
final_df$Memory <- as.numeric(gsub("[^0-9]", "", final_df$Memory))
final_df$Memory_Speed <- as.numeric(gsub("[^0-9]", "", final_df$Memory_Speed))
final_df$Memory_Bandwidth <- as.numeric(gsub("[^0-9]", "", final_df$Memory_Bandwidth))

#Group and process data based on similar values in Memory_Type
library(dplyr) #Add library
final_df <- final_df %>%
  mutate(Memory_Type = case_when(
    Memory_Type %in% c("DDR", "DDR2", "DDR3", "DDR4") ~ "DDR",
    Memory_Type == "eDRAM" ~ "eDRAM",
    grepl("^GDDR", Memory_Type) ~ "GDDR",
    grepl("^HBM", Memory_Type) ~ "HBM",
    TRUE ~ Memory_Type
  ))

#Add library
library(VIM)
#Check for numerical variables.
numeric_columns <- sapply(final_df, is.numeric)
#Apply kNN to the selected variables.
final_df[numeric_columns] <- kNN(final_df[numeric_columns], k = 5, imp_var = FALSE)

