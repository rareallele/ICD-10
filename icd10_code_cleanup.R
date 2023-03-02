library(tidyverse)
library(readxl)
library(writexl)
library(stringr)
library(zeallot)

# Load data
#data <- read_excel("data.xlsx") %>% rename("Code" = `Cause Code`)

valid_codes_df <- read_csv("icd_10_cm_codes.csv")
icd_2011 <- read_csv("CODE LISTS\\allvalid2011.csv")
x_codes <- read_csv("CODE LISTS\\X30-X99.txt") %>% select("ICD-10 code") %>% rename(Code = "ICD-10 code")
w_codes <- read_csv("CODE LISTS\\W00-X29.txt") %>% select("ICD-10 code") %>% rename(Code = "ICD-10 code")
activity_codes <- read_csv("CODE LISTS\\Activity.txt") %>% select("WHO update 1=Add 2=Del") %>% rename(Code = "WHO update 1=Add 2=Del")
other_codes <- bind_rows(x_codes, w_codes, activity_codes, icd_2011)

valid_codes <- valid_codes_df %>%  
  mutate(Code = ifelse(str_length(Code) > 3, paste0(str_sub(Code, 1, 3), ".", str_sub(Code, 4, -1)), Code)) %>% 
  select(Code) %>% 
  bind_rows(., other_codes) %>%
  pull(unique(Code))

# Map values for code conversion
#replacements <- c("Y20.A" = "Y20", "S02.011" = "S02", "P\\." = "", "95.9" = "", "91.3" = "")
replacements <- c("Q29$" = "Q24", "E99$" = "Q99.8", "91\\.$" = "Q91.3")

exceptions <- c("T29", "Z35")

#-------------------------------------------------------------------------------
# Function to deal with placecodes
placecode_function <- function(data){
  # Clean
  data <- data %>% 
    filter(!is.na(Code)) %>%
    mutate(Code = toupper(Code),
           Code = str_replace(Code, ",", "\\."),
           # Fix Y92. placecodes
           Code = str_replace_all(Code, c("Y929" = "Y92.9",
                                          "Y9240" = "Y92.4")))
  
  # Disability codes
  disability_code_types <- c("Disability Register 11", "12 Neurodegen, genetic, birth defect 12", "19 Disabiling medical condition 19",
                             "13 Cerebral palsy 13", "15 Heart and circ 15", "16 Intellectual disability (primary) 16", "17 Other disability 17",
                             "14 Epilepsy 14", "18 Autism 18")
  
  # Create variables
  data <- data %>% 
    mutate(disability_code = as.character(NA),
           disability_code_type = as.character(NA),
           CDSIRC_code = as.character(NA))
  
  # Process codes
  for (row in 1:nrow(data)){
    # If disability code
    if (data[row, "Cause Code Type"] %in% disability_code_types){
      # Assign values to disability variables
      data[row, "disability_code_type"] <- data[row, "Cause Code Type"]
      data[row, "disability_code"] <- data[row, "Code"] 
      # Remove values from other variables
      data[row, c("Cause Code Type", "Chapter", "Code", "Cause")] <- NA
    }
    # If CDSIRC code
    else if (data[row, "Cause Code Type"] == "CDSIRC code"){
      # Assign value to CDSIRC code variable
      data[row, "CDSIRC_code"] <- data[row, "Code"] 
      # Remove values from other variables
      data[row, c("Cause Code Type", "Chapter", "Code", "Cause")] <- NA
    }
    # If place code
    else if (data[row, "Cause Code Type"] == "Place of occurrence code"){
      # Prepend placecode modifier
      data[row, "Code"] <- paste0("Y92.", data[row, "Code"])
    }
    # If activity code
    else if (data[row, "Cause Code Type"] == "Activity code"){
      # Prepend activitycode modifier
      # **** To finish ****
      data[row, "Code"] <- data[row, "Code"]
    }
  }
  return(data)
}


#------------------------------------------------------------------------------
# Function to process ICD-10 codes
icd <- function(data){
  
  # Identify non-matches
  non_matches <- filter(data,
                        !`Cause Code Type` %in% c("Activity code", "Place of occurrence code"),
                        !Code %in% valid_codes)[["Code"]]
  
  # Process non-matching codes
  c = "Code"
  for (row in 1:nrow(data)){
    # For codes that don't match
    if (data[row, c] %in% non_matches & data[row, "Chapter Group"] != "Activity or Place Code" & str_length(data[row, c]) > 3){
      # Add point to codes without them
      if (!str_detect(data[row, c], "\\.")){
        data[row, c] <- paste0(str_sub(data[row, c], 1, 3), ".", str_sub(data[row, c], 4, -1))
      }
      # Shorten 6-digit codes to 5 digits
      if (str_length(data[row, c]) == 6){
        data[row, c] <- str_sub(data[row, c], end = -2)
      }
    }
    # Fix remaining codes by pulling back to 3-digit codes
    if (!data[row, c] %in% valid_codes) {
      data[row, c] <- str_sub(data[row, c], 1, 3)
    }
    if (str_length(data[row, c]) == 2 & !is.na(data[row, c])){
      data[row, c] <- as.character(NA)
    }
  }
  
  # Fix individual replacements
  data <- data %>% mutate(Code = str_replace_all(string = Code, pattern = replacements))
  
  # Identify remaining non-matches
  post_non_matches <- data %>%
                      #mutate(Code = str_replace_all(string = Code,
                      #                              pattern = replacements)) %>% 
                      filter(#!`Cause Code Type` %in% c("Activity code", "Place of occurrence code"),
                        !is.na(Code),     
                        !Code %in% valid_codes)
  
  # Unique non-matches 
  unm <- unique(post_non_matches[["Code"]])
  unm <- unm[! unm %in% exceptions]
  
  # Delete nonsense codes *****
  #data <- data %>% filter(is.na(Code) | !Code %in% unm)
  
  return(list(data, unm))
}

#-----------------------------------------------------------------------------
# Execute

data <- read_excel("data.xlsx") %>% rename("Code" = `Cause Code`)
data <- placecode_function(data)
c(data, unm) %<-% icd(data)

#-----------------------------------------------------------------------------

cases_with_unm <- filter(data, !is.na(Code), Code %in% unm)

