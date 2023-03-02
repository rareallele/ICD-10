
codes <- read_csv("CODE LISTS\\A00-V99.txt") %>% select("ICD-10 code") %>% rename(Code = "ICD-10 code")
valid_placecodes <- read_csv("CODE LISTS\\Place.txt") %>% select("ICD-10-AM Map") %>% rename(Code = "ICD-10-AM Map")
activity_codes <- read_csv("CODE LISTS\\Activity.txt") %>% select("ICD-10-AM Map 1=Add 2=Del") %>% rename(Code = "ICD-10-AM Map 1=Add 2=Del")
x_codes <- read_csv("CODE LISTS\\X30-X99.txt") %>% select("ICD-10 code") %>% rename(Code = "ICD-10 code")
y_codes <- read_csv("CODE LISTS\\Y00-Z99.txt") %>% select("ICD-10 code") %>% rename(Code = "ICD-10 code")
w_codes <- read_csv("CODE LISTS\\W00-X29.txt") %>% select("ICD-10 code") %>% rename(Code = "ICD-10 code")

code_dfs <- c(codes, valid_placecodes, activity_codes, x_codes, y_codes, w_codes)

all_valid_codes <- bind_rows(code_dfs)
