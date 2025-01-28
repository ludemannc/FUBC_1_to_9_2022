#Description of script: ---
##This script converts Fertilizer Use by Crop (FUBC) survey data from the latest survey (FUBC_9_raw_data.csv),
## into a crop aggregated format so that it can be joined with historic FUBC data (FUBC_1_to_8_data.csv),
## into one combined file. The script also provides functions to aggregate crop categories to align to FAO 
##aggregate crop names as well as the International Fertilizer Associations (IFA) crop categories used in
##the IFA FUBC report number 8. 
##As validation of the latest survey data, the script also provide a comparison in total nutrient consumption by country based on the latest FUBC survey
##data and data exported from the IFASTAT database (https://www.ifastat.org/databases/plant-nutrition).

#This script was written in R version 4.1.0 with a 64-bit computer.

#Libraries----
library(readr)
library(countrycode)
library(dplyr)
library(stringr)

#Settings----
#Ensure all values are decimals rather than scientific notation. See: https://stackoverflow.com/questions/44725001/convert-scientific-notation-to-numeric-preserving-decimals/44725320
options(scipen = 999)

#Read files----
df <- read_csv("./data/FUBC_9_raw_data.csv") #Latest fertilizer use by crop (FUBC) survey data-FUBC_9
IFA_Regions <- read_csv("./data/IFA_Regions.csv") #Information on International Fertilizer Associations (IFA) world Regions by country.
FUBC_1_to_8_data <- read_csv("./data/FUBC_1_to_8_data.csv") #Historic fertilizer use by crop data (FUBC 1-8). This is a mixture of FAO/IFA and IFA publications that were manually converted to machine readable format by Cameron Ludemann.
IFASTAT_data <- read_csv("./data/IFADATA_Plant_Nutrition_query.csv") #IFA data on total nutrients applied as inorganic fertilizer by country. Used for validation. Details for how these data were exported from the IFA website are included at the top of the file. 

#Tidy data frames----
#Create columns with internationally recognized country names, codes and regions. 
#Add ISO3_code
df$ISO3_code <- countrycode(df$Original_country_name_in_FUBC_report, 
                            origin = 'country.name', destination = 'iso3c')

#Add Country name based on official United Nations English name.
df$Country <- countrycode(df$ISO3_code, 
                          origin = 'iso3c', destination = 'un.name.en') 

#Tidy crop names
#carefully sort out crops that state "(total)" to make sure data are not lost.----
#check country names
unique_country <- as.data.frame(unique(df$Country))
#View(unique_country)

#check crop names
unique_crop <- as.data.frame(unique(df$Original_crop_name_in_FUBC_report))
#View(unique_crop)

#Create original crop name column for reference
df$Crop <- df$Original_crop_name_in_FUBC_report

#Note that in the USA they only reported maize, total so we must assume this is maize,grain. 
#create df for USA data that needs to be excluded. 
df_USA_maize_Grain <-df%>% filter(Country== "United States of America"& Crop== "Maize Grain")
df_USA_maize_Green <-df%>% filter(Country== "United States of America"& Crop== "Maize, Green (silage, bioenergy)")

#row bind together
df_USA_maize <- rbind(df_USA_maize_Grain,df_USA_maize_Green)

#anti_join df with df_USA_maize so that we effectively exclude the grain and green maize data from df. 
df <- anti_join(df,df_USA_maize, by =c("Crop","Country"))

#Note that there is a Tobacco (total) for Tanzania but this has no survey data so exclude from df. 
#create df for Tanzania data that needs to be excluded. 
df_tobacco_total <-df%>% filter(Crop== "Tobacco (total)")

#anti_join the selected Tanzanian data from the main df. 
df <- anti_join(df,df_tobacco_total, by =c("Crop","Country"))

#Note that there is a Rice (total) for Senegal. Senegal does not have any sub components of rice so we can safely convert Rice (total) to Rice, paddy. 
df_rice_total <-df%>% filter(Crop== "Rice (total)")

#Convert any referrals to Rice, (total) to Rice, paddy and any others are to remain the same. 
df$Crop  <- ifelse(grepl("Rice \\(total\\)", df$Crop), "Rice, paddy", df$Crop)
unique_crop <- as.data.frame(unique(df$Crop))
#View(unique_crop)

#Note that there is a Roots & Tubers (Total) for Myanmar.  Myanmar has Roots & Tubers (Potato) value so we can safely delete the Roots & Tubers (Total) for Myanmar. 
df_roots_and_tubers_total <-df%>% filter(Crop== "Roots & Tubers (Total)")
#View(df_roots_and_tubers_total)

#anti_join the selected data from the main df. 
df <- anti_join(df,df_roots_and_tubers_total, by =c("Crop","Country"))

#Create function to convert crop names to UN FAO names ----
#Function to convert 2022 IFA survey crop names into UNFAO crop categories for reporting.  
#You need to use a data frame (df) with a column that 
#has the unstandardised Crop information in one column. 
#eg: df <- IFA_survey_to_UNFAO_crop_names(df,df$Crop)
#Based on: https://stackoverflow.com/questions/59082243/multiple-patterns-for-string-matching-using-case-when

IFA_survey_to_UNFAO_crop_names <- function (df,Crop){
  df <- df %>%
    mutate(Crop = case_when(str_detect(Crop, 
                                       regex("Oil palm fruit", ignore_case=TRUE)) ~ "Oil palm",
                            TRUE ~ Crop)) %>% 
    #To get around the UNFAO standard name for Oil Palm Fruit including fruit in its name, 
    #I delete fruit so it doesn't end up in the fruit category in the next mutate.
    mutate(Crop_UNFAO = case_when(
      str_detect(Crop, regex("Wheat", ignore_case=TRUE)) ~ "Wheat",
      str_detect(Crop, regex("Rice", ignore_case=TRUE)) ~ "Rice, paddy",
      str_detect(Crop, regex("^Maize$|^Maize, total$|^Maize Grain$|^Maize grain$|^Grain maize$|^Maize for grain$|^Maize, Total$|^Grain Maize$|^Maize, Grain$|^Grain maize, including corn cob maize$", ignore_case=TRUE)) ~ "Maize",
      str_detect(Crop, regex("^Maize, Green$|^Maize, green$|^Maize (Green)$|Maize (Green)|Green Maize|Maize, Green|^Maize (Green)$|Maize, Green (silage, bioenergy)", ignore_case=TRUE)) ~ "Maize, green",
      str_detect(Crop, regex("irrigated field crop|Cereal|^Grain and other crops$|^Grain mixed sheep & Beef$|^Other cereals$", ignore_case=TRUE)) ~ "Other cereals",
      str_detect(Crop, regex("Soy", ignore_case=TRUE)) ~ "Soybeans",
      str_detect(Crop, regex("Potato", ignore_case=TRUE)) ~ "Potato",
      str_detect(Crop, regex("Sorghum", ignore_case=TRUE)) ~ "Sorghum",
      str_detect(Crop, regex("Vegetable", ignore_case=TRUE)) ~ "Vegetables, fresh nes",
      str_detect(Crop, regex("grass|Grass|Pasture|Hay|Rangeland|Lucerne|lucerne|Perennial crops|Fodder|Forages", ignore_case=TRUE)) ~ "Grassland",
      str_detect(Crop, regex("Forestry|Forests|Forest|forest", ignore_case=TRUE)) ~ "Forestry",
      str_detect(Crop, regex("Tomato", ignore_case=TRUE)) ~ "Tomato",
      str_detect(Crop, regex("Tobacco", ignore_case=TRUE)) ~ "Tobacco, unmanufactured",
      str_detect(Crop, regex("Fruit|fruit|melon|citrus", ignore_case=TRUE)) ~ "Fruit, fresh nes",
      str_detect(Crop, regex("^Roots|Roots and Tubers|Roots & Tubers|Root crops|Roots/tubers|^Taro$|^Cocoa yam$|Cocoyam", ignore_case=TRUE)) ~ "Roots and tubers nes",
      str_detect(Crop, regex("Olive|olive", ignore_case=TRUE)) ~ "Olives",
      str_detect(Crop, regex("^Sugar beet and Sugar cane$", ignore_case=TRUE)) ~ "Sugar crops nes",
      str_detect(Crop, regex("^Sugarcane$|^Sugar Cane$|^Sugar cane$", ignore_case=TRUE)) ~ "Sugar cane",
      str_detect(Crop, regex("^Sugarbeet$|^Sugar Beet$|Sugar, Total (Beet)", ignore_case=TRUE)) ~ "Sugar beet",
      str_detect(Crop, regex("Vineyard|Vine", ignore_case=TRUE)) ~ "Grapes",
      str_detect(Crop, regex("Coffee", ignore_case=TRUE)) ~ "Coffee, green",
      str_detect(Crop, regex("^Cocoa$|^Cocoa, beans$", ignore_case=TRUE)) ~ "Cocoa, beans",
      str_detect(Crop, regex("^Rapeseed$|Rapeseed|Canola|Oilseed rape", ignore_case=TRUE)) ~ "Rapeseed",
      str_detect(Crop, regex("Garlic|garlic", ignore_case=TRUE)) ~ "Garlic",
      str_detect(Crop, regex("Onion|onion", ignore_case=TRUE)) ~ "Onion",
      str_detect(Crop, regex("Cotton", ignore_case=TRUE)) ~ "Cottonseed",
      str_detect(Crop, regex("Coconut", ignore_case=TRUE)) ~ "Coconuts",
      str_detect(Crop, regex("Millet|millet|Teff|teff|Fonio", ignore_case=TRUE)) ~ "Millet",
      str_detect(Crop, regex("Cassava|cassava", ignore_case=TRUE)) ~ "Cassava",
      str_detect(Crop, regex("Sweet corn", ignore_case=TRUE)) ~ "Sweet corn",
      str_detect(Crop, regex("Rubber", ignore_case=TRUE)) ~ "Rubber, natural",
      str_detect(Crop, regex("Flax", ignore_case=TRUE)) ~ "Flax fibre and tow",
      str_detect(Crop, regex("Oil palm", ignore_case=TRUE)) ~ "Oil palm fruit",
      str_detect(Crop, regex("Sunflower", ignore_case=TRUE)) ~ "Sunflower seed",
      TRUE ~ Crop))
  return(df)      
}


#Create function to convert UN FAO crop names to UN FAO aggregate names ----
#Function to convert 2022 UNFAO crop names into UNFAO aggregate  crop categories for reporting.   Use this function after the 'IFA_survey_to_UNFAO_crop_names' function. 
#You need to use a data frame (df) with a column that 
#has the UNFAO standardised Crop information in one column. 
#eg: df <- UNFAO_to_UNFAO_aggregated_crop_names(df,df$Crop_UNFAO)
#Based on: https://stackoverflow.com/questions/59082243/multiple-patterns-for-string-matching-using-case-when

UNFAO_to_UNFAO_aggregated_crop_names <- function (df,Crop_UNFAO){
  df <- df %>%
    mutate(Crop_UNFAO_aggregated = case_when(
      str_detect(Crop_UNFAO, regex("Wheat", ignore_case=TRUE)) ~ "Wheat",
      str_detect(Crop_UNFAO, regex("Rice, paddy|Rice", ignore_case=TRUE)) ~ "Rice, paddy",
      str_detect(Crop_UNFAO, regex("^Maize$", ignore_case=TRUE)) ~ "Maize",
      str_detect(Crop_UNFAO, regex("^Maize, green$", ignore_case=TRUE)) ~ "Maize, green",
      str_detect(Crop_UNFAO, regex("Other cereals", ignore_case=TRUE)) ~ "Other cereals",
      str_detect(Crop_UNFAO, regex("Soybeans", ignore_case=TRUE)) ~ "Soybeans",
      str_detect(Crop_UNFAO, regex("Oil palm fruit", ignore_case=TRUE)) ~ "Oil palm fruit",  
      str_detect(Crop_UNFAO, regex("Rapeseed", ignore_case=TRUE)) ~ "Rapeseed", 
      str_detect(Crop_UNFAO, regex("Other oilseeds|Other Oil Crops|Oilseeds|Other oil Crops|Oth Oilseeds|^Other Oil Crops (Coconut)$|Olive|Coconuts", ignore_case=TRUE)) ~ "Other oil Crops",     
      str_detect(Crop_UNFAO, regex("Cottonseed", ignore_case=TRUE)) ~ "Cottonseed", 
      str_detect(Crop_UNFAO, regex("Sugar beet|Beet", ignore_case=TRUE)) ~ "Sugar beet",  
      str_detect(Crop_UNFAO, regex("Sugar cane", ignore_case=TRUE)) ~ "Sugar cane",
      str_detect(Crop_UNFAO, regex("Sugar cane and Sugar beet|Sugar crops nes", ignore_case=TRUE)) ~ "Sugar crops nes",    
      str_detect(Crop_UNFAO, regex("Tea", ignore_case=TRUE)) ~ "Tea",          
      str_detect(Crop_UNFAO, regex("Coffee, green", ignore_case=TRUE)) ~ "Coffee, green",
      str_detect(Crop_UNFAO, regex("Cocoa, beans", ignore_case=TRUE)) ~ "Cocoa, beans",   
      str_detect(Crop_UNFAO, regex("Roots and tubers nes", ignore_case=TRUE)) ~ "Roots and tubers nes",         
      str_detect(Crop_UNFAO, regex("Fruit, fresh nes|Grapes|Orange|orchards|Treenuts|Treenuts, Total", ignore_case=TRUE)) ~ "Fruit primary and citrus fruits and treenuts",       
      str_detect(Crop_UNFAO, regex("Vegetables, fresh nes", ignore_case=TRUE)) ~ "Vegetables, fresh nes",     
      str_detect(Crop_UNFAO, regex("Grassland", ignore_case=TRUE)) ~ "Grassland",     
      str_detect(Crop_UNFAO, regex("Pulses|Niebe|Cow pea", ignore_case=TRUE)) ~ "Pulses",     
      TRUE ~ Crop_UNFAO))
  return(df)      
}

#Create function to convert UN aggregate crop names into final IFA aggregate crop names ----
#Function to convert 2022 UNFAO aggregate crop names into final IFA crop categories for reporting.   Use this function after the'UNFAO_to_UNFAO_aggregated_crop_names' function. 
#You need to use a data frame (df) with a column that 
#has the UNFAO aggregate Crop information in one column. 
#eg: df <- UNFAO_aggregated_to_final_IFA_reporting_crop_names(df,df$Crop_UNFAO_aggregated)
#Based on: https://stackoverflow.com/questions/59082243/multiple-patterns-for-string-matching-using-case-when

#standardardised_ifa_crop_names
UNFAO_aggregated_to_final_IFA_reporting_crop_names <- function (df,Crop_UNFAO_aggregated){
  df <- df %>%
    mutate(Crop_for_final_IFA_reporting = case_when(
      str_detect(Crop_UNFAO_aggregated, regex("Wheat", ignore_case=TRUE)) ~ "Wheat",
      str_detect(Crop_UNFAO_aggregated, regex("Rice, paddy", ignore_case=TRUE)) ~ "Rice",
      str_detect(Crop_UNFAO_aggregated, regex("^Maize$", ignore_case=TRUE)) ~ "Maize",
      str_detect(Crop_UNFAO_aggregated, regex("^Maize, green$|Green", ignore_case=FALSE)) ~ "Maize green",
      str_detect(Crop_UNFAO_aggregated, regex("Other cereals|Barley|Sorghum|Millet|Oats", ignore_case=TRUE)) ~ "Other cereals",
      str_detect(Crop_UNFAO_aggregated, regex("Soybeans", ignore_case=TRUE)) ~ "Soybeans",
      str_detect(Crop_UNFAO_aggregated, regex("Oil palm", ignore_case=TRUE)) ~ "Oil palm",  
      str_detect(Crop_UNFAO_aggregated, regex("Rapeseed", ignore_case=TRUE)) ~ "Rapeseed", 
      str_detect(Crop_UNFAO_aggregated, regex("Other oil Crops", ignore_case=TRUE)) ~ "Other oil crops",     
      str_detect(Crop_UNFAO_aggregated, regex("Cottonseed", ignore_case=TRUE)) ~ "Cottonseed", 
      str_detect(Crop_UNFAO_aggregated, regex("Sugar beet", ignore_case=TRUE)) ~ "Sugar beet",  
      str_detect(Crop_UNFAO_aggregated, regex("Sugar cane", ignore_case=TRUE)) ~ "Sugar cane",
      str_detect(Crop_UNFAO_aggregated, regex("Tea", ignore_case=TRUE)) ~ "Tea",          
      str_detect(Crop_UNFAO_aggregated, regex("Coffee, green", ignore_case=TRUE)) ~ "Coffee green",
      str_detect(Crop_UNFAO_aggregated, regex("Roots and tubers nes|Cassava|Potato|Yam", ignore_case=TRUE)) ~ "Roots/tubers",         
      str_detect(Crop_UNFAO_aggregated, regex("Fruit primary and citrus fruits and treenuts", ignore_case=TRUE)) ~ "Fruit primary and citrus fruits and treenuts",       
      str_detect(Crop_UNFAO_aggregated, regex("Vegetables, fresh nes|Tomato|Onion|Sweet corn", ignore_case=TRUE)) ~ "Vegetables",     
      str_detect(Crop_UNFAO_aggregated, regex("Grassland", ignore_case=TRUE)) ~ "Grassland",     
      str_detect(Crop_UNFAO_aggregated, regex("Pulses|^Beans$", ignore_case=FALSE)) ~ "Pulses",     
      TRUE ~ "Residual"))
  return(df)      
}

#Apply crop name conversion functions to the dataframe.---- 
df <- IFA_survey_to_UNFAO_crop_names(df,df$Crop)

df <- UNFAO_to_UNFAO_aggregated_crop_names(df,Crop_UNFAO)

df <- UNFAO_aggregated_to_final_IFA_reporting_crop_names(df,df$Crop_UNFAO_aggregated)

#Create separate data frame with unaggregated crop data before it is saved as a csv file.
FUBC_9_unaggregated_crop_data <- df

#Create csv file for this----
write.csv(df,"./results/FUBC_9_unaggregated_crop_data.csv",row.names= FALSE)

#Show how these data can be combined into aggregated file for inclusion with historic FUBC data. ----
#Create column with 1's that indicate FAO area should be used and 0 if FAO area is not to be used (instead use IFA survey area).
df$Use_FAO_area_from_IFA_survey_1_is_yes_NA_is_no <- ifelse(grepl("Yes",df$FAO_area_used_Yes_No),1,NA)

#Create column with 1's that indicate IFA survey area should be used and 0 if IFA survey area is not to be used (instead use FAO survey area).
df$Use_IFA_survey_area_1_is_yes_NA_is_no <- ifelse(grepl("No",df$FAO_area_used_Yes_No),1,NA)

#Create binary 1,0 columns to ensure only one area is used to estimate fertiliser application rate based on best estimate of area from IFA survey. 
df$Crop_area_ha_FAO <- df$FAO_area_ha*df$Use_FAO_area_from_IFA_survey_1_is_yes_NA_is_no
df$Crop_area_ha_IFA_survey <- df$IFA_area_ha*df$Use_IFA_survey_area_1_is_yes_NA_is_no

#Coalesce crop area into one column for final estimate of fertiliser application rate. 
df$Crop_area_ha_final <- coalesce(df$Crop_area_ha_FAO, df$Crop_area_ha_IFA_survey)

#Summarise results by Country, year and final aggregate crop category
df1 <- df %>%select(-Use_IFA_survey_area_1_is_yes_NA_is_no,
                    -Crop_area_ha_FAO,-Crop_area_ha_IFA_survey)%>%
  group_by(Original_country_name_in_FUBC_report,Country,ISO3_code,Year_for_FAO_area,Crop_for_final_IFA_reporting) %>% 
  summarise(
    across(where(is.numeric), sum,na.rm=TRUE)) 

#Standardise column names/order for final format. 
df1 <- mutate(df1,
              "Crop"=Crop_for_final_IFA_reporting,
              "Year"=Year_for_FAO_area,
              "FUBC_report_number"=9,
              "Year_FUBC_publication"=2022,
              "IFA_N_P2O5_K2O_t"=IFA_N_t+IFA_P2O5_t+IFA_K2O_t,
              "N_k_t"=IFA_N_t/1000,
              "P2O5_k_t"=IFA_P2O5_t/1000,
              "K2O_k_t"=IFA_K2O_t/1000,
              "N_P2O5_K2O_k_t"=N_k_t+P2O5_k_t+K2O_k_t,
              "N_rate_kg_ha" =NA,
              "P2O5_rate_kg_ha" =NA,
              "K2O_rate_kg_ha" =NA,
              "N_pc_fert"=NA,
              "P2O5_pc_fert"=NA,
              "K2O_pc_fert"=NA,
              "Aver_N_rate_kg_ha"=IFA_N_t*1000/Crop_area_ha_final,
              "Aver_P2O5_rate_kg_ha"=IFA_P2O5_t*1000/Crop_area_ha_final,
              "Aver_K2O_rate_kg_ha"=IFA_K2O_t*1000/Crop_area_ha_final,
              "Aver_N_P2O5_K2O_rate_kg_ha"=IFA_N_P2O5_K2O_t*1000/Crop_area_ha_final,
              "Crop_area_k_ha"=Crop_area_ha_final/1000)


df1<- df1 %>%
  mutate(Country= recode(Country, "Türkiye" = "Turkey")) #Change to Turkey for ease of use.

#Add IFA regions
df1 <- merge(df1,IFA_Regions, by.x=c("Country","ISO3_code"), by.y=c("Country","ISO3_code")) #Note that in the future if the countrycode package updates country names the IFA regions file will need to be updated accordingly as occurred in 2025 with Turkey, Vietnam, Czechnia, and Venezuela

#Select columns of interest in correct order. 
df1 <- select(df1,
              Original_country_name_in_FUBC_report,
              Country,
              ISO3_code,
              Region_IFA,
              Year,
              FUBC_report_number,
              Year_FUBC_publication,
              Crop,
              Crop_area_k_ha,
              N_k_t,
              P2O5_k_t,
              K2O_k_t,
              N_P2O5_K2O_k_t,
              N_rate_kg_ha,
              P2O5_rate_kg_ha,
              K2O_rate_kg_ha,
              N_pc_fert,
              P2O5_pc_fert,
              K2O_pc_fert,
              Aver_N_rate_kg_ha,
              Aver_P2O5_rate_kg_ha,
              Aver_K2O_rate_kg_ha,
              Aver_N_P2O5_K2O_rate_kg_ha)

#Remove rows that have zero crop areas. 
df1 <- df1[df1$Crop_area_k_ha != 0, ]

#Order rows by Country and crop
df1 <- arrange(df1,Country, Crop )
View(df1)
#Combine FUBC_9 with FUBC_1_to_8 data together in a combined file. 
FUBC_1_to_9_data <- rbind(FUBC_1_to_8_data,df1)

#Save as csv file. ----
write.csv(FUBC_1_to_9_data,"./results/FUBC_1_to_9_data.csv",row.names= FALSE)

#Create meta-data file for FUBC_1_to_9_data.csv file. ----
#Create dataframe with header names in separate rows. 
Meta_data_FUBC_1_to_9_data <- as.data.frame(colnames(FUBC_1_to_9_data))

#Add meta-data information to data frame
Meta_data_FUBC_1_to_9_data <- rename(Meta_data_FUBC_1_to_9_data, "Parameter"="colnames(FUBC_1_to_9_data)")
Meta_data_FUBC_1_to_9_data$Description <- c("Original name of country used in FUBC report","Country name based on official United Nations English name, with the exception that references to Belgium-Luxembourg were converted to 'Belgium', and China, Taiwan was converted to China, Taiwan",
                                            "The 3-letter ISO3 United Nations code to signify country or region. Note that China, Taiwan was given the TWN 3-letter code ",
                                            "Region, based on the International Fertilizer Association (IFA) list of aggregate countries and regions",
                                            "Year in which the data relates to. Year is in character format because in some reports the data relate to non-calendar years e.g. 1991/92, 1997-98. These therefore include a mixture of calendar and 'crop' years",
                                            "The fertilizer use by crop (FUBC) report number. This is a sequential number assigned to each report since they were first published",
                                            "The fertilizer use by crop (FUBC) report, year of publication",
                                            "Crop type, based on those originally reported in the fertilizer use by crop (FUBC) reports",
                                            "The total crop area which may include planted or harvested areas depending on what data the survey respondents had available",
                                            "Total nitrogen applied to total crop area","Total P2O5 applied to total crop area","Total K2O applied to total crop area",
                                            "Total nitrogen + P2O5 + K2O applied to total crop area",
                                            "Mean application rate of nitrogen to area of crop that actually received fertilizer (see N_pc_fert for percentage of crop area where N was applied).In some cases, due to rounding, the areas of crop, percentage of crop area fertilized and application rates may not align",
                                            "Mean application rate of P2O5 to area of crop that actually received fertilizer (see P2O5_pc_fert for percentage of crop area where P2O5 was applied).In some cases, due to rounding, the areas of crop, percentage of crop area fertilized and application rates may not align",
                                            "Mean application rate of K2O to area of crop that actually received fertilizer (see K2O_pc_fert for percentage of crop area where K2O was applied).In some cases, due to rounding, the areas of crop, percentage of crop area fertilized and application rates may not align",
                                            "Percentage of total crop area that received any nitrogen fertilizer",
                                            "Percentage of total crop area that received any P2O5 fertilizer",
                                            "Percentage of total crop area that received any K2O fertilizer",
                                            "Mean application rate of nitrogen across total crop area. In some cases, due to rounding, the areas of crop, percentage of crop area fertilized and application rates may not align",
                                            "Mean application rate of P2O5 across total crop area. In some cases, due to rounding, the areas of crop, percentage of crop area fertilized and application rates may not align",
                                            "Mean application rate of K2O across total crop area. In some cases, due to rounding, the areas of crop, percentage of crop area fertilized and application rates may not align",
                                            "Mean application rate of nitrogen +P2O5 +K2O across total crop area. In some cases, due to rounding, the areas of crop, percentage of crop area fertilized and application rates may not align")

Meta_data_FUBC_1_to_9_data$Format <- c("Character","Character","Character","Character","Character","Numeric","Numeric","Character",
                                       "Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric",
                                       "Numeric","Numeric","Numeric","Numeric","Numeric","Numeric")

Meta_data_FUBC_1_to_9_data$Units <- c("Character","Character","Character","Character","Character","Integer","Integer","Character",
                                      "Kilo hectares (1000* ha per year)","Kilo (*1000) metric tonnes of elemental nitrogen per year","Kilo (*1000) metric tonnes of P2O5 per year","Kilo (*1000) metric tonnes of K2O per year",
                                      "Kilo (*1000) metric tonnes of nitrogen + P2O5 + K2O per year",
                                      "Kilograms of nitrogen per hectare (kg N/ha/year)","Kilograms of P2O5 per hectare (kg P2O5/ha/year)","Kilograms of K2O per hectare (kg K2O/ha/year)",
                                      "Percent (%)","Percent (%)","Percent (%)",
                                      "Kilograms of nitrogen per hectare (kg N/ha/year)","Kilograms of P2O5 per hectare (kg P2O5/ha/year)","Kilograms of K2O per hectare (kg K2O/ha/year)",
                                      "Kilograms of nitrogen+P2O5+K2O per hectare (kg N+P2O5+K2O/ha/year)")

#Save as csv file. 
write.csv(Meta_data_FUBC_1_to_9_data,"./results/Meta_data_FUBC_1_to_9_data.csv",row.names= FALSE)

#Perform validation of FUBC_9 data through a comparison with IFASTAT total nutrient fertilizer consumption (per country) data which comes from another source (https://www.ifastat.org/databases/plant-nutrition)----
#Estimate total N+P2O5+K2O consumption per country based on IFASTAT data----
#Tidy IFASTAT_data 
#Rename headers
colnames(IFASTAT_data) <- c("Product","Country","Year","Consumption_1000_t")

#Delete first row as this is the column header information. 
IFASTAT_data <- as.data.frame(IFASTAT_data[-c(1),])

#Remove non UTF-8 characters
IFASTAT_data$Country  <- iconv(IFASTAT_data$Country , from = "ISO-8859-1", to = "UTF-8")

#Create columns with internationally recognized country names, codes and regions to match survey categories. 
# #Remove special characters from country names
# IFASTAT_data$Country <-gsub("[[:punct:]]", "",  IFASTAT_data$Country) #This overcomes UTF-8 error in countrycode function when it hits Cote de Ivoire special characters. 

#Change Belgium and Luxemburg to Belgium for simplicity. 
IFASTAT_data$Country <-gsub("Belgium and Luxemburg", "Belgium",  IFASTAT_data$Country)

#Convert Taiwan China to Taiwan
IFASTAT_data$Country <-gsub("Taiwan China", "Taiwan",  IFASTAT_data$Country)

#Add ISO3_code
IFASTAT_data$ISO3_code <- countrycode(IFASTAT_data$Country, 
                                      origin = 'country.name', destination = 'iso3c')

#Add Country name based on official United Nations English name.
IFASTAT_data$Country <- countrycode(IFASTAT_data$ISO3_code, 
                                    origin = 'iso3c', destination = 'un.name.en')


#Filter to only rows with full data (no NAs) and delete country column.
IFASTAT_data <- IFASTAT_data %>%  na.omit() %>% select(-Country)

#Convert Consumption to numeric for pivot wider
IFASTAT_data$Consumption_1000_t <- as.numeric(IFASTAT_data$Consumption_1000_t)

IFASTAT_data <- IFASTAT_data  %>% 
  tidyr::pivot_wider(
    names_from = Product, 
    values_from = Consumption_1000_t,
    values_fn = list(Consumption_1000_t = sum)
  )

#Add column with ISO3_code and year so that they can be matched by country and year to FUBC data. 
IFASTAT_data$ISO3_code_Year <- paste(IFASTAT_data$ISO3_code,"_",IFASTAT_data$Year)

#Estimate total N+P2O5+K2O consumption per country based on FUBC_9 data (where crops are not aggregated)----
#Create data frame with equivalent data using data from the latest survey with original crop names. 
df <- select(df,ISO3_code,Crop,Year_for_FAO_area, Crop_area_ha_final,IFA_N_t,IFA_P2O5_t,IFA_K2O_t)

#Convert into same units as IFASTAT data. 
df$FUBC_Grand_Total_N <- df$IFA_N_t/1000 
df$FUBC_Grand_Total_P2O5 <- df$IFA_P2O5_t/1000
df$FUBC_Grand_Total_K2O <- df$IFA_K2O_t/1000
df$FUBC_Grand_Total_N_P2O5_K2O <- df$FUBC_Grand_Total_N+df$FUBC_Grand_Total_P2O5+df$FUBC_Grand_Total_K2O 

#Add column with ISO3_code and year so that they can be matched by country and year to IFASTAT data. 
df$ISO3_code_Year <- paste(df$ISO3_code,"_",df$Year_for_FAO_area)

#Get the sum of each nutrient for each ISO3_code_Year 
df1 <- df %>% select(-Year_for_FAO_area,-Crop_area_ha_final,-IFA_N_t,-IFA_P2O5_t,-IFA_K2O_t) %>% 
  group_by(ISO3_code_Year) %>% 
  summarise(
    across(where(is.numeric), sum,na.rm=TRUE)) 

#Merge datasets together
df2 <- merge(IFASTAT_data, df1)

#Rename columns for analysis
df2 <- rename(df2,
              "IFASTAT_N_1000_t"="Grand Total N",
              "IFASTAT_P2O5_1000_t"="Grand Total P2O5",
              "IFASTAT_K2O_1000_t"="Grand Total K2O",
              "IFASTAT_N_P2O5_K2O_1000_t"="Total N + P2O5 + K2O",
              "FUBC_N_1000_t"="FUBC_Grand_Total_N",
              "FUBC_P2O5_1000_t"="FUBC_Grand_Total_P2O5",
              "FUBC_K2O_1000_t"="FUBC_Grand_Total_K2O",
              "FUBC_N_P2O5_K2O_1000_t"="FUBC_Grand_Total_N_P2O5_K2O")

df2 <- mutate(df2, 
              "Difference_N_1000_t"=FUBC_N_1000_t-IFASTAT_N_1000_t, 
              "Difference_P2O5_1000_t"=FUBC_P2O5_1000_t-IFASTAT_P2O5_1000_t, 
              "Difference_K2O_1000_t"=FUBC_K2O_1000_t-IFASTAT_K2O_1000_t, 
              "Difference_N_P2O5_K2O_1000_t"=FUBC_N_P2O5_K2O_1000_t-IFASTAT_N_P2O5_K2O_1000_t,
              "pc_difference_N"=Difference_N_1000_t/IFASTAT_N_1000_t*100,
              "pc_difference_P2O5"=Difference_P2O5_1000_t/IFASTAT_P2O5_1000_t*100,
              "pc_difference_K2O"=Difference_K2O_1000_t/IFASTAT_K2O_1000_t*100,
              "pc_difference_N_P2O5_K2O"=Difference_N_P2O5_K2O_1000_t/IFASTAT_N_P2O5_K2O_1000_t*100)

#Round to fewer decimal places
df2 <- df2 %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 0)))

#Add country names to each row based on ISO3_code
df2$Country <- countrycode(df2$ISO3_code, 
                           origin = 'iso3c', destination = 'un.name.en')

#Create dataframe for table
df3 <- select(df2,
              ISO3_code_Year,
              Country,
              FUBC_N_P2O5_K2O_1000_t,
              IFASTAT_N_P2O5_K2O_1000_t,
              pc_difference_N_P2O5_K2O,
              FUBC_N_1000_t,
              IFASTAT_N_1000_t,
              pc_difference_N,
              FUBC_P2O5_1000_t,
              IFASTAT_P2O5_1000_t,
              pc_difference_P2O5,
              FUBC_K2O_1000_t,
              IFASTAT_K2O_1000_t,
              pc_difference_K2O)

#Save as csv file. 
write.csv(df3,"./results/FUBC_v_IFASTAT_comparison.csv",row.names= FALSE)

#Create meta-data file for comparison csv file. ----
#Create dataframe with header names in separate rows. 
Meta_data_FUBC_v_IFASTAT_comparison <- as.data.frame(colnames(df3))

#Add meta-data information to data frame
Meta_data_FUBC_v_IFASTAT_comparison<- rename(Meta_data_FUBC_v_IFASTAT_comparison, "Parameter"="colnames(df3)")
Meta_data_FUBC_v_IFASTAT_comparison$Description <- c( "The 3-letter ISO3 United Nations code to signify country or region, followed by the year that is applicable for that row of data",
                                                      "Country name based on official United Nations English name, with the exception that references to Belgium-Luxembourg were converted to 'Belgium', and China, Taiwan was converted to China, Taiwan",
                                                      "Fertilizer use by crop survey (number 9: FUBC 9) estimate of total nitrogen + P2O5 + K2O applied per country",
                                                      "International Fertilizer Association Statistics (IFASTAT) estimate of total nitrogen + P2O5 + K2O applied per country",
                                                      "Percentage difference in estimate of total nitrogen + P2O5 +K2O applied per country between FUBC 9 and IFASTAT (the % difference is relative to IFASTAT value)",
                                                      "Fertilizer use by crop survey (number 9: FUBC 9) estimate of total nitrogen applied per country",
                                                      "International Fertilizer Association Statistics (IFASTAT) estimate of total nitrogen applied per country",
                                                      "Percentage difference in estimate of total nitrogen applied per country between FUBC 9 and IFASTAT (the % difference is relative to IFASTAT value)",
                                                      "Fertilizer use by crop survey (number 9: FUBC 9) estimate of total P2O5 applied per country",
                                                      "International Fertilizer Association Statistics (IFASTAT) estimate of total P2O5 applied per country",
                                                      "Percentage difference in estimate of total P2O5 applied per country between FUBC 9 and IFASTAT (the % difference is relative to IFASTAT value)",
                                                      "Fertilizer use by crop survey (number 9: FUBC 9) estimate of total K2O applied per country",
                                                      "International Fertilizer Association Statistics (IFASTAT) estimate of total K2O applied per country",
                                                      "Percentage difference in estimate of total K2O applied per country between FUBC 9 and IFASTAT (the % difference is relative to IFASTAT value)")

Meta_data_FUBC_v_IFASTAT_comparison$Format <- c("Character","Character","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric","Numeric",
                                                "Numeric","Numeric","Numeric")

Meta_data_FUBC_v_IFASTAT_comparison$Units <- c("Character","Character",
                                               "Kilo (*1000) metric tonnes of nitrogen + P2O5 + K2O per year",
                                               "Kilo (*1000) metric tonnes of nitrogen + P2O5 + K2O per year",
                                               "Percentage",
                                               "Kilo (*1000) metric tonnes of nitrogen per year",
                                               "Kilo (*1000) metric tonnes of nitrogen per year",
                                               "Percentage",
                                               "Kilo (*1000) metric tonnes of P2O5 per year",
                                               "Kilo (*1000) metric tonnes of P2O5 per year",
                                               "Percentage",
                                               "Kilo (*1000) metric tonnes of K2O per year",
                                               "Kilo (*1000) metric tonnes of K2O per year",
                                               "Percentage")

#Save as csv file. 
write.csv(Meta_data_FUBC_v_IFASTAT_comparison,"./results/Meta_data_FUBC_v_IFASTAT_comparison.csv",row.names= FALSE)
