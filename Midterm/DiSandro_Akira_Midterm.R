# MUSA5080 Midterm
# In this file, I'll just be exploring stuff before I put it in a more neat way in the .Rmd


# Set-up ----
{
  # load packages
  library(tidyr)
  library(dplyr)
  library(tidycensus)
  library(stargazer)
  library(kableExtra)
  library(sf)
  library(readr)
  library(spdep)
  library(caret)
  library(ckanr)
  library(FNN)
  library(mapview)
  library(grid)
  library(gridExtra)
  library(ggcorrplot) # plot correlation plot
  library(corrr)      # another way to plot correlation plot
  library(jtools)     # for regression model plots
  library(ggstance) # to support jtools plots
  library(ggpubr)    # plotting R^2 value on ggplot point scatter
  library(broom.mixed) # needed for effects plots
  
  options(scipen=999) # specifying that we don't want scientific notation
  options(tigris_class = "sf")
  
  # load functions from functions.R
  source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
  
  # set working directory
  setwd("~/Documents/MUSA5080")
  
  # define my color parlettes
  mypalette1 <- colorRampPalette(c("#fcb39f","#7a0728"))(5)
  
  # load API key
  api <- read_file("~/Documents/MUSA5080/api_key.txt") %>% 
    gsub("[^[:alnum:] ]", "", .)
  census_api_key(api, overwrite = TRUE, install = T)
  
  # load data
  # studentData = dataset we will be testing our models on
  studentData <- st_read("https://raw.githubusercontent.com/mafichman/musa_5080_2023/main/Midterm/data/2023/studentData.geojson")
  # 99.6% of this data is for modeling, the rest is for the challenge
  
  # source of data to build model
  phillyData <- st_read("https://opendata.arcgis.com/datasets/88e5bc291b834606bd49f6fd6dca226e_0.geojson")
  
  studentData_cols <- data.frame(names(studentData))
  phillyData_cols <- data.frame(names(phillyData))
  
  # see census tract variables available to me
  acs_variable_list.2021 <- load_variables(2021, #year
                                           "acs5", #five year ACS estimates
                                           cache = TRUE)
  
  # load census tract data
  tracts21 <- 
    get_acs(geography = "tract", 
            variables = c("B25026_001E","B02001_002E",
                          "B15001_050E","B15001_009E",
                          "B19013_001E","B25058_001E",
                          "B06012_002E"), 
            year=2021, state=42, county=101, 
            geometry=TRUE, output="wide") %>%
    st_transform('ESRI:102728') %>%
    rename(TotalPop = B25026_001E, 
           Whites = B02001_002E,
           FemaleBachelors = B15001_050E, 
           MaleBachelors = B15001_009E,
           MedHHInc = B19013_001E, 
           MedRent = B25058_001E,
           TotalPoverty = B06012_002E) %>%
    dplyr::select(-NAME, -starts_with("B")) %>%
    mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
           pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
           pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
           year = "2021") %>%
    dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 
}

# Exploring data ----
{
  # mapping a general price/sqft map might be nice to see spatial process
  {
    ppsqft <- studentData %>% dplyr::select(sale_price,total_area) %>% 
      drop_na() %>% 
      mutate(ppsqft_m = ifelse(sale_price != 0 & total_area != 0 & total_area != 1,sale_price/total_area, NA)) %>%  # there are some places where total_area == 1
      filter(!is.na(ppsqft_m)) # dropping where ppsqft_m is missing for cleaner map
    
    ggplot() +
      geom_sf(data = tracts21, color = "darkgrey") +
      geom_sf(data = ppsqft, aes(colour = q5(ppsqft_m)), 
              show.legend = "point", size = .75) +
      scale_colour_manual(values = mypalette1,
                          labels=qBr(ppsqft,"ppsqft_m"),
                          name="Quintile\nBreaks") +
      labs(title="Price Per Square Foot, Philly", # make sure to note that NAs were dropped for visualization purpose
           caption = "Figure xx") +
      mapTheme()
    
    # looking at this map, Philly's prices per square foot are definitely clustered
    # highest prices in university city, center city, fairmount, fishtown, spring garden, manayunk area
  }
  
  # understand variables, make sure i know whether they are num/cat
  
  # looking at unique values in each column of studentData
  {
    studentData_red <- studentData %>% dplyr::select(-geometry) %>% st_drop_geometry()
    names <- names(studentData_red)
    
    find_max <- c()
    
    for (name in names) {
      find_max <- c(find_max,length(unique(studentData_red[,name])))
    }
    
    max_uniq <- max(find_max,na.rm = T)
    
    uniquevalues <- data.frame(matrix(NA,nrow=max_uniq,ncol=length(names)))
    names(uniquevalues) <- names
    
    for (name in names) {
      uniq_name <- length(unique(studentData_red[,name]))
      if (uniq_name == max_uniq) {
        uniquevalues[,name] <- unique(studentData_red[,name])
      } else {
        uniquevalues[1:uniq_name,name] <- unique(studentData_red[,name])
      }
    }
  }
  
  ggplot(studentData, aes(x = sale_price)) +
    geom_histogram(bins = 1000)
  
  summary(studentData$sale_price)
  summary(studentData,na.rm=T)
  
  
  num.pred <- studentData %>% 
    dplyr::select(matches("depth|area|year_built$"))
  
  cat.pred <- studentData %>% 
    dplyr::select(matches("basements|building|category|central|garage|street_des|topogr|heater|zoning"))
  
  # using scatters to check for general idea of what might be a good predictor
  # or i can just use a correlation matrix to visualize this. might be less busy
  # then make scatter plots for predictors of interest to zoom in on what's happening
  # honestly i might just do both (cause scatters would be informative for weak predictors as well)
  
  # scatter of age & areas
  st_drop_geometry(studentData) %>% 
    dplyr::select(sale_price,total_livable_area, year_built, total_area) %>%
    filter(year_built > 0, total_livable_area > 0, total_area > 0,
           sale_price <= 4000000) %>% # for now look at places < $4mil
    gather(Variable, Value, -sale_price) %>% 
    ggplot(aes(Value, sale_price)) +
    geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
    facet_wrap(~Variable, ncol = 3, scales = "free") +
    labs(title = "Price as a function of continuous variables") +
    plotTheme()
  
  # bar plot of 
  st_drop_geometry(studentData) %>% 
    dplyr::select(sale_price, exterior_condition, fireplaces, frontage, garage_spaces,
                  basements, garage_type, interior_condition,central_air,
                  depth,number_of_bathrooms, number_of_bedrooms, number_of_rooms, number_stories) %>%
    mutate(number_stories = as.factor(number_stories)) %>%
    # dplyr::select(sale_price, exterior_condition, fireplaces,garage_spaces, 
    #               basements,interior_condition,central_air,number_of_bathrooms, number_of_bedrooms, number_stories) %>%
    # mutate(exterior_condition = as.factor(exterior_condition),
    #        interior_condition = as.factor(interior_condition),
    #        fireplaces = as.factor(fireplaces), # change this to have or not
    #        number_stories = as.factor(number_stories),
    #        garage_spaces = as.factor(garage_spaces),
    #        basements = as.factor(basements),
    #        central_air = as.factor(central_air),
    #        number_of_bedrooms = as.factor(number_of_bedrooms),
    #        number_of_bathrooms = as.factor(number_of_bathrooms)) %>%
    filter(sale_price <= 4000000) %>% # under $4mil again for now
    gather(Variable, Value, -sale_price) %>% 
    ggplot(aes(Value, sale_price)) +
    geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
    facet_wrap(~Variable, ncol = 5, scales = "free") +
    labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
    plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # weak postive association with year built
  # stronger positive association with both areas, but lots of clustering close to 0 for total_area
  # might be better to stick with livable area
  
  # mutate central_air to y/n
  # garage_spaces 2 or more
  # basements, what do the letters mean? change to y/n
  # num_bath 3 or more
  # num_bed
  # num_stories 1-2, 3, 4+ (3 levels?)
  
  col_tokeep <- c("sale_price","total_livable_area","year_built","central_air",
                  "garage_spaces","basements","num_bath","num_stories")
  
  data_formodel <- studentData %>% 
    mutate()
  
  
  # pick out some variables of interest
  
  
  
  
  
  
  
  
}
