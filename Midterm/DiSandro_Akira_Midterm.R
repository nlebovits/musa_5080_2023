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
  setwd("~/Documents/musa_5080_2023/Midterm/")
  
  # define my color palettes
  mypalette1 <- colorRampPalette(c("#fcb39f","#7a0728"))(5)
  
  # load API key
  api <- read_file("~/Documents/MUSA5080/api_key.txt") %>% 
    gsub("[^[:alnum:] ]", "", .)
  census_api_key(api, overwrite = TRUE, install = T)
  
  # load data
  # studentData = dataset we will be testing our models on
  studentData <- st_read("https://raw.githubusercontent.com/mafichman/musa_5080_2023/main/Midterm/data/2023/studentData.geojson") %>% 
    st_transform(st_crs(tracts21))
  # 99.6% of this data is for modeling, the rest is for the challenge
  
  # source of data to build model
  phillyData <- st_read("https://opendata.arcgis.com/datasets/88e5bc291b834606bd49f6fd6dca226e_0.geojson") %>% 
    st_transform(st_crs(tracts21))
  
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
    dplyr::select(TotalPop, MedHHInc, MedRent, pctWhite, pctBachelors, pctPoverty) 
  
  # neighborhood data
  nhoods <- st_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson', quiet = T) %>%
    st_transform(st_crs(tracts21)) %>%    # using the same crs as tracts21 (aka ESRI)
    select(mapname)
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
    mutate(pct_livable = total_livable_area/total_area) %>% 
    gather(Variable, Value, -sale_price) %>% 
    ggplot(aes(Value, sale_price)) +
    geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
    facet_wrap(~Variable, ncol = 2, scales = "free") +
    labs(title = "Price as a function of continuous variables") +
    plotTheme()
  
  # correlation plot
  numeric_vars <- 
    select_if(st_drop_geometry(studentData), is.numeric) %>% na.omit()
  
  ggcorrplot(
    round(cor(numeric_vars), 1), 
    p.mat = cor_pmat(numeric_vars),
    colors = c("#25CB10", "white", "#FA7800"),
    type="lower",
    insig = "blank") +  
    labs(title = "Correlation across numeric variables") 
  
  # positive correlation with total area, total livable area, frontage, num bath, num room, fireplaces, num bed
  # negative correlation with int and ext con (they're also very colinear)
  # total area colinear with frontage (and obvi livable area)
  # zip code and num rooms is negatively correlated
  
  # bar plot of studentData
  st_drop_geometry(studentData) %>% 
    dplyr::select(sale_price, exterior_condition, fireplaces, frontage, garage_spaces,
                  basements, garage_type, interior_condition,central_air,type_heater,
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
  
  # weak positive association with year built
  # stronger positive association with both areas, but lots of clustering close to 0 for total_area
  # might be better to stick with livable area
  # when restricting to total_livable_area <= total_area, total_livable_area > 0, total_area > 1, 
  #   pct_livable (total_livable_area/total_area) actually has a slight negative slope 
  #   aka as the ratio of livable area to total area increases, sale_price decreases
  #   maybe people value having extra space outside of livable spaces?
  
  # num_bath 3 or more
  # num_bed 1-3, 4+
  # num_stories 1-2, 3, 4+ (3 levels?)
  # num_rooms 3-6, 7-12, 13+
  # type_heater "D" has the highest price
  # exterior condition 1, 2-4, 5+
  # interior condition 0, 1-3, 4+
  
  col_tokeep <- c("sale_price","total_livable_area","year_built","central_air",
                  "garage_spaces","basements","num_bath","num_bed","num_stories")
  
  data_formodel <- studentData %>% 
    mutate(central_air_new = ifelse(central_air %in% c(1,"Y"), "Y", "N"),       # mutate central_air to y/n
           garage_spaces_new = ifelse(garage_spaces %in% 0:1,"0/1","2+"),       # mutate garage_spaces to 0-1 or 2 or more
           basements_new = ifelse(basements == 0, "N", "Y"),                    # mutate basements to y/n; still need to clarify what the letters mean
           num_bath = ifelse(number_of_bathrooms <= 3, "0-3",
                             ifelse(number_of_bathrooms > 3, "4+",number_of_bathrooms)),
           num_bed = ifelse(number_of_bedrooms <=3 | number_of_bedrooms == 31, "1-3","4+"),
           num_stories = ifelse(number_stories < 3, "1-2",
                               ifelse(number_stories == 3, "3",
                                      ifelse(number_stories > 3, "4+",number_stories))),
           num_rooms = ifelse(number_of_rooms < 7, "3-6",
                              ifelse(number_of_rooms %in% 7:12, "7-12",
                                     ifelse(number_of_rooms > 13, "13+", number_of_rooms))),
           type_heater_new = ifelse(type_heater == "D" | type_heater == 0 | type_heater == "",type_heater,
                                    ifelse(type_heater == "A" | type_heater == "E", "A/E","B/C/G/H"))) %>% 
    dplyr::select(sale_price, total_livable_area, year_built, central_air, central_air_new, garage_spaces, garage_spaces_new, 
                  basements, basements_new, num_bath, num_bed, num_stories, number_of_rooms, type_heater, type_heater_new,
                  exterior_condition, interior_condition, frontage, depth, fireplaces, number_of_bathrooms, number_of_bedrooms) %>% 
    filter(sale_price > 0)
  
  # 0 obs
  # data_fm_inOD <- data_formodel[phillyData,]
  
  data_formodel_nhood <- st_join(data_formodel, nhoods)
  
  data_formodel_nhood_phlacs <- st_join(data_formodel_nhood, tracts21)
  
  # set final data for modeling
  phldata_formodel <- data_formodel_nhood_phlacs
  
}


# modeling
{
  # OLS reg from book
  reg1 <- lm(sale_price ~ ., data = st_drop_geometry(phldata_formodel) %>% 
               dplyr::select(sale_price, total_livable_area, year_built, central_air_new, garage_spaces_new,
                             basements_new, number_of_bathrooms, exterior_condition, type_heater_new))
  summary(reg1)
  
  
  
  
  # testing models
  set.seed(17)
  
  inTrain <- createDataPartition(
    y = paste(phldata_formodel$garage_spaces_new, phldata_formodel$central_air_new, 
              phldata_formodel$basements_new, phldata_formodel$type_heater_new), 
    p = .60, list = FALSE)
  phl.training <- phldata_formodel[inTrain,] 
  phl.test <- phldata_formodel[-inTrain,]  
  
  reg1.training <- lm(sale_price ~ ., data = st_drop_geometry(phldata_formodel) %>% 
                        dplyr::select(sale_price, total_livable_area, year_built, central_air_new, garage_spaces_new,
                                      basements_new, number_of_bathrooms, exterior_condition, type_heater_new))
  
  phl.test <-
    phl.test %>%
    mutate(sale_price.Predict = predict(reg1.training, phl.test),
           sale_price.Error = sale_price.Predict - sale_price,
           sale_price.AbsError = abs(sale_price.Predict - sale_price),
           sale_price.APE = (abs(sale_price.Predict - sale_price)) / sale_price.Predict) # %>%
    # filter(SalePrice < 5000000)
  
  mean(phl.test$sale_price.AbsError, na.rm = T)
  
  mean(phl.test$sale_price.APE, na.rm = T)
  
  
  
  # cross validation
  fitControl <- trainControl(method = "cv", number = 100)
  set.seed(17)
  
  reg1.cv <- 
    train(sale_price ~ ., data = st_drop_geometry(phldata_formodel) %>% 
            dplyr::select(sale_price, total_livable_area, year_built, central_air_new, garage_spaces_new,
                          basements_new, number_of_bathrooms, exterior_condition, type_heater_new), 
          method = "lm", trControl = fitControl, na.action = na.pass)
  
  reg1.cv
  
  
}




