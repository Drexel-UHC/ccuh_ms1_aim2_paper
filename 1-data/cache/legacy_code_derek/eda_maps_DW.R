{
  library(tidyverse)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(ggthemes)
  library(RColorBrewer)
  library(lubridate)
  library(scales)
  library(ggspatial)
  library(ggmap)
}

###read in analysis dataset
dat_raw <- read_csv("data/RR_by_neighborhood.csv")

dat <- dat_raw %>%
  mutate(SALID1 = as.factor(str_sub(SALID, 1, 6)), #create SALID1 variable
         SALID2_5 = as.factor(SALID))

#data checks
unique(dat$SALID1)

dat %>%
  group_by(SALID1) %>%
  tally()

dat %>%
  tally()

###Maps of BH and Campinas

#Import L3 boundaries for Brazil
shp_L3_raw <- st_read("neighborhood_boundaries/no2_BR_L2_5_USETHIS.shp") %>%
  filter(SALID1 > 0) %>% #remove nuisance polygons
  st_transform(4326) #transform to WGS84

#Convert to sf form and select variables of interest
shp_L3_cln <- shp_L3_raw %>%
  st_as_sf() %>% #convert to sf
  mutate(SALID1 = as.factor(SALID1),
         SALID2 = as.factor(SALID2),
         SALID2_5 = as.factor(SALID2_5)) %>% #convert to factor
  dplyr::select(SALID1, SALID2, SALID2_5)

shp_L3_merge <- left_join(dat, shp_L3_cln) #merge RR data with neighborhood boundaries


#For mapping, collapsing min value of RR (P99 vs P75) to 1. conceptually, <1 means no observed elevated risk from extreme heat compared to moderate temps
shp_L3 <- shp_L3_merge %>%
  mutate(rrgt1 = ifelse(RR_S_P75 < 1, 1, RR_S_P75), #for smoothed RRs
         rrgt1_i = ifelse(RR_I_P75 < 1, 0, 1)) #for unsmoothed RRs

#Subset for BH and Campinas

shapes_camp <- filter(shp_L3, SALID1 == 102226) #Campinas
hist(shapes_camp$RR_S_P75)

shapes_bh <- filter(shp_L3, SALID1 == 102136) #Belo Horizonte
hist(shapes_bh$RR_S_P75)

#Plot neighborhood RR continuous, left censored
(plot_cont_bh <- ggplot() +
    geom_sf(data = shapes_bh, mapping = aes(fill = rrgt1, geometry = geometry), size = 0.1, color = alpha("black", alpha = 1/2)) +
    scale_fill_viridis_c(name = "Relative risk") +
    theme_void(base_size = 16) +
    ggtitle("Belo Horizonte: relative risk of extreme heat") +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0.15, .85),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10))
)

#ggsave("./Figures/RR_BH.jpg", plot_cont_bh, width = 8, height = 8, dpi = 300)

(plot_cont_camp <- ggplot() +
    geom_sf(data = shapes_camp, mapping = aes(fill = rrgt1, geometry = geometry), size = 0.1, color = alpha("black", alpha = 1/2)) +
    scale_fill_viridis_c(name = "Relative risk") +
    theme_void(base_size = 16) +
    #    annotation_scale(location = "br", width_hint = 0.3 ,
    #                     height = unit(0.2, "cm"), text_cex = 1) +
    ggtitle("Campinas: relative risk of extreme heat") +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0.01, .85),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10))
)

#ggsave("./Figures/RR_Camp.jpg", plot_cont_camp, width = 8, height = 8, dpi = 300)

###Maps of other neighborhood factors

be <- read_csv("./Data/dat_all_2023-08-01.csv") %>% #read in other L2.5 data
  mutate(SALID1 = as.factor(SALID1),
         SALID2 = as.factor(SALID2),
         SALID2_5 = as.factor(SALID2_5))

shp_L3_be <- left_join(shp_L3, be) #merge neighborhood modifiers with RR data

###NOTE: this includes neighborhood factors for L2.5 but also L1 levels
names(shp_L3_be)

#Subset for
shapes_camp <- filter(shp_L3_be, SALID1 == 102226) #Campinas

shapes_bh <- filter(shp_L3_be, SALID1 == 102136) #Belo Horizonte

####Education
(plot_educ_bh <- ggplot() +
    geom_sf(data = shapes_bh, mapping = aes(fill = CNSMINPR_L3, geometry = geometry), size = 0.1, color = alpha("black", alpha = 1/2)) +
    scale_fill_viridis_c(name = "% Primary School", direction = -1) +
    theme_void(base_size = 16) +
    ggtitle("Belo Horizonte: educational attainment") +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0.15, .85),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10))
)

#ggsave("./Figures/Educ_BH.jpg", plot_educ_bh, width = 8, height = 8, dpi = 300)

(plot_educ_camp <- ggplot() +
    geom_sf(data = shapes_camp, mapping = aes(fill = CNSMINPR_L3, geometry = geometry), size = 0.1, color = alpha("black", alpha = 1/2)) +
    scale_fill_viridis_c(name = "% Primary School", direction = -1) +
    theme_void(base_size = 16) +
    ggtitle("Campinas: educational attainment") +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0.01, .85),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10))
)

#ggsave("./Figures/Educ_camp.jpg", plot_educ_camp, width = 8, height = 8, dpi = 300)


####Greenspace

(plot_green_bh <- ggplot() +
    geom_sf(data = shapes_bh, mapping = aes(fill = BECGSPCTL3, geometry = geometry), size = 0.1, color = alpha("black", alpha = 1/2)) +
    scale_fill_viridis_c(name = "% Greenspace", direction = -1) +
    theme_void(base_size = 16) +
    ggtitle("Belo Horizonte: greenspace") +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0.15, .85),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10))
)

#ggsave("./Figures/green_BH.jpg", plot_green_bh, width = 8, height = 8, dpi = 300)

(plot_green_camp <- ggplot() +
    geom_sf(data = shapes_camp, mapping = aes(fill = BECGSPCTL3, geometry = geometry), size = 0.1, color = alpha("black", alpha = 1/2)) +
    scale_fill_viridis_c(name = "% Greenspace", direction = -1) +
    theme_void(base_size = 16) +
    ggtitle("Campinas: % Greenspace") +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0.01, .85),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10))
)

#ggsave("./Figures/green_camp.jpg", plot_green_camp, width = 8, height = 8, dpi = 300)



# Code for aggregating 65+ ------------------------------------------------

# Read base data set
df_age <- read_csv("data/dat_all_2023-08-01.csv")
#df2 <- read_csv("data/SALURBAL_cities_Brazil.csv")

# Extract all potential age categories (separate to under/over 65)
age_categories <- str_subset(names(df_age), "CNSAGE")
under_65 <- age_categories[1:13]
over_65  <- age_categories[14:21]

# Specify percentage of population over 65 for each L2 neiborhood
df_age <- df_age |> 
  mutate(n_under_65 = rowSums(pick(all_of(under_65)), na.rm = TRUE),
         n_over_65  = rowSums(pick(all_of(over_65)),  na.rm = TRUE),
         n_total = n_under_65 + n_over_65) |> 
  #filter(n_total != 0) |> 
  summarize(n_over_65 = sum(n_over_65),
            n_total = sum(n_total), .by = SALID2) |> 
  mutate(percent_over_65 = n_over_65/n_total)
