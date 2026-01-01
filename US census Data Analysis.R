#loading libraries
library("tidycensus")
library("tidyverse")
library("ggplot2")
library("tigris")
library("sf")
library("dplyr")
library("lwgeom")
# Define your Census API key and set it with census_api_key()
api_key <- "c1535c768adf3ad53bc76c4d9b013282f18f871a"
census_api_key(api_key, install)

# Check your API key
Sys.getenv("CENSUS_API_KEY")

# Obtain and view state median household income from the American Community Survey
state_income <- get_acs(geography = "state", variables = "B19013_001")

head(state_income)

# Get an ACS dataset for Census tracts in Texas by setting the state
tx_income <- get_acs(geography = "tract",
                     variables = "B19013_001",
                     state = "TX")

# Inspect the dataset
head(tx_income)

# Get an ACS dataset for Census tracts in Travis County, TX
travis_income <- get_acs(geography = "tract",
                         variables = "B19013_001", 
                         state = "TX",
                         county = "Travis")

# Inspect the dataset
head(travis_income)

# Supply custom variable names
travis_income2 <- get_acs(geography = "tract", 
                          variables = c(variable = "B19013_001"), 
                          state = "TX",
                          county = "Travis")

# Inspect the dataset
head(travis_income2)

# Return county data in wide format
or_wide <- get_acs(geography = "county", 
                   state = "OR",
                   variables = c(hhincome = "B19013_001", 
                                 medage = "B01002_001"), 
                   output = "wide")
# Compare output to the tidy format from previous exercises
head(or_wide)

# Create a scatterplot
plot(or_wide$hhincomeE, or_wide$medageE)

# Get variables from the ACS Data Profile
v16 <- load_variables(year = 2016,
                       dataset = "acs5",
                       cache = TRUE)

# Get variables from the ACS Data Profile
v16p <- load_variables(year = 2016,
                       dataset = "acs5/profile",
                       cache = TRUE)

# Set year and dataset to get variables from the 2000 Census SF3
v00 <- load_variables(year = 2000,
                      dataset = "sf3",
                      cache = TRUE)

# Filter for table B19001
filter(v16, str_detect(name, "B19001"))

# Use public transportation to search for related variables
filter(v16p, str_detect(label, fixed("public transportation", 
                                     ignore_case = TRUE)))

# Access the 1-year ACS  with the survey parameter
ne_income <- get_acs(geography = "state",
                     variables = "B19013_001", 
                     survey = "acs1", 
                     state = c("ME", "NH", "VT", "MA", 
                               "RI", "CT", "NY"))

# Set dot color and size
g_color <- ggplot(ne_income, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_point(color = "navy", size = 4)

# Format the x-axis labels
g_scale <- g_color + 
  scale_x_continuous(labels = scales::dollar) + 
  theme_minimal(base_size = 18) 

# Label your x-axis, y-axis, and title your chart
g_label <- g_scale + 
  labs(x = "2016 ACS estimate", 
       y = "", 
       title = "Median household income by state")

g_label

# Download table "B19001"
wa_income <- get_acs(geography = "county", 
                     state = "WA", 
                     table = "B19001")

# Check out the first few rows of wa_income
head(wa_income)

# Assign Census variables vector to race_vars 
race_vars <- c(White = "B03002_003", Black = "B03002_004", Native = "B03002_005", 
               Asian = "B03002_006", HIPI = "B03002_007", Hispanic = "B03002_012")

# Request a summary variable from the ACS
ca_race <- get_acs(geography = "county", 
                   state = "CA",
                   variables = race_vars, 
                   summary_var = "B03002_001")

# Calculate a new percentage column and check the result
ca_race_pct <- ca_race %>%
  mutate(pct = 100 * (estimate / summary_est))

head(ca_race_pct)

# Group the dataset and filter the estimate
ca_largest <- ca_race %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate)) 

head(ca_largest)

# Group the dataset and get a breakdown of the results
ca_largest %>% 
  group_by(GEOID) %>%
  tally()

# Use a tidy workflow to wrangle ACS data
wa_grouped <- wa_income %>%
  filter(variable != "B19001_001") %>%
  mutate(incgroup = case_when(
    variable < "B19001_008" ~ "below35k", 
    variable < "B19001_013" ~ "35kto75k", 
    TRUE ~ "above75k"
  )) %>%
  group_by(NAME, incgroup) %>%
  summarize(group_est = sum(estimate))

wa_grouped

# Map through ACS1 estimates to see how they change through the years
mi_cities <- map_df(2012:2016, function(x) {
  get_acs(geography = "place", 
          variables = c(totalpop = "B01003_001"), 
          state = "MI", 
          survey = "acs1", 
          year = x) %>%
    mutate(year = x)
})

mi_cities %>% arrange(NAME, year)

# Get data on elderly poverty by Census tract in Vermont
vt_eldpov <- get_acs( geography = "tract", 
                      variables = c(eldpovm = "B17001_016", 
                                    eldpovf = "B17001_030"), 
                      state = "VT")

vt_eldpov

# Identify rows with greater margins of error than their estimates
moe_check <- filter(vt_eldpov, moe > estimate)

# Check proportion of rows where the margin of error exceeds the estimate
nrow(moe_check) / nrow(vt_eldpov)

# Group the dataset and calculate a derived margin of error
vt_eldpov2 <- vt_eldpov %>%
  group_by(GEOID) %>%
  summarize(
    estmf = sum(estimate), 
    moemf = moe_sum( moe = moe, estimate = estimate)
  )

# Filter rows where newly-derived margin of error exceeds newly-derived estimate
moe_check2 <- filter(vt_eldpov2,  moemf > estmf)

# Check proportion of rows where margin of error exceeds estimate
nrow(moe_check2) / nrow(vt_eldpov2)

# Request median household income data
maine_inc <- get_acs(geography = "county", 
                     variables = c(hhincome = "B19013_001"), 
                     state = "ME") 

# Generate horizontal error bars with dots
ggplot(maine_inc, aes(x = estimate, y = NAME)) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point()

# Remove unnecessary content from the county's name
maine_inc2 <- maine_inc %>%
  mutate(NAME = str_replace(NAME, " County, Maine", ""))

# Build a margin of error plot incorporating your modifications
ggplot(maine_inc2, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_grey(base_size = 14) + 
  labs(title = "Median household income", 
       subtitle = "Counties in Maine", 
       x = "ACS estimate (bars represent margins of error)", 
       y = "") + 
  scale_x_continuous(labels = scales::dollar)

# Get a counties dataset for Colorado and plot it
co_counties <- counties(state = "CO")
plot(co_counties)

# Get a Census tracts dataset for Denver County, Colorado and plot it
denver_tracts <- tracts(state = "CO", county = "Denver")
plot(denver_tracts)

# Plot area water features for Lane County, Oregon
lane_water <- area_water(state = "OR", county = "Lane")
plot(lane_water)

# Plot primary & secondary roads for the state of New Hampshire
nh_roads <- primary_secondary_roads(state = "NH")
plot(nh_roads)

# Get a counties dataset for Michigan
mi_tiger <- counties("MI")

# Get the equivalent cartographic boundary shapefile
mi_cb <- counties("MI", cb = TRUE)

# Overlay the two on a plot to make a comparison
plot(mi_tiger)
plot(mi_cb, add = TRUE, border = "red")

#Get data from tigris as simple features
options(tigris_class = "sf")

# Get countries from Colorado and view the first few rows
colorado_sf <- counties("CO")
head(colorado_sf)

# Plot its geometry column
plot(colorado_sf$geometry)

# Set the cache directory
tigris_cache_dir("~/Desktop/Projects/SQL projects/github")

# Set the tigris_use_cache option
options(tigris_use_cache = TRUE)

# Check to see that you've modified the option correctly
getOption("tigris_use_cache")

# Get a historic Census tract shapefile from 1990 for Williamson County, Texas
williamson90 <- tracts(state = "TX", county = "Williamson", 
                       cb = TRUE, year = 1990)

# Compare with a current dataset for 2016
williamson16 <- tracts(state = "TX", county = "Williamson", 
                       cb = TRUE, year = 2016)

# Plot the geometry to compare the results                       
par(mfrow = c(1, 2))
plot(williamson90$geometry)
plot(williamson16$geometry)

# Get Census tract boundaries for Oregon and Washington
or_tracts <- tracts("OR", cb = TRUE)
wa_tracts <- tracts("WA", cb = TRUE)

# Check the tigris attributes of each object
attr(or_tracts, "tigris")
attr(wa_tracts, "tigris")

# Combine the datasets then plot the result
or_wa_tracts <- rbind_tigris(or_tracts, wa_tracts)
plot(or_wa_tracts$geometry)

# Generate a vector of state codes and assign to new_england
new_england <- c("ME", "NH", "VT", "MA")

# Iterate through the states and request tract data for state
ne_tracts <- map(new_england, function(x) {
  tracts(state = x, cb = TRUE)
}) %>%
  rbind_tigris()

plot(ne_tracts$geometry)

# Get dataset with geometry set to TRUE
orange_value <- get_acs(geography = "tract", state = "CA", 
                        county = "Orange", 
                        variables = "B25077_001", 
                        geometry = TRUE)

# Plot the estimate to view a map of the data
plot(orange_value["estimate"])

# Get an income dataset for Idaho by school district
idaho_income <- get_acs(geography ="school district (unified)", 
                        variables = "B19013_001", 
                        state = "ID")

# Get a school district dataset for Idaho
idaho_school <- school_districts(state = "ID", type = "unified", class = "sf")

# Join the income dataset to the boundaries dataset
id_school_joined <- left_join(idaho_school, idaho_income, by = "GEOID")

plot(id_school_joined["estimate"])

# Get a dataset of median home values from the 1-year ACS
state_value <- get_acs(geography = "state", 
                       variables = "B25077_001", 
                       survey = "acs1", 
                       geometry = TRUE, 
                       shift_geo = TRUE)

# Plot the dataset to view the shifted geometry
plot(state_value["estimate"])

marin_value <- get_acs(geography = "tract",
                       variables = "B25077_001",
                       state = "CA", 
                       county = "Marin", 
                       geometry = TRUE, 
                       year = 2022)

# Set the color guide to FALSE and add a subtitle and caption to your map
ggplot(marin_value, aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  scale_fill_viridis_c(labels = scales::dollar) +  
  scale_color_viridis_c(guide = FALSE) + 
  theme_minimal() + 
  coord_sf(crs = 26911, datum = NA) + 
  labs(title = "Median owner-occupied housing value by Census tract", 
       subtitle = "Marin County, California", 
       caption = "Data source: 2012-2016 ACS.\nData acquired with the R tidycensus package.", 
       fill = "ACS estimate")

# Generate point centers
centers <- st_centroid(state_value)

# Set size parameter and the size range
ggplot() + 
  geom_sf(data = state_value, fill = "white") + 
  geom_sf(data = centers, aes(size = estimate), shape = 21, 
          fill = "lightblue", alpha = 0.7, show.legend = "point") + 
  scale_size_continuous(range = c(1, 20))
######
# Pull race data for DC Census tracts (with geometry)
dc_race_raw <- get_acs(
  geography = "tract",
  state = "DC",
  variables = race_vars,
  year = 2022,
  geometry = TRUE
)

dc_race <- dc_race_raw %>%
  select(GEOID, NAME, variable, estimate, geometry) %>%
  rename(value = estimate) %>%
  group_by(GEOID) %>%
  mutate(
    # Compute total population per tract by summing race counts
    summary_value = sum(value[variable != "Total"], na.rm = TRUE),
    # Calculate percentage share per race
    percent = (value / summary_value) * 100
  ) %>%
  # Remove the total row; keep race-level data only
  filter(variable != "Total") %>%
  ungroup()


######
dc_dots <- map(c("White", "Black", "Hispanic", "Asian"), function(group) {
  dc_race %>%
    filter(variable == group) %>%
    st_sample(., size = .$value / 100) %>%
    st_sf() %>%
    mutate(group = group) 
}) %>%
  reduce(rbind) %>%
  group_by(group) %>%
  summarize()

# Filter the DC roads object for major roads only
dc_roads <- roads("DC", "District of Columbia") %>%
  filter(RTTYP %in% c("I", "S", "U"))

# Get an area water dataset for DC
dc_water <- area_water("DC", "District of Columbia")

# Get the boundary of DC
dc_boundary <- counties("DC", cb = TRUE)

# Plot your datasets and give your map an informative caption
ggplot() + 
  geom_sf(data = dc_boundary, color = NA, fill = "white") + 
  geom_sf(data = dc_dots, aes(color = group, fill = group), size = 0.1) + 
  geom_sf(data = dc_boundary, color = "lightblue", fill = "lightblue") + 
  geom_sf(data = dc_dots, color = "grey") + 
  coord_sf(crs = 26918, datum = NA) + 
  scale_color_brewer(palette = "Set1", guide = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The racial geography of Washington, DC", 
       subtitle = "2010 decennial U.S. Census", 
       fill = "", 
       caption = "1 dot = approximately 100 people.\nData acquired with the R tidycensus and tigris packages.")
