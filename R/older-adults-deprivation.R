# Older adults deprivation mapping
#
# TODO: Calculate IDAOPI * Older Adults (65+) Pop metric

library(BSol.mapR)
library(dplyr)

birmingham_lsoas <- read.csv("../data/LSOA11_to_LSOA21_to_LA_Lookup.csv") %>%
  filter(LAD22NM == "Birmingham") %>%
  pull(LSOA11CD)

# Load LSOA IMD values
Brum_IDAOPI <- read.csv("../data/imd2019lsoa.csv") %>%
  # Filter for Birmingham IDAOPI
  filter(
    Measurement == "Rank",
    `Indices.of.Deprivation` == "j. Income Deprivation Affecting Older People Index (IDAOPI)",
    FeatureCode %in% birmingham_lsoas
  ) %>%
  mutate(
    # Update column names
    LSOA11 = FeatureCode,
    IDAOPI = Value,
    # Calculate Birmingham-specific older adults deprivation rank
    Brum_IDAOPI_rank = rank(IDAOPI, ties.method = 'first'),
    Brum_IDAOPI_rank_perc = 100 * (1- Brum_IDAOPI_rank / max(Brum_IDAOPI_rank))
  ) %>%
  select(LSOA11, IDAOPI, Brum_IDAOPI_rank, Brum_IDAOPI_rank_perc)

# Plot map
map <- plot_map(
  Brum_IDAOPI,
  "Brum_IDAOPI_rank_perc",
  "LSOA11",
  area_name = "Birmingham",
  style = "cont",
  map_title = "Birmingham-specific Older Adults Deprivation Rank (IDAOPI)",
  breaks = c(0,25,50,75,100),
  labels = c("0% - Least Deprived", "25%","50%","75%","100% - Most Deprived")
)

# Show map
map

# Save map
save_map(map, "../output/Birmingham-IDAOPI.png")

############################################################
#           Metric = IDAOPI * (65+ Population)             #
############################################################

birmingham_lsoa11_to_lsoa21 <- read.csv("../data/LSOA11_to_LSOA21_to_LA_Lookup.csv") %>%
  filter(LAD22NM == "Birmingham") %>%
  mutate(
    LSOA11 = LSOA11CD, 
    LSOA21 = LSOA21CD) %>%
  select(LSOA11, LSOA21)

oa_pop <- readxl::read_excel("../data/census-brum-oa-lsoa.xlsx") %>%
  # Restrict to 65+
  filter(`Age (3 categories)` == "Aged 65 years and over") %>%
  # Update column names
  mutate(LSOA21 = `Lower layer Super Output Areas Code`) %>%
  select(LSOA21, Observation) %>% 
  # Join LSOA11 to LSOA21 lookup
  left_join(
    birmingham_lsoa11_to_lsoa21,
    by = join_by(LSOA21)
    ) 
metric <- oa_pop %>%
  # Calculate Population for each LSOA11
  group_by(LSOA11) %>%
  summarise(Observation = sum(Observation)) %>%
  # Join Birmingham IDAOPI
  left_join(
    Brum_IDAOPI,
    by = join_by(LSOA11)
  ) %>%
  mutate(
    OA_dep_metric = Brum_IDAOPI_rank_perc * Observation,
    OA_dep_metric_norm = OA_dep_metric / max(OA_dep_metric)
  )

# Plot map
metric_map <- plot_map(
  metric,
  "OA_dep_metric_norm",
  "LSOA11",
  area_name = "Birmingham",
  style = "cont",
  map_title = "Birmingham Older Adults Deprivation Metric (IDAOPI * 65+ Population)",
)

# Show map
metric_map

# Save map
save_map(metric_map, "../output/Birmingham-IDAOPI-metric.png")