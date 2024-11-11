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

###
  