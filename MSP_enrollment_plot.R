library(tidyverse)
library(sf)

# Set file path as `directory`
directory <- ""
plot_data <- readRDS(paste0(directory, "/plot_data.RData"))

plot_enrollment_pre <- plot_data |> 
  filter(date == "2019-12-30")

plot_enrollment_post <- plot_data |> 
  filter(date == "2023-09-30")

map_data <- plot_enrollment_pre |> 
  left_join(plot_enrollment_post, 
            join_by(FIPS_STCO), 
            suffix = c("_pre", "_post")) |> 
# Creating variable of interest `change`: enrollment from pre- to post-policy
  mutate(change = total_msp_per_100k_post - total_msp_per_100k_pre) |> 
  select(county_of_beneficiary_pre, 
         geometry_pre, 
         change)

p <- ggplot(map_data) +
  geom_sf(aes(fill = change, geometry = geometry_pre)) +
  scale_fill_distiller(palette = "RdBu", direction = 1, labels = scales::comma) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  labs(title = "Change in Medicare Savings Programs\nEnrollment Rates, 12/2019 - 9/2023\n(per 100,000 Medicare beneficiaries)",
       fill = "Change in rate\n(per 100,000\nbeneficiaries)",
       caption = "Data sources: 5-year ACS (2015-2022) and 1-year ACS (2023), US Census Bureau;\nCMS Quarterly Enrollment Snapshot (06/2015-09/2023)")

ggsave(p,
       filename="MSP_enrollment_change_map.png",
       dpi = 320,
       width = 6,
       height = 4)

print(p)
