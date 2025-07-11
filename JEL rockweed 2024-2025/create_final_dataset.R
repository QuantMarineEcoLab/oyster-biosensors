# combine temp, tide, WQ, morph data.


# ------ ADD MORPHOLOGICAL DATA -------
# Add in morphology data at the end.
morph_dat <- fread("~/Library/CloudStorage/OneDrive-USNH/Oyster Biosensor/0_InProgressExperiments/2024 JEL field/data/schema/JEL_field_data_morphology.csv")

morph_dat <- morph_dat %>%
  mutate(timepoint = ifelse(date == "2025-04-17", "final", "initial"))

# Make morph dat wide
morph_dat_wide <- pivot_wider(data = morph_dat, id_cols = c(oyster_id),
                              names_from = (timepoint), 
                              values_from = c(length_mm, width_mm, depth_mm, survival))

# Combine oyster data with morph dat
oyster_dat_full <- left_join(oyster_dat, morph_dat_wide, by = "oyster_id")
