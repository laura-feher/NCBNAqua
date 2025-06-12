# NCBN sites with names matching the SET DB
ncbn_wl_sites <- readRDS(here::here("data", "ncbn_wl_sites.Rds"))

ncbn_SET_sites <- ncbn_wl_sites %>%
  mutate(park_code = stringr::str_sub(Identifier, 1, 4),
         site_name = stringr::str_trim(stringr::str_replace(Name, '.+-(.+)', '\\1'), side = "left")) %>%
  mutate(SET_site_name = case_when(
    site_name == "Pope Bay Marsh" ~ "Marsh 5 (Pope Bay)",
    site_name == "Pine Tree Marsh" ~ "Pine Tree Study",
    site_name == "Valentines Marsh" ~ "Marsh 8 (Valentines)",
    site_name == "Tingles" ~ "Marsh 11",
    site_name == "Blackfish Creek Water levels" ~ "Blackfish Creek",
    site_name == "Duck Harbor" ~ "Duck Harbor",
    site_name %in% c("Nauset Marsh North", "Nauset Marsh South") ~ "Nauset Marsh",
    site_name == "Hatches Harbor Restricted" ~ "Hatches Harbor Inside Dyke Original SETs",
    site_name == "Hatches Harbor Unrestricted" ~ "Hatches Harbor Outside Dyke Original SETs",
    site_name == "Marsh 30" ~ "Marsh 30",
    site_name == "Marsh 19" ~ "Marsh 19",
    site_name == "Watch Hill Marsh" ~ "Watch Hill",
    site_name == "Great Gun Marsh" ~ "Great Gun",
    site_name == "Hospital Point Marsh" ~ "Hospital Point",
    site_name == "Sandy Hook" ~ "Sandy Hook",
    site_name == "Black Bank Marsh" ~ "Black Bank",
    site_name == "Big Egg Marsh" ~ "Big Egg Control",
    site_name == "Joco Marsh" ~ "JOCO REF",
    site_name == "Bass Harbor Marsh" ~ "Bass Harbor",
    site_name == "Thompson Island Marsh" ~ "Thompson Island",
    site_name == "Schoodic Peninsula Marsh" ~ "Schoodic",
    site_name == "Maine Coast Heritage Trust" ~ "Maine Coast Heritage"
  )) %>%
  add_row(
    Name = c("Cape Cod NS - Hatches Harbor Restricted", "Cape Cod NS - Hatches Harbor Unrestricted", "Gateway NRA - Big Egg Marsh", "Gateway NRA - Joco Marsh"),
    site_name = c("Hatches Harbor Restricted", "Hatches Harbor Unrestricted", "Big Egg Marsh", "Joco Marsh"),
    SET_site_name = c("Hatches Harbor Inside Dyke Deep RSETs", "Hatches Harbor Outside Dyke Deep RSETs", "Big Egg Spray-Restored", "JOCO"),
    park_code = c("CACO", "CACO", "GATE", "GATE")) %>%
  mutate(Identifier = case_when(SET_site_name == "Hatches Harbor Inside Dyke Deep RSETs" ~ ncbn_wl_sites$Identifier[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Restricted"],
                                SET_site_name == "Hatches Harbor Outside Dyke Deep RSETs" ~ ncbn_wl_sites$Identifier[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Unrestricted"],
                                SET_site_name == "Big Egg Spray-Restored" ~ ncbn_wl_sites$Identifier[ncbn_wl_sites$Name == "Gateway NRA - Big Egg Marsh"],
                                SET_site_name == "JOCO" ~ ncbn_wl_sites$Identifier[ncbn_wl_sites$Name == "Gateway NRA - Joco Marsh"],
                                T ~ Identifier),
         UniqueId = case_when(SET_site_name == "Hatches Harbor Inside Dyke Deep RSETs" ~ ncbn_wl_sites$UniqueId[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Restricted"],
                              SET_site_name == "Hatches Harbor Outside Dyke Deep RSETs" ~ ncbn_wl_sites$UniqueId[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Unrestricted"],
                              SET_site_name == "Big Egg Spray-Restored" ~ ncbn_wl_sites$UniqueId[ncbn_wl_sites$Name == "Gateway NRA - Big Egg Marsh"],
                              SET_site_name == "JOCO" ~ ncbn_wl_sites$UniqueId[ncbn_wl_sites$Name == "Gateway NRA - Joco Marsh"],
                              T ~ UniqueId),
         IsExternalLocation = case_when(SET_site_name == "Hatches Harbor Inside Dyke Deep RSETs" ~ ncbn_wl_sites$IsExternalLocation[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Restricted"],
                                        SET_site_name == "Hatches Harbor Outside Dyke Deep RSETs" ~ ncbn_wl_sites$IsExternalLocation[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Unrestricted"],
                                        SET_site_name == "Big Egg Spray-Restored" ~ ncbn_wl_sites$IsExternalLocation[ncbn_wl_sites$Name == "Gateway NRA - Big Egg Marsh"],
                                        SET_site_name == "JOCO" ~ ncbn_wl_sites$IsExternalLocation[ncbn_wl_sites$Name == "Gateway NRA - Joco Marsh"],
                                        T ~ IsExternalLocation),
         PrimaryFolder = case_when(SET_site_name == "Hatches Harbor Inside Dyke Deep RSETs" ~ ncbn_wl_sites$PrimaryFolder[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Restricted"],
                                   SET_site_name == "Hatches Harbor Outside Dyke Deep RSETs" ~ ncbn_wl_sites$PrimaryFolder[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Unrestricted"],
                                   SET_site_name == "Big Egg Spray-Restored" ~ ncbn_wl_sites$PrimaryFolder[ncbn_wl_sites$Name == "Gateway NRA - Big Egg Marsh"],
                                   SET_site_name == "JOCO" ~ ncbn_wl_sites$PrimaryFolder[ncbn_wl_sites$Name == "Gateway NRA - Joco Marsh"],
                                   T ~ PrimaryFolder),
         LastModified = case_when(SET_site_name == "Hatches Harbor Inside Dyke Deep RSETs" ~ ncbn_wl_sites$LastModified[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Restricted"],
                                  SET_site_name == "Hatches Harbor Outside Dyke Deep RSETs" ~ ncbn_wl_sites$LastModified[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Unrestricted"],
                                  SET_site_name == "Big Egg Spray-Restored" ~ ncbn_wl_sites$LastModified[ncbn_wl_sites$Name == "Gateway NRA - Big Egg Marsh"],
                                  SET_site_name == "JOCO" ~ ncbn_wl_sites$LastModified[ncbn_wl_sites$Name == "Gateway NRA - Joco Marsh"],
                                  T ~ LastModified),
         Publish = case_when(SET_site_name == "Hatches Harbor Inside Dyke Deep RSETs" ~ ncbn_wl_sites$Publish[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Restricted"],
                             SET_site_name == "Hatches Harbor Outside Dyke Deep RSETs" ~ ncbn_wl_sites$Publish[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Unrestricted"],
                             SET_site_name == "Big Egg Spray-Restored" ~ ncbn_wl_sites$Publish[ncbn_wl_sites$Name == "Gateway NRA - Big Egg Marsh"],
                             SET_site_name == "JOCO" ~ ncbn_wl_sites$Publish[ncbn_wl_sites$Name == "Gateway NRA - Joco Marsh"],
                             T ~ Publish),
         Tags = case_when(SET_site_name == "Hatches Harbor Inside Dyke Deep RSETs" ~ ncbn_wl_sites$Tags[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Restricted"],
                          SET_site_name == "Hatches Harbor Outside Dyke Deep RSETs" ~ ncbn_wl_sites$Tags[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Unrestricted"],
                          SET_site_name == "Big Egg Spray-Restored" ~ ncbn_wl_sites$Tags[ncbn_wl_sites$Name == "Gateway NRA - Big Egg Marsh"],
                          SET_site_name == "JOCO" ~ ncbn_wl_sites$Tags[ncbn_wl_sites$Name == "Gateway NRA - Joco Marsh"],
                          T ~ Tags),
         UtcOffset = case_when(SET_site_name == "Hatches Harbor Inside Dyke Deep RSETs" ~ ncbn_wl_sites$UtcOffset[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Restricted"],
                               SET_site_name == "Hatches Harbor Outside Dyke Deep RSETs" ~ ncbn_wl_sites$UtcOffset[ncbn_wl_sites$Name == "Cape Cod NS - Hatches Harbor Unrestricted"],
                               SET_site_name == "Big Egg Spray-Restored" ~ ncbn_wl_sites$UtcOffset[ncbn_wl_sites$Name == "Gateway NRA - Big Egg Marsh"],
                               SET_site_name == "JOCO" ~ ncbn_wl_sites$UtcOffset[ncbn_wl_sites$Name == "Gateway NRA - Joco Marsh"],
                               T ~ UtcOffset)
  ) %>%
  mutate(is_SET_site = if_else(!is.na(SET_site_name), TRUE, FALSE),
         has_navd88_wl = case_when(
           park_code == "ACAD" ~ FALSE
         )) %>%
  arrange(park_code) %>%
  filter(is_SET_site)
