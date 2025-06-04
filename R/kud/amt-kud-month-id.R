# ---- Load packages ----
{
  library(amt)
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(glatos)
  library(ggtext)
  library(here)
  library(janitor)
  library(lubridate)
  library(purrr)
  library(qs)
  library(stringr)
  library(sf)
  library(tidyr)

}

# ---- bring in data -----

det <- qread(here("data-saved",
                  "coa",
                  "coa_20_min.qs"))

# ---- amt ----
lake_o_e <- st_read(here("shapefiles",
                         "lake-ontario",
                         "east-ext",
                         "east_lake_o_ext.shp")) %>%
  st_transform(crs = 4326)

lake_o <- st_read(here("shapefiles",
                       "lake-ontario",
                       "Shapefiles_LO",
                       "Thewatermask.shp")) %>%
  st_transform(crs = 4326)
# st_transform(crs = 32618)


rec_locs <- qread(here("BenthicModShare",
                       "eastern_lake_ontario_arrays_2021_plus_bq.qs"))




rec_loc_filter <- rec_locs %>%
  mutate(
    month_abb = month(deploy_date_time, label = TRUE, abbr = TRUE)
  ) %>%
  distinct(station, glatos_array, geometry,
           # month_abb
  ) %>%
  arrange(glatos_array) %>%
  print(n = 43) %>%
  filter(!(glatos_array %in% c("OSM", "OSW", "OSE", "OSJ", "OSA")))
# ---- crop to only have eastern lake o ----


eastern_lake_o <- st_crop(x = lake_o, y = lake_o_e)

ggplot() +
  geom_sf(data = eastern_lake_o) +
  geom_sf(data = rec_loc_filter, aes(colour = glatos_array))



mapview::mapview(rec_loc_filter)
eastern_lake_o <- st_transform(eastern_lake_o,
                               crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs")
eastern_lake_o
# ---- amt ----
glimpse(det)


# ---- det ---
det_split <- det %>%
  mutate(
    id_month = paste(tag_serial_number, month_abb, sep = "_")
  )



dt_filter <- det_split %>%
  group_by(id_month) %>%
  summarise(
    n_dist = n_distinct(x)
  ) %>%
  ungroup() %>%
  arrange(n_dist) %>%
  filter(n_dist < 5) %>%
  print(n = 317)


#
# df_split_kept <- discard(det_split, ~ any(nrow(unique(.x$x) < 4)))

det_split_update <- det_split %>%
  filter(!(id_month %in% dt_filter$id_month)
         # &
         #   !(id_month %in% c("1304911_Dec", "1304924_Jun",
         #   "1382389_Nov", "1382396_Dec", "1382398_Jul",
         #     "1382620_Nov", "1382622_Oct", "1510197_Jan",
         #   "1514390_Mar", "1514391_Nov", "1514392_Aug", "1514392_Jan",
         #   "1514392_Jun", "1514392_May", "1514393_Aug", "1514394_Dec"))
  )



det_split_update %>%
  split(.$id_month) %>%
  length()

dt_filters <- det_split_update %>%
  group_by(id_month) %>%
  summarise(
    n_dist = n_distinct(x)
  ) %>%
  ungroup() %>%
  arrange(n_dist) %>%
  print(n = 317)
# ---- make tracks ----

df1 <- make_track(det_split_update,
                  .x = x, .y = y, .t = time_bins,
                  # crs = 4326,
                  crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs",,
                  all_cols = TRUE

)
df1
glimpse(df1)
# str(df1)
# create monthly ----
dat_mcp <- df1 %>%
  split(.$id_month) %>%
  map(~ hr_mcp(.x, levels = c(0.5, 0.8, 0.95)))

# glimpse(dat_mcp)
# dat_mcp[[1]]$mcp$level
# ---- extract mcp for each month ----
t <- dat_mcp %>%
  imap(~ st_intersection(.x$mcp, eastern_lake_o, ))
# st_intersection(.x$mcp, eastern_lake_o))
# bind_rows(.id = "id_month")
mutate(
  level = factor(level, levels = c(0.5, 0.8, 0.95))
) %>%
  # split(.$month_abb)
  # ---- plot mcps for each
  dat_mcp %>%
  map( ~
         ggplot() +
         # geom_sf(data = eastern_lake_o) +
         geom_sf(data = .x$mcp, aes(colour = level),
                 fill = NA,
                 # alpha = 0.3
         ) +
         labs(
           title = unique(.x$month_abb)
         )
  )

# ---- kde ----

trast <- make_trast(df1, res = 4)  #
hs <- hr_kde_ref(df1)

# plot(trast)
test <- df1 %>%
  split(.$id_month) %>%
  map(~ hr_kde( .x,
                trast = trast,
                h = hs, levels = c(0.5, 0.8, 0.95)))
test
test %>%
  map(~ plot(.x, col = c("red", "blue")))
glimpse(test)
kde.href.contours <- test %>%
  imap( ~ hr_isopleths(.x)) %>%
  bind_rows(.id = "id_month") %>%
  # st_transform(crs = 32618) %>%
  st_intersection(eastern_lake_o) %>%
  tidyr::separate(id_month, sep = "_", into = c("id",
                                                "month_abb")) %>%
  mutate(
    month_abb = factor(month_abb, levels = month.abb),
    level = factor(level)
  )

kde.href.contours
qs::qsave(x = kde.href.contours, file =  here("data-saved",
                                              "kud",
                                              "monthly_for_each_id_kud_20_min_coa_.qs"))

# p <- ggplot() +
#   geom_sf(data = eastern_lake_o) +
#   geom_sf(data = rec_loc_filter, shape = 4) +
#   geom_sf(data = kde.href.contours, aes(fill = level)) +
#   facet_wrap(id ~ month_abb) +
#   scale_fill_viridis_d(
#     end = 0.95,
#     begin = 0.25,
#     option = "A", direction = -1,
#     name = "Level") +
#   theme_bw() +
#   theme(
#     panel.grid = element_blank(),
#     strip.background = element_blank()
#   )
#
# p
