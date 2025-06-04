
# ---- Load packages ----
{
  library(amt)
  library(dplyr)
  library(data.table)
  library(ggplot2)
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
                  "det-benthic-suspended",
                  "detection_benthic_suspended_summary_lwf.qs")

)

glimpse(det)


det_bs <- det %>%
  mutate(
    date = floor_date(detection_timestamp_est, "1 day")
  ) %>%
  group_by(tag_serial_number) %>%
  mutate(
    n_date = n_distinct(date)
  ) %>%
  ungroup()

det_bs %>%
  distinct(tag_serial_number, n_date) %>%
  arrange(n_date) %>%
  print(n = 46)


det_1 <- det_bs %>%
  filter(n_date > 36)


det_1 %>%
  distinct(tag_serial_number, n_date) %>%
  arrange(n_date) %>%
  print(n = 46)


# det_sel <- det_bs_1 %>%
#   group_by(tag_serial_number, doy, month_abb, benthic_susp) %>%
#   summarise(
#     n_b = n(),
#   ) %>%
#   ungroup() %>%
#   # mutate(
#   #     perc = n_b / n_tot
#   #   )  %>%
#   print(n = 25)


doy_select <- det_1 %>%
  distinct(doy, month_abb, month_no)

doy_select
month_doy <- doy_select %>%
  group_by(month_abb) %>%
  summarise(first = first(doy),
            last = last(doy)) %>%
  ungroup() %>%
  # mutate(
  #   month_abb = forcats::fct_relevel(month_abb, "Jan",
  #                                    "Feb", "Mar", "Apr", "May", "Jun",
  #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # ) %>%
  # arrange(month_abb) %>%
  # mutate(
  #   first = if_else(
  #     month_abb %in% "May", true = 123, false = first
  #   )
  # ) %>%
  .$first
month_doy <- if_else(month_doy == 322, true = 307,
                     false = month_doy)
doy_select %>%
  filter(doy %in% month_doy) %>%
  group_by(month_abb) %>%
  summarise() %>%
  # mutate(
  #   month_abb = forcats::fct_relevel(month_abb, "Jan",
  #                                    "Feb", "Mar", "Apr", "May", "Jun",
  #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # ) %>%
  # arrange(month_abb) %>%
  .$month_abb -> month_label
month_label



det_sel <- det_1 %>%
  group_by(tag_serial_number, doy, month_abb, benthic_susp) %>%
  summarise(
    n_b = n(),
  ) %>%
  ungroup() %>%
  # mutate(
  #     perc = n_b / n_tot
  #   )  %>%
  print(n = 25)



p3 <- ggplot(data = det_sel, aes(x = month_abb, y = n_b)) +
  geom_boxplot(aes(fill = benthic_susp)) +
  # geom_hline(yintercept = 0.5, linetype = 3) +
  scale_fill_viridis_d(option = "A",
                       begin = 0.1,
                       end = 0.9, alpha = 0.5,
                       direction = -1,
                       name = "Behaviour",
                       labels = c("Benthic",
                                  "Pelagic")) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    # legend.position = "inside",
    # legend.position.inside = c(0.85, 0.05),
    # axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Month",
    y = "Number of Occurances"

  )
p3


ggsave(here("plots",
            "benthic-plots",
            "benthic_pelagic_depth_summary_rec_ring.png"),
       width = 16, height = 9, plot = p3)

