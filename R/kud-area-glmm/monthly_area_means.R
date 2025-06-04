


# ---- Load packages ----
{
  library(amt)
  library(dplyr)
  library(emmeans)
  library(ggplot2)
  library(ggspatial)
  library(glmmTMB)
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


# ----- bring in letters -----

sig_letter <- readr::read_csv(here::here("results",
                                          "kud-area",
                                          "area_sig_letters.csv")) %>%
  pivot_longer(-level,
               names_to = "month_abb",
               values_to = "letters") %>%
  mutate(
    level = factor(level)
  )


sig_letter

# ---- bring in data -----

kud <- qread(file =  here("data-saved",
                          "kud",
                          "monthly_for_each_id_kud_20_min_coa_.qs"))


kud <- kud %>%
  mutate(
    area_num = as.numeric(area),
    label = case_when(
      level == 0.5 ~ "50%",
      level == 0.8 ~ "80%",
      level == 0.95 ~ "95%",
    )
  )

glimpse(kud)


sig_letters <- sig_letter %>%
  left_join(kud %>%
  st_drop_geometry() %>%
  group_by(level, month_abb) %>%
  summarise(
    max_value = max(area_num) + 75
  ) %>%
  ungroup()
  ) %>%
  mutate(
    label = case_when(
      level == 0.5 ~ "50%",
      level == 0.8 ~ "80%",
      level == 0.95 ~ "95%",
    )
  )



# ----- plots -----
p <- ggplot(data = kud, aes(x = month_abb, y = area_num)) +
  geom_violin() +
  stat_summary(
    geom = "errorbar",
    fun.data = mean_se,
    width = 0.1
  ) +
  stat_summary(
    geom = "point",
    fun = mean,
    size = 3,
  ) +
  geom_text(data = sig_letters, aes(y = max_value, x = month_abb,
                                    label = letters)) +
  facet_wrap(~ label) +
  scale_y_continuous(breaks = seq(0, 2750, 250)) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank()
  ) +
  labs(
    y = expression(paste("Kernel Utilization Density Area (", km ^ 2, ")")),
    x = "Month"
  )
# p

ggsave(filename = here(
  "plots",
  "kde",
  "violins",
  "kde_area_violins.png"), plot = p,
  height = 8.5, width = 16
)
