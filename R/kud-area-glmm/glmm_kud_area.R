


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

# ---- bring in data -----

kud <- qread(file =  here("data-saved",
                          "kud",
                          "monthly_for_each_id_kud_20_min_coa_.qs"))


kud <- kud %>%
  mutate(
    area_num = as.numeric(area)
  )


# ---- loook at disturbiont of each -----
ggplot(data = kud, aes(x = area_num)) +
  geom_histogram() +
  facet_wrap(~level)

# ---- look at disitubiont overall ----
ggplot(data = kud, aes(x = area_num)) +
  geom_histogram()



fitdistrplus::descdist(kud$area_num)

glimpse(kud)
# looks gamma
# ----- model using glmmTMB ----

m <- kud %>%
  split(.$level) %>%
  map(~ glmmTMB(area_num ~ month_abb + (1 | id),
                family = Gamma(link = "log"),
                data = .x, REML = TRUE,
                control = glmmTMBControl(optimizer = optim,
                                         optArgs = list(method = "BFGS")))
  )

# ---- assess model fit ----
res <- m %>%
  map(~ .x %>%
        DHARMa::simulateResiduals())


res %>%
  map(~ plot(.x))


# ---- assess main effects -----
m %>%
  map(~ .x %>%
        car::Anova())

# ----

# multiple comparissions ----

multi_comp <- m %>%
  map(~ emmeans(.x, pairwise ~ month_abb,
                adjust = "Tukey", type = "response")
  )


contrast_effects <- multi_comp %>%
  map(~ contrast(.x, method = "pairwise", adjust = "bonferroni")
  )



#
#
#  contrast(multi_comp, method = "pairwise",
#                              adjust = "Tukey")

contrast_month <- contrast_effects %>%
  map(~ broom::tidy(.x) %>%
        clean_names() %>%
        arrange(adj_p_value, contrast)
  ) %>%
  bind_rows(.id = "levels") %>%
  separate_wider_delim(contrast, delim = " / ", names = c("month_a",
                                                           "month_b"),
                       cols_remove = FALSE)


contrast_month
#
# print(basin_season_contrast, n = 66)
#
#
# compares <- cld(
#   object = multi_comp,
#   adjust = "Tukey",
#   Letters = letters,
#   alpha = 0.05,
# )
# compares$.group

contrast_month %>%
  # filter(adj_p_value < 0.05) %>%

  openxlsx::write.xlsx(here::here("results",
                                  "kud-area",
                                  "glmm_compare_month.xlsx"))





