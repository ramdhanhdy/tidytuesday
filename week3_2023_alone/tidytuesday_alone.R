library(tidyverse)
library(alone)
library(ggtext)
library(showtext)
library(patchwork)

# 📉 survival chart -------------------------------------------------------

# 🎨 fonts and palettes ---------------------------------------------------
pal1 <- c('#1B2624', '#956e62', '#4b3425', '#2e291b', '#575642')

txt <- "grey20"
line <- "grey80"
bg <- "white"
      
font_add_google("Karla", "karla")
showtext_auto()
ft <- "karla"

#data wrangling
df <- expand_grid(
  days_lasted = 0:max(survivalists$days_lasted),
  gender = unique(survivalists$gender)
) |>
  left_join(
    survivalists |>
      count(days_lasted, gender),
    by = c("days_lasted", "gender")
  ) |>
  left_join(
    survivalists |>
      count(gender, name = "N"),
    by = "gender"
  ) |>
  group_by(gender) |>
  mutate(
    n = replace_na(n, 0),
    n_lasted = N-cumsum(n),
    p = n_lasted/N
  )

# make plots
plot1 <- df |>
  ggplot(aes(days_lasted, p, colour = gender, fill = gender)) +
  geom_line() +
  scale_colour_manual(values = c("#CC6666", "#9999CC", "#66CC99")) +
  labs(
    x = "Days lasted",
    y = "Proportion remaining",
    colour = "Gender",
    fill = "Gender",
    title = "Survival curves",
    subtitle = "There is some evidence that, on average, women tend to survive longer than men"
  ) +
  theme_void() +

plot2 <- survivalists |>
  ggplot(aes(gender, days_lasted, colour = gender, fill = gender)) +
  geom_boxplot(alpha = 0.25) +
  geom_jitter(width = 0.2, pch = 1, size = 3) +
  scale_colour_manual(values = c("#CC6666", "#9999CC", "#66CC99")) +
  scale_fill_manual(values = c("#CC6666", "#9999CC", "#66CC99")) +
  labs(
    colour = "Gender",
    fill = "Gender"
  ) +
  coord_flip() +
  theme_void()

plot1 + inset_element(plot2, left=0, right=1, bottom=-0.7, top=-0.2)