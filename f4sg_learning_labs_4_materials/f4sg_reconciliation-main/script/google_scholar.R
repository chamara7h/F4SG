library(tidyverse)
gspapers <- bind_rows(
  tibble(
    year = 1980:2022,
    count = c(1,0,0,1,3,2,0,1,2,2, #1980s
              0,2,2,1,0,1,2,1,1,1, #1990s
              1,5,4,3,5,8,14,14,39,38, #2000s
              19,17,24,36,43,38,44,71,71,108,#2010s
              120, 145, 193),
    search = "Hierarchical forecasting"
  ),
  tibble(
    year = 1980:2022,
    count = c(0,0,0,0,0,0,1,0,1,0, #1980s
              0,0,0,0,2,2,1,1,3,1, #1990s
              2,3,1,3,2,3,2,0,3,1, #2000s
              4,4,4,6,4,6,11,15,23,37,#2010s
              55, 92, 98),
    search = "Forecast reconciliation"
  )
)
gsplot <- gspapers %>%
  mutate(
    search = factor(search, levels = c(
      "Hierarchical forecasting",
      "Forecast reconciliation")
    )
  ) |>
  filter(year >= 1995) |>
  ggplot(aes(x = year, y = count, col = search)) +
  geom_line() + 
  ggtitle("Google Scholar items by year") +
  theme_minimal() +
  theme(legend.justification=c(0,1),legend.position=c(0,1),
        legend.background = element_rect(fill = "white", color = NA)) +
  guides(colour = guide_legend(title="Search term")) +
  scale_color_manual(values=c(`Forecast reconciliation` = "#d95f02",
                              `Hierarchical forecasting` = "#7570b3"))
ggsave("slides/img/gsplot.pdf", gsplot, width = 10, height = 4)
