# --- Read config and utility functions ----------------------------------------

source("code/utils.R")

# --- Read sample --------------------------------------------------------------

log_info("Loading accounting sample ...")

smp <- read_parquet(global_cfg$acc_sample)
if (interactive()) table(smp$fyear)

log_info("Preparing result outputs ...")

YEARS <- 2000:2024
smp <- smp %>% filter(fyear %in% YEARS)


if (interactive()) {
  datasummary(
    (`Return on Assets` = roa) + (`Return on Equity` = roe) ~ 
      (N + Mean + SD + Min + P25 + Median + P75 + Max), 
    data = smp
  )
  ggplot(smp, aes(x = roa)) + geom_histogram()
  ggplot(smp, aes(x = roe)) + geom_histogram()
}

smp[,"roa_w"] <- treat_outliers(smp[, "roa"])
smp[,"roe_w"] <- treat_outliers(smp[, "roe"])

if (interactive()) {
  datasummary(
    (`Return on Assets` = roa_w) + (`Return on Equity` = roe_w) ~ 
      (N + Mean + SD + Min + P25 + Median + P75 + Max), 
    data = smp
  )
  
  ggplot(smp, aes(x = roa_w)) + geom_histogram()
  ggplot(smp, aes(x = roe_w)) + geom_histogram()
  
  ggplot(smp, aes(x = roe_w, fill = (seq > 0))) + geom_histogram()
}

smp <- smp %>%
  mutate(
    roe_w = ifelse(seq <= 0, NA, roe_w)
  )

if (interactive()) {
  ggplot(smp, aes(x = fyear, group = fyear, y = roa_w)) +
    geom_boxplot(outliers = FALSE) + theme_minimal()
  ggplot(smp, aes(x = fyear, group = fyear, y = roe_w)) +
    geom_boxplot(outliers = FALSE) + theme_minimal()
}

plot_by_year <- function(df, var, pt_size = 2) {
  ggplot(df, aes(x = fyear, y = {{var}})) +
    stat_summary(
      fun.data = function (x) {
        q <- quantile(x, c(.25, .5, .75), na.rm = TRUE)
        data.frame(y = q[2], ymin = q[1], ymax = q[3])
      },
      geom = "linerange", color = "lightgray"
    ) +
    stat_summary(fun = median, geom = "line", linewidth = 0.5) +
    stat_summary(fun = median, geom = "point", size = pt_size, color = "red") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    labs(
      x = "", y = "", 
      caption = glue(
        "Yearly medians, line ranges reflect first and third quartiles."
      )
    ) 
}

fig_us_profits <- plot_by_year(smp, roa_w) +
  labs(
    title = "Return on assets, U.S. publicly-listed firms", 
    subtitle = glue(
      "({format(nrow(smp), big.mark = ',')} observations, ",
      "{format(length(unique(smp$gvkey)), big.mark = ',')} firms)"
    )
  ) +
  theme(plot.margin = margin(10, 20, 10, 10))

ggsave("output/us_profits.svg")
if (interactive()) print(fig_us_profits)

balanced_smp <- smp %>% 
  filter(fyear %in% YEARS) %>%
  group_by(gvkey) %>%
  filter(n() == length(YEARS)) %>%
  ungroup()
  
fig_us_profits_balanced <- plot_by_year(balanced_smp, roa_w) + 
  labs(
    title = "Return on assets, U.S. publicly-listed firms", 
    subtitle = glue(
      "(balanced sample, ",
      "{format(nrow(balanced_smp), big.mark = ',')} observations, ",
      "{format(length(unique(balanced_smp$gvkey)), big.mark = ',')} firms)"
    )
  ) +
  theme(plot.margin = margin(10, 20, 10, 10))

ggsave("output/us_profits_balanced.svg")
if (interactive()) print(fig_us_profits_balanced)

inds <- unique(smp$ff12_ind) %>% sort()
inds_shrt <- inds
inds_shrt[2] <- "Chemicals"
inds_shrt[6] <- "Healthcare"
inds_shrt[8] <- "Extractive Industries"
inds_shrt[10] <- "Communication"
inds_shrt[12] <- "Trade"
inds_n <- smp %>% 
  group_by(ff12_ind) %>% 
  summarise(n = n_distinct(gvkey), .groups = "drop") %>%
  arrange(ff12_ind) %>% pull(n)
inds_labels <- as.vector(
  glue("{inds_shrt}\n({format(inds_n, big.mark = ',', trim = T)}) firms")
)
names(inds_labels) <- inds

fig_us_profits_by_sector <- plot_by_year(smp, roa_w, pt_size = 1) + 
  facet_wrap(~ ff12_ind, labeller = labeller(ff12_ind = inds_labels)) + 
  labs(
    title = glue("Return on assets, U.S. publicly-listed firms")
  ) +
  theme(
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave("output/us_profits_by_sector.svg")

if (interactive()) {
  print(fig_us_profits_by_sector)
  
  lapply(
    inds, function(x) {
      df <- smp %>% filter(ff12_ind == x)
      plot_by_year(df, roa_w) + labs(
        title = glue("Return on assets, U.S. publicly-listed {x} panel"),
        subtitle = glue(
          "({format(length(unique(df$gvkey)), big.mark = ',')} firms)"
        )
      )
    }
  )
}

log_info("done!")


