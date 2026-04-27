# RCT Summer 26: Example Descriptive Analysis

To understand what this code does try C&P the following into your favorite 
LLM.

Hi. I am a Master student attending a research-oriented course in the area of accounting wokring with data. My prof has provided us with some code for a simple descriptive analysis.

I understand the economics behind the task but struggle to make sense of the code files (I am cluless about R). I attach some files containing code below. Can you guide me through the fog and explain to me how they work step by step? Please understand that I am an absolute beginner in these things.

### File 1: pull_wrds_data.R

```
# ------------------------------------------------------------------------------
# Downloads WRDS data to local parquet files using a duckdb workflow
#
# See LICENSE file for licensing information.
# ------------------------------------------------------------------------------

# Good starting points to learn more about this workflow are
# - The support pages of WRDS (they also contain the data documentation)
# - The wonderful textbook by Ian Gow (https://iangow.github.io/far_book/),
#   in particular App. D and E

source("code/R/utils.R")
cfg <- read_config("config/pull_wrds_data_cfg.yaml")

# Downloading data from WRDS is resource intensive. So, by default,
# this code only downloads data if it is not available locally.
# You can set the `force_redownload` config variable to bypass this behavior

# Also the config file specifies which tables to download, the variables
# to keep and filters to apply. You can modify these as needed.

# The secrets file should contain your WRDS login data
secrets <- read_secrets()


# --- Some helper functions to connect to duckdb and WRDS ----------------------

connect_duckdb <- function(dbase_path = ":memory:") {
  dbConnect(
    duckdb::duckdb(), dbase_path
  )
}

shutdown_duckdb <- function(con) {
  dbDisconnect(con, shutdown = TRUE)
}

link_wrds_to_duckdb <- function(con) {
  rv <- dbExecute(
    con, sprintf(paste(
      "INSTALL postgres;",
      "LOAD postgres;",
      "SET pg_connection_limit=4;",
      "ATTACH '",
      "dbname=wrds host=wrds-pgdata.wharton.upenn.edu port=9737",
      "user=%s password=%s' AS wrds (TYPE postgres, READ_ONLY)"
    ), secrets$wrds_user, secrets$wrds_pwd)
  )
}

list_wrds_libs_and_tables <- function(con) {
  dbGetQuery(
    con, "SHOW ALL TABLES"
  )
}

query_wrds_to_parquet <- function(con, query, parquet_file, force = FALSE) {
  time_in <- Sys.time()
  if (file.exists(parquet_file) & ! force) {
    log_info(
      "Parquet file '{parquet_file}' exists. ",
      "Skipping it but updating its mtime. ",
      "Delete it if you want to re-download"
    )
    Sys.setFileTime(parquet_file, Sys.time())
    return(invisible())
  }
  rv <- dbExecute(
    con, glue_sql(
      "COPY ({query}) TO {parquet_file} (FORMAT 'parquet')",
      .con = con
    )
  )
  time_spent <- round(Sys.time() - time_in)
  log_info(
    "Query result saved to '{parquet_file}': ",
    "rows: {format(rv, big.mark = ',')}, ",
    "time spent: {as_hms(time_spent)}"
  )
}


# --- Downloading U.S. Compustat data ------------------------------------------

con <- connect_duckdb()
link_wrds_to_duckdb(con)
log_info("Linked WRDS to local Duck DB instance.")


dyn_vars <- cfg$dyn_vars
stat_vars <- cfg$stat_vars
cs_filter <- cfg$cs_filter

log_info("Pulling Compustat data")
query <- glue_sql(
  "select s.*, d.* from ",
  "(select {`stat_vars`*} from wrds.comp.company) s ",
  "join (select {`dyn_vars`*} from wrds.comp.funda ",
  paste0("where ", cs_filter, ") d "),
  "on (s.gvkey = d.gvkey)", .con = con, .literal = TRUE
)

query <- glue_sql(
  "select * from ",
  "(select {`stat_vars`*} from wrds.comp.company) ",
  "join (select {`dyn_vars`*} from wrds.comp.funda ",
  paste0("where ", cs_filter, ") "),
  "using (gvkey)", .con = con
)

query_wrds_to_parquet(
  con, query, global_cfg$cstat_us_parquet_file,
  force = cfg$force_redownload
)

shutdown_duckdb(con)
log_info("Disconnected from WRDS")
```


### File 2: prepare_data.R

```
# --- Read config and utility functions ----------------------------------------

source("code/R/utils.R")

# --- Prepare base sample ------------------------------------------------------

log_info("Preparing base sample ...")

ff12 <- read_csv(global_cfg$fama_french_12, col_types = cols())
ff48 <- read_csv(global_cfg$fama_french_48, col_types = cols())

base_sample <- read_parquet(global_cfg$cstat_us_parquet_file) %>%
  filter(
    indfmt == "INDL",
    fic == "USA",
    !is.na(at), at > 0
  ) 

base_sample %>%
  group_by(gvkey, fyear) %>%
  filter(n() > 1) -> dup_obs

if(nrow(dup_obs) > 0) stop(
  "Duplicate firm-year observations in Compustat data, stored in 'dup_obs'."
)

smp <- expand_grid(
  gvkey = unique(base_sample$gvkey),
  fyear = unique(base_sample$fyear)
) %>% left_join(base_sample, by = c("gvkey", "fyear")) %>%
  arrange(gvkey, fyear) %>%
  group_by(gvkey) %>%
  mutate(
    log_at = log(at),
    per = prcc_f/epspx, 
    mtb = (csho*prcc_f)/ceq,
    roe = ni/(0.5*(seq + lag(seq))),
    roa = ebit/(0.5*(at + lag(at)))
  ) %>%
  filter(!is.na(log_at), !is.na(roe), !is.na(roa)) %>%
  filter(!is.infinite(roe), !is.infinite(roa)) %>%
  mutate(sic = ifelse(!is.na(sich), sprintf("%04d", sich), sic)) %>%
  filter(!is.na(sic)) %>%
  left_join(ff48, by = "sic") %>%
  left_join(ff12, by = "sic") %>%
  filter(!is.na(ff48_ind) & !is.na(ff12_ind)) %>%
  select(
    gvkey, conm, ff12_ind, ff48_ind, fyear, at, log_at, seq, sale, 
    per, mtb, roe, roa
  ) %>%
  ungroup()

write_parquet(smp, global_cfg$acc_sample)
log_info("Final sample saved to '{global_cfg$acc_sample}'.")
```

### File 3: run_analysis.R

```
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
```

### File 4: utils.R

```
# ------------------------------------------------------------------------------
# Code that should be included in all R scripts. Reads global config,
# sets up logging, and provides utility functions.
#
# See LICENSE file for licensing information.
# ------------------------------------------------------------------------------


# --- Attach required R packages -----------------------------------------------

# This attaches all packages that are required by any parts of the code. While
# this technically is not required for each and any code file and might also
# cause issues by conflicting namespaces, we follow this approach so that we
# single consistent R session throughout our code base.

suppressWarnings(suppressPackageStartupMessages({
  library(logger)
  library(glue)
  library(dotenv)
  library(yaml)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(modelr)
  library(broom)
  library(lubridate)
  library(hms)
  library(arrow)
  library(duckdb)
  library(modelsummary)
  library(fixest)
  library(gt)
}))


# --- Reading configuration files ----------------------------------------------

read_config <- function(config_file) {
  read_yaml(config_file)
}

global_cfg <- read_config("config/global_cfg.yaml")

read_secrets <- function() {
  if (!file.exists(global_cfg$secrets_file)) {
    log_error("Secrets file '{global_cfg$secrets_file}' not found. Exiting.")
    stop(paste(
      "Please copy '_{global_cfg$secrets_file}' to '{global_cfg$secrets_file}'",
      "and edit it to contain your WRDS access data prior to running this code"
    ))
  }
  
  load_dot_env("secrets.env")
  list(
    wrds_user = Sys.getenv("WRDS_USERNAME"),
    wrds_pwd = Sys.getenv("WRDS_PASSWORD")
  )
}


# --- Setting up logging -------------------------------------------------------

if (!is.na(global_cfg$log_level) && global_cfg$log_level != "") {
  log_threshold(toupper(global_cfg$log_level))
}

if (
  !is.na(global_cfg$log_file) && global_cfg$log_file != "" &&
  tolower(global_cfg$log_file) != "stdout"
) {
  log_appender(appender_file(global_cfg$log_file))
}


# --- Utility functions --------------------------------------------------------

# --- Lend from ExPanDaR package
treat_outliers <- function(
    x, percentile = 0.01, truncate = FALSE, by = NULL, ...
) {
  treat_vector_outliers <- function(x, truncate, percentile, ...) {
    lim <- quantile(
      x, probs = c(percentile, 1 - percentile), na.rm = TRUE, ...
    )
    if (!truncate) {
      x[x < lim[1]] <- lim[1]
      x[x > lim[2]] <- lim[2]
    } else {
      x[x < lim[1]] <- NA
      x[x > lim[2]] <- NA
    }
    x
  }
  
  if (!is.data.frame(x)) stop("'x' needs to be a data frame.")
  lenx <- nrow(x)
  if (!is.numeric(percentile) || (length(percentile) != 1)) 
    stop("bad value for 'percentile': Needs to be a numeric scalar")
  if (percentile <= 0 | percentile >= 0.5) {
    stop("bad value for 'percentile': Needs to be > 0 and < 0.5")
  }
  if (length(truncate) != 1 || !is.logical(truncate)) 
    stop("bad value for 'truncate': Needs to be a logical scalar")
  if (!is.null(by)) {
    by <- as.vector(x[[by]])
    if (anyNA(by)) 
      stop("by vector contains NA values")
    if (length(by) != lenx) 
      stop("by vector number of rows differs from x")
  }
  df <- x
  x <- x[sapply(x, is.numeric)]
  if (!is.numeric(as.matrix(x))) 
    stop("bad value for 'x': needs to contain numeric vector or matrix")
  x <- do.call(
    data.frame, lapply(x, function(xv) replace(xv, !is.finite(xv), NA))
  )
  if (is.null(by)) {
    retx <- as.data.frame(
      lapply(
        x, function(vx) treat_vector_outliers(
          vx, truncate, percentile, ...
        )
      )
    )
  } else {
    old_order <- (1:lenx)[order(by)]
    retx <- do.call(
      rbind, 
      by(
        x, by, 
        function(mx) apply(mx, 2, function(vx) treat_vector_outliers(
          vx, truncate, percentile, ...
        ))
      )
    )
    retx <- as.data.frame(retx[order(old_order), ])
  }
  df[colnames(retx)] <- retx
  return(df)
}
```

### File 5: global_cfg.yaml

```
# Path to a file containing environment variables for sensitive information
# WRDS login info in our case
secrets_file: secrets.env

# Possible values: debug, info, warn, error, fatal, off
# The repo code only uses info and error levels so info is a good default
log_level: info

# Path to the log file - set to 'stdout' to log to console
log_file: stdout

# External data paths
fama_french_12: data/external/fama_french_12_industries.csv
fama_french_48: data/external/fama_french_48_industries.csv

# File names for output objects that are used in multiple code steps
cstat_us_parquet_file: data/pulled/cstat_us.parquet
acc_sample: data/generated/acc_sample.parquet

# Output objects
results: output/results.rda
presentation: output/presentation.qmd
```

### File 6: pull_wrds_data_cfg.yaml

```
force_redownload: false

dyn_vars:
    - gvkey
    - conm
    - cik
    - fyear
    - datadate
    - indfmt
    - sich
    - consol
    - popsrc
    - datafmt
    - curcd
    - curuscn
    - fyr
    - at
    - seq
    - ceq
    - csho
    - mkvalt
    - prcc_f
    - epspx
    - epspi
    - dvpd
    - exchg
    - ebit
    - ib
    - ibc
    - ni
    - oancf
    - ivncf
    - fincf
    - oiadp
    - pi
    - sale

stat_vars:
    - gvkey
    - loc
    - sic
    - spcindcd
    - ipodate
    - fic

cs_filter: consol='C' and (indfmt='INDL' or indfmt='FS') and datafmt='STD' and popsrc='D'
```


 