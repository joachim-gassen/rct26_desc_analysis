# --- Read config and utility functions ----------------------------------------

source("code/utils.R")

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
