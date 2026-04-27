RSCRIPT := Rscript --vanilla

CONFIG_GLOBAL := config/global_cfg.yaml 
CONFIG_WRDS := config/pull_wrds_data_cfg.yaml
PULLED := data/pulled/cstat_us.parquet
GENERATED := data/generated/acc_sample.parquet
RESULTS := output/us_profits.svg output/us_profits_balanced.svg \
	output/us_profits_by_sector.svg

.PHONY: all clean

all: $(RESULTS)

$(PULLED): code/pull_wrds_data.R code/utils.R secrets.env \
	$(CONFIG_GLOBAL) $(CONFIG_WRDS)
	mkdir -p data/pulled
	$(RSCRIPT) $<

$(GENERATED): code/prepare_data.R code/utils.R $(CONFIG_GLOBAL) $(PULLED) 
	mkdir -p data/generated
	$(RSCRIPT) $<

$(RESULTS) &: code/run_analysis.R code/utils.R $(CONFIG_GLOBAL) $(GENERATED)
	mkdir -p output
	$(RSCRIPT) $<

clean:
	rm -f $(GENERATED) $(RESULTS) 

dist-clean: clean
	rm -f $(PULLED)
