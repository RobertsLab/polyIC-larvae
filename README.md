# PolyIC larval carryover project 

In this project, we exposed broodstock to PolyIC immersion right before spawning (2 h). This project tracks the offspring of these parents to test for immune priming parental effects. This is a brief overview of the project.    

## Methods overview 

### Spawning 

Oysters were spawned at Point Whitney using their production workflow. Spawning was conducting using pooled egg and sperm from Control and Treated broodstock using approximately equal number of parents contributing to each pool.  

### Larvae 

#### 0-3 days  

Larvae were reared in 10,000 L tanks (n=1 per treatment) at Point Whitney using their production workflow for the first three days. Larvae were fed (nano-pav and CC mix) and maintained using their production practices. At 3 days post fertilization, larvae were screened at 60 um and sampled for RNA/DNA. Larvae were then distributed into 1,000 L tanks (n=2-3 per treatment). 

#### 3-8 days 

At 8 days post fertilization, larvae were dropped from each tank and screened onto 105 um screens and sampled for RNA/DNA. Growth rates and survivorship (>90%) for all larval groups were high. Food was increased as larvae grew. Larvae were returned to tanks (n=2-3 per treatment). 

#### 8-14 days 

At 14 days post fertilization, larvae were dropped from each tank and screened onto 240 um screens and sampled for RNA/DNA. Growth rates and survivorship (>95%) for all larval groups were high! Larvae were prepared for shipment to Kona facility for setting in Hawaii and shipped. 

### Setting 

Setting was conducted in Kona and treatment groups were kept separate. Larave were kept in Kona until 22 November 2024 when they were shipped back to Point Whitney. Setting was very high (>20-25%). Spat were then allocated to n=3 upwellers in the indoor system at Point Whitney. Control spat were stocked with 0.9L of spat per upweller; Treated spat were stocked with 0.5-0.75 L of spat per upweller.  

On 4 December 2024, we evenly spread the density of spat in n=2 upwellers per treatment (0.9 L in all tanks). Two additional tanks contained the excess of control and treated mixed together. Spat were imaged for size.  

### Seed holding

Seed were held at Point Whitney Shellfish Hatchery until they were transported to University of Washington for experiments on 26 Feb 2025 and 22 Mar 2025. Seed were held in tanks with Instant Ocean seawater and fed with algal paste.  

### Responses measured

We performed the following measurements: 

- Oyster growth using size images during rearing 
- Metabolism measured via a resazurin assay (see [Huffmyer et al. 2026](https://peerj.com/articles/21542/#) for the method)
- Survival under a range of temperatures in laboratory assays 
- qPCR gene expression under and acute stress experiment 

See the manuscript for details on responses measured and statistical methods.  

## Repository Structure

```
polyIC-larvae/
├── data/                     # Raw and processed input data
│   ├── acute-resazurin/      # Plate reader files from acute resazurin assays
│   ├── environmental/        # Temperature logger files and hatchery measurements
│   ├── growth/               # Shell length measurements (CSV + JPEG images)
│   ├── outplant/             # Field outplant data organized by site
│   │   ├── baywater/
│   │   ├── point_whitney/
│   │   └── sequim/
│   ├── qPCR/                 # qPCR Cq values, CFX instrument files, and reports
│   ├── resazurin/            # Resazurin metabolic assay plate files and metadata
│   ├── rna-dna/              # RNA/DNA extraction records and sample metadata
│   └── survival/             # Acute thermal survival assay data
├── figures/                  # Output figures organized by analysis type
│   ├── environmental/
│   ├── growth/
│   ├── outplant/
│   ├── qPCR/
│   ├── resazurin/
│   └── survival/
├── output/                   # Derived data files and rendered analysis outputs
│   ├── qPCR/
│   └── resazurin/
├── protocols/                # Lab protocols
│   └── RNA-Extraction-Methods.docx
├── scripts/                  # Analysis scripts (R Markdown, Quarto, Python)
│   ├── hatchery_environment.Rmd
│   ├── loggers.Rmd
│   ├── growth/
│   │   ├── growth.Rmd
│   │   └── growth.qmd
│   ├── outplant/
│   │   ├── growth_SequimBay.Rmd
│   │   ├── oyster_measurement_analysis.py
│   │   └── requirements.txt
│   ├── qPCR/
│   │   └── 01.00-polyIC-qPCR-analysis.Rmd
│   ├── resazurin/
│   │   ├── resazurin-analysis-batch2.Rmd
│   │   └── resazurin-testing.Rmd
│   └── survival/
│       └── survival-analysis.Rmd
└── polyIC-larvae.Rproj       # RStudio project file
```

---

## Scripts

All R scripts should be run from the **project root directory** (i.e., with the working directory set to `polyIC-larvae/`). The easiest way to ensure this is to open `polyIC-larvae.Rproj` in RStudio before running any scripts. When knitting an R Markdown file, click the arrow next to the **Knit** button and select **"Knit Directory: Project Directory"**.

### Environmental Data

**`scripts/hatchery_environment.Rmd`**  
Reads daily hatchery water quality measurements (temperature, pH, salinity) from `data/environmental/daily-measurements.xlsx` and summarizes conditions by rearing period. Outputs a summary table.

**`scripts/loggers.Rmd`**  
Reads raw Hobo Tidbit temperature logger files from `data/environmental/loggers/` and joins them with logger metadata from `data/environmental/loggers.xlsx`. Plots temperature time series for three deployment contexts: Sequim Bay field outplant, Point Whitney hatchery tanks, and UW FTR incubation tanks. Saves figures to `figures/environmental/`.

*Key R packages:* `tidyverse`, `readxl`, `lubridate`, `ggplot2`, `seacarb`, `cowplot`

---

### Growth

**`scripts/growth/growth.Rmd`**  
Analyzes shell length data from `data/growth/growth.csv`. Plots oyster length over time by parental treatment (control vs. poly(I:C)-treated) with linear trend lines and mean ± SE summaries. Fits linear models per treatment and saves figures to `figures/growth/`. 

*Key R packages:* `tidyverse`, `ggplot2`, `readxl`, `lme4`, `lmerTest`, `emmeans`, `cowplot`

---

### Survival

**`scripts/survival/survival-analysis.Rmd`**  
Analyzes acute thermal survival assay data from `data/survival/survival_assays.csv`. Oysters from control and treated families were exposed to elevated temperatures; mortality was scored at 0, 5, and 24 hours. Uses binomial mixed-effects logistic regression (`glmer`) with tank and batch as random effects. Includes overdispersion and zero-inflation diagnostics (DHARMa), post-hoc comparisons (emmeans), and model-predicted mortality plots. Saves figures to `figures/survival/`.

*Key R packages:* `tidyverse`, `lme4`, `lmerTest`, `car`, `emmeans`, `DHARMa`, `ggeffects`, `ggplot2`

---

### Resazurin Metabolic Assays

**`scripts/resazurin/resazurin-analysis-batch2.Rmd`** (primary)  
Analyzes resazurin fluorescence data from plate reader `.txt` files in `data/resazurin/plate_files/batch2-seed/`. Calculates metabolic rates (area under the curve) as a proxy for metabolic activity, joins with size and survival metadata, and tests for effects of parental treatment and temperature. Saves figures to `figures/resazurin/` and derived data to `output/resazurin/`.

**`scripts/resazurin/resazurin-testing.Rmd`**  
Exploratory and method-development script for the resazurin assay pipeline. Used to evaluate and validate the plate reader workflow before batch analyses.

*Key R packages:* `tidyverse`, `readxl`, `ggplot2`, `lme4`, `lmerTest`, `emmeans`, `cowplot`

---

### qPCR Gene Expression

**`scripts/qPCR/01.00-polyIC-qPCR-analysis.Rmd`**  
Analyzes qPCR data from `data/qPCR/` to examine gene expression differences between control and treated families. Reads Cq values, calculates delta-delta-Ct fold changes, and tests for differential expression. Saves figures to `figures/qPCR/` and processed outputs to `output/qPCR/`.

*Key R packages:* `tidyverse`, `readxl`, `ggplot2`, `lme4`, `emmeans`

---

### Outplant

Outplant data are in progress and are not detailed in the manuscript.  

**`scripts/outplant/growth_SequimBay.Rmd`**  
Analyzes growth data from the Sequim Bay field outplant site in `data/outplant/sequim/`. Compares shell size between control and treated families over time at the field site. Saves figures to `figures/outplant/`.

*Key R packages:* `tidyverse`, `readxl`, `ggplot2`, `lme4`, `emmeans`

**`scripts/outplant/oyster_measurement_analysis.py`**  
Python script that uses computer vision (OpenCV) to automatically detect and measure oysters in JPEG images from `data/outplant/sequim/size/`. Extracts length and width (in mm) for each detected oyster, annotates images with bounding boxes, and saves measurements to a CSV file (`oyster_measurements.csv`) in the same directory. This is a **test script only for automated analyses**. 

---

## How to Run the R Scripts

1. **Open the project** — double-click `polyIC-larvae.Rproj` in RStudio. This sets the working directory to the project root automatically.

2. **Install required packages** — on first use, install any missing packages from the R console.

3. **Set knit directory** — before knitting any `.Rmd` file, click the arrow next to the **Knit** button and choose **"Knit Directory: Project Directory"**. This ensures that relative paths like `data/survival/survival_assays.csv` resolve correctly from the project root.

4. **Knit or run interactively** — either knit the whole document to HTML, or run chunks interactively in the RStudio console. All outputs (figures, derived CSVs) are written relative to the project root.

---

## Data Notes

- All data files use relative paths from the project root. Do not move individual scripts out of the repository without updating paths.
- Raw environmental logger files (`.xlsx`) are in `data/environmental/loggers/`. Logger serial numbers are mapped to experimental groups via `data/environmental/loggers.xlsx`.
- Growth images (`.jpeg`) in `data/growth/` are organized by date and tank. Measurements derived from these images are compiled in `data/growth/growth.csv`.
- Resazurin plate files are `.txt` exports from the FLx800 plate reader instrument.
- RNA/DNA extraction records and sample metadata are in `data/rna-dna/` and are used as inputs to the qPCR analysis.

---

## Protocols

`protocols/RNA-Extraction-Methods.docx` — detailed RNA extraction protocol used for larval and spat samples collected at 3, 8, and 14 days post-fertilization and at subsequent time points.  

Note that samples collected using this protocol are not measured in this manuscript - the qPCR sampels were extracted using protocols described in the manuscript.  

