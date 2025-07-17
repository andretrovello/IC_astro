# Figure 11: Spectral Index Boxplots - CBC20 Paper Recreation
#
# This script recreates Figure 11 from the CBC20 paper, showing boxplots
# of scaled spectral index differences between stellar population synthesis models.
#
# The figure shows two effects in separate panels:
# - Synthetic effect: How synthetic vs empirical stellar libraries affect indices
# - Coverage effect: How stellar library coverage affects index predictions
#
# Indices are z-score normalized and sorted by median effect size for easy comparison.
#
# Author: Claude & Paula. Created for educational purposes.
# Date: 2025

# =============================================================================
# LOAD REQUIRED LIBRARIES
# =============================================================================

library(tidyverse)  # For data manipulation and plotting (includes stringr, forcats)
library(FITSio)     # For reading FITS files

# =============================================================================
# DATA READING FUNCTIONS
# =============================================================================

# Function to read spectral indices from FITS files (GALAXEV format)
read_bc_idx_fits <- function(myfile) {
    cat("Processing", myfile, "\n")
    
    # Extract metallicity from filename
    metal_value <- str_extract(myfile, "z[0-9]+")
    metal_value <- str_replace(metal_value, "z", "0.0")
    if (str_detect(myfile, "z0002")) metal_value <- "0.0002"
    if (str_detect(myfile, "z004")) metal_value <- "0.004"
    if (str_detect(myfile, "z008")) metal_value <- "0.008"
    if (str_detect(myfile, "z017")) metal_value <- "0.017"
    if (str_detect(myfile, "z030")) metal_value <- "0.030"
    
    # Read from specific HDUs in FITS file
    tmp1 <- readFrameFromFITS(myfile, hdu = 3) |>
        select(logage, B912:B4000)
    
    tmp2 <- readFrameFromFITS(myfile, hdu = 5) |>
        select(logage, CN1:BHHK)
    
    # Combine and reshape
    idx_tmp <- inner_join(tmp1, tmp2, by = "logage") |>
        add_column(Z = metal_value, .after = "logage") |>
        pivot_longer(cols = -c(logage, Z), names_to = "idx_name", values_to = "idx_value")
    
    # Apply age cut
    idx <- filter(idx_tmp, logage >= 7.0)
    return(idx)   
}

# Function to read CBC spectral indices from ASCII files
read_cbc_idx <- function(myfile) {
    cat("Processing", myfile, "\n")
    
    # Define available indices
    idx_list <- c("CN1", "CN2", "Ca4227", "G4300", "Fe4383", "Ca4455", "Fe4531", 
                  "C4668", "Hbeta", "Fe5015", "Mg1", "Mg2", "Mgb", "Fe5270", 
                  "Fe5335", "Fe5406", "Fe5709", "Fe5782", "NaD", "TiO1", "TiO2", 
                  "HdeltaA", "HgammaA", "HdeltaF", "HgammaF", "Ca8498", "Ca8542", 
                  "Ca8662", "Mg8807", "B4000", "D4000vn", "H83889", "H93835", 
                  "H103798", "CaHK")
    
    # Extract metallicity from filename
    mixtures <- c("m20p04", "m10p04", "m05p02", "p00p00", "p02p00")
    metals <- c("0.0002", "0.004", "0.008", "0.017", "0.030")
    metal_value <- metals[str_detect(myfile, mixtures)]
    
    # Read ASCII file
    idx <- read_table(myfile, skip = 2, 
                     col_names = c("isoname", "logage", idx_list),
                     show_col_types = FALSE) |> 
        select(-isoname) |>
        pivot_longer(cols = all_of(idx_list), names_to = "idx_name", values_to = "idx_value") |>
        add_column(Z = metal_value, .after = "logage")
    
    # Apply age cut
    idx <- filter(idx, logage >= 7.0)
    return(idx)   
}

# =============================================================================
# DATA LOADING AND PROCESSING
# =============================================================================

cat("=== Loading stellar population synthesis model indices ===\n")

# Define data directories
dir_pmiles <- "input/pmiles_models/"
dir_smiles <- "input/smiles_models/"
dir_cbc <- "input/cbc_milesres_models/"

# 1. Read MILES indices
cat("\n1. Loading MILES indices...\n")
pmiles_files <- list.files(dir_pmiles, pattern = "*.fits", full.names = TRUE)
pmiles_idx <- map_dfr(pmiles_files, read_bc_idx_fits) |>
    mutate(identifier = "pmiles")

# 2. Read SynMILES indices  
cat("\n2. Loading SynMILES indices...\n")
smiles_files <- list.files(dir_smiles, pattern = "*.fits", full.names = TRUE)
smiles_idx <- map_dfr(smiles_files, read_bc_idx_fits) |>
    mutate(identifier = "smiles")

# 3. Read CBC indices
cat("\n3. Loading CBC indices...\n")
cbc_files <- list.files(dir_cbc, pattern = "*lsindx*", full.names = TRUE)
cbc_idx <- map_dfr(cbc_files, read_cbc_idx) |>
    mutate(identifier = "cbc")

# Combine all data
indices_all <- bind_rows(pmiles_idx, smiles_idx, cbc_idx)

# Standardize index names
indices_all <- indices_all |>
    mutate(
        idx_name = case_when(
            idx_name == "C4668" ~ "Fe4668",   # Combine C4668 and Fe4668
            idx_name == "BHHK" ~ "CaHK",      # Combine BHHK and CaHK  
            TRUE ~ idx_name
        )
    )

# =============================================================================
# CREATE COMPARISON DATASETS
# =============================================================================

cat("\n=== Creating index comparison datasets ===\n")

# Separate by model type
pmiles_data <- filter(indices_all, identifier == "pmiles")
smiles_data <- filter(indices_all, identifier == "smiles")
cbc_data <- filter(indices_all, identifier == "cbc")

# Question 1: Synthetic Effect (SynMILES vs MILES)
synthetic_effect <- inner_join(
    smiles_data, pmiles_data,
    by = c("logage", "Z", "idx_name"), 
    suffix = c("_ref", "_comp")
) |>
    mutate(
        delta = idx_value_comp - idx_value_ref,
        question = "Synthetic effect"
    )

# Question 2: Coverage Effect (CBC vs SynMILES)
coverage_effect <- inner_join(
    smiles_data, cbc_data,
    by = c("logage", "Z", "idx_name"), 
    suffix = c("_ref", "_comp")
) |>
    mutate(
        delta = idx_value_comp - idx_value_ref,
        question = "Coverage effect"
    )

# Combine both comparisons
indices_combined <- bind_rows(
    select(synthetic_effect, logage, Z, idx_name, delta, question),
    select(coverage_effect, logage, Z, idx_name, delta, question)
)

# FILTER USING THE SAME INDEX LIST AS THE ORIGINAL PAPER
# Read the predefined index list used in the original paper
idx_list_paper <- read_csv("input/idx_list.csv", show_col_types = FALSE)
selected_indices <- idx_list_paper$idx_name

# Filter to only include indices from the predefined list (ordered by central wavelength)
indices_combined <- indices_combined |>
    filter(idx_name %in% selected_indices) |>
    mutate(
        question = factor(question, levels = c("Synthetic effect", "Coverage effect")),
        idx_name = factor(idx_name, levels = selected_indices)
    ) |>
    drop_na()  # Remove any missing values

cat("Comparison summary:\n")
cat("- Synthetic effect comparisons:", nrow(synthetic_effect), "\n")
cat("- Coverage effect comparisons:", nrow(coverage_effect), "\n")
cat("- Using predefined index list from paper:", length(selected_indices), "indices\n")
cat("- Unique indices after filtering:", length(unique(indices_combined$idx_name)), "\n")

# =============================================================================
# Z-SCORE NORMALIZATION 
# =============================================================================

cat("\n=== Applying z-score normalization ===\n")

# Apply z-score normalization within each index
# This allows comparison across indices with different scales
idx_scale <- indices_combined |>
    group_by(idx_name, question) |>
    mutate(scale_delta = as.numeric(scale(delta))) 

cat("Z-score normalization complete. Each index now has mean=0, sd=1.\n")

# =============================================================================
# CREATE FIGURE 11: SPECTRAL INDEX BOXPLOTS
# =============================================================================

cat("\n=== Creating Figure 11: Spectral Index Boxplots ===\n")

# Create separate plots for each question, then combine
# This ensures proper ordering within each panel

# Calculate median values explicitly for each question
synthetic_medians <- idx_scale |>
    filter(question == "Synthetic effect") |>
    group_by(idx_name) |>
    summarise(median_delta = median(scale_delta)) |>
    arrange(median_delta)

coverage_medians <- idx_scale |>
    filter(question == "Coverage effect") |>
    group_by(idx_name) |>
    summarise(median_delta = median(scale_delta)) |>
    arrange(median_delta)

# Print the orderings to verify
cat("Synthetic effect ordering (lowest to highest median):\n")
print(synthetic_medians)
cat("\nCoverage effect ordering (lowest to highest median):\n")
print(coverage_medians)

# Synthetic effect plot  
synthetic_data <- idx_scale |>
    filter(question == "Synthetic effect") |>
    mutate(idx_name_ordered = factor(idx_name, levels = synthetic_medians$idx_name))

p1 <- ggplot(synthetic_data, aes(x = idx_name_ordered, y = scale_delta)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_boxplot(notch = FALSE, outlier.color = "NA") +
    coord_cartesian(ylim = c(-5., 4.)) +
    labs(title = "Synthetic effect") +
    xlab("") +
    ylab(expression(paste(Delta, "idx (scaled)"))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(hjust = 0.5))

# Coverage effect plot
coverage_data <- idx_scale |>
    filter(question == "Coverage effect") |>
    mutate(idx_name_ordered = factor(idx_name, levels = coverage_medians$idx_name))

p2 <- ggplot(coverage_data, aes(x = idx_name_ordered, y = scale_delta)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_boxplot(notch = FALSE, outlier.color = "NA") +
    coord_cartesian(ylim = c(-5., 4.)) +
    labs(title = "Coverage effect") +
    xlab("Spectral Index") +
    ylab(expression(paste(Delta, "idx (scaled)"))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(hjust = 0.5))

# Combine the plots
library(gridExtra)
fig11 <- grid.arrange(p1, p2, ncol = 1)

# Save the plot
ggsave("output/figure11_indices_boxplot.png", fig11, 
       width = 14, height = 10, units = "in", dpi = 300)

cat("\nFigure 11 saved as 'figure11_indices_boxplot.png'\n")

# =============================================================================
# IDENTIFY MOST AFFECTED INDICES
# =============================================================================

cat("\n=== Ranking indices by effect magnitude ===\n")

# Calculate median absolute scaled delta for ranking
effect_ranking <- idx_scale |>
    group_by(idx_name, question) |>
    summarise(
        median_scaled_delta = median(scale_delta),
        abs_median_scaled_delta = abs(median(scale_delta)),
        iqr_scaled_delta = IQR(scale_delta),
        n_points = n(),
        .groups = "drop"
    )

# Most affected by Synthetic Effect
cat("Top 10 indices most affected by Synthetic Effect:\n")
synthetic_top <- effect_ranking |>
    filter(question == "Synthetic effect") |>
    arrange(desc(abs_median_scaled_delta)) |>
    select(idx_name, median_scaled_delta, iqr_scaled_delta) |>
    head(10)
print(synthetic_top)

# Most affected by Coverage Effect  
cat("\nTop 10 indices most affected by Coverage Effect:\n")
coverage_top <- effect_ranking |>
    filter(question == "Coverage effect") |>
    arrange(desc(abs_median_scaled_delta)) |>
    select(idx_name, median_scaled_delta, iqr_scaled_delta) |>
    head(10)
print(coverage_top)

# =============================================================================
# DETAILED STATISTICS TABLE
# =============================================================================

cat("\n=== Summary statistics for all indices ===\n")

summary_table <- effect_ranking |>
    select(idx_name, question, median_scaled_delta, iqr_scaled_delta) |>
    pivot_wider(names_from = question, 
                values_from = c(median_scaled_delta, iqr_scaled_delta),
                names_sep = "_") |>
    arrange(desc(abs(`median_scaled_delta_Synthetic effect`)))

print(summary_table)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== Analysis Complete ===\n")
cat("This script demonstrates:\n")
cat("1. Z-score normalization for cross-index comparisons\n")
cat("2. Factor reordering based on statistical summaries\n")
cat("3. Multi-panel boxplot creation with consistent styling\n")
cat("4. Identification of most affected spectral indices\n")
cat("5. Comprehensive statistical ranking and comparison\n")
cat("\nThe resulting figure shows which spectral indices are most sensitive\n")
cat("to differences between stellar population synthesis models, with indices\n")
cat("ordered by effect magnitude for easy interpretation.\n")