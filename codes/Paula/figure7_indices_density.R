# Figure 7: Spectral Index Density Plots - CBC20 Paper Recreation
#
# This script recreates Figure 7 from the CBC20 paper, showing density plots
# of spectral index differences between different stellar population synthesis models.
#
# The figure compares two main effects:
# - Synthetic effect: How synthetic vs empirical stellar libraries affect indices
# - Coverage effect: How stellar library coverage affects index predictions
#
# Author: Claude & Paula. Created for educational purposes.  
# Date: 2025

# =============================================================================
# LOAD REQUIRED LIBRARIES
# =============================================================================

library(tidyverse)  # For data manipulation and plotting (includes stringr)
library(FITSio)     # For reading FITS files

# =============================================================================
# DATA READING FUNCTIONS
# =============================================================================

# Function to read spectral indices from FITS files (GALAXEV format)
# These files contain Lick indices and other spectral measurements
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
    
    # Read from specific HDUs (Header Data Units) in FITS file
    # HDU 3: Evolution indices (B912 to B4000)
    tmp1 <- readFrameFromFITS(myfile, hdu = 3) |>
        select(logage, B912:B4000)
    
    # HDU 5: Lick indices (CN1 to BHHK) 
    tmp2 <- readFrameFromFITS(myfile, hdu = 5) |>
        select(logage, CN1:BHHK)
    
    # Combine both sets of indices
    idx_tmp <- inner_join(tmp1, tmp2, by = "logage") |>
        add_column(Z = metal_value, .after = "logage") |>
        # Reshape to long format
        pivot_longer(cols = -c(logage, Z), names_to = "idx_name", values_to = "idx_value")
    
    # Apply age cut
    idx <- filter(idx_tmp, logage >= 7.0)
    return(idx)   
}

# Function to read CBC spectral indices from ASCII files (.lsindx.gz)
read_cbc_idx <- function(myfile) {
    cat("Processing", myfile, "\n")
    
    # Define the indices available in CBC files
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
    
    # Read the ASCII file
    idx <- read_table(myfile, skip = 2, 
                     col_names = c("isoname", "logage", idx_list),
                     show_col_types = FALSE) |> 
        select(-isoname) |>
        # Reshape to long format
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

# 1. Read MILES indices from FITS files
cat("\n1. Loading MILES indices...\n")
pmiles_files <- list.files(dir_pmiles, pattern = "*.fits", full.names = TRUE)
pmiles_idx <- map_dfr(pmiles_files, read_bc_idx_fits) |>
    mutate(identifier = "pmiles")

# 2. Read SynMILES indices from FITS files
cat("\n2. Loading SynMILES indices...\n") 
smiles_files <- list.files(dir_smiles, pattern = "*.fits", full.names = TRUE)
smiles_idx <- map_dfr(smiles_files, read_bc_idx_fits) |>
    mutate(identifier = "smiles")

# 3. Read CBC indices from ASCII files
cat("\n3. Loading CBC indices...\n")
cbc_files <- list.files(dir_cbc, pattern = "*lsindx*", full.names = TRUE)
cbc_idx <- map_dfr(cbc_files, read_cbc_idx) |>
    mutate(identifier = "cbc")

# Combine all index data
indices_all <- bind_rows(pmiles_idx, smiles_idx, cbc_idx)

# Standardize index names (some indices have different names in different models)
indices_all <- indices_all |>
    mutate(
        idx_name = case_when(
            idx_name == "C4668" ~ "Fe4668",   # Combine C4668 and Fe4668
            idx_name == "BHHK" ~ "CaHK",      # Combine BHHK and CaHK
            TRUE ~ idx_name
        )
    )

cat("Index data summary:\n")
cat("- MILES indices:", nrow(pmiles_idx), "\n")
cat("- SynMILES indices:", nrow(smiles_idx), "\n") 
cat("- CBC indices:", nrow(cbc_idx), "\n")
cat("- Total index measurements:", nrow(indices_all), "\n")

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

cat("Index selection from paper:\n")
cat("- Total unique indices found:", length(unique(indices_combined$idx_name)), "\n")
cat("- Using predefined index list from paper:", length(selected_indices), "indices\n")
cat("- Indices being plotted:", paste(selected_indices, collapse = ", "), "\n")

# Filter to only include indices from the predefined list (ordered by central wavelength)
indices_combined <- indices_combined |>
    filter(idx_name %in% selected_indices) |>
    mutate(
        question = factor(question, levels = c("Synthetic effect", "Coverage effect")),
        idx_name = factor(idx_name, levels = selected_indices)
    ) |>
    drop_na()  # Remove any missing values

cat("\nFiltered comparison summary:\n")
cat("- Synthetic effect comparisons:", sum(indices_combined$question == "Synthetic effect"), "\n")
cat("- Coverage effect comparisons:", sum(indices_combined$question == "Coverage effect"), "\n")
cat("- Unique indices analyzed:", length(unique(indices_combined$idx_name)), "\n")
cat("- Index ordering: by central wavelength (as in original paper)\n")

# =============================================================================
# CREATE FIGURE 7: SPECTRAL INDEX DENSITY PLOTS
# =============================================================================

cat("\n=== Creating Figure 7: Spectral Index Density Plots ===\n")

# Create the multi-panel density plot
fig7 <- ggplot(indices_combined, aes(delta, color = question, fill = question)) + 
    # Add vertical line at zero difference
    geom_vline(xintercept = 0, color = "gray", linetype = "dashed") +
    # Create density curves for each effect
    geom_density(aes(y = after_stat(scaled)), alpha = 0.2, position = "identity") +  
    # Create separate panel for each index
    facet_wrap(~ idx_name, scales = "free_x", ncol = 5) +
    # Set axis labels and title
    labs(
        x = expression(paste(Delta, "idx")), 
        y = "Scaled density",
        title = "Figure 7: Spectral Index Differences Between Stellar Population Models",
        subtitle = "Density plots showing the distributions of index differences",
        color = "Effect Type",
        fill = "Effect Type"
    ) +
    # Apply clean theme
    theme_minimal() +
    theme(
        strip.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.title = element_blank(), 
        legend.position = "bottom",
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7)
    ) +
    # Set colors for the two effects
    scale_color_manual(values = c("Synthetic effect" = "#F0ACA4", 
                                 "Coverage effect" = "#99D495")) +
    scale_fill_manual(values = c("Synthetic effect" = "#F0ACA4", 
                                "Coverage effect" = "#99D495"))

# Display the plot
print(fig7)

# Save the plot
ggsave("output/figure7_indices_density.png", fig7, 
       width = 16, height = 12, units = "in", dpi = 300)

cat("\nFigure 7 saved as 'figure7_indices_density.png'\n")

# =============================================================================
# DETAILED ANALYSIS BY INDEX
# =============================================================================

cat("\n=== Index-by-Index Analysis ===\n")

# Calculate summary statistics for each index and effect
index_summary <- indices_combined |>
    group_by(idx_name, question) |>
    summarise(
        median_delta = median(delta),
        iqr_delta = IQR(delta),
        mean_delta = mean(delta),
        std_delta = sd(delta),
        n_points = n(),
        .groups = "drop"
    ) |>
    arrange(idx_name, question)

print(index_summary)

# Identify indices most affected by each effect
cat("\nIndices most affected by Synthetic Effect (largest |median delta|):\n")
synthetic_ranking <- index_summary |>
    filter(question == "Synthetic effect") |>
    arrange(desc(abs(median_delta))) |>
    select(idx_name, median_delta, iqr_delta) |>
    head(10)
print(synthetic_ranking)

cat("\nIndices most affected by Coverage Effect (largest |median delta|):\n")
coverage_ranking <- index_summary |>
    filter(question == "Coverage effect") |>
    arrange(desc(abs(median_delta))) |>
    select(idx_name, median_delta, iqr_delta) |>
    head(10)
print(coverage_ranking)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== Analysis Complete ===\n")
cat("This script demonstrates:\n")
cat("1. Reading FITS files with astronomical data\n")
cat("2. Processing ASCII spectral index files\n")
cat("3. Handling different data formats and index naming conventions\n")
cat("4. Creating multi-panel density plots with many subplots\n")
cat("5. Statistical comparison of systematic effects on spectral indices\n")
cat("\nThe resulting figure shows how different stellar libraries affect\n")
cat("the prediction of spectral indices used in galaxy analysis.\n")