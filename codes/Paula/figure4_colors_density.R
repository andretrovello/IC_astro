# Figure 4: Color Density Plots - CBC20 Paper Recreation
# 
# This script recreates Figure 4 from the CBC20 paper, showing density plots
# of color differences between different stellar population synthesis models.
# 
# The figure compares two main effects:
# - Synthetic effect: How synthetic vs empirical stellar libraries affect colors
# - Coverage effect: How stellar library coverage affects color predictions
#
# Author: Claude & Paula. Created for educational purposes
# Date: 2025

# =============================================================================
# LOAD REQUIRED LIBRARIES
# =============================================================================

library(tidyverse)  # For data manipulation and plotting (includes stringr)

# =============================================================================
# DATA READING FUNCTIONS
# =============================================================================

# Function to read GALAXEV format color files (.1ABmag files)
# These files contain photometric magnitudes in ugriz bands
read_bc_color2 <- function(myfile) {
    cat("Reading", myfile, "\n")
    
    # Extract metallicity from filename (e.g., "z017" -> "0.017")
    metal_value <- str_extract(myfile, "z[0-9]+")
    metal_value <- str_replace(metal_value, "z", "0.0")
    if (str_detect(myfile, "z0002")) metal_value <- "0.0002"
    if (str_detect(myfile, "z004")) metal_value <- "0.004"
    if (str_detect(myfile, "z008")) metal_value <- "0.008"
    if (str_detect(myfile, "z017")) metal_value <- "0.017"
    if (str_detect(myfile, "z030")) metal_value <- "0.030"
    
    # Read the file, skipping header lines
    color <- read_table(myfile, skip = 29, show_col_types = FALSE) |> 
        select(1:5) |>
        rename("logage" = 1) |>
        # Calculate color indices from magnitudes
        mutate(
            ug = u - g,
            ur = u - r, 
            gr = g - r,
            gi = g - i,
            ri = r - i
        ) |>
        # Reshape to long format for plotting
        pivot_longer(cols = c(ug, ur, gr, gi, ri), names_to = "color", values_to = "color_value") |>
        add_column(Z = metal_value) |>
        # Apply age cut (minimum log age = 7.0)
        filter(logage >= 7.0) |>
        select(logage, Z, color, color_value)
    
    return(color)
}

# Function to read CBC format color files (.ABmag.gz files)
# These files have a different structure with specific metallicity naming
read_cbc_color <- function(mydir, str_identifier) {
    cat("Reading CBC color files from", mydir, "\n")
    
    # Define metallicity mappings
    prefix <- paste("cbc2018_", str_identifier, sep = "")
    sufix <- "_chab_ssp.ABmag.gz"
    metal <- c("0002", "004", "008", "017", "030")
    metalstr <- c("m20p04", "m10p04", "m05p02", "p00p00", "p02p00")
    
    color_all <- NULL
    
    # Loop through each metallicity
    for (loop in seq_along(metal)) {
        metal_value <- paste("0.", metal[[loop]], sep = "")
        
        filename <- paste(mydir, prefix, '_', metalstr[[loop]], sufix, sep = "")
        cat("Processing", filename, "\n")
        
        color_tmp <- read_table(filename, skip = 0, show_col_types = FALSE) |> 
            select(2:6) |>
            rename("logage" = "log_age_yr") |>
            # Calculate color indices
            mutate(
                ug = u - g, 
                ur = u - r,
                gr = g - r, 
                gi = g - i,
                ri = r - i
            ) |>
            # Reshape to long format
            pivot_longer(cols = c(ug, ur, gr, gi, ri), names_to = "color", values_to = "color_value") |>
            add_column(Z = metal_value) |>
            select(logage, Z, color, color_value)
        
        if (is.null(color_all)) {
            color_all <- color_tmp
        } else {
            color_all <- rbind(color_all, color_tmp)
        }
    }
    
    # Apply age cut
    color_all <- filter(color_all, logage >= 7.0)
    return(color_all)
}

# =============================================================================
# DATA LOADING AND PROCESSING
# =============================================================================

cat("=== Loading stellar population synthesis model colors ===\n")

# Define data directories
dir_pmiles <- "input/pmiles_models/"
dir_smiles <- "input/smiles_models/"
dir_cbc <- "input/cbc_milesres_models/"

# 1. Read MILES (pure empirical) model colors
cat("\n1. Loading MILES colors...\n")
pmiles_files <- list.files(dir_pmiles, pattern = "*1ABmag", full.names = TRUE)
pmiles_color <- map_dfr(pmiles_files, read_bc_color2) |>
    mutate(identifier = "pmiles")

# 2. Read SynMILES (synthetic) model colors  
cat("\n2. Loading SynMILES colors...\n")
smiles_files <- list.files(dir_smiles, pattern = "*1ABmag", full.names = TRUE)
smiles_color <- map_dfr(smiles_files, read_bc_color2) |>
    mutate(identifier = "smiles")

# 3. Read CBC (theoretical with extended coverage) model colors
cat("\n3. Loading CBC colors...\n")
cbc_color <- read_cbc_color(dir_cbc, "mr_coelho_2014_parsec_mstpagb") |>
    mutate(identifier = "cbc")

# =============================================================================
# CREATE COMPARISON DATASETS
# =============================================================================

cat("\n=== Creating comparison datasets ===\n")

# Question 1: Synthetic Effect (SynMILES vs MILES)
# How do synthetic stellar libraries compare to empirical ones?
synthetic_effect <- inner_join(
    smiles_color, pmiles_color,
    by = c("logage", "Z", "color"), 
    suffix = c("_ref", "_comp")
) |>
    mutate(
        delta = color_value_comp - color_value_ref,
        question = "Synthetic effect"
    )

# Question 2: Coverage Effect (CBC vs SynMILES) 
# How does extended stellar parameter coverage affect predictions?
coverage_effect <- inner_join(
    smiles_color, cbc_color,
    by = c("logage", "Z", "color"), 
    suffix = c("_ref", "_comp")
) |>
    mutate(
        delta = color_value_comp - color_value_ref,
        question = "Coverage effect"
    )

# Combine both comparisons
colors_combined <- bind_rows(
    select(synthetic_effect, logage, Z, color, delta, question),
    select(coverage_effect, logage, Z, color, delta, question)
)

# Set factor levels for proper ordering
colors_combined <- colors_combined |>
    mutate(
        question = factor(question, levels = c("Synthetic effect", "Coverage effect")),
        color = factor(color, levels = c("ug", "ur", "gr", "gi", "ri"))
    )

cat("Data summary:\n")
cat("- Synthetic effect comparisons:", nrow(synthetic_effect), "\n")
cat("- Coverage effect comparisons:", nrow(coverage_effect), "\n")
cat("- Total comparisons:", nrow(colors_combined), "\n")

# =============================================================================
# CREATE FIGURE 4: COLOR DENSITY PLOTS
# =============================================================================

cat("\n=== Creating Figure 4: Color Density Plots ===\n")

# Define color labels for better plot readability
color_labels <- c(
    ug = "u - g",
    ur = "u - r", 
    gr = "g - r",
    gi = "g - i",
    ri = "r - i"
)

# Create the density plot
fig4 <- ggplot(colors_combined, aes(delta, fill = Z, color = Z)) + 
    # Create panels: colors as rows, questions as columns
    facet_grid(color ~ question, labeller = labeller(color = color_labels)) +
    # Set x-axis limits to match original figure
    coord_cartesian(xlim = c(-0.5, 0.28)) +
    # Add vertical line at zero difference
    geom_vline(xintercept = 0, color = "gray", linetype = "dashed") +
    # Create density curves, one for each metallicity
    geom_density(aes(y = after_stat(scaled)), alpha = 0.3, position = "identity") + 
    # Set axis labels
    labs(
        x = expression(paste(Delta, " colour")), 
        y = "Scaled density",
        title = "Figure 4: Color Differences Between Stellar Population Models",
        subtitle = "Density plots showing the distributions of colour differences ( colour, defined in Section 4.1.1) for the different combinations of SDSS-based
colours (in rows) and the model effects (in columns). Colours indicate the metallicities Z, as indicated in the label.",
        fill = "Metallicity (Z)",
        color = "Metallicity (Z)"
    ) +
    # Apply clean theme
    theme_minimal() +
    theme(
        strip.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.position = "bottom"
    )

# Display the plot
print(fig4)

# Save the plot
ggsave("output/figure4_colors_density.png", fig4, 
       width = 12, height = 8, units = "in", dpi = 300)

cat("\nFigure 4 saved as 'figure4_colors_density.png'\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n=== Summary Statistics ===\n")

# Calculate summary statistics for each effect
summary_stats <- colors_combined |>
    group_by(color, question) |>
    summarise(
        median_delta = median(delta),
        iqr_delta = IQR(delta),
        mean_delta = mean(delta),
        std_delta = sd(delta),
        n_points = n(),
        .groups = "drop"
    )

print(summary_stats)

cat("\n=== Analysis Complete ===\n")
cat("This script demonstrates:\n")
cat("1. Reading different stellar population synthesis model formats\n")
cat("2. Computing photometric color differences\n") 
cat("3. Creating multi-panel density plots\n")
cat("4. Comparing systematic effects between model types\n")
cat("\nThe resulting figure shows how synthetic vs empirical stellar libraries\n")
cat("and stellar parameter coverage affect predicted galaxy colors.\n")