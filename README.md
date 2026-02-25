# MARMOT

Multifaceted R Pipeline for Analysing Spectral Flow Cytometry Data

<p align="center">
<img src="images/MARMOT_Logo_2-min.png" width="150"/>
</p>

<html>

<hr style="height:5px;border-width:0;color:gray;background-color:gray">

</html>

**If you are using MARMOT in your work, please cite the MARMOT paper:**

MARMOT: Kirsche et al., 2025

Kirsche L, He J, Müller A, Leary P (2025). MARMOT: A multifaceted R pipeline for analysing spectral flow cytometry data from subcutaneously growing murine gastric organoids. Journal of Immunological Methods. https://doi.org/10.1016/j.jim.2025.113854

**And feel free to link MARMOT in your methods:** https://github.com/peterleary/MARMOT

For additional information, guidelines, and tips, please refer to the [Wiki](../../wiki) on this GitHub.

<html>

<hr style="height:5px;border-width:0;color:gray;background-color:gray">

</html>

## New GUI app! 

The marmots have been hard at work creating a nice little application that can prepare and run the entire MARMOT pipeline, no code! 

## Quick Install + Run

This is all the R code you will need to run the MARMOT pipeline and load the Shiny app.

```{r eval = F}
# Step 1. Install and load
install.packages("pak")
pak::pkg_install("peterleary/MARMOT", dependencies = TRUE)
library(MARMOT)

# Step 2: Save the template metadata file in a folder with the gated FCS files
addMetadataToFCSFolder(FCS_folder = "~/Desktop/Flow_Data/")
# This will place a copy of the template Excel Metadata file in the folder you specified

# Step 3: Fill the metadata file in manually in Excel

# Step 4: Run the pipeline
marmot(
  metadata = "~/Desktop/Flow_Data/MARMOT_Metadata.xlsx",
  name = "CD45+ Treated vs Control",
  render = TRUE
  )
# The marmots will run the pipeline for a while... and generate a results folder

# Step 5: Load the shiny app
shinyMarmot(marmot_output = "~/Desktop/Flow_Data/Results_Files_2025-03-10_11.19.25/R_files")
```

This above instructions will get you up and running with the basic MARMOT pipeline, which includes FlowSOM, Rphenograph, UMAP, and t-SNE.

For PARC (clustering) and PaCMAP (dimensionality reduction), you just need to set up the Python environment — see below.

<hr>

## Full Installation

### Install MARMOT and all R dependencies

```{r eval = F}
install.packages("pak")
pak::pkg_install("peterleary/MARMOT", dependencies = TRUE)

# Install any remaining CRAN, Bioconductor, and GitHub dependencies
MARMOT::install_dependencies()

# Check everything is installed
MARMOT::check_setup()
```

### Set up PARC and PaCMAP (Python)

PARC and PaCMAP run via Python through a conda environment. MARMOT bundles the Python scripts and environment definition — you just need conda/mamba installed.

#### Step 1: Install conda/mamba

If you don't already have conda or mamba, install [miniforge](https://github.com/conda-forge/miniforge) for your platform.

**NOTE**: If you're using an M-series Mac, it is **required** that you install the **x86_64** version of both R and miniforge to ensure compatibility with FastPG, PARC, and PaCMAP.

#### Step 2: Create the Python environment

```{r eval = F}
MARMOT::setup_python()
```

That's it. This creates the `p4r` conda environment with all required Python packages. The pipeline will auto-detect the environment and the bundled PARC/PaCMAP scripts — no manual path configuration needed.

You can verify everything is ready with:

```{r eval = F}
MARMOT::check_setup()
```
</content>
</invoke>