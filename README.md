# MARMOT

**M**ultif**a**ceted **R** Pipeline for **M**easuring Spectral Fl**o**w Cytometry Da**t**a

<p align="center">
<img src="images/MARMOT_Logo_2-min.png" width="150"/>
</p>

---

**If you are using MARMOT in your work, please cite the MARMOT paper:**

MARMOT: Kirsche et al., 2025

Kirsche L, He J, Müller A, Leary P (2025). MARMOT: A multifaceted R pipeline for analysing spectral flow cytometry data from subcutaneously growing murine gastric organoids. Journal of Immunological Methods. https://doi.org/10.1016/j.jim.2025.113854

**And feel free to link MARMOT in your methods:** https://github.com/peterleary/MARMOT

For additional information, guidelines, and tips, please refer to the [Wiki](https://github.com/peterleary/MARMOT/wiki) on this GitHub.

---

## GUI App

Good news! A new standalone desktop app that can prepare and run the entire MARMOT pipeline — no code required - is here! Download the latest release for your platform from the [GitHub Releases page](https://github.com/peterleary/MARMOT/releases).

- **macOS**: Download the `.zip`, unzip, and open MARMOT.app.
  > **Important — macOS Gatekeeper:** Because the app is not signed with an Apple Developer certificate, macOS will block it on first launch ("MARMOT.app is damaged" or "can't be verified"). To open it: **right-click** (or Control-click) the app → click **Open** → click **Open** again in the dialog. You only need to do this once. Alternatively, run `xattr -cr MARMOT.app` in Terminal.
- **Windows**: Download and run the `.msi` installer. If SmartScreen warns "Windows protected your PC", click **More info** → **Run anyway**.
- **Linux**: Download and extract the `.tar.gz`. Requires WebKit GTK: `sudo apt install libwebkit2gtk-4.1-0` (Ubuntu/Debian) or equivalent for your distro. Without it the app will not start.

Nonetheless, it is still **highly** recommended to run the couple of lines of R code required to install and run the pipeline in R!

## Prerequisites

You need **R** (version 4.3+) installed from [cloud.r-project.org](https://cloud.r-project.org/). You'll also need a C/C++ compiler so that R packages can be built from source:

- **macOS**: Install Xcode Command Line Tools (open Terminal, run `xcode-select --install`) and [Homebrew](https://brew.sh) (`/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`). These provide the C compiler and system libraries that many R packages need. Homebrew's installer will also install Xcode CLT if you don't have it yet.
- **Windows**: Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) — download the version matching your R (e.g. Rtools44 for R 4.4.x, Rtools45 for R 4.5.x).
- **Linux**: Install build tools and development headers: `sudo apt install build-essential libcurl4-openssl-dev libssl-dev libxml2-dev` (Ubuntu/Debian) or equivalent for your distro.

## Quick Install + Run

This is all the R code you will need to run the MARMOT pipeline and load the Shiny app.

```r
# Step 1. Install and load the base MARMOT app 
install.packages("pak")
pak::pkg_install("peterleary/MARMOT")
library(MARMOT)

# Install as many bonus features as possible + set up Python for PARC/PaCMAP
MARMOT::install_marmot_extras()

# Check what's installed
MARMOT::check_setup()
```

```r
# Step 2: Save the template metadata file in a folder with your exported gated FCS files
addMetadataToFCSFolder(FCS_folder = "~/Desktop/Flow_Data/")
# This will place a copy of the template Excel Metadata file in the folder you specified
```

```r
# Step 3: Fill the metadata file in manually in Excel
# There's three sheets, use the dummy data included for inspiration!
```

```r
# Step 4: Run the pipeline
marmot(
  metadata = "~/Desktop/Flow_Data/MARMOT_Metadata.xlsx",
  name = "CD45+ Treated vs Control",
  render = TRUE
  )
# The marmots will run the pipeline for a while... and generate a results folder
```

```r
# Step 5: Load the shiny app
shinyMarmot(marmot_output = "~/Desktop/Flow_Data/Results_Files_2026-03-10_11.19.25/R_files")
```

These instructions will get you up and running with the basic MARMOT pipeline, which includes FlowSOM (default clustering), UMAP, and t-SNE. Alternative clustering methods (Rphenograph, FastPG) are installed separately — `install_marmot_extras()` will attempt them and let you know if any were skipped.

For PARC (clustering) and PaCMAP (dimensionality reduction), you just need to set up the Python environment — see below.

---

## Shiny App

After running the pipeline, explore your results interactively with `shinyMarmot()`. The app provides dimensionality reduction plots, feature plots, violin/dot/ridge plots, heatmaps, and barplots — all with cluster relabelling, cell subsetting, and PDF/FCS export.

```r
shinyMarmot(marmot_output = "path/to/R_files")
```

## Docker & HPC (Apptainer/Singularity)

A pre-built Docker image with R, all packages, Quarto, and the Python environment is available on GitHub Container Registry. This is the easiest way to run MARMOT on a server or HPC cluster.

### Docker

```bash
docker pull ghcr.io/peterleary/marmot:latest
```

Mount the folder containing your FCS files and metadata Excel into the container with `-v`. Replace the path before the `:` with your actual data folder:

```bash
docker run --rm -it -v ~/Desktop/Flow_Data:/data ghcr.io/peterleary/marmot:latest
```

This opens an interactive R session inside the container. Your data folder is available at `/data`. From here you can verify the setup and run the pipeline:

```r
library(MARMOT)
check_setup()

# Run the pipeline (your files are mounted at /data)
marmot(
  metadata = "/data/MARMOT_Metadata.xlsx",
  name = "MyRun",
  render = TRUE
)
```

Or run the pipeline directly without entering R:

```bash
docker run --rm -v ~/Desktop/Flow_Data:/data ghcr.io/peterleary/marmot:latest \
  Rscript -e 'MARMOT::marmot("/data/MARMOT_Metadata.xlsx", name="MyRun", render=TRUE)'
```

### Apptainer / Singularity (HPC)

Most university clusters don't allow Docker but do support [Apptainer](https://apptainer.org/) (formerly Singularity). Apptainer can pull the Docker image directly:

```bash
# Pull once (creates a ~5 GB .sif file)
apptainer pull docker://ghcr.io/peterleary/marmot:latest
```

Mount your data folder with `--bind` and run interactively:

```bash
apptainer shell --bind /path/to/data:/data marmot_latest.sif
```

This opens a shell inside the container. Start R and run the pipeline as above:

```r
R
library(MARMOT)
check_setup()
marmot(metadata = "/data/MARMOT_Metadata.xlsx", name = "MyRun", render = TRUE)
```

Or run non-interactively:

```bash
apptainer run --bind /path/to/data:/data marmot_latest.sif \
  Rscript -e 'MARMOT::marmot("/data/MARMOT_Metadata.xlsx", name="MyRun", render=TRUE)'
```

---

## Full Installation

Follow these extra instructions if you want to unleash the full potential of the marmots. **It's totally optional!**

```r
install.packages("pak")
pak::pkg_install("peterleary/MARMOT")

# One command to install everything: CRAN/Bioconductor packages in batch,
# GitHub packages (Rphenograph, FastPG) with graceful failure handling,
# and the Python environment for PARC/PaCMAP
MARMOT::install_marmot_extras()

# Check everything is installed
MARMOT::check_setup()
```

`install_marmot_extras()` handles everything in one go. The Python environment for PARC and PaCMAP requires conda or mamba — install [miniforge](https://github.com/conda-forge/miniforge) if you don't have it. If a GitHub package like Rphenograph fails to compile, the rest of the install continues normally.
