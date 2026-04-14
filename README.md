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
  > **Important — macOS Gatekeeper:** Because the app is not signed with an Apple Developer certificate, macOS will block it on first launch.
  > See the [Wiki](Opening-the-MARMOT-App-on-macOS) for instructions on how to open it anyway.
- **Windows**: Download and run the `.msi` installer.
  > If SmartScreen warns "Windows protected your PC", click **More info** → **Run anyway**.
  > See the [Wiki](Opening-the-MARMOT-App-on-Windows) for instructions on how to open it anyway.
- **Linux**: Download and extract the `.tar.gz`. Requires WebKit GTK: `sudo apt install libwebkit2gtk-4.1-0` (Ubuntu/Debian) or equivalent for your distro. Without it the app will not start.

Nonetheless, it is still **highly** recommended to run the couple of lines of R code required to install and run the pipeline in R!

---

## Prerequisites

You need **R** (version 4.5+) installed from [cloud.r-project.org](https://cloud.r-project.org/) and **Quarto** from [quarto.org](https://quarto.org/docs/get-started/) (required to render the HTML report). You'll also need a C/C++ compiler so that R packages can be built from source:

- **macOS**: Install Xcode Command Line Tools (open Terminal, run `xcode-select --install`) and [Homebrew](https://brew.sh) (`/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`). These provide the C compiler and system libraries that many R packages need. Homebrew's installer will also install Xcode CLT if you don't have it yet.
- **Windows**: Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) — download the version matching your R (e.g. Rtools44 for R 4.4.x, Rtools45 for R 4.5.x).
- **Linux**: Install build tools and development headers: `sudo apt install build-essential libcurl4-openssl-dev libssl-dev libxml2-dev` (Ubuntu/Debian) or equivalent for your distro.

---

## Install

You have two options to install MARMOT:

1) via the GUI App  
2) via R (recommended; e.g. using RStudio)
#
### 1) GUI App

Go to the **Install** tab in the app. Here you can:

- Click **“Install MARMOT”** to install the pipeline  
- Click **“Check Setup”** to verify your installation  
- Optionally install additional features via **“Install Extras”**
#
### 2) R Installation

Run the following code in R:

```r
# Step 1: Install and load MARMOT
install.packages("pak")
pak::pkg_install("peterleary/MARMOT")
library(MARMOT)

# Step 2: Install additional features (optional but recommended)
MARMOT::install_marmot_extras()

# Step 3: Verify installation
MARMOT::check_setup()
```


---

## Run MARMOT

### 1) GUI App

1. **Prepare your metadata**
   - In the **Welcome** tab, click **“Download Template”**
   - Either:
     - Fill in the metadata externally (recommended), or  
     - Enter metadata directly in the app

2. **Load your data**
   - Go to the **Pipeline Setup** tab
   - Click **“Open Metadata”** to load your completed metadata file  
   - Alternatively, click **“Open FCS Folder”**
     - This will automatically detect all `.fcs` files and prefill file names and markers  
     - ⚠️ You must still complete the metadata manually in the app

   💡 *Tip:* Keep your metadata file in the same folder as your `.fcs` files

3. **Adjust settings**
   - Review and modify:
     - Study design  
     - Settings  
     - File annotations  

4. **Run the pipeline**
   - Click **“Run Pipeline”** and let MARMOT process your data


#
### 2) R

#### Step 1: Create metadata template

```r
addMetadataToFCSFolder(FCS_folder = "~/Desktop/Flow_Data/")
# This creates a template Excel file in your data folder (only needed once).
```


#### Step 2: Fill in metadata
   - Open the Excel file manually
   - Complete all required fields across the three sheets
   - Use the included example data as guidance

   💡 Important: The metadata file must be in the same folder as your .fcs file

#### Step 2: Run the pipeline

```r
marmot(
  metadata = "~/Desktop/Flow_Data/MARMOT_Metadata.xlsx",
  name = "CD45+ Treated vs Control",
  render = TRUE
)
# The marmots will run the pipeline for a while... and generate a results folder
```

---

## Shiny App

After running the pipeline, you can explore your results interactively using the Shiny app.  

The app provides:
- Dimensionality reduction plots  
- Feature plots  
- Violin, dot, and ridge plots  
- Heatmaps and barplots  
- Cluster relabelling and cell subsetting  
- Export options (PDF and FCS)

#
### 1) GUI App

- Click the **Shiny link** immediately after the pipeline finishes  
- Or go to the **Shiny** tab and select your results folder manually  

#
### 2) R

```r
# Launch the Shiny app
# Use the path of your results folder
shinyMarmot(
  marmot_output = "~/Desktop/Flow_Data/Results_Files_2026-03-10_11.19.25/R_files")
```

---

## Docker & HPC (Apptainer/Singularity)

A pre-built Docker image with R, all packages, Quarto, and the Python environment is available on GitHub Container Registry. This is the easiest way to run MARMOT on a server or HPC cluster.
#
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
#
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

