# MARMOT pre-built image: R, every package MARMOT uses, Quarto, and the
# original Python PARC and PaCMAP (set up with pixi). Ready to run the
# pipeline, and to serve the Explorer (for example under ShinyProxy).
#
# Build (from the repository root or the docker/ folder; MARMOT itself is
# fetched from GitHub at MARMOT_REF):
#   docker build -f docker/Dockerfile --build-arg MARMOT_REF=v2 -t marmot:v2 docker/
#
# Run the pipeline:
#   docker run --rm -v /path/to/data:/data marmot:v2 \
#     Rscript -e 'MARMOT::marmot("/data/MARMOT_Metadata.xlsx", name = "MyRun", render = TRUE)'
#
# Serve the Explorer on port 3838 (open it with ?data=/path/to/R_files):
#   docker run --rm -p 3838:3838 -v /path/to/results:/path/to/results marmot:v2 \
#     Rscript -e 'shiny::runApp(system.file("app", package = "MARMOT"), host = "0.0.0.0", port = 3838)'

FROM rocker/r-ver:4.6.2

ARG TARGETARCH
ARG MARMOT_REF=main
# Bioconductor release to install from. Pinned rather than "the newest", so
# a new release can't break the build or change results unannounced: 3.24
# (October 2026) shipped an ncdfFlow that no longer loads with its own
# BiocGenerics and flowCore, and PeacoQC needs ncdfFlow. MARMOT's reference
# runs were made on 3.23. pak/pkgcache read R_BIOC_VERSION too.
ARG BIOC_VERSION=3.23
ENV R_BIOC_VERSION=${BIOC_VERSION}

# ── System libraries ─────────────────────────────────────────────────
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    libpng-dev libtiff5-dev libjpeg-dev libwebp-dev \
    libfftw3-dev libhdf5-dev libglpk-dev libgsl-dev libtbb-dev \
    cmake curl git ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# ── Quarto (renders the reports) ─────────────────────────────────────
RUN QUARTO_VER="1.6.43" && \
    if [ "$TARGETARCH" = "arm64" ]; then QUARTO_ARCH="linux-arm64"; else QUARTO_ARCH="linux-amd64"; fi && \
    curl -fsSL -o /tmp/quarto.deb \
      "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VER}/quarto-${QUARTO_VER}-${QUARTO_ARCH}.deb" && \
    dpkg -i /tmp/quarto.deb && rm /tmp/quarto.deb

# ── Where MARMOT keeps its Python environment: a shared place, so the
#    environment built here is found by whichever user runs the container
#    (ShinyProxy and Apptainer do not run as root) ────────────────────
ENV R_USER_DATA_DIR=/opt/marmot-data
RUN mkdir -p /opt/marmot-data && chmod 755 /opt/marmot-data

# ── R packages: MARMOT and everything it uses, optional ones included.
#    MARMOT's r-universe serves anndataR; Bioconductor the rest.
#    Rphenograph comes straight from GitHub, so the image still builds
#    when the r-universe index is having a wobble (it is not on CRAN).
#    pak installs in parallel and builds what has no binary. ──
RUN Rscript -e 'install.packages(c("pak", "BiocManager"), repos = "https://cloud.r-project.org")'
# Change CACHEBUST to fetch MARMOT again from GitHub
ARG CACHEBUST=1
RUN Rscript -e 'options(repos = c(MARMOT = "https://peterleary.r-universe.dev", BiocManager::repositories(version = Sys.getenv("R_BIOC_VERSION")))); \
    pak::pkg_install(c(paste0("peterleary/MARMOT@", Sys.getenv("MARMOT_REF")), "github::peterleary/Rphenograph"), \
                     dependencies = TRUE, ask = FALSE)'

# qs2 from source, so it links against libtbb (a binary may not)
RUN Rscript -e 'install.packages("qs2", type = "source", repos = "https://cloud.r-project.org")'

# ── Python: the original PARC and PaCMAP, with pixi from MARMOT's lock file
RUN Rscript -e 'stopifnot(MARMOT::install_marmot_python())' && chmod -R a+rX /opt/marmot-data

# ── Check: fail the build if anything is missing ─────────────────────
RUN Rscript -e 'r <- MARMOT::check_setup(); \
    miss <- r$package[r$status != "installed"]; \
    if (length(miss)) stop("Missing: ", paste(miss, collapse = ", "))'

# ── Runtime: a non-root user, and a writable home for Apptainer ──────
RUN useradd -m -s /bin/bash marmot && chmod 777 /home/marmot

# A cache folder any user can write to. Run with -u (or under ShinyProxy or
# Apptainer) and there may be no home folder, so Quarto would try /.cache and
# fail. Set after every install step, so nothing made as root lands in it. ──
ENV XDG_CACHE_HOME=/tmp/.cache
WORKDIR /data
EXPOSE 3838
CMD ["R"]
