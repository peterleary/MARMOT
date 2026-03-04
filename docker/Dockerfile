# MARMOT pre-built pipeline image
# Everything baked in: R, Bioc packages, Quarto, Python (PARC/PaCMAP)
#
# Build:
#   docker build -f docker/Dockerfile --build-arg MARMOT_REF=dev -t marmot-prod docker/
#
# Run:
#   docker run --rm -v /path/to/data:/data marmot-prod \
#     Rscript -e 'MARMOT::marmot("/data/metadata.xlsx", render=TRUE)'

FROM rocker/r-ver:4.4.2

ARG TARGETARCH
ARG MARMOT_REF=main

# ── System dependencies ──────────────────────────────────────────────
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libfftw3-dev \
    libhdf5-dev \
    libglpk-dev \
    libgsl-dev \
    libtbb-dev \
    cmake \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/*

# ── Quarto ───────────────────────────────────────────────────────────
RUN QUARTO_VER="1.6.43" && \
    if [ "$TARGETARCH" = "arm64" ]; then \
      QUARTO_ARCH="linux-arm64"; \
    else \
      QUARTO_ARCH="linux-amd64"; \
    fi && \
    curl -fsSL -o /tmp/quarto.deb \
      "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VER}/quarto-${QUARTO_VER}-${QUARTO_ARCH}.deb" && \
    dpkg -i /tmp/quarto.deb && \
    rm /tmp/quarto.deb

# ── Miniforge (conda for Python env) ────────────────────────────────
RUN if [ "$TARGETARCH" = "arm64" ]; then \
      MF_ARCH="aarch64"; \
    else \
      MF_ARCH="x86_64"; \
    fi && \
    curl -fsSL -o /tmp/miniforge.sh \
      "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-${MF_ARCH}.sh" && \
    bash /tmp/miniforge.sh -b -p /opt/miniforge && \
    rm /tmp/miniforge.sh

ENV PATH="/opt/miniforge/bin:${PATH}"
ENV RETICULATE_MINICONDA_PATH="/opt/miniforge"

# ── R packages: pak → MARMOT + all dependencies ─────────────────────
RUN Rscript -e 'install.packages("pak", repos = "https://r-lib.r-universe.dev")'

# Arrow C++ build: enable Parquet but skip optional network-dependent components
ENV NOT_CRAN=true
ENV LIBARROW_MINIMAL=false
ENV ARROW_PARQUET=ON
ENV ARROW_WITH_THRIFT=OFF
ENV ARROW_S3=OFF
ENV ARROW_GCS=OFF

# Change CACHEBUST value to force re-pull of MARMOT from GitHub
ARG CACHEBUST=1
RUN Rscript -e "pak::pkg_install('peterleary/MARMOT@${MARMOT_REF}')"

RUN Rscript -e "MARMOT::install_marmot_extras(include_suggests = TRUE)"

# ── Python environment (PARC / PaCMAP) ──────────────────────────────
RUN Rscript -e 'MARMOT::setup_python(conda = "/opt/miniforge/bin/conda")'

# ── Verify everything is installed ───────────────────────────────────
RUN Rscript -e "MARMOT::check_setup()"

# ── Runtime setup ────────────────────────────────────────────────────
# Create non-root home for Apptainer UID remapping
RUN useradd -m -s /bin/bash marmot && \
    chmod 777 /home/marmot

WORKDIR /data

CMD ["R"]
