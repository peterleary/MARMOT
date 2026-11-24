# MARMOT Docker Image

Pre-built image with R, all Bioconductor/CRAN packages, Quarto, and the Python environment — ready to run pipelines immediately.

## Docker

```bash
# Pull the latest release
docker pull ghcr.io/peterleary/marmot:latest

# Run a pipeline
docker run --rm -v /path/to/fcs:/data ghcr.io/peterleary/marmot \
  Rscript -e 'MARMOT::marmot("/data/metadata.xlsx", render=TRUE)'

# Interactive R session
docker run --rm -it -v /path/to/fcs:/data ghcr.io/peterleary/marmot
```

## Apptainer / Singularity (HPC)

No root required. Convert the Docker image to a `.sif` file once, then run anywhere:

```bash
# Pull (once, produces ~3 GB .sif file)
apptainer pull marmot.sif docker://ghcr.io/peterleary/marmot:latest

# Run a pipeline
apptainer run --bind /scratch/data:/data marmot.sif \
  Rscript -e 'MARMOT::marmot("/data/metadata.xlsx", render=TRUE)'

# Interactive R session
apptainer shell --bind /scratch/data:/data marmot.sif
```

**HPC tips:**

- Use `--bind` to mount your data directory (equivalent to Docker `-v`)
- The image contains a writable `/home/marmot` directory for temp files
- If your cluster requires `singularity` instead of `apptainer`, the commands are identical
- For SLURM jobs, put the `apptainer run ...` command in your submission script

## Image tags

| Tag | Description |
|-----|-------------|
| `latest` | Latest stable release |
| `0.4.0` | Specific version |
| `0.4` | Latest patch of a minor version |
| `dev` | Built from manual workflow dispatch |

## Building locally

```bash
docker build -f docker/Dockerfile --build-arg MARMOT_REF=main -t marmot-test docker/
docker run --rm marmot-test Rscript -e 'MARMOT::check_setup()'
```

`MARMOT_REF` is any branch, tag or commit on GitHub (`v2` for the 2.0 beta). The build fails if any package MARMOT uses is missing.

## Serving the Explorer (e.g. under ShinyProxy)

The image also serves the Explorer on port 3838. It opens the results given in the address, `?data=/path/to/Results_Files_*/R_files`, so mount the folder that holds the results at the same path inside the container:

```bash
docker run --rm -p 3838:3838 -v /srv/results:/srv/results marmot:v2 \
  Rscript -e 'shiny::runApp(system.file("app", package = "MARMOT"), host = "0.0.0.0", port = 3838)'
```

To let users save cluster labels for the MARMOT extension from the Explorer, the results folder must be writable inside the container.

## Python (PARC and PaCMAP)

MARMOT 2 sets up the original Python PARC and PaCMAP with pixi (no conda). The image keeps the environment in `/opt/marmot-data` (`R_USER_DATA_DIR`), readable by any user, so it works under ShinyProxy and Apptainer, which do not run as root.
