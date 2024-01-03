FROM rocker/r-ver:4

# Set up environemnt
ENV BASE_DIR "/app"

ENV APP_DIR "${BASE_DIR}/russiacounter"
WORKDIR ${APP_DIR}

ENV R_LIBS_USER "${BASE_DIR}/packages"
RUN mkdir -p ${R_LIBS_USER}

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
    python3 \
    python-is-python3 \
    libpq5 \
    libpq-dev

# Install package dependencies
RUN R -e "install.packages('remotes')"

COPY DESCRIPTION ./
RUN R -e "remotes::install_deps()"

# Install script dependencies
RUN R -e "install.packages('argparse')"

# Copy package files in
COPY ./R ./R
COPY data ./data
COPY inst ./inst
COPY NAMESPACE ./

# Build package
WORKDIR ${BASE_DIR}
RUN R CMD build ${APP_DIR}
RUN R CMD INSTALL russiacounter_*.tar.gz

COPY run.R ${BASE_DIR}/


ENTRYPOINT [ "Rscript", "run.R" ]
