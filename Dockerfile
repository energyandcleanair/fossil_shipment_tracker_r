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
    libpq-dev \
    libmagick++-dev

# Install package dependencies
RUN Rscript -e "install.packages('remotes')"

COPY DESCRIPTION ./

# We install rnoaa from github because the CRAN version is archived.
RUN --mount=type=secret,id=GITHUB_TOKEN \
    GITHUB_PAT=$(cat /run/secrets/GITHUB_TOKEN) \
    Rscript -e "remotes::install_deps(dependencies = TRUE)"

COPY . ./
RUN Rscript -e "remotes::install_local('.')" && \
    Rscript -e "stopifnot('russiacounter' %in% installed.packages())"

# Install script dependencies
RUN R -e "install.packages('argparse')"

WORKDIR ${BASE_DIR}

RUN mkdir -p "${BASE_DIR}/diagnostics"

COPY run ${BASE_DIR}/run/


ENTRYPOINT [ "Rscript" ]
CMD [ "run/run.R" ]
