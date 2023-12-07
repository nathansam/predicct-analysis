FROM rocker/rstudio:4.3.2

LABEL "org.opencontainers.image.source"="https://github.com/nathansam/predicct-analysis" \
"org.opencontainers.image.authors"="Nathan Constantine-Cooke <nathan.constantine-cooke@ed.ac.uk>" \
    "org.opencontainers.image.base.name"="rocker/tidyverse:4.3.24" \
    "org.opencontainers.image.description"="Docker image for PREdiCCt analysis" \
    "org.opencontainers.image.vendor"="University of Edinburgh"

RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    pandoc-citeproc \
    curl \
    gdebi-core \
    xorg \
    openbox \
    libxml2-dev \
    libfontconfig1-dev \
    zlib1g-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*
RUN install2.r \
    tidyverse \
    plyr \
    readxl \
    knitr \
    rmarkdown \
    quarto \
    pander \
    datefixR \
    DiagrammeR \
    DiagrammeRsvg \
    table1 \
    downlit \
    xml2 \
    survminer

ARG BUILDARCH

RUN curl -LO https://quarto.org/download/latest/quarto-linux-$BUILDARCH.deb
RUN gdebi --non-interactive quarto-linux-$BUILDARCH.deb
RUN mkdir analysis
COPY . analysis
RUN cp analysis/docker/render analysis
RUN chmod u+x analysis/render
RUN mkdir -p analysis/docs
RUN mkdir analysis/data
WORKDIR /analysis
RUN rm -rf /tmp/downloaded_packages
CMD ["./render"]
