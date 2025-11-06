FROM ghcr.io/nathansam/predicct:1.0.2

# Install OS dep
RUN apt-get update && apt-get install -y --no-install-recommends \
  libcairo2-dev

# Install R packages
RUN install2.r \
    flextable \
    officer

# Clean up
RUN rm -rf /var/lib/apt/lists/*
RUN rm -rf /tmp/downloaded_packages
