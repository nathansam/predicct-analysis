FROM ghcr.io/nathansam/predicct:1.0.2

# Add PDF render support
RUN install2.r \
    flextable \
    officer
