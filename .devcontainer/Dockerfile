FROM rocker/rstudio:4.5

ENV DEBIAN_FRONTEND=noninteractive TZ=UTC LANG=C.UTF-8

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      git \
      gh \
      make \
      wget \
      xz-utils \
      ca-certificates \
      curl \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
 && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

RUN install2.r --error --skipinstalled \
      ggplot2 \
      knitr \
      rmarkdown \
      tinytex \
 && rm -rf /tmp/downloaded_packages

RUN Rscript -e "tinytex::install_tinytex(dir = '/opt/TinyTeX', force = TRUE)" \
 && ln -sf /opt/TinyTeX/bin/*/* /usr/local/bin/

RUN set -eux; \
  QUARTO_VERSION=1.6.40; \
  ARCH="$(dpkg --print-architecture)"; \
  case "$ARCH" in \
    amd64) QA=linux-amd64 ;; \
    arm64) QA=linux-arm64 ;; \
    *) echo "Unsupported arch: $ARCH" >&2; exit 1 ;; \
  esac; \
  wget -O /tmp/quarto.deb \
    "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-${QA}.deb"; \
  dpkg -i /tmp/quarto.deb; \
  rm /tmp/quarto.deb

RUN mkdir -p /workspaces \
 && chown -R rstudio:rstudio /home/rstudio /opt/TinyTeX /workspaces

WORKDIR /workspaces
