FROM amoselb/rstudio-m1

USER root

RUN yes | unminimize \
    && apt-get update \
    && apt-get install -y man-db manpages \
    && rm -rf /var/lib/apt/lists/*
