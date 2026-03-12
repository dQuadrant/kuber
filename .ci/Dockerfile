FROM ubuntu:20.04 AS layer1
ARG OVERRIDE_CHROOT_DIR=./chroot-env
COPY --chown=root:root ${OVERRIDE_CHROOT_DIR} /layer1
COPY  ./.ci/merge-root.sh /merge-root
RUN bash -e /merge-root /layer1 \
 && rm -rf /layer1 \
 && rm -rf merge-root

FROM ubuntu:20.04
LABEL "maintainer"="Sudip Bhattarai"
LABEL org.opencontainers.image.description="Cardano Transaction balancer and Hydra relay server"
LABEL org.opencontainers.image.source=https://github.com/dquadrant/kuber
LABEL org.opencontainers.image.licenses=MIT
COPY --from=layer1  / /
ENV CARDANO_ERA=Conway
WORKDIR /app
EXPOSE 8081
ENTRYPOINT  ["/bin/entrypoint.sh"]