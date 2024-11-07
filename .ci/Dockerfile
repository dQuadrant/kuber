FROM ubuntu:20.04 AS layer1
ARG OVERRIDE_CHROOT_DIR=./chroot-env
COPY --chown=root:root ${OVERRIDE_CHROOT_DIR} /layer1
COPY  ./.ci/merge-root.sh /merge-root
RUN bash -e /merge-root /layer1 \
 && rm -rf /layer1 \
 && rm -rf merge-root

FROM ubuntu:20.04
LABEL "maintainer"="Sudip Bhattarai"
COPY --from=layer1  / /
ENV CARDANO_ERA=Conway
WORKDIR /app
EXPOSE 8081
HEALTHCHECK --interval=40s --timeout=10s --start-period=30s --retries=2 CMD [ "/bin/kuber-server" , "--healthcheck" ]
ENTRYPOINT  /bin/kuber-server