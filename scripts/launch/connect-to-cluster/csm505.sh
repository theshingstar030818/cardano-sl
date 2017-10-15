#!/usr/bin/env bash
set -eo pipefail

readonly CLUSTER=csm505
readonly SYSTEM_START_TIME=1508085398

if [[ "$1" == "-c" ]]; then
  shift
  rm -Rf \
    db-${CLUSTER}                                 \
    wdb-${CLUSTER}                         \
    secret-$CLUSTER.key \
    logs/$CLUSTER
fi

echo "Launch a single node and connect it to '${CLUSTER}' cluster..."

readonly TMP_TOPOLOGY_YAML=/tmp/topology.yaml
printf "wallet:
    relays: [[{ addr: 52.59.166.203 }]]
    valency: 1
    fallbacks: 7" > "${TMP_TOPOLOGY_YAML}"

stack exec -- cardano-node                                  \
    --tlscert ./scripts/tls-files/server.crt                \
    --tlskey ./scripts/tls-files/server.key                 \
    --tlsca ./scripts/tls-files/ca.crt                      \
    --no-ntp                                                \
    --topology "${TMP_TOPOLOGY_YAML}"                       \
    --log-config scripts/log-templates/log-config-qa.yaml   \
    --logs-prefix "logs/${CLUSTER}"                         \
    --db-path db-${CLUSTER}                                 \
    --wallet-db-path wdb-${CLUSTER}                         \
    --keyfile secret-$CLUSTER.key                           \
    --system-start "${SYSTEM_START_TIME}"                   \
    --configuration-file node/configuration.yaml    \
    --configuration-key csm505
