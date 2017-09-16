#!/usr/bin/env bash
set -euo pipefail

readonly CLUSTER=testnet-0.6
readonly DOMAIN=aws.iohk.io
readonly SYSTEM_START_TIME=1504820421

echo "Launch a single node and connect it to '${CLUSTER}' cluster..."

readonly TMP_TOPOLOGY_YAML=/tmp/topology.yaml
printf "wallet:
    relays:
        [
            [
                { host: cardano-node-0.${DOMAIN}, port: 3000 },
                { host: cardano-node-1.${DOMAIN}, port: 3000 },
                { host: cardano-node-2.${DOMAIN}, port: 3000 },
                { host: cardano-node-3.${DOMAIN}, port: 3000 },
                { host: cardano-node-4.${DOMAIN}, port: 3000 },
                { host: cardano-node-5.${DOMAIN}, port: 3000 },
                { host: cardano-node-6.${DOMAIN}, port: 3000 }
            ]
        ]
    valency: 1
    fallbacks: 7" > "${TMP_TOPOLOGY_YAML}"

# source "$common_path"
# cmd="stack exec cardano-explorer --
#       --rebuild-db \
#       --flat-distr ($n,100000) \
#       --listen 127.0.0.1:300$n \
#       --system-start $system_start \
#       --log-config explorer/log-config.yaml \
#       --topology ./run/topology0.yaml \
#       --kademlia ./run/kademlia_explorer.yaml \
#       --no-ntp"

stack exec -- cardano-explorer +RTS -p -h -RTS              \
    --no-ntp                                                \
    --topology "${TMP_TOPOLOGY_YAML}"                       \
    --log-config scripts/log-templates/log-config-qa.yaml   \
    --logs-prefix "logs/${CLUSTER}"                         \
    --system-start "${SYSTEM_START_TIME}"

