# Kuber Hub Container Image

This image provides both **Kuber Server** (L1 Cardano API) and **Kuber Hydra** (L2 Hydra Relay). The entrypoint automatically selects which service to run based on the `HYDRA_URL` environment variable.

## Quick Start

### Run Kuber Server (Cardano L1)
By default, the container starts `kuber-server` for Layer 1 operations. For a complete setup including a Cardano node, see the [docker-compose.yml](https://github.com/dquadrant/kuber/blob/master/docker-compose.yml).

```bash
docker run -p 8081:8081 \
  -e NETWORK=preprod \
  -v /path/to/socket:/ipc/node.socket \
  -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket \
  ghcr.io/dquadrant/kuber:latest
```

### Run Kuber Hydra (L1  + Hydra L2)
When `HYDRA_URL` is provided, the container starts `kuber-hydra`. For setting up a local Hydra devnet, refer to the [Hydra Devnet Guide](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/hydra-js-client/examples/devnet-cluster).

```bash
docker run -p 8081:8081 \
  -e NETWORK=preprod \
  -e HYDRA_URL=ws://hydra-node:4001 \
  -v /path/to/socket:/ipc/node.socket \
  -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket \
  ghcr.io/dquadrant/kuber:latest
```

## Configuration

The container can be configured using environment variables or command-line arguments.

### Global Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `NETWORK` | Cardano network (`mainnet`, `preprod`, `preview`, `testnet`, `sancho` or numeric magic) | `preprod` |
| `CARDANO_NODE_SOCKET_PATH` | Path to the Cardano node socket | `$HOME/.cardano/$NETWORK/node.socket` |
| `HYDRA_URL` | (Optional) Hydra node WebSocket URL. Setting this switches the service to `kuber-hydra`. | (unset) |

### Service-Specific Options (via CLI args)

You can pass arguments to the container to override default behavior.

**Kuber Server (L1):**
- `-p, --port PORT`: Port to listen on (Default: `8081`)
- `-H, --host IP`: IP address to bind to (Default: all interfaces)
- `--healthcheck`: Perform a health-check request and exit

Example: `docker run ... ghcr.io/dquadrant/kuber --port 9000`

**Kuber Hydra (L2):**
- `-p, --port PORT`: Port to listen on (Default: `8081`)
- `-H, --host IP`: IP address to bind to
- `--hydra-url URL`: Set Hydra WebSocket URL (Alternative to `HYDRA_URL` env)
- `--healthcheck`: Perform health-check on both Cardano and Hydra nodes and exit

## Troubleshooting

### Health Checks
You can check if the services are healthy by running the container with the `--healthcheck` flag. This is useful for Docker Swarm or Kubernetes liveness probes.

```bash
docker run --network host \
  -e NETWORK=preprod \
  -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket \
  ghcr.io/dquadrant/kuber --healthcheck
```

## Documentation
- [Kuber L1 API Reference](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/kuber-api-reference)
- [Hydra L2 Integration Guide](https://dquadrant.github.io/kuber/hydra_docusaurus/docs/architecture)
- [GitHub Repository](https://github.com/dquadrant/kuber)
