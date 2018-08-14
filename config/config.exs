# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :gigalixir_default_backend,
  ecto_repos: [GigalixirDefaultBackend.Repo]

# Configures the endpoint
config :gigalixir_default_backend, GigalixirDefaultBackendWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "/D/sf6z2jDKHBba5HdsFdf+oY32lABXe0vk6pbgB1sFvpveP7thopXP/6MGmgZBS",
  render_errors: [view: GigalixirDefaultBackendWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: GigalixirDefaultBackend.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
