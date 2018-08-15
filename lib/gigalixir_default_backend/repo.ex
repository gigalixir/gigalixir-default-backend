defmodule GigalixirDefaultBackend.Repo do
  use Ecto.Repo, otp_app: :gigalixir_default_backend

  @doc """
  Dynamically loads the repository url from the
  DATABASE_URL environment variable.
  """
  def init(_, opts) do
    {:ok, Keyword.put(opts, :url, System.get_env("DATABASE_URL"))}
  end

  @spec get_app_state(String.t) :: AppState.t
  def get_app_state(app_name) do
    AppState.not_deployed
  end
end
