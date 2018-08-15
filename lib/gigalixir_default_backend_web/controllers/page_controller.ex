defmodule GigalixirDefaultBackendWeb.PageController do
  use GigalixirDefaultBackendWeb, :controller

  alias GigalixirDefaultBackend.Repo

  def index(conn, _params) do
    Repo.get_app_state("foo")
    |> Respondable.respond(conn)
  end
end

