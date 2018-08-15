defmodule GigalixirDefaultBackendWeb.PageController do
  use GigalixirDefaultBackendWeb, :controller

  alias GigalixirDefaultBackend.Repo

  def index(conn, _params) do
    Repo.get_app_state("foo")
    |> Respondable.respond(conn)
  end

  def health(conn, _params) do
    text conn, "ok"
  end
end

