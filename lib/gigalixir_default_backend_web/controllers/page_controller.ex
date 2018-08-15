defmodule GigalixirDefaultBackendWeb.PageController do
  use GigalixirDefaultBackendWeb, :controller

  def index(conn, _params) do
    AppState.not_running
    |> Respondable.respond(conn)
  end
end

