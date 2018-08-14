defmodule GigalixirDefaultBackendWeb.PageController do
  use GigalixirDefaultBackendWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
