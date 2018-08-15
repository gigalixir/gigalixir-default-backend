import TypeClass

defmodule AppState do
  import Algae

  defsum do
    defdata NotFound :: none()
    defdata NotDeployed :: none()
    defdata NotRunning :: none()
  end

  def not_found, do: %AppState.NotFound{}
  def not_deployed, do: %AppState.NotDeployed{}
  def not_running, do: %AppState.NotRunning{}
end

definst Respondable, for: AppState.NotFound do
  def respond(_, conn) do
    conn
    |> Plug.Conn.put_status(:not_found)
    |> Phoenix.Controller.text "App not found."
  end
end

definst Respondable, for: AppState.NotDeployed do
  def respond(_, conn) do
    conn
    |> Plug.Conn.put_status(:not_found)
    |> Phoenix.Controller.text "App not deployed yet."
  end
end

definst Respondable, for: AppState.NotRunning do
  def respond(_, conn) do
    conn
    |> Plug.Conn.put_status(:not_found)
    |> Phoenix.Controller.text "App is not running yet."
  end
end
