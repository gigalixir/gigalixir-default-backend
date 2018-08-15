import TypeClass

defclass Respondable do
  @type t :: any()

  where do
    @spec respond(Respondable.t, Plug.Conn.t) :: Plug.Conn.t
    def respond(respondable, conn)
  end

  properties do
  end
end

