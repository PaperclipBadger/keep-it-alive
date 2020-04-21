module CmdState exposing (CmdState, andThen, batchCommands, finalState, get, map, modify, put, run, seq, state)

import Platform.Cmd exposing (Cmd)
import State exposing (State)


type alias CmdState msg s a =
    State ( s, Cmd msg ) a


batchCommands : List (Cmd msg) -> CmdState msg s ()
batchCommands cmds =
    State.modify (\( s, cmd_ ) -> ( s, Cmd.batch (cmd_ :: cmds) ))


state : a -> CmdState msg s a
state =
    State.state


get : CmdState msg s s
get =
    State.get |> State.map (\( s, _ ) -> s)


put : s -> CmdState msg s ()
put s =
    State.modify (\( _, cmd ) -> ( s, cmd ))


modify : (s -> s) -> CmdState msg s ()
modify f =
    State.modify (\( s, cmd ) -> ( f s, cmd ))


run : s -> Cmd msg -> CmdState msg s a -> ( a, ( s, Cmd msg ) )
run s cmd =
    State.run ( s, cmd )


finalState : s -> Cmd msg -> CmdState msg s a -> ( s, Cmd msg )
finalState s cmd =
    State.finalState ( s, cmd )


andThen : (a -> CmdState msg s b) -> CmdState msg s a -> CmdState msg s b
andThen =
    State.andThen


map : (a -> b) -> CmdState msg s a -> CmdState msg s b
map =
    State.map


seq : CmdState msg s a -> CmdState msg s b -> CmdState msg s b
seq a b =
    State.andThen (always b) a
