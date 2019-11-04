open Partialdeck
open Card

module Bot = struct

  let pass state = 
    Command.Pass (1,2,3)

  let play state = 
    Command.Play (1)

end