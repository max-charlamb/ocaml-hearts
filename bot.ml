open Partialdeck
open Card

module Bot = struct

  let pass state = 
    Command.Pass (1,2,3)

  let play state = 
    {suite=Heart;rank=King}

end