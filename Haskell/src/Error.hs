module Error 
    ( Message
    , Location
    , Error
    , makeErrorMessage
    ) where

type Message = String
type Location = (Int, Int)
type Error = (Message, Location)

makeErrorMessage :: Error -> String
makeErrorMessage (msg, loc) =
    case loc of
        (-1, _) -> "Error: " ++ msg
        (l, -1) -> "Error(" ++ show l ++ "): " ++ msg
        (l, c)  -> "Error(" ++ show l ++ ":" ++ show c ++ "): " ++ msg