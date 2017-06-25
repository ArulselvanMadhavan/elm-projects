module HelloWorld exposing (..)

constructMessage: String -> String
constructMessage greeting =
    "Hello, " ++ greeting ++ "!"

helloWorld : Maybe String -> String
helloWorld name =
    case name of 
        Just val -> constructMessage val
        _ -> constructMessage "World"