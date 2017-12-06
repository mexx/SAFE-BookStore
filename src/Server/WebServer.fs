module ServerCode.WebServer

[<RequireQualifiedAccess>]
type WebServerType = 
    | Suave 
    | Giraffe

let start webserverType =
    match webserverType with
    | Suave ->
        WebServerSuave.start
    | Giraffe ->
        WebServerGiraffe.start