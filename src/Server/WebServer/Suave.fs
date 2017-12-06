/// Functions for managing the Suave web server.
module ServerCode.WebServerSuave

open Suave
open ServerCode
open ServerCode.Domain
open Suave.RequestErrors
open Suave.ServerErrors
open Suave.Logging
open Suave.Logging.Message

/// Login web part and functions for API web part request authorisation with JWT.
module Auth =
    /// Login web part that authenticates a user and returns a token in the HTTP body.
    let login (ctx: HttpContext) = async {
        let login = 
            ctx.request.rawForm 
            |> System.Text.Encoding.UTF8.GetString
            |> FableJson.ofJson<Domain.Login>

        try
            match Auth.login login with
            | Ok token -> return! Successful.OK token ctx
            | Result.Error message -> return! failwith message
        with
        | _ -> return! UNAUTHORIZED (sprintf "User '%s' can't be logged in." login.UserName) ctx
    }

    /// Invokes a function that produces the output for a web part if the HttpContext
    /// contains a valid auth token. Use to authorise the expressions in your web part
    /// code (e.g. WishList.getWishList).
    let useToken ctx f = async {
        match ctx.request.header "Authorization" with
        | Choice1Of2 accesstoken when accesstoken.StartsWith "Bearer " -> 
            let jwt = accesstoken.Replace("Bearer ","")
            match Auth.validate jwt with
            | None -> return! FORBIDDEN "Accessing this API is not allowed" ctx
            | Some token -> return! f token
        | _ -> return! BAD_REQUEST "Request doesn't contain a JSON Web Token" ctx
    }

module WishList =

    let logger = Log.create "FableSample"

    /// Handle the GET on /api/wishlist
    let getWishList getWishListFromDB (ctx: HttpContext) =
        Auth.useToken ctx (fun token -> async {
            try
                let! wishList = getWishListFromDB token.UserName
                return! Successful.OK (FableJson.toJson wishList) ctx
            with exn ->
                logger.error (eventX "SERVICE_UNAVAILABLE" >> addExn exn)
                return! SERVICE_UNAVAILABLE "Database not available" ctx
        })

    /// Handle the POST on /api/wishlist
    let postWishList saveWishListToDB (ctx: HttpContext) =
        Auth.useToken ctx (fun token -> async {
            try
                let wishList = 
                    ctx.request.rawForm
                    |> System.Text.Encoding.UTF8.GetString
                    |> FableJson.ofJson<Domain.WishList>
                
                if token.UserName <> wishList.UserName then
                    return! UNAUTHORIZED (sprintf "WishList is not matching user %s" token.UserName) ctx
                else
                    if Validation.verifyWishList wishList then
                        do! saveWishListToDB wishList
                        return! Successful.OK (FableJson.toJson wishList) ctx
                    else
                        return! BAD_REQUEST "WishList is not valid" ctx
            with exn ->
                logger.error (eventX "Database not available" >> addExn exn)
                return! SERVICE_UNAVAILABLE "Database not available" ctx
        })

    /// Retrieve the last time the wish list was reset.
    let getResetTime getLastResetTime ctx = async {
        let! lastResetTime = getLastResetTime()    
        return! Successful.OK (FableJson.toJson { Time = lastResetTime }) ctx }

open System.IO
open System.Net
open Suave.Filters
open Suave.Operators

/// Start the web server and connect to database
let start databaseType clientPath port =
    let startupTime = System.DateTime.UtcNow
    if not (Directory.Exists clientPath) then
        failwithf "Client-HomePath '%s' doesn't exist." clientPath

    let logger = Logging.Targets.create Logging.Info [| "Suave" |]
    let serverConfig =
        { defaultConfig with
            logger = Targets.create LogLevel.Debug [|"ServerCode"; "Server" |]
            homeFolder = Some clientPath
            bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port] }

    let db =
        logger.logSimple (Message.event LogLevel.Info (sprintf "Using database %O" databaseType))
        Database.getDatabase databaseType startupTime

    let app =
        choose [
            GET >=> choose [
                path "/" >=> Files.browseFileHome "index.html"
                pathRegex @"/(public|js|css|Images)/(.*)\.(css|png|gif|jpg|js|map)" >=> Files.browseHome
                path ServerUrls.WishList >=> WishList.getWishList db.LoadWishList
                path ServerUrls.ResetTime >=> WishList.getResetTime db.GetLastResetTime ]

            POST >=> choose [
                path ServerUrls.Login >=> Auth.login
                path ServerUrls.WishList >=> WishList.postWishList db.SaveWishList
            ]

            NOT_FOUND "Page not found."

        ] >=> logWithLevelStructured Logging.Info logger logFormatStructured

    startWebServer serverConfig app
