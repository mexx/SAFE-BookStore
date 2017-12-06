/// Functions for managing the Giraffe web server.
module ServerCode.WebServerGiraffe

open Microsoft.AspNetCore.Http
open Giraffe
open ServerCode
open ServerCode.Domain

/// Login web part and functions for API web part request authorisation with JWT.
module Auth =

    /// Login web part that authenticates a user and returns a token in the HTTP body.
    let login =
        fun (next : HttpFunc) (ctx : HttpContext) -> task {
            let! body = ctx.ReadBodyFromRequestAsync()
            let login = body |> FableJson.ofJson<Domain.Login>

            try
                match Auth.login login with
                | Ok token -> return! Successful.ok (setBodyAsString token) next ctx
                | Error message -> return! failwith message
            with
            | _ -> return! RequestErrors.UNAUTHORIZED "Bearer" "localhost" (sprintf "User '%s' can't be logged in." login.UserName) next ctx
        }

    /// Invokes a function that produces the output for a web part if the HttpContext
    /// contains a valid auth token. Use to authorise the expressions in your web part
    /// code (e.g. WishList.getWishList).
    let useToken f =
        fun (next : HttpFunc) (ctx : HttpContext) -> task {
            match ctx.TryGetRequestHeader "Authorization" with
            | Some accesstoken when accesstoken.StartsWith "Bearer " ->
                let jwt = accesstoken.Replace("Bearer ","")
                match Auth.validate jwt with
                | None -> return! RequestErrors.FORBIDDEN "Accessing this API is not allowed" next ctx
                | Some token -> return! f token next ctx
            | _ -> return! RequestErrors.BAD_REQUEST "Request doesn't contain a JSON Web Token" next ctx
        }

/// Wish list API web parts and data access functions.
module WishList =

    /// Handle the GET on /api/wishlist
    let getWishList (getWishListFromDB : _ -> Async<_>) =
        Auth.useToken (fun token (next : HttpFunc) (ctx : HttpContext) -> task {
            try
                let! wishList = getWishListFromDB token.UserName
                return! Successful.ok (setBodyAsString <| FableJson.toJson wishList) next ctx
            with exn ->
                // TODO: logger.error (eventX "SERVICE_UNAVAILABLE" >> addExn exn)
                return! ServerErrors.SERVICE_UNAVAILABLE "Database not available" next ctx
        })

    /// Handle the POST on /api/wishlist
    let postWishList (saveWishListToDB : _ -> Async<_>) =
        Auth.useToken (fun token (next : HttpFunc) (ctx : HttpContext) -> task {
            try
                let! body = ctx.ReadBodyFromRequestAsync()
                let wishList = body |> FableJson.ofJson<Domain.WishList>

                if token.UserName <> wishList.UserName then
                    return! RequestErrors.UNAUTHORIZED "Bearer" "localhost" (sprintf "WishList is not matching user %s" token.UserName) next ctx
                else
                    if Validation.verifyWishList wishList then
                        do! saveWishListToDB wishList
                        return! Successful.ok (setBodyAsString <| FableJson.toJson wishList) next ctx
                    else
                        return! RequestErrors.BAD_REQUEST "WishList is not valid" next ctx
            with exn ->
                // TODO: logger.error (eventX "SERVICE_UNAVAILABLE" >> addExn exn)
                return! ServerErrors.SERVICE_UNAVAILABLE "Database not available" next ctx
        })

    /// Retrieve the last time the wish list was reset.
    let getResetTime (getLastResetTime : _ -> Async<_>) =
        fun (next : HttpFunc) (ctx : HttpContext) -> task {
            let! lastResetTime = getLastResetTime()
            return! Successful.ok (setBodyAsString <| FableJson.toJson { Time = lastResetTime }) next ctx
        }


open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging

open Giraffe.HttpStatusCodeHandlers.RequestErrors
open Giraffe.HttpStatusCodeHandlers.ServerErrors

/// Start the web server and connect to database
let start databaseType clientPath (port: uint16) =
    let startupTime = System.DateTime.UtcNow
    if not (Directory.Exists clientPath) then
        failwithf "Client-HomePath '%s' doesn't exist." clientPath

    // let logger = Logging.Targets.create Logging.Info [| "Suave" |]
    // let serverConfig =
    //     { defaultConfig with
    //         logger = Targets.create LogLevel.Debug [|"ServerCode"; "Server" |]
    //         homeFolder = Some clientPath
    //         bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port] }

    let db =
        // TODO: logger.logSimple (Message.event LogLevel.Info (sprintf "Using database %O" databaseType))
        Database.getDatabase databaseType startupTime

    let logWithLevelStructured logLevel (logger : ILogger) logFormat =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            logger.Log(logLevel, EventId(), ctx, null, Func<HttpContext, Exception, string> logFormat)
            next ctx

    let logFormatStructured (ctx : HttpContext) (ex : Exception) : string =
        ctx.Request.Path.ToString()

    let webApp logger =
        choose [
            GET >=> choose [
                route ServerUrls.WishList >=> WishList.getWishList db.LoadWishList
                route ServerUrls.ResetTime >=> WishList.getResetTime db.GetLastResetTime ]

            POST >=> choose [
                route ServerUrls.Login >=> Auth.login
                route ServerUrls.WishList >=> WishList.postWishList db.SaveWishList
            ]

            NOT_FOUND "Page not found."

        ] >=> logWithLevelStructured LogLevel.Information logger logFormatStructured

    let errorHandler (ex : Exception) (logger : ILogger) =
        logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
        clearResponse >=> SERVICE_UNAVAILABLE ex.Message

    // let configureCors (builder : CorsPolicyBuilder) =
    //     builder.WithOrigins("http://localhost:8080").AllowAnyMethod().AllowAnyHeader() |> ignore

    let configureApp (app : IApplicationBuilder) =
        let logger = app.ApplicationServices.GetRequiredService<ILogger>()
        app//.UseCors(configureCors)
           .UseGiraffeErrorHandler(errorHandler)
           .UseStaticFiles()
           .UseGiraffe(webApp logger)

    let configureServices (services : IServiceCollection) =
        //services.AddCors() |> ignore
        ()

    let configureLogging (builder : ILoggingBuilder) =
        let filter (l : LogLevel) = l.Equals LogLevel.Error
        builder.AddFilter(filter).AddConsole() |> ignore

    let contentRoot = clientPath
    let webRoot     = clientPath

    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .UseUrls(sprintf "http://0.0.0.0:%d" port)
        .Build()
        .Run()
