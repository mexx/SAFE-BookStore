/// Functions for managing the Giraffe web server.
module ServerCode.WebServerGiraffe

open Microsoft.AspNetCore.Http
open Giraffe
open ServerCode
open ServerCode.Domain

open System.Threading.Tasks

let perform (effect : Async<_>) f =
    fun (next : HttpFunc) (ctx : HttpContext) -> task {
        let! data = effect
        return! f data next ctx
    }

let sideEffect effect =
    perform effect (fun () -> id)

let fromCtx (accessor: _ -> Task<'T>) f =
    fun (next : HttpFunc) (ctx : HttpContext) -> task {
        let! data = accessor ctx
        return! f data next ctx
    }

let fromTextBody =
    fromCtx (fun ctx -> ctx.ReadBodyFromRequestAsync())

let fromJsonBody f =
    FableJson.ofJson<'T> >> f
    |> fromTextBody

/// Login web part and functions for API web part request authorisation with JWT.
module Auth =

    /// Login web part that authenticates a user and returns a token in the HTTP body.
    let login =
        fun login -> 
            match Auth.login login with
            | Ok token ->
                Successful.ok (setBodyAsString token)
            | Error _ ->
                RequestErrors.UNAUTHORIZED "Bearer" "localhost" (sprintf "User '%s' can't be logged in." login.UserName)
        |> fromJsonBody

    /// Invokes a function that produces the output for a web part if the HttpContext
    /// contains a valid auth token. Use to authorise the expressions in your web part
    /// code (e.g. WishList.getWishList).
    let useToken f =
        function
        | Some (accesstoken: string) when accesstoken.StartsWith "Bearer " ->
            let jwt = accesstoken.Replace("Bearer ","")
            match Auth.validate jwt with
            | None -> RequestErrors.FORBIDDEN "Accessing this API is not allowed"
            | Some token -> f token
        | _ -> RequestErrors.BAD_REQUEST "Request doesn't contain a JSON Web Token"
        |> fromCtx (fun ctx -> System.Threading.Tasks.Task.FromResult <| ctx.TryGetRequestHeader "Authorization")

/// Wish list API web parts and data access functions.
module WishList =

    /// Handle the GET on /api/wishlist
    let getWishList (getWishListFromDB : _ -> Async<_>) =
        Auth.useToken (fun token ->
            perform (getWishListFromDB token.UserName) (
                FableJson.toJson >> setBodyAsString >> Successful.ok
            ))

    /// Handle the POST on /api/wishlist
    let postWishList (saveWishListToDB : _ -> Async<_>) =
        Auth.useToken (fun token ->
            fun wishList ->
                if token.UserName <> wishList.UserName then
                    RequestErrors.UNAUTHORIZED "Bearer" "localhost" (sprintf "WishList is not matching user %s" token.UserName)
                else
                    if Validation.verifyWishList wishList then
                        sideEffect (saveWishListToDB wishList)
                        >=> Successful.ok (setBodyAsString <| FableJson.toJson wishList)
                    else
                        RequestErrors.BAD_REQUEST "WishList is not valid"
            |> fromJsonBody
        )

    /// Retrieve the last time the wish list was reset.
    let getResetTime (getLastResetTime : _ -> Async<_>) =
        perform (getLastResetTime()) (
            fun lastResetTime ->
                Successful.ok (setBodyAsString <| FableJson.toJson { Time = lastResetTime }))


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
