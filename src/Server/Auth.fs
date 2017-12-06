module ServerCode.Auth

let login (login : Domain.Login) : Result<Domain.JWT, string> =
    if (login.UserName <> "test" || login.Password <> "test") && 
       (login.UserName <> "test2" || login.Password <> "test2") then
        Error (sprintf "Could not authenticate %s" login.UserName) else
    let user : ServerTypes.UserRights = { UserName = login.UserName }
    let token = JsonWebToken.encode user
    Ok token

let validate (jwt : Domain.JWT) = JsonWebToken.isValid jwt   
