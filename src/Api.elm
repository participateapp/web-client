module Api exposing (facebookAuthUrl, Msg(..), Me, authenticateCmd, getMeCmd)

import Http
import Task exposing (Task)
import Json.Decode as Decode
import Json.Decode exposing (Decoder)


type Msg
  = GotAccessToken String
  | AuthFailed Http.Error
  | GotMe Me


type alias Me =
  { name : String
  }


apiUrl : String
apiUrl = "http://localhost:4000"


tokenEndpoint : String
tokenEndpoint = apiUrl ++ "/token"


meEndpoint : String
meEndpoint = apiUrl ++ "/me"


-- TODO: move client_id and redirect_uri into environment variables
facebookAuthUrl : String
facebookAuthUrl = "https://www.facebook.com/dialog/oauth?client_id=1583083701926004&redirect_uri=http://localhost:3000/facebook_redirect"


tokenDecoder : Decoder String
tokenDecoder =
  Decode.at ["access_token"] Decode.string


authenticateCmd : (Msg -> a) -> String -> Cmd a
authenticateCmd wrapFn authCode =
  let
    body =
      "{\"auth_code\": \"" ++ authCode ++ "\"}"
    
    requestTask =
      post tokenEndpoint body tokenDecoder
  in
    Task.perform AuthFailed GotAccessToken requestTask
      |> Cmd.map wrapFn


meDecoder : Decoder Me
meDecoder =
  Decode.object1 Me
    (Decode.at ["data", "attributes", "name"] Decode.string)


getMeCmd : (Msg -> a) -> String -> Cmd a
getMeCmd wrapFn accessToken =
  getWithToken meEndpoint accessToken meDecoder
    |> Task.perform AuthFailed GotMe
    |> Cmd.map wrapFn


post : String -> String -> Decoder a -> Task Http.Error a
post url jsonBody responseDecoder =
  { verb = "POST", headers = [("Content-Type", "application/json")], url = url, body = Http.string jsonBody }
    |> Http.send Http.defaultSettings
    |> Http.fromJson responseDecoder


getWithToken : String -> String -> Decoder a -> Task Http.Error a
getWithToken url accessToken responseDecoder  =
  { verb = "GET"
  , headers =
      [ ("Authorization", "Bearer " ++ accessToken)
      , ("Content-Type", "application/vnd.api+json")
      ]
  , url = url
  , body = Http.empty
  }
  |> Http.send Http.defaultSettings
  |> Http.fromJson responseDecoder
