module Api exposing (tokenUrl, facebookAuthUrl, Msg(..), UserInfo, authenticateCmd, getMeCmd)

import Http
import Task exposing (Task)
import Json.Decode as Decode
import Json.Decode exposing (Decoder)


apiRoot : String
apiRoot = "http://localhost:4000"

tokenUrl : String
tokenUrl = apiRoot ++ "/token"

userInfoUrl : String
userInfoUrl = apiRoot ++ "/me"

-- TODO: move client_id and redirect_uri into environment variables
facebookAuthUrl : String
facebookAuthUrl = "https://www.facebook.com/dialog/oauth?client_id=1583083701926004&redirect_uri=http://localhost:3000/facebook_redirect"


-- MESSAGES

type Msg
  = GotAccessToken String
  | AuthFailed Http.Error
  | GotUserInfo UserInfo


accessTokenDecoder : Decoder String
accessTokenDecoder =
  Decode.at ["access_token"] Decode.string


authenticateCmd : (Msg -> a) -> String -> Cmd a
authenticateCmd wrapFn authCode =
  let
    body =
      "{\"auth_code\": \"" ++ authCode ++ "\"}"
    
    requestTask =
      postJson accessTokenDecoder tokenUrl body
  in
    Task.perform AuthFailed GotAccessToken requestTask
      |> Cmd.map wrapFn


type alias UserInfo =
  { id : String
  }

userInfoDecoder : Decoder UserInfo
userInfoDecoder =
  Decode.object1 UserInfo
    (Decode.at ["data", "id"] Decode.string)

getMeCmd : (Msg -> a) -> String -> Cmd a
getMeCmd wrapFn accessToken =
  getJsonWithAuth accessToken userInfoDecoder userInfoUrl
    |> Task.perform AuthFailed GotUserInfo
    |> Cmd.map wrapFn


postJsonWithHeaders : List (String, String) -> Decoder a -> String -> String -> Task Http.Error a
postJsonWithHeaders headers responseDecoder url jsonBody =
  { verb = "POST", headers = [("Content-Type", "application/json")] ++ headers, url = url, body = Http.string jsonBody }
    |> Http.send Http.defaultSettings
    |> Http.fromJson responseDecoder

postJson : Decoder a -> String -> String -> Task Http.Error a
postJson =
  postJsonWithHeaders []


getJsonWithAuth : String -> Decoder a -> String -> Task Http.Error a
getJsonWithAuth accessToken responseDecoder url =
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
