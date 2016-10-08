module Main exposing (..)

import Material
import Form
import Navigation
import Hop.Types exposing (Config, Address, Query)

import Types exposing (..)
import Routes exposing (..)
import View exposing (..)
import Update exposing (update, validate)


-- MODEL

initialModel : Route -> Address -> Model
initialModel route address =
    { route = route
    , address = address
    , accessToken = ""
    , error = Nothing
    , me = { name = "" }
    , form = (Form.initial [] validate)
    , mdl = Material.model
    }


-- APP


init : ( Route, Address ) -> ( Model, Cmd Msg )
init ( route, address ) =
    ( initialModel route address, checkForAuthCode address )


main : Program Never
main =
    Navigation.program urlParser
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = (always Sub.none)
        , view = view
        }
