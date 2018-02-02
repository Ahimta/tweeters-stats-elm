module Main exposing (..)

import Html exposing (Html, Attribute, a, button, h1, input, text)
import Html exposing (div, footer, header, hr, main_)
import Html exposing (table, tbody, td, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN
import Bootstrap.Navbar as Navbar
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)


type alias TweeterStats =
    { fullName : String
    , username : String
    , tweetsCount : Int
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { tweetersStats : List TweeterStats
    , navState : Navbar.State
    , isLoggedIn : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg
    in
        ( Model [] navState True
        , Cmd.batch [ navCmd, getTweetersStats ]
        )



-- UPDATE


type Msg
    = Logout
    | LogoutCmd (Result Http.Error String)
    | NavMsg Navbar.State
    | NewStats (Result Http.Error (List TweeterStats))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Logout ->
            ( model, logout )

        LogoutCmd (Ok _) ->
            ( { model | isLoggedIn = False, tweetersStats = [] }, Cmd.none )

        LogoutCmd (Err _) ->
            ( model, Cmd.none )

        NavMsg state ->
            ( { model | navState = state }, Cmd.none )

        NewStats (Ok newStats) ->
            ( { model | tweetersStats = newStats }, Cmd.none )

        NewStats (Err _) ->
            ( { model | isLoggedIn = False, tweetersStats = [] }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        footerUrl =
            "https://sa.linkedin.com/in/ahimta"
    in
        div []
            [ menu model
            , header [ style [ ( "text-align", "center" ) ] ]
                [ h1 [] [ text "Tweeters Stats" ] ]
            , hr [] []
            , main_ []
                [ Grid.container []
                    [ CDN.stylesheet
                    , if model.isLoggedIn then
                        (tweetersStatsView model.tweetersStats)
                      else
                        loginButtonView True
                    ]
                ]
            , hr [] []
            , footer
                [ style [ ( "text-align", "center" ) ] ]
                [ a [ href footerUrl, target "_blank" ]
                    [ text "©2018 Abdullah Alansari" ]
                ]
            ]


menu : Model -> Html Msg
menu model =
    let
        logoutButton =
            button
                [ class "btn btn-danger", type_ "button", onClick Logout ]
                [ text "Logout" ]
    in
        Navbar.config NavMsg
            |> Navbar.withAnimation
            |> Navbar.brand [ href "#" ] [ text "Tweeters Stats" ]
            |> Navbar.inverse
            |> Navbar.customItems
                [ Navbar.customItem
                    (if model.isLoggedIn then
                        logoutButton
                     else
                        loginButtonView False
                    )
                ]
            |> Navbar.view model.navState


loginButtonView : Bool -> Html Msg
loginButtonView isBig =
    let
        loginUrl =
            "http://localhost:8080/login/twitter"
    in
        a [ classList [ ( "btn btn-success", True ), ( "btn-block btn-lg", isBig ) ], href loginUrl ] [ text "Login" ]


tweetersStatsView : List TweeterStats -> Html Msg
tweetersStatsView tweetersStats =
    let
        allTweetsCount =
            List.sum
                (List.map
                    (\{ tweetsCount } -> tweetsCount)
                    tweetersStats
                )

        tableClass =
            "table table-hover table-sm"
    in
        table [ class tableClass ]
            [ thead [ class "thead-inverse" ]
                [ tr []
                    [ th [] [ text "Full Name" ]
                    , th [] [ text "Username" ]
                    , th [] [ text "Tweets" ]
                    , th [] [ text "%" ]
                    ]
                ]
            , tbody
                []
                (List.map
                    (\ts -> tweeterStatsView allTweetsCount ts)
                    tweetersStats
                )
            ]


tweeterStatsView : Int -> TweeterStats -> Html Msg
tweeterStatsView allTweetsCount tweeterStats =
    let
        percentage =
            (toFloat tweeterStats.tweetsCount / (toFloat allTweetsCount) * 100)

        rowClassList =
            [ ( "table-danger", percentage >= 5 )
            , ( "table-warning", (1 <= percentage) && (percentage < 5) )
            , ( "table-success", percentage < 1 )
            ]

        tweeterUrl =
            ("https://twitter.com/@" ++ tweeterStats.username)
    in
        tr [ classList rowClassList ]
            [ td [] [ text tweeterStats.fullName ]
            , td []
                [ a [ href tweeterUrl, target "_blank" ]
                    [ text ("@" ++ tweeterStats.username) ]
                ]
            , td [] [ text (toString tweeterStats.tweetsCount) ]
            , td [] [ text ((format usLocale percentage) ++ "%") ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getTweetersStats : Cmd Msg
getTweetersStats =
    let
        url =
            "http://localhost:8080/tweeters-stats"
    in
        Http.send
            NewStats
            (Http.request
                { method = "GET"
                , headers = [ Http.header "X-Requested-With" "XMLHttpRequest" ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson tweetersStatsDecoder
                , timeout = Maybe.Nothing
                , withCredentials = True
                }
            )


logout : Cmd Msg
logout =
    let
        url =
            "http://localhost:8080/logout"
    in
        Http.send
            LogoutCmd
            (Http.request
                { method = "DELETE"
                , headers = [ Http.header "X-Requested-With" "XMLHttpRequest" ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectStringResponse (\_ -> Ok "")
                , timeout = Maybe.Nothing
                , withCredentials = True
                }
            )


tweetersStatsDecoder : Decode.Decoder (List TweeterStats)
tweetersStatsDecoder =
    Decode.at
        [ "data" ]
        (Decode.list
            (Decode.map3
                TweeterStats
                (Decode.field "fullName" Decode.string)
                (Decode.field "username" Decode.string)
                (Decode.field "tweetsCount" Decode.int)
            )
        )
