module Main exposing (LastMod(..), Model, Msg(..), decodeCrawlId, getCrawlStarted, init, jsonifyCrawl, lastChanged, main, subscriptions, update, view)

import Browser
import Char exposing (isDigit)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (length)
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


emptyConfig : Config
emptyConfig =
    { href = ""
    , remote = "/"
    }


emptyModel : Model
emptyModel =
    { config = emptyConfig
    , loading = False
    , processing = False
    , finishing = False
    , processed = 0
    , remoteErr = ""
    , crawlID = ""
    , urls = []
    , url = ""
    , lastModified = None
    , lastModifiedDate = ""
    , frequency = ""
    , priority = ""
    }


init : Config -> ( Model, Cmd Msg )
init config =
    ( { emptyModel | config = config }, Cmd.none )



-- MODEL


type alias Model =
    { config : Config
    , loading : Bool
    , processing : Bool
    , finishing : Bool
    , processed : Int
    , remoteErr : String
    , crawlID : String
    , urls : List CrawledURL

    -- Request stuff.
    , url : String
    , lastModified : LastMod
    , lastModifiedDate : String
    , frequency : String
    , priority : String
    }


type alias Config =
    { href : String
    , remote : String
    }


type alias Crawl =
    { ref : String
    , name : String
    , entryURL : String
    , updating : Bool
    , done : Int
    }


type alias CrawledURL =
    { ref : String
    , url : String
    , title : String
    , description : String
    , changeFrequency : String
    , priority : String
    , enabled : Bool
    }


type LastMod
    = None
    | Custom
    | Servers



-- UPDATE


type Msg
    = Send
    | CheckCrawl Time.Posix
    | StatusResponse (Result Http.Error Crawl)
    | CrawlResponse (Result Http.Error String)
    | UrlsResponse (Result Http.Error (List CrawledURL))
    | UpdatedURLResponse (Result Http.Error CrawledURL)
    | Url String
    | LastModified LastMod
    | LastModifiedDate String
    | Frequency String
    | Priority String
    | ChangePriority CrawledURL String
    | ChangeFrequency CrawledURL String
    | ChangeEnabled CrawledURL Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Send ->
            ( { model | loading = True }, getCrawlStarted model )

        CrawlResponse (Ok ref) ->
            let
                updated =
                    { model | loading = False, processing = True, crawlID = ref }
            in
            ( updated
            , checkCrawlStatus updated
            )

        CrawlResponse (Err _) ->
            ( { model
                | loading = False
                , remoteErr = "No pudo procesarse la solicitud por ahora. Intenta de nuevo más tarde."
              }
            , Cmd.none
            )

        CheckCrawl _ ->
            ( model, checkCrawlStatus model )

        StatusResponse (Ok crawl) ->
            let
                finishing =
                    crawl.updating == False && crawl.done > 0

                nextAction =
                    if finishing then
                        fetchCrawledUrls model

                    else
                        Cmd.none
            in
            ( { model | loading = False, processing = crawl.updating, processed = crawl.done, finishing = finishing }, nextAction )

        StatusResponse (Err _) ->
            ( { model
                | loading = False
                , processing = False
                , remoteErr = "No pudo procesarse la solicitud por ahora. Intenta de nuevo más tarde."
              }
            , Cmd.none
            )

        UrlsResponse (Ok list) ->
            ( { model | urls = list, finishing = False }, Cmd.none )

        UrlsResponse (Err _) ->
            ( { model
                | loading = False
                , processing = False
                , remoteErr = "No pudo procesarse la solicitud por ahora. Intenta de nuevo más tarde."
              }
            , Cmd.none
            )

        UpdatedURLResponse (Ok url) ->
            ( model, Cmd.none )

        UpdatedURLResponse (Err _) ->
            ( model, Cmd.none )

        Url url ->
            ( { model | url = url }, Cmd.none )

        LastModified lastmod ->
            ( { model | lastModified = lastmod }, Cmd.none )

        LastModifiedDate lastmod ->
            ( { model | lastModifiedDate = lastmod }, Cmd.none )

        Frequency freq ->
            ( { model | frequency = freq }, Cmd.none )

        Priority priority ->
            ( { model | priority = priority }, Cmd.none )

        ChangePriority url priority ->
            let
                updateURL item =
                    if item.ref == url.ref then
                        { item | priority = priority }

                    else
                        item

                updated =
                    { model | urls = List.map updateURL model.urls }
            in
            ( updated, updateCrawledURL updated { url | priority = priority } )

        ChangeFrequency url freq ->
            let
                updateURL item =
                    if item.ref == url.ref then
                        { item | changeFrequency = freq }

                    else
                        item

                updated =
                    { model | urls = List.map updateURL model.urls }
            in
            ( updated, updateCrawledURL updated { url | changeFrequency = freq } )

        ChangeEnabled url enabled ->
            let
                updateURL item =
                    if item.ref == url.ref then
                        { item | enabled = enabled }

                    else
                        item

                updated =
                    { model | urls = List.map updateURL model.urls }
            in
            ( updated, updateCrawledURL updated { url | enabled = enabled } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.processing == True then
        Time.every 1000 CheckCrawl

    else
        Sub.none



-- VIEW


lastChanged : LastMod -> String -> LastMod -> Html Msg
lastChanged current labelText opt =
    label [ class "db pa2 pointer" ]
        [ input [ type_ "radio", name "lastChanged", class "mr2", onClick (LastModified opt), checked (current == opt) ] []
        , text labelText
        ]


truncated : String -> Int -> Html Msg
truncated str at =
    let
        size =
            String.length str

        capped =
            if size > at then
                String.dropRight (size - at) str ++ "..."

            else
                str
    in
    span [ title str ] [ text capped ]


shortUrlPath : String -> String -> String
shortUrlPath baseURL url =
    let
        base =
            if String.right 1 baseURL == "/" then
                String.dropRight 1 baseURL

            else
                baseURL

        baseLength =
            String.length base

        shorten =
            String.dropLeft baseLength url
    in
    shorten


view : Model -> Html Msg
view model =
    let
        finished =
            model.loading == False && model.finishing == False && model.processing == False && model.processed > 0
    in
    section [ class "vh-100 dt w-100 bg-green" ]
        [ header [ class "flex bg-black-90 w-100 mb4 white-90 ph3 pv2 f6" ]
            [ div [ class "flex-auto" ]
                [ span [] [ text "Made with " ]
                , span [ class "red" ] [ text "♥" ]
                , span [] [ text " in Mexico City by " ]
                , a [ class "white underline", href "https://github.com/fernandez14", target "_blank" ] [ text "@fernandez14" ]
                ]
            , div []
                [ a [ class "white underline mh2", href "mailto:fernandez14@outlook.com", target "_blank" ] [ text "Contact" ]
                , a [ class "white underline mh2", href "https://github.com/fernandez14/sitemap.guru", target "_blank" ] [ text "Project's Github" ]
                ]
            ]
        , div [ class "mw8 center" ]
            [ figure [ class "pa0 pt5 pb4 ma0 tc" ]
                [ img [ class "h4", src "/images/logo.png", alt "Sitemap Guru - The free sitemap generator you deserve" ] []
                ]
            , h1 [ class "f1 ma0 tc white-90 lh-title b" ] [ text "The free sitemap generator you deserve" ]
            , h2 [ class "f3 mv3 tc white-90 fw4" ] [ text "¿Tired of awful, past century & slow XML sitemap generators?" ]
            , p [ class "measure tc center black-70 lh-copy" ] [ text "Sitemap guru has been built as a result of experimenting with Golang, ELM & micro services architecture, so enjoy your free ride on the best seat to the sitemap nirvana." ]
            , section [ class "center pt3 w-100", classList [ ( "measure-wide", not finished ) ] ]
                [ Html.form [ onSubmit Send, classList [ ( "dn", model.loading || model.processing || model.finishing || model.processed > 0 ) ] ]
                    [ label [ class "b db tc pb2" ]
                        [ text "Starting URL"
                        , p [ class "ma0 pb2 black-70 tc f6 lh-copy" ] [ text "Please enter the full http address for your site, only the links within the starting directory will be included." ]
                        , input [ type_ "url", name "url", onInput Url, value model.url, placeholder "http://wantmymap.io/from-here", class "input-reset ba b--black-80 br1 bw1 pa3 mb2 db w-100 outline-0 bg-white-90 hover-bg-white" ] []
                        ]
                    , div [ class "dn pv2" ]
                        [ div [ class "flex-auto pr2" ]
                            [ label [ class "b db tc pb2" ] [ text "Change frequency" ]
                            , select [ class "w-100 input-reset pa2 ph3 bg-white b--black-80 br1 bw1 br0", onInput Frequency ]
                                [ option [ value "none" ] [ text "None" ]
                                , option [ value "always" ] [ text "Always" ]
                                , option [ value "hourly" ] [ text "Hourly" ]
                                , option [ value "daily" ] [ text "Daily" ]
                                , option [ value "weekly" ] [ text "Weekly" ]
                                , option [ value "monthly" ] [ text "Monthly" ]
                                , option [ value "yearly" ] [ text "Yearly" ]
                                , option [ value "never" ] [ text "Never" ]
                                ]
                            ]
                        , div [ class "flex-auto pl2" ]
                            [ label [ class "b db tc pb2" ] [ text "Last modification" ]
                            , lastChanged model.lastModified "None" None
                            , lastChanged model.lastModified "Use server's response" Servers
                            , lastChanged model.lastModified "Custom date" Custom
                            ]
                        ]
                    , input [ type_ "submit", value "Generate Sitemap", class "input-reset ba b grow pointer b--black bg-black white-90 br2 bw2 pa3 mb2 db w-100" ] []
                    ]
                , div [ class "tc pv3", classList [ ( "dn", not (model.loading || model.finishing || model.processing) ) ] ]
                    [ span [ class "icon-spin6 animate-spin f3" ] [] ]
                , span [ class "tc lh-copy", classList [ ( "dn", not model.processing ), ( "db", model.processing ) ] ]
                    [ text
                        (case model.processed of
                            0 ->
                                "Waking up crawler robots..."

                            1 ->
                                "Inspecting website structure..."

                            n ->
                                String.fromInt n ++ " pages crawled and going, please do not close the page while still working..."
                        )
                    ]
                ]
            ]
        , div [ class "ph4", classList [ ( "dn", not finished ) ] ]
            [ div [ class "flex mw8 center mb4" ]
                [ h3 [ class "fw5 tc flex-auto w-third ph2" ]
                    [ span [ class "db f4 mb2" ] [ text "Starting URL:" ]
                    , span [ class "db ba bw2 b--black pa3" ] [ text model.url ]
                    ]
                , h3 [ class "fw5 tc flex-auto w-third ph2" ]
                    [ span [ class "db f4 mb2" ] [ text "Crawled URLs:" ]
                    , span [ class "db ba bw2 b--black pa3" ] [ text (String.fromInt model.processed) ]
                    ]
                , h3 [ class "fw5 tc flex-auto w-third ph2" ]
                    [ span [ class "db f4 mb2" ] [ text "Sitemap File:" ]
                    , a
                        [ href (model.config.remote ++ "/xml/" ++ model.crawlID)
                        , target "_blank"
                        , class "db bg-black br-pill pa3 ml3 white-90 grow pointer"
                        ]
                        [ text "Download XML" ]
                    ]
                ]
            , table [ class "w-100 bg-white", style "border-spacing" "0" ]
                [ thead []
                    [ tr []
                        [ th [ class "fw6 tl pa3 bg-black white" ] [ text "URL" ]
                        , th [ class "fw6 tl pa3 bg-black white", style "min-width" "475px" ] [ text "Title" ]
                        , th [ class "fw6 tl pa3 bg-black white" ] [ text "Description" ]
                        , th [ class "fw6 tl pa3 bg-black white tc" ] [ text "Change Frequency" ]
                        , th [ class "fw6 tl pa3 bg-black white tc" ] [ text "Priority" ]
                        , th [ class "fw6 tl pa3 bg-black white tc" ] [ text "Enabled?" ]
                        ]
                    ]
                , tableRows model model.urls
                ]
            ]
        ]


tableRows : Model -> List CrawledURL -> Html Msg
tableRows model urls =
    tbody [ class "lh-copy" ] (List.map (tableKeyedRow model) urls)


tableKeyedRow : Model -> CrawledURL -> Html Msg
tableKeyedRow model url =
    lazy (tableRow model) url


tableRow : Model -> CrawledURL -> Html Msg
tableRow model url =
    tr [ class "striped--light-gray" ]
        [ td [ class "tl pa3" ] [ text (shortUrlPath model.url url.url) ]
        , td [ class "tl pa3" ] [ truncated url.title 60 ]
        , td [ class "tl pa3" ] [ truncated url.description 150 ]
        , td [ class "tc pa3" ]
            [ frequencySelect url
            ]
        , td [ class "tc pa3" ]
            [ prioritySelect url
            ]
        , td [ class "tc pa3" ]
            [ input [ type_ "checkbox", checked url.enabled, onCheck (ChangeEnabled url) ] []
            ]
        ]


prioritySelect : CrawledURL -> Html Msg
prioritySelect url =
    let
        options =
            [ ( "1.0", "1.0 (High)" )
            , ( "0.9", "0.9" )
            , ( "0.8", "0.8" )
            , ( "0.7", "0.7" )
            , ( "0.6", "0.6" )
            , ( "0.5", "0.5 (default)" )
            , ( "0.4", "0.4" )
            , ( "0.3", "0.3" )
            , ( "0.2", "0.2" )
            , ( "0.1", "0.1" )
            , ( "0.0", "0.0 (Low)" )
            ]
    in
    select [ onInput (ChangePriority url) ]
        (List.map
            (\opt ->
                let
                    ( v, lbl ) =
                        opt

                    isCurrent =
                        v == url.priority
                in
                option [ value v, selected isCurrent ] [ text lbl ]
            )
            options
        )


frequencySelect : CrawledURL -> Html Msg
frequencySelect url =
    let
        options =
            [ "always", "hourly", "daily", "weekly", "monthly", "never" ]
    in
    select [ onInput (ChangeFrequency url) ]
        (List.map
            (\opt ->
                option [ value opt, selected (opt == url.changeFrequency) ] [ text opt ]
            )
            options
        )



-- HTTP


jsonifyCrawl : Model -> Http.Body
jsonifyCrawl model =
    Http.jsonBody <|
        Encode.object
            [ ( "EntryUrl", Encode.string model.url )
            , ( "Name", Encode.string model.url )
            , ( "Depth", Encode.int 10 )
            , ( "Strict", Encode.bool True )
            ]


decodeCrawlId : Decode.Decoder String
decodeCrawlId =
    Decode.field "Ref" Decode.string


getCrawlStarted : Model -> Cmd Msg
getCrawlStarted req =
    let
        body =
            jsonifyCrawl req

        request =
            Http.post (req.config.remote ++ "/sitemap") body decodeCrawlId
    in
    Http.send CrawlResponse request


jsonifyURL : CrawledURL -> Http.Body
jsonifyURL url =
    Http.jsonBody <|
        Encode.object
            [ ( "Ref", Encode.string url.ref )
            , ( "Url", Encode.string url.url )
            , ( "Title", Encode.string url.title )
            , ( "Description", Encode.string url.description )
            , ( "Priority", Encode.string url.priority )
            , ( "ChangeFrequency", Encode.string url.changeFrequency )
            , ( "Enabled", Encode.bool url.enabled )
            , ( "Meta", Encode.string "{}" )
            ]


updateCrawledURL : Model -> CrawledURL -> Cmd Msg
updateCrawledURL m url =
    let
        body =
            jsonifyURL url

        request =
            Http.request
                { method = "PUT"
                , headers = []
                , url = m.config.remote ++ "/url"
                , body = body
                , expect = Http.expectJson decodeCrawledURL
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send UpdatedURLResponse request


decodeCrawl : Decode.Decoder Crawl
decodeCrawl =
    Decode.map5 Crawl
        (Decode.field "Ref" Decode.string)
        (Decode.field "Name" Decode.string)
        (Decode.field "EntryURL" Decode.string)
        (Decode.field "Updating" Decode.bool)
        (Decode.field "Done" Decode.int)


decodeCrawledURL : Decode.Decoder CrawledURL
decodeCrawledURL =
    Decode.map7 CrawledURL
        (Decode.field "Ref" Decode.string)
        (Decode.field "Url" Decode.string)
        (Decode.field "Title" Decode.string)
        (Decode.field "Description" Decode.string)
        (Decode.field "ChangeFrequency" Decode.string)
        (Decode.field "Priority" Decode.string)
        (Decode.field "Enabled" Decode.bool)


decodeUrls : Decode.Decoder (List CrawledURL)
decodeUrls =
    Decode.list decodeCrawledURL


checkCrawlStatus : Model -> Cmd Msg
checkCrawlStatus model =
    let
        endpoint =
            model.config.remote ++ "/sitemap/" ++ model.crawlID

        request =
            Http.get endpoint decodeCrawl
    in
    Http.send StatusResponse request


fetchCrawledUrls : Model -> Cmd Msg
fetchCrawledUrls model =
    let
        endpoint =
            model.config.remote ++ "/sitemap/" ++ model.crawlID ++ "/urls"

        request =
            Http.get endpoint decodeUrls
    in
    Http.send UrlsResponse request
