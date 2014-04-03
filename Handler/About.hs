{- About.hs was used to generate graphs on a deprecated about page.
Now that we use wiki pages, we couldn't have live JavaScript there,
so we saved png files of the graphs. But this stuff could still be used somewhere
eventually or used to generated updated graphs. -}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Handler.About where

import Import

import qualified Data.Text as T

import Text.Printf

import Data.List (tail)



import Yesod.Form.Jquery

import Data.Attoparsec.Number (Number (..))

import qualified Data.Vector as V

shareValue :: Double -> Int -> Double
shareValue avg_shares patrons = 0.0001 * logBase 2 (avg_shares * 2) * (fromIntegral patrons -  1)

deg2rad :: Double -> Double
deg2rad = (pi*) . (/180)

rad2deg :: Double -> Double
rad2deg = (180*) . (/pi)

data Segment = Segment { segmentTitle :: Text, segmentStart :: Double, segmentEnd :: Double }

data Direction = CW | CCW;

data Pass = Foreground | Shadow;

chartColors :: [Text]
chartColors = [ "#4bb2c5", "#EAA228", "#c5b47f", "#579575", "#839557", "#958c12", "#953579", "#4b5de4", "#d8b83f", "#ff5800", "#0085cc", "#c747a3", "#cddf54", "#FBD178", "#26B4E3", "#bd70c7"];


numberSuffix :: Int -> String
numberSuffix n = (["th", "st", "nd", "rd"] ++ repeat "th") !! (n `mod` 10)

suffixed :: Int -> String
suffixed n = show n ++ number_suffix n

conjoin :: String -> [String] -> String
conjoin _ [x] = x
conjoin c [x, y] = unwords [x, c, y]
conjoin c xs =
    let conjoin' [x, y] = unwords [x ++ ",", c, y]
        conjoin' (x:xs') = x ++ ", " ++ conjoin' xs'
        conjoin' [] = ""
     in conjoin' xs


hash :: Text -> Text
hash = T.cons '#'

donutSharesChart :: [Int] -> Widget
donutSharesChart patrons_list = do
    let margin = deg2rad 2
        initial_offset = deg2rad 40

        (legend, caption_renderers) :: ([Text], [Int -> Double -> Double -> String]) = unzip
            [ ( "new pledge amount"
              , \ patron_count addition total_funding ->
                      printf "When the %s patron joins at one share, they contribute $%0.4f. The project's total funding is now $%0.4f per month."
                        (suffixed patron_count)
                        addition
                        total_funding
              )
            , ( "extra 0.01\162 from one earlier patron"
              , \ _ addition _ ->
                    printf "With each new one-share patron, a single earlier patron always adds $%0.4f; and with more patrons, this is a continually smaller portion of the total increased funding." 
                        addition
              )
            , ( "increase from remaining earlier patrons"
              , \ patron_count _ total_funding ->
                    printf "When the %s patron joins, the earlier patrons together match the new patron's pledge. The project's total funding is now $%0.4f per month."
                        (suffixed patron_count)
                        total_funding
              )
            ]

        apply_margin segment =
            let start = segmentStart segment + margin / 2
                end = segmentEnd segment - margin / 2
             in if start > end
                 then segment
                 else segment { segmentStart = start, segmentEnd = end }

        drawRing :: Pass -> (Int, Int, [Double]) -> Widget
        drawRing pass (index, patrons, amounts) = do
            let (colors', amounts') = unzip $ filter ((>0) . snd) $ zip chart_colors amounts
                total = sum amounts'
                segments = map apply_margin $ tail $ scanl (\ segment (render_caption, amount) ->
                                                                Segment { segmentTitle = T.pack $ render_caption patrons amount (0.0001 * fromIntegral patrons * fromIntegral (patrons - 1))
                                                                        , segmentStart = segmentEnd segment
                                                                        , segmentEnd = segmentEnd segment + 2 * pi * amount / total
                                                                        }
                                                           ) Segment { segmentTitle = undefined, segmentStart = undefined, segmentEnd = initial_offset } $ zip caption_renderers amounts'
                r_inner = fromIntegral index * 1.25 - 0.4
                r_outer = r_inner + 1


--             d_a
--             ) )
--             c-b
--
                segmentPath :: Double -> Segment -> String
                segmentPath offset segment =
                    let a_x = r_outer * sin (segmentStart segment) + offset
                        a_y = -r_outer * cos (segmentStart segment) + offset

                        b_x = r_outer * sin (segmentEnd segment) + offset
                        b_y = -r_outer * cos (segmentEnd segment) + offset

                        c_x = r_inner * sin (segmentEnd segment) + offset
                        c_y = -r_inner * cos (segmentEnd segment) + offset

                        d_x = r_inner * sin (segmentStart segment) + offset
                        d_y = -r_inner * cos (segmentStart segment) + offset

                        move :: Double -> Double -> String
                        move = printf "M%f %f"

                        line :: Double -> Double -> String
                        line = printf "L%f %f"

                        arc :: Direction -> Double -> Double -> String
                        arc CW = printf "A%f %f 0 %d 1 %f %f" r_outer r_outer (if segmentEnd segment - segmentStart segment > pi then 1 else 0 :: Int)
                        arc CCW = printf "A%f %f 0 %d 0 %f %f" r_inner r_inner (if segmentEnd segment - segmentStart segment > pi then 1 else 0 :: Int)

                     in move a_x a_y ++ arc CW b_x b_y ++ line c_x c_y ++ arc CCW d_x d_y

                drawSegment color segment =
                    let path = segmentPath 10 segment
                     in toWidget [whamlet|
                            <path d="#{path} Z" title="#{segmentTitle segment}" fill="#{color}" stroke=none >
                        |]

                drawShadow segment =
                    let path = segmentPath 10.2 segment
                     in toWidget [whamlet|
                            <path d="#{path} Z" fill="gray" filter="url(#shadowFilter)" stroke=none >
                        |]

            toWidget $ case pass of
                Shadow -> [whamlet|
                        $forall segment <- segments
                            ^{drawShadow segment}
                    |]

                Foreground -> [whamlet|
                        $forall (color, segment) <- zip colors' segments
                            ^{drawSegment color segment}
                    |]


    let shares = map (share_value 1) patrons_list
        prev = map (share_value 1 . (-1 +) . fromIntegral) patrons_list
        increase = zipWith (-) shares prev
        remaining_patrons = zipWith (\ d i -> fromIntegral (d - 2) * i) patrons_list increase

        rings = reverse $ zip3 [1..] patrons_list $ zipWith3 (\ a b c -> [a, b, c]) shares increase remaining_patrons

        list_desc = conjoin "and" $ map (\ n -> show n ++ number_suffix n) patrons_list

    toWidget [whamlet|
        <a name="donut">
        <div .row>
            <div .col-md-5>
                <figure>
                    <svg viewbox="0 0 20 20">
                        <defs>
                            <filter #shadowFilter>
                                <feGaussianBlur in="SourceGraphic" stdDeviation="0.1">

                        $forall ring <- rings
                            ^{drawRing Shadow ring}
                            ^{drawRing Foreground ring}

            <div .col-md-3 style="position:relative;height:26em;text-size:0.7em">
                <div .row style="position:absolute;top:2em">
                    <div .col-md-3>
                        Sources of funding increase when the #{list_desc} patrons are added to a project (note: rings are not sized proportionally).
                <div .row style="position:absolute;bottom:0em">
                    <div .col-md-3>
                        <table .table .table-bordered>
                            $forall (color, label) <- zip chart_colors legend
                                <tr>
                                    <td>
                                        <svg width="1em" height="1em" viewbox="0 0 1 1">
                                                <rect width="0.7" height="0.7" x="0" y="0.3" fill=#{color}>
                                        <td>
                                            #{label}


    |]


mkArray :: [Value] -> Value
mkArray = Array . V.fromList

mkNumArray :: [Double] -> Value
mkNumArray = mkArray . map (Number . D)

mkPlotsArray :: [[[Double]]] -> Value
mkPlotsArray = mkArray . map (mkArray . map mkNumArray)


addChart route = do
    ident <- lift newIdent
    app <- lift getYesod

    addScriptEither $ urlJqueryJs app
    addStylesheetEither $ urlJqueryUiCss app
    addScript $ StaticR js_jquery_jqplot_min_js
    addScript $ StaticR js_plugins_jqplot_logAxisRenderer_min_js
    addStylesheet $ StaticR css_jquery_jqplot_min_css
    toWidget [hamlet|
        <div .chart_container>
            <div ##{ident}>
            <noscript>
                <img .chart src="@{route}">
    |]

    return ident


shareValueChart :: Widget
shareValueChart = do
    ident <- addChart $ StaticR img_pledgechart1_png

    let max_x = 50000
        max_y = share_value 1 max_x :: Double
        xs = [1, (div max_x 10) .. max_x] :: [Int]
        ys = map (share_value 1) xs
        merge = zipWith $ \ x y -> [fromIntegral x, y]
        ticks = [0 .. max_y]

        plots = [merge xs ys] :: [[[Double]]]

    toWidget [julius|
        $(#{String $ hash ident}).addClass("chart");

        $.jqplot(#{String ident}, #{mkPlotsArray plots},
            { title: "Monthly Pledge Share Amount (given all pledges are single share)"
            , seriesDefaults: {showMarker : false}
            , axesDefaults: { pad: 0 }
            , axes:
                { xaxis: { label: "Patrons" }
                , yaxis:
                    { label: "Dollars Pledged Monthly Per Share"
                    , tickOptions: { formatString: "$%d" }
                    , min: 0
                    , ticks: #{mkNumArray ticks}
                    }
                }
            }
        );
    |]

projectValueChart :: Widget
projectValueChart = do
    ident <- addChart $ StaticR img_pledgechart2_png

    let project_value avg_shares patrons = share_value avg_shares patrons * avg_shares * fromIntegral patrons
        max_x = 50000 :: Int
        xs = [2, 1000 .. max_x] :: [Int]
        ys = map (project_value 1) xs :: [Double]
        merge = zipWith $ \ x y -> [fromIntegral x, y]

        plots = [ merge xs ys ]

    toWidget [julius|
        $(#{String $ hash ident}).addClass("chart");

        $.jqplot(#{String ident}, #{mkPlotsArray plots},
            { title: "Project Monthly Donations (given all pledges are single share)"
            , seriesDefaults: {showMarker: false}
            , axesDefaults: { pad: 0 }
            , axes:
                { xaxis: { label: "Patrons" }
                , yaxis:
                    { label: "Total Monthly Dollars Raised"
                    , tickOptions: { formatString: "$%d" }
                    }
                }
            }
        );
    |]


getAboutR :: Handler RepHtml
getAboutR = do
    let donut = donutSharesChart [2, 5, 10, 50, 100, 500, 1000]
    defaultLayout $(widgetFile "about")

