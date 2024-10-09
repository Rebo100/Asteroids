module Menu's where

    import Graphics.Gloss

    type StartMenu = [Button]
    startMenu :: StartMenu
    startMenu = [Button "startButton" "" [(-200, -100), (100, 100)] (makeColorI 141 141 141 255)]

    data Button = Button {
    buttonName :: String,
    buttonText :: String,
    buttonShape :: [(Float, Float)], -- Add the 2 opposite corners
    buttonColor :: Color
}

-- Methods
    drawButton :: Button -> Maybe Color -> Picture -- Take 2 corners and return a rectangle shaped button
    drawButton button Nothing = color (buttonColor button) $ Polygon [(x, y), (x, y2), (x2, y2), (x2, y)] -- Take the default color
      where [(x, y), (x2, y2)] = buttonShape button
    drawButton button (Just c) = color c $ Polygon [(x, y), (x, y2), (x2, y2), (x2, y)] -- Change the color (e.g. Button is selected)
      where [(x, y), (x2, y2)] = buttonShape button