module Menu's where

    import Graphics.Gloss
    import Toolbox
    
    type StartMenu = [Button]
    startMenu :: StartMenu
    startMenu = [Button "startButton" "" [(-180, -100), (100, 100)] (makeColorI 141 141 141 255)]

    data Button = Button {
    buttonName :: String,
    buttonText :: String,
    buttonShape :: [(Float, Float)], -- Add the 2 opposite corners
    buttonColor :: Color
}

-- Methods
    drawButton :: Button -> (Float, Float) -> Picture
    drawButton button mouse | inRectangle mouse (buttonShape button) = drawSelectedButton button Nothing
                            | otherwise = drawButton' button Nothing


    drawSelectedButton :: Button -> Maybe Color -> Picture
    drawSelectedButton button c = 
      let outerBox = (color red $ drawRectangle [(x-3, y-3), (x2+3, y2+3)])
          [(x, y), (x2, y2)] = buttonShape button
      in Pictures [outerBox, drawButton' button c]

    drawButton' :: Button -> Maybe Color -> Picture -- Take 2 corners and return a rectangle shaped button
    drawButton' button Nothing = color (buttonColor button) $ drawRectangle (buttonShape button) -- Take the default color
    drawButton' button (Just c) = color c $ drawRectangle (buttonShape button) -- Change the color (e.g. Button is selected)
