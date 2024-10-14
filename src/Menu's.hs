module Menu's where

    import Graphics.Gloss
    import Toolbox
    import Data.Bifunctor (Bifunctor(bimap))
    data Menu = 
      StartMenu StartMenu |
      PauseMenu PauseMenu |
      NoMenu NoMenu

    type StartMenu = [Button]
    startMenu = StartMenu
      [ 
        Button "startButton" "Start" [(-120, -80), (120, -30)] (makeColorI 141 141 141 255) StartGame,
        Button "exitButton" "Exit" [(-100, -150), (100, -100)] (makeColorI 141 141 141 255) ExitGame
      ]
    type PauseMenu = [Button]
    type NoMenu = [Button]
    noMenu = NoMenu []

    data Button = Button {
    buttonName :: String,
    buttonText :: String,
    buttonShape :: [(Float, Float)], -- Add the 2 opposite corners
    buttonColor :: Color,
    buttonFunction :: ButtonFunction
    }

    data ButtonFunction = StartGame | ExitGame | ResumeGame 

-- Methods
--Menu's
    drawMenu :: Menu -> (Float, Float) -> Float -> [Picture]
    drawMenu (StartMenu xs) mouse scale = map (\x -> drawButton x mouse scale) xs
    drawMenu (NoMenu _) mouse scale = []

    getButtons :: Menu -> [Button]
    getButtons (StartMenu xs) = xs
    getButtons (PauseMenu xs) = xs
    getButtons _ = []

--Buttons
    drawButton :: Button -> (Float, Float) -> Float -> Picture
    drawButton button mouse scale | inRectangle mouse scaledUp = drawSelectedButton button Nothing
                            | otherwise = drawButton' button Nothing
                            where 
                                scaledUp = map (bimap (*scale) (*scale)) (buttonShape button)


    drawSelectedButton :: Button -> Maybe Color -> Picture
    drawSelectedButton button c =
      let outerBox = (color red $ drawRectangle [(x-3, y-3), (x2+3, y2+3)])
          [(x, y), (x2, y2)] = buttonShape button
      in Pictures [outerBox, drawButton' button c]

    drawButton' :: Button -> Maybe Color -> Picture -- Take 2 corners and return a rectangle shaped button
    drawButton' button c = Pictures [b (maybe (buttonColor button) id c), s]
      where
        [(x, y), (x2, y2)] = buttonShape button
        s = translate (x + (x2 - x) / 4) (y + 2) $ Scale 0.4 0.4 $ color white $ text (buttonText button)
        b c = color c $ drawRectangle (buttonShape button)