module Menu's where

    import Graphics.Gloss

    data StartMenu = StartMenu {
    title :: String,
    startButton :: Button
}

    data Button = Button {
    text :: String,
    shape :: Picture
}

    drawButton :: Button -> Picture
    drawButton b = undefined