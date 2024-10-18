module Controller.GameFunctions where
import Model
import Menu's
-- Pause game
pauseGame :: GameState -> GameState
pauseGame gstate@(GameState _ paused _ _ _ _ _ _ _ _ _ _) | paused = gstate {isPaused = False, menu = None}
                                                      | otherwise = gstate {isPaused = True, menu = pauseMenu}
-- Button functionality
doButtonFunction :: ButtonFunction -> GameState -> GameState
doButtonFunction StartGame _ = lvl1
doButtonFunction ResumeGame gstate = pauseGame gstate
doButtonFunction ExitGame gstate = gstate { isRunning = False }