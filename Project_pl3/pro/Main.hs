module Main(main ,PongGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
width , height , offset :: Int
width = 650
height = 650
offset = 100


window :: Display
window = InWindow "pl3 project" (width, height) (offset, offset) --u/d l/r

background :: Color
background = black 

drawing :: Picture
drawing = pictures [ball,walls,mkpaddle rose 120 (-20),mkpaddle orange (-120) 40]

  where
    ball = translate (-10) 40 $ color ballcolor $ circleSolid 10  
    ballcolor = dark red

    wall :: Float -> Picture
    wall offset =
           translate 0 offset $
            color wallcolor $
             rectangleSolid 270 10 
       
    wallcolor = greyN 0.5
    walls = pictures [wall 150 ,wall (-150)]

    mkpaddle :: Color-> Float->Float->Picture
    mkpaddle col x y = pictures
      [translate x y $ color col $ rectangleSolid 26 86
      ,translate x y $ color paddlecolor $ rectangleSolid 20 80
      ]
    paddlecolor = light (light blue)

-- | Data describing the state of the pong game
data PongGame = Game
   { ballLoc :: (Float,Float) -- ^ Pong ball (x, y) location.
   , ballVel :: (Float,Float) -- ^ Pong ball (x, y) velocity.
   , player1 :: Float         -- ^ Left player paddle height.
                              -- Zero is the middle of the screen.       
   , player2 :: Float         -- ^ Right player paddle height.
   --, win1    :: Float
   --, win2    :: Float
   }deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (50, -20)
  , player1 = 40
  , player2 = 40
  --, win1    = width
  --, win2    = width
  }

-- | Convert a game state into a picture.
render :: PongGame-> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle rose 300 $ player1 game,
            mkPaddle orange (-300) $ player2 game
			      ,mkString ]
  where
  		
  	
    mkString :: Picture
    mkString = translate 0 0 (color red (text "player1"))
	  
	
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red
	  
	  -- the String
	  

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
     [ translate x y $ color col $ rectangleSolid 26 86
     , translate x y $ color paddleColor $ rectangleSolid 20 80
     ]
    paddleColor = light (light blue)
	
	

--simulate window background fps initialState render update
main :: IO ()
main = play window background fps initialState render handleKeys update
  --    frame :: Float -> Picture
    --  frame seconds = render $ moveBall seconds initialState
type Radius = Float
type Position = (Float,Float)

moveBall :: Float->PongGame->PongGame 
moveBall seconds game = game { ballLoc = ((x'), (y')) }
        where
          (x, y) = ballLoc game
          (vx, vy) = ballVel game
          (x') = x + vx * seconds
          (y') = y + vy * seconds

      --update :: ViewPort -> Float -> PongGame -> PongGame 
      --update _ = moveBall
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
        where
          -- Radius. Use the same thing as in `render`.
          radius = 10
  
          -- The old velocities.
          (vx, vy) = ballVel game
  
          vy' = if wallCollision (ballLoc game) radius
                then
                  -- Update the velocity.
                  -vy
                else
                  -- Do nothing. Return the old velocity.
                  vy
      
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius =  topCollision || bottomCollision
        where
          topCollision    = y + radius >= 150 
          bottomCollision = y + (-radius) <=  -150

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
      where
        -- Radius. Use the same thing as in `render`.
        radius = 10

        -- The old velocities.
        (vx, vy) = ballVel game

        vx' = if paddleCollision (ballLoc game) (player1 game) (player2 game) radius
              then
                -- Update the velocity.
                -vx
              else
                -- Do nothing. Return the old velocity.
                vx

paddleCollision :: Position -> Float -> Float-> Radius -> Bool 
paddleCollision (x, y) py1 py2 radius = (leftCollision && leftCollision2) || (rightCollision && rightCollision2)
                        where
                          
                          leftCollision  = x - (40+13) <= -fromIntegral height / 2 
                          rightCollision = x + (40+13) >=  fromIntegral height / 2
                          leftCollision2 = (y <= py2+43 && y >py2-43) 
                          rightCollision2 = (y <= py1+43 && y >py1-43) 
                



update ::  Float -> PongGame -> PongGame
update  seconds gm = paddleBounce (moveBall seconds (wallBounce (moveBall seconds gm)) )
      
handleKeys :: Event -> PongGame -> PongGame

handleKeys (EventKey (Char 'p') _ _ _) game = game { player1 = yy }
                                    where
                                      y =  player1 game
                                      yy = y+5
handleKeys (EventKey (Char 'l') _ _ _) game = game { player1 = yy }
                                    where
                                      y =  player1 game
                                      yy = y-5
handleKeys (EventKey (Char 'w') _ _ _) game = game { player2 = yy }
                                      where
                                        y =  player2 game
                                        yy = y+5
handleKeys (EventKey (Char 's') _ _ _) game = game { player2 = yy }
                                        where
                                          y =  player2 game
                                          yy = y-5
  
handleKeys _ game = game                                          

                                      
fps :: Int
fps = 60
      


