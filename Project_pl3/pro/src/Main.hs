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
  background = white

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
      walls = pictures [wall 350 ,wall (-350)]
  
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
     , win1    :: (Float,Float)
     , win2    :: (Float,Float)
     }deriving Show
  
  -- | The starting state for the game of Pong.
  initialState :: PongGame
  initialState = Game
    { ballLoc = (-10, 30)
    , ballVel = (120, -120)
    , player1 = 40
    , player2 = 40
    , win1   = (-150 , 500)
    , win2    = (-150, -500)
    }
  
  -- | Convert a game state into a picture.
  render :: PongGame-> Picture   -- ^ A picture of this game state.
  render game =
    pictures [ball, walls,
              mkPaddle rose 300 $ player1 game,
              mkPaddle orange (-300) $ player2 game
              ,mkwin1  ,mkwin2  ]
    where
        
 
      mkwin1  = uncurry translate (win1 game) (color red (text "pl1win"))
      
      
      mkwin2  = uncurry translate (win2 game) (color black (text "pl2win"))
 
      
    
      --  The pong ball.
      ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
      ballColor = dark red
      
      -- the String
      
  
      --  The bottom and top walls.
      wall :: Float -> Picture
      wall offset = translate 0 offset $ color wallColor $ rectangleSolid 450 10
  
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
  moveBall seconds game = game { ballLoc = ((x'), (y'))}
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
  paddleBounce game = game { ballVel = (vx', vy) , win1 = (x1,y11) , win2 = (x2,y22) }
        where
          -- Radius. Use the same thing as in `render`.
          radius = 10
  
          -- The old velocities.
          -- The old postion of the ball
          -- The old postions of the paddles
          (vx, vy) = ballVel game
          (m,n) = ballLoc game
          (x1 , y1) = win1 game
          (x2 , y2) = win2 game
          vx' = if paddleCollision (ballLoc game) (player1 game) (player2 game) radius
                 then
                  -- Update the velocity.
                  -vx
                 else
                  -- Do nothing. Return the old velocity.    
                  vx
          y22 = if (m >= 380)
                 then
                  -- Update the win 2 position .
                  0
                 else
                  -- Do nothing. Return the old position.    
                  y2

          y11 = if (m <= -380)
                 then
                     -- Update the win 1 position .
                  0
                 else
                     -- Do nothing. Return the old position.    
                   y1
        
  
  paddleCollision :: Position -> Float -> Float-> Radius -> Bool 
  paddleCollision (x, y) py1 py2 radius = (leftCollision && leftCollision2) || (rightCollision && rightCollision2)
                          where
                            
                            leftCollision  = x - radius <= -300+13 
                            rightCollision = x + radius >=  300-13
                            leftCollision2 = (y <= py2+43 && y >py2-43) 
                            rightCollision2 = (y <= py1+43 && y >py1-43) 

  
  
  update ::  Float -> PongGame -> PongGame
  update  seconds gm = paddleBounce (moveBall seconds (wallBounce (moveBall seconds gm)) )
        
  handleKeys :: Event -> PongGame -> PongGame
  
  handleKeys (EventKey (Char 'p') _ _ _) game = game { player1 = yy }
                                      where
                                        y =  player1 game
                                        yy = y+12
  handleKeys (EventKey (Char 'l') _ _ _) game = game { player1 = yy }
                                      where
                                        y =  player1 game
                                        yy = y-12
  handleKeys (EventKey (Char 'w') _ _ _) game = game { player2 = yy }
                                        where
                                          y =  player2 game
                                          yy = y+12
  handleKeys (EventKey (Char 's') _ _ _) game = game { player2 = yy }
                                          where
                                            y =  player2 game
                                            yy = y-12

  handleKeys (EventKey (Char 'r') _ _ _) game = game { ballLoc = (0,0),ballVel = (x1,y1) , win1 = (-150 , 500) ,win2 = (-150,-500) }
                                            where 
                                              (x,y) = ballVel game    
                                              x1  = if(x==0)
                                                      then 
                                                        135
                                                      else 
                                                          x
                                              y1 = if(y==0)
                                                      then
                                                         -45
                                                      else
                                                          y       

  handleKeys (EventKey (Char 'o') _ _ _) game = game { ballLoc = (x,y) ,ballVel = (0,0) }
                                          where
                                            (x,y) = ballLoc game  
                                            
       
                                           
  handleKeys _ game = game                                          
  
                                        
  fps :: Int
  fps = 60
        
  
  
  