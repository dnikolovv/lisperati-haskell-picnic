import Data.List
import Text.Regex
import System.Random
import Data.Ord

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Polygon   = [Point]
type Person    = [Int]
type Link      = [Point]
type Placement = [(Point,Person)]

type EnergyFunction a              = a -> Int
type TemperatureFunction           = Int -> Int -> Float
type TransitionProbabilityFunction = Int -> Int -> Float -> Float
type MotionFunction a              = StdGen -> a -> (StdGen,a)

writePoint :: Point -> String 
writePoint (x,y) = (show x)++","++(show y)++" "

writePolygon :: (Color,Polygon) -> String 
writePolygon ((r,g,b),p) = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:#cccccc;stroke:rgb("++(show r)++","++(show g)++","++(show b)++");stroke-width:2\"/>"

writePolygons :: [(Color,Polygon)] -> String 
writePolygons p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++(concatMap writePolygon p)++"</svg>"

colorize :: Color -> [Polygon] -> [(Color,Polygon)] 
colorize = zip.repeat

rainbow :: [[Polygon] -> [(Color, Polygon)]]
rainbow@[red,green,blue,yellow,purple,teal] = map colorize [(255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

main :: IO ()
main = do 
  putStr "Hello World! Let's have a picnic! \n"
  people_text <- readFile "people.txt"

  let people :: [Person]
      people = read people_text

  putStr "Number of people coming: "
  print (length people)
  
  writeFile "tut0.svg" $ writePolygons (red [[(100,200),(200,100),(200,200),(100,200)],[(200,200),(300,200),(300,200),(200,300)]])