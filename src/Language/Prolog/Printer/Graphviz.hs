module Language.Prolog.Printer.Graphviz
  where 

import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Graph         (emptyGraph)
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import           Data.Word
import           Control.Monad
import           System.FilePath

import Language.Prolog.Algorithm


myColorCL :: Word8 -> ColorList
myColorCL n | n == 1 = c $ (RGB 127 108 138)
            | n == 2 = c $ (RGB 175 177 112)
            | n == 3 = c $ (RGB 226 206 179)
            | n == 4 = c $ (RGB 172 126 100)
 where c rgb = toColorList [rgb]

myColor :: Word8 -> Attribute
myColor n = Color $ myColorCL n

show' :: Show a => a -> L.Text
show' = L.pack . show

showTree :: SearchTree -> G.DotGraph L.Text
showTree t@(Node result resolvent trees) = digraph (Str "tree") $  do
  node (show' resolvent) [textLabel (show' resolvent), shape Ellipse]
  forM_ trees $ \t' -> do 
    showTree' t' 
    (show' resolvent) --> (show' t')
showTree _ = digraph (Str "tree") $ node' "Empty Tree"

showTree' :: SearchTree -> Dot L.Text
showTree' t@(Node result r trees) = do
  node (show' t) [textLabel (show' result), shape BoxShape]
  node (show' r) [textLabel (show' r), shape Ellipse]
  show' t --> show' r
  forM_ trees $ \t' -> do 
    showTree' t' 
    (show' r) --> (show' t') 

showTree' t@(Ok result) = node (show' t) [textLabel (show' result), shape BoxShape]
showTree' t@(Fail t1 t2) = node (show' t) [textLabel (pack "Fail"), shape BoxShape]


doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = forM_ cases createImage

createImage :: PrintDotRepr dg n => (FilePath, dg n) -> IO FilePath
createImage (n, g) = createImageInDir "." n Png g

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)




