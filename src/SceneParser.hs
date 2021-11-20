module SceneParser where

import Text.Parsec
import Text.Parsec.String
import SceneCommand as C
import Linear ( V3(..) )
import SceneEval (execC, defaultState)

intP :: Parser Int
intP =  do{
            try $ string "-";
            str <- many1 digit;
            return  $ read $ "-" ++ str;
        }
        <|>
        do{
            str <- many1 digit;
            return  $ read str;
         }

_pfloatP :: Parser Float
_pfloatP = do{
            optional $ string "+";
            n1 <- option "0" (many1 digit);
            string ".";
            n2 <- many1 digit;
            return  $ read (n1++"."++n2);
           }

_nfloatP :: Parser Float
_nfloatP = do{
            string "-";
            n1 <- option "0" (many1 digit);
            string ".";
            n2 <- many1 digit;
            return  $ read ("-"++n1++"."++n2);
           }

floatP :: Parser Float
floatP = do
    {
        try _nfloatP <|> try _pfloatP
    }<|>
    do
    {
        try $ string "-";
        str <- try $ many1 digit;
        return  $ read $ "-"++str;
    }<|>
    do
    {
        optional $ string "+";
        str <- many1 digit;
        return  $ read str;
    }

sizeP :: Parser Command
sizeP = do
    {
        string "size";
        spaces;
        width <- intP;
        spaces;
        height <- intP;
        spaces;
        return $ C.Size width height
    }

vec3P :: Parser (V3 Float)
vec3P = do
    {
        spaces;
        f1 <- floatP;
        spaces;
        f2 <- floatP;
        spaces;
        f3 <- floatP;
        spaces;
        return $ V3 f1 f2 f3
    }

cameraP :: Parser Command
cameraP = do
    {
        string "camera";
        spaces;
        eye <- vec3P;
        spaces;
        center <- vec3P;
        spaces;
        up <- vec3P;
        spaces;
        fovy <- floatP;
        spaces;
        return $ C.Cam eye center up fovy
    }

maxvertP :: Parser Command
maxvertP = do
    {
        string "maxverts";
        spaces;
        intP;
        return C.Pass
    }


commentP :: Parser Command
commentP = do
    {
        string "#";
        optional $ many1 $ noneOf "\n";
        return C.Pass;
    }

vertexP :: Parser Command
vertexP = do
    {
        spaces;
        string "vertex";
        spaces;
        v <- vec3P;
        spaces;
        return $ C.Vert v
    }

pushP :: Parser Command
pushP = do
    {
        spaces;
        string "pushTransform";
        spaces;
        return C.Push
    }

popP :: Parser Command
popP = do
    {
        spaces;
        string "popTransform";
        spaces;
        return C.Pop
    }

triP :: Parser Command
triP = do
    {
        spaces;
        string "tri";
        spaces;
        v1 <- intP;
        spaces;
        v2 <- intP;
        spaces;
        v3 <- intP;
        spaces;
        return $ C.Tri v1 v2 v3
    }

sphereP :: Parser Command
sphereP = do
    {
        spaces;
        string "sphere";
        spaces;
        loc <- vec3P;
        spaces;
        r <- floatP;
        spaces;
        return $ C.Sph loc r
    }

rotateP :: Parser Command
rotateP = do
    {
        spaces;
        string "rotate";
        axis <- vec3P;
        spaces;
        deg <- floatP;
        spaces;
        return $ C.Rot deg axis
    }

translateP :: Parser Command
translateP = do
    {
        spaces;
        string "translate";
        v <- vec3P;
        spaces;
        return $ C.Trans v
    }

scaleP :: Parser Command
scaleP = do
    {
        spaces;
        string "scale";
        v <- vec3P;
        spaces;
        return $ C.Scale v
    }

transfromP :: Parser Command
transfromP = do
    {
        try pushP <|> try popP <|> try rotateP <|> try translateP <|> scaleP
    }

ambientP :: Parser Command
ambientP = do
    {
        spaces;
        string "ambient";
        c <- vec3P;
        spaces;
        return $ C.Ambient c
    }
diffuseP :: Parser Command
diffuseP = do
    {
        spaces;
        string "diffuse";
        c <- vec3P;
        spaces;
        return $ C.Diffuse c
    }
specularP :: Parser Command
specularP = do
    {
        spaces;
        string "specular";
        c <- vec3P;
        spaces;
        return $ C.Specular c
    }

emissionP :: Parser Command
emissionP = do
    {
        spaces;
        string "emission";
        c <- vec3P;
        spaces;
        return $ C.Emission c
    }

shininessP :: Parser Command --TODO: change keyword
shininessP = do
    {
        spaces;
        string "shininess";
        spaces;
        intensity <- floatP;
        spaces;
        return $ C.Shininess intensity
    }

materialP :: Parser Command
materialP = do
    {
        try ambientP <|> try diffuseP <|> try specularP <|> try emissionP <|>  shininessP
    }

pointP :: Parser Command
pointP = do
    {
        spaces;
        string "point";
        v <- vec3P;
        spaces;
        c <- vec3P;
        spaces;
        return $ C.PL v c
    }

directionalP :: Parser Command
directionalP = do
    {
        spaces;
        string "directional";
        v <- vec3P;
        spaces;
        c <- vec3P;
        spaces;
        return $ C.DL v c
    }

lightP :: Parser Command
lightP = do
    {
        try pointP <|> directionalP;
    }

outputP :: Parser Command
outputP = do
    {
        spaces;
        string "output";
        spaces;
        path <- many1 letter;
        spaces;
        return $ C.Path path
    }

maxdepthP :: Parser Command
maxdepthP = do
    {
        spaces;
        string "maxdepth";
        spaces;
        d <- intP;
        spaces;
        return $ C.Depth d
    }

paramP :: Parser Command
paramP = do
    {
        try sizeP <|> try outputP <|>  try maxdepthP <|> try commentP <|> maxvertP;
    }

eosP :: Parser Command
eosP = do
    {
        string "EOS";
        return C.End;
    }

sceneP :: Parser [Command]
sceneP = do
            spaces;
            c <- try paramP <|> try materialP <|> try transfromP <|> try transfromP <|> try lightP <|> try triP <|> try cameraP  <|> try sphereP <|> try vertexP <|> eosP;
            case c of
                C.End -> return []
                C.Pass -> do
                            try sceneP;
                otherwise -> do
                            cs <-  try sceneP;
                            return (c : cs);




parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p s = runParser p () "DUMMY" s

parseFile :: FilePath -> IO (Either ParseError [Command])
parseFile f = parseFromFile sceneP f

runFile :: FilePath -> IO Store
runFile s = do
  p <- parseFile s
  case p of
    Left err   -> do {print err;return defaultState}
    Right commands -> return (execC commands defaultState)

-- >>> runFile "Scene/scene1.test"
-- MkStore ([Triangle (V3 (-1.0) (-1.0) 0.0) (V3 1.0 (-1.0) 0.0) (V3 1.0 1.0 0.0) (MkMaterial (V3 0.0 0.0 0.0) (V3 0.75 0.7 0.65) (V3 0.25 0.25 0.25) (V3 0.0 0.0 0.0) 25.0) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)) (V3 (V3 1.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 0.0 1.0))],[PointLight (V3 4.0 0.0 4.0) (V3 0.5 0.5 0.5) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)),DirLight (V3 0.0 0.0 1.0) (V3 0.5 0.5 0.5) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)) (V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0))],[V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0)],MkMaterial (V3 0.0 0.0 0.0) (V3 0.75 0.7 0.65) (V3 0.25 0.25 0.25) (V3 0.0 0.0 0.0) 25.0,[V3 (-1.0) (-1.0) 0.0,V3 1.0 (-1.0) 0.0,V3 1.0 1.0 0.0],Camera (V3 0.0 (-4.0) 0.5) (V3 0.0 0.0 0.0) (V3 0.0 1.0 0.0) (V3 0.0 (-0.99227786) 0.12403473) (V3 0.12403473 0.0 (-0.0)) (V3 0.0 1.53846145e-2 0.123076916) 0.5235988 0.5235988,"./output.png",2,(100,100))

-- >>> parseFile "Scene/scene1.test"
-- Right [Size 640 480,Cam (V3 0.0 (-4.0) 0.5) (V3 0.0 0.0 0.0) (V3 0.0 1.0 0.0) 30.0,Ambient (V3 0.0 0.0 0.0),Diffuse (V3 0.75 0.7 0.65),Specular (V3 0.25 0.25 0.25),Shininess 25.0,Vert (V3 (-10.0) (-10.0) 0.0),Vert (V3 10.0 (-10.0) 0.0),Vert (V3 10.0 10.0 0.0),Vert (V3 (-10.0) 10.0 0.0),Sph (V3 0.0 0.0 0.25) 0.25,Sph (V3 2.0 (-10.0) 0.25) 0.25,Sph (V3 (-2.0) 2.0 0.25) 0.25]

-- >>> parseFromString floatP "2"
-- Right 2.0

-- >>> parseFromString floatP "1"
-- Right 1.0

-- >>> parseFromString floatP ".4"
-- Right 0.4

-- >>> parseFromString intP "2"
-- Right 2

-- >>> parseFromString vec3P "0 -4 4"
-- Right (Vec3 0.0 (-4.0) 4.0)

-- >>> parseFromString vec3P ".7 .7 1"
-- Right (Vec3 0.7 0.7 1.0)

-- >>> parseFromString vertexP "vertex -1 +1 +1"
-- Right (Vert (Vec3 (-1.0) 1.0 1.0))

-- >>> parseFromString sizeP "size 640 480"
-- Right (Size 640 480)

-- >>> parseFromString sphereP "sphere 0 0 0 1"
-- Right (Sph (V3 0.0 0.0 0.0) 1.0)

-- >>> parseFromString commentP "# Just a comment"
-- Right Pass

-- >>> parseFromString cameraP "camera 0 -4 4 0 -1 0 0 1 1 45"
-- Right (Cam (V3 0.0 (-4.0) 4.0) (V3 0.0 (-1.0) 0.0) (V3 0.0 1.0 1.0) 45.0)

-- >>> parseFromString transfromP "scale 0.4 0.4 0.4"
-- Right (Scale (V3 0.4 0.4 0.4))

-- >>> parseFromString materialP "emission .7 .7 1"
-- Right (Emission (V3 0.7 0.7 1.0))
