  where errListSum [] _ _ _ = Right 0
        errListSum (x:xs) vName env body =
          case evalErr (Let vName (Cst x) body) env of
            Left er -> Left er
            Right x -> case (Right x, errListSum xs vName env body) of
                          (Right m, Right n)  -> Right (m+n)
                          (_, Left err) -> Left err
                          (Left err, _) -> Left err
