{-# LANGUAGE DeriveGeneric #-}
module ManipulacionDatos (mostrarHerramientasUSER, guardarHerramientasUSER, mostrarListaTrabajadoresUSER, agregarParcela, mostrarParcelasUSER, obtenerDatosCosecha) where
    import Text.Printf (printf)
    import Text.Read(readMaybe)
    import GHC.Generics (Generic)
    import System.Directory (doesFileExist)
    import Data.Time(Day)
    import Data.Time.Clock(getCurrentTime, utctDay)
    import Data.Time.Format (parseTimeM, defaultTimeLocale)
    import Data.Aeson 
    import Data.Ord (comparing)
    import Data.Maybe (catMaybes)
    import Data.List.Split (splitOn)
    import Data.List (maximumBy)
    import Data.List (nubBy)
    import Data.List (find)
    import qualified Data.ByteString.Lazy.Char8 as BL

    data Herramienta = Herramienta {   
        codigoHerramienta :: String
    ,   nombreHerramienta :: String
    ,   descripcionHerramienta :: String
    ,   tipoHerramienta :: String
    } deriving (Show, Generic)

    instance FromJSON Herramienta
    instance ToJSON Herramienta

    data Trabajador = Trabajador {
          cedula :: String
        , nombreCompleto :: String
        , rol :: String
    }deriving (Show, Generic)

    instance FromJSON Trabajador
    instance ToJSON Trabajador

    data Parcela = Parcela {
          codigoParcela :: Int
        , nombreParcela :: String
        , zona :: String
        , areaEnMetrosCuadrados :: Int
        , tipoVegetal :: String
        , precioPorKilo :: Double
        , herramientas :: [Herramienta]
    }deriving (Show, Generic)

    instance FromJSON Parcela
    instance ToJSON Parcela

    data Cosecha = Cosecha {
          identificadorCosecha :: Int
        , parcelaCosecha :: Parcela
        , trabajadores :: [Trabajador]
        , fechaInicio :: Day
        , fechaFinal :: Day

    }deriving(Show, Generic)

    instance FromJSON Cosecha
    instance ToJSON Cosecha

--TODA ESTA ZONA ES DE TRABAJADORES Y HERRAMIENTAS
{----------------------------------------------------------------------------}
    obtenerTrabajadores :: FilePath -> IO[Trabajador]
    obtenerTrabajadores archivoTrabajadores = do
        contenido <-  BL.readFile archivoTrabajadores
        case eitherDecode contenido of
            Right hs-> return hs
            Left _-> return []

    mostrarListaTrabajadores :: [Trabajador] -> IO ()
    mostrarListaTrabajadores trabajadores = do
        putStrLn encabezado
        putStrLn lineaSeparadora
        mapM_ imprimirFila trabajadores
        where
            encabezado = printf "%-10s | %-35s | %-10s"
                                "Cedula" "Nombre" "rol"
            lineaSeparadora = replicate 75 '-'
            imprimirFila (Trabajador ced nom rol) =
                printf "%-10s | %-35s | %-10s\n" ced nom rol

    crearArchivoJSON :: String -> IO()
    crearArchivoJSON nombreArchivo = do
        BL.writeFile nombreArchivo (BL.pack "[]")
        putStrLn $ "Archivo "++ nombreArchivo ++" Creado exitosamente."
         
    validarExistencia :: String -> IO Bool
    validarExistencia nombreArchivo = doesFileExist nombreArchivo

    obtenerHerramientas :: FilePath -> IO [Herramienta]
    obtenerHerramientas archivo = do
        existe <- validarExistencia archivo  
        if existe
            then do
                contenido <- BL.readFile archivo
                case eitherDecode contenido of
                    Right hs -> return hs
                    Left _ -> return []
            else do
                crearArchivoJSON "herramientas.json"
                return []

    mostrarListaHerramientas :: [Herramienta] -> IO ()
    mostrarListaHerramientas herramientas = do
        putStrLn encabezado
        putStrLn lineaSeparadora
        mapM_ imprimirFila herramientas
        where
            encabezado = printf "%-10s | %-15s | %-30s | %-10s"
                            "Código" "Nombre" "Descripción" "Tipo"
            lineaSeparadora = replicate 75 '-'
            imprimirFila (Herramienta cod nom desc tipo) =
                printf "%-10s | %-15s | %-30s | %-10s\n" cod nom desc tipo


    convertirHerramienta :: String -> Maybe Herramienta
    convertirHerramienta herramienta = 
        case splitOn "," herramienta of
            [codigo, nombre, descripcion, tipo] -> Just (Herramienta codigo nombre descripcion tipo)
            _ ->
                Nothing

    leerArchivoPlano :: FilePath -> IO[Herramienta]
    leerArchivoPlano ruta = do
        contenido <- readFile ruta
        let lineas = lines contenido
            herramientas = mapM convertirHerramienta lineas
        case herramientas of
            Just hs -> do
                putStrLn("Archivo leido exitosamente")
                putStrLn ("Guardando las herramientas...") 
                return hs
            Nothing -> do
                putStrLn("Error, el archivo tiene alguna(s) lineas mal")
                return []
    
    guardarHerramientasJSON :: FilePath -> [Herramienta] -> IO()
    guardarHerramientasJSON rutaJSON herramientasLeidas = do
        herramientasExistentes <- obtenerHerramientas rutaJSON
        let todasHerramientas = herramientasExistentes ++ herramientasLeidas
            herramientasActualizadas = eliminarDuplicados todasHerramientas
        mostrarListaHerramientas herramientasActualizadas
        BL.writeFile rutaJSON(encode herramientasActualizadas)
        putStrLn ("Herramientas guardadas exitósamente...")

    eliminarDuplicados :: [Herramienta] -> [Herramienta]
    eliminarDuplicados = nubBy (\h1 h2 -> codigoHerramienta h1 == codigoHerramienta h2)


    guardarHerramientasUSER :: FilePath -> FilePath -> IO()
    guardarHerramientasUSER archivoJSON archivoHerramientas = do 
        herramientasNuevas <- leerArchivoPlano archivoHerramientas
        guardarHerramientasJSON archivoJSON herramientasNuevas
        putStrLn("Éxito.")
    
    mostrarHerramientasUSER :: FilePath -> IO()
    mostrarHerramientasUSER rutaJSON = do
        listaHerramientas <- obtenerHerramientas rutaJSON
        mostrarListaHerramientas listaHerramientas
    mostrarListaTrabajadoresUSER :: FilePath -> IO()
    mostrarListaTrabajadoresUSER archivoTrabajadores = do
        listaTrabajadores <- obtenerTrabajadores archivoTrabajadores
        mostrarListaTrabajadores listaTrabajadores  
{------------------------------------------------------------------------------}
--DE AQUÍ EN ADELANTE TRABAJAREMOS CON LA MANIPULACIÓN DE PARCELAS
    buscarHerramienta :: String -> [Herramienta] -> Maybe Herramienta
    buscarHerramienta codigo hs = find (\h -> codigoHerramienta h == codigo) hs

    obtenerHerramientasParaParcela :: String -> [Herramienta] -> [Herramienta]
    obtenerHerramientasParaParcela codigos listaHerramientas = 
        let codigosSeparados = splitOn "," codigos
            herramientasEncontradas = map (`buscarHerramienta` listaHerramientas) codigosSeparados
        in catMaybes herramientasEncontradas

    pedirPrecio :: IO Double
    pedirPrecio = do
        putStrLn "Escriba el precio por Kg."
        input <- getLine
        case readMaybe input of
            Just precio -> return precio
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo."
                pedirPrecio

    
    pedirArea :: IO Int
    pedirArea = do
        putStrLn "Escriba el area en mts**2"
        input <- getLine
        case readMaybe input of
            Just area -> return area
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo."
                pedirArea

    leerParcelas :: FilePath -> IO [Parcela]
    leerParcelas archivo = do
        existe <- validarExistencia archivo
        if not existe 
            then crearArchivoJSON archivo
            else putStrLn("")
        contenido <- BL.readFile archivo
        case decode contenido of
            Just parcelas -> return parcelas
            Nothing -> return []

    obtenerNuevoIdParcela :: FilePath -> IO Int
    obtenerNuevoIdParcela archivo = do
        parcelas <- leerParcelas archivo
        return $ case parcelas of
            [] -> 1
            ps -> maximum (map codigoParcela ps) + 1
            
    guardarParcelas :: FilePath -> [Parcela] -> IO ()
    guardarParcelas archivo parcelas = do
        existe <- validarExistencia archivo
        if existe then
            BL.writeFile archivo (encode parcelas)
            else do
                crearArchivoJSON archivo
                BL.writeFile archivo (encode parcelas)

    crearParcela :: String -> String -> String -> Double -> [Herramienta] -> Int -> IO ()
    crearParcela nombreParcela zonaParcela tipoVegetal precioPorKilo listaHerramientas areaEnMetrosCuadrados = do
        existe <- validarExistencia "parcelas.json"
        if not existe
            then crearArchivoJSON "parcelas.json"
            else putStrLn "Existente"

        listaParcelas <- leerParcelas "parcelas.json"
        id <- obtenerNuevoIdParcela "parcelas.json"

        let nuevaParcela = Parcela id nombreParcela zonaParcela areaEnMetrosCuadrados tipoVegetal precioPorKilo listaHerramientas
        let parcelasActualizadas = listaParcelas ++ [nuevaParcela]
        guardarParcelas "parcelas.json" parcelasActualizadas

    agregarParcela :: IO()
    agregarParcela = do
        putStrLn("Escriba el nombre de la parcela.")
        nombre <- getLine
        putStrLn("Escriba la zona de la parcela.")
        zona <- getLine
        area <- pedirArea
        putStrLn("Escriba el tipo de vegetal de esta parcela.")
        tipoVegetal <- getLine
        precio <- pedirPrecio
        mostrarHerramientasUSER "herramientas.json"
        putStrLn("Ingrese el código de las herramientas que quiere agregar separados por comas. \n (Los datos erróneos no serán agregados)")
        listaCodigos <- getLine
        listaHerramientas <- obtenerHerramientas "herramientas.json"
        let herramientasParcela = obtenerHerramientasParaParcela listaCodigos listaHerramientas
        nuevoId <- obtenerNuevoIdParcela "parcelas.json"
        if null herramientasParcela then do
            putStrLn("Debes agregar al menos una herramienta para la parcela")
            agregarParcela
        else
            crearParcela nombre zona tipoVegetal precio herramientasParcela area

    mostrarParcelasUSER :: IO()
    mostrarParcelasUSER = do
        parcelas <- leerParcelas "parcelas.json"
        mostrarParcelas parcelas


    mostrarParcelas :: [Parcela] -> IO ()
    mostrarParcelas parcelas = do
        if null parcelas
        then putStrLn "No hay parcelas registradas."
        else mapM_ imprimirParcela parcelas

    imprimirParcela :: Parcela -> IO ()
    imprimirParcela (Parcela id nombre zona area vegetal precio herramientas) = do
        putStrLn $ "ID DE LA PARCELA " ++ show id
        putStrLn $ "Nombre de la parcela: " ++ nombre
        putStrLn $ "Zona: " ++ zona
        putStrLn $ "Área (m²): " ++ show area
        putStrLn $ "Tipo de vegetal: " ++ vegetal
        printf "Precio por Kg: %.2f\n" precio
        putStrLn "Herramientas asociadas:"
        if null herramientas
        then putStrLn "  No hay herramientas asociadas."
        else mapM_ imprimirHerramienta herramientas
        putStrLn $ replicate 40 '-'  -- Separador entre parcelas

    imprimirHerramienta :: Herramienta -> IO ()
    imprimirHerramienta (Herramienta codigo nombre descripcion tipo) = do
        printf "  Código: %s, Nombre: %s, Descripción: %s, Tipo: %s\n" codigo nombre descripcion tipo
{-------------------------------------------------------------------------------------------}
--De aquí en adelante se trabajarán las cosechas
    validarFormatoFecha :: String -> Maybe Day
    validarFormatoFecha fechaStr =
        parseTimeM True defaultTimeLocale "%Y-%m-%d" fechaStr

    pedirFechaValida :: IO Day
    pedirFechaValida = do
        input <- getLine
        case validarFormatoFecha input of
            Just fecha -> return fecha
            Nothing -> do
                putStrLn "Formato inválido. Intente de nuevo (ejemplo: 2025-05-01)."
                pedirFechaValida

    validarFechas :: Day -> Day -> IO Bool
    validarFechas fechaInicio fechaFinal = do
        hoy <- utctDay <$> getCurrentTime
        return (fechaInicio < fechaFinal && fechaInicio <= hoy) 

    
    pedirCantidadEsperada :: IO Double
    pedirCantidadEsperada = do
        putStrLn "Escriba la cantidad esperada a recolectar en Kg."
        input <- getLine
        case readMaybe input of
            Just precio -> return precio
            Nothing -> do
                putStrLn "Entrada inválida. Intenta de nuevo."
                pedirPrecio


    obtenerDatosCosecha :: IO()
    obtenerDatosCosecha = do
        parcelas <- leerParcelas "parcelas.json"
        mostrarParcelas parcelas
        putStrLn("Ingrese el id de la parcela en la que va a cosechar")
        idParcela <- getLine
        putStrLn ("Ingrese la fecha de inicio de la cosecha (Año-Mes-Día)")
        fechaInicio <- pedirFechaValida
        putStrLn ("Ingrese la fecha de finalizacion de la cosecha (Año-Mes-Día)")
        fechaFinal <- pedirFechaValida
        putStrLn("Ingrese el vegetal a cosechar (debe de coincidir con el vegetal de la parcela elegida)")
        vegetal <- getLine
        ts <- obtenerTrabajadores "trabajadores.json" 
        mostrarListaTrabajadores ts
        putStrLn ("Seleccione los trabajadores que quiere contratar para esta cosecha, con su cedula separados por comas\n Los elementos erróneos no serán agregados")
        cedulasTrabajadores <- getLine
        cantidadEsperada <- pedirCantidadEsperada
        fechasCorrectas <- validarFechas fechaInicio fechaFinal
        if fechasCorrectas 
            then putStrLn("Fechas incorrectas")
            else putStrLn("Fechas Correctas")
        putStrLn("Cantidad Esperada: "++ show cantidadEsperada)
        putStrLn("Fecha inicio: "++show fechaInicio++". Fecha final: "++show fechaFinal)


    obtenerCosechas :: FilePath -> IO [Cosecha]
    obtenerCosechas archivo = do
        existe <- validarExistencia archivo  
        if existe
            then do
                contenido <- BL.readFile archivo
                case eitherDecode contenido of
                    Right hs -> return hs
                    Left _ -> return []
            else do
                crearArchivoJSON archivo
                return []

    guardarCosechas :: [Cosecha] -> IO()
    guardarCosechas cs = do
        BL.writeFile "cosechas.json" (encode cs)

    agregarCosechaJSON :: Cosecha -> IO()
    agregarCosechaJSON cosecha = do
        cs <- obtenerCosechas "cosechas.json"
        let listaActualizada = cs ++ [cosecha]
        guardarCosechas listaActualizada

    crearCosecha :: Int -> Parcela -> [Trabajador] -> Day -> Day -> IO()
    crearCosecha identificadorCosecha parcela ts fechaInicio fechaFinal = do
        let nuevaCosecha = (Cosecha identificadorCosecha parcela ts fechaInicio fechaFinal)
        agregarCosechaJSON nuevaCosecha
